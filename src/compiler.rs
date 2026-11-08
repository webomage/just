use super::*;

pub(crate) struct Compiler;

impl Compiler {
  pub(crate) fn compile<'src>(
    config: &Config,
    loader: &'src Loader,
    root: &Utf8Path,
  ) -> RunResult<'src, Compilation<'src>> {
    let mut asts = HashMap::<(Modulepath, Utf8PathBuf), Ast>::new();
    let mut loaded = Vec::new();
    let mut numerator = Numerator::new();
    let mut paths = HashMap::<Utf8PathBuf, Utf8PathBuf>::new();
    let mut stack = Vec::new();
    stack.push(Source::root(root));

    while let Some(current) = stack.pop() {
      let key = (
        current
          .namepath
          .as_ref()
          .map(Modulepath::from)
          .unwrap_or_default(),
        current.path.clone(),
      );

      if asts.contains_key(&key) {
        continue;
      }

      let (relative, src) = loader.load(root, &current.path)?;

      if paths
        .insert(current.path.clone(), relative.into())
        .is_none()
      {
        loaded.push(relative.into());
      }

      let mut ast = Parser::parse_source(&mut numerator, relative, &current, src)?;

      for item in &mut ast.items {
        if !item.is_enabled() {
          continue;
        }

        match item {
          Item::Module {
            absolute,
            name,
            optional,
            relative,
            ..
          } => {
            let parent = current.path.parent().unwrap();

            let relative = relative
              .as_ref()
              .map(|relative| Self::expand_tilde(&relative.cooked))
              .transpose()?;

            let import = Self::find_module_file(parent, *name, relative.as_deref())?;

            if let Some(import) = import {
              if current.file_path.contains(&import) {
                return Err(Error::CircularImport {
                  current: current.path,
                  import,
                });
              }
              *absolute = Some(import.clone());
              stack.push(current.module(*name, import));
            } else if !*optional {
              return Err(Error::MissingModuleFile { module: *name });
            }
          }
          Item::Import {
            relative,
            absolute,
            absolutes,
            optional,
            ..
          } => {
            let parent = current.path.parent().unwrap();
            let expanded = Self::expand_tilde(&relative.cooked)?;

            if expanded.as_str().contains('*') || expanded.as_str().contains('?') {
              let matched = Self::expand_wildcard_imports(parent, expanded.as_str())?;
              if matched.is_empty() {
                if !*optional {
                  return Err(Error::MissingImportFile {
                    path: relative.token,
                  });
                }
              } else {
                for (idx, import) in matched.into_iter().enumerate() {
                  if current.file_path.contains(&import) {
                    return Err(Error::CircularImport {
                      current: current.path,
                      import,
                    });
                  }
                  absolutes.push(import.clone());
                  stack.push(current.import(import, relative.token.offset + idx));
                }
              }
            } else {
              let import = parent.join(expanded).clean();

              if filesystem::is_file(&import)? {
                if current.file_path.contains(&import) {
                  return Err(Error::CircularImport {
                    current: current.path,
                    import,
                  });
                }
                *absolute = Some(import.clone());
                stack.push(current.import(import, relative.token.offset));
              } else if !*optional {
                return Err(Error::MissingImportFile {
                  path: relative.token,
                });
              }
            }
          }
          _ => {}
        }
      }

      asts.insert(key, ast.clone());
    }

    let mut overrides = HashMap::new();

    let justfile = Analyzer::analyze(
      &asts,
      config,
      None,
      &[],
      &loaded,
      &Modulepath::default(),
      None,
      &mut overrides,
      &paths,
      false,
      root,
    )?;

    let mut unknown_overrides = Vec::new();

    for ((path, name), value) in &config.overrides {
      if let Some(assignment) = justfile
        .submodule(path)
        .and_then(|module| module.assignments.get(name))
      {
        overrides.insert(assignment.number, value.clone());
      } else {
        unknown_overrides.push(path.join(name).to_string());
      }
    }

    if !unknown_overrides.is_empty() {
      return Err(Error::UnknownOverrides {
        overrides: unknown_overrides,
      });
    }

    Ok(Compilation {
      asts,
      justfile,
      overrides,
      root: root.into(),
    })
  }

  fn find_module_file<'src>(
    parent: &Utf8Path,
    module: Name<'src>,
    path: Option<&Utf8Path>,
  ) -> RunResult<'src, Option<Utf8PathBuf>> {
    let mut candidates = Vec::new();

    if let Some(path) = path {
      let full = parent.join(path).clean();

      if filesystem::is_file(&full)? {
        return Ok(Some(full));
      }

      candidates.push((path.join("mod.just"), true));

      for name in search::JUSTFILE_NAMES {
        candidates.push((path.join(name), false));
      }
    } else {
      candidates.push((format!("{module}.just").into(), true));
      candidates.push((format!("{module}/mod.just").into(), true));

      for name in search::JUSTFILE_NAMES {
        candidates.push((format!("{module}/{name}").into(), false));
      }
    }

    let mut grouped = BTreeMap::<Utf8PathBuf, Vec<(Utf8PathBuf, bool)>>::new();

    for (candidate, case_sensitive) in candidates {
      let candidate = parent.join(candidate).clean();
      grouped
        .entry(candidate.parent().unwrap().into())
        .or_default()
        .push((candidate, case_sensitive));
    }

    let mut found = Vec::new();

    for (directory, candidates) in grouped {
      let entries = match fs::read_dir(&directory) {
        Ok(entries) => entries,
        Err(io_error) => {
          if io_error.kind() == io::ErrorKind::NotADirectory
            || io_error.kind() == io::ErrorKind::NotFound
          {
            continue;
          }

          return Err(
            SearchError::FilesystemIo {
              source: io_error,
              path: directory,
            }
            .into(),
          );
        }
      };

      for entry in entries {
        let entry = entry.context(search_error::FilesystemIo { path: &directory })?;

        if let Some(name) = entry.file_name().to_str() {
          for (candidate, case_sensitive) in &candidates {
            let candidate_name = candidate.file_name().unwrap();

            let eq = if *case_sensitive {
              name == candidate_name
            } else {
              name.eq_ignore_ascii_case(candidate_name)
            };

            if eq {
              found.push(candidate.parent().unwrap().join(name));
            }
          }
        }
      }
    }

    if found.len() > 1 {
      found.sort();
      Err(Error::AmbiguousModuleFile {
        found: found
          .into_iter()
          .map(|found| {
            found
              .strip_prefix(parent)
              .map(Utf8PathBuf::from)
              .unwrap_or(found)
          })
          .collect(),
        module,
      })
    } else {
      Ok(found.pop())
    }
  }

  fn expand_tilde(path: &str) -> RunResult<'static, Utf8PathBuf> {
    Ok(if let Some(path) = path.strip_prefix("~/") {
      dir::home_directory_required()?.join(path.trim_start_matches('/'))
    } else {
      Utf8PathBuf::from(path)
    })
  }

  fn wildcard_matches(pattern: &str, name: &str) -> bool {
    let mut pat_str = String::from("^");
    for ch in pattern.chars() {
      match ch {
        '*' => pat_str.push_str(".*"),
        '?' => pat_str.push('.'),
        '.' | '+' | '(' | ')' | '[' | ']' | '{' | '}' | '^' | '$' | '|' | '\\' => {
          pat_str.push('\\');
          pat_str.push(ch);
        }
        _ => pat_str.push(ch),
      }
    }
    pat_str.push('$');
    Regex::new(&pat_str)
      .map(|re| re.is_match(name))
      .unwrap_or(false)
  }

  fn expand_wildcard_imports(
    parent: &Utf8Path,
    pattern: &str,
  ) -> RunResult<'static, Vec<Utf8PathBuf>> {
    let mut candidates = vec![parent.to_owned()];

    for component in Utf8Path::new(pattern).components() {
      match component {
        Utf8Component::CurDir => {}
        Utf8Component::ParentDir => {
          candidates = candidates
            .into_iter()
            .map(|c| c.join("..").clean())
            .collect();
        }
        Utf8Component::RootDir | Utf8Component::Prefix(_) => {
          candidates = vec![Utf8PathBuf::from(component.as_str())];
        }
        Utf8Component::Normal(seg) => {
          if seg.contains('*') || seg.contains('?') {
            let mut next = Vec::new();
            for dir in candidates {
              if let Ok(entries) = fs::read_dir(&dir) {
                let mut matched_in_dir = Vec::new();
                for entry in entries.flatten() {
                  if let Ok(utf8_path) = Utf8PathBuf::from_path_buf(entry.path())
                    && let Some(file_name) = utf8_path.file_name()
                    && Self::wildcard_matches(seg, file_name)
                  {
                    matched_in_dir.push(utf8_path.clean());
                  }
                }
                matched_in_dir.sort();
                next.extend(matched_in_dir);
              }
            }
            candidates = next;
          } else {
            candidates = candidates
              .into_iter()
              .map(|c| c.join(seg).clean())
              .collect();
          }
        }
      }
    }

    let mut files = Vec::new();
    for candidate in candidates {
      if filesystem::is_file(&candidate)? && !files.contains(&candidate) {
        files.push(candidate);
      }
    }
    files.sort();
    Ok(files)
  }

  #[cfg(test)]
  pub(crate) fn test_compile(src: &str) -> CompileResult<Justfile> {
    let tokens = Lexer::test_lex(src)?;
    let ast = Parser::parse_tokens(&mut Numerator::new(), &tokens)?;
    let root = Utf8PathBuf::from("justfile");
    let mut asts: HashMap<(Modulepath, Utf8PathBuf), Ast> = HashMap::new();
    asts.insert((Modulepath::default(), root.clone()), ast);
    let mut paths: HashMap<Utf8PathBuf, Utf8PathBuf> = HashMap::new();
    paths.insert(root.clone(), root.clone());
    Analyzer::analyze(
      &asts,
      &Config::new().unwrap(),
      None,
      &[],
      &[],
      &Modulepath::default(),
      None,
      &mut HashMap::new(),
      &paths,
      false,
      &root,
    )
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn recursive_includes_fail() {
    let tmp = tempfile::tempdir().unwrap();
    let root = dir::temporary_directory(&tmp).unwrap();
    fs::write(root.join("justfile"), "import './subdir/b'\na: b").unwrap();
    fs::create_dir_all(root.join("subdir")).unwrap();
    fs::write(root.join("subdir/b"), "import '../justfile'\nb:").unwrap();

    let loader = Loader::new();

    let justfile_a_path = root.join("justfile");
    let loader_output =
      Compiler::compile(&Config::new().unwrap(), &loader, &justfile_a_path).unwrap_err();

    assert_matches!(loader_output, Error::CircularImport { current, import }
      if current == root.join("subdir").join("b").clean() &&
      import == root.join("justfile").clean()
    );
  }

  #[test]
  fn find_module_file() {
    #[track_caller]
    fn case(path: Option<&str>, files: &[&str], expected: Result<Option<&str>, &[&str]>) {
      let module = Name {
        token: Token {
          column: 0,
          kind: TokenKind::Identifier,
          length: 3,
          line: 0,
          offset: 0,
          path: Utf8Path::new(""),
          src: "foo",
        },
      };

      let tempdir = tempfile::tempdir().unwrap();
      let root = dir::temporary_directory(&tempdir).unwrap();

      for file in files {
        if let Some(parent) = Utf8Path::new(file).parent() {
          fs::create_dir_all(root.join(parent)).unwrap();
        }

        fs::write(root.join(file), "").unwrap();
      }

      let actual = Compiler::find_module_file(root, module, path.map(Utf8Path::new));

      match expected {
        Err(expected) => match actual.unwrap_err() {
          Error::AmbiguousModuleFile { found, .. } => {
            assert_eq!(
              found,
              expected
                .iter()
                .map(|expected| expected.replace('/', std::path::MAIN_SEPARATOR_STR).into())
                .collect::<Vec<Utf8PathBuf>>()
            );
          }
          _ => panic!("unexpected error"),
        },
        Ok(Some(expected)) => assert_eq!(
          actual.unwrap().unwrap(),
          root.join(expected.replace('/', std::path::MAIN_SEPARATOR_STR))
        ),
        Ok(None) => assert_eq!(actual.unwrap(), None),
      }
    }

    case(None, &["foo.just"], Ok(Some("foo.just")));
    case(None, &["FOO.just"], Ok(None));
    case(None, &["foo/mod.just"], Ok(Some("foo/mod.just")));
    case(None, &["foo/MOD.just"], Ok(None));
    case(None, &["foo/justfile"], Ok(Some("foo/justfile")));
    case(None, &["foo/JUSTFILE"], Ok(Some("foo/JUSTFILE")));
    case(None, &["foo/.justfile"], Ok(Some("foo/.justfile")));
    case(None, &["foo/.JUSTFILE"], Ok(Some("foo/.JUSTFILE")));
    case(
      None,
      &["foo/.justfile", "foo/justfile"],
      Err(&["foo/.justfile", "foo/justfile"]),
    );
    case(None, &["foo/JUSTFILE"], Ok(Some("foo/JUSTFILE")));

    case(Some("bar"), &["bar"], Ok(Some("bar")));
    case(Some("bar"), &["bar/mod.just"], Ok(Some("bar/mod.just")));
    case(Some("bar"), &["bar/justfile"], Ok(Some("bar/justfile")));
    case(Some("bar"), &["bar/JUSTFILE"], Ok(Some("bar/JUSTFILE")));
    case(Some("bar"), &["bar/.justfile"], Ok(Some("bar/.justfile")));
    case(Some("bar"), &["bar/.JUSTFILE"], Ok(Some("bar/.JUSTFILE")));

    case(
      Some("bar"),
      &["bar/justfile", "bar/mod.just"],
      Err(&["bar/justfile", "bar/mod.just"]),
    );
  }
}
