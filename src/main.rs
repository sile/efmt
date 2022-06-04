use anyhow::Context;
use clap::{CommandFactory as _, Parser};
use efmt::files::RebarConfigValue;
use efmt::items::ModuleOrConfig;
use env_logger::Env;
use rayon::iter::{IntoParallelIterator as _, ParallelIterator};
use std::io::Read as _;
use std::io::Write as _;
use std::path::{Path, PathBuf};

const DEFAULT_CACHE_DIR: &str = ".efmt/cache";

/// Erlang Code Formatter.
#[derive(Debug, Parser)]
#[clap(about, version)]
struct Opt {
    /// Maximum line length.
    ///
    /// Note that this is a soft limit. That is, some lines could exceed the limit after formatting.
    /// Besides, this limit doesn't apply to comments.
    #[clap(long, default_value_t = 120)]
    print_width: usize,

    /// Checks if input is formatted correctly.
    ///
    /// If so, exits with 0. Otherwise, exits with 1 and shows a diff.
    #[clap(long, short)]
    check: bool,

    /// Overwrites input file with the formatted text.
    #[clap(long, short, conflicts_with = "check")]
    write: bool,

    /// Shows the target input files.
    ///
    /// You can use this flag to exclude some files from the default target, e.g., `$ efmt $(efmt --show-files | grep -v rebar.config)`.
    #[clap(long, conflicts_with = "check", conflicts_with = "write")]
    show_files: bool,

    /// Outputs debug log messages.
    #[clap(long)]
    verbose: bool,

    /// Where to search for include files to process Erlang `-include` directives.
    ///
    /// If omitted, "../", "../include/", "../src/" and "../test/" of the target file will be added as the include directories.
    #[clap(short = 'I', long = "include-search-dir")]
    include_dirs: Vec<PathBuf>,

    /// Format target files.
    ///
    /// `-` means the standard input.
    /// If no files are specified and any of `-c`, `-w` or `--show-files` options is specified,
    /// All of the files named `**.{hrl,erl,app.src}` and `**/rebar.config` are used as the default
    /// (note that files spcified by `.gitignore` will be ignored).
    files: Vec<PathBuf>,

    /// Executes formatting in parallel.
    #[clap(long)]
    parallel: bool,

    /// Disables `-include` and `-include_lib` processing.
    /// This could improve formatting speed. All unknown macros will be replaced with `EFMT_DUMMY` atom.
    #[clap(long)]
    disable_include: bool,

    /// Where to save the caches for the macro definitions collected during processing `-include` or `-include_lib` directives
    /// [default: .efmt/cache]
    #[clap(long)]
    include_cache_dir: Option<PathBuf>,

    /// Disables include cache.
    #[clap(long)]
    disable_include_cache: bool,

    /// Disables formatting by default.
    /// efmt behaves as if there is a "% @efmt:off" comment at the head of the each target file.
    #[clap(long)]
    default_off: bool,

    /// Disables mimicking the behavior of `rebar3 efmt`.
    #[clap(long)]
    disable_rebar3_efmt_mode: bool,
}

impl Opt {
    fn collect_default_files_if_need(&mut self) -> anyhow::Result<()> {
        if !self.files.is_empty() || !(self.check || self.write || self.show_files) {
            return Ok(());
        }

        self.files = efmt::files::collect_default_target_files()?;
        if !self.files.is_empty() && !self.show_files {
            log::info!(
                "The following files were added as the default input files:\n{}",
                self.files
                    .iter()
                    .map(|f| format!("- {}", f.to_str().unwrap_or("<unknown>")))
                    .collect::<Vec<_>>()
                    .join("\n")
            );
        }
        Ok(())
    }

    fn to_format_options(&self) -> efmt::Options {
        let mut format_options = efmt::Options::new()
            .max_columns(self.print_width)
            .include_dirs(self.include_dirs.clone());

        if !self.disable_include_cache {
            format_options = format_options.include_cache_dir(
                self.include_cache_dir
                    .clone()
                    .unwrap_or_else(|| PathBuf::from(DEFAULT_CACHE_DIR)),
            );
        }
        if self.disable_include {
            format_options = format_options.disable_include();
        }
        if self.default_off {
            format_options = format_options.default_off();
        }

        format_options
    }

    fn enable_rebar3_efmt_mode(&mut self, rebar_config_dir: PathBuf) -> anyhow::Result<()> {
        // rebar.config
        let rebar_config_path = rebar_config_dir.join("rebar.config");
        for value in efmt::files::load_rebar_config(&rebar_config_path)
            .with_context(|| format!("failed to load rebar.config file: {rebar_config_path:?}"))?
        {
            if let RebarConfigValue::Tuple(item) = value {
                if item.len() != 2 {
                    continue;
                }
                match (&item[0], &item[1]) {
                    (RebarConfigValue::Atom(key), RebarConfigValue::List(items))
                        if key == "erl_opts" =>
                    {
                        self.handle_rebar_config_erl_opts(&items, &rebar_config_dir);
                    }
                    (RebarConfigValue::Atom(key), RebarConfigValue::List(items))
                        if key == "efmt" =>
                    {
                        self.handle_rebar_config_efmt(&items, &rebar_config_dir);
                    }
                    _ => {}
                }
            }
        }

        // ERL_LIBS
        let libs_path = rebar_config_dir.join("_build/default/lib/");
        if let Some(paths) = std::env::var_os("ERL_LIBS") {
            let paths = std::env::split_paths(&paths).chain(std::iter::once(libs_path));
            let paths = std::env::join_paths(paths)?;
            std::env::set_var("ERL_LIBS", paths);
        } else {
            std::env::set_var("ERL_LIBS", libs_path);
        }
        if let Some(paths) = std::env::var_os("ERL_LIBS") {
            log::debug!("set 'ERL_LIBS' envvar to {paths:?}");
        }

        // include cache dir
        if self.include_cache_dir.is_none() {
            self.include_cache_dir = Some(rebar_config_dir.join(DEFAULT_CACHE_DIR));
        }

        Ok(())
    }

    fn handle_rebar_config_erl_opts(
        &mut self,
        items: &[RebarConfigValue],
        rebar_config_dir: &PathBuf,
    ) {
        for item in items {
            if let RebarConfigValue::Tuple(kv) = item {
                if kv.len() != 2 {
                    continue;
                }
                match (&kv[0], &kv[1]) {
                    (RebarConfigValue::Atom(k), RebarConfigValue::String(v)) if k == "i" => {
                        log::debug!("found `{{i, {v:?}}}` in rebar.config");
                        self.include_dirs.push(rebar_config_dir.join(v));
                    }
                    _ => {}
                }
            }
        }
    }

    fn handle_rebar_config_efmt(&mut self, items: &[RebarConfigValue], rebar_config_dir: &PathBuf) {
        let matches = Self::command().get_matches();
        for item in items {
            log::debug!("found an efmt option in rebar.config: {item:?}");
            if let RebarConfigValue::Atom(k) = item {
                match k.as_str() {
                    "parallel" => {
                        self.parallel = true;
                        continue;
                    }
                    "disable_include" => {
                        self.disable_include = true;
                        continue;
                    }
                    "disable_include_cache" => {
                        self.disable_include_cache = true;
                        continue;
                    }
                    "default_off" => {
                        self.default_off = true;
                        continue;
                    }
                    _ => {}
                }
            } else if let Some((k, v)) = item.as_kv_tuple() {
                match k {
                    "print_width" => {
                        if let RebarConfigValue::Integer(v) = v {
                            if matches.occurrences_of("print-width") == 0 {
                                self.print_width = *v as usize;
                            } else {
                                log::debug!("ignored {k:?} option in rebar.config in favor of command line arg");
                            }
                            continue;
                        }
                    }
                    "I" | "include_search_dir" => {
                        if let RebarConfigValue::String(v) = v {
                            self.include_dirs.push(rebar_config_dir.join(v));
                            continue;
                        }
                    }
                    "include_cache_dir" => {
                        if let RebarConfigValue::String(v) = v {
                            if matches.occurrences_of("include-cache-dir") == 0 {
                                self.include_cache_dir = Some(rebar_config_dir.join(v));
                            } else {
                                log::debug!("ignored {k:?} option in rebar.config in favor of command line arg");
                            }
                            continue;
                        }
                    }
                    _ => {}
                }
            }
            log::warn!("found an unhandled efmt option in rebar.config: {item:?}");
        }
    }
}

fn main() -> anyhow::Result<()> {
    let mut opt = Opt::parse();

    let loglevel = if opt.verbose { "debug" } else { "info" };
    env_logger::Builder::from_env(Env::default().default_filter_or(loglevel)).init();

    if !opt.disable_rebar3_efmt_mode {
        if let Some(rebar_config_dir) = efmt::files::find_rebar_config_dir() {
            log::debug!("rebar.config file found: dir={rebar_config_dir:?}");
            opt.enable_rebar3_efmt_mode(rebar_config_dir)?;
        } else {
            log::debug!("rebar.config file not found");
        }
    }

    opt.collect_default_files_if_need()?;
    if opt.files.is_empty() {
        Opt::command().print_help()?;
        println!();
        std::process::exit(1);
    }

    if opt.show_files {
        for file in opt.files {
            if let Some(file) = file.to_str() {
                println!("{}", file);
            }
        }
        Ok(())
    } else if opt.check {
        check_files(&opt)
    } else {
        format_files(&opt)
    }
}

fn format_file<P: AsRef<Path>>(
    format_options: &efmt::Options,
    path: P,
) -> anyhow::Result<(String, String)> {
    let original = std::fs::read_to_string(&path)?;
    let formatted = format_options
        .clone()
        .format_file::<ModuleOrConfig, _>(path)?;
    Ok((original, formatted))
}

fn format_stdin(format_options: &efmt::Options) -> anyhow::Result<(String, String)> {
    let mut original = String::new();
    std::io::stdin().lock().read_to_string(&mut original)?;
    let formatted = format_options
        .clone()
        .format_text::<ModuleOrConfig>(&original)?;
    Ok((original, formatted))
}

fn format_file_or_stdin<P: AsRef<Path>>(
    format_options: &efmt::Options,
    path: P,
) -> anyhow::Result<(String, String)> {
    let (original, formatted) = if path.as_ref().to_str() == Some("-") {
        format_stdin(format_options)
    } else {
        format_file(format_options, &path)
    }?;
    validate_formatted_text(path, &original, &formatted).context(concat!(
        "Found a token mismatch between the original text ",
        "and the formatted one (maybe efmt bug)"
    ))?;
    Ok((original, formatted))
}

fn format_files(opt: &Opt) -> anyhow::Result<()> {
    let format_options = opt.to_format_options();

    fn do_format(opt: &Opt, format_options: &efmt::Options, file: &Path) -> anyhow::Result<()> {
        match format_file_or_stdin(format_options, file) {
            Err(e) => {
                log::error!("Failed to format {:?}\n{:?}", file, e);
                Err(e)
            }
            Ok((original, formatted)) => {
                if opt.write {
                    if original != formatted {
                        let result = overwrite(file, &formatted);
                        if let Err(e) = &result {
                            log::error!("Failed to write formatted text to {:?}: {:?}", file, e);
                        } else {
                            log::info!("Overwrote {:?}", file);
                        }
                        result
                    } else {
                        Ok(())
                    }
                } else {
                    print!("{}", formatted);
                    Ok(())
                }
            }
        }
    }

    let error_files = if opt.parallel {
        opt.files
            .clone()
            .into_par_iter()
            .filter(|file| do_format(opt, &format_options, file).is_err())
            .collect::<Vec<_>>()
    } else {
        opt.files
            .iter()
            .filter(|file| do_format(opt, &format_options, file).is_err())
            .cloned()
            .collect::<Vec<_>>()
    };

    if !error_files.is_empty() {
        eprintln!();
        anyhow::bail!(
            "Failed to format the following files:\n{}",
            error_files
                .iter()
                .map(|f| format!("- {}", f.to_str().unwrap_or("<unknown>")))
                .collect::<Vec<_>>()
                .join("\n"),
        );
    } else if opt.write {
        log::info!("All files were formatted correctly!");
    }

    Ok(())
}

fn check_files(opt: &Opt) -> anyhow::Result<()> {
    let format_options = opt.to_format_options();

    fn do_check(format_options: &efmt::Options, file: &Path) -> bool {
        match format_file_or_stdin(format_options, file) {
            Err(e) => {
                log::error!("Failed to format {:?}\n{:?}", file, e);
                false
            }
            Ok((original, formatted)) => {
                if original == formatted {
                    log::info!("{:?} is already formatted correctly.", file);
                    true
                } else {
                    let diff = efmt::diff::text_diff(&original, &formatted, file);
                    println!("{}", diff);
                    log::info!("{:?} is not formatted correctly.", file);
                    false
                }
            }
        }
    }

    let unformatted_files = if opt.parallel {
        opt.files
            .clone()
            .into_par_iter()
            .filter(|file| !do_check(&format_options, file))
            .collect::<Vec<_>>()
    } else {
        opt.files
            .iter()
            .filter(|file| !do_check(&format_options, file))
            .cloned()
            .collect::<Vec<_>>()
    };

    if !unformatted_files.is_empty() {
        eprintln!();
        anyhow::bail!(
            "The following files need to be formatted:\n{}",
            unformatted_files
                .iter()
                .map(|f| format!("- {}", f.to_str().unwrap_or("<unknown>")))
                .collect::<Vec<_>>()
                .join("\n"),
        );
    } else {
        eprintln!("All input files are formatted correctly!");
    }
    Ok(())
}

fn validate_formatted_text<P: AsRef<Path>>(
    path: P,
    original: &str,
    formatted: &str,
) -> anyhow::Result<()> {
    use erl_tokenize::{PositionRange as _, Result, Token, Tokenizer};

    fn is_visible_token(t: &Result<Token>) -> bool {
        !matches!(t, Ok(Token::Whitespace(_)))
    }

    fn check_extra_token<P: AsRef<Path>>(
        path: P,
        text: &str,
        next_token: Option<Result<Token>>,
    ) -> anyhow::Result<()> {
        let next_position = next_token.map(|r| {
            r.map(|t| t.start_position())
                .unwrap_or_else(|e| e.position().clone())
        });
        if let Some(p) = next_position {
            anyhow::bail!(
                "{}",
                efmt::error::generate_error_message(text, Some(path), p.into(), "extra token")
            );
        }
        Ok(())
    }

    fn text(token: &Token) -> &str {
        if let Token::Comment(token) = token {
            token.text().trim_end()
        } else {
            token.text()
        }
    }

    let mut tokens0 = Tokenizer::new(original).filter(is_visible_token);
    let mut tokens1 = Tokenizer::new(formatted).filter(is_visible_token);
    while let Some(t0) = tokens0.next().transpose().expect("unreachable") {
        let t1 = match tokens1.next() {
            Some(Ok(t1)) => t1,
            Some(Err(e)) => {
                let reason = e.to_string();
                let reason_end = reason.find(" (").unwrap_or(reason.len());
                anyhow::bail!(
                    "{}",
                    efmt::error::generate_error_message(
                        formatted,
                        Some("<formatted>"),
                        e.position().clone().into(),
                        &reason[..reason_end]
                    )
                );
            }
            None => {
                return check_extra_token(path, original, Some(Ok(t0)));
            }
        };
        anyhow::ensure!(
            text(&t0) == text(&t1),
            "{}\n{}",
            efmt::error::generate_error_message(
                original,
                Some(path),
                t0.start_position().into(),
                "expected"
            ),
            efmt::error::generate_error_message(
                formatted,
                Some("<formatted>"),
                t1.start_position().into(),
                "actual"
            ),
        );
    }
    check_extra_token("<formatted>", formatted, tokens1.next())
}

fn overwrite<P: AsRef<Path>>(path: P, text: &str) -> anyhow::Result<()> {
    let mut temp = tempfile::NamedTempFile::new()?;
    temp.write_all(text.as_bytes())?;
    temp.persist(path)?;
    Ok(())
}
