use efmt::format::FormatOptions;
use efmt::items::module::Module;
use env_logger::Env;
use std::io::Read as _;
use std::path::PathBuf;
use structopt::StructOpt;

// TODO: Provide `erlfmt` compatible options
#[derive(Debug, StructOpt)]
struct Opt {
    file: Option<PathBuf>,
    #[structopt(long, default_value = "120")]
    max_columns: usize,

    /// Where to search for include files.
    #[structopt(short = "I")]
    include_dirs: Vec<PathBuf>,

    #[structopt(long)]
    skip_validation: bool,

    #[cfg(feature = "pprof")]
    #[structopt(long)]
    profile: bool,

    #[cfg(feature = "pprof")]
    #[structopt(long, default_value = "flamegraph.svg")]
    flamegraph_path: PathBuf,
}

fn main() -> anyhow::Result<()> {
    env_logger::Builder::from_env(Env::default().default_filter_or("warn")).init();

    let opt = Opt::from_args();
    let format_options = FormatOptions::new()
        .max_columns(opt.max_columns)
        .include_dirs(opt.include_dirs.clone());

    #[cfg(feature = "pprof")]
    let guard = if opt.profile {
        Some(pprof::ProfilerGuard::new(100)?)
    } else {
        None
    };

    let (text, formatted_text) = match opt.file {
        Some(path) => {
            let text = std::fs::read_to_string(&path)?;
            (text, format_options.format_file::<Module, _>(path)?)
        }
        None => {
            let mut text = String::new();
            std::io::stdin().lock().read_to_string(&mut text)?;
            let formatted = format_options.format_text::<Module>(&text)?;
            (text, formatted)
        }
    };

    #[cfg(feature = "pprof")]
    if let Some(report) = guard.map(|x| x.report().build()).transpose()? {
        let file = std::fs::File::create(&opt.flamegraph_path)?;
        report.flamegraph(file)?;
    };

    if !opt.skip_validation {
        validate_formatted_text(&text, &formatted_text)?;
    }

    print!("{}", formatted_text);

    Ok(())
}

fn validate_formatted_text(text: &str, formatted_text: &str) -> anyhow::Result<()> {
    let tokens0 = erl_tokenize::Tokenizer::new(text);
    let tokens1 = erl_tokenize::Tokenizer::new(formatted_text);
    for (t0, t1) in tokens0.zip(tokens1) {
        anyhow::ensure!(t0?.text() == t1?.text());
    }
    Ok(())
}
