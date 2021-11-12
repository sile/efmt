use efmt::items::module::Module;
use env_logger::Env;
use std::io::Read as _;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Opt {
    file: Option<PathBuf>,
    #[structopt(long, default_value = "120")]
    max_columns: usize,
    // TODO: code_path option
}

fn main() -> anyhow::Result<()> {
    env_logger::Builder::from_env(Env::default().default_filter_or("warn")).init();

    let opt = Opt::from_args();

    let format_options = efmt::FormatOptions::<Module>::new().max_columns(opt.max_columns);
    let formatted_text = match opt.file {
        Some(path) => format_options.format_file(path)?,
        None => {
            let mut text = String::new();
            std::io::stdin().lock().read_to_string(&mut text)?;
            format_options.format_text(&text)?
        }
    };

    // TODO: check before/after texts represent the same semantic meaning
    //       (e.g., remove newlines and redundant spaces from those and compare the results)

    println!("{}", formatted_text);

    Ok(())
}
