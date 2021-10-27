use structopt::StructOpt;

#[derive(Debug, StructOpt)]
enum Opt {
    // Tokenize(efmt::commands::tokenize::TokenizeOpt),
// Pp(efmt::commands::pp::PreprocessOpt),
// Parse(efmt::commands::ParseOpt),
// Format(efmt::commands::FormatOpt),
}

fn main() -> anyhow::Result<()> {
    let _opt = Opt::from_args();
    // match opt {
    // Opt::Tokenize(opt) => opt.run()?,
    // Opt::Pp(opt) => opt.run()?,
    // Opt::Parse(opt) => opt.run()?,
    // Opt::Format(opt) => opt.run()?,
    // }
    Ok(())
}
