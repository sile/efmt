use structopt::StructOpt;

#[derive(Debug, StructOpt)]
enum Opt {
    Pp(efmt::commands::PreprocessOpt),
    Parse(efmt::commands::ParseOpt),
}

fn main() -> anyhow::Result<()> {
    let opt = Opt::from_args();
    match opt {
        Opt::Pp(opt) => opt.run()?,
        Opt::Parse(opt) => opt.run()?,
    }
    Ok(())
}
