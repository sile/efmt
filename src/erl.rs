use std::path::PathBuf;
use std::process::Command;

pub fn erl_eval(expr: &str) -> anyhow::Result<String> {
    let args = [
        "-noshell".to_owned(),
        "-eval".to_owned(),
        format!("io:format(\"~s\", [{}]), halt().", expr),
    ];
    let output = Command::new("erl")
        .args(args)
        .env("ERL_CRASH_DUMP_SECONDS", "0")
        .output()?;
    anyhow::ensure!(
        output.status.success(),
        "failed to evaluate an Erlang expr: {:?}",
        expr
    );
    Ok(String::from_utf8(output.stdout)?)
}

pub fn code_lib_dir(app_name: &str) -> anyhow::Result<PathBuf> {
    erl_eval(&format!("code:lib_dir({})", app_name)).map(PathBuf::from)
}
