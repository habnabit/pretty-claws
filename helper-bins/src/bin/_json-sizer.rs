#![feature(try_blocks)]

use std::io::Write;

use anyhow::Result;

fn main() -> Result<()> {
    let mut args = std::env::args();
    let out_file = match try {
        let _ = args.next()?;
        args.next()?
    } {
        Some(p) => p,
        None => anyhow::bail!("argv < 2"),
    };
    let mut out_file = std::fs::File::create(out_file)?;
    let mut sizes = json::object::Object::new();
    for path in args {
        let path: std::path::PathBuf = path.try_into()?;
        let stat = std::fs::metadata(&path)?;
        let file_name = path.file_name().unwrap().to_string_lossy();
        sizes.insert(&file_name, stat.len().into());
    }
    out_file.write_all(json::stringify(sizes).as_bytes())?;
    Ok(())
}
