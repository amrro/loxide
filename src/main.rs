#![allow(dead_code)]

use clap::{command, Parser, Subcommand};
use loxide::lexer::tokenize;
use miette::{Context, IntoDiagnostic};

use std::{fs, path::PathBuf};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Tokenize { filename: PathBuf },
}

fn main() -> miette::Result<()> {
    let args = Args::parse();
    match args.command {
        Commands::Tokenize { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("Failed to read {}", filename.display()))?;
            let token_iter = tokenize(&file_contents);

            for token in token_iter.take(15) {
                println!("{:?}", token);
            }
        }
    }

    Ok(())
}
