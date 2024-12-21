#![allow(dead_code)]

use clap::{command, Parser, Subcommand};
use loxide::lex::{Scanner, Token};
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
            let scanner = Scanner::from(file_contents.as_str());

            for token in scanner {
                let token = token?;
                println!("{:?}", token);
            }

            let eof_token = Token::eof();
            println!("{eof_token:?}");
        }
    }

    Ok(())
}
