#![allow(dead_code)]

use clap::{command, Parser, Subcommand};
use loxide::{lexer::tokenize, parse::parse};
use miette::{Context, IntoDiagnostic};

use std::{
    fs,
    io::{self, stdout, Write},
    path::PathBuf,
};

#[derive(Parser, Debug)]
#[command(
    version = "0.0.1",
    about = "Loxide: A simple interpreter with a REPL.",
    long_about = "Loxide is a simple interpreter for educational purposes. I just created it for fun."
)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    #[command(about = "Tokenize a source file and output the resulting tokens.")]
    Tokenize {
        /// Path to the source file
        #[arg(help = "The path to a file to tokenize.")]
        filename: PathBuf,
    },

    #[command(about = "Start an interactive REPL to evaluate expressions.")]
    Interpret,
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
        _ => run_repl(),
    }

    Ok(())
}

fn run_repl() {
    let mut input = String::new();
    let stdin = io::stdin();

    loop {
        print!("> ");
        input.clear();
        stdout().flush().unwrap();

        if let Err(e) = stdin.read_line(&mut input) {
            eprintln!("failed to read input");
            eprintln!("{e}");
            println!();
            continue;
        }

        let trimmed_input = input.trim();
        if trimmed_input == ":q" || trimmed_input == "quite" || trimmed_input == "exit" {
            break;
        }

        match parse(trimmed_input) {
            Ok(()) => {}
            Err(e) => eprintln!("Error: {e}"),
        }
    }
}
