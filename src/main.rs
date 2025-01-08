#![allow(dead_code)]

use clap::{command, Parser, Subcommand};
use loxide::{interpret, lexer::tokenize};
use miette::{Context, IntoDiagnostic};

use std::{
    fs,
    io::{self, stdout, Write},
    path::PathBuf,
};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Tokenize { filename: PathBuf },
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

        match interpret(trimmed_input) {
            Ok(literal) => println!("{literal}\n"),
            Err(e) => eprintln!("Error: {e}"),
        }
    }
}
