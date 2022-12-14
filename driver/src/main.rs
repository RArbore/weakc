/*
 * This file is part of weakc.
 * weakc is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 * weakc is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License
 * along with weakc. If not, see <https://www.gnu.org/licenses/>.
 */

use std::env;
use std::fs::File;
use std::io::Read;
use std::io::Write;

fn print_help() {
    let text = r#"weakc: A compiler for the weak programming language.

Usage:
    weakc COMMAND [OPTIONS] INPUT

Commands:
    build         Compile the input program
    run           Interpret the input program

Options:
    -h, --help            Display this message
    -o, --output <OUTPUT> Set the output file when building
    -dot-cfg              Output a dot CFG instead of textual IR
"#;
    println!("{}", text);
}

enum Command {
    Build(String, String, bool),
    Run(String),
}

enum Option {
    Output(String),
    DotCFG,
}

fn parse_options(args: &[String]) -> Result<(Vec<Option>, &[String]), String> {
    let mut cursor = 0;
    let mut options = vec![];
    while cursor < args.len() {
        match args[cursor].as_str() {
            "-o" | "--output" => {
                if cursor + 1 == args.len() {
                    Err("ERROR: Used option that expected argument, but no argument provided.")?;
                }
                let output = args[cursor + 1].clone();
                options.push(Option::Output(output));
                cursor += 2;
            }
            "-dot-cfg" => {
                options.push(Option::DotCFG);
                cursor += 1;
            }
            _ => {
                break;
            }
        }
    }
    Ok((options, &args[cursor..]))
}

fn parse_command(args: &[String]) -> Result<(Command, &[String]), String> {
    match args.get(0).ok_or("ERROR: No arguments provided.")?.as_str() {
        "build" => {
            let (options, args) = parse_options(&args[1..])?;
            let input = args
                .get(0)
                .ok_or("ERROR: No input program provided.")?
                .clone();
            if args.len() > 1 {
                Err("ERROR: Provided too many arguments.")?;
            }
            let mut output = String::new();
            let mut dot = false;
            for option in options {
                match option {
                    Option::Output(out) => {
                        output = out;
                    }
                    Option::DotCFG => {
                        dot = true;
                    }
                }
            }
            if output == "" {
                let suffix = ".s";
                output = if input.len() > 2 && &input[input.len() - 2..] == ".w" {
                    String::from(&input[..input.len() - 2]) + suffix
                } else {
                    input.clone() + suffix
                };
            }
            let command = Command::Build(input, output, dot);
            Ok((command, &args[1..]))
        }
        "run" => {
            let (options, args) = parse_options(&args[1..])?;
            let input = args
                .get(0)
                .ok_or("ERROR: No input program provided.")?
                .clone();
            for option in options {
                match option {
                    Option::Output(_) => {
                        Err("ERROR: Unsupported argument for command \"run\".")?;
                    }
                    Option::DotCFG => {
                        Err("ERROR: Unsupported argument for command \"run\".")?;
                    }
                }
            }
            let command = Command::Run(input);
            Ok((command, &args[1..]))
        }
        "-h" | "--help" => Err("")?,
        _ => Err("ERROR: Unsupported command used.")?,
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let command = parse_command(&args[1..]);
    match command {
        Ok((command, _)) => match command {
            Command::Build(input, output, isdot) => {
                let mut file = File::open(input).expect("PANIC: Unable to open input file.");
                let mut program = vec![];
                file.read_to_end(&mut program)
                    .expect("PANIC: Unable to read input file.");
                let bump = bump::BumpAllocator::new();
                let tokens = parse::lex(&program, &bump)
                    .expect("PANIC: Something went wrong during lexing.");
                let (ast, _) = parse::parse_program(&tokens, &bump)
                    .expect("PANIC: Something went wrong during parsing.");
                let typed_program = semant::typecheck_program(ast, &bump)
                    .expect("PANIC: Something went wrong during typechecking.");
                let ir_program = ir::irgen(typed_program, &bump);
                let mut file =
                    File::create(output.clone()).expect("PANIC: Unable to open output file.");
                file.write_all(ir_program.to_string().as_bytes())
                    .expect("PANIC: Unable to write output file.");
                if isdot {
                    for i in 0..ir_program.funcs.len() {
                        let file = File::create(
                            output.clone()
                                + "."
                                + core::str::from_utf8(ir_program.funcs.at(i).name).unwrap()
                                + ".dot",
                        )
                        .expect("PANIC: Unable to open output dot file.");
                        ir::write_dot_graph(&ir_program, file, i as ir::IRFunctionID);
                    }
                }
            }
            Command::Run(input) => {
                let mut file = File::open(input).expect("PANIC: Unable to open input file.");
                let mut program = vec![];
                file.read_to_end(&mut program)
                    .expect("PANIC: Unable to read input file.");
                let bump = bump::BumpAllocator::new();
                let tokens = parse::lex(&program, &bump)
                    .expect("PANIC: Something went wrong during lexing.");
                let (ast, _) = parse::parse_program(&tokens, &bump)
                    .expect("PANIC: Something went wrong during parsing.");
                let context = semant::InterpContext::default();
                semant::eval_program(bump.alloc(ast), context)
                    .expect("PANIC: Something went wrong during interpretation.");
            }
        },
        Err(error) => {
            if error.len() > 0 {
                println!("{}", error);
            }
            print_help();
        }
    }
}
