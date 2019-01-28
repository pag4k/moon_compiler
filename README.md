# Moon-Compiler
Compiler for the Moon Processor (COMP442)
Pierre-André Gagnon - 40067198
Assignment 1 - January 28, 2019

## Execution instructions
- Go to (https://rustup.rs/)[https://rustup.rs/] and follow the instruction to install rustup, an installer for
the systems programming language Rust.
- Run rustup to install all the needed elements to be able to build a Rust program.
- Follow the instructions on the terminal and select default installation. All environment variables should be automatically configured, but it might be required to restart the computer.
- On the terminal, go to the root of the provided source files, that is, on the same folder as Cargo.toml and NOT in the src/ folder.
- Run "cargo build" to compile the program and "cargo run" to execute it.
- Command line arguments are not required, but they can be used to change the default source input file, AtoCC ouput file, and the error output file. They must be entered in that order: "Cargo run source.txt atocc.txt error.txt".
