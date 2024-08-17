# my-cc: My C Compiler (educational purposes)

my-cc is a C compiler being implemented in Haskell using the `Writing A C Compiler` book by Nora Sandler.

## Features

- Lexical analysis
- Parsing
- Semantic analysis
- Code generation

## Target Architecture

my-cc generates assembly code for the x86_64 architecture.

## Dependencies

This project relies on gcc or clang for preprocessing and linking stages. Ensure you have either gcc (Linux) or clang (MacOS) installed on your system. Also you need `stack` to build it.

## Usage

`my-cc [OPTIONS] <input_file>`

### Command-line Options

- `--lex <input_file>`: Perform lexical analysis and print tokens
- `--parse <input_file>`: Parse the input and print the AST
- `--validate <input_file>`: Perform semantic analysis and print the resolved AST
- `--codegen <input_file>`: Generate and print intermediate code
- `--tacky <input_file>`: Convert to Three-Address Code (TAC) and print the result
- `-S <input_file>`: Compile to assembly (.s file)
- `<input_file>`: Compile and link to create an executable

If no option is specified, my-cc will compile and link the input file to create an executable.

## Compilation Process

1. Preprocessing: Uses gcc/clang to preprocess the input file
2. Lexical Analysis: Tokenizes the preprocessed input
3. Parsing: Generates an Abstract Syntax Tree (AST)
4. Semantic Analysis: Resolves and validates the AST
5. Intermediate Code Generation: Converts to Three-Address Code (TAC)
6. Code Generation: Produces x86_64 assembly code
7. Assembly: Uses gcc/clang to assemble the generated code (when creating an executable)
8. Linking: Uses gcc/clang to link the object file (when creating an executable)

## Note on System Architecture

The compiler automatically detects the system architecture. On ARM64 systems (like Apple Silicon Macs), it uses `arch -x86_64 gcc` to ensure x86_64 compatibility.

## Building

`stack build` or `stack install`

## License

[LICENSE](LICENSE)