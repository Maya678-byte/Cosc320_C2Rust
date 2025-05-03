# 🦀 c4-rust — A Rust Port of "C in Four Functions"

This is a faithful port of [**c4**](https://github.com/rswier/c4) — a minimalist self-interpreting C compiler by **Robert Swierczek** — rewritten in [Rust](https://www.rust-lang.org). The goal is to replicate the original's compact design and educational value while taking advantage of Rust's safety and performance.

> ✅ **Original Project:** [rswier/c4](https://github.com/rswier/c4)  
> 📚 **Purpose:** Learn compiler internals, VM design, and systems-level programming.

---

## 🔧 Features

- Single-pass compiler and virtual machine
- Supports a small but functional subset of ANSI C
- Fully written in safe/unsafe Rust using raw pointers where required
- Implements core compiler functions: `next()`, `expr()`, `stmt()`, and `main()`

---

## 📋 Supported C Subset

This compiler supports a minimal subset of C, including:
- Basic types: `char`, `int`, and pointers
- Function declarations and calls
- Control structures: `if`/`else`, `while`, `return`
- Basic expressions and operators
- String literals and character constants
- Standard library integration via system calls (e.g., `printf`)

---

## 🚀 How to Run

### 1. 📥 Clone the Repository
### 2. 🛠️ Make sure you have Rust installed
### 3. ▶️ Run the Interpreter with a C File

Place your `.c` file (e.g., `hello.c`) in the project directory and run:

```bash
cargo run --release hello.c
```

### Command-line Options

- `-s`: Print source and assembly code during compilation
- `-d`: Enable debug mode with more detailed trace output

Example with options:
```bash
cargo run --release -- -s -d hello.c
```

---

## 📝 Example

Create a file named `hello.c` with the following content:

```c
#include <stdio.h>

int main()
{
  printf("hello, world\n");
  return 0;
}
```

Then compile and run it:

```bash
cargo run --release hello.c
```

---

## 🧠 How It Works

This compiler follows the same four-function design as the original C4:

1. **`next()`**: Lexer/tokenizer that reads source code and produces tokens
2. **`expr()`**: Expression parser and code generator (uses recursive descent)
3. **`stmt()`**: Statement parser and code generator
4. **`main()`**: Program entry point and VM execution

The compilation process:
1. Source code is tokenized into a sequence of tokens
2. Tokens are parsed into an abstract syntax tree (AST) implicitly
3. Virtual machine code is generated on-the-fly during parsing
4. The generated code is executed by a simple stack-based VM

---

## 🔍 Differences from Original C4

- Uses Rust's safety features where possible while maintaining the original architecture
- Improved error reporting and debugging output
- Memory management using Rust's ownership model with unsafe blocks only where necessary
- More explicit symbol table management

---

