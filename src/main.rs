/// A Rust translation of the `c4` compiler by Robert Swierczek.
///
/// ## Design Goals
/// - Preserve the original C4 architecture, including tokenization, parsing, and virtual machine execution.
/// - Use `unsafe` code to maintain pointer arithmetic and raw memory handling similar to C.
/// - Match enum values and global layout with C for 1:1 behavior fidelity.
///
/// ## Memory Model
/// - Global state is retained using `static mut` for compatibility with the original design.
/// - Memory for source code, emitted instructions, symbol table, etc., is allocated as contiguous blocks.
/// - Function calls use a simulated stack in raw memory.
///
/// ## Safety Notes
/// - All major components (`next`, `expr`, etc.) are `unsafe` due to raw pointer manipulation.
/// - Proper encapsulation and memory-safe abstractions could be added in a future refactor.




use std::env;
use std::fs;
use std::process::exit;
use std::ptr;
use std::mem;

// Constants
const POOLSZ: usize = 256 * 1024; // Default memory pool size
const NSYMS: usize = 1024;        // Maximum number of symbols

//Initializations - global state
static mut P: *mut i8 = ptr::null_mut();     // Current position in source code
static mut LP: *mut i8 = ptr::null_mut();    // Line position for error reporting
static mut DATA: *mut i8 = ptr::null_mut();  // Data segment

static mut E: *mut i32 = ptr::null_mut();    // Current position in emitted code
static mut LE: *mut i32 = ptr::null_mut();   // Last emitted position for debugging
static mut ID: *mut i32 = ptr::null_mut();   // Current identifier being processed
static mut SYM: *mut i32 = ptr::null_mut();  // Symbol table

static mut TK: i32 = 0;      // Current token
static mut IVAL: i32 = 0;    // Current token value
static mut TY: i32 = 0;      // Current expression type
static mut LOC: i32 = 0;     // Local variable offset
static mut LINE: i32 = 0;    // Current line number
static mut SRC: i32 = 0;     // Print source and assembly flag
static mut DEBUG: i32 = 0;   // Debug flag

/// Tokens and classes (operators last and in precedence order)
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token {
    Num = 128, Fun, Sys, Glo, Loc, Id,
    Char, Else, Enum, If, Int, Return, Sizeof, While,
    Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr,
    Add, Sub, Mul, Div, Mod, Inc, Dec, Brak,
}

/// Virtual machine opcodes
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Opcode {
    LEA, IMM, JMP, JSR, BZ, BNZ, ENT, ADJ, LEV, LI, LC, SI, SC, PSH,
    OR, XOR, AND, EQ, NE, LT, GT, LE, GE, SHL, SHR, ADD, SUB, MUL, DIV, MOD,
    OPEN, READ, CLOS, PRTF, MALC, FREE, MSET, MCMP, EXIT,
}

// Type kinds
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypeKind {
    CHAR = 0,
    INT = 1,
    PTR = 2,
}

// identifier fields (since we can't create an ident struct)
#[repr(i32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IdentField {
    Name = 0,       // Variable name
    Class = 1,      // Token type (Token enum)
    Type = 2,       // Data type (TypeKind enum)
    Val = 3,        // Value
    Idsz = 4,       // Size of identifier record (for advancing pointer)
}

/// Get next character from source
#[inline]
unsafe fn next() -> i8 {
    let ch = *P;
    P = P.add(1);
    ch
}

/// Tokenizer - advance to the next token
unsafe fn next_token() {
    // Skip whitespace
    while *P as u8 == ' ' as u8 || *P as u8 == '\t' as u8 || *P as u8 == '\n' as u8 || *P as u8 == '\r' as u8 {
        if *P as u8 == '\n' as u8 {
            LINE += 1;
        }
        P = P.add(1);
    }
    
    // Record position for line display in debugging
    LP = P;
    
    // Read the next token
    let first_char = *P as u8;
    println!("First character: {}", first_char as char);
    
    if first_char == 0 {
        // End of file
        return;
    } else if first_char == '#' as u8 {
        // Skip preprocessor directive
        println!("Skipping preprocessor directive starting with #");
        while *P != 0 && *P as u8 != '\n' as u8 {
            P = P.add(1);
        }
        println!("Preprocessor directive skipped, next character: {}", *P as u8 as char);
        next_token();
        return;
    } else if (first_char >= 'a' as u8 && first_char <= 'z' as u8) || 
              (first_char >= 'A' as u8 && first_char <= 'Z' as u8) || 
              first_char == '_' as u8 {
        // Parse identifier or keyword
        println!("Parsing identifier starting with: {}", first_char as char);
        parse_identifier();
    } else if first_char >= '0' as u8 && first_char <= '9' as u8 {
        // Parse number
        P = P.add(1);
        parse_number(first_char as i8);
    } else if first_char == '\'' as u8 {
        // Character literal
        P = P.add(1);
        parse_char();
    } else if first_char == '"' as u8 {
        // String literal
        P = P.add(1);
        parse_string();
    } else {
        // Operator or punctuation
        P = P.add(1);
        parse_operator(first_char as i8);
    }
}

/// Parse identifier (variable name or keyword)
unsafe fn parse_identifier() {
    println!("ID buffer initialized, reading remainder of identifier...");
    
    // Build the identifier string
    let mut id_start = P;
    let mut id_len = 0;
    
    while (*P as u8).is_ascii_alphanumeric() || *P as u8 == '_' as u8 {
        P = P.add(1);
        id_len += 1;
    }
    
    // Create a string for the identifier
    let id_string = std::str::from_utf8_unchecked(
        std::slice::from_raw_parts(id_start as *const u8, id_len)
    ).to_string();
    
    // Check if it's a keyword
    println!("Checking if identifier '{}' is a keyword...", id_string);
    
    // Check for keywords
    match id_string.as_str() {
        "char" => { TK = Token::Char as i32; println!("Matched keyword 'char' with token ID: {}", TK); },
        "else" => { TK = Token::Else as i32; println!("Matched keyword 'else' with token ID: {}", TK); },
        "enum" => { TK = Token::Enum as i32; println!("Matched keyword 'enum' with token ID: {}", TK); },
        "if" => { TK = Token::If as i32; println!("Matched keyword 'if' with token ID: {}", TK); },
        "int" => { TK = Token::Int as i32; println!("Matched keyword 'int' with token ID: {}", TK); },
        "return" => { TK = Token::Return as i32; println!("Matched keyword 'return' with token ID: {}", TK); },
        "sizeof" => { TK = Token::Sizeof as i32; println!("Matched keyword 'sizeof' with token ID: {}", TK); },
        "while" => { TK = Token::While as i32; println!("Matched keyword 'while' with token ID: {}", TK); },
        _ => {
            // Not a keyword, look up in symbol table
            println!("Not a keyword, treating as identifier");
            TK = Token::Id as i32;
            identifier_lookup(id_string);
        }
    }
}

/// Look up an identifier in the symbol table
unsafe fn identifier_lookup(id_string: String) {
    println!("Looking up identifier in symbol table");
    println!("Searching for identifier '{}' in symbol table", id_string);
    
    // Debug symbol table state before searching
    println!("Debug: current id pointer: {:p}", ID);
    println!("Debug: start of symbol table: {:p}", SYM);
    println!("Debug: difference: {} bytes", (ID as usize - SYM as usize));
    
    // Iterate through symbol table entries until we reach the current id pointer
    let entries = (ID as usize - SYM as usize) / (IdentField::Idsz as usize * mem::size_of::<i32>());
    println!("Current symbol table contains {} entries", entries);
    
    // Debug: Print symbol table entries
    let mut idx = 0;
    let mut temp = SYM;
    while temp < ID {
        let class = *temp.add(IdentField::Class as usize);
        let mut name_str = String::new();
        let mut i = 0;
        loop {
            let c = *temp.add(IdentField::Name as usize + i);
            if c == 0 { break; }
            name_str.push(c as u8 as char);
            i += 1;
        }
        println!("  Symbol {}: class={}, name={}", idx, class, name_str);
        temp = temp.add(IdentField::Idsz as usize);
        idx += 1;
    }
    
    // TEMP FIX: Special handling for printf
    if id_string == "printf" {
        println!("Special handling for printf - returning direct system call");
        ID = ID.sub(IdentField::Idsz as usize);  // Force reuse of current id slot
        
        // Set up printf properties directly
        for (i, byte) in "printf".bytes().enumerate() {
            *ID.add(IdentField::Name as usize + i) = byte as i32;
        }
        *ID.add(IdentField::Name as usize + "printf".len()) = 0;
        *ID.add(IdentField::Class as usize) = Token::Sys as i32;
        *ID.add(IdentField::Type as usize) = TypeKind::INT as i32;
        *ID.add(IdentField::Val as usize) = Opcode::PRTF as i32;
        
        return;  // Skip normal identifier processing
    }
    
    // Now look for the identifier
    let mut found = false;
    let mut current = SYM;
    while current < ID {
        // Debug print current symbol entry
        let mut name_str = String::new();
        let mut i = 0;
        loop {
            let c = *current.add(IdentField::Name as usize + i);
            if c == 0 { break; }
            name_str.push(c as u8 as char);
            i += 1;
        }
        println!("Checking symbol '{}' against '{}'", name_str, id_string);
        
        if name_str == id_string {
            // Found a match
            found = true;
            ID = current;
            println!("Found identifier '{}' in symbol table", id_string);
            
            // Print identifier class and type for debugging
            let id_class = *ID.add(IdentField::Class as usize);
            let id_type = *ID.add(IdentField::Type as usize);
            let id_val = *ID.add(IdentField::Val as usize);
            println!("Identifier class: {}, type: {}, val: {}", id_class, id_type, id_val);
            break;
        }
        
        current = current.add(IdentField::Idsz as usize);
    }
    
    if !found {
        // For new identifiers, just advance the symbol table
        println!("Creating new identifier entry in symbol table for '{}'", id_string);
        
        // Copy identifier name to the name field (starting at Name)
        for (i, byte) in id_string.bytes().enumerate() {
            *ID.add(IdentField::Name as usize + i) = byte as i32;
        }
        // Null-terminate the name
        *ID.add(IdentField::Name as usize + id_string.len()) = 0;
        
        // Set default class and type
        // Special handling for printf - make sure we recognize it as a system call
        if id_string == "printf" {
            println!("Detected printf identifier - setting as system call");
            *ID.add(IdentField::Class as usize) = Token::Sys as i32;
            *ID.add(IdentField::Type as usize) = TypeKind::INT as i32;
            *ID.add(IdentField::Val as usize) = Opcode::PRTF as i32;
        } else {
            *ID.add(IdentField::Class as usize) = Token::Id as i32;
            *ID.add(IdentField::Type as usize) = TypeKind::INT as i32;
            *ID.add(IdentField::Val as usize) = 0;
        }
        
        // Advance id pointer
        ID = ID.add(IdentField::Idsz as usize);
        println!("New identifier stored in symbol table, id pointer adjusted");
    }
}

/// Parse number
unsafe fn parse_number(mut ch: i8) {
    TK = Token::Num as i32;
    
    // Parse the number including base prefixes
    let mut val = 0i32;
    
    if ch == '0' as i8 {
        ch = next();
        if ch == 'x' as i8 || ch == 'X' as i8 {
            // Hexadecimal
            ch = next();
            while (ch >= '0' as i8 && ch <= '9' as i8) || 
                  (ch >= 'a' as i8 && ch <= 'f' as i8) || 
                  (ch >= 'A' as i8 && ch <= 'F' as i8) {
                val = (val << 4) + if ch <= '9' as i8 { 
                    (ch - '0' as i8) as i32 
                } else if ch <= 'F' as i8 { 
                    (ch - 'A' as i8 + 10) as i32 
                } else { 
                    (ch - 'a' as i8 + 10) as i32 
                };
                ch = next();
            }
        } else {
            // Octal
            while ch >= '0' as i8 && ch <= '7' as i8 {
                val = (val << 3) + (ch - '0' as i8) as i32;
                ch = next();
            }
        }
    } else {
        // Decimal
        val = (ch - '0' as i8) as i32;
        ch = next();
        while ch >= '0' as i8 && ch <= '9' as i8 {
            val = val * 10 + (ch - '0' as i8) as i32;
            ch = next();
        }
    }
    
    P = P.offset(-1);
    IVAL = val;
}

/// Parse character literal
unsafe fn parse_char() {
    TK = Token::Num as i32;
    
    let mut ch = next();
    if ch == '\\' as i8 {
        ch = next();
        if ch == 'n' as i8 {
            ch = '\n' as i8;
        } else if ch == 't' as i8 {
            ch = '\t' as i8;
        } else if ch == 'v' as i8 {
            ch = '\x0B' as i8;
        } else if ch == 'f' as i8 {
            ch = '\x0C' as i8;
        } else if ch == 'r' as i8 {
            ch = '\r' as i8;
        }
    }
    
    IVAL = ch as i32;
    
    if next() != '\'' as i8 {
        println!("{}: unterminated character constant", LINE);
        exit(1);
    }
}

/// Parse string literal
unsafe fn parse_string() {
    // This should store the string in the data segment and return a pointer
    TK = Token::Num as i32;
    
    // Point to the data segment - for offset calculation
    let base_addr = DATA;
    IVAL = (DATA as usize - base_addr as usize) as i32;
    
    let mut ch = next();
    while ch != '"' as i8 && ch != 0 {
        if ch == '\\' as i8 {
            ch = next();
            if ch == 'n' as i8 {
                ch = '\n' as i8;
            } else if ch == 't' as i8 {
                ch = '\t' as i8;
            } else if ch == 'v' as i8 {
                ch = '\x0B' as i8;
            } else if ch == 'f' as i8 {
                ch = '\x0C' as i8;
            } else if ch == 'r' as i8 {
                ch = '\r' as i8;
            }
        }
        
        *DATA = ch;
        DATA = DATA.offset(1);
        ch = next();
    }
    
    if ch != '"' as i8 {
        println!("{}: unterminated string literal", LINE);
        exit(1);
    }
    
    // Null-terminate the string
    *DATA = 0;
    DATA = DATA.offset(1);
}

/// Parse operator
unsafe fn parse_operator(ch: i8) {
    match ch as u8 {
        b'+' => {
            if *P == b'+' as i8 {
                P = P.offset(1);
                TK = Token::Inc as i32;
            } else {
                TK = Token::Add as i32;
            }
        },
        b'-' => {
            if *P == b'-' as i8 {
                P = P.offset(1);
                TK = Token::Dec as i32;
            } else {
                TK = Token::Sub as i32;
            }
        },
        b'*' => { TK = Token::Mul as i32; },
        b'/' => { TK = Token::Div as i32; },
        b'%' => { TK = Token::Mod as i32; },
        b'=' => {
            if *P == b'=' as i8 {
                P = P.offset(1);
                TK = Token::Eq as i32;
            } else {
                TK = Token::Assign as i32;
            }
        },
        b'!' => {
            if *P == b'=' as i8 {
                P = P.offset(1);
                TK = Token::Ne as i32;
            } else {
                TK = b'!' as i32;
            }
        },
        b'<' => {
            if *P == b'=' as i8 {
                P = P.offset(1);
                TK = Token::Le as i32;
            } else if *P == b'<' as i8 {
                P = P.offset(1);
                TK = Token::Shl as i32;
            } else {
                TK = Token::Lt as i32;
            }
        },
        b'>' => {
            if *P == b'=' as i8 {
                P = P.offset(1);
                TK = Token::Ge as i32;
            } else if *P == b'>' as i8 {
                P = P.offset(1);
                TK = Token::Shr as i32;
            } else {
                TK = Token::Gt as i32;
            }
        },
        b'|' => {
            if *P == b'|' as i8 {
                P = P.offset(1);
                TK = Token::Lor as i32;
            } else {
                TK = Token::Or as i32;
            }
        },
        b'&' => {
            if *P == b'&' as i8 {
                P = P.offset(1);
                TK = Token::Lan as i32;
            } else {
                TK = Token::And as i32;
            }
        },
        b'^' => { TK = Token::Xor as i32; },
        b'?' => { TK = Token::Cond as i32; },
        b'[' => { TK = Token::Brak as i32; },
        _ => { TK = ch as i32; }
    }
}

/// Parse and compile expressions
unsafe fn expression(level: i32) {
    println!("Parsing expression at level: {}", level);
    
    // Handle expressions based on token type and precedence level
    match TK {
        // Handle identifiers (variables and function calls)
        tk if tk == Token::Id as i32 => {
            println!("Expression: Identifier detected");
            
            // Check if this identifier is a function call
            let id_class = *ID.add(IdentField::Class as usize);
            let id_type = *ID.add(IdentField::Type as usize);
            let id_val = *ID.add(IdentField::Val as usize);
            
            println!("Identifier class: {}, type: {}, val: {}", id_class, id_type, id_val);
            next_token();
            
            // Handle function call
            if TK == '(' as i32 {
                println!("Function call detected");
                
                // Verify this is a function or system call
                if id_class == Token::Fun as i32 || id_class == Token::Sys as i32 {
                    println!("Valid function call: {}", if id_class == Token::Fun as i32 { "user-defined" } else { "system" });
                    
                    // Process function call arguments
                    let mut args = 0;
                    next_token();
                    
                    // Parse arguments in the function call
                    while TK != ')' as i32 {
                        expression(Token::Assign as i32);
                        args += 1;
                        
                        if TK == ',' as i32 {
                            next_token();
                        }
                    }
                    
                    // Check for closing parenthesis
                    if TK != ')' as i32 {
                        println!("{}: missing closing parenthesis", LINE);
                        exit(1);
                    }
                    
                    next_token();
                    
                    // Generate function call code
                    if id_class == Token::Sys as i32 {
                        // System call
                        println!("Generating system call instruction: {}", id_val);
                        *E = id_val; // Opcode for the system call
                        E = E.add(1);
                    } else {
                        // User-defined function
                        *E = Opcode::JSR as i32;
                        E = E.add(1);
                        *E = id_val;
                        E = E.add(1);
                    }
                    
                    // Clean up stack after function call if needed
                    if args > 0 {
                        *E = Opcode::ADJ as i32;
                        E = E.add(1);
                        *E = args;
                        E = E.add(1);
                    }
                } else {
                    println!("{}: call to non-function", LINE);
                    exit(1);
                }
            }
        },
        
        // Handle numeric literals
        tk if tk == Token::Num as i32 => {
            // Emit the numeric literal value
            *E = Opcode::IMM as i32;
            E = E.add(1);
            *E = IVAL;
            E = E.add(1);
            
            // Move to next token
            next_token();
        },
        
        // Other token types...
        _ => {
            println!("Expression: unsupported token type {}", TK);
            next_token();
        }
    }
}

/// Parse and compile statements
unsafe fn parse_statement() {
    // Parse a single statement
    if TK == Token::If as i32 {
        // IF statement
        next_token();
        if TK != '(' as i32 {
            println!("{}: missing opening parenthesis in if", LINE);
            exit(1);
        }
        
        next_token();
        expression(Token::Assign as i32); // Parse condition
        
        if TK != ')' as i32 {
            println!("{}: missing closing parenthesis in if", LINE);
            exit(1);
        }
        
        next_token();
        
        // Emit jump instruction for false condition
        *E = Opcode::BZ as i32;
        E = E.add(1);
        let if_false = E; // Address to patch
        E = E.add(1);
        
        parse_statement(); // IF body
        
        if TK == Token::Else as i32 {
            // ELSE part - Patching requires the base address of the stack in C4
            // In Rust translation we'll use a placeholder memory offset calculation
            *if_false = ((E as usize) / mem::size_of::<i32>() + 1) as i32; // Skip to after ELSE
            
            // Emit jump instruction for true condition (to skip ELSE)
            *E = Opcode::JMP as i32;
            E = E.add(1);
            let if_exit = E; // Address to patch
            E = E.add(1);
            
            next_token();
            parse_statement(); // ELSE body
            
            // Set exit point
            *if_exit = ((E as usize) / mem::size_of::<i32>()) as i32;
        } else {
            // No ELSE clause
            *if_false = ((E as usize) / mem::size_of::<i32>()) as i32;
        }
    } else if TK == Token::While as i32 {
        // WHILE statement implementation
        next_token();
        
        // Save the start position of the condition
        let while_start = ((E as usize) / mem::size_of::<i32>()) as i32;
        
        if TK != '(' as i32 {
            println!("{}: missing opening parenthesis in while", LINE);
            exit(1);
        }
        
        next_token();
        expression(Token::Assign as i32); // Parse condition
        
        if TK != ')' as i32 {
            println!("{}: missing closing parenthesis in while", LINE);
            exit(1);
        }
        
        next_token();
        
        // Emit jump instruction for false condition
        *E = Opcode::BZ as i32;
        E = E.add(1);
        let while_end = E; // Address to patch
        E = E.add(1);
        
        parse_statement(); // WHILE body
        
        // Jump back to condition
        *E = Opcode::JMP as i32;
        E = E.add(1);
        *E = while_start;
        E = E.add(1);
        
        // Set exit point
        *while_end = ((E as usize) / mem::size_of::<i32>()) as i32;
    }
    // Additional statement types would be implemented here
}

/// Initialize symbol table and global state
unsafe fn init() {
    // Allocate memory for the segments
    let mut mem_src = vec![0i8; POOLSZ];
    let mut mem_data = vec![0i8; POOLSZ];
    let mut mem_stack = vec![0i32; POOLSZ];
    
    // Set up the pointers
    P = mem_src.as_mut_ptr();
    LP = P;
    LINE = 1;
    
    DATA = mem_data.as_mut_ptr();
    
    E = mem_stack.as_mut_ptr();
    LE = E;
    
    // Initialize symbol table
    SYM = E;
    ID = SYM;
    
    println!("Setting up symbol table...");
    
    // Initialize library functions
    println!("Initializing library functions in symbol table");
    
    // Define printf
    println!("Setting up printf() as a system function");
    
    // Set up printf as a system call
    for (i, byte) in "printf".bytes().enumerate() {
        *ID.add(IdentField::Name as usize + i) = byte as i32;
    }
    *ID.add(IdentField::Name as usize + "printf".len()) = 0;
    *ID.add(IdentField::Class as usize) = Token::Sys as i32;
    *ID.add(IdentField::Type as usize) = TypeKind::INT as i32;
    *ID.add(IdentField::Val as usize) = Opcode::PRTF as i32;
    
    // Advance ID pointer past printf
    ID = ID.add(IdentField::Idsz as usize);
    
    println!("Library function initialization complete");
    
    // Add main function stub to symbol table
    println!("Adding 'main' function to symbol table");
    
    // Set up main function entry
    for (i, byte) in "main".bytes().enumerate() {
        *ID.add(IdentField::Name as usize + i) = byte as i32;
    }
    *ID.add(IdentField::Name as usize + "main".len()) = 0;
    *ID.add(IdentField::Class as usize) = Token::Fun as i32;
    *ID.add(IdentField::Type as usize) = TypeKind::INT as i32;
    *ID.add(IdentField::Val as usize) = 0;  // Entry point (to be set later)
    
    // Advance ID pointer past main
    ID = ID.add(IdentField::Idsz as usize);
    
    // We need to keep the vectors from being dropped to maintain valid pointers
    mem::forget(mem_src);
    mem::forget(mem_data);
    mem::forget(mem_stack);
}

/// Main compiler function
unsafe fn compile() -> i32 {
    // Start parsing
    next_token();
    
    while TK != 0 {
        parse_statement();
    }
    
    // Return the entry point
    0 // Placeholder
}

fn main() {
    println!("Starting compiler run...");
    
    // Get command line arguments
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        eprintln!("usage: c4_rust [-s] [-d] file...");
        exit(1);
    }
    
    // Parse command line options
    let mut src_flag = 0;      // print source flag
    let mut debug_flag = 0;    // debug flag
    let mut arg_index = 1;     // current argument index
    
    // Check for options
    while arg_index < args.len() && args[arg_index].starts_with('-') {
        if args[arg_index] == "-s" {
            src_flag = 1;
        } else if args[arg_index] == "-d" {
            debug_flag = 1;
        } else {
            eprintln!("unknown option: {}", args[arg_index]);
            exit(1);
        }
        arg_index += 1;
    }
    
    // Must have a source file
    if arg_index >= args.len() {
        eprintln!("no input file");
        exit(1);
    }
    
    // Read source file
    let source_path = &args[arg_index];
    let source_code = match fs::read_to_string(source_path) {
        Ok(code) => code,
        Err(err) => {
            eprintln!("could not read {}: {}", source_path, err);
            exit(1);
        }
    };
    
    println!("Source code loaded successfully. Length: {}", source_code.len());
    
    unsafe {
        // Set global state
        SRC = src_flag;
        DEBUG = debug_flag;
        
        // Copy source to memory (will be done in init())
        init();
        
        // Copy source to input buffer
        for (i, byte) in source_code.bytes().enumerate() {
            *P.add(i) = byte as i8;
        }
        
        // Reset P to beginning of source
        P = P.sub(0);
        
        // Compile
        let entry = compile();
        
        // Run if compilation successful
        if entry != 0 {
            // run() would be implemented here
        }
    }
}



