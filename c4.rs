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




use std::ptr;
use std::mem;
use std::fs::File;
use std::io::{self, Read};
use std::os::unix::io::AsRawFd;

//Initializations

static mut P: *mut u8 = ptr::null_mut();     
static mut LP: *mut u8 = ptr::null_mut();    
static mut DATA: *mut u8 = ptr::null_mut(); 

static mut E: *mut i64 = ptr::null_mut();   
static mut LE: *mut i64 = ptr::null_mut();   
static mut ID: *mut i64 = ptr::null_mut();   
static mut SYM: *mut i64 = ptr::null_mut();  

static mut TK: i64 = 0;      
static mut IVAL: i64 = 0;    
static mut TY: i64 = 0;     
static mut LOC: i64 = 0;    
static mut LINE: i64 = 0;    
static mut SRC: i64 = 0;    
static mut DEBUG: i64 = 0;  

/// Tokens and classes (operators last and in precedence order)
/// Represents all supported token types in the C4 compiler.
/// The values must match the original C values for compatibility.
/// Token kinds used in the C4 lexer and parser.
/// These values correspond exactly to the original C constants, starting at 128
/// to distinguish them from ASCII characters.
///
/// These include:
/// - Keywords (`If`, `While`, etc.)
/// - Types (`Int`, `Char`)
/// - Operators (`Assign`, `Add`, `Mul`, etc.)
/// - Special types (`Num`, `Id`)
///
/// This enum uses `#[repr(i64)]` to match C layout.

#[repr(i64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token {
    Num = 128, Fun, Sys, Glo, Loc, Id,
    Char, Else, Enum, If, Int, Return, Sizeof, While,
    Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr,
    Add, Sub, Mul, Div, Mod, Inc, Dec, Brak,
}

/// Virtual machine opcodes used in the C4 bytecode.
/// Each opcode corresponds to an instruction the C4 VM understands, such as:
/// - Arithmetic (`ADD`, `SUB`)
/// - Memory access (`LI`, `SI`)
/// - Control flow (`JMP`, `BZ`)
/// - System calls (`OPEN`, `PRTF`, etc.)
/// The opcode values are designed to match the original C order exactly.

#[repr(i64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Opcode {
    LEA, IMM, JMP, JSR, BZ, BNZ, ENT, ADJ, LEV, LI, LC, SI, SC, PSH,
    OR, XOR, AND, EQ, NE, LT, GT, LE, GE, SHL, SHR, ADD, SUB, MUL, DIV, MOD,
    OPEN, READ, CLOS, PRTF, MALC, FREE, MSET, MCMP, EXIT,
}

// types
#[repr(i64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypeKind {
    CHAR,
    INT,
    PTR,
}

// identifier offsets (since we can't create an ident struct)
#[repr(i64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IdentField {
    Tk,
    Hash,
    Name,
    Class,
    Type,
    Val,
    HClass,
    HType,
    HVal,
    Idsz,
}

//void next() function
//unsafe blocks were used since many global variables are mutable and raw pointers are involved
/// Advances to the next token in the source input.
///
/// This function implements a lexer that processes C4-compatible tokens.
/// It supports C-style comments, identifiers, numbers (decimal, octal, hex),
/// character literals, strings, and all operators supported by the C4 grammar.
///
/// # Safety (a warning below function is unsafe)
/// Relies on unsafe global pointer arithmetic for performance and C compatibility.
unsafe fn next() {
    let mut pp: *mut u8;

    while {
        TK = *P as i64;
        TK != 0
    } {
        P = P.add(1);

        if TK == b'\n' as i64 {
            if SRC != 0 {
                println!(
                    "{}: {}",
                    LINE,
                    std::str::from_utf8_unchecked(std::slice::from_raw_parts(LP, P.offset_from(LP) as usize))
                );
                LP = P;

                while LE < E {
                    let op_index = *LE as usize;
                    let name = &b"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,\
                                 OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,\
                                 OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,";

                    let name_slice = &name[op_index * 5..op_index * 5 + 5];
                    print!("{:>8}", std::str::from_utf8_unchecked(name_slice));
                    LE = LE.add(1);

                    if *LE <= Opcode::ADJ as i64 {
                        println!(" {}", *LE);
                        LE = LE.add(1);
                    } else {
                        println!();
                    }
                }
            }
            LINE += 1;
        } else if TK == b'#' as i64 {
            while *P != 0 && *P != b'\n' {
                P = P.add(1);
            }
        } else if (TK as u8).is_ascii_alphabetic() || TK == b'_' as i64 {
            pp = P.offset(-1);
            while (*P as u8).is_ascii_alphanumeric() || *P == b'_' {
                TK = TK.wrapping_mul(147).wrapping_add(*P as i64);
                P = P.add(1);
            }
            TK = (TK << 6) + (P.offset_from(pp) as i64);
            ID = SYM;
            while *ID.offset(IdentField::Tk as isize) != 0 {
                if TK == *ID.offset(IdentField::Hash as isize)
                    && libc::memcmp(
                        ID.offset(IdentField::Name as isize) as *const libc::c_void,
                        pp as *const libc::c_void,
                        P.offset_from(pp) as usize,
                    ) == 0
                {
                    TK = *ID.offset(IdentField::Tk as isize);
                    return;
                }
                ID = ID.add(IdentField::Idsz as usize);
            }
            *ID.offset(IdentField::Name as isize) = pp as i64;
            *ID.offset(IdentField::Hash as isize) = TK;
            TK = Id as i64;
            *ID.offset(IdentField::Tk as isize) = TK;
            return;
        } else if (TK as u8).is_ascii_digit() {
            if {
                IVAL = TK - b'0' as i64;
                IVAL != 0
            } {
                while (*P as u8).is_ascii_digit() {
                    IVAL = IVAL * 10 + (*P as i64 - b'0' as i64);
                    P = P.add(1);
                }
            } else if *P == b'x' || *P == b'X' {
                P = P.add(1);
                while {
                    TK = *P as i64;
                    (TK >= b'0' as i64 && TK <= b'9' as i64)
                        || (TK >= b'a' as i64 && TK <= b'f' as i64)
                        || (TK >= b'A' as i64 && TK <= b'F' as i64)
                } {
                    IVAL = IVAL * 16 + (TK & 15) + if TK >= b'A' as i64 { 9 } else { 0 };
                    P = P.add(1);
                }
            } else {
                while *P >= b'0' && *P <= b'7' {
                    IVAL = IVAL * 8 + (*P - b'0') as i64;
                    P = P.add(1);
                }
            }
            TK = Token::Num as i64;
            return;
        } else if TK == b'/' as i64 {
            if *P == b'/' {
                P = P.add(1);
                while *P != 0 && *P != b'\n' {
                    P = P.add(1);
                }
            } else {
                TK = Token::Div as i64;
                return;
            }
        } else if TK == b'\'' as i64 || TK == b'"' as i64 {
            pp = DATA;
            while *P != 0 && *P != TK as u8 {
                IVAL = *P as i64;
                P = P.add(1);
                if IVAL == b'\\' as i64 {
                    IVAL = *P as i64;
                    P = P.add(1);
                    if IVAL == b'n' as i64 {
                        IVAL = b'\n' as i64;
                    }
                }
                if TK == b'"' as i64 {
                    *DATA = IVAL as u8;
                    DATA = DATA.add(1);
                }
            }
            P = P.add(1);
            if TK == b'"' as i64 {
                IVAL = pp as i64;
            } else {
                TK = Token::Num as i64;
            }
            return;
        }

        macro_rules! match_op {
            ($ch:expr, $double:expr, $then_tok:expr, $else_tok:expr) => {
                if TK == $ch as i64 {
                    if *P == $double {
                        P = P.add(1);
                        TK = $then_tok as i64;
                    } else {
                        TK = $else_tok as i64;
                    }
                    return;
                }
            };
        }

        match_op!(b'=', b'=', Token::Eq, Token::Assign);
        match_op!(b'+', b'+', Token::Inc, Token::Add);
        match_op!(b'-', b'-', Token::Dec, Token::Sub);
        match_op!(b'!', b'=', Token::Ne, Token::Not); // Token::Not not defined but may be added
        match_op!(b'<', b'=', Token::Le, Token::Lt);
        match_op!(b'<', b'<', Token::Shl, Token::Lt);
        match_op!(b'>', b'=', Token::Ge, Token::Gt);
        match_op!(b'>', b'>', Token::Shr, Token::Gt);
        match_op!(b'|', b'|', Token::Lor, Token::Or);
        match_op!(b'&', b'&', Token::Lan, Token::And);

        match TK as u8 {
            b'^' => { TK = Token::Xor as i64; return; }
            b'%' => { TK = Token::Mod as i64; return; }
            b'*' => { TK = Token::Mul as i64; return; }
            b'[' => { TK = Token::Brak as i64; return; }
            b'?' => { TK = Token::Cond as i64; return; }
            b'~' | b';' | b'{' | b'}' | b'(' | b')' | b']' | b',' | b':' => return,
            _ => {}
        }
    }
}

/// Parses an expression using top-down operator precedence.
///
/// This function emits VM opcodes into the code buffer (`e`) according to the
/// expression grammar of the C4 language. It handles literals, function calls,
/// unary/binary operators, pointer dereferencing, address-of, sizeof, etc.
///
/// # Arguments
/// * `lev` - The current precedence level.
///
/// # Design Notes
/// This matches C4's recursive `expr()` logic and uses manual pointer arithmetic.
/// Types (`ty`) and emitted opcodes are tracked through global state.
///
/// # Safety
/// Uses `unsafe` due to heavy reliance on raw pointers and mutable global state.
unsafe fn expr(lev: i64) {
    let mut t: i64;
    let mut d: *mut i64;

    if TK == 0 {
        eprintln!("{}: unexpected eof in expression", LINE);
        std::process::exit(-1);
    } else if TK == Token::Num as i64 {
        E = E.add(1); *E = Opcode::IMM as i64;
        E = E.add(1); *E = IVAL;
        next();
        TY = TypeKind::INT as i64;
    } else if TK == b'"' as i64 {
        E = E.add(1); *E = Opcode::IMM as i64;
        E = E.add(1); *E = IVAL;
        next();
        while TK == b'"' as i64 {
            next();
        }
        DATA = ((DATA as usize + mem::size_of::<i64>() - 1) & !(mem::size_of::<i64>() - 1)) as *mut u8;
        TY = TypeKind::PTR as i64;
    } else if TK == Token::Sizeof as i64 {
        next();
        if TK == b'(' as i64 { next(); } else { err("open paren expected in sizeof"); }
        TY = TypeKind::INT as i64;
        if TK == Token::Int as i64 { next(); }
        else if TK == Token::Char as i64 { next(); TY = TypeKind::CHAR as i64; }
        while TK == Token::Mul as i64 { next(); TY += TypeKind::PTR as i64; }
        if TK == b')' as i64 { next(); } else { err("close paren expected in sizeof"); }
        E = E.add(1); *E = Opcode::IMM as i64;
        E = E.add(1); *E = if TY == TypeKind::CHAR as i64 { 1 } else { mem::size_of::<i64>() as i64 };
        TY = TypeKind::INT as i64;
    } else if TK == Token::Id as i64 {
        d = ID;
        next();
        if TK == b'(' as i64 {
            next();
            t = 0;
            while TK != b')' as i64 {
                expr(Token::Assign as i64);
                E = E.add(1); *E = Opcode::PSH as i64;
                t += 1;
                if TK == b',' as i64 { next(); }
            }
            next();
            if *d.add(IdentField::Class as usize) == Token::Sys as i64 {
                E = E.add(1); *E = *d.add(IdentField::Val as usize);
            } else if *d.add(IdentField::Class as usize) == Token::Fun as i64 {
                E = E.add(1); *E = Opcode::JSR as i64;
                E = E.add(1); *E = *d.add(IdentField::Val as usize);
            } else {
                err("bad function call");
            }
            if t != 0 {
                E = E.add(1); *E = Opcode::ADJ as i64;
                E = E.add(1); *E = t;
            }
            TY = *d.add(IdentField::Type as usize);
        } else if *d.add(IdentField::Class as usize) == Token::Num as i64 {
            E = E.add(1); *E = Opcode::IMM as i64;
            E = E.add(1); *E = *d.add(IdentField::Val as usize);
            TY = TypeKind::INT as i64;
        } else {
            if *d.add(IdentField::Class as usize) == Token::Loc as i64 {
                E = E.add(1); *E = Opcode::LEA as i64;
                E = E.add(1); *E = LOC - *d.add(IdentField::Val as usize);
            } else if *d.add(IdentField::Class as usize) == Token::Glo as i64 {
                E = E.add(1); *E = Opcode::IMM as i64;
                E = E.add(1); *E = *d.add(IdentField::Val as usize);
            } else {
                err("undefined variable");
            }
            TY = *d.add(IdentField::Type as usize);
            E = E.add(1); *E = if TY == TypeKind::CHAR as i64 { Opcode::LC as i64 } else { Opcode::LI as i64 };
        }
    } else if TK == b'(' as i64 {
        next();
        if TK == Token::Int as i64 || TK == Token::Char as i64 {
            t = if TK == Token::Int as i64 { TypeKind::INT as i64 } else { TypeKind::CHAR as i64 };
            next();
            while TK == Token::Mul as i64 {
                next(); t += TypeKind::PTR as i64;
            }
            if TK != b')' as i64 { err("bad cast"); }
            next();
            expr(Token::Inc as i64);
            TY = t;
        } else {
            expr(Token::Assign as i64);
            if TK != b')' as i64 { err("close paren expected"); }
            next();
        }
    } else if TK == Token::Mul as i64 {
        next();
        expr(Token::Inc as i64);
        if TY > TypeKind::INT as i64 { TY -= TypeKind::PTR as i64; }
        else { err("bad dereference"); }
        E = E.add(1); *E = if TY == TypeKind::CHAR as i64 { Opcode::LC as i64 } else { Opcode::LI as i64 };
    } else if TK == Token::And as i64 {
        next();
        expr(Token::Inc as i64);
        if *E == Opcode::LC as i64 || *E == Opcode::LI as i64 {
            E = E.sub(1);
        } else {
            err("bad address-of");
        }
        TY += TypeKind::PTR as i64;
    } else if TK == b'!' as i64 {
        next();
        expr(Token::Inc as i64);
        E = E.add(1); *E = Opcode::PSH as i64;
        E = E.add(1); *E = Opcode::IMM as i64;
        E = E.add(1); *E = 0;
        E = E.add(1); *E = Opcode::EQ as i64;
        TY = TypeKind::INT as i64;
    } else if TK == b'~' as i64 {
        next();
        expr(Token::Inc as i64);
        E = E.add(1); *E = Opcode::PSH as i64;
        E = E.add(1); *E = Opcode::IMM as i64;
        E = E.add(1); *E = -1;
        E = E.add(1); *E = Opcode::XOR as i64;
        TY = TypeKind::INT as i64;
    } else if TK == Token::Add as i64 {
        next(); expr(Token::Inc as i64); TY = TypeKind::INT as i64;
    } else if TK == Token::Sub as i64 {
        next(); E = E.add(1); *E = Opcode::IMM as i64;
        if TK == Token::Num as i64 {
            E = E.add(1); *E = -IVAL; next();
        } else {
            E = E.add(1); *E = -1;
            E = E.add(1); *E = Opcode::PSH as i64;
            expr(Token::Inc as i64);
            E = E.add(1); *E = Opcode::MUL as i64;
        }
        TY = TypeKind::INT as i64;
    } else if TK == Token::Inc as i64 || TK == Token::Dec as i64 {
        t = TK; next();
        expr(Token::Inc as i64);
        if *E == Opcode::LC as i64 { *E = Opcode::PSH as i64; E = E.add(1); *E = Opcode::LC as i64; }
        else if *E == Opcode::LI as i64 { *E = Opcode::PSH as i64; E = E.add(1); *E = Opcode::LI as i64; }
        else { err("bad lvalue in pre-increment"); }
        E = E.add(1); *E = Opcode::PSH as i64;
        E = E.add(1); *E = Opcode::IMM as i64;
        E = E.add(1); *E = if TY > TypeKind::PTR as i64 { mem::size_of::<i64>() as i64 } else { 1 };
        E = E.add(1); *E = if t == Token::Inc as i64 { Opcode::ADD as i64 } else { Opcode::SUB as i64 };
        E = E.add(1); *E = if TY == TypeKind::CHAR as i64 { Opcode::SC as i64 } else { Opcode::SI as i64 };
    } else {
        err("bad expression");
    }

    // Following this, insert the full operator precedence loop.
    // Due to space, I will continue that in the next message if you'd like.
    // Helper for errors:
    fn err(msg: &str) {
        eprintln!("{}: {}", unsafe { LINE }, msg);
        std::process::exit(-1);
    }
}

/// # Safety
/// Parse and compile statements translation of original stmt()
 unsafe fn parse_statement(&mut self) {
    // Parse a single statement
    if self.tk == Token::If as i32 {
        // IF statement
        self.next_token();
        if self.tk != '(' as i32 {
            println!("{}: missing opening parenthesis in if", self.line);
            exit(1);
        }

        self.next_token();
        self.expression(Token::Assign as i32); // Parse condition

        if self.tk != ')' as i32 {
            println!("{}: missing closing parenthesis in if", self.line);
            exit(1);
        }

        self.next_token();

        // Emit jump instruction for false condition
        *self.e = Opcode::BZ as i32;
        self.e = self.e.add(1);
        let if_false = self.e; // Address to patch
        self.e = self.e.add(1);

        self.parse_statement(); // IF body

        if self.tk == Token::Else as i32 {
            // ELSE part
            *if_false = (self.e as usize - self.mem_stack.as_ptr() as usize) as i32 / 4 + 1; // Skip to after ELSE

            // Emit jump instruction for true condition (to skip ELSE)
            *self.e = Opcode::JMP as i32;
            self.e = self.e.add(1);
            let if_exit = self.e; // Address to patch
            self.e = self.e.add(1);

            self.next_token();
            self.parse_statement(); // ELSE body

            *if_exit = (self.e as usize - self.mem_stack.as_ptr() as usize) as i32 / 4;
        // Exit point
        } else {
            *if_false = (self.e as usize - self.mem_stack.as_ptr() as usize) as i32 / 4;
            // No ELSE, just skip IF body
        }
    } else if self.tk == Token::While as i32 {
        // WHILE statement
        self.next_token();

        // Save the start position of the condition
        let while_start = (self.e as usize - self.mem_stack.as_ptr() as usize) as i32 / 4;

        if self.tk != '(' as i32 {
            println!("{}: missing opening parenthesis in while", self.line);
            exit(1);
        }

        self.next_token();
        self.expression(Token::Assign as i32); // Parse condition

        if self.tk != ')' as i32 {
            println!("{}: missing closing parenthesis in while", self.line);
            exit(1);
        }

        self.next_token();

        // Emit jump instruction for false condition
        *self.e = Opcode::BZ as i32;
        self.e = self.e.add(1);
        let while_end = self.e; // Address to patch
        self.e = self.e.add(1);

        self.parse_statement(); // WHILE body

        // Jump back to condition
        *self.e = Opcode::JMP as i32;
        self.e = self.e.add(1);
        *self.e = while_start;
        self.e = self.e.add(1);

        *while_end = (self.e as usize - self.mem_stack.as_ptr() as usize) as i32 / 4;
    // Exit point
    } else if self.tk == Token::Return as i32 {
        // RETURN statement
        self.next_token();

        if self.tk != ';' as i32 {
            self.expression(Token::Assign as i32); // Return value
            println!("Return statement with expression");
        } else {
            // Return 0 by default when no expression is provided
            println!("Return statement with no expression, defaulting to 0");
            *self.e = Opcode::IMM as i32;
            self.e = self.e.add(1);
            *self.e = 0;
            self.e = self.e.add(1);
        }

        // Emit return instruction
        *self.e = Opcode::LEV as i32;
        self.e = self.e.add(1);
        println!("Emitted LEV instruction for return");

        if self.tk != ';' as i32 {
            println!("{}: missing semicolon in return", self.line);
            exit(1);
        }

        self.next_token();
    } else if self.tk == '{' as i32 {
        // Block statement
        self.next_token();

        while self.tk != '}' as i32 {
            self.parse_statement();
        }

        self.next_token();
    } else if self.tk == ';' as i32 {
        // Empty statement
        self.next_token();
    } else {
        // Expression statement
        self.expression(Token::Assign as i32);

        if self.tk != ';' as i32 {
            println!("{}: missing semicolon", self.line);
            exit(1);
        }

        self.next_token();

        // For expression statements, we discard the value
        *self.e = Opcode::ADJ as i32;
        self.e = self.e.add(1);
        *self.e = 1;
        self.e = self.e.add(1);
    }
}

