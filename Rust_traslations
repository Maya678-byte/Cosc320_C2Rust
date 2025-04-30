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

// tokens and classes (operators last and in precedence order)
#[repr(i64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token {
    Num = 128, Fun, Sys, Glo, Loc, Id,
    Char, Else, Enum, If, Int, Return, Sizeof, While,
    Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr,
    Add, Sub, Mul, Div, Mod, Inc, Dec, Brak,
}

// opcodes
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


