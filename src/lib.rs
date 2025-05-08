pub static mut P: *mut i8 = std::ptr::null_mut();
pub static mut LINE: i32 = 1;
pub static mut TK: i32 = 0;
pub static mut IVAL: i32 = 0;

#[repr(i32)]
#[derive(Debug, PartialEq)]
pub enum Token {
    Num = 128,
    Id,
    Char,
    Else,
    Enum,
    If,
    Int,
    Return,
    Sizeof,
    While,
    Assign,  // '='
    Cond,    // '?'
    Lor,     // '||'
    Lan,     // '&&'
    Or,      // '|'
    Xor,     // '^'
    And,     // '&'
    Eq,      // '=='
    Ne,      // '!='
    Lt,      // '<'
    Gt,      // '>'
    Le,      // '<='
    Ge,      // '>='
    Shl,     // '<<'
    Shr,     // '>>'
    Add,     // '+'
    Sub,     // '-'
    Mul,     // '*'
    Div,     // '/'
    Mod,     // '%'
    Inc,     // '++'
    Dec,     // '--'
    Brak,    // '[]'
    // Add more as needed
}

// Dummy implementation for next_token; replace this with real tokenizer logic.
pub unsafe fn next_token() {
    // This is a stub for demonstration; you'll want to replace it with
    // the real logic from your Rust translation of C4's `next()` function.

    // Example for minimal tokenizer:
    if !P.is_null() && *P != 0 {
        match *P as u8 {
            b' ' | b'\t' | b'\n' | b'\r' => {
                P = P.add(1);
                next_token(); // skip whitespace
            }
            b'0'..=b'9' => {
                TK = Token::Num as i32;
                IVAL = 0;
                while *P >= b'0' as i8 && *P <= b'9' as i8 {
                    IVAL = IVAL * 10 + (*P - b'0' as i8) as i32;
                    P = P.add(1);
                }
            }
            b';' => {
                TK = *P as i32;
                P = P.add(1);
            }
            b'r' => {
                // crude "return" keyword detection
                let keyword = std::slice::from_raw_parts(P as *const u8, 6);
                if keyword.starts_with(b"return") {
                    TK = Token::Return as i32;
                    P = P.add(6);
                } else {
                    TK = Token::Id as i32;
                    P = P.add(1);
                }
            }
            _ => {
                TK = *P as i32;
                P = P.add(1);
            }
        }
    } else {
        TK = 0; // EOF
    }
}
