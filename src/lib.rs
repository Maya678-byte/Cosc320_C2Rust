// Global state used by tokenizer and parser
pub static mut P: *mut i8 = std::ptr::null_mut(); // Current position in source
pub static mut LINE: i32 = 1; // Source line number
pub static mut TK: i32 = 0; // Current token
pub static mut IVAL: i32 = 0; // Integer literal value
pub static mut CH: i32 = 0; // Current character

// Token enum with exact integer values as in C4 (starting at 128)
#[repr(i32)]
#[derive(Debug, PartialEq, Clone, Copy)]
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
}

// Helper to advance the current character
pub unsafe fn next_char() {
    CH = *P as i32;
    P = P.add(1);
}

// Core tokenizer function, sets TK, IVAL, etc.
pub unsafe fn next_token() {
    // Skip whitespace
    while CH == b' ' as i32 || CH == b'\t' as i32 || CH == b'\r' as i32 || CH == b'\n' as i32 {
        if CH == b'\n' as i32 {
            LINE += 1;
        }
        next_char();
    }

    // Identifier or keyword
    if (CH >= b'a' as i32 && CH <= b'z' as i32) || (CH >= b'A' as i32 && CH <= b'Z' as i32) || CH == b'_' as i32 {
        let mut start = P.offset(-1); // include first char
        let mut len = 1;

        while (*P >= b'a' as i8 && *P <= b'z' as i8) || (*P >= b'A' as i8 && *P <= b'Z' as i8) || (*P >= b'0' as i8 && *P <= b'9' as i8) || *P == b'_' as i8 {
            P = P.add(1);
            len += 1;
        }

        let ident = std::slice::from_raw_parts(start as *const u8, len as usize);

        TK = match ident {
            b"return" => Token::Return as i32,
            b"if" => Token::If as i32,
            b"else" => Token::Else as i32,
            b"while" => Token::While as i32,
            b"enum" => Token::Enum as i32,
            b"int" => Token::Int as i32,
            b"sizeof" => Token::Sizeof as i32,
            _ => Token::Id as i32,
        };
        return;
    }

    // Numeric literal
    if CH >= b'0' as i32 && CH <= b'9' as i32 {
        TK = Token::Num as i32;
        IVAL = CH - b'0' as i32;
        next_char();

        while CH >= b'0' as i32 && CH <= b'9' as i32 {
            IVAL = IVAL * 10 + (CH - b'0' as i32);
            next_char();
        }
        return;
    }

    // Character literal: e.g., 'a'
    if CH == b'\'' as i32 {
        next_char();
        IVAL = CH;
        next_char(); // skip char
        if CH == b'\'' as i32 {
            next_char();
        }
        TK = Token::Char as i32;
        return;
    }

    // Operators and punctuation (examples)
    if CH == b'=' as i32 {
        next_char();
        if CH == b'=' as i32 {
            next_char();
            TK = Token::Eq as i32;
        } else {
            TK = Token::Assign as i32;
        }
        return;
    }

    // Single-character tokens (default)
    TK = CH;
    next_char();
}
