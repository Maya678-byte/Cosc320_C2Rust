//Test cases
use c4_rust_Al_Aweer::*;

    #[test]
    fn test_tokenizer_with_simple_if() {
        unsafe {
            let source = b"if (1) return 0;\0";
            P = source.as_ptr() as *mut i8;
            LINE = 1;
            next_token();
            assert_eq!(TK, Token::If as i32);
            next_token();
            assert_eq!(TK, b'(' as i32);
            next_token();
            assert_eq!(TK, Token::Num as i32);
            assert_eq!(IVAL, 1);
            next_token();
            assert_eq!(TK, b')' as i32);
            next_token();
            assert_eq!(TK, Token::Return as i32);
            next_token();
            assert_eq!(TK, Token::Num as i32);
            assert_eq!(IVAL, 0);
            next_token();
            assert_eq!(TK, b';' as i32);
        }
    }

