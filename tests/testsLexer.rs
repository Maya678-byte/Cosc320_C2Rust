use c4_rust_Al_Aweer::*;
#[test]
fn test_lexer_basic_keywords() {
    use std::ptr;
    extern "Rust" {
        static mut P: *mut i8;
        static mut LINE: i32;
        static mut TK: i32;
        static mut IVAL: i32;
        fn next_token();
    }

    unsafe {
        let src = b"return 42;\0";
        P = src.as_ptr() as *mut i8;
        LINE = 1;
        next_token();
        assert_eq!(TK, 139); // Token::Return
        next_token();
        assert_eq!(TK, 128); // Token::Num
        assert_eq!(IVAL, 42);
        next_token();
        assert_eq!(TK, ';' as i32);
    }
}
