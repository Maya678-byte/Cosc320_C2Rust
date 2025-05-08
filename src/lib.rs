pub static mut P: *mut i8 = std::ptr::null_mut();
pub static mut LINE: i32 = 0;
pub static mut TK: i32 = 0;
pub static mut IVAL: i32 = 0;

#[repr(i32)]
#[derive(Debug, PartialEq)]
pub enum Token {
    Num = 128,
    If = 129,
    Return = 130,

}

pub unsafe fn next_token() {

}
