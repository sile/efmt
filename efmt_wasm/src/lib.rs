#![allow(clippy::not_unsafe_ptr_arg_deref)]

#[unsafe(no_mangle)]
pub fn check(text: *const u8, text_len: i32) -> *mut Vec<u8> {
    let text = unsafe { std::slice::from_raw_parts(text, text_len as usize) };
    let text = std::str::from_utf8(text).unwrap_or_else(|e| panic!("{e}"));
    let result = efmt_core::format_text::<efmt_core::items::ModuleOrConfig>(text);
    if let Err(e) = result {
        Box::into_raw(Box::new(e.to_string().into_bytes()))
    } else {
        Box::into_raw(Box::default())
    }
}

#[unsafe(no_mangle)]
pub fn format(text: *const u8, text_len: i32) -> *mut Vec<u8> {
    let text = unsafe { std::slice::from_raw_parts(text, text_len as usize) };
    let text = std::str::from_utf8(text).unwrap_or_else(|e| panic!("{e}"));
    let formatted = efmt_core::format_text::<efmt_core::items::ModuleOrConfig>(text)
        .unwrap_or_else(|e| panic!("{e}"));
    Box::into_raw(Box::new(formatted.into_bytes()))
}

#[unsafe(no_mangle)]
pub fn vec_offset(v: *mut Vec<u8>) -> *mut u8 {
    unsafe { &mut *v }.as_mut_ptr()
}

#[unsafe(no_mangle)]
pub fn vec_len(v: *mut Vec<u8>) -> i32 {
    unsafe { &*v }.len() as i32
}

#[unsafe(no_mangle)]
pub fn allocate_vec(len: i32) -> *mut Vec<u8> {
    Box::into_raw(Box::new(vec![0; len as usize]))
}

#[unsafe(no_mangle)]
pub fn free_vec(v: *mut Vec<u8>) {
    let _ = unsafe { Box::from_raw(v) };
}
