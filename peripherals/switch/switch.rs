// CoreA Switch Monitor (Rust)
// Monitors simulated switch state, stores in shared memory

use std::process::{Command, exit};
use std::ffi::CString;
use std::ptr;

#[link(name = "syscall")]
extern "C" {
    fn sys_shm(size: u32) -> *mut u8;
    fn sys_write(fd: i32, buf: *const u8, count: u32);
    fn sys_exit(status: i32);
}

fn main() {
    // Check kernel configuration
    let status = Command::new("perl")
        .arg("-e")
        .arg("exit 1 unless do \"config/kernel.conf\"->{SHM} && do \"config/kernel.conf\"->{PROCESS}")
        .status()
        .expect("Failed to check config");
    if !status.success() {
        let msg = "Required features disabled\n";
        let c_msg = CString::new(msg).unwrap();
        unsafe { sys_write(1, c_msg.as_ptr() as *const u8, msg.len() as u32) };
        unsafe { sys_exit(1) };
    }

    // Allocate shared memory
    let shm_addr = unsafe { sys_shm(1024) };
    if shm_addr.is_null() {
        let msg = "SHM allocation failed\n";
        let c_msg = CString::new(msg).unwrap();
        unsafe { sys_write(1, c_msg.as_ptr() as *const u8, msg.len() as u32) };
        unsafe { sys_exit(1) };
    }

    // Simulate switch monitoring
    for i in 0..5 {
        let state = if i % 2 == 0 { 1 } else { 0 };
        unsafe { ptr::write(shm_addr as *mut u32, state) };
        let msg = format!("Switch State: {}\n", state);
        let c_msg = CString::new(msg.clone()).unwrap();
        unsafe { sys_write(1, c_msg.as_ptr() as *const u8, msg.len() as u32) };
    }

    unsafe { sys_exit(0) };
}
