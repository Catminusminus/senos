#![no_std]
#![no_main]
//#![feature(naked_functions)]
use core::arch::asm;
use core::fmt::{self, Write};
use core::panic::PanicInfo;

const SYS_PUTCHAR: usize = 1;
const SYS_GETCHAR: usize = 2;
const SYS_EXIT: usize = 3;
const SYS_READFILE: usize = 4;
const SYS_WRITEFILE: usize = 5;

fn sys_call(sysno: usize, arg0: usize, arg1: usize, arg2: usize, arg3: usize) -> usize {
    let value;
    unsafe {
        asm!(
            "ecall",
            in("a0") arg0, in("a1") arg1, in("a2") arg2, in("a3") sysno, in("a4") arg3,
            lateout("a0") value,
        );
    }
    value
}

extern "C" {
    static __stack_top: u8;
}

#[no_mangle]
extern "C" fn exit() -> ! {
    sys_call(SYS_EXIT, 0, 0, 0, 0);
    loop {}
}

fn putchar(ch: usize) {
    sys_call(SYS_PUTCHAR, ch, 0, 0, 0);
}

fn getchar() -> usize {
    sys_call(SYS_GETCHAR, 0, 0, 0, 0)
}

fn readfile(filename: *const u32, filenamelen: usize, buf: *mut u8, len: usize) -> usize {
    println!("filelen {}", filenamelen);
    return sys_call(
        SYS_READFILE,
        filename as usize,
        buf as usize,
        len,
        filenamelen,
    );
}

fn writefile(filename: *const u32, filenamelen: usize, buf: *const u8, len: usize) -> usize {
    return sys_call(
        SYS_WRITEFILE,
        filename as usize,
        buf as usize,
        len,
        filenamelen,
    );
}

#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => ($crate::_print(format_args!($($arg)*)));
}

#[macro_export]
macro_rules! println {
    ($fmt:expr) => (print!(concat!($fmt, "\n")));
    ($fmt:expr, $($arg:tt)*) => (print!(concat!($fmt, "\n"), $($arg)*));
}

pub fn _print(args: fmt::Arguments) {
    let mut writer = UartWriter {};
    writer.write_fmt(args).unwrap();
}

struct UartWriter;

impl Write for UartWriter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for c in s.bytes() {
            //write_byte(c);
            putchar(c as usize)
        }
        Ok(())
    }
}

#[no_mangle]
#[link_section = ".text.start"]
unsafe extern "C" fn start() -> ! {
    asm!(
        "mv sp, {stack_top}",
        "call main",
        "call exit",
        stack_top = in(reg) &__stack_top,
        options(noreturn)
    )
}

#[no_mangle]
extern "C" fn main() {
    loop {
        let mut cmdline = [0; 128];
        let mut goto_here = false;
        print!("> ");
        let mut count = 0;
        loop {
            let ch = getchar();
            putchar(ch);
            if count == 127 {
                println!("command line too long");
                goto_here = true;
                break;
            } else if ch == (b'\r' as usize) {
                break;
            } else {
                cmdline[count] = ch as u8;
                count += 1;
                continue;
            }
        }
        if goto_here {
            break;
        }
        if &cmdline[..count] == b"hello" {
            println!("Hello world from shell!");
        } else if &cmdline[..count] == b"exit" {
            exit();
        } else if &cmdline[..count] == b"readfile" {
            let mut buf: [u8; 32] = [0; 32];
            println!("buf ptr: {:x?}", buf.as_mut_ptr() as *mut u8);
            readfile(
                b"./hello.txt".as_ptr() as *const u32,
                b"./hello.txt".len(),
                buf.as_mut_ptr() as *mut u8,
                buf.len(),
            );
            println!("{:?}", buf);
        } else if &cmdline[..count] == b"writefile" {
            writefile(
                b"./hello.txt".as_ptr() as *const u32,
                b"./hello.txt".len(),
                b"Hello from shell!\n\0".as_ptr() as *const u8,
                19,
            );
        } else {
            println!("unknown command: {:?}", &cmdline[..count]);
        }
    }
    //println!("Hello World from shell!");
    /*
    unsafe {
        ptr::write_bytes(0x80200000 as *mut u32, 0x1 as u8, 1);
    }
    loop {}
    */
}

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    loop {}
}
