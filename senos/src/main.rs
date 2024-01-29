#![feature(naked_functions)]
#![feature(fn_align)]
#![feature(strict_provenance)]
#![feature(ptr_from_ref)]
#![feature(offset_of)]
#![feature(ascii_char)]
#![feature(ptr_metadata)]
#![feature(thread_local)]
#![no_std]
#![no_main]
use core::alloc::{GlobalAlloc, Layout};
use core::arch::asm;
use core::borrow::BorrowMut;
use core::cell::{OnceCell, RefCell, UnsafeCell};
use core::mem::uninitialized;
use core::panic::PanicInfo;
use core::ptr::{self, addr_of, null, null_mut};
static HELLO: &[u8] = "Hello World!".as_bytes();

const SYS_PUTCHAR: u32 = 1;
const SYS_GETCHAR: u32 = 2;
const SYS_EXIT: u32 = 3;
const SYS_READFILE: u32 = 4;
const SYS_WRITEFILE: u32 = 5;
const SCAUSE_ECALL: usize = 8;
const PROC_EXITED: usize = 2;
const SATP_SV32: usize = (1 << 31);
const SSTATUS_SPIE: usize = (1 << 5);
const PAGE_V: usize = (1 << 0);
const PAGE_R: usize = (1 << 1);
const PAGE_W: usize = (1 << 2);
const PAGE_X: usize = (1 << 3);
const PAGE_U: usize = (1 << 4);
const PAGE_SIZE: usize = 4096;

const SSTATUS_SUM: usize = (1 << 18);

const SECTOR_SIZE: usize = 512;
const VIRTQ_ENTRY_NUM: usize = 16;
const VIRTIO_DEVICE_BLK: usize = 2;
const VIRTIO_BLK_PADDR: usize = 0x10001000;
const VIRTIO_REG_MAGIC: usize = 0x00;
const VIRTIO_REG_VERSION: usize = 0x04;
const VIRTIO_REG_DEVICE_ID: usize = 0x08;
const VIRTIO_REG_QUEUE_SEL: usize = 0x30;
const VIRTIO_REG_QUEUE_NUM_MAX: usize = 0x34;
const VIRTIO_REG_QUEUE_NUM: usize = 0x38;
const VIRTIO_REG_QUEUE_ALIGN: usize = 0x3c;
const VIRTIO_REG_QUEUE_PFN: usize = 0x40;
const VIRTIO_REG_QUEUE_READY: usize = 0x44;
const VIRTIO_REG_QUEUE_NOTIFY: usize = 0x50;
const VIRTIO_REG_DEVICE_STATUS: usize = 0x70;
const VIRTIO_REG_DEVICE_CONFIG: usize = 0x100;
const VIRTIO_STATUS_ACK: usize = 1;
const VIRTIO_STATUS_DRIVER: usize = 2;
const VIRTIO_STATUS_DRIVER_OK: usize = 4;
const VIRTIO_STATUS_FEAT_OK: usize = 8;
const VIRTQ_DESC_F_NEXT: usize = 1;
const VIRTQ_DESC_F_WRITE: usize = 2;
const VIRTQ_AVAIL_F_NO_INTERRUPT: usize = 1;
const VIRTIO_BLK_T_IN: usize = 0;
const VIRTIO_BLK_T_OUT: usize = 1;

const USER_BASE: usize = 0x1000000;

const FILES_MAX: usize = 2;

#[repr(packed)]
struct TarHeader {
    name: [u8; 100],
    mode: [u8; 8],
    uid: [u8; 8],
    gid: [u8; 8],
    size: [u8; 12],
    mtime: [u8; 12],
    checksum: [u8; 8],
    type_: u8,
    linkname: [u8; 100],
    magic: [u8; 6],
    version: [u8; 2],
    uname: [u8; 32],
    gname: [u8; 32],
    devmajor: [u8; 8],
    devminor: [u8; 8],
    prefix: [u8; 155],
    padding: [u8; 12],
    data: [u8; 0], // ヘッダに続くデータ領域を指す配列 (フレキシブル配列メンバ)
}

#[derive(Clone, Copy)]
struct File {
    in_use: bool,     // このファイルエントリが使われているか
    name: [u8; 100],  // ファイル名
    data: [u8; 1024], // ファイルの内容
    size: usize,      // ファイルサイズ
}

impl File {
    fn new() -> Self {
        File {
            in_use: false,
            name: [0; 100],
            data: [0; 1024],
            size: 0,
        }
    }
}

fn oct2int(oct: &[u8]) -> i32 {
    let mut dec = 0;
    println!("{:?}", oct);
    for elem in oct {
        if *elem < b'0' || *elem > b'7' {
            break;
        }
        dec = dec * 8 + (*elem - b'0');
    }
    dec as i32
}

const DISK_MAX_SIZE: usize = align_up(
    (core::mem::size_of::<File>() * FILES_MAX) as u32,
    SECTOR_SIZE as u32,
) as usize;

struct FS {
    files: [File; FILES_MAX],
    disk: [u8; DISK_MAX_SIZE],
}

impl FS {
    fn new() -> Self {
        FS {
            files: [File::new(); FILES_MAX],
            disk: [0; DISK_MAX_SIZE],
        }
    }
    fn fs_init<'a>(&mut self, device: &mut VirtioDevice<'a>) {
        for sector in 0..self.disk.len() / SECTOR_SIZE {
            println!("aaaa {:x?}", addr_of!(self.disk[sector * SECTOR_SIZE..]));
            device.read_write_disk(&mut self.disk[sector * SECTOR_SIZE..], sector, false);
        }
        let mut off = 0;
        for i in 0..FILES_MAX {
            println!("{:x?}", ptr::addr_of!(self.disk[off]));
            let header = unsafe { &mut *(ptr::addr_of!(self.disk[off]) as *mut TarHeader) };
            if header.name[0] == b'\0' {
                break;
            }
            if header.magic != *b"ustar\0" {
                panic!("invalid tar header: magic=\"{:?}\"", header.magic);
            }
            let filesz = oct2int(&header.size);
            //let file = unsafe { &mut *(ptr::addr_of!(self.files[i]) as *mut File) };
            let file = &mut self.files[i];
            file.in_use = true;
            file.name = header.name;
            println!("filesz {}", filesz);
            unsafe {
                ptr::copy_nonoverlapping(
                    header.data.as_ptr(),
                    file.data.as_mut_ptr(),
                    filesz as usize,
                );
            }
            file.size = filesz as usize;
            println!(
                "file: {}, size={}",
                file.name.as_ascii().unwrap().as_str(),
                file.size
            );
            println!("header data: {:?}", unsafe {
                core::slice::from_raw_parts(header.data.as_mut_ptr(), 5)
            });
            println!("file data addr: {:?}", ptr::addr_of!(file.data));
            println!("file disk addr: {:?}", ptr::addr_of!(header.data));

            off += align_up(
                (core::mem::size_of::<TarHeader>() as u32 + filesz as u32),
                SECTOR_SIZE as u32,
            ) as usize;
        }
    }
    fn fs_flush<'a>(&mut self, device: &mut VirtioDevice<'a>) {
        println!("flush");
        self.disk = [0; DISK_MAX_SIZE];
        //unsafe {
        //    ptr::write_bytes(&mut self.disk, 0, self.disk.len());
        //}

        let mut off = 0;
        for file_i in 0..FILES_MAX {
            let file = unsafe { &mut *(ptr::addr_of!(self.files[file_i]) as *mut File) };
            if !file.in_use {
                continue;
            }
            let header = unsafe { &mut *(ptr::addr_of!(self.disk[off]) as *mut TarHeader) };

            println!("before");

            unsafe {
                ptr::write_bytes(
                    ptr::addr_of!(self.disk[off]) as *mut u8,
                    0,
                    core::mem::size_of::<TarHeader>(),
                )
            }
            println!("before");

            header.name = file.name;
            header.mode = *b"000644\0\0";
            header.magic = *b"ustar\0";
            header.version = *b"00";
            header.type_ = b'0';
            let mut filesz = file.size;
            let mut i = 0;
            header.size[i] = (filesz % 8) as u8 + b'0';
            filesz /= 8;
            i += 1;
            while 0 < filesz {
                header.size[i] = (filesz % 8) as u8 + b'0';
                filesz /= 8;
                i += 1;
            }
            let mut checksum: i32 = b' ' as i32 * header.checksum.len() as i32;
            for i in 0..core::mem::size_of::<TarHeader>() {
                checksum += self.disk[off + i] as i32;
            }
            for i in (0..6).rev() {
                header.checksum[i] = (checksum % 8) as u8 + b'0';
                checksum /= 8;
            }
            unsafe {
                ptr::copy_nonoverlapping(
                    file.data.as_ptr(),
                    header.data.as_mut_ptr(),
                    file.size as usize,
                );
            }
            off += align_up(
                (core::mem::size_of::<TarHeader>() as u32 + file.size as u32),
                SECTOR_SIZE as u32,
            ) as usize;
        }
        println!("before sector");
        for sector in 0..self.disk.len() / SECTOR_SIZE {
            device.read_write_disk(&mut self.disk[sector * SECTOR_SIZE..], sector, true);
        }
        println!("wrote {} bytes to disk", self.disk.len());
    }
    fn fs_lookup(&mut self, filename: *const u8, filenamelen: usize) -> Option<&mut File> {
        println!("PM! {:x?}", unsafe { addr_of!(PM) });
        println!("PM! {}", unsafe { PM.len() });

        println!("PM! {}", unsafe { PM.len() });

        for i in 0..FILES_MAX {
            println!("PM! {:x?}", unsafe { addr_of!(PM) });

            println!("PM!! {}", unsafe { PM.len() });
            let file = self.files[i];
            println!("================");
            println!("filename: {:x?}", filename);
            println!("filename: {:?}", unsafe {
                core::slice::from_raw_parts(filename, filenamelen)
            });
            println!("file name: {:?}", file.name);
            println!("file name ptr: {:x?}", file.name.as_ptr());
            println!("PM!!!!! {}", unsafe { PM.len() });

            if file.name[..filenamelen]
                == *unsafe { core::slice::from_raw_parts(filename, filenamelen) }
            {
                println!("PM!!! {}", unsafe { PM.len() });
                return Some(&mut self.files[i]);
            }
        }
        None
    }
}

#[repr(packed)]
struct VirtqDesc {
    addr: u64,
    len: u32,
    flags: u16,
    next: u16,
}

#[repr(packed)]
struct VirtqAvail {
    flags: u16,
    index: u16,
    ring: [u16; VIRTQ_ENTRY_NUM],
}

#[repr(packed)]
struct VirtqUsedElem {
    id: u32,
    len: u32,
}

#[repr(packed)]
struct VirtqUsed {
    flags: u16,
    index: u16,
    ring: [VirtqUsedElem; VIRTQ_ENTRY_NUM],
}

const size_of_u16: usize = core::mem::size_of::<u16>();
const size_of_u32: usize = core::mem::size_of::<u32>();
const size_of_u64: usize = core::mem::size_of::<u64>();

const SIZE: usize = (size_of_u64 + size_of_u16 + size_of_u32 + size_of_u16) * VIRTQ_ENTRY_NUM
    + (size_of_u16 + size_of_u16 + size_of_u16 * VIRTQ_ENTRY_NUM);

#[repr(packed)]
struct VirtioVirtq {
    descs: [VirtqDesc; VIRTQ_ENTRY_NUM],
    avail: VirtqAvail,
    _padding: [u8; (PAGE_SIZE - (SIZE % PAGE_SIZE)) / core::mem::size_of::<u8>()],
    used: VirtqUsed,
    queue_index: i32,
    used_index: *const u16,
    last_used_index: u16,
}

#[repr(packed)]
struct VirtioBlkReq {
    type_: u32,
    reserved: u32,
    sector: u64,
    data: [u8; 512],
    status: u8,
}

fn virtio_reg_read32(offset: u32) -> u32 {
    let ret;
    unsafe {
        ret = ptr::read_volatile(ptr::from_exposed_addr::<u32>(
            VIRTIO_BLK_PADDR + offset as usize,
        ))
    }
    ret
}

fn virtio_reg_read64(offset: u32) -> u64 {
    let ret;
    unsafe {
        ret = ptr::read_volatile(ptr::from_exposed_addr::<u64>(
            VIRTIO_BLK_PADDR + offset as usize,
        ));
    }
    ret
}

fn virtio_reg_write32(offset: u32, value: u32) {
    unsafe {
        ptr::write_volatile(
            ptr::from_exposed_addr_mut::<u32>(VIRTIO_BLK_PADDR + offset as usize),
            value,
        );
    }
}

fn virtio_reg_fetch_and_or32(offset: u32, value: u32) {
    unsafe {
        virtio_reg_write32(offset, virtio_reg_read32(offset) | value);
    }
}

struct VirtioDevice<'a> {
    blk_request_vq: &'a mut VirtioVirtq,
    blk_req: &'a mut VirtioBlkReq,
    blk_req_paddr: *const u32,
    blk_capacity: u64,
}

const fn align_up(addr: u32, align: u32) -> u32 {
    assert!(align.is_power_of_two(), "`align` must be a power of two");
    let align_mask = align - 1;
    if addr & align_mask == 0 {
        addr // already aligned
    } else {
        // FIXME: Replace with .expect, once `Option::expect` is const.
        if let Some(aligned) = (addr | align_mask).checked_add(1) {
            aligned
        } else {
            panic!("attempt to add with overflow")
        }
    }
}

impl<'a> VirtioDevice<'a> {
    fn new() -> VirtioDevice<'a> {
        if virtio_reg_read32(VIRTIO_REG_MAGIC as u32) != 0x74726976 {
            panic!("virtio: invalid magic value");
        }
        if virtio_reg_read32(VIRTIO_REG_VERSION as u32) != 1 {
            panic!("virtio: invalid version");
        }
        if virtio_reg_read32(VIRTIO_REG_DEVICE_ID as u32) != VIRTIO_DEVICE_BLK as u32 {
            panic!("virtio: invalid device id");
        }
        virtio_reg_write32(VIRTIO_REG_DEVICE_STATUS as u32, 0);
        virtio_reg_fetch_and_or32(VIRTIO_REG_DEVICE_STATUS as u32, VIRTIO_STATUS_ACK as u32);
        virtio_reg_fetch_and_or32(VIRTIO_REG_DEVICE_STATUS as u32, VIRTIO_STATUS_DRIVER as u32);
        virtio_reg_fetch_and_or32(
            VIRTIO_REG_DEVICE_STATUS as u32,
            VIRTIO_STATUS_FEAT_OK as u32,
        );
        let blk_request_vq = Self::virtq_init(0);
        virtio_reg_write32(
            VIRTIO_REG_DEVICE_STATUS as u32,
            VIRTIO_STATUS_DRIVER_OK as u32,
        );
        let blk_capacity =
            virtio_reg_read64(VIRTIO_REG_DEVICE_CONFIG as u32 + 0) * SECTOR_SIZE as u64;
        println!("virtio-blk: capacity is {} bytes", blk_capacity);
        let layout = Layout::from_size_align(core::mem::size_of::<VirtioBlkReq>(), 4096).unwrap();
        let blk_req_paddr;
        unsafe {
            blk_req_paddr = alloc::alloc::alloc_zeroed(layout) as *const u32;
        };
        let blk_req = unsafe { &mut *(blk_req_paddr as *mut VirtioBlkReq) };
        VirtioDevice {
            blk_request_vq,
            blk_req,
            blk_req_paddr,
            blk_capacity,
        }
    }
    fn virtq_init(index: usize) -> &'a mut VirtioVirtq {
        //println!("{}", core::mem::size_of::<VirtioVirtq>());
        let layout = Layout::from_size_align(core::mem::size_of::<VirtioVirtq>(), 4096).unwrap();
        let virtq_paddr;
        unsafe {
            virtq_paddr = alloc::alloc::alloc_zeroed(layout);
        };
        let vq = unsafe { &mut *(virtq_paddr as *mut VirtioVirtq) };
        vq.queue_index = index as i32;
        vq.used_index = unsafe {
            let tmp = ptr::addr_of!(vq.used.index) as *const u16;
            tmp
            //ptr::read_unaligned(tmp)
        } as *const u16;
        virtio_reg_write32(VIRTIO_REG_QUEUE_SEL as u32, index as u32);
        virtio_reg_write32(VIRTIO_REG_QUEUE_NUM as u32, VIRTQ_ENTRY_NUM as u32);
        virtio_reg_write32(VIRTIO_REG_QUEUE_ALIGN as u32, 0);
        virtio_reg_write32(VIRTIO_REG_QUEUE_PFN as u32, virtq_paddr as u32);
        vq
    }
    fn virtq_kick<'b>(vq: &'b mut VirtioVirtq, desc_index: u16) {
        vq.avail.ring[(vq.avail.index as usize) % VIRTQ_ENTRY_NUM] = desc_index;
        vq.avail.index += 1;
        unsafe { asm!("fence") }
        virtio_reg_write32(VIRTIO_REG_QUEUE_NOTIFY as u32, vq.queue_index as u32);
        vq.last_used_index += 1;
    }
    fn virtq_is_busy<'b>(vq: &'b mut VirtioVirtq) -> bool {
        //println!("aaaa");
        let ret = vq.last_used_index != unsafe { core::ptr::read_volatile(vq.used_index) };
        //println!("bbbb");
        ret
    }
    fn read_write_disk(&mut self, buf: &mut [u8], sector: usize, is_write: bool) {
        println!("read_write_disk");
        if sector >= self.blk_capacity as usize / SECTOR_SIZE {
            println!(
                "virtio: tried to read/write sector={}, but capacity is {}",
                sector,
                self.blk_capacity as usize / SECTOR_SIZE
            );
            return;
        }
        println!("write1");
        //println!("{:?}", self.blk_req);
        self.blk_req.sector = sector as u64;
        println!("write2");
        self.blk_req.type_ = if is_write {
            VIRTIO_BLK_T_OUT as u32
        } else {
            VIRTIO_BLK_T_IN as u32
        };
        println!("write3");
        if is_write {
            println!("write!");
            unsafe {
                ptr::copy_nonoverlapping(buf.as_ptr(), self.blk_req.data.as_mut_ptr(), SECTOR_SIZE);
            };
            println!("{:?}", self.blk_req.data);
        }
        self.blk_request_vq.descs[0].addr = self.blk_req_paddr as u64;
        self.blk_request_vq.descs[0].len =
            (core::mem::size_of::<u32>() * 2 + core::mem::size_of::<u64>()) as u32;
        self.blk_request_vq.descs[0].flags = VIRTQ_DESC_F_NEXT as u16;
        self.blk_request_vq.descs[0].next = 1;
        self.blk_request_vq.descs[1].addr =
            self.blk_req_paddr as u64 + core::mem::offset_of!(VirtioBlkReq, data) as u64;
        self.blk_request_vq.descs[1].len = SECTOR_SIZE as u32;
        self.blk_request_vq.descs[1].flags =
            (VIRTQ_DESC_F_NEXT | (if is_write { 0 } else { VIRTQ_DESC_F_WRITE })) as u16;
        self.blk_request_vq.descs[1].next = 2;

        self.blk_request_vq.descs[2].addr =
            self.blk_req_paddr as u64 + core::mem::offset_of!(VirtioBlkReq, status) as u64;
        self.blk_request_vq.descs[2].len = core::mem::size_of::<u8>() as u32;
        self.blk_request_vq.descs[2].flags = VIRTQ_DESC_F_WRITE as u16;
        Self::virtq_kick(self.blk_request_vq, 0);
        while Self::virtq_is_busy(self.blk_request_vq) {
            //unsafe { asm!("nop") }
        }
        if self.blk_req.status != 0 {
            println!(
                "virtio: warn: failed to read/write sector={} status={}",
                sector, self.blk_req.status
            );
            return;
        }
        if !is_write {
            println!("buf 3 {:x?}", ptr::addr_of!(buf));
            unsafe {
                ptr::copy_nonoverlapping(
                    self.blk_req.data.as_mut_ptr(),
                    buf.as_mut_ptr(),
                    SECTOR_SIZE,
                );
            };
        }
    }
}

fn sys_call(sysno: usize, arg0: usize, arg1: usize, arg2: usize) -> usize {
    let value;
    unsafe {
        asm!(
            "ecall",
            in("a0") arg0, in("a1") arg1, in("a2") arg2, in("a3") sysno,
            lateout("a0") value,
        );
    }
    value
}

fn sbi_call(
    arg0: usize,
    arg1: usize,
    arg2: usize,
    arg3: usize,
    arg4: usize,
    arg5: usize,
    fid: usize,
    eid: usize,
) -> (i32, usize) {
    let (error, value);
    unsafe {
        asm!(
            "ecall",
            in("a0") arg0, in("a1") arg1, in("a2") arg2, in("a3") arg3, in("a4") arg4, in("a5") arg5, in("a6") fid, in("a7") eid,
            lateout("a0") error, lateout("a1") value,
        );
    }
    (error, value)
}

fn putchar(ch: usize) {
    sbi_call(ch, 0, 0, 0, 0, 0, 0, 1);
}

fn getchar() -> i32 {
    let (error, _) = sbi_call(0, 0, 0, 0, 0, 0, 0, 2);
    error
}

use core::fmt::{self, Write};
extern crate alloc;
use alloc::vec::Vec;

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

extern "C" {
    static mut __bss: u8;
    static mut __bss_end: u8;
    static mut __stack_top: u8;
}

extern "C" fn proc_a_entry() -> ! {
    println!("starting process A");
    loop {
        putchar('A' as usize);
        unsafe {
            PM.yield_();
            /*
            asm!(
                "mv a0, {sp0}",
                "mv a1, {sp1}",
                sp0 = in(reg)&(PM.process_list[0].sp as *mut u32),
                sp1 = in(reg)&(PM.process_list[1].sp as *mut u32),
            );
            */
            // switch_context((&PM.process_list[0].sp), (&PM.process_list[1].sp));
        }
        for i in 0..30000000 {
            unsafe { asm!("nop") }
        }
    }
}

extern "C" fn proc_b_entry() -> ! {
    println!("starting process B");
    loop {
        putchar('B' as usize);
        unsafe {
            PM.yield_();
            /*
            asm!(
                "mv a0, {sp0}",
                "mv a1, {sp1}",
                sp0 = in(reg)&(PM.process_list[1].sp as *mut u32),
                sp1 = in(reg)&(PM.process_list[0].sp as *mut u32),
            );
            */
            //switch_context(&PM.process_list[1].sp, &PM.process_list[0].sp);
        }
        for i in 0..30000000 {
            unsafe { asm!("nop") }
        }
    }
}
static mut DEVICE: OnceCell<VirtioDevice> = OnceCell::new();
static mut FS_: FS = FS {
    files: [File {
        in_use: false,
        name: [0; 100],
        data: [0; 1024],
        size: 0,
    }; FILES_MAX],
    disk: [0; DISK_MAX_SIZE],
};
fn kernel_main() {
    //println!("Hello {}", "World");
    //println!("1 + 2 = {}, {}", 1 + 2, 0x1234abcd);
    /*
    for ch in HELLO {
        putchar(*ch as usize)
    }
    */

    unsafe {
        let bss_size = &__bss_end as *const u8 as usize - &__bss as *const u8 as usize;
        ptr::write_bytes(&mut __bss, 0, bss_size);
        asm!(
            "csrw stvec, {kernel_entry}",
            kernel_entry = in(reg) kernel_entry
        )
    }
    let device = VirtioDevice::new();
    let fs = FS::new();
    println!("fs: {:x?}", addr_of!(fs.disk[0]));
    println!("fs: {:x?}", addr_of!(fs.disk[DISK_MAX_SIZE - 1]));
    unsafe {
        DEVICE.set(device);
        FS_.fs_init(DEVICE.get_mut().unwrap());
    }
    //let mut fs = FS::new();
    /*
    let mut buf = [0u8; SECTOR_SIZE];
    device.read_write_disk(&mut buf, 0, false);
    println!("first sector: {:?}", buf.as_ascii());

    let mut buf = [0u8; SECTOR_SIZE];
    let buf2 = *b"hello from kernel!!!\n";
    for i in 0..21 {
        buf[i] = buf2[i];
    }
    device.read_write_disk(&mut buf, 0, true);
    */
    let bin_shell = include_bytes!("shell_bin");
    #[link_section = ".app_stack"]
    static mut APP_STACK_IDLE: [u8; 2048] = [0; 2048];
    let idle_process = Process::new(unsafe { &mut APP_STACK_IDLE }, user_entry as u32, &[0]);
    #[link_section = ".app_stack"]
    static mut APP_STACK_A: [u8; 2048] = [0; 2048];
    let proc_a = Process::new(unsafe { &mut APP_STACK_A }, user_entry as u32, bin_shell);
    /*
    let proc_a = Process::new(unsafe { &mut APP_STACK_A }, proc_a_entry as u32);
    #[link_section = ".app_stack"]
    static mut APP_STACK_B: [u8; 2048] = [0; 2048];
    let proc_b = Process::new(unsafe { &mut APP_STACK_B }, proc_b_entry as u32);
    */
    unsafe {
        PM.push(idle_process);
        PM.push(proc_a);
        //PM.push(proc_b);
        //proc_a_entry();
        PM.yield_();
    }
    panic!("switched to idle");
    //println!("not");
    loop {
        unsafe { asm!("wfi") }
    }
}
#[repr(C)]
struct ContextFrame_ {
    ra: u32,
    s0: u32,
    s1: u32,
    s2: u32,
    s3: u32,
    s4: u32,
    s5: u32,
    s6: u32,
    s7: u32,
    s8: u32,
    s9: u32,
    s10: u32,
    s11: u32,
}

#[repr(C)]
struct ContextFrame {
    s11: u32,
    s10: u32,
    s9: u32,
    s8: u32,
    s7: u32,
    s6: u32,
    s5: u32,
    s4: u32,
    s3: u32,
    s2: u32,
    s1: u32,
    s0: u32,
    ra: u32,
}

#[repr(packed)]
struct TrapFrame {
    ra: u32,
    gp: u32,
    tp: u32,
    t0: u32,
    t1: u32,
    t2: u32,
    t3: u32,
    t4: u32,
    t5: u32,
    t6: u32,
    a0: u32,
    a1: u32,
    a2: u32,
    a3: u32,
    a4: u32,
    a5: u32,
    a6: u32,
    a7: u32,
    s0: u32,
    s1: u32,
    s2: u32,
    s3: u32,
    s4: u32,
    s5: u32,
    s6: u32,
    s7: u32,
    s8: u32,
    s9: u32,
    s10: u32,
    s11: u32,
    sp: u32,
}

// see https://zenn.dev/sphendami/scraps/e3b9bdb82d0b7e
#[macro_export]
macro_rules! read_csr {
    ($csr:literal) => {{
        let mut val: u32;
        unsafe {
            ::core::arch::asm!(concat!("csrr {}, ", $csr), out(reg) val);
        }
        val
    }};
}
#[macro_export]
macro_rules! write_csr {
    ($csr:literal, $val:expr) => {{
        ::core::arch::asm!(concat!("csrw ", $csr, ", {}"), in(reg) $val);
    }};
}

fn handle_trap(f: &mut TrapFrame) {
    let scause = read_csr!("scause");
    let stval = read_csr!("stval");
    let mut sepc = read_csr!("sepc");
    if scause == (SCAUSE_ECALL as u32) {
        handle_syscall(f);
        sepc += 4;
    } else {
        panic!(
            "unexpected trap scause={:#010x}, stval={:#010x}, sepc={:#010x}",
            scause, stval, sepc
        )
    }
    unsafe {
        asm!(
            "csrw sepc, {sepc}",
            sepc = in(reg) sepc
        )
    }
}

fn handle_syscall(f: &mut TrapFrame) {
    match f.a3 {
        SYS_EXIT => {
            println!("process {} exited", unsafe { PM.current_pid });
            unsafe {
                PM.exit_current_process();
                PM.yield_();
            }
            panic!("unreachable");
        }
        SYS_GETCHAR => {
            loop {
                let ch = getchar();
                if ch >= 0 {
                    f.a0 = ch as u32;
                    break;
                }
            }
            unsafe {
                PM.yield_();
            }
        }
        SYS_PUTCHAR => putchar(f.a0 as usize),
        SYS_READFILE | SYS_WRITEFILE => {
            println!("PM {}", unsafe { PM.len() });
            println!("PM {}", unsafe { PM.len() });

            let filename = f.a0;
            let buf = f.a1;
            //println!("a1: {}", buf);
            //println!("buf: {:x?}", buf as *const u32);
            let mut len = f.a2;
            let filenamelen = f.a4;
            println!("a4:{}", filenamelen);
            let file_op = unsafe { FS_.fs_lookup(filename as *const u8, filenamelen as usize) };
            println!("PM {}", unsafe { PM.len() });

            match file_op {
                None => {
                    println!("file not found: {}", filename);
                    f.a0 = u32::MAX;
                }
                Some(file) => {
                    if len > file.data.len() as u32 {
                        len = file.size as u32;
                    }
                    if f.a3 == SYS_WRITEFILE {
                        unsafe {
                            ptr::copy_nonoverlapping(
                                buf as *const u8,
                                file.data.as_mut_ptr() as *mut u8,
                                len as usize,
                            );
                        }
                        file.size = len as usize;
                        unsafe { FS_.fs_flush(DEVICE.get_mut().unwrap()) }
                    } else {
                        println!("PM {}", unsafe { PM.len() });
                        println!("data: {:x?}", file.data.as_ptr());
                        println!("data: {:x?}", addr_of!(file.data));
                        println!("data: {:?}", file.data);
                        unsafe {
                            ptr::copy_nonoverlapping(
                                file.data.as_ptr() as *const u8,
                                buf as *mut u8,
                                len as usize,
                            )
                        }
                        println!("BUF: {:?}", unsafe {
                            core::slice::from_raw_parts(buf as *const u8, len as usize)
                        });
                    }
                    f.a0 = len;
                }
            }
        }
        _ => {
            let a3 = f.a3;
            panic!("unexpected syscall a3={:#010x}", a3);
        }
    }
}

#[no_mangle]
#[naked]
#[repr(align(4))]
fn switch_context(prev_sp: &*mut u8, next_sp: &*mut u8) {
    unsafe {
        asm!(
            "addi sp, sp, -13 * 4",
            "sw ra,  0  * 4(sp)",
            "sw s0,  1  * 4(sp)",
            "sw s1,  2  * 4(sp)",
            "sw s2,  3  * 4(sp)",
            "sw s3,  4  * 4(sp)",
            "sw s4,  5  * 4(sp)",
            "sw s5,  6  * 4(sp)",
            "sw s6,  7  * 4(sp)",
            "sw s7,  8  * 4(sp)",
            "sw s8,  9  * 4(sp)",
            "sw s9,  10 * 4(sp)",
            "sw s10, 11 * 4(sp)",
            "sw s11, 12 * 4(sp)",
            "sw sp, (a0)",
            "lw sp, (a1)",
            "lw ra,  0  * 4(sp)",
            "lw s0,  1  * 4(sp)",
            "lw s1,  2  * 4(sp)",
            "lw s2,  3  * 4(sp)",
            "lw s3,  4  * 4(sp)",
            "lw s4,  5  * 4(sp)",
            "lw s5,  6  * 4(sp)",
            "lw s6,  7  * 4(sp)",
            "lw s7,  8  * 4(sp)",
            "lw s8,  9  * 4(sp)",
            "lw s9,  10 * 4(sp)",
            "lw s10, 11 * 4(sp)",
            "lw s11, 12 * 4(sp)",
            "addi sp, sp, 13 * 4",
            "ret",
            options(noreturn)
        )
    }
}

#[naked]
#[repr(align(4))]
fn kernel_entry() {
    unsafe {
        asm!(
            "csrrw sp, sscratch, sp",
            "addi sp, sp, -4 * 31",
            "sw ra,  4 * 0(sp)",
            "sw gp,  4 * 1(sp)",
            "sw tp,  4 * 2(sp)",
            "sw t0,  4 * 3(sp)",
            "sw t1,  4 * 4(sp)",
            "sw t2,  4 * 5(sp)",
            "sw t3,  4 * 6(sp)",
            "sw t4,  4 * 7(sp)",
            "sw t5,  4 * 8(sp)",
            "sw t6,  4 * 9(sp)",
            "sw a0,  4 * 10(sp)",
            "sw a1,  4 * 11(sp)",
            "sw a2,  4 * 12(sp)",
            "sw a3,  4 * 13(sp)",
            "sw a4,  4 * 14(sp)",
            "sw a5,  4 * 15(sp)",
            "sw a6,  4 * 16(sp)",
            "sw a7,  4 * 17(sp)",
            "sw s0,  4 * 18(sp)",
            "sw s1,  4 * 19(sp)",
            "sw s2,  4 * 20(sp)",
            "sw s3,  4 * 21(sp)",
            "sw s4,  4 * 22(sp)",
            "sw s5,  4 * 23(sp)",
            "sw s6,  4 * 24(sp)",
            "sw s7,  4 * 25(sp)",
            "sw s8,  4 * 26(sp)",
            "sw s9,  4 * 27(sp)",
            "sw s10, 4 * 28(sp)",
            "sw s11, 4 * 29(sp)",
            "csrr a0, sscratch",
            "sw a0, 4 * 30(sp)",
            "addi a0, sp, 4 * 31",
            "csrw sscratch, a0",
            "mv a0, sp",
            "call {handle_trap}",
            "lw ra,  4 * 0(sp)",
            "lw gp,  4 * 1(sp)",
            "lw tp,  4 * 2(sp)",
            "lw t0,  4 * 3(sp)",
            "lw t1,  4 * 4(sp)",
            "lw t2,  4 * 5(sp)",
            "lw t3,  4 * 6(sp)",
            "lw t4,  4 * 7(sp)",
            "lw t5,  4 * 8(sp)",
            "lw t6,  4 * 9(sp)",
            "lw a0,  4 * 10(sp)",
            "lw a1,  4 * 11(sp)",
            "lw a2,  4 * 12(sp)",
            "lw a3,  4 * 13(sp)",
            "lw a4,  4 * 14(sp)",
            "lw a5,  4 * 15(sp)",
            "lw a6,  4 * 16(sp)",
            "lw a7,  4 * 17(sp)",
            "lw s0,  4 * 18(sp)",
            "lw s1,  4 * 19(sp)",
            "lw s2,  4 * 20(sp)",
            "lw s3,  4 * 21(sp)",
            "lw s4,  4 * 22(sp)",
            "lw s5,  4 * 23(sp)",
            "lw s6,  4 * 24(sp)",
            "lw s7,  4 * 25(sp)",
            "lw s8,  4 * 26(sp)",
            "lw s9,  4 * 27(sp)",
            "lw s10, 4 * 28(sp)",
            "lw s11, 4 * 29(sp)",
            "lw sp,  4 * 30(sp)",
            "sret",
            handle_trap = sym handle_trap,
            options(noreturn)
        );
    }
}

pub struct BumpPointerAlloc {
    pub head: UnsafeCell<*const u8>,
    pub end: *const u8,
}
unsafe impl Sync for BumpPointerAlloc {}

unsafe impl GlobalAlloc for BumpPointerAlloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let size = layout.size();
        let align = layout.align();

        let head = self.head.get();

        let rem = *head as usize % align;
        let start_addr = if rem == 0 {
            *head
        } else {
            (*head).add(align - rem)
        };

        let next_head = start_addr.add(size);
        if next_head > self.end {
            ptr::null_mut()
        } else {
            *head = next_head;
            start_addr as *mut u8
        }
    }

    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {
        // never deallocate
    }
}

extern "C" {
    static __free_ram: u8;
    static __free_ram_end: u8;
    static __kernel_base: u8;
}

#[global_allocator]
static HEAP: BumpPointerAlloc = BumpPointerAlloc {
    head: UnsafeCell::new(unsafe { &__free_ram } as *const u8),
    end: unsafe { &__free_ram_end } as *const u8,
};
#[allow(arithmetic_overflow)]
fn map_page(table1: *mut u32, vaddr: u32, paddr: u32, flags: u32) {
    let vpn1 = (vaddr >> 22) & 0x3ff;
    //print!("{:x}", vpn1);
    if unsafe { *table1.add(vpn1 as usize) } & PAGE_V as u32 == 0 {
        let layout = Layout::from_size_align(4096, 4096).unwrap();
        unsafe {
            let pt_paddr = alloc::alloc::alloc_zeroed(layout);
            *table1.add(vpn1 as usize) =
                ((pt_paddr as u32 / PAGE_SIZE as u32) << 10) | PAGE_V as u32;
            println!("{:?}", ((pt_paddr as usize / PAGE_SIZE) << 10) | PAGE_V);
        };
    }
    let vpn0 = (vaddr >> 12) & 0x3ff;
    //println!("{:?}", unsafe { *table1.add(vpn1 as usize) });
    //println!("{:?}", unsafe { *table1.add(vpn1 as usize) >> 10 });
    unsafe {
        let table0 = ((*table1.add(vpn1 as usize) >> 10) * (PAGE_SIZE as u32)) as *mut u32;
        *table0.add(vpn0 as usize) =
            ((paddr as u32 / PAGE_SIZE as u32) << 10) | flags | PAGE_V as u32;
    }
}

#[derive(Clone, Copy, PartialEq)]
enum State {
    UNUSED,
    RUNNABLE,
    EXITED,
}

//#[naked]
unsafe fn user_entry() {
    asm!("csrw sepc, {sepc}",
    "csrw sstatus, {sstatus}",
    "sret",
    sepc = in(reg) USER_BASE,
    sstatus = in(reg) SSTATUS_SPIE | SSTATUS_SUM,
    options(noreturn))
}

#[repr(C)]
#[derive(Clone)]
struct Process<'a> {
    pid: usize,
    state: State,
    //sp: usize,
    sp: *mut u8,
    regs: [u32; 8], //stack: [u32; 8192],
    page_table: *mut u8,
    marker: core::marker::PhantomData<&'a u8>,
}

static mut PID: usize = 0;

impl<'a> Process<'a> {
    pub fn new(stack: &'a mut [u8], pc: u32, image: &'a [u8]) -> Self {
        let sp = (&stack[0] as *const u8 as usize) + stack.len() - 0x34;
        //println!("{:?}", sp as *const u32);
        //println!("{:?}", &stack[0] as *const u8);
        let trap_freame: &mut ContextFrame_ = unsafe { &mut *(sp as *mut ContextFrame_) };
        trap_freame.s11 = 0;
        trap_freame.s10 = 0;
        trap_freame.s9 = 0;
        trap_freame.s8 = 0;
        trap_freame.s7 = 0;
        trap_freame.s6 = 0;
        trap_freame.s5 = 0;
        trap_freame.s4 = 0;
        trap_freame.s3 = 0;
        trap_freame.s2 = 0;
        trap_freame.s1 = 0;
        trap_freame.s0 = 0;
        trap_freame.ra = pc;
        //println!("{:?}", &trap_freame.ra as *const u32);
        //println!("{:?}", &trap_freame.s0 as *const u32);
        unsafe { PID += 1 };
        let layout = Layout::from_size_align(4096, 4096).unwrap();
        let page_table;
        unsafe {
            page_table = alloc::alloc::alloc_zeroed(layout);
        };
        let mut paddr = unsafe { &__kernel_base as *const u8 };
        println!("{:?}", paddr as *const u32);
        while paddr < unsafe { &__free_ram_end as *const u8 } {
            map_page(
                page_table as *mut u32,
                paddr as u32,
                paddr as u32,
                (PAGE_R | PAGE_W | PAGE_X) as u32,
            );
            unsafe {
                paddr = paddr.add(PAGE_SIZE as usize);
            }
        }
        let mut off = 0;
        while off < image.len() {
            let page;
            unsafe {
                page = alloc::alloc::alloc_zeroed(layout);
                ptr::copy_nonoverlapping(image.as_ptr().add(off), page, PAGE_SIZE);
            };
            map_page(
                page_table as *mut u32,
                (USER_BASE + off) as u32,
                page as u32,
                (PAGE_U | PAGE_R | PAGE_W | PAGE_X) as u32,
            );
            off += PAGE_SIZE;
        }
        map_page(
            page_table as *mut u32,
            VIRTIO_BLK_PADDR as u32,
            VIRTIO_BLK_PADDR as u32,
            (PAGE_R | PAGE_W) as u32,
        );

        Process {
            pid: unsafe { PID } - 1,
            state: State::RUNNABLE,
            sp: sp as *mut u8,
            regs: [0; 8],
            page_table: page_table,
            marker: core::marker::PhantomData,
        }
    }
}

struct ProcessManager<'a> {
    _p: [u8; 8],
    current_pid: usize,
    idle_pid: usize,
    process_list: [Option<Process<'a>>; 4],
    len: usize,
    _p2: [u8; 8],
}

/*
static mut PM: ProcessManager<'static> = ProcessManager {
    current_pid: 0,
    idle_pid: 0,
    process_list: Vec::new(),
};
*/

static mut PM: ProcessManager<'static> = ProcessManager {
    _p: [0; 8],
    current_pid: 0,
    idle_pid: 0,
    process_list: [None, None, None, None],
    len: 0,
    _p2: [0; 8],
};

impl<'a> ProcessManager<'a> {
    fn exit_current_process(&mut self) {
        self.process_list[self.current_pid].as_mut().unwrap().state = State::EXITED;
    }
    fn push(&mut self, process: Process<'a>) {
        self.process_list[self.len] = Some(process);
        self.len += 1;
    }
    fn len(&self) -> usize {
        self.len
        //unsafe { self.process_list.get().as_ref().unwrap().len() }
    }
    fn yield_(&mut self) {
        let p = &self.process_list;
        let mut next_process = p[self.idle_pid].as_ref().unwrap();
        for i in 0..2 {
            let process = p[(self.current_pid + i + 1) % 2].as_ref().unwrap();
            if process.state == State::RUNNABLE && process.pid > 0 {
                next_process = process;
                break;
            }
        }
        if next_process.pid == self.current_pid {
            return;
        }
        let current_process = p[self.current_pid].as_ref().unwrap();
        let trap_frame: &TrapFrame = unsafe { &*(next_process.sp as *mut TrapFrame) };
        unsafe {
            asm!(
                "sfence.vma",
                "csrw satp, {satp}",
                "sfence.vma",
                "csrw sscratch, {sscratch}",
                satp = in(reg) SATP_SV32 | (next_process.page_table as usize/ PAGE_SIZE),
                sscratch = in(reg) trap_frame,
            )
        }
        self.current_pid = next_process.pid;
        switch_context(&(current_process.sp), &(next_process.sp));
    }
}

/**
unsafe fn create_process(pc: u32) -> &'static mut Process {
    let mut proc: &mut Process = unsafe { core::mem::uninitialized() };
    let mut count = 0;
    while count < 8 {
        if PROCS[count].state == State::UNUSED {
            proc = &mut PROCS[count];
            break;
        }
        count += 1;
    }
    if count == 8 {
        panic!("no free process slots")
    }
    let sp: *mut u32 = &mut proc.stack[proc.stack.len() as usize];
    unsafe { *sp.offset(-1) = 0 };
    unsafe { *sp.offset(-2) = 0 };
    unsafe { *sp.offset(-3) = 0 };
    unsafe { *sp.offset(-4) = 0 };
    unsafe { *sp.offset(-5) = 0 };
    unsafe { *sp.offset(-6) = 0 };
    unsafe { *sp.offset(-7) = 0 };
    unsafe { *sp.offset(-8) = 0 };
    unsafe { *sp.offset(-9) = 0 };
    unsafe { *sp.offset(-10) = 0 };
    unsafe { *sp.offset(-11) = 0 };
    unsafe { *sp.offset(-12) = 0 }; // s0
    unsafe { *sp.offset(-13) = pc }; //ra

    proc.pid = count + 1;
    proc.state = State::RUNNABLE;
    proc.sp = unsafe { sp.offset(-13) };
    proc
}*/

#[no_mangle]
#[link_section = ".text.boot"]
pub unsafe extern "C" fn boot() -> ! {
    // スタックポインタを初期化
    asm!(
        "mv sp, {stack_top}",
        "j {kernel_main}",
        stack_top = in(reg) &__stack_top,
        kernel_main = sym kernel_main,
        options(noreturn)
    );
}

/// This function is called on panic.
#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    println!("{:?}", info);
    loop {}
}
