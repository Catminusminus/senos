# !/bin/bash
set -xue

# QEMUのファイルパス
QEMU=/usr/local/bin/qemu-system-riscv32
(cd senos/disk && tar cf ../disk.tar --format=ustar ./*.txt)
# QEMUを起動
$QEMU -machine virt -bios default -nographic -serial mon:stdio --no-reboot -drive id=drive0,file=senos/disk.tar,format=raw -device virtio-blk-device,drive=drive0,bus=virtio-mmio-bus.0 -kernel senos/target/riscv32i-unknown-none-elf/debug/kernel_elf
