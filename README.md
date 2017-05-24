# AsmFileLoader
for x86 real mode, in assembly language, load a file in given path to memory, applicable to ext2 and FAT32

##ext2_gas.S
for ext2 and to be compiled by gas (at&t style)
`gcc -nostdlib -Wl,-Ttext=0x500 -Wl,--oformat=binary -o ext2_gas.bin ext2_gas.S`

##ext2_nasm.asm
same as ext2_gas.S but in intel style, to be compiled by nasm
`nasm -o ext2_nasm.bin ext2_nasm.asm`

##FAT32_nasm.asm
for FAT32 and to be compiled by nasm
`nasm -o FAT32_nasm.bin FAT32_nasm.asm`