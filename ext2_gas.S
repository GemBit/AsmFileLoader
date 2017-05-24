/* ===============================ext2_booter===================================
 *
 * Load the a boot file with given path to 0:0x7c00 by analizing ext2 filesystem
 *
 * Created by <1943825697@qq.com> 2016
 *
 *     This program is aimed at writing a booter for MBR based storage devices
 * (mainly for a usb flash drive). If witten to the MBR(and next few sectors),
 * it can load the file located at the certain given path in the devices by
 * resolving partition table and ext2 file system structure.
 *     If found, the target file will be loaded to 0:0x7c00 and be executed,
 * else an error will be reported.
 *     By this way can we realize a two-stage boot-up. The loaded program can be
 * update simply by change the file in the disk, and its size limit increases to
 * 608KB(seems to be rich enough for initializing and loading kernel).
 *
 *============================================================================*/

/* ================================requirement==================================
 *     ONLY ext2 with revision 1 and higher is supported
 *
 *     Block size should never be larger than 8kB(this is commonly satisfied)
 *
 *     Size of the occupied blocks of target file(not the file size) limits to
 * 608KB, but since 608/8=integer, it can also be regarded as a file size limit.
 *============================================================================*/


    .equ    MAX_BLOCK_SIZE,             8 * 1024
    .equ    GDT_ITEM_SIZE,              0x20
    .equ    SECTOR_SIZE,                0x200
    .equ    EXT2_MAGIC_NUMBER,          0xef53

    .equ    PROGRAM_ADDRES,             0x500
    .equ    STACK_BOTTOM,               0x1600
    .equ    SUPER_BLOCK_BUFFER,         0x1600
    .equ    GDT_BUFFER,                 0x1800
    .equ    INODE_TABLE_BUFFER,         0x1a00

    .equ    BLOCK_POINTER_BUFFER1,      0x1c00
    .equ    BLOCK_POINTER_BUFFER1_END,  BLOCK_POINTER_BUFFER1 + MAX_BLOCK_SIZE
    .equ    BLOCK_POINTER_BUFFER2,      0x3c00
    .equ    BLOCK_POINTER_BUFFER2_END,  BLOCK_POINTER_BUFFER2 + MAX_BLOCK_SIZE
    .equ    BLOCK_POINTER_BUFFER3,      0x5c00
    .equ    BLOCK_POINTER_BUFFER3_END,  BLOCK_POINTER_BUFFER3 + MAX_BLOCK_SIZE

    .equ    LOAD_DESTINATION,           0x7c00
    .equ    LOAD_DESTINATION_END,       0x9fc00

    /* .text section should be located at address PROGRAM_ADDRES when link*/
    .code16
    .section .text
    .global _start

/* Jmp statement and some global variables */
_start:
    jmp initialize

    /* absolute path of the target file, a terminating '\0' is a must */
    targetFilePath:
        .string "/boot/loader.bin"
    targetFilePath.end:

    /* driveIndex is required when load sectors by int $0x13 */
    driveIndex: .byte 0

    /* some geometric parameters about current partition */
    partitionFirstSector:   .long 0
    sectorsPerBlock:        .long 0
    bytesPerBlock:          .long 0
    firstGDTSector:         .long 0

initialize:
    xorw %ax, %ax
    movw %ax, %ds
    movw %ax, %es
    cli
    movw %ax, %ss
    movw $STACK_BOTTOM, %sp
    sti

    /* When MBR is just loaded to 0x7c00 by BIOS, drive index is saved in dl,
     * thus we use it to initialize "driveIndex" and load the all the code.
     */
    movb %dl, driveIndex
    xorl %eax, %eax
    movw $((_end - _start) / SECTOR_SIZE), %cx
    movw $PROGRAM_ADDRES, %di
    call loadSectors
    jc fail /* read fail */
    /* Since driveIndex has been overwrited, write for the second time. */
    movb %dl, driveIndex
    jmp startToLoad + PROGRAM_ADDRES - LOAD_DESTINATION

/* What to do if fails to load */
fail:
    /* clear screen */
    movw $0x0600, %ax
    movb $0b00001100, %bh
    xorw %cx, %cx
    movw $((25 - 1)<<8 + (80 - 1)), %dx
    int $0x10

    /* print error infomation */
    movw $0x1301, %ax
    xorb %bh, %bh

    movb $0b00001100, %bl
    movw $(fail.infoText.end - fail.infoText), %cx
    movw $0x100, %dx
    movw $fail.infoText, %bp
    int $0x10

    movb $0b00011100, %bl
    movw $(targetFilePath.end - targetFilePath - 1), %cx
    movw $(((fail.infoText.end - fail.infoText) / 80 + 3)<<8 + 4), %dx
    movw $targetFilePath, %bp
    int $0x10

    fail.endlessLoop:
        hlt
        jmp fail.endlessLoop
    fail.infoText:
        .ascii "ERROR, fail to load the boot file with the give path:"
    fail.infoText.end:


/* Load certain sectors to mermory
 * Input: eax = LBA value of the start sector
 *        cx = number of sectors to read
 *        es:di = pointer of the buffer to where sectors will be transferred
 * Output:clear cf if no error, set cf on error
 */
loadSectors:
    pushl %eax
    pushw %dx
    pushw %si
    movw %cx, loadSectors.dap.sectorCount
    movw %di, loadSectors.dap.desOff
    movw %es, loadSectors.dap.desSeg
    movl %eax, loadSectors.dap.LBA
    movw $loadSectors.dap, %si
    movb driveIndex, %dl
    movb $0x42, %ah
    int $0x13
    popw %si
    popw %dx
    popl %eax
    ret
    loadSectors.dap:
        .byte 0x10
        .byte 0
        loadSectors.dap.sectorCount:    .word 0
        loadSectors.dap.desOff:         .word 0
        loadSectors.dap.desSeg:         .word 0
        loadSectors.dap.LBA:            .long 0
        .long 0

/* eax is block index, but cx is still sector count instead of block count */
loadSectorsByBlockIndex:
    pushl %eax
    pushl %edx
    mull sectorsPerBlock
    addl partitionFirstSector, %eax
    call loadSectors
    popl %edx
    popl %eax
    ret

/* Starting load process and try to find the the file in partitions one by one.
 * Once found a EXT2 partition, initialize all the geometric parameters of it,
 * then calls findInCurrentPartition to try in this partition.
 * If success, jumps and executes it, otherwise gives a failure output.
 */
startToLoad:
    movw $(0x1be+0x08), %bp
    startToLoad.onePartition:
        /* check for partitions left and initialize partitionFirstSector */
        cmpw $SECTOR_SIZE, %bp
        jg fail
        movl PROGRAM_ADDRES(%bp), %eax
        movl %eax, partitionFirstSector
        testl %eax, %eax
        jz startToLoad.nextPartition

        /* load superblock */
        addl $2, %eax
        movw $1, %cx
        movw $SUPER_BLOCK_BUFFER, %di
        call loadSectors
        jc startToLoad.nextPartition

        /* check magic number to ensure it is an ext2 filesystem partition */
        cmpw $EXT2_MAGIC_NUMBER, 0x38 + SUPER_BLOCK_BUFFER
        jne startToLoad.nextPartition

        /* check is it revision 1 or later */
        cmpw $1, 0x4c + SUPER_BLOCK_BUFFER
        jb startToLoad.nextPartition

        /* initialize bytesPerBlock */
        movl $1024, %eax
        movb 0x18 + SUPER_BLOCK_BUFFER, %cl
        shll %cl, %eax
        movl %eax, bytesPerBlock
        cmpl $MAX_BLOCK_SIZE, %eax
        ja startToLoad.nextPartition /* maximum supported block size is 8K */

        /* initialize sectorsPerBlock */
        xorl %edx, %edx
        movl $SECTOR_SIZE, %ebx
        divl %ebx
        movl %eax, sectorsPerBlock

        /* initialize firstGDTSector */
        movl %eax, %ebx
        movl 0x14 + SUPER_BLOCK_BUFFER, %eax
        incl %eax
        mull %ebx
        addl partitionFirstSector, %eax
        movl %eax, firstGDTSector

        /* if find the file in the partition, jump to execute */
        call findInCurrentPartition
        jc startToLoad.nextPartition /* no, try next */
        movb driveIndex, %dl /* restor dl with driveIndex */
        jmp LOAD_DESTINATION

    startToLoad.nextPartition:
        addw $0x10, %bp
        jmp startToLoad.onePartition

/* According to the requirement of MBR, 64 bytes for partition table should be
 * reserved(you ought to ensure it after assembling).
 * terminating 55 aa serves as a signature
 */
padFirstSector:
    .fill 512 - 2 - (padFirstSector - _start), 1, 0
    .byte 0x55, 0xaa

/* Try to find and load the target file in one partition
 * Input: prepared global geometric parameter variables
 * Output:clear cf if success, set cf not found
 */
findInCurrentPartition:
    push %eax
    push %bx
    push %cx
    push %edx
    push %di
    push %si
    push %es
    movl $2, %eax /* root directory's inode index */
    movw $targetFilePath, %si

    findInCurrentPartition.enterDir:
        incw %si /* skip the '/' character */
        xorw %bx, %bx

        findInCurrentPartition.enterDir.nameNextChar:
            cmpb $'/', (%bx, %si)
            je findInCurrentPartition.enterDir.dirNameEnd
            cmpb $0, (%bx, %si)
            je findInCurrentPartition.enterDir.fileNameEnd
            incw %bx
            jmp findInCurrentPartition.enterDir.nameNextChar

        findInCurrentPartition.enterDir.fileNameEnd:
            movb $1, findInCurrentPartition.itemIsFile
        findInCurrentPartition.enterDir.dirNameEnd:
            movw %bx, %cx

        /* call findChildItem and determine whether it is file or directory */
        call findChildItem
        jc findInCurrentPartition.fail
        addw %cx, %si
        cmpb $1, findInCurrentPartition.itemIsFile
        jne findInCurrentPartition.enterDir

        /* eventually, it reaches the target file and now start to load it. */
        call dataBlock.reset
        jc findInCurrentPartition.fail

        /* refuse to load a file with too large size */
        call dataBlock.getLeftBlockCount
        mull bytesPerBlock
        cmpl $(LOAD_DESTINATION_END - LOAD_DESTINATION), %eax
        ja findInCurrentPartition.fail

        /* Since the allowable size is larger than 0xffff,
         * we should make es instead di increase after every block beenloaded.
         */
        movw $(LOAD_DESTINATION / 0x10), %ax
        movw %ax, %es
        xorw %di, %di
        movw bytesPerBlock, %bx
        shrw $4, %bx /* bx is the increment of es after a block loaded */

        /* load one block of the target file */
        findInCurrentPartition.loadOneBlock:
            call dataBlock.getNext
            jcxz findInCurrentPartition.success
            jc findInCurrentPartition.fail
            movw sectorsPerBlock, %cx
            call loadSectorsByBlockIndex
            jc findInCurrentPartition.fail
            movw %es, %ax
            addw %bx, %ax
            movw %ax, %es
            jmp findInCurrentPartition.loadOneBlock

    findInCurrentPartition.success:
        clc
        jmp findInCurrentPartition.return
    findInCurrentPartition.fail:
        stc
    findInCurrentPartition.return:
        pop %es
        pop %si
        pop %di
        pop %edx
        pop %cx
        pop %bx
        pop %eax
        ret
    findInCurrentPartition.itemIsFile: .byte 0

/* Given the inode index of a directory, get the inode index of an item
 * Input: eax = index of the directory's inode
 *        si = pointer of the item's name
 *        cx = name length(since length < 256, actually, only cl is used.)
 * Output:clear cf if found, set cf on miss
 *        eax = the child item's inode index
 */
findChildItem:
    push %bx
    push %dx
    push %di
    call dataBlock.reset
    jc findChildItem.return

    findChildItem.oneBlock:
        /* note bx is free now, use it to save and restor cx is more convinient
         * because unnecessary to consider the balance of stack 
         */
        mov %cx, %bx 
        call dataBlock.getNext
        jc findChildItem.return
        movw $(LOAD_DESTINATION + MAX_BLOCK_SIZE), %di
        subw bytesPerBlock, %di
        movw sectorsPerBlock, %cx
        call loadSectorsByBlockIndex
        jc findChildItem.return
        movw %bx, %cx

        /* check every entry one by one and attempt to find the right one */
        findChildItem.oneBlock.oneEntry:
            movl (%di), %eax /* note we make eax inode index and never change */
            testl %eax, %eax
            jz findChildItem.oneBlock.oneEntry.nextEntry
            cmpb 6(%di), %cl
            jne findChildItem.oneBlock.oneEntry.nextEntry
            movw %cx, %bx

            /* compare one char of the given item name and current entry */
            findChildItem.oneBlock.oneEntry.oneChar:
                decw %bx
                movb (%bx, %si), %dl
                cmpb 8(%bx, %di), %dl
                jne findChildItem.oneBlock.oneEntry.nextEntry
                testw %bx, %bx
                jz findChildItem.found /* coincident, give a positive return */
                jmp findChildItem.oneBlock.oneEntry.oneChar

            /* Not this entry, try next one(if there exists) */
            findChildItem.oneBlock.oneEntry.nextEntry:
                movw 4(%di), %bx
                addw %bx, %di
                cmp $(LOAD_DESTINATION + MAX_BLOCK_SIZE), %di
                jnb findChildItem.oneBlock
                jmp findChildItem.oneBlock.oneEntry

    findChildItem.found:
        clc
    findChildItem.return:
        pop %di
        pop %dx
        pop %bx
        ret

/* The most complex but well packaged function of this program
 * It tries to analize the inode structure and give the data block's index
 * includes three subfunctions .reset .getLeftBlockCount and .getNext
 */
dataBlock:
    dataBlock.inode: .word 0
    dataBlock.leftBlockCount: .long 0
    dataBlock.currentRecord: .byte 0
    dataBlock.1stPointer: .word 0
    dataBlock.2ndPointer: .word 0
    dataBlock.3rdPointer: .word 0

    /* One of the subfunctions, to reset all variables and do some other work
     * Input: eax = index of inode
     * Output:clear cf if no error, set cf on error
     */
    dataBlock.reset:
        push %eax
        push %edx
        push %cx
        push %si
        push %di

        /* The inode structure should be prepared first in the buffer */
        call loadInodeByIndex
        jc dataBlock.reset.return
        movw %si, dataBlock.inode /* pointer to the structure in the buffer */

        /* calculate its block count by byte size */
        movl 0x4(%si), %eax
        xorl %edx, %edx
        divl bytesPerBlock
        addl $0xffffffff, %edx
        adcl $0, %eax /* not a full block but regarded as one block */
        movl %eax, dataBlock.leftBlockCount

        addw $0x28, %si /* si point to the block records inside inode */ 
        movw $(BLOCK_POINTER_BUFFER1_END - 12 * 4), %di
        movw $12, %cx
        cld
        rep
        movsl

        movb $12, dataBlock.currentRecord
        movw $(BLOCK_POINTER_BUFFER1_END - 12 * 4), dataBlock.1stPointer
        movw $BLOCK_POINTER_BUFFER2_END, dataBlock.2ndPointer
        movw $BLOCK_POINTER_BUFFER3_END, dataBlock.3rdPointer

        pop %di
        pop %si
        pop %cx
        pop %edx
        pop %eax

        dataBlock.reset.return:
            ret

    /* Another simple subfunction to return the left block count with eax */
    dataBlock.getLeftBlockCount:
        movl dataBlock.leftBlockCount, %eax
        ret

    /* The last subfunction, get the next block's index one by one after reset
     * Input: NONE, only mind dataBlock.reset should be called at first
     * Output:clear cf if and only if gives the next, else set cf(eof or error)
     *        cx = 0 if and only if reaches eof, else cx = 1(success or error)
     *        eax = the next block's index(makes sense when cf = 0 and cx = 1)
     */
    dataBlock.getNext:
        cmpw $BLOCK_POINTER_BUFFER1_END, dataBlock.1stPointer
        jb dataBlock.getNext.get
        jmp dataBlock.refreshBuffer1 /* out of boundary, refresh it first */

        /* the action of get the next from the buffer */
        dataBlock.getNext.get:
            cmpl $0, dataBlock.leftBlockCount
            je dataBlock.getNext.eof
            decl dataBlock.leftBlockCount
            push %ebx
            push %di
            movw dataBlock.1stPointer, %di
            movl (%di), %eax
            addw $4, dataBlock.1stPointer
            movl bytesPerBlock, %ebx
            pop %di
            pop %ebx

        dataBlock.getNext.success:
            movw $1, %cx
            clc
            ret
        dataBlock.getNext.eof:
            movw $0, %cx
            stc
            ret
        dataBlock.getNext.fail:
            movw $1, %cx
            stc
            ret

        /* process of refresh the first block buffer by the second one */
        dataBlock.refreshBuffer1:
            /* we refresh the first by the second, but when the second is out of
             * boundary, the second buffer itself should be refresh in advance
             */
            cmpw $BLOCK_POINTER_BUFFER2_END, dataBlock.2ndPointer
            jb dataBlock.refreshBuffer1.refresh
            jmp dataBlock.refreshBuffer2

            dataBlock.refreshBuffer1.refresh:
                push %eax
                push %cx
                push %di
                movw dataBlock.2ndPointer, %di
                movl (%di), %eax
                addw $4, dataBlock.2ndPointer
                movw $BLOCK_POINTER_BUFFER1_END, %di
                subw bytesPerBlock, %di
                movw %di, dataBlock.1stPointer
                movw sectorsPerBlock, %cx
                call loadSectorsByBlockIndex
                pop %di
                pop %cx
                pop %eax
                jc dataBlock.getNext.fail
                jmp dataBlock.getNext

        /* like 2nd buffer to refresh 1st one, here 3rd to refresh 2nd one */
        dataBlock.refreshBuffer2:
            cmpw $BLOCK_POINTER_BUFFER3_END, dataBlock.3rdPointer
            jb dataBlock.refreshBuffer2.refresh
            jmp dataBlock.refreshBuffer3

            dataBlock.refreshBuffer2.refresh:
                push %eax
                push %cx
                push %di
                movw dataBlock.3rdPointer, %di
                movl (%di), %eax
                addw $4, dataBlock.3rdPointer
                movw $BLOCK_POINTER_BUFFER2_END, %di
                subw bytesPerBlock, %di
                movw %di, dataBlock.2ndPointer
                movw sectorsPerBlock, %cx
                call loadSectorsByBlockIndex
                pop %di
                pop %cx
                pop %eax
                jc dataBlock.getNext.fail
                jmp dataBlock.refreshBuffer1

        /* Tiresome, 3rd buffer itself sometimes need to refresh by analizing 
         * the inode structure.
         */
        dataBlock.refreshBuffer3:
            cmpb $12, dataBlock.currentRecord
            je dataBlock.refreshBuffer3.singlyIndirect
            cmpb $13, dataBlock.currentRecord
            je dataBlock.refreshBuffer3.doublyIndirect
            cmpb $14, dataBlock.currentRecord
            je dataBlock.refreshBuffer3.triplyIndirect
            jmp dataBlock.getNext.fail

            /* the 13th block pointer record is a singly indirect one */
            dataBlock.refreshBuffer3.singlyIndirect:
                incb dataBlock.currentRecord
                push %eax
                push %di
                movw dataBlock.inode, %di
                movl 0x28+12*4(%di), %eax
                movl %eax, BLOCK_POINTER_BUFFER2_END - 4
                pop %di
                pop %eax
                movw $(BLOCK_POINTER_BUFFER2_END - 4), dataBlock.2ndPointer
                jmp dataBlock.refreshBuffer1

            /* the 14th block pointer record is a doubly indirect one */
            dataBlock.refreshBuffer3.doublyIndirect:
                incb dataBlock.currentRecord
                push %eax
                push %di
                movw dataBlock.inode, %di
                movl 0x28+13*4(%di), %eax
                movl %eax, BLOCK_POINTER_BUFFER3_END - 4
                pop %di
                pop %eax
                movw $(BLOCK_POINTER_BUFFER3_END - 4), dataBlock.3rdPointer
                jmp dataBlock.refreshBuffer2

            /* the 15th block pointer record is a triply indirect one */
            dataBlock.refreshBuffer3.triplyIndirect:
                incb dataBlock.currentRecord
                push %eax
                push %cx
                push %di
                movw dataBlock.inode, %di
                movl 0x28+14*4(%di), %eax
                movw $BLOCK_POINTER_BUFFER3_END, %di
                subw bytesPerBlock, %di
                movw %di, dataBlock.3rdPointer
                movw sectorsPerBlock, %cx
                call loadSectorsByBlockIndex
                pop %di
                pop %cx
                pop %eax
                jc dataBlock.getNext.fail
                jmp dataBlock.refreshBuffer2


/* Load an inode structure by its index
 * Input: eax = index of inode
 * Output:clear cf if no error, set cf on error
 *       si = pointer of the in inode structure buffer
 */
loadInodeByIndex:
    push %eax
    push %ebx
    push %cx
    push %edx
    push %di

    /* make eax = index of block group where this inode is located
     * also get the offset of this inode at its group
     */
    decl %eax
    xorl %edx, %edx
    divl SUPER_BLOCK_BUFFER + 0x28 /* inode count per group */
    movl %edx, loadInodeByIndex.inodeOffsetAtGroup

    /* load the propriate portion of GDT to buffer by eax's value
     * make item of this inode's group is the 'edx'th one in the loaded GDT
     */
    xorl %edx, %edx
    movl $(SECTOR_SIZE / GDT_ITEM_SIZE), %ebx
    divl %ebx
    addl firstGDTSector, %eax
    movw $1, %cx
    movw $GDT_BUFFER, %di
    call loadSectors
    jc loadInodeByIndex.fail

    /* get the first sector's LBA of the inode table of this inode's group */
    movl %edx, %eax
    movw $GDT_ITEM_SIZE, %bx
    mulw %bx
    addw %ax, %di
    movl 0x8(%di), %eax
    xorl %edx, %edx
    mull sectorsPerBlock
    addl partitionFirstSector, %eax
    movl %eax, loadInodeByIndex.inodeTableFirstSector

    /* make eax = LBA value of the inode's sector
     * make edx = byte offset of the inode item in the sector
     */
    xorl %eax, %eax
    movw SUPER_BLOCK_BUFFER + 0x58, %ax
    mull loadInodeByIndex.inodeOffsetAtGroup
    movl $SECTOR_SIZE, %ebx
    divl %ebx
    addl loadInodeByIndex.inodeTableFirstSector, %eax

    /* load the sector to buffer and make si point to the inode */
    movw $1, %cx
    movw $INODE_TABLE_BUFFER, %di
    call loadSectors
    jc loadInodeByIndex.fail
    movw %di, %si
    addw %dx, %si
    clc
    jmp loadInodeByIndex.return

    loadInodeByIndex.fail:
        stc
    loadInodeByIndex.return:
        pop %di
        pop %edx
        pop %cx
        pop %ebx
        pop %eax
        ret

    loadInodeByIndex.inodeOffsetAtGroup: .long 0
    loadInodeByIndex.inodeTableFirstSector: .long 0

/* Padding the last sector of program */
padLastSector:
    .fill 511 - (padLastSector - _start - 1) % SECTOR_SIZE, 1, 0
_end:

