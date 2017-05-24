 ; =============================================================================
 ;
 ; Load the a boot file with given path to 0:0x7c00 by analizing ext2 filesystem
 ;
 ;     This program is aimed at writing a booter for MBR based storage devices
 ; (mainly for a usb flash drive). If witten to the MBR(and next few sectors),
 ; it can load the file located at the certain given path in the devices by
 ; resolving partition table and ext2 file system structure.
 ;     If found, the target file will be loaded to 0:0x7c00 and be executed,
 ; else an error will be reported.
 ;     By this way can we realize a two-stage boot-up. The loaded program can be
 ; update simply by change the file in the disk, and its size limit increases to
 ; 608KB(seems to be rich enough for initializing and loading kernel).
 ;
 ; =============================================================================

 ; ================================requirement=================================
 ;     ONLY ext2 with revision 1 and higher is supported
 ;
 ;     Block size should never be larger than 8kB(this is commonly satisfied)
 ;
 ;     Size of the occupied blocks of target file(not the file size) limits to
 ; 608KB, but since 608/8=integer, it can also be regarded as a file size limit.
 ; =============================================================================


    MAX_BLOCK_SIZE equ 8 * 1024
    GDT_ITEM_SIZE equ 0x20
    SECTOR_SIZE equ 0x200
    EXT2_MAGIC_NUMBER equ 0xef53

    PROGRAM_ADDRES equ 0x500
    STACK_BOTTOM equ 0x1600
    SUPER_BLOCK_BUFFER equ 0x1600
    GDT_BUFFER equ 0x1800
    INODE_TABLE_BUFFER equ 0x1a00
    
    BLOCK_POINTER_BUFFER1 equ 0x1c00
    BLOCK_POINTER_BUFFER1_END equ BLOCK_POINTER_BUFFER1 + MAX_BLOCK_SIZE
    BLOCK_POINTER_BUFFER2 equ 0x3c00
    BLOCK_POINTER_BUFFER2_END equ BLOCK_POINTER_BUFFER2 + MAX_BLOCK_SIZE
    BLOCK_POINTER_BUFFER3 equ 0x5c00
    BLOCK_POINTER_BUFFER3_END equ BLOCK_POINTER_BUFFER3 + MAX_BLOCK_SIZE
    
    LOAD_DESTINATION equ 0x7c00
    LOAD_DESTINATION_END equ 0x9fc00

    ;Two macro function to make it easy to push and pop
    ;EASY_PUSH ax,bx,cx,... = {push ax}{push bx}{push cx} ...
    ;EASY_POP ax,bx,cx,... = ...{pop cx}{pop bx}{pop ax}

    %macro EASY_PUSH 1-*
        %rep %0
            push %1
            %rotate 1
        %endrep
    %endmacro

    %macro EASY_POP 1-*
        %rep %0
            %rotate -1
            pop %1
        %endrep
    %endmacro

    bits 16
    org PROGRAM_ADDRES

;Jmp statement and some global variables
program.start:
    jmp initialize

    ;absolute path of the target file, MUST include a terminating null character
    targetFilePath:
        db `/boot/loader.bin\x00`
    targetFilePath.end:

    ;drive index is used when load sectors from the physical drive by int 0x13
    driveIndex: db 0

    ;some geometric parameters about current partition
    partitionFirstSector: dd 0
    sectorsPerBlock: dd 0
    bytesPerBlock: dd 0
    firstGDTSector: dd 0

initialize:
    xor ax,ax
    mov ds,ax
    mov es,ax
    cli
    mov ss,ax
    mov sp,STACK_BOTTOM
    sti

    ;When MBR is just loaded to 0x7c00 by BIOS, drive index is saved in dl,
    ;thus we use it to initialize "driveIndex" and load the all the code
    mov [driveIndex],dl
    xor eax,eax
    mov cx,(program.end - program.start) / SECTOR_SIZE
    mov di,PROGRAM_ADDRES
    call loadSectors
    jc fail;read fail
    mov [driveIndex],dl;since it has been overwrited, write for the second time
    jmp startToLoad + PROGRAM_ADDRES - LOAD_DESTINATION

;What to do if fails to load
fail:
    ;clear screen
    mov ax,0x0600
    mov bh,0b00001100
    xor cx,cx
    mov dx,(25 - 1) * 0x100 + (80 - 1)
    int 0x10
    ;print error infomation
    mov ax,0x1301
    xor bh,bh

    mov bl,0b00001100
    mov cx,.infoText.end - .infoText
    mov dx,0x100
    mov bp,.infoText
    int 0x10

    mov bl,0b00011100
    mov cx,targetFilePath.end - targetFilePath - 1
    mov dx,((.infoText.end - .infoText) / 80 + 3) * 0x100 + 4
    mov bp,targetFilePath
    int 0x10

    .endlessLoop:
        hlt
        jmp .endlessLoop
    .infoText:
        db `ERROR, fail to load the boot file with the give path:`
        .infoText.end:


;Load certain sectors to mermory
;Input: eax = LBA value of the start sector
;       cx = number of sectors to read
;       es:di = pointer of the buffer to where sectors will be transferred
;Output:clear cf if no error, set cf on error
loadSectors:
    EASY_PUSH eax,dx,si
    mov [.dap.sectorCount],cx
    mov [.dap.desOff],di
    mov [.dap.desSeg],es
    mov [.dap.LBA],eax
    mov si,.dap
    mov dl,[driveIndex]
    mov ah,0x42
    int 0x13
    EASY_POP eax,dx,si
    ret
    .dap:
        db 0x10
        db 0
        .dap.sectorCount: dw 0
        .dap.desOff: dw 0
        .dap.desSeg: dw 0
        .dap.LBA: dd 0
        dd 0

;eax is block index, but cx is still sector count instead of block count
loadSectorsByBlockIndex:
    EASY_PUSH eax,edx
    mul dword [sectorsPerBlock]
    add eax,[partitionFirstSector]
    call loadSectors
    EASY_POP eax,edx
    ret

;Initialization is done and start to find the target file partition by partition
;Once found a EXT2 partition, initialize all the geometric parameters about it
;Then calls findInCurrentPartition to try in this partition
;If success, jumps and executes, otherwise gives a failure output
startToLoad:
    mov bp,0x1be + 0x08
    .onePartition:
        ;check is there any partition left and initialize "partitionFirstSector"
        cmp bp,SECTOR_SIZE
        jg fail
        mov eax,[PROGRAM_ADDRES + bp]
        mov [partitionFirstSector],eax
        test eax,eax
        jz .nextPartition
        ;load superblock
        add eax,2
        mov cx,1
        mov di,SUPER_BLOCK_BUFFER
        call loadSectors
        jc .nextPartition
        ;check magic number to ensure it is an ext2 filesystem partition
        cmp [SUPER_BLOCK_BUFFER + 0x38], word EXT2_MAGIC_NUMBER
        jne .nextPartition
        ;check is it revision 1 or later
        cmp [SUPER_BLOCK_BUFFER + 0x4c], word 1
        jb .nextPartition
        ;initialize "bytesPerBlock"
        mov eax,1024
        mov cl,[SUPER_BLOCK_BUFFER + 0x18]
        shl eax,cl
        mov [bytesPerBlock],eax
        cmp eax,MAX_BLOCK_SIZE
        ja .nextPartition;block size larger than 8K isn't supported
        ;initialize "sectorsPerBlock"
        xor edx,edx
        mov ebx,SECTOR_SIZE
        div ebx
        mov [sectorsPerBlock],eax
        ;initialize "firstGDTSector"
        mov ebx,eax
        mov eax,[SUPER_BLOCK_BUFFER + 0x14];first data block
        inc eax
        mul ebx
        add eax, [partitionFirstSector]
        mov [firstGDTSector],eax
        ;if find the file in the partition, jump to execute
        call findInCurrentPartition
        jc .nextPartition
        mov dl,[driveIndex]
        jmp LOAD_DESTINATION
    .nextPartition:
        add bp,0x10
        jmp .onePartition

;According to the requirement of MBR
;64 bytes for partition table should reserved
;terminating 55 aa serves as a signature
padFirstSector:
    %if (padFirstSector - program.start > 512 - 16 * 4 - 2)
        %error "the last 64+2 bits of first sector should be reserved"
    %endif
    times 512 - 2 - (padFirstSector - program.start) db 0x00
    dw 0xaa55

;Try to find and load the target file in one partition
;Input: prepared global geometric parameter variables
;Output:clear cf if success, set cf not found
findInCurrentPartition:
    EASY_PUSH eax,bx,cx,edx,di,si,es
    mov eax,2;root directory's inode index
    mov si,targetFilePath
    .enterDir:
        inc si;skip the '/'
        xor bx,bx
        .enterDir.nameNextChar:
            cmp [si + bx],byte '/'
            je .enterDir.dirNameEnd
            cmp [si + bx],byte 0
            je .enterDir.fileNameEnd
            inc bx
            jmp .enterDir.nameNextChar
        .enterDir.fileNameEnd:
            mov [.itemIsFile],byte 1
        .enterDir.dirNameEnd:
            mov cx,bx
        ;call findChildItem and determine whether it is a file or a directory
        call findChildItem
        jc .fail
        add si,cx
        cmp [.itemIsFile],byte 1
        jne .enterDir
        ;eventually, it reaches the target file and now start to load it
        call dataBlock.reset
        jc .fail
        ;refuse to load a file with too large size
        call dataBlock.getLeftBlockCount
        mul dword [bytesPerBlock]
        cmp eax,LOAD_DESTINATION_END - LOAD_DESTINATION
        ja .fail
        ;Since the allowable size is larger than 0xffff,
        ;we should make es instead di increase after every block been loaded
        mov ax,word LOAD_DESTINATION / 0x10
        mov es,ax
        xor di,di
        mov bx,[bytesPerBlock]
        shr bx,4;bx is the needed increment of es after a block been loaded
        ;load one block of the target file
        .loadOneBlock:
            call dataBlock.getNext
            jcxz .success
            jc .fail
            mov cx,[sectorsPerBlock]
            call loadSectorsByBlockIndex
            jc .fail
            mov ax,es
            add ax,bx
            mov es,ax
            jmp .loadOneBlock
    .success:
        clc
        jmp .return
    .fail:
        stc
    .return:
        EASY_POP eax,bx,cx,edx,di,si,es
        ret
    .itemIsFile: db 0

;Given the inode index of a directory, get the inode index of an item
;Input: eax = index of the directory's inode
;       si = pointer of the item's name
;       cx = name length(since length < 256, actually, only cl is used.)
;Output:clear cf if found, set cf on miss
;       eax = the child item's inode index
findChildItem:
    EASY_PUSH bx,dx,di
    call dataBlock.reset
    jc .return
    .oneBlock:
        mov bx,cx
        call dataBlock.getNext
        jc .return
        mov di,LOAD_DESTINATION + MAX_BLOCK_SIZE
        sub di,[bytesPerBlock]
        mov cx,[sectorsPerBlock]
        call loadSectorsByBlockIndex
        jc .return
        mov cx,bx
        ;Check every entry one by one and attempt to find the right one
        .oneBlock.oneEntry:
            mov eax,[di];note we make eax = inode index and not change it
            test eax,eax
            jz .oneBlock.oneEntry.nextEntry
            cmp cl,[di + 6]
            jne .oneBlock.oneEntry.nextEntry
            mov bx,cx
            ;compare one char of the given item name and current entry
            .oneBlock.oneEntry.oneChar:
                dec bx
                mov dl,[si + bx]
                cmp dl,[di + 8 + bx]
                jne .oneBlock.oneEntry.nextEntry
                test bx,bx
                jz .found;coincident, give a positive return
                jmp .oneBlock.oneEntry.oneChar
            ;Not this entry, try next one(if there exists)
            .oneBlock.oneEntry.nextEntry:
                mov bx,[di + 4]
                add di,bx
                cmp di,LOAD_DESTINATION + MAX_BLOCK_SIZE
                jnb .oneBlock
                jmp .oneBlock.oneEntry
    .found:
        clc
    .return:
        EASY_POP bx,dx,di
        ret

;The most complex but well packaged function of this program
;It tries to analize the inode structure and give the data block's index
;includes three subfunctions .reset .getLeftBlockCount and .getNext
dataBlock:
    .inode: dw 0
    .leftBlockCount: dd 0
    .currentRecord: db 0
    .1stPointer: dw 0
    .2ndPointer: dw 0
    .3rdPointer: dw 0

    ;One of the subfunctions, aims to reset all variables and do some other work
    ;Input: eax = index of inode
    ;Output:clear cf if no error, set cf on error
    .reset:
        EASY_PUSH eax,edx,cx,si,di
        ;The inode structure should be prepared first in the buffer
        call loadInodeByIndex
        jc .reset.return
        mov [.inode],si;pointer to the structure in the buffer
        ;calculate its block count by byte size
        mov eax,[si + 0x4]
        xor edx,edx
        div dword [bytesPerBlock]
        add edx,0xffffffff
        adc eax,0;not a full block but regarded as one block
        mov [.leftBlockCount],eax
        add si,0x28;pointer of the block pointers inside inode structure
        mov di,BLOCK_POINTER_BUFFER1_END - 12 * 4;12 direct blocks
        mov cx,12
        cld
        rep movsd
        mov [.currentRecord],byte 12
        mov [.1stPointer],word BLOCK_POINTER_BUFFER1_END - 12 * 4
        mov [.2ndPointer],word BLOCK_POINTER_BUFFER2_END
        mov [.3rdPointer],word BLOCK_POINTER_BUFFER3_END
        EASY_POP eax,edx,cx,si,di
        .reset.return:
            ret

    ;Another simple subfunction to return the left block count with eax
    .getLeftBlockCount:
        mov eax,[.leftBlockCount]
        ret

    ;The last subfunction, get the next block's index one by one after reset
    ;Input: NONE, only mind dataBlock.reset should be called at first
    ;Output:clear cf if and only if gives the next, else set cf(eof or error)
    ;       cx = 0 if and only if reaches eof, else cx = 1(success or error)
    ;       eax = the next block's index(makes sense when cf = 0 and cx = 1)
    .getNext:
        cmp [.1stPointer],word BLOCK_POINTER_BUFFER1_END;check buffer boundary
        jb .getNext.get
        jmp .refreshBuffer1;out of boundary, refresh it first
        ;the action of get the next from the buffer
        .getNext.get:
            cmp [.leftBlockCount],dword 0
            je .getNext.eof
            dec dword [.leftBlockCount]
            EASY_PUSH ebx,di
            mov di,[.1stPointer]
            mov eax,[di]
            add [.1stPointer],word 4
            mov ebx,[bytesPerBlock]
            EASY_POP ebx,di
        .getNext.success:
            mov cx,1
            clc
            ret
        .getNext.eof:
            mov cx,0
            stc
            ret
        .getNext.fail:
            mov cx,1
            stc
            ret

        ;process of refresh the first block buffer by the second one
        .refreshBuffer1:
            ;we refresh the first by the second, but when the second is out of
            ;boundary, the second buffer itself should be refresh in advance
            cmp [.2ndPointer],word BLOCK_POINTER_BUFFER2_END
            jb .refreshBuffer1.refresh
            jmp .refreshBuffer2
            .refreshBuffer1.refresh:
                EASY_PUSH eax,cx,di
                mov di,[.2ndPointer]
                mov eax,[di]
                add [.2ndPointer],word 4
                mov di,word BLOCK_POINTER_BUFFER1_END
                sub di,[bytesPerBlock]
                mov [.1stPointer],di
                mov cx,[sectorsPerBlock]
                call loadSectorsByBlockIndex
                EASY_POP eax,cx,di
                jc .getNext.fail
                jmp .getNext

        ;like 2nd buffer to refresh 1st one, here 3rd to refresh 2nd one
        .refreshBuffer2:
            cmp [.3rdPointer],word BLOCK_POINTER_BUFFER3_END
            jb .refreshBuffer2.refresh
            jmp .refreshBuffer3
            .refreshBuffer2.refresh:
                EASY_PUSH eax,cx,di
                mov di,[.3rdPointer]
                mov eax,[di]
                add [.3rdPointer],word 4
                mov di,word BLOCK_POINTER_BUFFER2_END
                sub di,[bytesPerBlock]
                mov [.2ndPointer],di
                mov cx,[sectorsPerBlock]
                call loadSectorsByBlockIndex
                EASY_POP eax,cx,di
                jc .getNext.fail
                jmp .refreshBuffer1

        ;Tiresome, 3rd buffer itself is refresh by analizing the inode structure
        .refreshBuffer3:
            cmp [.currentRecord],byte 12
            je .refreshBuffer3.singlyIndirect
            cmp [.currentRecord],byte 13
            je .refreshBuffer3.doublyIndirect
            cmp [.currentRecord],byte 14
            je .refreshBuffer3.triplyIndirect
            jmp .getNext.fail
            ;the 13th block pointer record is a singly indirect one
            .refreshBuffer3.singlyIndirect:
                inc byte [.currentRecord]
                EASY_PUSH eax,di
                mov di,[.inode]
                mov eax,[di + 0x28 + 12 * 4]
                mov [BLOCK_POINTER_BUFFER2_END - 4],eax
                EASY_POP eax,di
                mov [.2ndPointer],word BLOCK_POINTER_BUFFER2_END - 4
                jmp .refreshBuffer1
            ;the 14th block pointer record is a doubly indirect one
            .refreshBuffer3.doublyIndirect:
                inc byte [.currentRecord]
                EASY_PUSH eax,di
                mov di,[.inode]
                mov eax,[di + 0x28 + 13 * 4]
                mov [BLOCK_POINTER_BUFFER3_END - 4],eax
                EASY_POP eax,di
                mov [.3rdPointer],word BLOCK_POINTER_BUFFER3_END - 4
                jmp .refreshBuffer2
            ;the 15th block pointer record is a triply indirect one
            .refreshBuffer3.triplyIndirect:
                inc byte [.currentRecord]
                EASY_PUSH eax,cx,di
                mov di,[.inode]
                mov eax,[di + 0x28 + 14 * 4]
                mov di,word BLOCK_POINTER_BUFFER3_END
                sub di,[bytesPerBlock]
                mov [.3rdPointer],di
                mov cx,[sectorsPerBlock]
                call loadSectorsByBlockIndex
                EASY_POP eax,cx,di
                jc .getNext.fail
                jmp .refreshBuffer2


;Load an inode structure by its index
;Input: eax = index of inode
;Output:clear cf if no error, set cf on error
;       si = pointer of the in inode structure buffer
loadInodeByIndex:
    EASY_PUSH eax,ebx,cx,edx,di
    ;eax = index of block group where this inode is
    ;get the offset of this inode at its group
    dec eax
    xor edx,edx
    div dword [SUPER_BLOCK_BUFFER + 0x28];inode per group
    mov [.inodeOffsetAtGroup],edx
    ;load the propriate portion of GDT to buffer by eax's value
    ;edx : item of this inode's group is the 'edx'th one in the loaded GDT
    xor edx,edx
    mov ebx,SECTOR_SIZE / GDT_ITEM_SIZE
    div ebx
    add eax,[firstGDTSector]
    mov cx,1
    mov di,GDT_BUFFER
    call loadSectors
    jc .fail
    ;get the first sector's LBA value of the inode table of this inode's group
    mov eax,edx
    mov bx,GDT_ITEM_SIZE
    mul bx
    add di,ax
    mov eax,[di + 0x8]
    xor edx,edx
    mul dword [sectorsPerBlock]
    add eax,[partitionFirstSector]
    mov [.inodeTableFirstSector],eax
    ;eax = LBA value of the inode's sector
    ;edx = byte offset of the inode item in the sector
    xor eax,eax
    mov ax,[SUPER_BLOCK_BUFFER + 0x58];inode size
    mul dword [.inodeOffsetAtGroup]
    mov ebx,SECTOR_SIZE
    div ebx
    add eax,[.inodeTableFirstSector]
    ;load the sector to buffer and make si point to the inode
    mov cx,1
    mov di,INODE_TABLE_BUFFER
    call loadSectors
    jc .fail
    mov si,di
    add si,dx
    clc
    jmp .return
    .fail:
        stc
    .return:
        EASY_POP eax,ebx,cx,edx,di
        ret
    .inodeOffsetAtGroup: dd 0
    .inodeTableFirstSector: dd 0

;Padding the last sector of program
padLastSector:
    %if (padLastSector - program.start > \
        STACK_BOTTOM - PROGRAM_ADDRES - SECTOR_SIZE)
            %error "The program is too large to leave enough space for stack"
    %endif
    %if ((padLastSector - program.start) % SECTOR_SIZE <> 0)
        times 512 - (padLastSector - program.start) % SECTOR_SIZE db 0x00
    %endif
    
program.end:
