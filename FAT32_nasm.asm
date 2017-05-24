;Load the code located in the file with given path to 0:0x7c00
;No size limit for the file, but since is loaded to 0x7c00
;mind memories after about 0x9ffff are commonly reserved for BIOS
;Only FAT32 file system is supported


    MBR_OFF equ 0x0800
    STACKTOP_OFF equ MBR_OFF
    LOAD_OFF equ 0x7C00
    SECTOR_SIZE equ 0x200

    org MBR_OFF

program_start:

    jmp initialize

    driveIndex: db 0

    supportedFileSystem: db `FAT32   `

    partition.startSector: dd 0
    partition.rootCluster: dd 0
    partition.sectorsPerClustor: db 0
    partition.sectorsPerFAT: dd 0
    partition.FATCount: db 0
    partition.reservedSectors: dw 0
    partition.clusterSize: dw 0

    filePath:
        db `boot       `
        db `boot    bin`
        filePath.end:

;Initialize some segment register and stack point
;Move the loaded and executing MBR code to 0:MBR_OFF, and jump to there
;Back up the drive index as soon as possible since it is vital for disk read
initialize:
    cli
    xor ax,ax
    mov ds,ax
    mov es,ax
    mov ss,ax
    mov sp,STACKTOP_OFF
    sti
    mov si,LOAD_OFF
    mov di,MBR_OFF
    mov cx,SECTOR_SIZE / 2
    cld
    rep movsw
    mov [driveIndex],dl
    jmp loadRestCode - LOAD_OFF + MBR_OFF

;What to do if fails to load
fail:
    mov ah,0x03
    xor bh,bh
    int 0x10
    mov cx,fail.infoText.end - fail.infoText
    mov bp,fail.infoText
    mov bl,0b00001100
    mov ax,0x1301
    int 0x10
    fail.loop:
        hlt
        jmp fail.loop
    fail.infoText: db `Fail to load file!\x00`
        fail.infoText.end:

;Since only a SECTOR_SIZE of code has been loaded, the rest should be loaded by ourselves
loadRestCode:
    mov cx,(program_end - program_start) / SECTOR_SIZE - 1
    mov di,MBR_OFF + SECTOR_SIZE
    mov ax,1
    xor bx,bx
    call loadSectors
    jcxz fail

;Find the file with certain path and name in the drive where boot sector is located
;Success: code in the file is loaded at 0:0x7c00, and program will jumps to there
;Fail: jump to fail
findInDrive:
    mov si,MBR_OFF + 0x01be - 0x10
    findInDrive.onePartition:
        add si,0x10
        cmp si,MBR_OFF + SECTOR_SIZE - 2
        jnb fail
        mov ax,[si + 0x08]
        mov bx,[si + 0x0a]
        call checkPartition
        jcxz findInDrive.onePartition
        call findInPartition
        jcxz findInDrive.onePartition
        mov dl,[driveIndex]
        jmp LOAD_OFF

;Analize data at the DBR which located at the first sector of partition,
;determine is it supported file system ,and write all the information needed to gloable variables
;Input: ax&bx = first sector of partition
;Success: information about partition is written to the gloable variables and cx = 1
;Fail��cx = 0 (read error or unsupported file system leads to a failure)
checkPartition:
    push ax
    push bx
    push dx
    push si
    push di

    mov di,LOAD_OFF
    mov [partition.startSector],ax
    mov [partition.startSector + 2],bx
    mov cx,1
    call loadSectors
    jcxz checkPartition.fail

    mov si,supportedFileSystem
    mov di,LOAD_OFF + 0x52
    mov cx,8
    call stringCompare
    jcxz checkPartition.fail

    mov ax,[LOAD_OFF + 0x2c]
    mov bx,[LOAD_OFF + 0x2c + 2]
    mov [partition.rootCluster],ax
    mov [partition.rootCluster + 2],bx

    mov ax,[LOAD_OFF + 0x0e]
    mov [partition.reservedSectors],ax

    mov al,[LOAD_OFF + 0x10]
    mov [partition.FATCount],al

    mov ax,[LOAD_OFF + 0x24]
    mov bx,[LOAD_OFF + 0x24 + 2]
    mov [partition.sectorsPerFAT],ax
    mov [partition.sectorsPerFAT + 2],bx

    mov al,[LOAD_OFF + 0x0d]
    mov [partition.sectorsPerClustor],al
    xor ah,ah
    mov dx, word SECTOR_SIZE
    mul dx
    mov [partition.clusterSize],ax

    mov cx,1
    jmp checkPartition.finish
    checkPartition.fail:
        xor cx,cx
    checkPartition.finish:
        pop di
        pop si
        pop dx
        pop bx
        pop ax
        ret

;Try to find the file in the partition whose information is prepared
;Input: prepared infomation about this partition
;Success: all code in the file is loaded to 0x7c00 and cx = 1
;Fail��cx = 0
findInPartition:
    push ax
    push bx
    push dx
    push di
    push si
    push es

    mov ax,[partition.rootCluster]
    mov bx,[partition.rootCluster + 2]
    mov si,filePath
    mov di,LOAD_OFF
    findInPartition.enterOneDir:
        cmp si,filePath.end
        jnb findInPartition.found
        call findEntry
        jcxz findInPartition.fail
        add si,11
        jmp findInPartition.enterOneDir
    findInPartition.found:
        push ax
        push bx
        mov ax,[partition.clusterSize]
        xor dx,dx
        mov bx,0x10
        div bx
        mov dx,ax
        pop bx
        pop ax
        findInPartition.found.loadOneCluster:
            call loadCluster
            jcxz findInPartition.fail
            mov cx,es
            add cx,dx
            mov es,cx
            call nextClusterIndex
            jcxz findInPartition.fail
            cmp cx,1
            je findInPartition.found.loadOneCluster
            mov cx,1
            jmp findInPartition.finish
    findInPartition.fail:
        xor cx,cx
    findInPartition.finish:
        pop es
        pop si
        pop di
        pop dx
        pop bx
        pop ax
        ret

;Load certain sectors into selected position
;Input: ax&bx = LBA value of the start sector
;       cx = number of sectors to read
;       es:di = pointer to the memory buffer to which sectors will be transferred
;Success: cx = 1
;Fail��cx = 0
loadSectors:
    push ax
    push bx
    push dx
    push si
    mov [loadSectors.dap.sectorCount],cx
    mov [loadSectors.dap.desOff],di
    mov [loadSectors.dap.desSeg],es
    mov [loadSectors.dap.LBA],ax
    and bh,0x0F
    mov [loadSectors.dap.LBA + 2],bx
    mov dl,[driveIndex]
    mov si,loadSectors.dap
    mov ah,0x42
    int 0x13
    jc loadSectors.error
    mov cx,1
    jmp loadSectors.finish
    loadSectors.error:
        xor cx,cx
    loadSectors.finish:
        pop si
        pop dx
        pop bx
        pop ax
        ret
    loadSectors.dap:
        db 0x10
        db 0
        loadSectors.dap.sectorCount: dw 0
        loadSectors.dap.desOff: dw 0
        loadSectors.dap.desSeg: dw 0
        loadSectors.dap.LBA: dd 0
        dd 0

fillFirstSector:
    times 512 - 2 - ($ - $$) db 0x00
    dw 0xaa55


;Find an entry at a FAT32 directory
;Input: ax&bx = the cluster index of the directory in which we find
;       ds:si = pointer to the 11 bits short name of the entry
;       es:di = pointer to the memory buffer, a cluster size of memories here will be used
;Success: cx = 1
;         ax&bx = cluster index of the target file or directory
;Fail: cx = 0
findEntry:
    push dx
    push di
    mov dx,[partition.clusterSize]
    add dx,di
    findEntry.oneCluster:
        call loadCluster
        jcxz findEntry.fail
        pop di
        push di
        sub di,0x20
        findEntry.oneCluster.nextEntry:
            add di,0x20
            cmp di,dx
            jnb findEntry.oneCluster.notFound
            mov cl,[es:di + 0x0b]
            and cl,0b00110000
            jz findEntry.oneCluster.nextEntry
            mov cx,11
            call stringCompare
            jcxz findEntry.oneCluster.nextEntry
            mov ax,[es:di + 0x1a]
            mov bx,[es:di + 0x14]
            jmp findEntry.found
        findEntry.oneCluster.notFound:
            call nextClusterIndex
            cmp cx,1
            je findEntry.oneCluster
    findEntry.fail:
        xor cx,cx
        jmp findEntry.finish
    findEntry.found:
        mov cx,1
    findEntry.finish:
        pop di
        pop dx
        ret

;Load a cluster by cluster index
;Input: ax&bx = index of cluster
;       es:di = pointer to the memory buffer to which cluster will be transferred
;Success: cx = 1
;Fail: cx = 0
loadCluster:
    push ax
    push bx
    push cx
    push dx

    and bh,0x0f
    xor dx,dx
    mov cx,2
    call dSubD

    mov cl,[partition.sectorsPerClustor]
    xor ch,ch
    call dMulW
    push ax
    push bx

    mov ax,[partition.sectorsPerFAT]
    mov bx,[partition.sectorsPerFAT + 2]
    mov cl,[partition.FATCount]
    xor ch,ch
    call dMulW

    pop dx
    pop cx
    call dAddD

    mov cx,[partition.reservedSectors]
    xor dx,dx
    call dAddD

    mov cx,[partition.startSector]
    mov dx,[partition.startSector + 2]
    call dAddD

    mov cl,[partition.sectorsPerClustor]
    xor ch,ch
    call loadSectors

    pop dx
    pop cx
    pop bx
    pop ax
    ret

;Get the index of next cluster
;Input: ax&bx = index of current cluster
;       es:di = pointer to the memory buffer, a SECTOR_SIZE of memories there will be used
;Success: cx = 1
;         ax&bx = gotten index of next cluster
;NoNext: cx = 2 (means that successfully to analize but the given cluster is the last one)
;Fail: cx = 0 (a error takes place when read from the disk)
nextClusterIndex:
    push dx
    push bp
    mov cx,SECTOR_SIZE / 4
    call dDivW
    push dx
    mov cx,[partition.reservedSectors]
    xor dx,dx
    call dAddD
    mov cx,[partition.startSector]
    mov dx,[partition.startSector + 2]
    call dAddD

    mov cx,1
    call loadSectors
    jcxz nextClusterIndex.fail
    pop ax
    mov cx,4
    mul cx
    mov bp,ax
    mov ax,[es:di + bp]
    mov bx,[es:di + bp + 2]

    and bh,0x0F
    cmp bx,0x0FFF
    jne nextClusterIndex.hasNext
    mov cx,ax
    and cx,0xFFF0
    cmp cx,0xFFF0
    je nextClusterIndex.hasnotNext
    nextClusterIndex.hasNext:
        mov cx,1
        jmp nextClusterIndex.finish
    nextClusterIndex.hasnotNext:
        mov cx,2
        jmp nextClusterIndex.finish
    nextClusterIndex.fail:
        xor cx,cx
    nextClusterIndex.finish:
        pop bp
        pop dx
        ret

;Ignore case and compare two string
;Input: ds:si = pointer to one string
;       es:di = pointer to another string
;       cx = length of these strings
;Equal: cx = 1
;Inequal: cx = 0
stringCompare:
    push ax
    push bx
    mov bx,cx
    mov cx,1
    stringCompare.nextChar:
        dec bx
        cmp bx,0
        jl stringCompare.finish
        mov al,[ds:si + bx]
        mov ah,[es:di + bx]
        cmp al,'a'
        jl stringCompare.upperCaseEnd1
        cmp al,'z'
        jg stringCompare.upperCaseEnd1
        sub al, 'a' - 'A'
    stringCompare.upperCaseEnd1:
        cmp ah,'a'
        jl stringCompare.upperCaseEnd2
        cmp ah,'z'
        jg stringCompare.upperCaseEnd2
        sub ah, 'a' - 'A'
    stringCompare.upperCaseEnd2:
        cmp al,ah
        je stringCompare.nextChar
        xor cx,cx
    stringCompare.finish:
        pop bx
        pop ax
        ret

;Add a dword with another one
;ax&bx = ax&bx + cx&dx
dAddD:
    add ax,cx
    jnc dAddD.noCarry
        add bx,dx
        inc bx
        ret
    dAddD.noCarry:
        add bx,dx
        ret

;Substract a dword from another one
;ax&bx = ax&bx - cx&dx
dSubD:
    sub ax,cx
    jnc dSubD.noCarry
        sub bx,dx
        dec bx
        ret
    dSubD.noCarry:
        sub bx,dx
        ret

;Multiply a dword by an word
;ax&bx = ax&bx * cx
dMulW:
    push dx
    mul cx
    push ax
    push dx
    mov ax,bx
    mul cx
    pop dx
    add ax,dx
    mov bx,ax
    pop ax
    pop dx
    ret

;Divide a dword by an word
;ax&bx = ax&bx / cx
;dx = ax&bx % cx
dDivW:
    push bp
    mov bp,ax
    mov ax,bx
    xor dx,dx
    div cx
    mov bx,ax
    mov ax,bp
    div cx
    pop bp
    ret

fillLastSector:
    %if (fillLastSector - program_start) % SECTOR_SIZE
        times 512 - (fillLastSector - program_start) % SECTOR_SIZE db 0x00
    %endif

program_end: 
