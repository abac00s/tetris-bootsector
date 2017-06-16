; TETRIS by abac00s
; 
; Use left and right arrows to move the falling brick.
; When you complete one row, it is removed, other
; bricks fall on its place and your score
; (displayed in left-top corner) is increased.
; The game is over, when bricks reach the top of the screen.


[bits 16]
[org 0x7c00]

VRAM_SEG equ 0xb800
KEYBOARD_INTERRUPT equ 1

jmp word 0x0000:start

start:
  cld
  mov ax, 0x2000
  mov ss, ax
  mov sp, 0xffff
  
  mov byte [ss:0], 0
  mov word [ss:5], (0b00010000 << 8) | 0x20
  mov word [ss:2], bricks
  
  ; --------------------------------- Set up keyboard interrupts.
  cli
  xor ax, ax
  mov ds, ax
  mov di, 0x24
  mov ax, keyboard_handler
  stosw
  mov ax, cs
  stosw
  
  mov ax, VRAM_SEG
  mov es, ax
  mov ds, ax
  call clear_screen
  mov di, (4*80 + 29)*2
  call draw_vert_line
  mov di, (4*80 + 50)*2
  call draw_vert_line
  
  call remove_full
  call print_score
  
  jmp add_new
  
  main_loop:

    ; ------------------------------ Check for collisions and move...
    mov bx, 2*80
    call check_collisions
    cmp ax, 0
    jne ml_next1
    call move_brick
    jmp ml_next2
    
    ; ------------------------------- ...and if there are some, stop the falling brick.
    ml_next1:
      xor si, si
      mov cx, 80*25
      .fix_loop:
        mov di, si
        lodsw
        cmp al, 0x20
        jne .continue2
          xor ax, ax
          stosb
        .continue2:
        loop .fix_loop
      
    ; ------------------------------- Check if we haven't lost.
    mov si, (3*80 + 30)*2
    mov cx, 20
    .cl_loop:
      lodsw
      cmp ah, 0
      jne the_end
      loop .cl_loop
      
    call remove_full
    call print_score
    
    ; ------------------------------- add a new brick.
    add_new:
    
    mov si, word [ss:2]
    mov ax, word [ss:5]
    mov cx, 5
    .loopx:
      mov di, word [cs:si]
      add si, 2
      stosw
      loop .loopx
    cmp si, bricks_end
    jne .adf
      mov si, bricks
    .adf:
    mov word [ss:2], si
    add ah, 0b00010000
    and ah, 0b01111111
    cmp ah, 0
    jne ghx
      add ah, 0b00010000
    ghx:
    mov word [ss:5], ax
  
    ml_next2:
    
    xor dx, dx
    sti
    mov bx, 0x80
    .wait_loop:
      push bx
      
      cmp dx, 'K'
      jne .next1
        xor dx, dx
        mov bx, -2
        call check_collisions
        cmp ax, 0
        jne .next1
          call move_brick
      .next1:
      
      cmp dx, 'M'
      jne .next2
        xor dx, dx
        mov bx, 2
        call check_collisions
        cmp ax, 0
        jne .next1
          call move_brick
      .next2:
      
      pop bx
      mov cx, 0xffff
      .inner_loop:
        loop .inner_loop
      dec bx
      cmp bx, 0
      jne .wait_loop
    cli
    
    jmp main_loop
  
  the_end:
  
  call clear_screen
  
  jmp $
  
clear_screen:
  xor di, di
  mov cx, 25*80
  xor ax, ax
  rep stosw
  mov cx, 80
  mov ax, 0x4100
  rep stosw
  ret

draw_vert_line:
  mov cx, 26
  mov ax, 0x4100
  dvl_loop:
    stosw
    add di, 2*80 - 2
    loop dvl_loop
  ret

keyboard_handler:
  push ax
  in al, 0x60
  test al, 128
  jnz .end
  
  mov dl, al
  
  .end:
  mov al, 0x61
  out 0x20, al
  pop ax
  iret

move_brick:
  mov si, 80*25*2 - 2
  mov dx, -2
  cmp bx, -2
  jne .next1
    xor si, si
    mov dx, 2
  .next1:
  mov di, si
  add di, bx
  mov cx, 25*80
  
  mb_loop:
    mov ax, word [ds:si]
    cmp al, 0x20
    jne .next2
      mov word [es:di], ax
      mov word [ds:si], 0
    .next2:
    add si, dx
    add di, dx
    loop mb_loop
  ret
  
check_collisions:
  xor si, si
  mov di, si
  add di, bx
  mov cx, 25*80
  
  cc_loop:
    lodsw
    cmp al, 0x20
    jne .next1
      mov ax, word [es:di]
      cmp ah, 0
      je .next1
        cmp al, 0x20
        je .next1
          mov ax, 1
          ret
    .next1:
    add di, 2
    loop cc_loop
    
  xor ax, ax
  ret
  
remove_full:
  mov si, (80*24 + 30)*2
  xor bx, bx
  
  .loop1:
    cmp si, 30*2
    je .end
    
    mov cx, 20
    mov dx, 1
    .loop2:
      lodsw
      cmp ah, 0
      jne .continue
        xor dx, dx 
      .continue:
      loop .loop2
    sub si, 20*2
    
    add bx, dx
    
    mov ax, 80*2
    mul bx
    mov di, si
    add di, ax
    
    mov cx, 20
    .loop3:
      lodsw
      stosw
      loop .loop3
    
    sub si, (80 + 20)*2
    jmp .loop1
  
  .end:
  
  add byte [ss:0], bl
  
  ret
  

print_score:
  mov al, byte [ss:0]
  mov bl, 10
  div bl
  
  mov bx, ax
  add bx, ('0' << 8) | '0'
  
  mov di, 2*80 + 2
  mov ah, 0x3
  mov al, bl
  stosw
  mov al, bh
  stosw
  ret

bricks:
  dw ((0*80 + 39)*2), ((1*80 + 39)*2), ((2*80 + 39)*2), ((2*80 + 40)*2), ((2*80 + 41)*2)
  dw ((1*80 + 43)*2), ((0*80 + 40)*2), ((1*80 + 40)*2), ((1*80 + 41)*2), ((1*80 + 42)*2)
  dw ((0*80 + 40)*2), ((1*80 + 40)*2), ((2*80 + 39)*2), ((2*80 + 40)*2), ((2*80 + 41)*2)
  dw ((0*80 + 40)*2), ((1*80 + 40)*2), ((2*80 + 40)*2), ((3*80 + 40)*2), ((4*80 + 40)*2),
bricks_end:

times (510 - ($ - $$)) db 0
db 0x55
db 0xAA
