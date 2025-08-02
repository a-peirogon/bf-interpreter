program brainfuck
    implicit none
    
    integer, parameter :: MEMSIZE = 65535
    integer :: memory(0:MEMSIZE-1)
    integer :: ptr, i, code_len, cnt
    character(len=10000) :: code
    character :: ch
    
    memory = 0
    ptr = 0
    
    print *, 'Codigo:'
    read '(A)', code
    code_len = len_trim(code)
    
    print *, 'Salida:'
    
    i = 1
    do while (i <= code_len)
        ch = code(i:i)
        
        select case (ch)
        case ('>')
            if (ptr == MEMSIZE-1) then
                ptr = 0
            else
                ptr = ptr + 1
            end if
            
        case ('<')
            if (ptr == 0) then
                ptr = MEMSIZE-1
            else
                ptr = ptr - 1
            end if
            
        case ('+')
            memory(ptr) = memory(ptr) + 1
            
        case ('-')
            memory(ptr) = memory(ptr) - 1
            
        case ('.')
            write(*, '(A)', advance='no') char(memory(ptr))
            
        case (',')
            read(*, '(A)') ch
            memory(ptr) = ichar(ch)
            
        case ('[')
            if (memory(ptr) == 0) then
                cnt = 0
                i = i + 1
                do while (cnt > 0 .or. code(i:i) /= ']')
                    if (code(i:i) == '[') cnt = cnt + 1
                    if (code(i:i) == ']') cnt = cnt - 1
                    i = i + 1
                end do
            end if
            
        case (']')
            if (memory(ptr) /= 0) then
                cnt = 0
                i = i - 1
                do while (cnt > 0 .or. code(i:i) /= '[')
                    if (code(i:i) == ']') cnt = cnt + 1
                    if (code(i:i) == '[') cnt = cnt - 1
                    i = i - 1
                end do
            end if
            
        end select
        
        i = i + 1
    end do
    
    print *
    
end program brainfuck