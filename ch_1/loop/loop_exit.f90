program loop_exit
    implicit none
    integer sum, n, i_

    do
        write(*, '(a)', advance='no') 'input n (input 0 to stop) :'
        read(*, *) n
        if (n == 0) then
            exit
        else if (n<0) then
            write(*,*) 'sorry, input positive n ...'
            cycle
        endif
        sum = 0
        do i_ = 1, n
            sum = sum + i_
        enddo
        write(*, *) 'sum = ', sum
    enddo
    write(*, *) 'exit from do-loop ...'
end program loop_exit