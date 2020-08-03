program loop
    implicit none
    integer i, i_sum

    i_sum = 0
    do i = 1, 100
        i_sum = i_sum + i
    enddo

    write(*, *) 'sum = ', i_sum
end program loop