program loop_odd_even
    implicit none
    integer i, sum_e, sum_o
    sum_e = 0
    sum_o = 0
    do i = 1,100
        if (mod(i, 2)==0) then
            sum_e = sum_e + i
        else if (mod(i, 2)==1) then
            sum_o = sum_o + i
        else
            stop 'something strange happens'
        endif
    enddo
    write(*, *) 'Even, Odd, Total', sum_e, sum_o, sum_e+sum_o
end program