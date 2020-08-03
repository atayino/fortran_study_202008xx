program fibonacchi
    implicit none
    integer i, a_0, a_1, a_2
    a_0 = 1
    a_1 = 2
    write(*, *) 'a_', 1, a_0
    write(*, *) 'a_', 2, a_1
    do i=3, 10
        a_2 = a_0 + a_1
        write(*, *) 'a_', i, a_0 + a_1
        a_0 = a_1
        a_1 = a_2
    enddo
end program fibonacchi