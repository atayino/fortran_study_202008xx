program power_234
    implicit none
    integer i, i2, i3, i4
    i2 = 0
    i3 = 0
    i4 = 0
    do i=1, 10
        i2 = i2 + i
        i3 = i3 + i ** 2
        i4 = i4 + i ** 3
    enddo
    write(*, *) i
    write(*, *) 'k2', 10 * (10+1) / 2, i2
    write(*, *) 'k3', 10 * (10+1) * (2*10+1)/6, i3
    write(*, *) 'k4', 10 * 10 * (10+1) * (10+1)/4, i4
    
end program power_234