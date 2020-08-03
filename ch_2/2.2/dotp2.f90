program dotp2
    implicit none
    integer i
    integer, parameter :: n=2
    real(8) u(n), v(n), dotp
    u(:) = (/1.0d0, 2.0d0/)
    v(:) = (/3.0d0, 4.0d0/)

    dotp = 0.0d0
    do i=1,2
        dotp = dotp + u(i)*v(i)
    enddo
    write(*,*) 'dot product', dotp
end program dotp2