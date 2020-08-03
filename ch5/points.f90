program random_points
    implicit none
    integer :: i, n, fo=10
    real(8), allocatable :: x(:), y(:), r(:)

    write(*, *) 'input n:'
    read(*, *) n
    allocate(x(n))
    allocate(y(n))
    allocate(r(n))
    call random_number(r(1:n))
    open(fo, file='random_points.d')
    do i=1, n
        x(i) = -10.0d0 + 20.0d0 * dble(i-1) / (dble(n) - 1.0d0)
        y(i) = 0.1d0 * x(i) ** 3 + 0.2d0 * x(i)**2 + 0.5d0 * x(i) + 1.0d0 + 2.0d0*r(i)-1
        write(fo, '(100f10.4)') x(i), y(i)
    enddo
    close(fo)
    deallocate(x, y, r)

end program random_points