program main
    use subprogs
    implicit none
    real(8), allocatable :: a(:, :), b(:), x(:)
    integer :: i, n, itrmax = 10, fi=10, fo=11
    real(8) :: er0 = 1.0d-6
    open(fi, file='cgstab_in.d')
    read(fi, *) n
    write(*, '("The dimension of matrix is" i6 ".")') n
    allocate(a(n, n))
    do i=1,n
        read(fi, *) a(i, 1:n)
    enddo
    allocate(b(n))
    do i=1,n
        read(fi, *) b(i)
    enddo
    close(fi)
    allocate(x(n))
    call bicgstab(a, b, x, n, itrmax, er0)
    do i=1, n
        write(*, '(10f8.3)') a(i, 1:n), x(i), b(i)
    enddo
    deallocate(a, b, x)
end program main