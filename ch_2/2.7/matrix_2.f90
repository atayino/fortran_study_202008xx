program matrix2
    implicit none
    integer :: n1, n2, i, fi=10
    real(8), allocatable :: a(:, :)
    open(fi, file='mat_input.d')
    read(fi, *) n1, n2
    allocate(a(n1, n2))
    do i=1,n1
        read(fi, *) a(i, 1:n2)
    enddo
    close(fi)
    do i=1,n1
        write(*, '(100f12.4)') a(i,1:n2)
    enddo
    deallocate(a)
end program matrix2