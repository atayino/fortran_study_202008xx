module subprogs
    implicit none
contains
    function eigen_2x2mat(a) result(eval)
        real(8), intent(in) :: a(:, :)
        complex(8) eval(2)
        real(8) b, c, d, e
        if (size(a, 1) /= size(a, 2)) stop 'this is not the square matrix'
        if (size(a, 1) /= 2) stop 'matrix size is not 2'
        b = -0.5d0 * (a(1,1) + a(2,2))
        c = a(1,1) * a(2,2) - a(1,2)*a(2,1)
        d = b**2 - c
        if (d < 0.0d0) then
            eval(1) = cmplx(-b, sqrt(-d))
            eval(2) = conjg(eval(1))
        elseif( d > 0.0d0) then
            e = -b + sign(sqrt(d), -b)
            eval(1) = cmplx(e, 0.0d0)
            eval(2) = cmplx(c/e, 0.0d0)
        else
            eval(1) = cmplx(-b, 0.0d0)
            eval(2) = eval(1)
        endif
    end function eigen_2x2mat
end module subprogs

program main
    use subprogs
    implicit none
    integer :: fi=10, i, n1, n2
    real(8), allocatable :: a(:, :), eval(:)
    open(fi, file='matrix.d')
    read(fi, *) n1, n2
    allocate(a(n1, n2))
    do i=1,n1
        read(fi, *) a(i, 1:n2)
    enddo
    close(fi)
    do i=1,n1
        write(*, '(100f12.4)') a(i,1:n2)
    enddo
    write(*, '("The solution:" 2f12.3)') eigen_2x2mat(a)
    deallocate(a)
end program main