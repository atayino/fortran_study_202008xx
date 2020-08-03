program montecalro_pi
    implicit none
    real(8) x, y, pi, pi0
    integer :: n, i, im = 2 ** 20
    pi0 = 2.0d0 * acos(0.0d0)
    n = 0
    write(*, *) 'current i', i
    do i=0, im
        call random_number(x)
        call random_number(y)
        if(x**2 + y**2 <= 1.0d0) n = n+1
    enddo
    pi = 4.0d0 * dble(n) / dble(im)
    write(*, *) 'pi, pi0, er:', pi, pi0, pi-pi0
end program montecalro_pi