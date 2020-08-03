module subprogs
    implicit none
contains
    subroutine bicgstab(a, b, x, n, itrmax, er0)
        integer, intent(in) :: n, itrmax
        real(8), intent(in) :: a(n, n), b(n), er0
        real(8), intent(inout) :: x(n)

        integer itr, i
        real(8) alp, bet, c1, c2, c3, ev, vv, rr
        real(8) r(n), r0(n), p(n), y(n), e(n), v(n)

        x(:) = 0.0d0
        r(:) = b - matmul(a, x)
        c1 = dot_product(r, r)
        if (c1 < er0) return
        p(:) = r(:)
        r0(:) = r(:)
        do itr = 1, itrmax
            y(:) = matmul(a, p)
            c2 = dot_product(r0, y)
            alp = c1/c2
            e(:) = r(:) - alp * y(:)
            v(:) = matmul(a, e)
            ev = dot_product(e, v)
            vv = dot_product(v, v)
            c3 = ev/(vv + er0)
            x(:) = x(:) + alp * p(:) + c3 * e(:)
            r(:) = e(:) - c3 * v(:)
            rr = dot_product(r, r)
            write(*, *) 'itr, rr', itr, rr
            if (rr < er0) exit
            c1 = dot_product(r0, r)
            bet = c1 / (c2 * c3)
            p(:) = r(:) + bet * (p(:) - c3 * y(:))

        enddo
    end subroutine bicgstab
end module subprogs