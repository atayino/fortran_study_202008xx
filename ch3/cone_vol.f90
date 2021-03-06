module subprogs
    implicit none
contains
    subroutine cone_vol(r, h, v)
        real(8), intent(in) :: r,h
        real(8), intent(out) :: v
        real(8) s, pi
        pi = 2.0d0 * acos(0.0d0)
        s = pi * r ** 2
        v = s * h / 3.0d0
    end subroutine cone_vol
    function f_cone_vol(r, h) result(v)
        real(8), intent(in) :: r,h
        real(8) v, s, pi
        pi = 2.0d0 * acos(0.0d0)
        s = pi * r ** 2
        v = s * h / 3.0d0
    end function f_cone_vol
end module subprogs

program main
    use subprogs
    implicit none
    real(8) :: a = 1.5d0, l = 3.0d0, vol
    !call cone_vol(a, l, vol)
    write(*, '("Volume is:", 2x, f10.4)')  f_cone_vol(a, l)
end program main