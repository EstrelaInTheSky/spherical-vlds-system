module math_func
    use, intrinsic :: iso_c_binding, only: c_int, c_long_double
    implicit none
contains
    ! Modular Function.
    function Absl(a) result(AbsValue)
        integer(c_int), intent(in) :: a
        integer(c_int) :: AbsValue, Mask
        Mask = ishft(a, -(bit_size(a) - 1))
        AbsValue = ieor(a + Mask, Mask)
    end function Absl

    ! Square Function.
    function Sq(b) result(SqValue)
        real(c_long_double), intent(in) :: b
        real(c_long_double) :: SqValue
        SqValue = b * b
    end function Sq

    ! Cubic Function.
    function Cb(c) result(CbValue)
        real(c_long_double), intent(in) :: c
        real(c_long_double) :: CbValue
        CbValue = c * c * c
    end function Cb

    ! Quadric Function.
    function Qd(d) result(QdValue)
        real(c_long_double), intent(in) :: d
        real(c_long_double) :: QdValue
        QdValue = d * d * d * d
    end function Qd
end module math_func