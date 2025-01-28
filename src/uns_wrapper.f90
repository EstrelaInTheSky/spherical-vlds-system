module uns_wrapper
    use iso_c_binding, only: c_int, c_double
    implicit none
    ! Bits Quantity.
    integer, parameter :: UINT_BIT = 32
    ! 32-Bit Integer Derivate Type.
    type :: uint32
        integer(kind=4) :: value = 0
    contains
        procedure :: setUns   => set_uint32
        procedure :: toDecimal  => to_decimal
    end type uint32
contains
    ! Type Value Attribute.
    subroutine set_uint32(CurrValue, NewValue)
        class(uint32), intent(inout) :: CurrValue
        integer, intent(in) :: NewValue

        CurrValue%value = iand(NewValue, z'FFFFFFFF')
    end subroutine set_uint32

    ! Return Decimal Value.
    function to_decimal(CurrValue) result(DecValue)
        class(uint32), intent(in) :: CurrValue
        integer :: DecValue

        DecValue = abs(iand(CurrValue%value, z'FFFFFFFF'))
    end function to_decimal
end module uns_wrapper