module c_to_fort
    use, intrinsic :: iso_c_binding
    implicit none
    interface
        ! Seed Generator Interoperability.
        real(c_double) function Ran3(idum) bind(C, name="Ran3")
            import c_int, c_double
            integer(c_int), intent(in) :: idum
        end function Ran3
    end interface
end module c_to_fort