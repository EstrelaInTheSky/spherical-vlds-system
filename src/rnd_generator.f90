module rnd_generator
    use, intrinsic :: iso_c_binding, only: c_int, c_double
    use math_func
    use uns_wrapper
    use seed_ass
    implicit none
    real(c_double), parameter :: MAX_INT32 = 4294967296.0
contains
    ! Randomic Probability Function.
    real(c_double) function RProb()
        integer(c_int) :: RStrProb
        ! RProb Operations.
        call RProb2%setUns(RProb2%toDecimal() * 1101513973)
        call RProb1%setUns(RProb1%toDecimal() * 1101513973)
        call RProb1%setUns(RProb1%toDecimal() + iand(ishft(RProb2%toDecimal(), -31), 1) * 65538 * RProb1%toDecimal())
        call RProb6%setUns(RProb6%toDecimal() * 1101513973)
        call RProb6%setUns(RProb6%toDecimal() + iand(ishft(RProb2%toDecimal(), -31), 1) * 16806 * RProb6%toDecimal())
        call RProb7%setUns(RProb7%toDecimal() * 1101513973)
        call RProb7%setUns(RProb7%toDecimal() * (16807))
        call RProb3%setUns(RProb3%toDecimal() * 1101513973)
        call RProb3%setUns(RProb3%toDecimal() + iand(ishft(RProb2%toDecimal(), -31), 1) * 16806 * RProb3%toDecimal())
        call RProb4%setUns(RProb4%toDecimal() * 1101513973)
        call RProb4%setUns(RProb4%toDecimal() + iand(ishft(RProb2%toDecimal(), -31), 1) * 1101513972 * RProb4%toDecimal())
        call RProb5%setUns(RProb5%toDecimal() * 1101513973)
        ! RProb String.
        RStrProb = ishft(ishft(RProb1%toDecimal(), -26), 26) + ishft(ishft(RProb6%toDecimal(), -26), 20) + ishft(ishft(RProb4%toDecimal(), -27), 15) &
                   + ishft(ishft(RProb3%toDecimal(), -27), 10) + ishft(ishft(RProb5%toDecimal(), -27), 5) + ishft(RProb7%toDecimal(), -27)
        RProb = real(RStrProb, kind=c_double)/ (2500000000.0)
    end function RProb

    ! Randomic Line Function.
    integer(c_int) function RLine(XAxis)
        integer(c_int), intent(in) :: XAxis
        integer(c_int) :: RStrLine
        ! RLine Operations.
        call RLine2%setUns(RLine2%toDecimal() * 1101513973)
        call RLine1%setUns(RLine1%toDecimal() * 1101513973)
        call RLine1%setUns(RLine1%toDecimal() + iand(ishft(RLine2%toDecimal(), -31), 1) * 65538 * RLine1%toDecimal())
        call RLine6%setUns(RLine6%toDecimal() * 1101513973)
        call RLine6%setUns(RLine6%toDecimal() + iand(ishft(RLine2%toDecimal(), -31), 1) * 16806 * RLine6%toDecimal())
        call RLine7%setUns(RLine7%toDecimal() * 1101513973)
        call RLine7%setUns(RLine7%toDecimal() * 16807)
        call RLine3%setUns(RLine3%toDecimal() * 1101513973)
        call RLine3%setUns(RLine3%toDecimal() + iand(ishft(RLine2%toDecimal(), -31), 1) * 16806 * RLine3%toDecimal())
        call RLine4%setUns(RLine4%toDecimal() * 1101513973)
        call RLine4%setUns(RLine4%toDecimal() + iand(ishft(RLine2%toDecimal(), -31), 1) * 1101513972 * RLine4%toDecimal())
        call RLine5%setUns(RLine5%toDecimal() * 1101513973)
        ! RLine String.
        RStrLine = ishft(ishft(RLine1%toDecimal(), -26), 26) + ishft(ishft(RLine6%toDecimal(), -26), 20) + ishft(ishft(RLine4%toDecimal(), -27), 15) &
                   + ishft(ishft(RLine3%toDecimal(), -27), 10) + ishft(ishft(RLine5%toDecimal(), -27), 5) + ishft(RLine7%toDecimal(), -27)
        RLine = int((real(RStrLine, kind=c_double) / MAX_INT32) * XAxis, kind=c_int)
    end function RLine

    ! Randomic Column Function.
    integer(c_int) function RCol(YAxis)
        integer(c_int), intent(in) :: YAxis
        integer(c_int) :: RStrCol
        ! RCol Operations.
        call RCol2%setUns(RCol2%toDecimal() * 1101513973)
        call RCol1%setUns(RCol1%toDecimal() * 1101513973)
        call RCol1%setUns(RCol1%toDecimal() + iand(ishft(RCol2%toDecimal(), -31), 1) * 65538 * RCol1%toDecimal())
        call RCol6%setUns(RCol6%toDecimal() * 1101513973)
        call RCol6%setUns(RCol6%toDecimal() + iand(ishft(RCol2%toDecimal(), -31), 1) * 16806 * RCol6%toDecimal())
        call RCol7%setUns(RCol7%toDecimal() * 1101513973)
        call RCol7%setUns(RCol7%toDecimal() * 16807)
        call RCol3%setUns(RCol3%toDecimal() * 1101513973)
        call RCol3%setUns(RCol3%toDecimal() + iand(ishft(RCol2%toDecimal(), -31), 1) * 16806 * RCol3%toDecimal())
        call RCol4%setUns(RCol4%toDecimal() * 1101513973)
        call RCol4%setUns(RCol4%toDecimal() + iand(ishft(RCol2%toDecimal(), -31), 1) * 1101513972 * RCol4%toDecimal())
        call RCol5%setUns(RCol5%toDecimal() * 1101513973)
        ! RCol String.
        RStrCol = ishft(ishft(RCol1%toDecimal(), -26), 26) + ishft(ishft(RCol6%toDecimal(), -26), 20) + ishft(ishft(RCol4%toDecimal(), -27), 15) &
                   + ishft(ishft(RCol3%toDecimal(), -27), 10) + ishft(ishft(RCol5%toDecimal(), -27), 5) + ishft(RCol7%toDecimal(), -27)
        RCol = int((real(RStrCol, kind=c_double) / MAX_INT32) * YAxis, kind=c_int)
    end function RCol

    ! Randomic Neighbor Function.
    integer(c_int) function RNbr(NbrQuantity)
        integer(c_int), intent(in) :: NbrQuantity
        integer(c_int) :: RStrNbr
        ! RNbr Operations.
        call RNbr2%setUns(RNbr2%toDecimal() * 1101513973)
        call RNbr1%setUns(RNbr1%toDecimal() * 1101513973)
        call RNbr1%setUns(RNbr1%toDecimal() + iand(ishft(RNbr2%toDecimal(), -31), 1) * 65538 * RNbr1%toDecimal())
        call RNbr6%setUns(RNbr6%toDecimal() * 1101513973)
        call RNbr6%setUns(RNbr6%toDecimal() + iand(ishft(RNbr2%toDecimal(), -31), 1) * 16806 * RNbr6%toDecimal())
        call RNbr7%setUns(RNbr7%toDecimal() * 1101513973)
        call RNbr7%setUns(RNbr7%toDecimal() * 16807)
        call RNbr3%setUns(RNbr3%toDecimal() * 1101513973)
        call RNbr3%setUns(RNbr3%toDecimal() + iand(ishft(RNbr2%toDecimal(), -31), 1) * 16806 * RNbr3%toDecimal())
        call RNbr4%setUns(RNbr4%toDecimal() * 1101513973)
        call RNbr4%setUns(RNbr4%toDecimal() + iand(ishft(RNbr2%toDecimal(), -31), 1) * 1101513972 * RNbr4%toDecimal())
        call RNbr5%setUns(RNbr5%toDecimal() * 1101513973)
        ! RNbr String.
        RStrNbr = ishft(ishft(RNbr1%toDecimal(), -26), 26) + ishft(ishft(RNbr6%toDecimal(), -26), 20) + ishft(ishft(RNbr4%toDecimal(), -27), 15) &
                   + ishft(ishft(RNbr3%toDecimal(), -27), 10) + ishft(ishft(RNbr5%toDecimal(), -27), 5) + ishft(RNbr7%toDecimal(), -27)
        RNbr = int((real(RStrNbr, kind=c_double) / MAX_INT32) * NbrQuantity, kind=c_int)
    end function RNbr
end module rnd_generator