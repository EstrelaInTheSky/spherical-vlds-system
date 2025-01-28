module seed_ass
    use, intrinsic :: iso_c_binding, only: c_int
    use c_to_fort
    use uns_wrapper
    implicit none
    integer(c_int), parameter :: MAX_INT24 = 999999999
    integer(c_int), parameter :: MIN_INT24 = 100000000
    ! Chosen Seed Archive Name.
    character(len=*), parameter :: SAVE_FILE = "Chosen-RAN3.dat"
    ! Array Percolation.
    integer :: i, j
    ! RAN3 Seed.
    type(uint32) :: RAN3_SEED
    ! RProb Seed Assign.
    type(uint32) :: RProb1, RProb2, RProb3, RProb4, RProb5, RProb6, RProb7
    ! RLine Seed Assign.
    type(uint32) :: RLine1, RLine2, RLine3, RLine4, RLine5, RLine6, RLine7
    ! RCol Seed Assign.
    type(uint32) :: RCol1, RCol2, RCol3, RCol4, RCol5, RCol6, RCol7
    ! RNbr Seed Assign.
    type(uint32) :: RNbr1, RNbr2, RNbr3, RNbr4, RNbr5, RNbr6, RNbr7
contains
    ! Randomic Seed Attribute.
    function RSeed() result(RSeedPar)
        type(uint32) :: RSeedPar
        ! RSeedPar Initialization.
        call RSeedPar%setUns(0)
        ! "Warming Up" the RSeed.
        do while (RSeedPar%toDecimal() <= 1000000 .or. RSeedPar%toDecimal() > 9999999)
            call RSeedPar%setUns(int(10000000*Ran3(RAN3_SEED%toDecimal()), kind=c_int))
        end do
        if (mod(int(RSeedPar%toDecimal(), kind=c_int), 2) == 0) then
            call RSeedPar%setUns(RSeedPar%toDecimal() - 1)
        end if
    end function RSeed

    ! Get Odd Number Function.
    function GetOddNum() result(OddNum)
        type(uint32) :: OddNum
        integer :: tmpValue
        ! Open the '/dev/random' Folder.
        open(unit=3, file='/dev/random', status="OLD", action="READ", form='UNFORMATTED', access='STREAM')
        do
            read(3) tmpValue
            ! Putting OddNum is Desired Interval.
            tmpValue = abs(mod(tmpValue, (MAX_INT24 - MIN_INT24 + 1))) + MIN_INT24
            if (mod(tmpValue, 2) == 0) tmpValue = tmpValue - 1
            call OddNum%setUns(tmpValue)
            if (IsChoiced(OddNum) .eqv. .false.) then
                call SaveOddNum(OddNum)
                exit
            end if
        end do
        ! Close the '/dev/random' Folder.
        close(unit=3)
    end function GetOddNum

    ! Chosen Odd Number Function.
    logical function IsChoiced(OddNum)
        type(uint32), intent(in) :: OddNum
        type(uint32) :: ChoicedOddNum
        integer :: ierr
        ! Open the SAVE_FILE.
        open(unit=1, file=SAVE_FILE, status="OLD", action="READ", form='FORMATTED')
        verify: block
            do
                read(1, *, iostat=ierr) ChoicedOddNum
                if (ierr /= 0) exit
                if (ChoicedOddNum%toDecimal() == OddNum%toDecimal()) then
                    IsChoiced = .true.
                    exit verify
                end if
            end do
            IsChoiced = .false.
        end block verify
        ! Close the SAVE_FILE.
        close(unit=1)
    end function IsChoiced

    ! Ran3 Seed Acquisition.
    subroutine R3SeedAssign()
        RAN3_SEED = GetOddNum()
    end subroutine R3SeedAssign

    ! Save Odd Number.
    subroutine SaveOddNum(OddNum)
        type(uint32), intent(in) :: OddNum
        open(unit=2, file=SAVE_FILE, status="OLD", action="WRITE", form='FORMATTED', position='APPEND')
        write(2, *) OddNum%toDecimal()
        ! Close the SAVE_FILE.
        close(unit=2)
    end subroutine SaveOddNum

    ! RProb Seed Attribute.
    subroutine RProbSeedAssign()
        ! 'RProb' Seed Assign.
        RProb1 = RSeed()
        RProb2 = RSeed()
        RProb3 = RSeed()
        RProb4 = RSeed()
        RProb5 = RSeed()
        RProb6 = RSeed()
        RProb7 = RSeed()
        do i = 1, 999
            call RProb1%setUns(RProb1%toDecimal() * 1101513973)
            call RProb2%setUns(RProb2%toDecimal() * 1101513973)
            call RProb3%setUns(RProb3%toDecimal() * 1101513973)
            call RProb4%setUns(RProb4%toDecimal() * 1101513973)
            call RProb5%setUns(RProb5%toDecimal() * 1101513973)
            call RProb6%setUns(RProb6%toDecimal() * 1101513973)
            call RProb7%setUns(RProb7%toDecimal() * 1101513973)
        end do
    end subroutine RProbSeedAssign

    ! RLine Seed Attribute.
    subroutine RLineSeedAssign()
        ! 'RLine' Seed Assign.
        RLine1 = RSeed()
        RLine2 = RSeed()
        RLine3 = RSeed()
        RLine4 = RSeed()
        RLine5 = RSeed()
        RLine6 = RSeed()
        RLine7 = RSeed()
        do i = 1, 999
            call RLine1%setUns(RLine1%toDecimal() * 1101513973)
            call RLine2%setUns(RLine2%toDecimal() * 1101513973)
            call RLine3%setUns(RLine3%toDecimal() * 1101513973)
            call RLine4%setUns(RLine4%toDecimal() * 1101513973)
            call RLine5%setUns(RLine5%toDecimal() * 1101513973)
            call RLine6%setUns(RLine6%toDecimal() * 1101513973)
            call RLine7%setUns(RLine7%toDecimal() * 1101513973)
        end do
    end subroutine RLineSeedAssign

    ! RCol Seed Attribute.
    subroutine RColSeedAssign()
        ! 'RCol' Seed Assign.
        RCol1 = RSeed()
        RCol2 = RSeed()
        RCol3 = RSeed()
        RCol4 = RSeed()
        RCol5 = RSeed()
        RCol6 = RSeed()
        RCol7 = RSeed()
        do i = 1, 999
            call RCol1%setUns(RCol1%toDecimal() * 1101513973)
            call RCol2%setUns(RCol2%toDecimal() * 1101513973)
            call RCol3%setUns(RCol3%toDecimal() * 1101513973)
            call RCol4%setUns(RCol4%toDecimal() * 1101513973)
            call RCol5%setUns(RCol5%toDecimal() * 1101513973)
            call RCol6%setUns(RCol6%toDecimal() * 1101513973)
            call RCol7%setUns(RCol7%toDecimal() * 1101513973)
        end do
    end subroutine RColSeedAssign

    ! RNbr Seed Attribute.
    subroutine RNbrSeedAssign()
        ! 'RNbr' Seed Assign.
        RNbr1 = RSeed()
        RNbr2 = RSeed()
        RNbr3 = RSeed()
        RNbr4 = RSeed()
        RNbr5 = RSeed()
        RNbr6 = RSeed()
        RNbr7 = RSeed()
        do i = 1, 999
            call RNbr1%setUns(RNbr1%toDecimal() * 1101513973)
            call RNbr2%setUns(RNbr2%toDecimal() * 1101513973)
            call RNbr3%setUns(RNbr3%toDecimal() * 1101513973)
            call RNbr4%setUns(RNbr4%toDecimal() * 1101513973)
            call RNbr5%setUns(RNbr5%toDecimal() * 1101513973)
            call RNbr6%setUns(RNbr6%toDecimal() * 1101513973)
            call RNbr7%setUns(RNbr7%toDecimal() * 1101513973)
        end do
    end subroutine RNbrSeedAssign
end module seed_ass