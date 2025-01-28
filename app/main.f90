program main
    use, intrinsic :: iso_c_binding, only: c_int, c_float, c_double, c_long_double
    use c_to_fort
    use uns_wrapper
    use seed_ass
    use rnd_generator
    use math_func
    implicit none
    ! Randomic Variables Declaration.
    type(uint32) :: RandomLine, RandomCol, RandomNbr
    ! Measurement Variables Declaration.
    integer(c_int) :: M, maxSamples, maxLines, maxColumns, maxTime, Lx0, Ly0, Lx, Ly, sampleCount, timeParameter, heightParameter, &
                      Hours, Minutes
    real(c_double) :: RandomProb, timeAmount, depProbability, Omega, startTime, endTime, Duration, Seconds
    real(c_long_double) :: avgH, avgSquaredM, avgCubicM, avgQuadricM, FirstK, SecondK, ThirdK, FourthK, Skewness, Kurtosis
    ! Arrays Declaration:
    integer(c_int), allocatable :: Substract(:,:) ! Main Substract.
    integer(c_int), allocatable :: MeasLineDB(:), MeasColumnDB(:), NbrHeight(:), TopNeighbor(:), BottomNeighbor(:), RightNeighbor(:), LeftNeighbor(:)
    real(c_long_double), allocatable :: CentralMomentsDB(:,:), CumulantsDB(:,:), MeasTimeDB(:), RawMomentsDB(:), SkewnessDB(:), &
                                        KurtosisDB(:)
    ! Randomic Seed Assign.
    call R3SeedAssign()

    ! Request Informations:
    ! Presentation.
    print *, "Behavior: Expanded Spherical Geometry Deposition."
    print *, "Model: Conservative Restricted Solid-on-Solid (C-RSOS)."
    ! Data Entry.
    print *, "General Configuration:"
    write(*, '(A)', advance="no") " Amount of Samples: "; read *, maxSamples
    write(*, '(A)', advance="no") " Growth Speed: "; read *, Omega
    write(*, '(A)', advance="no") " Local Slope: "; read *, M
    write(*, '(A)', advance="no") " Maximum Time: "; read *, maxTime
    print *, "Substract Setup:"
    write(*, '(A)', advance="no") " Lx0 Size: "; read *, Lx0
    write(*, '(A)', advance="no") " Ly0 Size: "; read *, Ly0
    write(*, '(A)', advance="no") " Max. Lx Size: "; read *, maxLines
    write(*, '(A)', advance="no") " Max. Ly Size: "; read *, maxColumns

    ! Bidimensional Array Allocation:
    allocate(Substract(0:maxLines - 1, 0:maxColumns - 1)) ! Substract Array.
    allocate(CentralMomentsDB(4, maxTime)) ! Central Moments Database.
    allocate(CumulantsDB(4, maxTime)) ! Cumulants Database.
    ! Unidimensional Array Allocation:
    allocate(MeasTimeDB(maxTime)) ! Measurement Time Database.
    allocate(MeasLineDB(maxTime)) ! Measurement Line Database.
    allocate(MeasColumnDB(maxTime)) ! Measurement Column Database.
    allocate(NbrHeight(4)) ! Neighbor's Height Array.
    allocate(RawMomentsDB(maxTime)) ! Raw Moments Database.
    allocate(SkewnessDB(maxTime)) ! Skewness Database.
    allocate(KurtosisDB(maxTime)) ! Kurtosis Database.
    allocate(TopNeighbor(0:maxLines - 1)) ! Top Neighbor Database.
    allocate(BottomNeighbor(0:maxLines - 1)) ! Bottom Neighbor Database.
    allocate(RightNeighbor(0:maxColumns - 1)) ! Right Neighbor Database.
    allocate(LeftNeighbor(0:maxColumns - 1)) ! Left Neighbor Database.
    ! Iniatilizing (Except the Substract and Neighbor's Array).
    CentralMomentsDB = 0.0_c_long_double
    CumulantsDB = 0.0_c_long_double
    MeasTimeDB = 0.0_c_double
    MeasLineDB = 0
    MeasColumnDB = 0
    RawMomentsDB = 0.0_c_long_double
    SkewnessDB = 0.0_c_long_double
    KurtosisDB = 0.0_c_long_double

    ! Start Timer.
    call cpu_time(startTime)
    do sampleCount = 1, maxSamples
        ! Axis Configure.
        Lx = Lx0
        Ly = Ly0
        ! Initializing Substract and Neighbor's Array.
        Substract = 0
        TopNeighbor = 0
        BottomNeighbor = 0
        RightNeighbor = 0
        LeftNeighbor = 0
        ! Reseting Last Iteration of Variables:
        FirstK = 0.0_c_long_double
        SecondK = 0.0_c_long_double
        ThirdK = 0.0_c_long_double
        FourthK = 0.0_c_long_double
        Skewness = 0.0_c_long_double
        Kurtosis = 0.0_c_long_double
        timeAmount = 0.0_c_double
        timeParameter = 1

        ! Seed Assignmets.
        call RProbSeedAssign() ! To 'RProb'.
        call RLineSeedAssign() ! To 'RLine'.
        call RColSeedAssign() ! To 'RCol'.
        call RNbrSeedAssign() ! To 'RNbr'.

        ! Periodic Boundary Condition:
        ! X Axis.
        do i = 0, Lx - 1
            if (i == 0) then
                TopNeighbor(i)    = Lx - 1
                BottomNeighbor(i) = i + 1
            else if (i == Lx - 1) then
                TopNeighbor(i)    = i - 1
                BottomNeighbor(i) = 0
            else
                TopNeighbor(i)    = i - 1
                BottomNeighbor(i) = i + 1
            end if
        end do
        ! Y Axis.
        do j = 0, Ly - 1
            if (j == 0) then
                RightNeighbor(j) = j + 1
                LeftNeighbor(j)  = Ly - 1
            else if (j == Ly - 1) then
                RightNeighbor(j) = 0
                LeftNeighbor(j)  = j - 1
            else
                RightNeighbor(j) = j + 1
                LeftNeighbor(j)  = j - 1
            end if
        end do
        ! Main Simulation.
        do while (timeAmount < maxTime)
            ! Time Stamp.
            timeAmount = timeAmount + 1.0_c_double/(Lx*Ly + 2*Omega)
            ! RProb between [0, 1].
            RandomProb = RProb()
            ! Deposition Probability.
            depProbability = real((Lx*Ly)/(Lx*Ly + 2*Omega), kind=c_float)
            ! Deposition Process.
            if (RandomProb <= depProbability) then
                ! Random Position Assortment.
                call RandomLine%setUns(RLine(Lx)) ! For X Axis.
                call RandomCol%setUns(RCol(Ly)) ! For Y Axis.
                ! C-RSOS Rule.
                c_rsos: do
                    ! Current Pseudo-Height.
                    heightParameter = Substract(RandomLine%toDecimal(), RandomCol%toDecimal()) + 1
                    ! Neighbor's Information Acquisition.
                    NbrHeight(1) = Substract(TopNeighbor(RandomLine%toDecimal()), RandomCol%toDecimal())
                    NbrHeight(2) = Substract(BottomNeighbor(RandomLine%toDecimal()), RandomCol%toDecimal())
                    NbrHeight(3) = Substract(RandomLine%toDecimal(), RightNeighbor(RandomCol%toDecimal()))
                    NbrHeight(4) = Substract(RandomLine%toDecimal(), LeftNeighbor(RandomCol%toDecimal()))
                    ! RSOS Rule.
                    rsos: block
                        do i = 1, 4
                            if (abs(NbrHeight(i) - heightParameter) > M) then
                                ! RSOS Rule isn't Satisfied.
                                exit rsos
                            end if
                        end do
                        ! RSOS Rule is Satisfied.
                        Substract(RandomLine%toDecimal(), RandomCol%toDecimal()) = Substract(RandomLine%toDecimal(), RandomCol%toDecimal()) + 1
                        exit c_rsos
                    end block rsos
                    call RandomNbr%setUns(RNbr(4))
                    ! Diffusion Process.
                    select case (RandomNbr%toDecimal())
                        case (0)
                            call RandomLine%setUns(TopNeighbor(RandomLine%toDecimal()))
                        case (1)
                            call RandomLine%setUns(BottomNeighbor(RandomLine%toDecimal()))
                        case (2)
                            call RandomCol%setUns(RightNeighbor(RandomCol%toDecimal()))
                        case (3)
                            call RandomCol%setUns(LeftNeighbor(RandomCol%toDecimal()))
                    end select
                end do c_rsos
            ! Expansion Process.
            else
                RandomProb = RProb()
                ! Expansion in X-Axis Direction.
                if (RandomProb >= 0.5) then
                    call RandomLine%setUns(RLine(Lx))
                    do i = 0, Ly - 1
                        Substract(Lx, i) = Substract(RandomLine%toDecimal(), i)
                    end do
                    ! Neighbor's Update.
                    TopNeighbor(BottomNeighbor(RandomLine%toDecimal())) = Lx
                    BottomNeighbor(Lx) = BottomNeighbor(RandomLine%toDecimal())
                    BottomNeighbor(RandomLine%toDecimal()) = Lx
                    TopNeighbor(Lx) = RandomLine%toDecimal()
                    ! Update the Lx Substract Size.
                    Lx = Lx + 1
                ! Expansion in Y-Axis Direction.
                else
                    call RandomCol%setUns(RCol(Ly))
                    do i = 0, Lx - 1
                        Substract(i, Ly) = Substract(i, RandomCol%toDecimal())
                    end do
                    ! Neighbor's Update.
                    LeftNeighbor(RightNeighbor(RandomCol%toDecimal())) = Ly
                    RightNeighbor(Ly) = RightNeighbor(RandomCol%toDecimal())
                    RightNeighbor(RandomCol%toDecimal()) = Ly
                    LeftNeighbor(Ly) = RandomCol%toDecimal()
                    ! Update the Ly Substract Size.
                    Ly = Ly + 1
                end if
            end if
            ! Analysis Process.
            if (timeAmount >= timeParameter) then
                ! Reseting Periodic Iteration of Variables.
                avgH = 0.0_c_long_double
                avgSquaredM = 0.0_c_long_double
                avgCubicM = 0.0_c_long_double
                avgQuadricM = 0.0_c_long_double
                ! Computing:
                ! Raw Moment.
                do i = 0, Lx - 1
                    do j = 0, Ly - 1
                        avgH = avgH + real(Substract(i, j), kind=c_long_double) / real(Lx*Ly, kind=c_long_double)
                    end do
                end do
                ! Central Moments.
                do i = 0, Lx - 1
                    do j = 0, Ly - 1
                        avgSquaredM = avgSquaredM + Sq(real(Substract(i, j) - avgH, kind=c_long_double)) / real(Lx*Ly, kind=c_long_double)
                        avgCubicM = avgCubicM + Cb(real(Substract(i, j) - avgH, kind=c_long_double)) / real(Lx*Ly, kind=c_long_double)
                        avgQuadricM = avgQuadricM + Qd(real(Substract(i, j) - avgH, kind=c_long_double)) / real(Lx*Ly, kind=c_long_double)
                    end do
                end do
                ! Cumulants.
                FirstK = avgH
                SecondK = avgSquaredM
                ThirdK = avgCubicM
                FourthK = avgQuadricM - 3*Sq(avgSquaredM)
                ! Skewness and Kurtosis.
                Skewness = ThirdK/Cb(sqrt(SecondK))
                Kurtosis = FourthK/Sq(SecondK)
                ! Measure's Storage.
                MeasTimeDB(timeParameter) = MeasTimeDB(timeParameter) + timeAmount
                MeasLineDB(timeParameter) = MeasLineDB(timeParameter) + Lx
                MeasColumnDB(timeParameter) = MeasColumnDB(timeParameter) + Ly
                RawMomentsDB(timeParameter) = RawMomentsDB(timeParameter) + avgH
                CentralMomentsDB(2, timeParameter) = CentralMomentsDB(2, timeParameter) + avgSquaredM
                CentralMomentsDB(3, timeParameter) = CentralMomentsDB(3, timeParameter) + avgCubicM
                CentralMomentsDB(4, timeParameter) = CentralMomentsDB(4, timeParameter) + avgQuadricM
                CumulantsDB(1, timeParameter) = CumulantsDB(1, timeParameter) + FirstK
                CumulantsDB(2, timeParameter) = CumulantsDB(2, timeParameter) + SecondK
                CumulantsDB(3, timeParameter) = CumulantsDB(3, timeParameter) + ThirdK
                CumulantsDB(4, timeParameter) = CumulantsDB(4, timeParameter) + FourthK
                SkewnessDB(timeParameter) = SkewnessDB(timeParameter) + Skewness
                KurtosisDB(timeParameter) = KurtosisDB(timeParameter) + Kurtosis
                ! Time Parameter Update.
                timeParameter = timeParameter + 1
            end if
        end do
    end do
    ! Taking the Average.
    do i = 1, maxTime
        MeasTimeDB(i) = MeasTimeDB(i)/maxSamples
        MeasLineDB(i) = MeasLineDB(i)/maxSamples
        MeasColumnDB(i) = MeasColumnDB(i)/maxSamples
        RawMomentsDB(i) = RawMomentsDB(i)/maxSamples
        SkewnessDB(i) = SkewnessDB(i)/maxSamples
        KurtosisDB(i) = KurtosisDB(i)/maxSamples
        do j = 1, 4
            CentralMomentsDB(j, i) = CentralMomentsDB(j, i)/maxSamples
            CumulantsDB(j, i) = CumulantsDB(j, i)/maxSamples
        end do
    end do
    ! Information Transcript.
    ! Substract Size.
    open(unit=1, file="LxSize-CRSOS-Simulation.dat", status='NEW', action='WRITE')
    do i = 1, maxTime
        write(1, '(F40.34, 1X, I0)') MeasTimeDB(i), MeasLineDB(i)
    end do
    close(unit=1)
    open(unit=2, file="LySize-CRSOS-Simulation.dat", status='NEW', action='WRITE')
    do i = 1, maxTime
        write(2, '(F40.34, 1X, I0)') MeasTimeDB(i), MeasColumnDB(i)
    end do
    close(unit=2)
    ! Method A.
    open(unit=3, file="MethodA-CRSOS-Simulation.dat", status='NEW', action='WRITE')
    do i = 1, maxTime
        write(3, '(F40.34, 1X, F37.34, 1X, F37.34, 1X, F37.34, 1X, F50.34)') MeasTimeDB(i), RawMomentsDB(i), SkewnessDB(i), &
                 KurtosisDB(i), CumulantsDB(2, i)
    end do
    close(unit=3)
    ! Method B.
    do i = 1, maxTime
        SkewnessDB(i) = CumulantsDB(3, i)/Cb(real(sqrt(CumulantsDB(2, i)), kind=c_long_double))
        KurtosisDB(i) = CumulantsDB(4, i)/Sq(real(CumulantsDB(2, i), kind=c_long_double))
    end do
    open(unit=4, file="MethodB-CRSOS-Simulation.dat", status='NEW', action='WRITE')
    do i = 1, maxTime
        write(4, '(F40.34, 1X, F37.34, 1X, F37.34, 1X, F37.34, 1X, F50.34)') MeasTimeDB(i), RawMomentsDB(i), SkewnessDB(i), &
                  KurtosisDB(i), CumulantsDB(2, i)
    end do
    close(4)
    ! Method C.
    do i = 1, maxTime
        CumulantsDB(1, i) = RawMomentsDB(i)
        CumulantsDB(2, i) = CentralMomentsDB(2, i)
        CumulantsDB(3, i) = CentralMomentsDB(3, i)
        CumulantsDB(4, i) = CentralMomentsDB(4, i) - 3*Sq(real(CentralMomentsDB(2, i), kind=c_long_double))
    end do
    do i = 1, maxTime
        SkewnessDB(i) = CumulantsDB(3, i)/Cb(real(sqrt(CumulantsDB(2, i)), kind=c_long_double))
        KurtosisDB(i) = CumulantsDB(4, i)/Sq(real(CumulantsDB(2, i), kind=c_long_double))
    end do
    open(unit=5, file="MethodC-CRSOS-Simulation.dat", status='NEW', action='WRITE')
    do i = 1, maxTime
        write(5, '(F40.34, 1X, F37.34, 1X, F37.34, 1X, F37.34, 1X, F50.34)') MeasTimeDB(i), RawMomentsDB(i), SkewnessDB(i), &
                  KurtosisDB(i), CumulantsDB(2, i)
    end do
    close(5)

    ! End Timer.
    call cpu_time(endTime)
    Duration = endTime - startTime
    Hours = int(Duration/3600)
    Minutes = int((Duration - Hours + 3600) / 60)
    Seconds = Duration - Hours * 3600 - Minutes * 60

    ! Memory Deallocation.
    deallocate(Substract, CentralMomentsDB, CumulantsDB)
    deallocate(TopNeighbor, BottomNeighbor, RightNeighbor, LeftNeighbor)
    deallocate(MeasTimeDB, MeasLineDB, MeasColumnDB, RawMomentsDB, SkewnessDB, KurtosisDB)
end program main

