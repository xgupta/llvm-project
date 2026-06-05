program real_kinds
    implicit none

    real(4)  :: float_four
    real(8)  :: float_eight
    real(16) :: float_sixteen

    float_four    = 3.1415926_4
    float_eight   = 2.718281828459045_8
    float_sixteen = 1.618033988749894848204586834365638_16

    print *, "Done" ! Breakpoint here

end program real_kinds