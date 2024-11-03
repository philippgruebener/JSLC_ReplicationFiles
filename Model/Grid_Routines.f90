!#######################################################################
! Module Grids
!#######################################################################

module Grid_Routines

    implicit none
    
contains

    subroutine LinearSpacedGrid(n,low,high,y)
    ! gives a linearly spaced between low and high

        integer, intent(in) :: n        ! number of grid points
        real*8, intent(in) :: low,high  ! lower and upper bound
        real*8, intent(out) :: y(:)     ! grid
        integer :: i                    ! counter
        real*8 :: x(n)                  ! auxiliary variable to create grid

        if (n<2) then
            write(*,*) 'n must be at least 2 to make grids'
            return
        end if

        if (n==2) then
            y(1) = low
            y(2) = high
            return
        end if

    x(1) = 0.0
    x(n) = 1.0
    do i = 2,n-1
        x(i) = (i-1)/dble(n-1)
    end do

    y = low + (high-low)*x

    end subroutine LinearSpacedGrid

    subroutine PowerSpacedGrid(n,k,low,high,y)
    ! gives a grid spaced between low and high based on the unit interval with a function x^(1/k)
    ! with k = 1 grid is linear; with k = 0 grid is L-shaped

        integer, intent(in) :: n            ! number of grid points
        real*8, intent(in) :: k,low,high    ! power parameter, lower bound, upper bound
        real*8, intent(out) :: y(:)         ! grid
        integer :: i                        ! counter
        real*8 :: x(n),z(n)                 ! auxiliary variables to create grid

        if (n<2) then
            write(*,*) 'n must be at least 2 to make grids'
            return
        end if

        if (n==2) then
            y(1) = low
            y(2) = high
            return
        end if

        x(1) = 0d0
        x(n) = 1d0
        do i = 2,n-1
            x(i) = (i-1)/dble(n-1)
        end do

        z = x**(1d0/k)

        y = low + (high-low)*z
    
    end subroutine PowerSpacedGrid

    subroutine FindGridWeights(a,agrid,AA,indabove,indbelow,wabove,wbelow)

        integer, intent(in) :: AA           ! number of grid points
        real*8, intent(in) :: a             ! asset level
        real*8, intent(in) :: agrid(:)      ! asset grid
        real*8, intent(out) :: wabove, wbelow           ! weights nearest grid points
        integer, intent(out) :: indabove, indbelow      ! indices nearest grid points
        integer :: i                        ! counter

        !  find index below asset level
        indbelow = 1

        do i=2,AA
            if((agrid(i)<=a)) then
                indbelow=i
            end if
        end do
        indbelow = MIN(indbelow,AA-1)
        indabove = indbelow + 1
        
        ! find weights attached to point below/above policy
        wabove = (a-agrid(indbelow))/(agrid(indbelow+1)-agrid(indbelow))
        wabove = MIN(wabove,1d0)
        wabove = MAX(wabove,0d0) ! can be binding if choice out of grid
        wbelow= 1d0-wabove

    end subroutine FindGridWeights

end module
