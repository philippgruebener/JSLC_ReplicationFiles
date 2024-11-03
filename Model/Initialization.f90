!#######################################################################
! Module Initialization
! Create grids, transition matrices, initial distribution etc. 
!#######################################################################

include "Grid_Routines.f90"

module Initialization

    implicit none
    
contains

    subroutine initialize()
    
        use Globals
        use Grid_Routines

        ! Local variable declarations
        integer :: iter_i, iter_j, iter_k       ! counter

        ! helpers for initial distributions
        integer, dimension(2,2) :: h_by_college
        real*8, dimension(2,2) :: college_EE, college_EU, college_EN, college_UU, college_UN, college_NN
        real*8, dimension(2,2) :: asset_EE, asset_EU, asset_EN, asset_UU, asset_UN, asset_NN
        
        real*8 :: wabove, wbelow         ! weights nearest grid points
        integer :: indabove, indbelow          ! indices nearest grid points
        integer :: indInit_a_below, indInit_a_above
        real*8 :: mass_a
        real*8, dimension(3) :: dist_h
        real*8, dimension(3) :: dist_a_below
        real*8, dimension(6) :: dist_a_above


        ! Store number of grid points in different grids
        open (unit=10,file="Output/dimensions.txt",action="write",status="replace")
        write (10,*) AA, TT, HH, NN
        close(10)
        
        ! Asset grid
        call PowerSpacedGrid(AA,0.7D0,Amin,Amax,assgrid)
        open (unit=11,file="Output/assgrid.txt",action="write",status="replace")
        write (11,*) assgrid
        close(11)
        
        ! Human capital grid
        call PowerSpacedGrid(HH,1D0,hmin,hmax,hgrid)
        hgrid = hgrid/chi   ! hgrid bounds in globals are set such that they correspond to earnings; human capital adjusted to also cover profits
        open (unit=12,file="Output/hgrid.txt",action="write",status="replace")
        write (12,*) hgrid
        close(12)
        
        ! Wages
        do iter_i = 1, HH
            wage(iter_i) = (1d0-0.28d0)*chi*hgrid(iter_i)   ! taxes 0.28 from Trabandt Uhlig; wages coded as after-tax wages; based on human capital less profits
        enddo
        open (unit=18,file="Output/wage.txt",action="write",status="replace")
        write (18,*) wage
        close(18)
        
        ! firm flow profit
        do iter_i = 1, HH
            prof(iter_i) = (1d0-chi)*hgrid(iter_i)
        enddo
        open (unit=18,file="Output/prof.txt",action="write",status="replace")
        write (18,*) prof
        close(18)

        ! UI benefits
        do iter_i = 1,HH
            ben(iter_i) = min(ben_rep*chi*hgrid(iter_i),ben_rep*chi*hgrid(5))
        enddo

        ! pensions
        do iter_i=1,HH
            do iter_j=1,HH
                pen(iter_i,iter_j)= 0.35d0
            enddo 
        enddo
        
        open (unit=20,file="Output/ben.txt",action="write",status="replace")
        write (20,*) ben
        close(20)

        open (unit=22,file="Output/pen.txt",action="write",status="replace")
        write (22,*) pen
        close(22)
        
        ! PBs conditional on human capital
        do iter_i=1,HH
            ! job losses
            delta(:,iter_i) = delta_lev * (iter_i**delta_exp)
            ! human capital transitions
            phiup(iter_i) = phiup_lev * (iter_i**phiup_exp)
            phidown(iter_i) = phidown_lev * (iter_i**phidown_exp)
            phidown_emp(iter_i) = phidown_emp_lev * (iter_i**phidown_emp_exp)
        enddo
        
        open (unit=22,file="Output/delta.txt",action="write",status="replace")
        write (22,*) delta(1,:)
        close(22)

        open (unit=22,file="Output/phiup.txt",action="write",status="replace")
        write (22,*) phiup
        close(22)

        open (unit=22,file="Output/phidown.txt",action="write",status="replace")
        write (22,*) phidown
        close(22)

        open (unit=22,file="Output/phidown_emp.txt",action="write",status="replace")
        write (22,*) phidown_emp
        close(22)
        
        ! fill h transition matrices
        htrans_EE = 0d0

        htrans_EE(HH,HH-1) = phidown_emp(HH)
        htrans_EE(HH,HH) = 1d0-phidown_emp(HH)

        htrans_EE(1,2) = phiup(1)
        htrans_EE(1,1) = 1d0-phiup(1)

        do iter_i=2,(HH-1)
                htrans_EE(iter_i,iter_i) = 1d0-phiup(iter_i)-phidown_emp(iter_i)
                htrans_EE(iter_i,iter_i+1) = phiup(iter_i)
                htrans_EE(iter_i,iter_i-1) = phidown_emp(iter_i)
        enddo

        htrans_XX = 0d0
        htrans_XX(1,1) = 1d0
        do iter_i=2,(HH)
                htrans_XX(iter_i,iter_i) = 1d0-phidown(iter_i)
                htrans_XX(iter_i,iter_i-1) = phidown(iter_i)
        enddo

        htrans_EX = htrans_EE

        open (unit=22,file="Output/htrans_EE.txt",action="write",status="replace")
        write (22,*) htrans_EE
        close(22)

        open (unit=22,file="Output/htrans_XX.txt",action="write",status="replace")
        write (22,*) htrans_XX
        close(22)

        open (unit=22,file="Output/htrans_EX.txt",action="write",status="replace")
        write (22,*) htrans_EX
        close(22)

        ! initialize lambdas
        lambda_UE_E = lambda_u
        lambda_UE_X = lambda_u
        lambda_UU = lambda_u
        lambda_UN = lambda_u
        lambda_NU = lambda_n
        lambda_NN = lambda_n
        lambda_SS = lambda_s
        lambda_SE_E = lambda_s
        lambda_SE_X = lambda_s
        lambda_SN = lambda_s
        lambda_NS = lambda_n
        lambda_SU = lambda_s
        lambda_US = lambda_u
        lambda_NE_E = lambda_n
        lambda_NE_X = lambda_n

        ! "psi": (dis-) utility of work/search
        psi_EE = 0d0 
        psi_UU = 0.5d0
        psi_NN = eps1
        psi_EU = eps3
        
        do iter_i = 1, TT
            psi_EN(iter_i) = psi_scale + (-psi_search)*(1d0/(1d0+exp(-0.05d0*(dble(iter_i)-100d0))))
        enddo
        do iter_i = 1, TT
            psi_UN(iter_i) = eps2 + (-psi_search)*(1d0/(1d0+exp(-0.05d0*(dble(iter_i)-100d0))))
        enddo

        psi_UE = psi_EU
        psi_NU = psi_UN
        psi_SS = psi_UU
        psi_SE = psi_UE
        psi_ES = psi_EU
        psi_SN = psi_UN
        psi_NS = psi_NU
        psi_SU = psi_UU
        psi_US = psi_UU
        psi_NE = psi_EN

        open (unit=22,file="Output/psi_EE.txt",action="write",status="replace")
        write (22,*) psi_EE
        close(22)

        open (unit=22,file="Output/psi_EU.txt",action="write",status="replace")
        write (22,*) psi_EU
        close(22)

        open (unit=22,file="Output/psi_UE.txt",action="write",status="replace")
        write (22,*) psi_UE
        close(22)

        open (unit=22,file="Output/psi_EN.txt",action="write",status="replace")
        write (22,*) psi_EN
        close(22)

        open (unit=22,file="Output/psi_NE.txt",action="write",status="replace")
        write (22,*) psi_NE
        close(22)

        open (unit=22,file="Output/psi_UU.txt",action="write",status="replace")
        write (22,*) psi_UU
        close(22)

        open (unit=22,file="Output/psi_UN.txt",action="write",status="replace")
        write (22,*) psi_UN
        close(22)

        open (unit=22,file="Output/psi_NU.txt",action="write",status="replace")
        write (22,*) psi_NU
        close(22)

        open (unit=22,file="Output/psi_NN.txt",action="write",status="replace")
        write (22,*) psi_NN
        close(22)

        open (unit=22,file="Output/psi_SS.txt",action="write",status="replace")
        write (22,*) psi_SS
        close(22)

        open (unit=22,file="Output/psi_SU.txt",action="write",status="replace")
        write (22,*) psi_SU
        close(22)

        open (unit=22,file="Output/psi_US.txt",action="write",status="replace")
        write (22,*) psi_US
        close(22)

        open (unit=22,file="Output/psi_SN.txt",action="write",status="replace")
        write (22,*) psi_SN
        close(22)

        open (unit=22,file="Output/psi_NS.txt",action="write",status="replace")
        write (22,*) psi_NS
        close(22)

        open (unit=22,file="Output/psi_SE.txt",action="write",status="replace")
        write (22,*) psi_SE
        close(22)

        open (unit=22,file="Output/psi_ES.txt",action="write",status="replace")
        write (22,*) psi_ES
        close(22)

        
        ! initial distributions
        
        ! initial distribution over labor market states
        distinit_LS = 0d0
        
        distinit_LS(1)= 0.65d0      ! EE
        distinit_LS(2)= 0d0         ! EU
        distinit_LS(3)= 0d0         ! UE
        distinit_LS(4)= 0.125d0     ! EN
        distinit_LS(5)= 0.125d0     ! NE
        distinit_LS(6)= 0d0         ! UU
        distinit_LS(7)= 0d0         ! UN
        distinit_LS(8)= 0d0         ! NU
        distinit_LS(9)= 0.02d0      ! NN
        distinit_LS(10)= 0.01d0     ! SS 
        distinit_LS(11)= 0.01d0     ! SN
        distinit_LS(12)= 0.01d0     ! NS
        distinit_LS(13)= 0d0        ! SU 
        distinit_LS(14)= 0d0        ! US
        distinit_LS(15)= 0.025d0    ! SE
        distinit_LS(16)= 0.025d0    ! ES       

        ! distributions of couples in different joint labor market states over assets and human capital of both spouses
        distinit_EE = 0d0
        distinit_EU = 0d0
        distinit_EN = 0d0
        distinit_UE = 0d0
        distinit_NE = 0d0
        distinit_UU = 0d0
        distinit_UN = 0d0
        distinit_NU = 0d0
        distinit_NN = 0d0
        distinit_SS = 0d0
        distinit_SE = 0d0
        distinit_ES = 0d0
        distinit_SU = 0d0
        distinit_US = 0d0
        distinit_SN = 0d0
        distinit_NS = 0d0
        
        ! initial HC states by college (1 is college, 2 is non college, column is individual itself and row is spouse):
        h_by_college(1,1) = 5
        h_by_college(1,2) = 4
        h_by_college(2,1) = 4
        h_by_college(2,2) = 3

        ! spread total mass around central grid point for human capital
        ! careful: works only for 2<=h<=11
        dist_h(1) = 0.2d0
        dist_h(2) = 0.6d0
        dist_h(3) = 0.2d0

        ! spread total mass around asset grid points
        indInit_a_below = 3
        indInit_a_above = 6

        mass_a=0.35d0

        dist_a_below(3)=0.1d0*(1d0-mass_a)/2d0
        dist_a_below(2)=0.3d0*(1d0-mass_a)/2d0
        dist_a_below(1)=0.6d0*(1d0-mass_a)/2d0

        dist_a_above(6)=0.05d0*(1d0-mass_a)/2d0
        dist_a_above(5)=0.05d0*(1d0-mass_a)/2d0
        dist_a_above(4)=0.1d0*(1d0-mass_a)/2d0
        dist_a_above(3)=0.1d0*(1d0-mass_a)/2d0
        dist_a_above(2)=0.25d0*(1d0-mass_a)/2d0
        dist_a_above(1)=0.45d0*(1d0-mass_a)/2d0


        ! initial distribution of college degrees by LS (column is head, i.e. E spouse)

        college_EE(1,1)= 0.28d0
        college_EE(1,2)= 0.11d0
        college_EE(2,1)= 0.11d0
        college_EE(2,2)= 1d0-college_EE(1,1)-college_EE(1,2)-college_EE(2,1)  

        college_EU(1,1)= 0.16d0 
        college_EU(1,2)= 0.07d0
        college_EU(2,1)= 0.08d0 
        college_EU(2,2)= 1d0-college_EU(1,1)-college_EU(1,2)-college_EU(2,1)    

        college_EN(1,1)= 0.16d0 
        college_EN(1,2)= 0.07d0 
        college_EN(2,1)= 0.09d0 
        college_EN(2,2)= 1d0-college_EN(1,1)-college_EN(1,2)-college_EN(2,1)    

        college_UU(1,1)= 0.09d0 
        college_UU(1,2)= 0.06d0 
        college_UU(2,1)= 0.05d0
        college_UU(2,2)= 1d0-college_UU(1,1)-college_UU(1,2)-college_UU(2,1)    

        college_UN(1,1)= 0.07d0 
        college_UN(1,2)= 0.04d0 
        college_UN(2,1)= 0.02d0 
        college_UN(2,2)= 1d0-college_UN(1,1)-college_UN(1,2)-college_UN(2,1)    

        college_NN(1,1)= 0.15d0 
        college_NN(1,2)= 0.08d0 
        college_NN(2,1)= 0.06d0 
        college_NN(2,2)= 1d0-college_NN(1,1)-college_NN(1,2)-college_NN(2,1)    


        ! initial asset levels by LS and college degree (median)

        asset_EE(1,1)= 2.70d0   
        asset_EE(1,2)= 0.74d0   
        asset_EE(2,1)= 0.67d0   
        asset_EE(2,2)= 0.05d0   

        asset_EU(1,1)= 1.01d0   
        asset_EU(1,2)= 0.00d0   
        asset_EU(2,1)= 0.10d0   
        asset_EU(2,2)= 0.00d0   

        asset_EN(1,1)= 2.33d0   
        asset_EN(1,2)= 0.10d0   
        asset_EN(2,1)= 0.16d0   
        asset_EN(2,2)= 0.00d0   

        asset_UU(1,1)= 0.00d0   
        asset_UU(1,2)= 0.03d0   
        asset_UU(2,1)= 0.00d0   
        asset_UU(2,2)= 0.00d0   

        asset_UN(1,1)= 0.46d0  
        asset_UN(1,2)= 0.00d0   
        asset_UN(2,1)= 0.01d0   
        asset_UN(2,2)= 0.00d0   

        asset_NN(1,1)= 0.10d0   
        asset_NN(1,2)= 0.02d0   
        asset_NN(2,1)= 0.00d0   
        asset_NN(2,2)= 0.00d0   

        
        ! initialize joint distribution of assets / college by LS
        
        ! loops over college of head and spouse
        do iter_k = 1,2 
            do iter_j = 1,2    

                ! EE couple
                call FindGridWeights(asset_EE(iter_k,iter_j),assgrid,AA,indabove,indbelow,wabove,wbelow) ! find grid point for median
                call add2dist(distinit_EE,college_EE(iter_k,iter_j),indabove,indbelow,wabove,wbelow,h_by_college(iter_k,iter_j),h_by_college(iter_j,iter_k),dist_h,mass_a,dist_a_below,dist_a_above,indInit_a_below,indInit_a_above,AA,HH)

                ! EU couple
                call FindGridWeights(asset_EU(iter_k,iter_j),assgrid,AA,indabove,indbelow,wabove,wbelow)
                call add2dist(distinit_EU,college_EU(iter_k,iter_j),indabove,indbelow,wabove,wbelow,h_by_college(iter_k,iter_j),h_by_college(iter_j,iter_k),dist_h,mass_a,dist_a_below,dist_a_above,indInit_a_below,indInit_a_above,AA,HH)

                ! UE couple
                call FindGridWeights(asset_EU(iter_j,iter_k),assgrid,AA,indabove,indbelow,wabove,wbelow)
                call add2dist(distinit_UE,college_EU(iter_j,iter_k),indabove,indbelow,wabove,wbelow,h_by_college(iter_j,iter_k),h_by_college(iter_k,iter_j),dist_h,mass_a,dist_a_below,dist_a_above,indInit_a_below,indInit_a_above,AA,HH)

                ! ES couple
                call FindGridWeights(asset_EU(iter_k,iter_j),assgrid,AA,indabove,indbelow,wabove,wbelow)
                call add2dist(distinit_ES,college_EU(iter_k,iter_j),indabove,indbelow,wabove,wbelow,h_by_college(iter_k,iter_j),h_by_college(iter_j,iter_k),dist_h,mass_a,dist_a_below,dist_a_above,indInit_a_below,indInit_a_above,AA,HH)

                ! SE couple
                call FindGridWeights(asset_EU(iter_j,iter_k),assgrid,AA,indabove,indbelow,wabove,wbelow)
                call add2dist(distinit_SE,college_EU(iter_j,iter_k),indabove,indbelow,wabove,wbelow,h_by_college(iter_j,iter_k),h_by_college(iter_k,iter_j),dist_h,mass_a,dist_a_below,dist_a_above,indInit_a_below,indInit_a_above,AA,HH)
                
                ! EN couple
                call FindGridWeights(asset_EN(iter_k,iter_j),assgrid,AA,indabove,indbelow,wabove,wbelow)
                call add2dist(distinit_EN,college_EN(iter_k,iter_j),indabove,indbelow,wabove,wbelow,h_by_college(iter_k,iter_j),h_by_college(iter_j,iter_k),dist_h,mass_a,dist_a_below,dist_a_above,indInit_a_below,indInit_a_above,AA,HH)

                ! NE couple
                call FindGridWeights(asset_EN(iter_j,iter_k),assgrid,AA,indabove,indbelow,wabove,wbelow)
                call add2dist(distinit_NE,college_EN(iter_j,iter_k),indabove,indbelow,wabove,wbelow,h_by_college(iter_j,iter_k),h_by_college(iter_k,iter_j),dist_h,mass_a,dist_a_below,dist_a_above,indInit_a_below,indInit_a_above,AA,HH)

                ! UU couple
                call FindGridWeights(asset_UU(iter_k,iter_j),assgrid,AA,indabove,indbelow,wabove,wbelow)
                call add2dist(distinit_UU,college_UU(iter_k,iter_j),indabove,indbelow,wabove,wbelow,h_by_college(iter_k,iter_j),h_by_college(iter_j,iter_k),dist_h,mass_a,dist_a_below,dist_a_above,indInit_a_below,indInit_a_above,AA,HH)

                ! SS couple
                call FindGridWeights(asset_UU(iter_k,iter_j),assgrid,AA,indabove,indbelow,wabove,wbelow)
                call add2dist(distinit_SS,college_UU(iter_k,iter_j),indabove,indbelow,wabove,wbelow,h_by_college(iter_k,iter_j),h_by_college(iter_j,iter_k),dist_h,mass_a,dist_a_below,dist_a_above,indInit_a_below,indInit_a_above,AA,HH)

                ! US couple
                call FindGridWeights(asset_UU(iter_k,iter_j),assgrid,AA,indabove,indbelow,wabove,wbelow)
                call add2dist(distinit_US,college_UU(iter_k,iter_j),indabove,indbelow,wabove,wbelow,h_by_college(iter_k,iter_j),h_by_college(iter_j,iter_k),dist_h,mass_a,dist_a_below,dist_a_above,indInit_a_below,indInit_a_above,AA,HH)

                ! SU couple
                call FindGridWeights(asset_UU(iter_j,iter_k),assgrid,AA,indabove,indbelow,wabove,wbelow)
                call add2dist(distinit_SU,college_UU(iter_j,iter_k),indabove,indbelow,wabove,wbelow,h_by_college(iter_j,iter_k),h_by_college(iter_k,iter_j),dist_h,mass_a,dist_a_below,dist_a_above,indInit_a_below,indInit_a_above,AA,HH)

                ! UN couple
                call FindGridWeights(asset_UN(iter_k,iter_j),assgrid,AA,indabove,indbelow,wabove,wbelow)
                call add2dist(distinit_UN,college_UN(iter_k,iter_j),indabove,indbelow,wabove,wbelow,h_by_college(iter_k,iter_j),h_by_college(iter_j,iter_k),dist_h,mass_a,dist_a_below,dist_a_above,indInit_a_below,indInit_a_above,AA,HH)

                ! NU couple
                call FindGridWeights(asset_UN(iter_j,iter_k),assgrid,AA,indabove,indbelow,wabove,wbelow)
                call add2dist(distinit_NU,college_UN(iter_j,iter_k),indabove,indbelow,wabove,wbelow,h_by_college(iter_j,iter_k),h_by_college(iter_k,iter_j),dist_h,mass_a,dist_a_below,dist_a_above,indInit_a_below,indInit_a_above,AA,HH)

                ! SN couple
                call FindGridWeights(asset_UN(iter_k,iter_j),assgrid,AA,indabove,indbelow,wabove,wbelow)
                call add2dist(distinit_SN,college_UN(iter_k,iter_j),indabove,indbelow,wabove,wbelow,h_by_college(iter_k,iter_j),h_by_college(iter_j,iter_k),dist_h,mass_a,dist_a_below,dist_a_above,indInit_a_below,indInit_a_above,AA,HH)

                ! NS couple
                call FindGridWeights(asset_UN(iter_j,iter_k),assgrid,AA,indabove,indbelow,wabove,wbelow)
                call add2dist(distinit_NS,college_UN(iter_j,iter_k),indabove,indbelow,wabove,wbelow,h_by_college(iter_j,iter_k),h_by_college(iter_k,iter_j),dist_h,mass_a,dist_a_below,dist_a_above,indInit_a_below,indInit_a_above,AA,HH)

                ! NN couple
                call FindGridWeights(asset_NN(iter_k,iter_j),assgrid,AA,indabove,indbelow,wabove,wbelow)
                call add2dist(distinit_NN,college_NN(iter_k,iter_j),indabove,indbelow,wabove,wbelow,h_by_college(iter_k,iter_j),h_by_college(iter_j,iter_k),dist_h,mass_a,dist_a_below,dist_a_above,indInit_a_below,indInit_a_above,AA,HH)
                
            end do
        end do
        

        open (unit=226,file="Output/distinit_EE.txt",action="write",status="replace")
        write (226,*) distinit_EE
        close(226)

        open (unit=226,file="Output/distinit_EU.txt",action="write",status="replace")
        write (226,*) distinit_EU
        close(226)

        open (unit=226,file="Output/distinit_UE.txt",action="write",status="replace")
        write (226,*) distinit_UE
        close(226)

        open (unit=226,file="Output/distinit_ES.txt",action="write",status="replace")
        write (226,*) distinit_ES
        close(226)

        open (unit=226,file="Output/distinit_SE.txt",action="write",status="replace")
        write (226,*) distinit_SE
        close(226)

        open (unit=226,file="Output/distinit_EN.txt",action="write",status="replace")
        write (226,*) distinit_EN
        close(226)

        open (unit=226,file="Output/distinit_NE.txt",action="write",status="replace")
        write (226,*) distinit_NE
        close(226)

        open (unit=226,file="Output/distinit_UU.txt",action="write",status="replace")
        write (226,*) distinit_UU
        close(226)

        open (unit=226,file="Output/distinit_SS.txt",action="write",status="replace")
        write (226,*) distinit_SS
        close(226)

        open (unit=226,file="Output/distinit_US.txt",action="write",status="replace")
        write (226,*) distinit_US
        close(226)

        open (unit=226,file="Output/distinit_SU.txt",action="write",status="replace")
        write (226,*) distinit_SU
        close(226)

        open (unit=226,file="Output/distinit_UN.txt",action="write",status="replace")
        write (226,*) distinit_UN
        close(226)

        open (unit=226,file="Output/distinit_NU.txt",action="write",status="replace")
        write (226,*) distinit_NU
        close(226)

        open (unit=226,file="Output/distinit_SN.txt",action="write",status="replace")
        write (226,*) distinit_SN
        close(226)

        open (unit=226,file="Output/distinit_NS.txt",action="write",status="replace")
        write (226,*) distinit_NS
        close(226)

        open (unit=226,file="Output/distinit_NN.txt",action="write",status="replace")
        write (226,*) distinit_NN
        close(226)


    end subroutine
    
    subroutine add2dist(distinit,college,indabove,indbelow,wabove,wbelow,h,hsp,dist_h,mass_a,dist_a_below,dist_a_above,indInit_a_below,indInit_a_above,AA,HH)

        integer, intent(in):: indabove,indbelow,AA,HH,h,hsp,indInit_a_below,indInit_a_above
        real*8, intent(in):: wabove,wbelow
        real*8, intent(in):: college,dist_h(3),dist_a_above(indInit_a_above),dist_a_below(indInit_a_below), mass_a
        real*8, intent(inout):: distinit(AA,HH,HH)
        integer :: iter_i,iter_l,iter_t,overlap,index
        real*8 :: mass_overlap

        ! loops over h grid points around median
        do iter_i = 1,3
            do iter_l = 1,3

                ! actual asset grid points
                distinit(indbelow,h+iter_i-2,hsp+iter_l-2) = distinit(indbelow,h+iter_i-2,hsp+iter_l-2) + mass_a*dist_h(iter_i)*dist_h(iter_l)*wbelow*college
                distinit(indabove,h+iter_i-2,hsp+iter_l-2) = distinit(indabove,h+iter_i-2,hsp+iter_l-2) + mass_a*dist_h(iter_i)*dist_h(iter_l)*wabove*college

                ! allocate assets below
                overlap = indbelow-1-indInit_a_below
                if (overlap<0) then
                    index = (indInit_a_below+overlap+1)
                    mass_overlap=SUM(dist_a_below(index:indInit_a_below))
                    distinit(1,h+iter_i-2,hsp+iter_l-2) = distinit(1,h+iter_i-2,hsp+iter_l-2) + mass_overlap*dist_h(iter_i)*dist_h(iter_l)*wbelow*college
                    distinit(2,h+iter_i-2,hsp+iter_l-2) = distinit(2,h+iter_i-2,hsp+iter_l-2) + mass_overlap*dist_h(iter_i)*dist_h(iter_l)*wabove*college
                
                    do iter_t=1,(indInit_a_below+overlap)
                        distinit(indbelow-iter_t,h+iter_i-2,hsp+iter_l-2) = distinit(indbelow-iter_t,h+iter_i-2,hsp+iter_l-2) + dist_a_below(iter_t)*dist_h(iter_i)*dist_h(iter_l)*wbelow*college
                        distinit(indabove-iter_t,h+iter_i-2,hsp+iter_l-2) = distinit(indabove-iter_t,h+iter_i-2,hsp+iter_l-2) + dist_a_below(iter_t)*dist_h(iter_i)*dist_h(iter_l)*wabove*college
                    end do
                else
                    do iter_t=1,indInit_a_below
                        distinit(indbelow-iter_t,h+iter_i-2,hsp+iter_l-2) = distinit(indbelow-iter_t,h+iter_i-2,hsp+iter_l-2) + dist_a_below(iter_t)*dist_h(iter_i)*dist_h(iter_l)*wbelow*college
                        distinit(indabove-iter_t,h+iter_i-2,hsp+iter_l-2) = distinit(indabove-iter_t,h+iter_i-2,hsp+iter_l-2) + dist_a_below(iter_t)*dist_h(iter_i)*dist_h(iter_l)*wabove*college
                    end do
                end if

                ! allocate assets above
                overlap = (AA-indabove)-indInit_a_above
                if (overlap<0) then
                    index = (indInit_a_above+overlap+1)
                    mass_overlap=SUM(dist_a_above(index:indInit_a_above))
                    distinit(AA-1,h+iter_i-2,hsp+iter_l-2) = distinit(AA-1,h+iter_i-2,hsp+iter_l-2) + mass_overlap*dist_h(iter_i)*dist_h(iter_l)*wbelow*college
                    distinit(AA,h+iter_i-2,hsp+iter_l-2) = distinit(AA,h+iter_i-2,hsp+iter_l-2) + mass_overlap*dist_h(iter_i)*dist_h(iter_l)*wabove*college
                
                    do iter_t=1,(indInit_a_above+overlap)
                        distinit(indbelow+iter_t,h+iter_i-2,hsp+iter_l-2) = distinit(indbelow+iter_t,h+iter_i-2,hsp+iter_l-2) + dist_a_above(iter_t)*dist_h(iter_i)*dist_h(iter_l)*wbelow*college
                        distinit(indabove+iter_t,h+iter_i-2,hsp+iter_l-2) = distinit(indabove+iter_t,h+iter_i-2,hsp+iter_l-2) + dist_a_above(iter_t)*dist_h(iter_i)*dist_h(iter_l)*wabove*college
                    end do
                else
                    do iter_t=1,indInit_a_above
                        distinit(indbelow+iter_t,h+iter_i-2,hsp+iter_l-2) = distinit(indbelow+iter_t,h+iter_i-2,hsp+iter_l-2) + dist_a_above(iter_t)*dist_h(iter_i)*dist_h(iter_l)*wbelow*college
                        distinit(indabove+iter_t,h+iter_i-2,hsp+iter_l-2) = distinit(indabove+iter_t,h+iter_i-2,hsp+iter_l-2) + dist_a_above(iter_t)*dist_h(iter_i)*dist_h(iter_l)*wabove*college
                    end do
                end if

            end do
        end do

    end subroutine

end module

