!#######################################################################
! Module Household Problem
! Solves the household problem
!#######################################################################
    
include "Store_Results.f90"

module Household_Problem

    implicit none
    
contains

    subroutine solve_model()
    
        use Globals
        use Interpolation_Routines
        use Store_Results
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    
        ! Local variable declarations
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        
        real*8, dimension(AA) :: astate, cpol_endo, VF_endo ! aux variables for endogenous grids
        real*8, dimension(AA,HH,HH) :: cpol_endoR

        integer, dimension(AA) :: aind_non
        
        integer :: AA_red
        
        real*8, allocatable :: astate_red(:), apgrid_red(:)
        
        real*8, dimension(AA) :: EVFp, EVFp_interp  ! aux store for future VF (and interp to asset policy)
        
        real*8, dimension(AA,HH,HH) :: sumVF_aux, corr! aux store used for comp of choice PBs
                        
        real*8, allocatable :: VFstack9(:,:,:,:)! aux store used for comp of choice PBs
        
        real*8, allocatable :: VFstack6(:,:,:,:)! aux store used for comp of choice PBs
        
        real*8, allocatable :: VFstack4(:,:,:,:)! aux store used for comp of choice PBs

        integer :: hhh ! looping over human capital
        integer :: h, hsp, a
        
        integer, dimension(2,HH*HH) :: jointhhh ! joint index

        !testing consistency of pi matrices
        logical :: test1, test2, test3, test4, test5, test6, test7, test8, test9, test10
        logical :: test11, test12, test13, test14, test15, test16, testpi

        real*8, dimension(HH,HH,AA) :: EJ_EE_EE_XX, EJ_EE_EX_XB
        real*8, dimension(HH,HH,AA) :: EJ_EX_EE_XB, EJ_EX_EE_XX, EJ_EX_EX_XB, EJ_EX_EX_XX
        real*8, dimension(HH,HH,AA) :: EJ_XE_EE_BX, EJ_XE_EE_XX, EJ_XE_EX_BB, EJ_XE_EX_XB
        real*8, dimension(HH,HH,AA) :: EJ_XX_EE_XX, EJ_XX_EE_BX, EJ_XX_EE_XB, EJ_XX_EE_BB, EJ_XX_EX_XX, EJ_XX_EX_BX, EJ_XX_EX_XB, EJ_XX_EX_BB

        real*8, dimension(HH,HH) :: EJ_help_mat

        real*8 :: theta1_help, theta2_help, q_help, EJ_EE_1,EJ_EX_1,EJ_EE_2,EJ_EX_2

        real*8, dimension(HH,HH) :: htrans ! PBs of future HC states
        
        real*8, dimension(AA) :: EJ_asset
        real*8 :: EJ_help

        ! allocate variables
        allocate (VFstack9(AA,HH,HH,9))
        allocate (VFstack6(AA,HH,HH,6))
        allocate (VFstack4(AA,HH,HH,4))

        ! creating joint loop index
        do h=1,HH   ! HC loop spouse
        
            do hsp=1,HH     ! HC loop head
                
                jointhhh(1,HH*(h-1)+hsp) = h
                jointhhh(2,HH*(h-1)+hsp) = hsp
                
            end do
        
        end do
        
        !print *, jointhhh(1,:)       
        !print *, jointhhh(2,:)
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! retirement
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ! final period of life
        do a=1,AA
            do h=1,HH
              do hsp=1,HH
                cpol_R(a,h,hsp,TD-TT) = pen(h,hsp) + (1d0 + r) * assgrid(a) ! consumption policy retired
                VF_R(a,h,hsp,TD-TT) = util(cpol_R(a,h,hsp,TD-TT))
              enddo
            enddo
        enddo

        ! iterate backwards over retirement life
        do t=1,TD-TT-1  
            do h=1,HH
                do hsp=1,HH

                    !backout endogenous asset grid
                    do a=1,AA
                        astate(a) = (((1d0+r)*beta*utilp(cpol_R(a,h,hsp,TD-TT-t+1)))**(-1d0/gam) + assgrid(a) - pen(h,hsp))/(1d0+r)
                    enddo

                    ! interpolate to original grid
                    if (minval(astate(2:AA)-astate(1:AA-1)) < 0d0) then
                        print *, "astate not monotonically increasing"
                        stop
                    endif
                    call LinInterp_Extrap(AA,astate,assgrid,AA,assgrid,apol_R(:,h,hsp,TD-TT-t))
                    where (apol_R(:,h,hsp,TD-TT-t) < assgrid(1)) apol_R(:,h,hsp,TD-TT-t) = assgrid(1) ! adjusting for borrowing constraint
                    
                    ! consumption and value function given asset choice
                    cpol_R(:,h,hsp,TD-TT-t) = pen(h,hsp) + (1d0 + r) * assgrid - apol_R(:,h,hsp,TD-TT-t)                                
                    call LinInterp_Extrap(AA,assgrid,VF_R(:,h,hsp,TD-TT-t+1),AA,apol_R(:,h,hsp,TD-TT-t),EVFp_interp)
                    VF_R(:,h,hsp,TD-TT-t) = util_vec(cpol_R(:,h,hsp,TD-TT-t)) + beta*EVFp_interp
                        
                enddo
            enddo
        enddo

        ! compute marginal benefit of assets upon retirement
        do h=1,HH
            do hsp=1,HH
              dVF_R(:,h,hsp) = (1d0+r)*utilp_vec(cpol_R(:,h,hsp,1)) ! marginal utility in first period of retirement
            enddo
        enddo


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
        ! period before retirement
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
        cpol_endoR = (beta*dVF_R)**(-1d0/gam) ! TT cons from Euler

        
        do hsp=1,HH   ! HC loop spouse
        
            do h=1,HH     ! HC loop head
                        
                !!!!!!!!!!!
                ! EE
                !!!!!!!!!!!
                do a=1,AA ! asset loop -- asset choice on exogenous grid
                    
                    astate(a) = (cpol_endoR(a,h,hsp) + assgrid(a) - (1d0-tau_ss)*wage(h) - &
                                                    (1d0-tau_ss)*wage(hsp)) / (1d0+r)

                end do  ! end of asset loop
                
                if (minval(astate(2:AA)-astate(1:AA-1)) < 0d0) then
                    print *, "astate not monotonically increasing"
                    stop
                endif
                call LinInterp_Extrap(AA,astate,assgrid,AA,assgrid,apol_EE(:,h,hsp,TT))
                where (apol_EE(:,h,hsp,TT) < assgrid(1)) apol_EE(:,h,hsp,TT) = assgrid(1) ! adjusting for borrowing constraint
                cpol_EE(:,h,hsp,TT) = (1d0-tau_ss)*wage(h) + (1d0-tau_ss)*wage(hsp) &
                                                + (1d0 + r) * assgrid - apol_EE(:,h,hsp,TT)
                                                
                call LinInterp_Extrap(AA,assgrid,VF_R(:,h,hsp,1),AA,apol_EE(:,h,hsp,TT),EVFp_interp)
                VF_EE(:,h,hsp,TT) = util_vec(cpol_EE(:,h,hsp,TT)) + psi_EE(TT) + beta*EVFp_interp
                    

                !!!!!!!!
                ! UE
                !!!!!!!!
                do a=1,AA ! asset loop
                    
                    astate(a) = (cpol_endoR(a,h,hsp) + assgrid(a) - (1d0-tau_ss)*ben(h) - &
                                                        (1d0-tau_ss)*wage(hsp)) / (1d0+r)
                
                end do  ! end of asset loop
                
                if (minval(astate(2:AA)-astate(1:AA-1)) < 0d0) then
                    print *, "astate not monotonically increasing"
                    stop
                endif
                call LinInterp_Extrap(AA,astate,assgrid,AA,assgrid,apol_UE(:,h,hsp,TT))
                where (apol_UE(:,h,hsp,TT) < assgrid(1)) apol_UE(:,h,hsp,TT) = assgrid(1)
                cpol_UE(:,h,hsp,TT) = (1d0-tau_ss)*ben(h) + (1d0-tau_ss)*wage(hsp) &
                                                    + (1d0 + r) * assgrid - apol_UE(:,h,hsp,TT)
                
                call LinInterp_Extrap(AA,assgrid,VF_R(:,h,hsp,1),AA,apol_UE(:,h,hsp,TT),EVFp_interp)
                    VF_UE(:,h,hsp,TT) = util_vec(cpol_UE(:,h,hsp,TT)) + psi_UE(TT) + beta*EVFp_interp
                    
                    
                
                !!!!!!!!
                ! NE
                !!!!!!!! 
                do a=1,AA ! asset loop
                
                    astate(a) = (cpol_endoR(a,h,hsp) + assgrid(a) - &
                                                        (1d0-tau_ss)*wage(hsp)) / (1d0+r)
                
                end do  ! end of asset loop
                if (minval(astate(2:AA)-astate(1:AA-1)) < 0d0) then
                    print *, "astate not monotonically increasing"
                    stop
                endif

                call LinInterp_Extrap(AA,astate,assgrid,AA,assgrid,apol_NE(:,h,hsp,TT))
                where (apol_NE(:,h,hsp,TT) < assgrid(1)) apol_NE(:,h,hsp,TT) = assgrid(1)
                cpol_NE(:,h,hsp,TT) = (1d0-tau_ss)*wage(hsp) &
                                                    + (1d0 + r) * assgrid - apol_NE(:,h,hsp,TT)
                
                call LinInterp_Extrap(AA,assgrid,VF_R(:,h,hsp,1),AA,apol_NE(:,h,hsp,TT),EVFp_interp)
                    VF_NE(:,h,hsp,TT) = util_vec(cpol_NE(:,h,hsp,TT)) + psi_NE(TT) + beta*EVFp_interp
            

                        
                !!!!!!!!
                ! UU
                !!!!!!!! 
                do a=1,AA ! asset loop
                
                    astate(a) = (cpol_endoR(a,h,hsp) + assgrid(a) - (1d0-tau_ss)*ben(h) - &
                                                            (1d0-tau_ss)*ben(hsp)) / (1d0+r)
                    
                end do  ! end of asset loop
                
                if (minval(astate(2:AA)-astate(1:AA-1)) < 0d0) then
                        print *, "astate not monotonically increasing"
                        stop
                endif

                call LinInterp_Extrap(AA,astate,assgrid,AA,assgrid,apol_UU(:,h,hsp,TT))
                where (apol_UU(:,h,hsp,TT) < assgrid(1)) apol_UU(:,h,hsp,TT) = assgrid(1)
                cpol_UU(:,h,hsp,TT) = (1d0-tau_ss)*ben(h) + (1d0-tau_ss)*ben(hsp) &
                                                     + (1d0 + r) * assgrid - apol_UU(:,h,hsp,TT)
                
                call LinInterp_Extrap(AA,assgrid,VF_R(:,h,hsp,1),AA,apol_UU(:,h,hsp,TT),EVFp_interp)
                        VF_UU(:,h,hsp,TT) = util_vec(cpol_UU(:,h,hsp,TT)) + psi_UU(TT) + beta*EVFp_interp
                 
                !!!!!!!!
                ! UN
                !!!!!!!!  
                do a=1,AA ! asset loop
                
                    astate(a) = (cpol_endoR(a,h,hsp) + assgrid(a) - &
                                                           (1d0-tau_ss)*ben(h) ) / (1d0+r)
                                                                               
                end do  ! end of asset loop 
                if (minval(astate(2:AA)-astate(1:AA-1)) < 0d0) then
                        print *, "astate not monotonically increasing"
                        stop
                endif

                call LinInterp_Extrap(AA,astate,assgrid,AA,assgrid,apol_UN(:,h,hsp,TT))
                where (apol_UN(:,h,hsp,TT) < assgrid(1)) apol_UN(:,h,hsp,TT) = assgrid(1)
                cpol_UN(:,h,hsp,TT) = (1d0-tau_ss)*ben(h) &
                                                     + (1d0 + r) * assgrid - apol_UN(:,h,hsp,TT)
                
                call LinInterp_Extrap(AA,assgrid,VF_R(:,h,hsp,1),AA,apol_UN(:,h,hsp,TT),EVFp_interp)
                        VF_UN(:,h,hsp,TT) = util_vec(cpol_UN(:,h,hsp,TT)) + psi_UN(TT) + beta*EVFp_interp
                
                !!!!!!!!
                ! NN
                !!!!!!!!  
                do a=1,AA ! asset loop
                
                    astate(a) = (cpol_endoR(a,h,hsp) + assgrid(a)) / (1d0+r)
                    
                end do  ! end of asset loop
                if (minval(astate(2:AA)-astate(1:AA-1)) < 0d0) then
                        print *, "astate not monotonically increasing"
                        stop
                endif

                call LinInterp_Extrap(AA,astate,assgrid,AA,assgrid,apol_NN(:,h,hsp,TT))
                where (apol_NN(:,h,hsp,TT) < assgrid(1)) apol_NN(:,h,hsp,TT) = assgrid(1)
                cpol_NN(:,h,hsp,TT) = (1d0 + r) * assgrid - apol_NN(:,h,hsp,TT)
                where (cpol_NN(:,h,hsp,TT)<=1d-2) cpol_NN(:,h,hsp,TT) = 1d-2
                
                call LinInterp_Extrap(AA,assgrid,VF_R(:,h,hsp,1),AA,apol_NN(:,h,hsp,TT),EVFp_interp)
                        VF_NN(:,h,hsp,TT) = util_vec(cpol_NN(:,h,hsp,TT)) + psi_NN(TT) + beta*EVFp_interp
            
            end do    ! end of HC head loop
        
        end do    ! end of HC spouse loop
    
       ! mirror policy functions for NU & EN & EU
       ! (mirroring policies only works if no systematic difference benefits / wages) 
       do hsp=1,HH   ! HC loop spouse
        
            do h=1,HH     ! HC loop head 
            
            cpol_NU(:,h,hsp,TT) = cpol_UN(:,hsp,h,TT)
            cpol_EN(:,h,hsp,TT) = cpol_NE(:,hsp,h,TT)
            cpol_EU(:,h,hsp,TT) = cpol_UE(:,hsp,h,TT)
            
            VF_NU(:,h,hsp,TT) = VF_UN(:,hsp,h,TT)
            VF_EN(:,h,hsp,TT) = VF_NE(:,hsp,h,TT)
            VF_EU(:,h,hsp,TT) = VF_UE(:,hsp,h,TT)
            
            end do    ! end of HC head loop
        
        end do    ! end of HC spouse loop
    
        ! exploit that the problem is identical for S and N in last period except for psis
        VF_SS(:,:,:,TT)=VF_NN(:,:,:,TT)+psi_SS(TT)-psi_NN(TT)
        VF_SN(:,:,:,TT)=VF_NN(:,:,:,TT)+psi_SN(TT)-psi_NN(TT)
        VF_NS(:,:,:,TT)=VF_NN(:,:,:,TT)+psi_NS(TT)-psi_NN(TT)
        VF_SU(:,:,:,TT)=VF_NU(:,:,:,TT)+psi_SU(TT)-psi_NU(TT)
        VF_US(:,:,:,TT)=VF_UN(:,:,:,TT)+psi_US(TT)-psi_UN(TT)
        VF_SE(:,:,:,TT)=VF_NE(:,:,:,TT)+psi_SE(TT)-psi_NE(TT)
        VF_ES(:,:,:,TT)=VF_EN(:,:,:,TT)+psi_ES(TT)-psi_EN(TT)

        cpol_SS(:,:,:,TT)=cpol_NN(:,:,:,TT)
        cpol_SN(:,:,:,TT)=cpol_NN(:,:,:,TT)
        cpol_NS(:,:,:,TT)=cpol_NN(:,:,:,TT)
        cpol_SU(:,:,:,TT)=cpol_NU(:,:,:,TT)
        cpol_US(:,:,:,TT)=cpol_UN(:,:,:,TT)
        cpol_SE(:,:,:,TT)=cpol_NE(:,:,:,TT)
        cpol_ES(:,:,:,TT)=cpol_EN(:,:,:,TT)

        apol_SS(:,:,:,TT)=apol_NN(:,:,:,TT)
        apol_SN(:,:,:,TT)=apol_NN(:,:,:,TT)
        apol_NS(:,:,:,TT)=apol_NN(:,:,:,TT)
        apol_SU(:,:,:,TT)=apol_NU(:,:,:,TT)
        apol_US(:,:,:,TT)=apol_UN(:,:,:,TT)
        apol_SE(:,:,:,TT)=apol_NE(:,:,:,TT)
        apol_ES(:,:,:,TT)=apol_EN(:,:,:,TT)

        ! set final job finding rates
        lambda_UE_E(:,:,:,TT) = 0d0
        lambda_UE_X(:,:,:,TT) = 0d0
        lambda_UU(:,:,:,TT) = 0d0
        lambda_UN(:,:,:,TT) = 0d0
        lambda_NU(:,:,:,TT) = 0d0
        lambda_NN(:,:,:,TT) = 0d0
        lambda_SS(:,:,:,TT) = 0d0
        lambda_SE_E(:,:,:,TT) = 0d0
        lambda_SE_X(:,:,:,TT) = 0d0
        lambda_SN(:,:,:,TT) = 0d0
        lambda_NS(:,:,:,TT) = 0d0
        lambda_SU(:,:,:,TT) = 0d0
        lambda_US(:,:,:,TT) = 0d0
        lambda_NE_E(:,:,:,TT) = 0d0
        lambda_NE_X(:,:,:,TT) = 0d0

        ! compute final firm values
            do h=1,HH
                J_EE(:,h,:,TT)  = prof(h)
                J_EU(:,h,:,TT)  = prof(h)
                J_EN(:,h,:,TT)  = prof(h)
                J_ES(:,h,:,TT)  = prof(h)
            enddo

    print *, 'Solved period:', TT

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! all remaining periods
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
        do t=TT-1,1,-1 ! age loop 
        
            
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !!! computing choice probabilities
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


            ! both members eligible for benefits
            ! preparing correction to avoid floating point exception
            VFstack9(:,:,:,1) = VF_EE(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,2) = VF_EU(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,3) = VF_UE(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,4) = VF_EN(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,5) = VF_NE(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,6) = VF_UU(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,7) = VF_UN(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,8) = VF_NU(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,9) = VF_NN(:,:,:,t+1)/sigma_eps
            corr=MAXVAL(VFstack9,4)

            ! choice PBs if both have a job (offer)
            sumVF_aux= exp(VF_EE(:,:,:,t+1)/sigma_eps-corr) + exp(VF_EU(:,:,:,t+1)/sigma_eps-corr) + exp(VF_UE(:,:,:,t+1)/sigma_eps-corr) &
                     + exp(VF_EN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NE(:,:,:,t+1)/sigma_eps-corr) + exp(VF_UU(:,:,:,t+1)/sigma_eps-corr) &
                     + exp(VF_UN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NU(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NN(:,:,:,t+1)/sigma_eps-corr)

            pi_EE_BB_EE(:,:,:,t+1) = exp(VF_EE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_BB_EU(:,:,:,t+1) = exp(VF_EU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_BB_UE(:,:,:,t+1) = exp(VF_UE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_BB_EN(:,:,:,t+1) = exp(VF_EN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_BB_NE(:,:,:,t+1) = exp(VF_NE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_BB_UU(:,:,:,t+1) = exp(VF_UU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_BB_UN(:,:,:,t+1) = exp(VF_UN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_BB_NU(:,:,:,t+1) = exp(VF_NU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_BB_NN(:,:,:,t+1) = exp(VF_NN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux

            ! only member 1 eligible for benefits
            ! preparing correction to avoid floating point exception
            VFstack9(:,:,:,1) = VF_EE(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,2) = VF_ES(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,3) = VF_UE(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,4) = VF_EN(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,5) = VF_NE(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,6) = VF_US(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,7) = VF_UN(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,8) = VF_NS(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,9) = VF_NN(:,:,:,t+1)/sigma_eps
            corr=MAXVAL(VFstack9,4)

            ! choice PBs if both have a job (offer)
            sumVF_aux= exp(VF_EE(:,:,:,t+1)/sigma_eps-corr) + exp(VF_ES(:,:,:,t+1)/sigma_eps-corr) + exp(VF_UE(:,:,:,t+1)/sigma_eps-corr) &
                     + exp(VF_EN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NE(:,:,:,t+1)/sigma_eps-corr) + exp(VF_US(:,:,:,t+1)/sigma_eps-corr) &
                     + exp(VF_UN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NS(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NN(:,:,:,t+1)/sigma_eps-corr)
            
            pi_EE_BX_EE(:,:,:,t+1) = exp(VF_EE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_BX_ES(:,:,:,t+1) = exp(VF_ES(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_BX_UE(:,:,:,t+1) = exp(VF_UE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_BX_EN(:,:,:,t+1) = exp(VF_EN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_BX_NE(:,:,:,t+1) = exp(VF_NE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_BX_US(:,:,:,t+1) = exp(VF_US(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_BX_UN(:,:,:,t+1) = exp(VF_UN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_BX_NS(:,:,:,t+1) = exp(VF_NS(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_BX_NN(:,:,:,t+1) = exp(VF_NN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux

            ! only member 2 eligible for benefits
            ! preparing correction to avoid floating point exception
            VFstack9(:,:,:,1) = VF_EE(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,2) = VF_EU(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,3) = VF_SE(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,4) = VF_EN(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,5) = VF_NE(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,6) = VF_SU(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,7) = VF_SN(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,8) = VF_NU(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,9) = VF_NN(:,:,:,t+1)/sigma_eps
            corr=MAXVAL(VFstack9,4)

            ! choice PBs if both have a job (offer)
            sumVF_aux= exp(VF_EE(:,:,:,t+1)/sigma_eps-corr) + exp(VF_EU(:,:,:,t+1)/sigma_eps-corr) + exp(VF_SE(:,:,:,t+1)/sigma_eps-corr) &
                     + exp(VF_EN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NE(:,:,:,t+1)/sigma_eps-corr) + exp(VF_SU(:,:,:,t+1)/sigma_eps-corr) &
                     + exp(VF_SN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NU(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NN(:,:,:,t+1)/sigma_eps-corr)
            
            pi_EE_XB_EE(:,:,:,t+1) = exp(VF_EE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_XB_EU(:,:,:,t+1) = exp(VF_EU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_XB_SE(:,:,:,t+1) = exp(VF_SE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_XB_EN(:,:,:,t+1) = exp(VF_EN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_XB_NE(:,:,:,t+1) = exp(VF_NE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_XB_SU(:,:,:,t+1) = exp(VF_SU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_XB_SN(:,:,:,t+1) = exp(VF_SN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_XB_NU(:,:,:,t+1) = exp(VF_NU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_XB_NN(:,:,:,t+1) = exp(VF_NN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux

            ! no member eligible for benefits
            ! preparing correction to avoid floating point exception
            VFstack9(:,:,:,1) = VF_EE(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,2) = VF_ES(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,3) = VF_SE(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,4) = VF_EN(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,5) = VF_NE(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,6) = VF_SS(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,7) = VF_SN(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,8) = VF_NS(:,:,:,t+1)/sigma_eps
            VFstack9(:,:,:,9) = VF_NN(:,:,:,t+1)/sigma_eps
            corr=MAXVAL(VFstack9,4)

            ! choice PBs if both have a job (offer)
            sumVF_aux= exp(VF_EE(:,:,:,t+1)/sigma_eps-corr) + exp(VF_ES(:,:,:,t+1)/sigma_eps-corr) + exp(VF_SE(:,:,:,t+1)/sigma_eps-corr) &
                     + exp(VF_EN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NE(:,:,:,t+1)/sigma_eps-corr) + exp(VF_SS(:,:,:,t+1)/sigma_eps-corr) &
                     + exp(VF_SN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NS(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NN(:,:,:,t+1)/sigma_eps-corr)
            
            pi_EE_XX_EE(:,:,:,t+1) = exp(VF_EE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_XX_ES(:,:,:,t+1) = exp(VF_ES(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_XX_SE(:,:,:,t+1) = exp(VF_SE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_XX_EN(:,:,:,t+1) = exp(VF_EN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_XX_NE(:,:,:,t+1) = exp(VF_NE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_XX_SS(:,:,:,t+1) = exp(VF_SS(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_XX_SN(:,:,:,t+1) = exp(VF_SN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_XX_NS(:,:,:,t+1) = exp(VF_NS(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EE_XX_NN(:,:,:,t+1) = exp(VF_NN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
                            
            
            ! both members eligible for benefits
            ! preparing correction to avoid floating point exception
            VFstack6(:,:,:,1) = VF_EU(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,2) = VF_EN(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,3) = VF_UU(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,4) = VF_UN(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,5) = VF_NU(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,6) = VF_NN(:,:,:,t+1)/sigma_eps
            corr=MAXVAL(VFstack6,4)

            ! choice PBs if only head has a job (offer)
            sumVF_aux= exp(VF_EU(:,:,:,t+1)/sigma_eps-corr) + exp(VF_EN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_UU(:,:,:,t+1)/sigma_eps-corr) &
                     + exp(VF_UN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NU(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NN(:,:,:,t+1)/sigma_eps-corr)
            pi_EX_BB_EU(:,:,:,t+1) = exp(VF_EU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_BB_EN(:,:,:,t+1) = exp(VF_EN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_BB_UU(:,:,:,t+1) = exp(VF_UU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_BB_UN(:,:,:,t+1) = exp(VF_UN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_BB_NU(:,:,:,t+1) = exp(VF_NU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_BB_NN(:,:,:,t+1) = exp(VF_NN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux


            ! only member 1 eligible for benefits
            ! preparing correction to avoid floating point exception
            VFstack6(:,:,:,1) = VF_ES(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,2) = VF_EN(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,3) = VF_US(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,4) = VF_UN(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,5) = VF_NS(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,6) = VF_NN(:,:,:,t+1)/sigma_eps
            corr=MAXVAL(VFstack6,4)

            ! choice PBs if only head has a job (offer)
            sumVF_aux= exp(VF_ES(:,:,:,t+1)/sigma_eps-corr) + exp(VF_EN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_US(:,:,:,t+1)/sigma_eps-corr) &
                     + exp(VF_UN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NS(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NN(:,:,:,t+1)/sigma_eps-corr)
            pi_EX_BX_ES(:,:,:,t+1) = exp(VF_ES(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_BX_EN(:,:,:,t+1) = exp(VF_EN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_BX_US(:,:,:,t+1) = exp(VF_US(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_BX_UN(:,:,:,t+1) = exp(VF_UN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_BX_NS(:,:,:,t+1) = exp(VF_NS(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_BX_NN(:,:,:,t+1) = exp(VF_NN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux


            ! only member 2 eligible for benefits
            ! preparing correction to avoid floating point exception
            VFstack6(:,:,:,1) = VF_EU(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,2) = VF_EN(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,3) = VF_SU(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,4) = VF_SN(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,5) = VF_NU(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,6) = VF_NN(:,:,:,t+1)/sigma_eps
            corr=MAXVAL(VFstack6,4)

            ! choice PBs if only head has a job (offer)
            sumVF_aux= exp(VF_EU(:,:,:,t+1)/sigma_eps-corr) + exp(VF_EN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_SU(:,:,:,t+1)/sigma_eps-corr) &
                     + exp(VF_SN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NU(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NN(:,:,:,t+1)/sigma_eps-corr)
            pi_EX_XB_EU(:,:,:,t+1) = exp(VF_EU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_XB_EN(:,:,:,t+1) = exp(VF_EN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_XB_SU(:,:,:,t+1) = exp(VF_SU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_XB_SN(:,:,:,t+1) = exp(VF_SN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_XB_NU(:,:,:,t+1) = exp(VF_NU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_XB_NN(:,:,:,t+1) = exp(VF_NN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux


            ! no member eligible for benefits
            ! preparing correction to avoid floating point exception
            VFstack6(:,:,:,1) = VF_ES(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,2) = VF_EN(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,3) = VF_SS(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,4) = VF_SN(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,5) = VF_NS(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,6) = VF_NN(:,:,:,t+1)/sigma_eps
            corr=MAXVAL(VFstack6,4)

            ! choice PBs if only head has a job (offer)
            sumVF_aux= exp(VF_ES(:,:,:,t+1)/sigma_eps-corr) + exp(VF_EN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_SS(:,:,:,t+1)/sigma_eps-corr) &
                     + exp(VF_SN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NS(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NN(:,:,:,t+1)/sigma_eps-corr)
            pi_EX_XX_ES(:,:,:,t+1) = exp(VF_ES(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_XX_EN(:,:,:,t+1) = exp(VF_EN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_XX_SS(:,:,:,t+1) = exp(VF_SS(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_XX_SN(:,:,:,t+1) = exp(VF_SN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_XX_NS(:,:,:,t+1) = exp(VF_NS(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_EX_XX_NN(:,:,:,t+1) = exp(VF_NN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux


            ! both members eligible for benefits
            ! preparing correction to avoid floating point exception
            VFstack6(:,:,:,1) = VF_UE(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,2) = VF_NE(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,3) = VF_UU(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,4) = VF_UN(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,5) = VF_NU(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,6) = VF_NN(:,:,:,t+1)/sigma_eps
            corr=MAXVAL(VFstack6,4)

            ! choice PBs if only spouse has a job (offer)
            sumVF_aux=  exp(VF_UE(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NE(:,:,:,t+1)/sigma_eps-corr) + exp(VF_UU(:,:,:,t+1)/sigma_eps-corr) &
                      + exp(VF_UN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NU(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NN(:,:,:,t+1)/sigma_eps-corr)
            pi_XE_BB_UE(:,:,:,t+1) = exp(VF_UE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_BB_NE(:,:,:,t+1) = exp(VF_NE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_BB_UU(:,:,:,t+1) = exp(VF_UU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_BB_UN(:,:,:,t+1) = exp(VF_UN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_BB_NU(:,:,:,t+1) = exp(VF_NU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_BB_NN(:,:,:,t+1) = exp(VF_NN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux


            ! only member 1 eligible for benefits
            ! preparing correction to avoid floating point exception
            VFstack6(:,:,:,1) = VF_UE(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,2) = VF_NE(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,3) = VF_US(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,4) = VF_UN(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,5) = VF_NS(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,6) = VF_NN(:,:,:,t+1)/sigma_eps
            corr=MAXVAL(VFstack6,4)

            ! choice PBs if only spouse has a job (offer)
            sumVF_aux=  exp(VF_UE(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NE(:,:,:,t+1)/sigma_eps-corr) + exp(VF_US(:,:,:,t+1)/sigma_eps-corr) &
                      + exp(VF_UN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NS(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NN(:,:,:,t+1)/sigma_eps-corr)
            pi_XE_BX_UE(:,:,:,t+1) = exp(VF_UE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_BX_NE(:,:,:,t+1) = exp(VF_NE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_BX_US(:,:,:,t+1) = exp(VF_US(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_BX_UN(:,:,:,t+1) = exp(VF_UN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_BX_NS(:,:,:,t+1) = exp(VF_NS(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_BX_NN(:,:,:,t+1) = exp(VF_NN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux


            ! only member 2 eligible for benefits
            ! preparing correction to avoid floating point exception
            VFstack6(:,:,:,1) = VF_SE(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,2) = VF_NE(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,3) = VF_SU(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,4) = VF_SN(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,5) = VF_NU(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,6) = VF_NN(:,:,:,t+1)/sigma_eps
            corr=MAXVAL(VFstack6,4)

            ! choice PBs if only spouse has a job (offer)
            sumVF_aux=  exp(VF_SE(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NE(:,:,:,t+1)/sigma_eps-corr) + exp(VF_SU(:,:,:,t+1)/sigma_eps-corr) &
                      + exp(VF_SN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NU(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NN(:,:,:,t+1)/sigma_eps-corr)
            pi_XE_XB_SE(:,:,:,t+1) = exp(VF_SE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_XB_NE(:,:,:,t+1) = exp(VF_NE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_XB_SU(:,:,:,t+1) = exp(VF_SU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_XB_SN(:,:,:,t+1) = exp(VF_SN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_XB_NU(:,:,:,t+1) = exp(VF_NU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_XB_NN(:,:,:,t+1) = exp(VF_NN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux


            ! no member eligible for benefits
            ! preparing correction to avoid floating point exception
            VFstack6(:,:,:,1) = VF_SE(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,2) = VF_NE(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,3) = VF_SS(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,4) = VF_SN(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,5) = VF_NS(:,:,:,t+1)/sigma_eps
            VFstack6(:,:,:,6) = VF_NN(:,:,:,t+1)/sigma_eps
            corr=MAXVAL(VFstack6,4)

            ! choice PBs if only spouse has a job (offer)
            sumVF_aux=  exp(VF_SE(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NE(:,:,:,t+1)/sigma_eps-corr) + exp(VF_SS(:,:,:,t+1)/sigma_eps-corr) &
                      + exp(VF_SN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NS(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NN(:,:,:,t+1)/sigma_eps-corr)
            pi_XE_XX_SE(:,:,:,t+1) = exp(VF_SE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_XX_NE(:,:,:,t+1) = exp(VF_NE(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_XX_SS(:,:,:,t+1) = exp(VF_SS(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_XX_SN(:,:,:,t+1) = exp(VF_SN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_XX_NS(:,:,:,t+1) = exp(VF_NS(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XE_XX_NN(:,:,:,t+1) = exp(VF_NN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
        

            ! both members eligible for benefits
            ! preparing correction to avoid floating point exception
            VFstack4(:,:,:,1) = VF_UU(:,:,:,t+1)/sigma_eps
            VFstack4(:,:,:,2) = VF_UN(:,:,:,t+1)/sigma_eps
            VFstack4(:,:,:,3) = VF_NU(:,:,:,t+1)/sigma_eps
            VFstack4(:,:,:,4) = VF_NN(:,:,:,t+1)/sigma_eps
            corr=MAXVAL(VFstack4,4)

            ! choice PBs if none has a job (offer)
            sumVF_aux=  exp(VF_UU(:,:,:,t+1)/sigma_eps-corr) + exp(VF_UN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NU(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NN(:,:,:,t+1)/sigma_eps-corr)
            pi_XX_BB_UU(:,:,:,t+1) = exp(VF_UU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XX_BB_UN(:,:,:,t+1) = exp(VF_UN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XX_BB_NU(:,:,:,t+1) = exp(VF_NU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XX_BB_NN(:,:,:,t+1) = exp(VF_NN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux


            ! only member 1 eligible for benefits
            ! preparing correction to avoid floating point exception
            VFstack4(:,:,:,1) = VF_US(:,:,:,t+1)/sigma_eps
            VFstack4(:,:,:,2) = VF_UN(:,:,:,t+1)/sigma_eps
            VFstack4(:,:,:,3) = VF_NS(:,:,:,t+1)/sigma_eps
            VFstack4(:,:,:,4) = VF_NN(:,:,:,t+1)/sigma_eps
            corr=MAXVAL(VFstack4,4)

            ! choice PBs if none has a job (offer)
            sumVF_aux=  exp(VF_US(:,:,:,t+1)/sigma_eps-corr) + exp(VF_UN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NS(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NN(:,:,:,t+1)/sigma_eps-corr)
            pi_XX_BX_US(:,:,:,t+1) = exp(VF_US(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XX_BX_UN(:,:,:,t+1) = exp(VF_UN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XX_BX_NS(:,:,:,t+1) = exp(VF_NS(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XX_BX_NN(:,:,:,t+1) = exp(VF_NN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            
            
            ! only member 2 eligible for benefits
            ! preparing correction to avoid floating point exception
            VFstack4(:,:,:,1) = VF_SU(:,:,:,t+1)/sigma_eps
            VFstack4(:,:,:,2) = VF_SN(:,:,:,t+1)/sigma_eps
            VFstack4(:,:,:,3) = VF_NU(:,:,:,t+1)/sigma_eps
            VFstack4(:,:,:,4) = VF_NN(:,:,:,t+1)/sigma_eps
            corr=MAXVAL(VFstack4,4)

            ! choice PBs if none has a job (offer)
            sumVF_aux=  exp(VF_SU(:,:,:,t+1)/sigma_eps-corr) + exp(VF_SN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NU(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NN(:,:,:,t+1)/sigma_eps-corr)
            pi_XX_XB_SU(:,:,:,t+1) = exp(VF_SU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XX_XB_SN(:,:,:,t+1) = exp(VF_SN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XX_XB_NU(:,:,:,t+1) = exp(VF_NU(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XX_XB_NN(:,:,:,t+1) = exp(VF_NN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux


            ! no member eligible for benefits
            ! preparing correction to avoid floating point exception
            VFstack4(:,:,:,1) = VF_SS(:,:,:,t+1)/sigma_eps
            VFstack4(:,:,:,2) = VF_SN(:,:,:,t+1)/sigma_eps
            VFstack4(:,:,:,3) = VF_NS(:,:,:,t+1)/sigma_eps
            VFstack4(:,:,:,4) = VF_NN(:,:,:,t+1)/sigma_eps
            corr=MAXVAL(VFstack4,4)

            ! choice PBs if none has a job (offer)
            sumVF_aux=  exp(VF_SS(:,:,:,t+1)/sigma_eps-corr) + exp(VF_SN(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NS(:,:,:,t+1)/sigma_eps-corr) + exp(VF_NN(:,:,:,t+1)/sigma_eps-corr)
            pi_XX_XX_SS(:,:,:,t+1) = exp(VF_SS(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XX_XX_SN(:,:,:,t+1) = exp(VF_SN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XX_XX_NS(:,:,:,t+1) = exp(VF_NS(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            pi_XX_XX_NN(:,:,:,t+1) = exp(VF_NN(:,:,:,t+1)/sigma_eps-corr) / sumVF_aux
            


            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !!! Expected Continuation Value of Firms
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            do hhh=1,(HH*HH)   ! hhh loop
            
                
                h=jointhhh(1,hhh)
                hsp=jointhhh(2,hhh)
                
                ! notation for firm value EJ: _AB_CD_FG: AB: today employed or not, CD tomorrow employment offer or not, FG: tomorrow eligible for benefits or not
                ! recall notation for pi _LK_MN_PQ: LK employment offer or not, MN: eligible for benefits or not, PQ: chosen joint employment state
                        
                ! transitions over h if both are employed and stay employed (1,1)
                call exo_trans(1,1, h, hsp, htrans) ! get relevant joint probs of future h 

                do a=1,AA ! future a
                    EJ_help_mat= pi_EE_XX_EE(a,:,:,t+1)*J_EE(a,:,:,t+1) + pi_EE_XX_ES(a,:,:,t+1)*J_ES(a,:,:,t+1) + pi_EE_XX_EN(a,:,:,t+1)*J_EN(a,:,:,t+1)    
                    EJ_EE_EE_XX(h,hsp,a) =   SUM(htrans*EJ_help_mat)
                enddo                         

                ! today both employed, spouse no offer tomorrow (1,2), spouse eligible
                call exo_trans(1,2, h, hsp, htrans) ! get relevant indices and transition probs of future h 

                do a=1,AA ! future a
                    EJ_help_mat= pi_EX_XB_EU(a,:,:,t+1)*J_EU(a,:,:,t+1) + pi_EX_XB_EN(a,:,:,t+1)*J_EN(a,:,:,t+1) 
                    EJ_EE_EX_XB(h,hsp,a) =   SUM(htrans*EJ_help_mat)
                enddo
                
                ! transitions over h if only head employed today
                call exo_trans(1,3, h, hsp, htrans) ! get relevant indices and transition probs of future h 
                do a=1,AA !
                    
                    ! today only head employed, both offer tomorrow, spouse eligible
                    EJ_help_mat= pi_EE_XB_EE(a,:,:,t+1)*J_EE(a,:,:,t+1) + pi_EE_XB_EU(a,:,:,t+1)*J_EU(a,:,:,t+1) + pi_EE_XB_EN(a,:,:,t+1)*J_EN(a,:,:,t+1)    
                    EJ_EX_EE_XB(h,hsp,a) =   SUM(htrans*EJ_help_mat)

                    ! today only head employed, both offer tomorrow, none eligible
                    EJ_help_mat= pi_EE_XX_EE(a,:,:,t+1)*J_EE(a,:,:,t+1) + pi_EE_XX_ES(a,:,:,t+1)*J_ES(a,:,:,t+1) + pi_EE_XX_EN(a,:,:,t+1)*J_EN(a,:,:,t+1)    
                    EJ_EX_EE_XX(h,hsp,a) = SUM(htrans*EJ_help_mat)

                    ! today only head employed, head offer tomorrow, spouse eligible
                    EJ_help_mat= pi_EX_XB_EU(a,:,:,t+1)*J_EU(a,:,:,t+1) + pi_EX_XB_EN(a,:,:,t+1)*J_EN(a,:,:,t+1) 
                    EJ_EX_EX_XB(h,hsp,a) = SUM(htrans*EJ_help_mat)
                    
                    ! today only head employed, head offer tomorrow, none eligible
                    EJ_help_mat= pi_EX_XX_ES(a,:,:,t+1)*J_ES(a,:,:,t+1) + pi_EX_XX_EN(a,:,:,t+1)*J_EN(a,:,:,t+1) 
                    EJ_EX_EX_XX(h,hsp,a) = SUM(htrans*EJ_help_mat)
                    
                enddo

                ! transitions over h if only spouse employed today and stays employed tomorrow (3,1)
                call exo_trans(3,1, h, hsp, htrans) ! get relevant indices and transition probs of future h 
                do a=1,AA !
                    
                    ! today only spouse employed, both offer tomorrow, head eligible
                    EJ_help_mat= pi_EE_BX_EE(a,:,:,t+1)*J_EE(a,:,:,t+1) + pi_EE_BX_ES(a,:,:,t+1)*J_ES(a,:,:,t+1) + pi_EE_BX_EN(a,:,:,t+1)*J_EN(a,:,:,t+1)    
                    EJ_XE_EE_BX(h,hsp,a) = SUM(htrans*EJ_help_mat)
                    
                    ! today only spouse employed, both offer tomorrow, none eligible
                    EJ_help_mat= pi_EE_XX_EE(a,:,:,t+1)*J_EE(a,:,:,t+1) + pi_EE_XX_ES(a,:,:,t+1)*J_ES(a,:,:,t+1) + pi_EE_XX_EN(a,:,:,t+1)*J_EN(a,:,:,t+1)    
                    EJ_XE_EE_XX(h,hsp,a) = SUM(htrans*EJ_help_mat)
                enddo

                call exo_trans(3,2, h, hsp, htrans) ! get relevant indices and transition probs of future h 
                do a=1,AA !
                    ! today only spouse employed, head offer tomorrow, both eligible
                    EJ_help_mat= pi_EX_BB_EU(a,:,:,t+1)*J_EU(a,:,:,t+1) + pi_EX_BB_EN(a,:,:,t+1)*J_EN(a,:,:,t+1) 
                    EJ_XE_EX_BB(h,hsp,a) = SUM(htrans*EJ_help_mat)

                    ! today only spouse employed, head offer tomorrow, spouse eligible
                    EJ_help_mat= pi_EX_XB_EU(a,:,:,t+1)*J_EU(a,:,:,t+1) + pi_EX_XB_EN(a,:,:,t+1)*J_EN(a,:,:,t+1) 
                    EJ_XE_EX_XB(h,hsp,a) = SUM(htrans*EJ_help_mat)             
                enddo
            

                ! transitions over h if neither is employed today
                call exo_trans(3,3, h, hsp, htrans) ! get relevant indices and transition probs of future h 
                do a=1,AA !
                
                    ! today none employed, both offer tomorrow, none eligible
                    EJ_help_mat= pi_EE_XX_EE(a,:,:,t+1)*J_EE(a,:,:,t+1) + pi_EE_XX_ES(a,:,:,t+1)*J_ES(a,:,:,t+1) + pi_EE_XX_EN(a,:,:,t+1)*J_EN(a,:,:,t+1)    
                    EJ_XX_EE_XX(h,hsp,a) = SUM(htrans*EJ_help_mat) 
        
                    ! today none employed, both offer tomorrow, head eligible
                    EJ_help_mat= pi_EE_BX_EE(a,:,:,t+1)*J_EE(a,:,:,t+1) + pi_EE_BX_ES(a,:,:,t+1)*J_ES(a,:,:,t+1) + pi_EE_BX_EN(a,:,:,t+1)*J_EN(a,:,:,t+1)    
                    EJ_XX_EE_BX(h,hsp,a) = SUM(htrans*EJ_help_mat) 
               
                    ! today none employed, both offer tomorrow, spouse eligible
                    EJ_help_mat= pi_EE_XB_EE(a,:,:,t+1)*J_EE(a,:,:,t+1) + pi_EE_XB_EU(a,:,:,t+1)*J_EU(a,:,:,t+1) + pi_EE_XB_EN(a,:,:,t+1)*J_EN(a,:,:,t+1)    
                    EJ_XX_EE_XB(h,hsp,a) = SUM(htrans*EJ_help_mat) 
                        
                    ! today none employed, both offer tomorrow, both eligible    
                    EJ_help_mat= pi_EE_BB_EE(a,:,:,t+1)*J_EE(a,:,:,t+1) + pi_EE_BB_EU(a,:,:,t+1)*J_EU(a,:,:,t+1) + pi_EE_BB_EN(a,:,:,t+1)*J_EN(a,:,:,t+1)    
                    EJ_XX_EE_BB(h,hsp,a) = SUM(htrans*EJ_help_mat) 
                        
                    ! today none employed, head offer tomorrow, none eligible
                    EJ_help_mat= pi_EX_XX_ES(a,:,:,t+1)*J_ES(a,:,:,t+1) + pi_EX_XX_EN(a,:,:,t+1)*J_EN(a,:,:,t+1) 
                    EJ_XX_EX_XX(h,hsp,a) = SUM(htrans*EJ_help_mat) 
                    
                    ! today none employed, both head tomorrow, head eligible
                    EJ_help_mat= pi_EX_BX_ES(a,:,:,t+1)*J_ES(a,:,:,t+1) + pi_EX_BX_EN(a,:,:,t+1)*J_EN(a,:,:,t+1) 
                    EJ_XX_EX_BX(h,hsp,a) = SUM(htrans*EJ_help_mat) 
                    
                    ! today none employed, head offer tomorrow, spouse eligible
                    EJ_help_mat= pi_EX_XB_EU(a,:,:,t+1)*J_EU(a,:,:,t+1) + pi_EX_XB_EN(a,:,:,t+1)*J_EN(a,:,:,t+1) 
                    EJ_XX_EX_XB(h,hsp,a) = SUM(htrans*EJ_help_mat) 
                
                    ! today none employed, head offer tomorrow, both eligible       
                    EJ_help_mat= pi_EX_BB_EU(a,:,:,t+1)*J_EU(a,:,:,t+1) + pi_EX_BB_EN(a,:,:,t+1)*J_EN(a,:,:,t+1) 
                    EJ_XX_EX_BB(h,hsp,a) = SUM(htrans*EJ_help_mat) 
          
                enddo
            enddo




            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !!! Free Entry: Endogenous Arrival Rates
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            do hhh=1,(HH*HH)   ! hhh loop
            
                
                h=jointhhh(1,hhh)
                hsp=jointhhh(2,hhh)
                
                ! if spouse employed, invert simple free entry condition
                do a=1,AA ! asset loop -- future assets

                    ! spouse remains employed
                    q_help = kappa / ( phiUS*EJ_XE_EE_XX(h,hsp,a) + (1d0-phiUS)*EJ_XE_EE_BX(h,hsp,a) )
                    if (q_help>xi**(1d0/(1d0-alpha))) then
                        theta1_help = (q_help/xi)**(-1d0/alpha)
                    else
                        theta1_help = 1d0/q_help
                    end if
                    lambda_UE_E(a,h,hsp,t) = lambda_u*ptheta(theta1_help)

                    q_help = kappa / ( EJ_XE_EE_XX(h,hsp,a) ) ! compute vacancy filling rate to clear free entry
                    if (q_help>xi**(1d0/(1d0-alpha))) then ! invert q(theta), careful because includes minimum operator
                        theta1_help = (q_help/xi)**(-1d0/alpha)
                    else
                        theta1_help = 1d0/q_help
                    end if
                    lambda_SE_E(a,h,hsp,t) = lambda_s*ptheta(theta1_help) ! lambda = lambda_u*p(theta)
                    lambda_NE_E(a,h,hsp,t) = lambda_n*ptheta(theta1_help) ! same expectation as SE, only change is in fixed lambda

                    ! spouse loses job
                    q_help = kappa / ( phiUS*EJ_XE_EX_XB(h,hsp,a) + (1d0-phiUS)*EJ_XE_EX_BB(h,hsp,a) )
                    if (q_help>xi**(1d0/(1d0-alpha))) then
                        theta1_help = (q_help/xi)**(-1d0/alpha)
                    else
                        theta1_help = 1d0/q_help
                    end if
                    lambda_UE_X(a,h,hsp,t) = lambda_u*ptheta(theta1_help)

                    q_help = kappa / ( EJ_XE_EX_XB(h,hsp,a) ) ! compute vacancy filling rate to clear free entry
                    if (q_help>xi**(1d0/(1d0-alpha))) then ! invert q(theta), careful because includes minimum operator
                        theta1_help = (q_help/xi)**(-1d0/alpha)
                    else
                        theta1_help = 1d0/q_help
                    end if
                    lambda_SE_X(a,h,hsp,t) = lambda_s*ptheta(theta1_help) ! lambda = lambda_u*p(theta)
                    lambda_NE_X(a,h,hsp,t) = lambda_n*ptheta(theta1_help) ! same expectation as SE, only change is in fixed lambda
                
                end do
                
                
                ! here use function, because need to solve for two thetas in parallel 
                do a=1,AA ! asset loop -- future assets
                    
                    ! case where both can lose eligibility
                    ! compute expectations of firm value over future eligibility for benefits for all relevant cases of spousal job (yes / no)
                    EJ_EE_1 = (1d0-phiUS)*(1d0-phiUS)*EJ_XX_EE_BB(h,hsp,a) + phiUS*(1d0-phiUS)*EJ_XX_EE_BX(h,hsp,a) + phiUS*(1d0-phiUS)*EJ_XX_EE_XB(h,hsp,a) + phiUS*phiUS*EJ_XX_EE_XX(h,hsp,a)
                    EJ_EX_1 = (1d0-phiUS)*(1d0-phiUS)*EJ_XX_EX_BB(h,hsp,a) + phiUS*(1d0-phiUS)*EJ_XX_EX_BX(h,hsp,a) + phiUS*(1d0-phiUS)*EJ_XX_EX_XB(h,hsp,a) + phiUS*phiUS*EJ_XX_EX_XX(h,hsp,a)
                    ! need symmetric for spouse for parallel free entry condition, note here hsp and h flipped!
                    EJ_EE_2 = (1d0-phiUS)*(1d0-phiUS)*EJ_XX_EE_BB(hsp,h,a) + phiUS*(1d0-phiUS)*EJ_XX_EE_BX(hsp,h,a) + phiUS*(1d0-phiUS)*EJ_XX_EE_XB(hsp,h,a) + phiUS*phiUS*EJ_XX_EE_XX(hsp,h,a)
                    EJ_EX_2 = (1d0-phiUS)*(1d0-phiUS)*EJ_XX_EX_BB(hsp,h,a) + phiUS*(1d0-phiUS)*EJ_XX_EX_BX(hsp,h,a) + phiUS*(1d0-phiUS)*EJ_XX_EX_XB(hsp,h,a) + phiUS*phiUS*EJ_XX_EX_XX(hsp,h,a)
                    
                    ! solve free entry
                    call find_theta(lambda_u, lambda_u,EJ_EE_1,EJ_EX_1,EJ_EE_2,EJ_EX_2,theta1_help, theta2_help)
                    ! get lambda for household from equilibrium theta
                    lambda_UU(a,h,hsp,t) = lambda_u*ptheta(theta1_help)
                    


                    ! cases where head can lose eligibility
                    ! compute expectations of firm value over future eligibility for benefits for all relevant cases of spousal job (yes / no)
                    EJ_EE_1 = (1d0-phiUS)*EJ_XX_EE_BX(h,hsp,a) + phiUS*EJ_XX_EE_XX(h,hsp,a)
                    EJ_EX_1 = (1d0-phiUS)*EJ_XX_EX_BX(h,hsp,a) + phiUS*EJ_XX_EX_XX(h,hsp,a)
                    ! need symmetric for spouse for parallel free entry condition, note here hsp and h flipped and so is future eligibility!
                    EJ_EE_2 = (1d0-phiUS)*EJ_XX_EE_XB(hsp,h,a) + phiUS*EJ_XX_EE_XX(hsp,h,a)
                    EJ_EX_2 = (1d0-phiUS)*EJ_XX_EX_XB(hsp,h,a) + phiUS*EJ_XX_EX_XX(hsp,h,a)
                    
                    ! solve free entry
                    call find_theta(lambda_u, lambda_n,EJ_EE_1,EJ_EX_1,EJ_EE_2,EJ_EX_2,theta1_help, theta2_help)
                    ! get lambda for household from equilibrium theta
                    lambda_UN(a,h,hsp,t) = lambda_u*ptheta(theta1_help)
                    
                    ! solve free entry
                    call find_theta(lambda_u, lambda_s,EJ_EE_1,EJ_EX_1,EJ_EE_2,EJ_EX_2,theta1_help, theta2_help)
                    ! get lambda for household from equilibrium theta
                    lambda_US(a,h,hsp,t) = lambda_u*ptheta(theta1_help)



                    ! cases where spouse can lose eligibility
                    ! compute expectations of firm value over future eligibility for benefits for all relevant cases of spousal job (yes / no)
                    EJ_EE_1 = (1d0-phiUS)*EJ_XX_EE_XB(h,hsp,a) + phiUS*EJ_XX_EE_XX(h,hsp,a)
                    EJ_EX_1 = (1d0-phiUS)*EJ_XX_EX_XB(h,hsp,a) + phiUS*EJ_XX_EX_XX(h,hsp,a)
                    ! need symmetric for spouse for parallel free entry condition, note here hsp and h flipped and so is future eligibility!
                    EJ_EE_2 = (1d0-phiUS)*EJ_XX_EE_BX(hsp,h,a) + phiUS*EJ_XX_EE_XX(hsp,h,a)
                    EJ_EX_2 = (1d0-phiUS)*EJ_XX_EX_BX(hsp,h,a) + phiUS*EJ_XX_EX_XX(hsp,h,a)
                    
                    ! solve free entry
                    call find_theta(lambda_n, lambda_u,EJ_EE_1,EJ_EX_1,EJ_EE_2,EJ_EX_2,theta1_help, theta2_help)
                    ! get lambda for household from equilibrium theta
                    lambda_NU(a,h,hsp,t) = lambda_n*ptheta(theta1_help)
                    
                    ! solve free entry
                    call find_theta(lambda_s, lambda_u,EJ_EE_1,EJ_EX_1,EJ_EE_2,EJ_EX_2,theta1_help, theta2_help)
                    ! get lambda for household from equilibrium theta
                    lambda_SU(a,h,hsp,t) = lambda_s*ptheta(theta1_help)
                    
                    
                    
                    ! cases where none can lose eligibility
                    ! compute expectations of firm value over future eligibility for benefits for all relevant cases of spousal job (yes / no)
                    EJ_EE_1 = EJ_XX_EE_XX(h,hsp,a)
                    EJ_EX_1 = EJ_XX_EX_XX(h,hsp,a)
                    ! need symmetric for spouse for parallel free entry condition, note here hsp and h flipped and so is future eligibility!
                    EJ_EE_2 = EJ_XX_EE_XX(hsp,h,a)
                    EJ_EX_2 = EJ_XX_EX_XX(hsp,h,a)
                    
                    ! solve free entry
                    call find_theta(lambda_n, lambda_n,EJ_EE_1,EJ_EX_1,EJ_EE_2,EJ_EX_2,theta1_help, theta2_help)
                    ! get lambda for household from equilibrium theta
                    lambda_NN(a,h,hsp,t) = lambda_n*ptheta(theta1_help)

                    ! solve free entry
                    call find_theta(lambda_s, lambda_s,EJ_EE_1,EJ_EX_1,EJ_EE_2,EJ_EX_2,theta1_help, theta2_help)
                    ! get lambda for household from equilibrium theta
                    lambda_SS(a,h,hsp,t) = lambda_s*ptheta(theta1_help)

                    ! solve free entry
                    call find_theta(lambda_s, lambda_n,EJ_EE_1,EJ_EX_1,EJ_EE_2,EJ_EX_2,theta1_help, theta2_help)
                    ! get lambda for household from equilibrium theta
                    lambda_SN(a,h,hsp,t) = lambda_s*ptheta(theta1_help)

                    ! solve free entry
                    call find_theta(lambda_n, lambda_s,EJ_EE_1,EJ_EX_1,EJ_EE_2,EJ_EX_2,theta1_help, theta2_help)
                    ! get lambda for household from equilibrium theta
                    lambda_NS(a,h,hsp,t) = lambda_n*ptheta(theta1_help)
                    
                end do

            end do

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !!! Solve Consumption-Savings Problem
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            ! parallelize
        
            !$OMP PARALLEL DO PRIVATE(cpol_endo, astate, EVFp, EVFp_interp, VF_endo, aind_non, AA_red, astate_red, apgrid_red, hsp, h, a)
        
            do hhh=1,(HH*HH)   ! hhh loop
            
                
                h=jointhhh(1,hhh)
                hsp=jointhhh(2,hhh)
                
                    
                !!!!!!!!!!
                ! EE case
                !!!!!!!!!!
                
                do a=1,AA ! asset loop -- asset choice on exogenous grid

                    ! endogenous asset grid
                    cpol_endo(a) = RHS_EGM(a,1,1,h,hsp,t,(1d0-delta(t,h))*(1d0-delta(t,hsp)), (1d0-delta(t,h))*(delta(t,hsp)),(delta(t,h))*(1d0-delta(t,hsp)),(delta(t,h))*(delta(t,hsp)),0d0, 1d0, 0d0, 1d0)           
                    astate(a) = (cpol_endo(a) + assgrid(a) - (1d0-tau_ss)*wage(h) - &
                                                    (1d0-tau_ss)*wage(hsp)) / (1d0+r) 
                    EVFp(a)=EVF(a,1,1,h,hsp,t,(1d0-delta(t,h))*(1d0-delta(t,hsp)), (1d0-delta(t,h))*(delta(t,hsp)),(delta(t,h))*(1d0-delta(t,hsp)),(delta(t,h))*(delta(t,hsp)), 0d0, 1d0, 0d0, 1d0) ! expected future value function over exogenous grid
                    
                end do  ! end of asset loop
                
                ! value function on endogenous grid
                VF_endo=util_vec(cpol_endo) + psi_EE(t) + beta*EVFp
    
                ! upper envelope step
                aind_non = 0
                if (minval(astate(2:AA)-astate(1:AA-1)) < 0d0) then
                    print *, "astate not monotonically increasing before UEV step (EE)"
                    call UEVstep(astate,VF_endo,aind_non)
                endif
                
                ! number of non-eliminated gripoints
                AA_red=AA-sum(aind_non)
                if (AA_red/=AA) print *,(AA-AA_red)
                
                ! constructing reduced endogenous grid / policy correspondence
                allocate(astate_red(AA_red))
                allocate(apgrid_red(AA_red))                            
                astate_red=pack(astate,aind_non==0)
                apgrid_red=pack(assgrid,aind_non==0)
                if (minval(astate_red(2:AA_red)-astate_red(1:AA_red-1)) < 0d0) then
                    print *, "astate not monotonically increasing after UEV step (EE)"
                    stop
                endif
                
                ! interpolation to exogenous grid 
                call LinInterp_Extrap(AA_red,astate_red,apgrid_red,AA,assgrid,apol_EE(:,h,hsp,t))
                where (apol_EE(:,h,hsp,t) < assgrid(1)) apol_EE(:,h,hsp,t) = assgrid(1) ! adjusting for borrowing constraint

                deallocate(astate_red)
                deallocate(apgrid_red)
                
                ! consumption policy and value function
                cpol_EE(:,h,hsp,t) = (1d0-tau_ss)*wage(h) + (1d0-tau_ss)*wage(hsp) + (1d0 + r) * assgrid - apol_EE(:,h,hsp,t)
                call LinInterp_Extrap(AA,assgrid,EVFp,AA,apol_EE(:,h,hsp,t),EVFp_interp)
                VF_EE(:,h,hsp,t) = util_vec(cpol_EE(:,h,hsp,t)) + psi_EE(t) + beta*EVFp_interp
            
                
                !!!!!!!!!!!!!!!!!
                ! UE case
                !!!!!!!!!!!!!!!!!
                do a=1,AA ! asset loop -- asset choice on exogenous grid
                
                    ! endogenous asset grid
                    cpol_endo(a) = RHS_EGM(a,0,1,h,hsp,t,(lambda_UE_E(a,h,hsp,t))*(1d0-delta(t,hsp)),(lambda_UE_X(a,h,hsp,t))*(delta(t,hsp)),(1d0-lambda_UE_E(a,h,hsp,t))*(1d0-delta(t,hsp)),(1d0-lambda_UE_X(a,h,hsp,t))*(delta(t,hsp)), (1d0-phiUS), (1d0-phiUS), 0d0, 1d0)           
                    astate(a) = (cpol_endo(a) + assgrid(a) - (1d0-tau_ss)*ben(h) - &
                                                    (1d0-tau_ss)*wage(hsp)) / (1d0+r) 
                    EVFp(a)=EVF(a,0,1,h,hsp,t,(lambda_UE_E(a,h,hsp,t))*(1d0-delta(t,hsp)),(lambda_UE_X(a,h,hsp,t))*(delta(t,hsp)),(1d0-lambda_UE_E(a,h,hsp,t))*(1d0-delta(t,hsp)),(1d0-lambda_UE_X(a,h,hsp,t))*(delta(t,hsp)), (1d0-phiUS), (1d0-phiUS), 0d0, 1d0) ! expected future value function over exogenous grid
                    
                end do  ! end of asset loop

                ! value function on endogenous grid
                VF_endo=util_vec(cpol_endo) + psi_UE(t) + beta*EVFp
    
                ! upper envelope step
                aind_non = 0
                if (minval(astate(2:AA)-astate(1:AA-1)) < 0d0) then
                    print *, "astate not monotonically increasing before UEV step (UE)"
                    call UEVstep(astate,VF_endo,aind_non)
                endif
                
                ! number of non-eliminated gripoints
                AA_red=AA-sum(aind_non)
                if (AA_red/=AA) print *,(AA-AA_red)
                
                ! constructing reduced endogenous grid / policy correspondence
                allocate(astate_red(AA_red))
                allocate(apgrid_red(AA_red))                            
                astate_red=pack(astate,aind_non==0)
                apgrid_red=pack(assgrid,aind_non==0)
                if (minval(astate_red(2:AA_red)-astate_red(1:AA_red-1)) < 0d0) then
                    print *, "astate not monotonically increasing after UEV step (UE)"
                    stop
                endif
                
                ! interpolation to exogenous grid 
                call LinInterp_Extrap(AA_red,astate_red,apgrid_red,AA,assgrid,apol_UE(:,h,hsp,t))
                where (apol_UE(:,h,hsp,t) < assgrid(1)) apol_UE(:,h,hsp,t) = assgrid(1) ! adjusting for borrowing constraint
                
                deallocate(astate_red)
                deallocate(apgrid_red)
                
                ! consumption policy and value function
                cpol_UE(:,h,hsp,t) = (1d0-tau_ss)*ben(h) + (1d0-tau_ss)*wage(hsp) + (1d0 + r) * assgrid - apol_UE(:,h,hsp,t)
                call LinInterp_Extrap(AA,assgrid,EVFp,AA,apol_UE(:,h,hsp,t),EVFp_interp)
                VF_UE(:,h,hsp,t) = util_vec(cpol_UE(:,h,hsp,t)) + psi_UE(t) + beta*EVFp_interp

                !!!!!!!!!!!!!!!!!
                ! NE case
                !!!!!!!!!!!!!!!!!
                do a=1,AA ! asset loop -- asset choice on exogenous grid
                
                    ! endogenous asset grid
                    cpol_endo(a) = RHS_EGM(a,0,1,h,hsp,t,(lambda_NE_E(a,h,hsp,t))*(1d0-delta(t,hsp)), (lambda_NE_X(a,h,hsp,t))*(delta(t,hsp)), (1d0-lambda_NE_E(a,h,hsp,t))*(1d0-delta(t,hsp)), (1d0-lambda_NE_X(a,h,hsp,t))*(delta(t,hsp)), 0d0, 0d0, 0d0, 1d0)           
                    astate(a) = (cpol_endo(a) + assgrid(a) - (1d0-tau_ss)*wage(hsp)) / (1d0+r) 
                    EVFp(a)=EVF(a,0,1,h,hsp,t,(lambda_NE_E(a,h,hsp,t))*(1d0-delta(t,hsp)), (lambda_NE_X(a,h,hsp,t))*(delta(t,hsp)), (1d0-lambda_NE_E(a,h,hsp,t))*(1d0-delta(t,hsp)), (1d0-lambda_NE_X(a,h,hsp,t))*(delta(t,hsp)), 0d0, 0d0, 0d0, 1d0) ! expected future value function over exogenous grid
                    
                end do  ! end of asset loop

                ! value function on endogenous grid
                VF_endo=util_vec(cpol_endo) + psi_NE(t) + beta*EVFp
    
                ! upper envelope step
                aind_non = 0
                if (minval(astate(2:AA)-astate(1:AA-1)) < 0d0) then
                    print *, "astate not monotonically increasing before UEV step (NE)"
                    call UEVstep(astate,VF_endo,aind_non)
                endif
                
                ! number of non-eliminated gripoints
                AA_red=AA-sum(aind_non)
                if (AA_red/=AA) print *,(AA-AA_red)
                
                ! constructing reduced endogenous grid / policy correspondence
                allocate(astate_red(AA_red))
                allocate(apgrid_red(AA_red))                            
                astate_red=pack(astate,aind_non==0)
                apgrid_red=pack(assgrid,aind_non==0)
                if (minval(astate_red(2:AA_red)-astate_red(1:AA_red-1)) < 0d0) then
                    print *, "astate not monotonically increasing after UEV step (NE)"
                    stop
                endif
                
                ! interpolation to exogenous grid 
                call LinInterp_Extrap(AA_red,astate_red,apgrid_red,AA,assgrid,apol_NE(:,h,hsp,t))
                where (apol_NE(:,h,hsp,t) < assgrid(1)) apol_NE(:,h,hsp,t) = assgrid(1) ! adjusting for borrowing constraint
                
                deallocate(astate_red)
                deallocate(apgrid_red)
                
                ! consumption policy and value function
                cpol_NE(:,h,hsp,t) = (1d0-tau_ss)*wage(hsp) + (1d0 + r) * assgrid - apol_NE(:,h,hsp,t)
                call LinInterp_Extrap(AA,assgrid,EVFp,AA,apol_NE(:,h,hsp,t),EVFp_interp)
                VF_NE(:,h,hsp,t) = util_vec(cpol_NE(:,h,hsp,t)) + psi_NE(t) + beta*EVFp_interp

                !!!!!!!!!!!!!!!!!
                ! SE case
                !!!!!!!!!!!!!!!!!
                do a=1,AA ! asset loop -- asset choice on exogenous grid
                
                    ! endogenous asset grid
                    cpol_endo(a) = RHS_EGM(a,0,1,h,hsp,t,(lambda_SE_E(a,h,hsp,t))*(1d0-delta(t,hsp)), (lambda_SE_X(a,h,hsp,t))*(delta(t,hsp)), (1d0-lambda_SE_E(a,h,hsp,t))*(1d0-delta(t,hsp)), (1d0-lambda_SE_X(a,h,hsp,t))*(delta(t,hsp)), 0d0, 0d0, 0d0, 1d0)           
                    astate(a) = (cpol_endo(a) + assgrid(a) - &
                                                    (1d0-tau_ss)*wage(hsp)) / (1d0+r) 
                    EVFp(a)=EVF(a,0,1,h,hsp,t,(lambda_SE_E(a,h,hsp,t))*(1d0-delta(t,hsp)), (lambda_SE_X(a,h,hsp,t))*(delta(t,hsp)), (1d0-lambda_SE_E(a,h,hsp,t))*(1d0-delta(t,hsp)), (1d0-lambda_SE_X(a,h,hsp,t))*(delta(t,hsp)), 0d0, 0d0, 0d0, 1d0) ! expected future value function over exogenous grid
                    
                end do  ! end of asset loop

                ! value function on endogenous grid
                VF_endo=util_vec(cpol_endo) + psi_SE(t) + beta*EVFp
    
                ! upper envelope step
                aind_non = 0
                if (minval(astate(2:AA)-astate(1:AA-1)) < 0d0) then
                    print *, "astate not monotonically increasing before UEV step (SE)"
                    call UEVstep(astate,VF_endo,aind_non)
                endif
                
                ! number of non-eliminated gripoints
                AA_red=AA-sum(aind_non)
                if (AA_red/=AA) print *,(AA-AA_red)
                
                ! constructing reduced endogenous grid / policy correspondence
                allocate(astate_red(AA_red))
                allocate(apgrid_red(AA_red))                            
                astate_red=pack(astate,aind_non==0)
                apgrid_red=pack(assgrid,aind_non==0)
                if (minval(astate_red(2:AA_red)-astate_red(1:AA_red-1)) < 0d0) then
                    print *, "astate not monotonically increasing after UEV step (SE)"
                    stop
                endif
                
                ! interpolation to exogenous grid 
                call LinInterp_Extrap(AA_red,astate_red,apgrid_red,AA,assgrid,apol_SE(:,h,hsp,t))
                where (apol_SE(:,h,hsp,t) < assgrid(1)) apol_SE(:,h,hsp,t) = assgrid(1) ! adjusting for borrowing constraint
                
                deallocate(astate_red)
                deallocate(apgrid_red)
                
                ! consumption policy and value function
                cpol_SE(:,h,hsp,t) = (1d0-tau_ss)*wage(hsp) + (1d0 + r) * assgrid - apol_SE(:,h,hsp,t)
                call LinInterp_Extrap(AA,assgrid,EVFp,AA,apol_SE(:,h,hsp,t),EVFp_interp)
                VF_SE(:,h,hsp,t) = util_vec(cpol_SE(:,h,hsp,t)) + psi_SE(t) + beta*EVFp_interp
                    
                    
                !!!!!!!!!!!!!!!!!
                ! UU case
                !!!!!!!!!!!!!!!!!
                do a=1,AA ! asset loop -- asset choice on exogenous grid
                
                    ! endogenous asset grid
                    cpol_endo(a) = RHS_EGM(a,0,0,h,hsp,t,(lambda_UU(a,h,hsp,t))*(lambda_UU(a,hsp,h,t)), (lambda_UU(a,h,hsp,t))*(1d0-lambda_UU(a,hsp,h,t)), (1d0-lambda_UU(a,h,hsp,t))*(lambda_UU(a,hsp,h,t)), (1d0-lambda_UU(a,h,hsp,t))*(1d0-lambda_UU(a,hsp,h,t)), (1d0-phiUS), (1d0-phiUS), (1d0-phiUS), (1d0-phiUS))           
                    astate(a) = (cpol_endo(a) + assgrid(a) - (1d0-tau_ss)*ben(h) - &
                                                    (1d0-tau_ss)*ben(hsp)) / (1d0+r) 
                    EVFp(a)=EVF(a,0,0,h,hsp,t,(lambda_UU(a,h,hsp,t))*(lambda_UU(a,hsp,h,t)), (lambda_UU(a,h,hsp,t))*(1d0-lambda_UU(a,hsp,h,t)), (1d0-lambda_UU(a,h,hsp,t))*(lambda_UU(a,hsp,h,t)), (1d0-lambda_UU(a,h,hsp,t))*(1d0-lambda_UU(a,hsp,h,t)), (1d0-phiUS), (1d0-phiUS), (1d0-phiUS), (1d0-phiUS)) ! expected future value function over exogenous grid
                    
                end do  ! end of asset loop
                
                ! value function on endogenous grid
                VF_endo=util_vec(cpol_endo) + psi_UU(t) + beta*EVFp
    
                ! upper envelope step
                aind_non = 0
                if (minval(astate(2:AA)-astate(1:AA-1)) < 0d0) then
                    print *, "astate not monotonically increasing before UEV step (UU)"
                    call UEVstep(astate,VF_endo,aind_non)
                endif
                
                ! number of non-eliminated gripoints
                AA_red=AA-sum(aind_non)
                if (AA_red/=AA) print *,(AA-AA_red)
                
                ! constructing reduced endogenous grid / policy correspondence
                allocate(astate_red(AA_red))
                allocate(apgrid_red(AA_red))                            
                astate_red=pack(astate,aind_non==0)
                apgrid_red=pack(assgrid,aind_non==0)
                if (minval(astate_red(2:AA_red)-astate_red(1:AA_red-1)) < 0d0) then
                    print *, "astate not monotonically increasing after UEV step (UU)"
                    stop
                endif
                
                ! interpolation to exogenous grid 
                call LinInterp_Extrap(AA_red,astate_red,apgrid_red,AA,assgrid,apol_UU(:,h,hsp,t))
                where (apol_UU(:,h,hsp,t) < assgrid(1)) apol_UU(:,h,hsp,t) = assgrid(1) ! adjusting for borrowing constraint
                
                deallocate(astate_red)
                deallocate(apgrid_red)
                
                ! consumption policy and value function
                cpol_UU(:,h,hsp,t) = (1d0-tau_ss)*ben(h) + (1d0-tau_ss)*ben(hsp) &
                                                + (1d0 + r) * assgrid - apol_UU(:,h,hsp,t)
                call LinInterp_Extrap(AA,assgrid,EVFp,AA,apol_UU(:,h,hsp,t),EVFp_interp)
                VF_UU(:,h,hsp,t) = util_vec(cpol_UU(:,h,hsp,t)) + psi_UU(t) + beta*EVFp_interp

                !!!!!!!!!!!!!!!!!
                ! SS case
                !!!!!!!!!!!!!!!!!
                do a=1,AA ! asset loop -- asset choice on exogenous grid
                
                    ! endogenous asset grid
                    cpol_endo(a) = RHS_EGM(a,0,0,h,hsp,t,(lambda_SS(a,h,hsp,t))*(lambda_SS(a,hsp,h,t)), (lambda_SS(a,h,hsp,t))*(1d0-lambda_SS(a,hsp,h,t)), (1d0-lambda_SS(a,h,hsp,t))*(lambda_SS(a,hsp,h,t)), (1d0-lambda_SS(a,h,hsp,t))*(1d0-lambda_SS(a,hsp,h,t)), 0d0, 0d0, 0d0, 0d0)           
                    astate(a) = (cpol_endo(a) + assgrid(a)) / (1d0+r) 
                    EVFp(a)=EVF(a,0,0,h,hsp,t,(lambda_SS(a,h,hsp,t))*(lambda_SS(a,hsp,h,t)), (lambda_SS(a,h,hsp,t))*(1d0-lambda_SS(a,hsp,h,t)), (1d0-lambda_SS(a,h,hsp,t))*(lambda_SS(a,hsp,h,t)), (1d0-lambda_SS(a,h,hsp,t))*(1d0-lambda_SS(a,hsp,h,t)), 0d0, 0d0, 0d0, 0d0) ! expected future value function over exogenous grid
                    
                end do  ! end of asset loop

                ! value function on endogenous grid
                VF_endo=util_vec(cpol_endo) + psi_SS(t) + beta*EVFp
    
                ! upper envelope step
                aind_non = 0
                call UEVstep(astate,VF_endo,aind_non)
                if (minval(astate(2:AA)-astate(1:AA-1)) < 0d0) then
                    print *, "astate not monotonically increasing before UEV step (SS)"
                    call UEVstep(astate,VF_endo,aind_non)
                endif
                
                ! number of non-eliminated gripoints
                AA_red=AA-sum(aind_non)
                if (AA_red/=AA) print *,(AA-AA_red)
                
                ! constructing reduced endogenous grid / policy correspondence
                allocate(astate_red(AA_red))
                allocate(apgrid_red(AA_red))                            
                astate_red=pack(astate,aind_non==0)
                apgrid_red=pack(assgrid,aind_non==0)
                if (minval(astate_red(2:AA_red)-astate_red(1:AA_red-1)) < 0d0) then
                    print *, "astate not monotonically increasing after UEV step (SS)"
                    stop
                endif
                
                ! interpolation to exogenous grid 
                call LinInterp_Extrap(AA_red,astate_red,apgrid_red,AA,assgrid,apol_SS(:,h,hsp,t))
                where (apol_SS(:,h,hsp,t) < assgrid(1)) apol_SS(:,h,hsp,t) = assgrid(1) ! adjusting for borrowing constraint
                
                deallocate(astate_red)
                deallocate(apgrid_red)
                
                ! consumption policy and value function
                cpol_SS(:,h,hsp,t) = (1d0 + r) * assgrid - apol_SS(:,h,hsp,t)
                where (cpol_SS(:,h,hsp,t)<=1d-2) cpol_SS(:,h,hsp,t) = 1d-2
                call LinInterp_Extrap(AA,assgrid,EVFp,AA,apol_SS(:,h,hsp,t),EVFp_interp)
                VF_SS(:,h,hsp,t) = util_vec(cpol_SS(:,h,hsp,t)) + psi_SS(t) + beta*EVFp_interp
                
                !!!!!!!!!!!!!!!!!
                ! SU case
                !!!!!!!!!!!!!!!!!
                do a=1,AA ! asset loop -- asset choice on exogenous grid
                
                    ! endogenous asset grid
                    cpol_endo(a) = RHS_EGM(a,0,0,h,hsp,t,(lambda_SU(a,h,hsp,t))*(lambda_US(a,hsp,h,t)), (lambda_SU(a,h,hsp,t))*(1d0-lambda_US(a,hsp,h,t)), (1d0-lambda_SU(a,h,hsp,t))*(lambda_US(a,hsp,h,t)), (1d0-lambda_SU(a,h,hsp,t))*(1d0-lambda_US(a,hsp,h,t)), 0d0, 0d0, (1d0-phiUS), (1d0-phiUS))           
                    astate(a) = (cpol_endo(a) + assgrid(a) - &
                                                    (1d0-tau_ss)*ben(hsp)) / (1d0+r) 
                    EVFp(a)=EVF(a,0,0,h,hsp,t,(lambda_SU(a,h,hsp,t))*(lambda_US(a,hsp,h,t)), (lambda_SU(a,h,hsp,t))*(1d0-lambda_US(a,hsp,h,t)), (1d0-lambda_SU(a,h,hsp,t))*(lambda_US(a,hsp,h,t)), (1d0-lambda_SU(a,h,hsp,t))*(1d0-lambda_US(a,hsp,h,t)), 0d0, 0d0, (1d0-phiUS), (1d0-phiUS))  ! expected future value function over exogenous grid
                    
                end do  ! end of asset loop

                ! value function on endogenous grid
                VF_endo=util_vec(cpol_endo) + psi_SU(t) + beta*EVFp
    
                ! upper envelope step
                aind_non = 0
                if (minval(astate(2:AA)-astate(1:AA-1)) < 0d0) then
                    print *, "astate not monotonically increasing before UEV step (SU)"
                    call UEVstep(astate,VF_endo,aind_non)
                endif
                
                ! number of non-eliminated gripoints
                AA_red=AA-sum(aind_non)
                if (AA_red/=AA) print *,(AA-AA_red)
                
                ! constructing reduced endogenous grid / policy correspondence
                allocate(astate_red(AA_red))
                allocate(apgrid_red(AA_red))                            
                astate_red=pack(astate,aind_non==0)
                apgrid_red=pack(assgrid,aind_non==0)
                if (minval(astate_red(2:AA_red)-astate_red(1:AA_red-1)) < 0d0) then
                    print *, "astate not monotonically increasing after UEV step (SU)"
                    stop
                endif
                
                ! interpolation to exogenous grid 
                call LinInterp_Extrap(AA_red,astate_red,apgrid_red,AA,assgrid,apol_SU(:,h,hsp,t))
                where (apol_SU(:,h,hsp,t) < assgrid(1)) apol_SU(:,h,hsp,t) = assgrid(1) ! adjusting for borrowing constraint
                
                deallocate(astate_red)
                deallocate(apgrid_red)
                
                ! consumption policy and value function
                cpol_SU(:,h,hsp,t) = (1d0-tau_ss)*ben(hsp) + (1d0 + r) * assgrid - apol_SU(:,h,hsp,t)
                call LinInterp_Extrap(AA,assgrid,EVFp,AA,apol_SU(:,h,hsp,t),EVFp_interp)
                VF_SU(:,h,hsp,t) = util_vec(cpol_SU(:,h,hsp,t)) + psi_SU(t) + beta*EVFp_interp

                !!!!!!!!!!!!!!!!!
                ! UN case
                !!!!!!!!!!!!!!!!!
                do a=1,AA ! asset loop -- asset choice on exogenous grid
                
                    ! endogenous asset grid
                    cpol_endo(a) = RHS_EGM(a,0,0,h,hsp,t,(lambda_UN(a,h,hsp,t))*(lambda_NU(a,hsp,h,t)), (lambda_UN(a,h,hsp,t))*(1d0-lambda_NU(a,hsp,h,t)), (1d0-lambda_UN(a,h,hsp,t))*(lambda_NU(a,hsp,h,t)), (1d0-lambda_UN(a,h,hsp,t))*(1d0-lambda_NU(a,hsp,h,t)), (1d0-phiUS), (1d0-phiUS), 0d0, 0d0)           
                    astate(a) = (cpol_endo(a) + assgrid(a) - (1d0-tau_ss)*ben(h) ) / (1d0+r)                                                         
                    EVFp(a)=EVF(a,0,0,h,hsp,t,(lambda_UN(a,h,hsp,t))*(lambda_NU(a,hsp,h,t)), (lambda_UN(a,h,hsp,t))*(1d0-lambda_NU(a,hsp,h,t)), (1d0-lambda_UN(a,h,hsp,t))*(lambda_NU(a,hsp,h,t)), (1d0-lambda_UN(a,h,hsp,t))*(1d0-lambda_NU(a,hsp,h,t)), (1d0-phiUS), (1d0-phiUS), 0d0, 0d0) ! expected future value function over exogenous grid
                    
                end do  ! end of asset loop

                ! value function on endogenous grid
                VF_endo=util_vec(cpol_endo) + psi_UN(t) + beta*EVFp
    
                ! upper envelope step
                aind_non = 0
                if (minval(astate(2:AA)-astate(1:AA-1)) < 0d0) then
                    print *, "astate not monotonically increasing before UEV step (UN)"
                    call UEVstep(astate,VF_endo,aind_non)
                endif
                
                ! number of non-eliminated gripoints
                AA_red=AA-sum(aind_non)
                if (AA_red/=AA) print *,(AA-AA_red)
                
                ! constructing reduced endogenous grid / policy correspondence
                allocate(astate_red(AA_red))
                allocate(apgrid_red(AA_red))                            
                astate_red=pack(astate,aind_non==0)
                apgrid_red=pack(assgrid,aind_non==0)
                if (minval(astate_red(2:AA_red)-astate_red(1:AA_red-1)) < 0d0) then
                    print *, "astate not monotonically increasing after UEV step (UN)"
                    stop
                endif
                
                ! interpolation to exogenous grid 
                call LinInterp_Extrap(AA_red,astate_red,apgrid_red,AA,assgrid,apol_UN(:,h,hsp,t))
                where (apol_UN(:,h,hsp,t) < assgrid(1)) apol_UN(:,h,hsp,t) = assgrid(1) ! adjusting for borrowing constraint
                
                deallocate(astate_red)
                deallocate(apgrid_red)
                
                ! consumption policy and value function
                cpol_UN(:,h,hsp,t) = (1d0-tau_ss)*ben(h) + (1d0 + r) * assgrid - apol_UN(:,h,hsp,t)
                call LinInterp_Extrap(AA,assgrid,EVFp,AA,apol_UN(:,h,hsp,t),EVFp_interp)
                VF_UN(:,h,hsp,t) = util_vec(cpol_UN(:,h,hsp,t)) + psi_UN(t) + beta*EVFp_interp
                
                !!!!!!!!!!!!!!!!!
                ! SN case
                !!!!!!!!!!!!!!!!!
                do a=1,AA ! asset loop -- asset choice on exogenous grid
                
                    ! endogenous asset grid
                    cpol_endo(a) = RHS_EGM(a,0,0,h,hsp,t,(lambda_SN(a,h,hsp,t))*(lambda_NS(a,hsp,h,t)), (lambda_SN(a,h,hsp,t))*(1d0-lambda_NS(a,hsp,h,t)), (1d0-lambda_SN(a,h,hsp,t))*(lambda_NS(a,hsp,h,t)), (1d0-lambda_SN(a,h,hsp,t))*(1d0-lambda_NS(a,hsp,h,t)), 0d0, 0d0, 0d0, 0d0)           
                    astate(a) = (cpol_endo(a) + assgrid(a) ) / (1d0+r)                                                         
                    EVFp(a)=EVF(a,0,0,h,hsp,t,(lambda_SN(a,h,hsp,t))*(lambda_NS(a,hsp,h,t)), (lambda_SN(a,h,hsp,t))*(1d0-lambda_NS(a,hsp,h,t)), (1d0-lambda_SN(a,h,hsp,t))*(lambda_NS(a,hsp,h,t)), (1d0-lambda_SN(a,h,hsp,t))*(1d0-lambda_NS(a,hsp,h,t)), 0d0, 0d0, 0d0, 0d0) ! expected future value function over exogenous grid
                    
                end do  ! end of asset loop

                ! value function on endogenous grid
                VF_endo=util_vec(cpol_endo) + psi_SN(t) + beta*EVFp
    
                ! upper envelope step
                aind_non = 0
                if (minval(astate(2:AA)-astate(1:AA-1)) < 0d0) then
                    print *, "astate not monotonically increasing before UEV step (SN)"
                    call UEVstep(astate,VF_endo,aind_non)
                endif
                
                ! number of non-eliminated gripoints
                AA_red=AA-sum(aind_non)
                if (AA_red/=AA) print *,(AA-AA_red)
                
                ! constructing reduced endogenous grid / policy correspondence
                allocate(astate_red(AA_red))
                allocate(apgrid_red(AA_red))                            
                astate_red=pack(astate,aind_non==0)
                apgrid_red=pack(assgrid,aind_non==0)
                if (minval(astate_red(2:AA_red)-astate_red(1:AA_red-1)) < 0d0) then
                    print *, "astate not monotonically increasing after UEV step (SN)"
                    stop
                endif
                
                ! interpolation to exogenous grid 
                call LinInterp_Extrap(AA_red,astate_red,apgrid_red,AA,assgrid,apol_SN(:,h,hsp,t))
                where (apol_SN(:,h,hsp,t) < assgrid(1)) apol_SN(:,h,hsp,t) = assgrid(1) ! adjusting for borrowing constraint
                
                deallocate(astate_red)
                deallocate(apgrid_red)
                
                ! consumption policy and value function
                cpol_SN(:,h,hsp,t) =  (1d0 + r) * assgrid - apol_SN(:,h,hsp,t)
                where (cpol_SN(:,h,hsp,t)<=1d-2) cpol_SN(:,h,hsp,t) = 1d-2
                call LinInterp_Extrap(AA,assgrid,EVFp,AA,apol_SN(:,h,hsp,t),EVFp_interp)
                VF_SN(:,h,hsp,t) = util_vec(cpol_SN(:,h,hsp,t)) + psi_SN(t) + beta*EVFp_interp

                !!!!!!!!!!!!!!!!!
                ! NN case
                !!!!!!!!!!!!!!!!!
                do a=1,AA ! asset loop -- asset choice on exogenous grid
                
                    ! endogenous asset grid
                    cpol_endo(a) = RHS_EGM(a,0,0,h,hsp,t,(lambda_NN(a,h,hsp,t))*(lambda_NN(a,hsp,h,t)),(lambda_NN(a,h,hsp,t))*(1d0-lambda_NN(a,hsp,h,t)),(1d0-lambda_NN(a,h,hsp,t))*(lambda_NN(a,hsp,h,t)),(1d0-lambda_NN(a,h,hsp,t))*(1d0-lambda_NN(a,hsp,h,t)), 0d0, 0d0, 0d0, 0d0)           
                    astate(a) = (cpol_endo(a) + assgrid(a) ) / (1d0+r)                                                         
                    EVFp(a)=EVF(a,0,0,h,hsp,t,(lambda_NN(a,h,hsp,t))*(lambda_NN(a,hsp,h,t)),(lambda_NN(a,h,hsp,t))*(1d0-lambda_NN(a,hsp,h,t)),(1d0-lambda_NN(a,h,hsp,t))*(lambda_NN(a,hsp,h,t)),(1d0-lambda_NN(a,h,hsp,t))*(1d0-lambda_NN(a,hsp,h,t)), 0d0, 0d0, 0d0, 0d0) ! expected future value function over exogenous grid
                    
                end do  ! end of asset loop

                ! value function on endogenous grid
                VF_endo=util_vec(cpol_endo) + psi_NN(t) + beta*EVFp
    
                ! upper envelope step
                aind_non = 0
                if (minval(astate(2:AA)-astate(1:AA-1)) < 0d0) then
                    print *, "astate not monotonically increasing before UEV step (NN)"
                    call UEVstep(astate,VF_endo,aind_non)
                endif
                
                ! number of non-eliminated gripoints
                AA_red=AA-sum(aind_non)
                if (AA_red/=AA) print *,(AA-AA_red)
                
                ! constructing reduced endogenous grid / policy correspondence
                allocate(astate_red(AA_red))
                allocate(apgrid_red(AA_red))                            
                astate_red=pack(astate,aind_non==0)
                apgrid_red=pack(assgrid,aind_non==0)
                if (minval(astate_red(2:AA_red)-astate_red(1:AA_red-1)) < 0d0) then
                    print *, "astate not monotonically increasing after UEV step (NN)"
                    stop
                endif
                
                ! interpolation to exogenous grid 
                call LinInterp_Extrap(AA_red,astate_red,apgrid_red,AA,assgrid,apol_NN(:,h,hsp,t))
                where (apol_NN(:,h,hsp,t) < assgrid(1)) apol_NN(:,h,hsp,t) = assgrid(1) ! adjusting for borrowing constraint
                
                deallocate(astate_red)
                deallocate(apgrid_red)
                
                ! consumption policy and value function
                cpol_NN(:,h,hsp,t) =  (1d0 + r) * assgrid - apol_NN(:,h,hsp,t)
                where (cpol_NN(:,h,hsp,t)<=1d-2) cpol_NN(:,h,hsp,t) = 1d-2
                call LinInterp_Extrap(AA,assgrid,EVFp,AA,apol_NN(:,h,hsp,t),EVFp_interp)
                VF_NN(:,h,hsp,t) = util_vec(cpol_NN(:,h,hsp,t)) + psi_NN(t) + beta*EVFp_interp
             
              
            end do    ! end of hhh loop
            
            
            ! end of parallelization
                        
            !$OMP END PARALLEL DO
            
            ! mirror policy functions for NU & EN & EU
           ! (mirroring policies only works if no systematic difference benefits / wages) 
           do hsp=1,HH   ! HC loop spouse
            
                do h=1,HH     ! HC loop head 
                
                cpol_NU(:,h,hsp,t) = cpol_UN(:,hsp,h,t)
                cpol_EN(:,h,hsp,t) = cpol_NE(:,hsp,h,t)
                cpol_EU(:,h,hsp,t) = cpol_UE(:,hsp,h,t)
                cpol_US(:,h,hsp,t) = cpol_SU(:,hsp,h,t)
                cpol_NS(:,h,hsp,t) = cpol_SN(:,hsp,h,t)
                cpol_ES(:,h,hsp,t) = cpol_SE(:,hsp,h,t)
                
                apol_NU(:,h,hsp,t) = apol_UN(:,hsp,h,t)
                apol_EN(:,h,hsp,t) = apol_NE(:,hsp,h,t)
                apol_EU(:,h,hsp,t) = apol_UE(:,hsp,h,t)
                apol_US(:,h,hsp,t) = apol_SU(:,hsp,h,t)
                apol_NS(:,h,hsp,t) = apol_SN(:,hsp,h,t)
                apol_ES(:,h,hsp,t) = apol_SE(:,hsp,h,t)
                
                VF_NU(:,h,hsp,t) = VF_UN(:,hsp,h,t)
                VF_EN(:,h,hsp,t) = VF_NE(:,hsp,h,t)
                VF_EU(:,h,hsp,t) = VF_UE(:,hsp,h,t)
                VF_US(:,h,hsp,t) = VF_SU(:,hsp,h,t)
                VF_NS(:,h,hsp,t) = VF_SN(:,hsp,h,t)
                VF_ES(:,h,hsp,t) = VF_SE(:,hsp,h,t)
                
                end do    ! end of HC head loop
            
           end do    ! end of HC spouse loop


            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !!! Iterate on Firm Value
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            do hhh=1,(HH*HH)   ! hhh loop
            
                h=jointhhh(1,hhh)
                hsp=jointhhh(2,hhh)
                
                ! if both employed, take expectation only over job loss of spouse
                EJ_asset = delta(t,hsp)*EJ_EE_EX_XB(h,hsp,:)+(1d0-delta(t,hsp))*EJ_EE_EE_XX(h,hsp,:)

                do a=1,AA ! asset loop -- asset choice on exogenous grid

                    call LinInterp1_Extrap(AA,assgrid,EJ_asset,apol_EE(a,h,hsp,t),EJ_help)

                    if ( (apol_EE(a,h,hsp,t).le.assgrid(AA)).AND.(EJ_help<0d0) ) then
                        print *, "Your solution seems to not solve the problem! (EE)", apol_EE(a,h,hsp,t), EJ_help, a,h,hsp,t
                    end if
                    EJ_help=max(EJ_help,0d0) ! adjustment to bound extrapolation
                    J_EE(a,h,hsp,t)  = prof(h) + (1d0/(1d0+r))*(1d0-delta(t,h))*EJ_help

                enddo


                ! if head employed and spouse unemployed with benefits, take expectation over job offer of spouse and potential loss of eligibility
                do a=1,AA ! asset loop -- asset choice on exogenous grid
                EJ_asset(a) = lambda_UE_E(a,hsp,h,t)*(phiUS*EJ_EX_EE_XX(h,hsp,a)+(1d0-phiUS)*EJ_EX_EE_XB(h,hsp,a))&
                        &+(1d0-lambda_UE_E(a,hsp,h,t))*(phiUS*EJ_EX_EX_XX(h,hsp,a)+(1d0-phiUS)*EJ_EX_EX_XB(h,hsp,a))
                enddo

                !print *, EJ_asset
                !print *, "next"

                do a=1,AA ! asset loop -- asset choice on exogenous grid

                    call LinInterp1_Extrap(AA,assgrid,EJ_asset,apol_EU(a,h,hsp,t),EJ_help)

                    if ( (apol_EU(a,h,hsp,t).le.assgrid(AA)).AND.(EJ_help<0d0) ) then
                        print *, "Your solution seems to not solve the problem! (EU)", apol_EU(a,h,hsp,t), EJ_help, a,h,hsp,t
                    end if
                    EJ_help=max(EJ_help,0d0) ! adjustment to bound extrapolation
                    J_EU(a,h,hsp,t)  = prof(h) + (1d0/(1d0+r))*(1d0-delta(t,h))*EJ_help
                enddo

                    
 
                

                ! if head employed and spouse S, take expectation only over job offer of spouse
                do a=1,AA ! asset loop -- asset choice on exogenous grid
                EJ_asset(a) = lambda_SE_E(a,hsp,h,t)*EJ_EX_EE_XX(h,hsp,a)+(1d0-lambda_SE_E(a,hsp,h,t))*EJ_EX_EX_XX(h,hsp,a)
                enddo

                do a=1,AA ! asset loop -- asset choice on exogenous grid

                    call LinInterp1_Extrap(AA,assgrid,EJ_asset,apol_ES(a,h,hsp,t),EJ_help)

                    if ( (apol_ES(a,h,hsp,t).le.assgrid(AA)).AND.(EJ_help<0d0) ) then
                        print *, "Your solution seems to not solve the problem! (ES)", apol_ES(a,h,hsp,t), EJ_help, a,h,hsp,t
                    end if
                    EJ_help=max(EJ_help,0d0) ! adjustment to bound extrapolation
                    J_ES(a,h,hsp,t)  = prof(h) + (1d0/(1d0+r))*(1d0-delta(t,h))*EJ_help

                enddo

                ! if head employed and spouse N, take expectation only over job offer of spouse
                do a=1,AA ! asset loop -- asset choice on exogenous grid
                EJ_asset(a) = lambda_NE_E(a,hsp,h,t)*EJ_EX_EE_XX(h,hsp,a)+(1d0-lambda_NE_E(a,hsp,h,t))*EJ_EX_EX_XX(h,hsp,a)
                enddo

                do a=1,AA ! asset loop -- asset choice on exogenous grid

                    call LinInterp1_Extrap(AA,assgrid,EJ_asset,apol_EN(a,h,hsp,t),EJ_help)

                    if ( (apol_EN(a,h,hsp,t).le.assgrid(AA)).AND.(EJ_help<0d0) ) then
                        print *, "Your solution seems to not solve the problem! (EN)", apol_EN(a,h,hsp,t), EJ_help, a,h,hsp,t
                    end if
                    EJ_help=max(EJ_help,0d0) ! adjustment to bound extrapolation
                    J_EN(a,h,hsp,t)  = prof(h) + (1d0/(1d0+r))*(1d0-delta(t,h))*EJ_help

                enddo
            enddo


            write(*,*) 'Solved period:', t
        end do    ! end of age loop
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!! Test consistency
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        test1=ANY((ABS(pi_EE_BB_EE(:,:,:,2:TT) + pi_EE_BB_EU(:,:,:,2:TT) + pi_EE_BB_UE(:,:,:,2:TT) + pi_EE_BB_EN(:,:,:,2:TT) + pi_EE_BB_NE(:,:,:,2:TT) + pi_EE_BB_UU(:,:,:,2:TT) + pi_EE_BB_UN(:,:,:,2:TT) + pi_EE_BB_NU(:,:,:,2:TT) + pi_EE_BB_NN(:,:,:,2:TT))-1d0)>0.0001d0)
        test2=ANY((ABS(pi_EX_BB_EU(:,:,:,2:TT) + pi_EX_BB_EN(:,:,:,2:TT) + pi_EX_BB_UU(:,:,:,2:TT) + pi_EX_BB_UN(:,:,:,2:TT) + pi_EX_BB_NU(:,:,:,2:TT) + pi_EX_BB_NN(:,:,:,2:TT))-1d0)>0.0001d0)
        test3=ANY((ABS(pi_XE_BB_UE(:,:,:,2:TT) + pi_XE_BB_NE(:,:,:,2:TT) + pi_XE_BB_UU(:,:,:,2:TT) + pi_XE_BB_UN(:,:,:,2:TT) + pi_XE_BB_NU(:,:,:,2:TT) + pi_XE_BB_NN(:,:,:,2:TT))-1d0)>0.0001d0)
        test4=ANY((ABS(pi_XX_BB_UU(:,:,:,2:TT) + pi_XX_BB_UN(:,:,:,2:TT) + pi_XX_BB_NU(:,:,:,2:TT) + pi_XX_BB_NN(:,:,:,2:TT))-1d0)>0.0001d0)
        test5=ANY((ABS(pi_EE_BX_EE(:,:,:,2:TT) + pi_EE_BX_ES(:,:,:,2:TT) + pi_EE_BX_UE(:,:,:,2:TT) + pi_EE_BX_EN(:,:,:,2:TT) + pi_EE_BX_NE(:,:,:,2:TT) + pi_EE_BX_US(:,:,:,2:TT) + pi_EE_BX_UN(:,:,:,2:TT) + pi_EE_BX_NS(:,:,:,2:TT) + pi_EE_BX_NN(:,:,:,2:TT))-1d0)>0.0001d0)
        test6=ANY((ABS(pi_EX_BX_ES(:,:,:,2:TT) + pi_EX_BX_EN(:,:,:,2:TT) + pi_EX_BX_US(:,:,:,2:TT) + pi_EX_BX_UN(:,:,:,2:TT) + pi_EX_BX_NS(:,:,:,2:TT) + pi_EX_BX_NN(:,:,:,2:TT))-1d0)>0.0001d0)
        test7=ANY((ABS(pi_XE_BX_UE(:,:,:,2:TT) + pi_XE_BX_NE(:,:,:,2:TT) + pi_XE_BX_US(:,:,:,2:TT) + pi_XE_BX_UN(:,:,:,2:TT) + pi_XE_BX_NS(:,:,:,2:TT) + pi_XE_BX_NN(:,:,:,2:TT))-1d0)>0.0001d0)
        test8=ANY((ABS(pi_XX_BX_US(:,:,:,2:TT) + pi_XX_BX_UN(:,:,:,2:TT) + pi_XX_BX_NS(:,:,:,2:TT) + pi_XX_BX_NN(:,:,:,2:TT))-1d0)>0.0001d0)
        test9=ANY((ABS(pi_EE_XB_EE(:,:,:,2:TT) + pi_EE_XB_EU(:,:,:,2:TT) + pi_EE_XB_SE(:,:,:,2:TT) + pi_EE_XB_EN(:,:,:,2:TT) + pi_EE_XB_NE(:,:,:,2:TT) + pi_EE_XB_SU(:,:,:,2:TT) + pi_EE_XB_SN(:,:,:,2:TT) + pi_EE_XB_NU(:,:,:,2:TT) + pi_EE_XB_NN(:,:,:,2:TT))-1d0)>0.0001d0)
        test10=ANY((ABS(pi_EX_XB_EU(:,:,:,2:TT) + pi_EX_XB_EN(:,:,:,2:TT) + pi_EX_XB_SU(:,:,:,2:TT) + pi_EX_XB_SN(:,:,:,2:TT) + pi_EX_XB_NU(:,:,:,2:TT) + pi_EX_XB_NN(:,:,:,2:TT))-1d0)>0.0001d0)
        test11=ANY((ABS(pi_XE_XB_SE(:,:,:,2:TT) + pi_XE_XB_NE(:,:,:,2:TT) + pi_XE_XB_SU(:,:,:,2:TT) + pi_XE_XB_SN(:,:,:,2:TT) + pi_XE_XB_NU(:,:,:,2:TT) + pi_XE_XB_NN(:,:,:,2:TT))-1d0)>0.0001d0)
        test12=ANY((ABS(pi_XX_XB_SU(:,:,:,2:TT) + pi_XX_XB_SN(:,:,:,2:TT) + pi_XX_XB_NU(:,:,:,2:TT) + pi_XX_XB_NN(:,:,:,2:TT))-1d0)>0.0001d0)
        test13=ANY((ABS(pi_EE_XX_EE(:,:,:,2:TT) + pi_EE_XX_ES(:,:,:,2:TT) + pi_EE_XX_SE(:,:,:,2:TT) + pi_EE_XX_EN(:,:,:,2:TT) + pi_EE_XX_NE(:,:,:,2:TT) + pi_EE_XX_SS(:,:,:,2:TT) + pi_EE_XX_SN(:,:,:,2:TT) + pi_EE_XX_NS(:,:,:,2:TT) + pi_EE_XX_NN(:,:,:,2:TT))-1d0)>0.0001d0)
        test14=ANY((ABS(pi_EX_XX_ES(:,:,:,2:TT) + pi_EX_XX_EN(:,:,:,2:TT) + pi_EX_XX_SS(:,:,:,2:TT) + pi_EX_XX_SN(:,:,:,2:TT) + pi_EX_XX_NS(:,:,:,2:TT) + pi_EX_XX_NN(:,:,:,2:TT))-1d0)>0.0001d0)
        test15=ANY((ABS(pi_XE_XX_SE(:,:,:,2:TT) + pi_XE_XX_NE(:,:,:,2:TT) + pi_XE_XX_SS(:,:,:,2:TT) + pi_XE_XX_SN(:,:,:,2:TT) + pi_XE_XX_NS(:,:,:,2:TT) + pi_XE_XX_NN(:,:,:,2:TT))-1d0)>0.0001d0)
        test16=ANY((ABS(pi_XX_XX_SS(:,:,:,2:TT) + pi_XX_XX_SN(:,:,:,2:TT) + pi_XX_XX_NS(:,:,:,2:TT) + pi_XX_XX_NN(:,:,:,2:TT))-1d0)>0.0001d0)

        testpi=(test1.or.test2.or.test3.or.test4.or.test5.or.test6.or.test7.or.test8.or.test9.or. & 
              test10.or.test11.or.test12.or.test13.or.test14.or.test15.or.test16)

        ! print *, test1
        ! print *, test2
        ! print *, test3
        ! print *, test4
        ! print *, test5
        ! print *, test6
        ! print *, test7
        ! print *, test8
        ! print *, test9
        ! print *, test10
        ! print *, test11
        ! print *, test12
        ! print *, test13
        ! print *, test14
        ! print *, test15
        ! print *, test16

       if(testpi) print *, 'There is an issue with the pi matrices!'


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!! Store results
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ! Store results from solving HH problem
        call store_hh_problem()

        ! Store results from solving firm problem
        call store_firm_problem()

        deallocate (VFstack9)
        deallocate (VFstack6)
        deallocate (VFstack4)

    end subroutine
    
    
    
    ! function for EGM step
    ! compute current consumption as a function of current states and future assets
    function RHS_EGM(a_p,indE_h,indE_sp,h_s,hsp_s,t_s,pb_EE, pb_EX, pb_XE, pb_XX, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp)
    
        use globals

        integer, intent(in) :: a_p, indE_h,indE_sp, h_s, hsp_s, t_s ! indices for asset CHOICE and CURRENT states
        real*8, intent(in) :: pb_EE, pb_EX, pb_XE, pb_XX ! joint PB that head/spouse has a job (offer) tomorrow
        real*8, intent(in) :: pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp ! PB of eligibility conditional on having / not having job (offer) tomorrow
        ! storage for first step: expectations over future labor choice
        real*8, dimension(HH,HH) :: up_EE_BB, up_EE_BX, up_EE_XB, up_EE_XX
        real*8, dimension(HH,HH) :: up_EX_BB, up_EX_BX, up_EX_XB, up_EX_XX, up_XE_BB, up_XE_BX, up_XE_XB, up_XE_XX
        real*8, dimension(HH,HH) :: up_XX_BB, up_XX_BX, up_XX_XB, up_XX_XX
        !  storage for second step: expectations over future HC and MQ
        real*8 :: Eup_EE_BB,Eup_EE_BX,Eup_EE_XB,Eup_EE_XX
        real*8 :: Eup_EX_BB, Eup_EX_BX, Eup_EX_XB, Eup_EX_XX
        real*8 :: Eup_XE_BB, Eup_XE_BX, Eup_XE_XB, Eup_XE_XX
        real*8 :: Eup_XX_BB, Eup_XX_BX, Eup_XX_XB, Eup_XX_XX 
        real*8, dimension(HH,HH) :: htrans
        ! third step: expectation over job offer/loss and eligibility = final function output 
        real*8 :: RHS_EGM 

        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! first step: taking expectation over future labor status choice
                
        ! first step - both have job (offer)
        ! both members eligible
        up_EE_BB(:,:)= pi_EE_BB_EE(a_p,:,:,t_s+1)*cpol_EE(a_p,:,:,t_s+1)**(-gam) + pi_EE_BB_EU(a_p,:,:,t_s+1)*cpol_EU(a_p,:,:,t_s+1)**(-gam) + pi_EE_BB_UE(a_p,:,:,t_s+1)*cpol_UE(a_p,:,:,t_s+1)**(-gam) &
                     + pi_EE_BB_EN(a_p,:,:,t_s+1)*cpol_EN(a_p,:,:,t_s+1)**(-gam) + pi_EE_BB_NE(a_p,:,:,t_s+1)*cpol_NE(a_p,:,:,t_s+1)**(-gam) + pi_EE_BB_UU(a_p,:,:,t_s+1)*cpol_UU(a_p,:,:,t_s+1)**(-gam) &
                     + pi_EE_BB_UN(a_p,:,:,t_s+1)*cpol_UN(a_p,:,:,t_s+1)**(-gam) + pi_EE_BB_NU(a_p,:,:,t_s+1)*cpol_NU(a_p,:,:,t_s+1)**(-gam) + pi_EE_BB_NN(a_p,:,:,t_s+1)*cpol_NN(a_p,:,:,t_s+1)**(-gam)
        ! only member 1 eligible
        up_EE_BX(:,:)= pi_EE_BX_EE(a_p,:,:,t_s+1)*cpol_EE(a_p,:,:,t_s+1)**(-gam) + pi_EE_BX_ES(a_p,:,:,t_s+1)*cpol_ES(a_p,:,:,t_s+1)**(-gam) + pi_EE_BX_UE(a_p,:,:,t_s+1)*cpol_UE(a_p,:,:,t_s+1)**(-gam) &
                     + pi_EE_BX_EN(a_p,:,:,t_s+1)*cpol_EN(a_p,:,:,t_s+1)**(-gam) + pi_EE_BX_NE(a_p,:,:,t_s+1)*cpol_NE(a_p,:,:,t_s+1)**(-gam) + pi_EE_BX_US(a_p,:,:,t_s+1)*cpol_US(a_p,:,:,t_s+1)**(-gam) &
                     + pi_EE_BX_UN(a_p,:,:,t_s+1)*cpol_UN(a_p,:,:,t_s+1)**(-gam) + pi_EE_BX_NS(a_p,:,:,t_s+1)*cpol_NS(a_p,:,:,t_s+1)**(-gam) + pi_EE_BX_NN(a_p,:,:,t_s+1)*cpol_NN(a_p,:,:,t_s+1)**(-gam)
        ! only member 2 eligible
        up_EE_XB(:,:)= pi_EE_XB_EE(a_p,:,:,t_s+1)*cpol_EE(a_p,:,:,t_s+1)**(-gam) + pi_EE_XB_EU(a_p,:,:,t_s+1)*cpol_EU(a_p,:,:,t_s+1)**(-gam) + pi_EE_XB_SE(a_p,:,:,t_s+1)*cpol_SE(a_p,:,:,t_s+1)**(-gam) &
                     + pi_EE_XB_EN(a_p,:,:,t_s+1)*cpol_EN(a_p,:,:,t_s+1)**(-gam) + pi_EE_XB_NE(a_p,:,:,t_s+1)*cpol_NE(a_p,:,:,t_s+1)**(-gam) + pi_EE_XB_SU(a_p,:,:,t_s+1)*cpol_SU(a_p,:,:,t_s+1)**(-gam) &
                     + pi_EE_XB_SN(a_p,:,:,t_s+1)*cpol_SN(a_p,:,:,t_s+1)**(-gam) + pi_EE_XB_NU(a_p,:,:,t_s+1)*cpol_NU(a_p,:,:,t_s+1)**(-gam) + pi_EE_XB_NN(a_p,:,:,t_s+1)*cpol_NN(a_p,:,:,t_s+1)**(-gam)
        ! no members eligible
        up_EE_XX(:,:)= pi_EE_XX_EE(a_p,:,:,t_s+1)*cpol_EE(a_p,:,:,t_s+1)**(-gam) + pi_EE_XX_ES(a_p,:,:,t_s+1)*cpol_ES(a_p,:,:,t_s+1)**(-gam) + pi_EE_XX_SE(a_p,:,:,t_s+1)*cpol_SE(a_p,:,:,t_s+1)**(-gam) &
                     + pi_EE_XX_EN(a_p,:,:,t_s+1)*cpol_EN(a_p,:,:,t_s+1)**(-gam) + pi_EE_XX_NE(a_p,:,:,t_s+1)*cpol_NE(a_p,:,:,t_s+1)**(-gam) + pi_EE_XX_SS(a_p,:,:,t_s+1)*cpol_SS(a_p,:,:,t_s+1)**(-gam) &
                     + pi_EE_XX_SN(a_p,:,:,t_s+1)*cpol_SN(a_p,:,:,t_s+1)**(-gam) + pi_EE_XX_NS(a_p,:,:,t_s+1)*cpol_NS(a_p,:,:,t_s+1)**(-gam) + pi_EE_XX_NN(a_p,:,:,t_s+1)*cpol_NN(a_p,:,:,t_s+1)**(-gam)
    
        ! first step - only head has job (offer)
        !both members eligible
        up_EX_BB(:,:)=  pi_EX_BB_EU(a_p,:,:,t_s+1)*cpol_EU(a_p,:,:,t_s+1)**(-gam) + pi_EX_BB_EN(a_p,:,:,t_s+1)*cpol_EN(a_p,:,:,t_s+1)**(-gam) + pi_EX_BB_UU(a_p,:,:,t_s+1)*cpol_UU(a_p,:,:,t_s+1)**(-gam) &
                      + pi_EX_BB_UN(a_p,:,:,t_s+1)*cpol_UN(a_p,:,:,t_s+1)**(-gam) + pi_EX_BB_NU(a_p,:,:,t_s+1)*cpol_NU(a_p,:,:,t_s+1)**(-gam) + pi_EX_BB_NN(a_p,:,:,t_s+1)*cpol_NN(a_p,:,:,t_s+1)**(-gam)
        !only member 1 eligible
        up_EX_BX(:,:)=  pi_EX_BX_ES(a_p,:,:,t_s+1)*cpol_ES(a_p,:,:,t_s+1)**(-gam) + pi_EX_BX_EN(a_p,:,:,t_s+1)*cpol_EN(a_p,:,:,t_s+1)**(-gam) + pi_EX_BX_US(a_p,:,:,t_s+1)*cpol_US(a_p,:,:,t_s+1)**(-gam) &
                      + pi_EX_BX_UN(a_p,:,:,t_s+1)*cpol_UN(a_p,:,:,t_s+1)**(-gam) + pi_EX_BX_NS(a_p,:,:,t_s+1)*cpol_NS(a_p,:,:,t_s+1)**(-gam) + pi_EX_BX_NN(a_p,:,:,t_s+1)*cpol_NN(a_p,:,:,t_s+1)**(-gam)
        !only member 2 eligible
        up_EX_XB(:,:)=  pi_EX_XB_EU(a_p,:,:,t_s+1)*cpol_EU(a_p,:,:,t_s+1)**(-gam) + pi_EX_XB_EN(a_p,:,:,t_s+1)*cpol_EN(a_p,:,:,t_s+1)**(-gam) + pi_EX_XB_SU(a_p,:,:,t_s+1)*cpol_SU(a_p,:,:,t_s+1)**(-gam) &
                      + pi_EX_XB_SN(a_p,:,:,t_s+1)*cpol_SN(a_p,:,:,t_s+1)**(-gam) + pi_EX_XB_NU(a_p,:,:,t_s+1)*cpol_NU(a_p,:,:,t_s+1)**(-gam) + pi_EX_XB_NN(a_p,:,:,t_s+1)*cpol_NN(a_p,:,:,t_s+1)**(-gam)
        !no member eligible
        up_EX_XX(:,:)=  pi_EX_XX_ES(a_p,:,:,t_s+1)*cpol_ES(a_p,:,:,t_s+1)**(-gam) + pi_EX_XX_EN(a_p,:,:,t_s+1)*cpol_EN(a_p,:,:,t_s+1)**(-gam) + pi_EX_XX_SS(a_p,:,:,t_s+1)*cpol_SS(a_p,:,:,t_s+1)**(-gam) &
                      + pi_EX_XX_SN(a_p,:,:,t_s+1)*cpol_SN(a_p,:,:,t_s+1)**(-gam) + pi_EX_XX_NS(a_p,:,:,t_s+1)*cpol_NS(a_p,:,:,t_s+1)**(-gam) + pi_EX_XX_NN(a_p,:,:,t_s+1)*cpol_NN(a_p,:,:,t_s+1)**(-gam)
        

        ! first step - only spouse has job (offer)
        !both members eligible
        up_XE_BB(:,:)= pi_XE_BB_UE(a_p,:,:,t_s+1)*cpol_UE(a_p,:,:,t_s+1)**(-gam) + pi_XE_BB_NE(a_p,:,:,t_s+1)*cpol_NE(a_p,:,:,t_s+1)**(-gam) + pi_XE_BB_UU(a_p,:,:,t_s+1)*cpol_UU(a_p,:,:,t_s+1)**(-gam) &
                     + pi_XE_BB_UN(a_p,:,:,t_s+1)*cpol_UN(a_p,:,:,t_s+1)**(-gam) + pi_XE_BB_NU(a_p,:,:,t_s+1)*cpol_NU(a_p,:,:,t_s+1)**(-gam) + pi_XE_BB_NN(a_p,:,:,t_s+1)*cpol_NN(a_p,:,:,t_s+1)**(-gam)
        !only member 1 eligible
        up_XE_BX(:,:)= pi_XE_BX_UE(a_p,:,:,t_s+1)*cpol_UE(a_p,:,:,t_s+1)**(-gam) + pi_XE_BX_NE(a_p,:,:,t_s+1)*cpol_NE(a_p,:,:,t_s+1)**(-gam) + pi_XE_BX_US(a_p,:,:,t_s+1)*cpol_US(a_p,:,:,t_s+1)**(-gam) &
                     + pi_XE_BX_UN(a_p,:,:,t_s+1)*cpol_UN(a_p,:,:,t_s+1)**(-gam) + pi_XE_BX_NS(a_p,:,:,t_s+1)*cpol_NS(a_p,:,:,t_s+1)**(-gam) + pi_XE_BX_NN(a_p,:,:,t_s+1)*cpol_NN(a_p,:,:,t_s+1)**(-gam)
        !only member 2 eligible
        up_XE_XB(:,:)= pi_XE_XB_SE(a_p,:,:,t_s+1)*cpol_SE(a_p,:,:,t_s+1)**(-gam) + pi_XE_XB_NE(a_p,:,:,t_s+1)*cpol_NE(a_p,:,:,t_s+1)**(-gam) + pi_XE_XB_SU(a_p,:,:,t_s+1)*cpol_SU(a_p,:,:,t_s+1)**(-gam) &
                     + pi_XE_XB_SN(a_p,:,:,t_s+1)*cpol_SN(a_p,:,:,t_s+1)**(-gam) + pi_XE_XB_NU(a_p,:,:,t_s+1)*cpol_NU(a_p,:,:,t_s+1)**(-gam) + pi_XE_XB_NN(a_p,:,:,t_s+1)*cpol_NN(a_p,:,:,t_s+1)**(-gam)
        !no member eligible
        up_XE_XX(:,:)= pi_XE_XX_SE(a_p,:,:,t_s+1)*cpol_SE(a_p,:,:,t_s+1)**(-gam) + pi_XE_XX_NE(a_p,:,:,t_s+1)*cpol_NE(a_p,:,:,t_s+1)**(-gam) + pi_XE_XX_SS(a_p,:,:,t_s+1)*cpol_SS(a_p,:,:,t_s+1)**(-gam) &
                     + pi_XE_XX_SN(a_p,:,:,t_s+1)*cpol_SN(a_p,:,:,t_s+1)**(-gam) + pi_XE_XX_NS(a_p,:,:,t_s+1)*cpol_NS(a_p,:,:,t_s+1)**(-gam) + pi_XE_XX_NN(a_p,:,:,t_s+1)*cpol_NN(a_p,:,:,t_s+1)**(-gam)
        
        ! first step - none has job (offer)
        !both members eligible
        up_XX_BB(:,:)= pi_XX_BB_UU(a_p,:,:,t_s+1)*cpol_UU(a_p,:,:,t_s+1)**(-gam) + pi_XX_BB_UN(a_p,:,:,t_s+1)*cpol_UN(a_p,:,:,t_s+1)**(-gam) &
                     + pi_XX_BB_NU(a_p,:,:,t_s+1)*cpol_NU(a_p,:,:,t_s+1)**(-gam) + pi_XX_BB_NN(a_p,:,:,t_s+1)*cpol_NN(a_p,:,:,t_s+1)**(-gam)
        !only member 1 eligible
        up_XX_BX(:,:)= pi_XX_BX_US(a_p,:,:,t_s+1)*cpol_US(a_p,:,:,t_s+1)**(-gam) + pi_XX_BX_UN(a_p,:,:,t_s+1)*cpol_UN(a_p,:,:,t_s+1)**(-gam) &
                     + pi_XX_BX_NS(a_p,:,:,t_s+1)*cpol_NS(a_p,:,:,t_s+1)**(-gam) + pi_XX_BX_NN(a_p,:,:,t_s+1)*cpol_NN(a_p,:,:,t_s+1)**(-gam)
        !only member 2 eligible
        up_XX_XB(:,:)= pi_XX_XB_SU(a_p,:,:,t_s+1)*cpol_SU(a_p,:,:,t_s+1)**(-gam) + pi_XX_XB_SN(a_p,:,:,t_s+1)*cpol_SN(a_p,:,:,t_s+1)**(-gam) &
                     + pi_XX_XB_NU(a_p,:,:,t_s+1)*cpol_NU(a_p,:,:,t_s+1)**(-gam) + pi_XX_XB_NN(a_p,:,:,t_s+1)*cpol_NN(a_p,:,:,t_s+1)**(-gam)
        !no member eligible
        up_XX_XX(:,:)= pi_XX_XX_SS(a_p,:,:,t_s+1)*cpol_SS(a_p,:,:,t_s+1)**(-gam) + pi_XX_XX_SN(a_p,:,:,t_s+1)*cpol_SN(a_p,:,:,t_s+1)**(-gam) &
                     + pi_XX_XX_NS(a_p,:,:,t_s+1)*cpol_NS(a_p,:,:,t_s+1)**(-gam) + pi_XX_XX_NN(a_p,:,:,t_s+1)*cpol_NN(a_p,:,:,t_s+1)**(-gam)
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! second step: taking expectation over future HC
        !initialize
        Eup_EE_BB = 0d0
        Eup_EX_BB = 0d0
        Eup_XE_BB = 0d0
        Eup_XX_BB = 0d0
        Eup_EE_BX = 0d0
        Eup_EX_BX = 0d0
        Eup_XE_BX = 0d0
        Eup_XX_BX = 0d0
        Eup_EE_XB = 0d0
        Eup_EX_XB = 0d0
        Eup_XE_XB = 0d0
        Eup_XX_XB = 0d0
        Eup_EE_XX = 0d0
        Eup_EX_XX = 0d0
        Eup_XE_XX = 0d0
        Eup_XX_XX = 0d0
        
                
        ! second step - both have job (offer)
        if (indE_h==0) then ! not employed today

            if (indE_sp==0) then ! lowest grid point already
                call exo_trans(3, 3, h_s,hsp_s, htrans)
            elseif (indE_sp==1) then ! depreciation of HC
                call exo_trans(3, 1, h_s,hsp_s, htrans)
            end if

        elseif (indE_h==1) then! employed today

            if (indE_sp==0) then ! lowest grid point already
                call exo_trans(1, 3, h_s,hsp_s, htrans)
            else ! depreciation of HC
                call exo_trans(1, 1, h_s,hsp_s, htrans)
            end if

        end if

        ! both members eligible 
        Eup_EE_BB=SUM(htrans*up_EE_BB)
        ! only member 1 eligible 
        Eup_EE_BX=SUM(htrans*up_EE_BX)
        ! only member 2 eligible 
        Eup_EE_XB=SUM(htrans*up_EE_XB)
        ! no member eligible 
        Eup_EE_XX=SUM(htrans*up_EE_XX)


        ! second step - only head has job (offer)
        if (indE_h==0) then ! not employed today

            if (indE_sp==0) then ! lowest grid point already
                call exo_trans(3, 3, h_s,hsp_s, htrans)
            elseif (indE_sp==1) then ! depreciation of HC
                call exo_trans(3, 2, h_s,hsp_s, htrans)
            end if

        elseif (indE_h==1) then! employed today

            if (indE_sp==0) then ! lowest grid point already
                call exo_trans(1, 3, h_s,hsp_s, htrans)
            else ! depreciation of HC
                call exo_trans(1, 2, h_s,hsp_s, htrans)
            end if

        end if
        
        ! both members eligible 
        Eup_EX_BB=SUM(htrans*up_EX_BB)
        ! only member 1 eligible 
        Eup_EX_BX=SUM(htrans*up_EX_BX)
        ! only member 2 eligible 
        Eup_EX_XB=SUM(htrans*up_EX_XB)
        ! no member eligible 
        Eup_EX_XX=SUM(htrans*up_EX_XX)


        ! second step - only spouse has job (offer)
        if (indE_h==0) then ! not employed today

            if (indE_sp==0) then ! lowest grid point already
                call exo_trans(3, 3, h_s,hsp_s, htrans)
            elseif (indE_sp==1) then ! depreciation of HC
                call exo_trans(3, 1, h_s,hsp_s, htrans)
            end if

        elseif (indE_h==1) then! employed today

            if (indE_sp==0) then ! lowest grid point already
                call exo_trans(2, 3, h_s,hsp_s, htrans)
            else ! depreciation of HC
                call exo_trans(2, 1, h_s,hsp_s, htrans)
            end if

        end if

        ! both members eligible
        Eup_XE_BB=SUM(htrans*up_XE_BB)
        ! only member 1 eligible
        Eup_XE_BX=SUM(htrans*up_XE_BX)
        ! only member 2 eligible
        Eup_XE_XB=SUM(htrans*up_XE_XB)
        ! no member eligible
        Eup_XE_XX=SUM(htrans*up_XE_XX)


        ! second step - none has job (offer)
        if (indE_h==0) then ! not employed today

            if (indE_sp==0) then ! lowest grid point already
                call exo_trans(3, 3, h_s,hsp_s, htrans)
            elseif (indE_sp==1) then ! depreciation of HC
                call exo_trans(3, 2, h_s,hsp_s, htrans)
            end if

        elseif (indE_h==1) then! employed today

            if (indE_sp==0) then ! lowest grid point already
                call exo_trans(2, 3, h_s,hsp_s, htrans)
            else ! depreciation of HC
                call exo_trans(2, 2, h_s,hsp_s, htrans)
            end if

        end if

        ! both members eligible
        Eup_XX_BB=SUM(htrans*up_XX_BB)
        ! only member 1 eligible
        Eup_XX_BX=SUM(htrans*up_XX_BX)
        ! only member 2 eligible
        Eup_XX_XB=SUM(htrans*up_XX_XB)
        ! no member eligible
        Eup_XX_XX=SUM(htrans*up_XX_XX)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                
        ! third step: taking expectation over future job offer / loss
        RHS_EGM = (beta*(1d0+r)*(pb_EE*(pb_Eh_Bh*pb_Esp_Bsp*Eup_EE_BB + (1d0-pb_Eh_Bh)*pb_Esp_Bsp*Eup_EE_XB + pb_Eh_Bh*(1d0-pb_Esp_Bsp)*Eup_EE_BX + (1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*Eup_EE_XX) &
                + pb_EX*(pb_Eh_Bh*pb_Xsp_Bsp*Eup_EX_BB + (1d0-pb_Eh_Bh)*pb_Xsp_Bsp*Eup_EX_XB + pb_Eh_Bh*(1d0-pb_Xsp_Bsp)*Eup_EX_BX + (1d0-pb_Eh_Bh)*(1d0-pb_Xsp_Bsp)*Eup_EX_XX) &
                + pb_XE*(pb_Xh_Bh*pb_Esp_Bsp*Eup_XE_BB + (1d0-pb_Xh_Bh)*pb_Esp_Bsp*Eup_XE_XB + pb_Xh_Bh*(1d0-pb_Esp_Bsp)*Eup_XE_BX + (1d0-pb_Xh_Bh)*(1d0-pb_Esp_Bsp)*Eup_XE_XX) &
                + pb_XX*(pb_Xh_Bh*pb_Xsp_Bsp*Eup_XX_BB + (1d0-pb_Xh_Bh)*pb_Xsp_Bsp*Eup_XX_XB + pb_Xh_Bh*(1d0-pb_Xsp_Bsp)*Eup_XX_BX+ (1d0-pb_Xh_Bh)*(1d0-pb_Xsp_Bsp)*Eup_XX_XX)&
                  ))**(-1d0/gam)
        
        
    end function RHS_EGM
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! expected value function household
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! computed as a function of current states and future assets
    function EVF(a_p,indE_h,indE_sp,h_s,hsp_s,t_s, pb_EE, pb_EX, pb_XE, pb_XX, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp)
    
        use globals

        integer, intent(in) :: a_p, indE_h,indE_sp, h_s, hsp_s, t_s ! indices for asset CHOICE and CURRENT states
        real*8, intent(in) :: pb_EE, pb_EX, pb_XE, pb_XX ! joint PB that head/spouse has a job (offer) tomorrow
        real*8, intent(in) :: pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp ! PB of eligibility conditional on having / not having job (offer) tomorrow
        ! storage for first step: expectations over future labor choice
        real*8, dimension(HH,HH) :: VFp_EE_BB, VFp_EE_BX, VFp_EE_XB, VFp_EE_XX
        real*8, dimension(HH,HH) :: VFp_EX_BB, VFp_EX_BX, VFp_EX_XB, VFp_EX_XX, VFp_XE_BB, VFp_XE_BX, VFp_XE_XB, VFp_XE_XX
        real*8, dimension(HH,HH) :: VFp_XX_BB, VFp_XX_BX, VFp_XX_XB, VFp_XX_XX
        !  storage for second step: expectations over future HC and MQ
        real*8 :: EVFp_EE_BB, EVFp_EE_BX, EVFp_EE_XB, EVFp_EE_XX
        real*8 :: EVFp_EX_BB, EVFp_EX_BX, EVFp_EX_XB, EVFp_EX_XX
        real*8 :: EVFp_XE_BB, EVFp_XE_BX, EVFp_XE_XB, EVFp_XE_XX
        real*8 :: EVFp_XX_BB, EVFp_XX_BX, EVFp_XX_XB, EVFp_XX_XX 
        real*8 :: EVF ! third step: expectation over job offer/loss = final function output 
        real*8, dimension(HH,HH) :: htrans

        real*8, dimension(HH,HH) :: corrfun! aux store used for comp of choice PBs
        real*8, dimension(HH,HH,9) :: VFstack9fun! aux store used for comp of choice PBs
        real*8, dimension(HH,HH,6) :: VFstack6fun! aux store used for comp of choice PBs
        real*8, dimension(HH,HH,4) :: VFstack4fun! aux store used for comp of choice PBs
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! first step: taking expectation over future labor status choice
                            
        ! first step - both have job (offer)

        !both members eligible
        ! preparing correction to avoid floating point exception
        VFstack9fun(:,:,1) = VF_EE(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,2) = VF_EU(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,3) = VF_UE(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,4) = VF_EN(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,5) = VF_NE(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,6) = VF_UU(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,7) = VF_UN(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,8) = VF_NU(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,9) = VF_NN(a_p,:,:,t_s+1)/sigma_eps
        corrfun=MAXVAL(VFstack9fun,3)
        
        VFp_EE_BB(:,:)= sigma_eps*(log(exp(VF_EE(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_EU(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_UE(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                     + exp(VF_EN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NE(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_UU(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                     + exp(VF_UN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NU(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NN(a_p,:,:,t_s+1)/sigma_eps-corrfun))+corrfun)
        
        !only member 1 eligible
        ! preparing correction to avoid floating point exception
        VFstack9fun(:,:,1) = VF_EE(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,2) = VF_ES(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,3) = VF_UE(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,4) = VF_EN(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,5) = VF_NE(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,6) = VF_US(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,7) = VF_UN(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,8) = VF_NS(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,9) = VF_NN(a_p,:,:,t_s+1)/sigma_eps
        corrfun=MAXVAL(VFstack9fun,3)
        
        VFp_EE_BX(:,:)= sigma_eps*(log(exp(VF_EE(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_ES(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_UE(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                     + exp(VF_EN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NE(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_US(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                     + exp(VF_UN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NS(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NN(a_p,:,:,t_s+1)/sigma_eps-corrfun))+corrfun)
        
        !only member 2 eligible
        ! preparing correction to avoid floating point exception
        VFstack9fun(:,:,1) = VF_EE(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,2) = VF_EU(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,3) = VF_SE(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,4) = VF_EN(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,5) = VF_NE(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,6) = VF_SU(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,7) = VF_SN(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,8) = VF_NU(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,9) = VF_NN(a_p,:,:,t_s+1)/sigma_eps
        corrfun=MAXVAL(VFstack9fun,3)
        
        VFp_EE_XB(:,:)= sigma_eps*(log(exp(VF_EE(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_EU(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_SE(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                     + exp(VF_EN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NE(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_SU(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                     + exp(VF_SN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NU(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NN(a_p,:,:,t_s+1)/sigma_eps-corrfun))+corrfun)
        
        !no member eligible
        ! preparing correction to avoid floating point exception
        VFstack9fun(:,:,1) = VF_EE(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,2) = VF_ES(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,3) = VF_SE(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,4) = VF_EN(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,5) = VF_NE(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,6) = VF_SS(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,7) = VF_SN(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,8) = VF_NS(a_p,:,:,t_s+1)/sigma_eps
        VFstack9fun(:,:,9) = VF_NN(a_p,:,:,t_s+1)/sigma_eps
        corrfun=MAXVAL(VFstack9fun,3)
        
        VFp_EE_XX(:,:)= sigma_eps*(log(exp(VF_EE(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_ES(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_SE(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                     + exp(VF_EN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NE(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_SS(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                     + exp(VF_SN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NS(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NN(a_p,:,:,t_s+1)/sigma_eps-corrfun))+corrfun)
        
        ! first step - only head has job (offer)

        !both members eligible
        ! preparing correction to avoid floating point exception
        VFstack6fun(:,:,1) = VF_EU(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,2) = VF_EN(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,3) = VF_UU(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,4) = VF_UN(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,5) = VF_NU(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,6) = VF_NN(a_p,:,:,t_s+1)/sigma_eps
        corrfun=MAXVAL(VFstack6fun,3)

        VFp_EX_BB(:,:)=  sigma_eps*(log(exp(VF_EU(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_EN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_UU(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                      + exp(VF_UN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NU(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NN(a_p,:,:,t_s+1)/sigma_eps-corrfun))+corrfun)
        
        !only member 1 eligible
        ! preparing correction to avoid floating point exception
        VFstack6fun(:,:,1) = VF_ES(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,2) = VF_EN(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,3) = VF_US(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,4) = VF_UN(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,5) = VF_NS(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,6) = VF_NN(a_p,:,:,t_s+1)/sigma_eps
        corrfun=MAXVAL(VFstack6fun,3)

        VFp_EX_BX(:,:)=  sigma_eps*(log(exp(VF_ES(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_EN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_US(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                      + exp(VF_UN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NS(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NN(a_p,:,:,t_s+1)/sigma_eps-corrfun))+corrfun)
        
        !only member 2 eligible
        ! preparing correction to avoid floating point exception
        VFstack6fun(:,:,1) = VF_EU(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,2) = VF_EN(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,3) = VF_SU(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,4) = VF_SN(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,5) = VF_NU(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,6) = VF_NN(a_p,:,:,t_s+1)/sigma_eps
        corrfun=MAXVAL(VFstack6fun,3)

        VFp_EX_XB(:,:)=  sigma_eps*(log(exp(VF_EU(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_EN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_SU(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                      + exp(VF_SN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NU(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NN(a_p,:,:,t_s+1)/sigma_eps-corrfun))+corrfun)
        
        !no member eligible
        ! preparing correction to avoid floating point exception
        VFstack6fun(:,:,1) = VF_ES(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,2) = VF_EN(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,3) = VF_SS(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,4) = VF_SN(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,5) = VF_NS(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,6) = VF_NN(a_p,:,:,t_s+1)/sigma_eps
        corrfun=MAXVAL(VFstack6fun,3)

        VFp_EX_XX(:,:)=  sigma_eps*(log(exp(VF_ES(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_EN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_SS(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                      + exp(VF_SN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NS(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NN(a_p,:,:,t_s+1)/sigma_eps-corrfun))+corrfun)
        

        ! first step - only spouse has job (offer)

        !both members eligible
        ! preparing correction to avoid floating point exception
        VFstack6fun(:,:,1) = VF_UE(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,2) = VF_NE(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,3) = VF_UU(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,4) = VF_UN(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,5) = VF_NU(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,6) = VF_NN(a_p,:,:,t_s+1)/sigma_eps
        corrfun=MAXVAL(VFstack6fun,3)
        
        VFp_XE_BB(:,:)= sigma_eps*(log(exp(VF_UE(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NE(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_UU(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                     + exp(VF_UN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NU(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NN(a_p,:,:,t_s+1)/sigma_eps-corrfun))+corrfun)
        
        !only member 1 eligible
        ! preparing correction to avoid floating point exception
        VFstack6fun(:,:,1) = VF_UE(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,2) = VF_NE(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,3) = VF_US(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,4) = VF_UN(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,5) = VF_NS(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,6) = VF_NN(a_p,:,:,t_s+1)/sigma_eps
        corrfun=MAXVAL(VFstack6fun,3)
        
        VFp_XE_BX(:,:)= sigma_eps*(log(exp(VF_UE(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NE(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_US(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                     + exp(VF_UN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NS(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NN(a_p,:,:,t_s+1)/sigma_eps-corrfun))+corrfun)
        
        !only member 2 eligible
        ! preparing correction to avoid floating point exception
        VFstack6fun(:,:,1) = VF_SE(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,2) = VF_NE(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,3) = VF_SU(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,4) = VF_SN(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,5) = VF_NU(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,6) = VF_NN(a_p,:,:,t_s+1)/sigma_eps
        corrfun=MAXVAL(VFstack6fun,3)
        
        VFp_XE_XB(:,:)= sigma_eps*(log(exp(VF_SE(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NE(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_SU(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                     + exp(VF_SN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NU(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NN(a_p,:,:,t_s+1)/sigma_eps-corrfun))+corrfun)
        
        !no member eligible
        ! preparing correction to avoid floating point exception
        VFstack6fun(:,:,1) = VF_SE(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,2) = VF_NE(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,3) = VF_SS(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,4) = VF_SN(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,5) = VF_NS(a_p,:,:,t_s+1)/sigma_eps
        VFstack6fun(:,:,6) = VF_NN(a_p,:,:,t_s+1)/sigma_eps
        corrfun=MAXVAL(VFstack6fun,3)
        
        VFp_XE_XX(:,:)= sigma_eps*(log(exp(VF_SE(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NE(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_SS(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                     + exp(VF_SN(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NS(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NN(a_p,:,:,t_s+1)/sigma_eps-corrfun))+corrfun)
        

    
        ! first step - none has job (offer)

        ! both members eligible
        ! preparing correction to avoid floating point exception
        VFstack4fun(:,:,1) = VF_UU(a_p,:,:,t_s+1)/sigma_eps
        VFstack4fun(:,:,2) = VF_UN(a_p,:,:,t_s+1)/sigma_eps
        VFstack4fun(:,:,3) = VF_NU(a_p,:,:,t_s+1)/sigma_eps
        VFstack4fun(:,:,4) = VF_NN(a_p,:,:,t_s+1)/sigma_eps
        corrfun=MAXVAL(VFstack4fun,3)

        VFp_XX_BB(:,:)= sigma_eps*(log(exp(VF_UU(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_UN(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                     + exp(VF_NU(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NN(a_p,:,:,t_s+1)/sigma_eps-corrfun))+corrfun)
            
        !only member 1 eligible
        ! preparing correction to avoid floating point exception
        VFstack4fun(:,:,1) = VF_US(a_p,:,:,t_s+1)/sigma_eps
        VFstack4fun(:,:,2) = VF_UN(a_p,:,:,t_s+1)/sigma_eps
        VFstack4fun(:,:,3) = VF_NS(a_p,:,:,t_s+1)/sigma_eps
        VFstack4fun(:,:,4) = VF_NN(a_p,:,:,t_s+1)/sigma_eps
        corrfun=MAXVAL(VFstack4fun,3)

        VFp_XX_BX(:,:)= sigma_eps*(log(exp(VF_US(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_UN(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                     + exp(VF_NS(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NN(a_p,:,:,t_s+1)/sigma_eps-corrfun))+corrfun)
            
        !only member 2 eligible
        ! preparing correction to avoid floating point exception
        VFstack4fun(:,:,1) = VF_SU(a_p,:,:,t_s+1)/sigma_eps
        VFstack4fun(:,:,2) = VF_SN(a_p,:,:,t_s+1)/sigma_eps
        VFstack4fun(:,:,3) = VF_NU(a_p,:,:,t_s+1)/sigma_eps
        VFstack4fun(:,:,4) = VF_NN(a_p,:,:,t_s+1)/sigma_eps
        corrfun=MAXVAL(VFstack4fun,3)

        VFp_XX_XB(:,:)= sigma_eps*(log(exp(VF_SU(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_SN(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                     + exp(VF_NU(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NN(a_p,:,:,t_s+1)/sigma_eps-corrfun))+corrfun)
         
        !no member eligible
        ! preparing correction to avoid floating point exception
        VFstack4fun(:,:,1) = VF_SS(a_p,:,:,t_s+1)/sigma_eps
        VFstack4fun(:,:,2) = VF_SN(a_p,:,:,t_s+1)/sigma_eps
        VFstack4fun(:,:,3) = VF_NS(a_p,:,:,t_s+1)/sigma_eps
        VFstack4fun(:,:,4) = VF_NN(a_p,:,:,t_s+1)/sigma_eps
        corrfun=MAXVAL(VFstack4fun,3)

        VFp_XX_XX(:,:)= sigma_eps*(log(exp(VF_SS(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_SN(a_p,:,:,t_s+1)/sigma_eps-corrfun) &
                                     + exp(VF_NS(a_p,:,:,t_s+1)/sigma_eps-corrfun) + exp(VF_NN(a_p,:,:,t_s+1)/sigma_eps-corrfun))+corrfun)
         

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! second step: taking expectation over future HC and MQ
        
        ! second step - both have job (offer)
        if (indE_h==0) then ! not employed today

            if (indE_sp==0) then ! lowest grid point already
                call exo_trans(3, 3, h_s,hsp_s, htrans)
            elseif (indE_sp==1) then ! depreciation of HC
                call exo_trans(3, 1, h_s,hsp_s, htrans)
            end if

        elseif (indE_h==1) then! employed today

            if (indE_sp==0) then ! lowest grid point already
                call exo_trans(1, 3, h_s,hsp_s, htrans)
            else ! depreciation of HC
                call exo_trans(1, 1, h_s,hsp_s, htrans)
            end if

        end if
                            
        !both members eligible
        EVFp_EE_BB=SUM(htrans*VFp_EE_BB)
        !only member 1 eligible
        EVFp_EE_BX=SUM(htrans*VFp_EE_BX)
        !only member 2 eligible
        EVFp_EE_XB=SUM(htrans*VFp_EE_XB)
        !no member eligible
        EVFp_EE_XX=SUM(htrans*VFp_EE_XX)


        ! second step - only head has job (offer)
        if (indE_h==0) then ! not employed today

            if (indE_sp==0) then ! lowest grid point already
                call exo_trans(3, 3, h_s,hsp_s, htrans)
            elseif (indE_sp==1) then ! depreciation of HC
                call exo_trans(3, 2, h_s,hsp_s, htrans)
            end if

        elseif (indE_h==1) then! employed today

            if (indE_sp==0) then ! lowest grid point already
                call exo_trans(1, 3, h_s,hsp_s, htrans)
            else ! depreciation of HC
                call exo_trans(1, 2, h_s,hsp_s, htrans)
            end if

        end if
        !both members eligible
        EVFp_EX_BB=SUM(htrans*VFp_EX_BB)
        !only member 1 eligible
        EVFp_EX_BX=SUM(htrans*VFp_EX_BX)
        !only member 2 eligible
        EVFp_EX_XB=SUM(htrans*VFp_EX_XB)
        !no member eligible
        EVFp_EX_XX=SUM(htrans*VFp_EX_XX)
        

        ! second step - only spouse has job (offer)
        if (indE_h==0) then ! not employed today

            if (indE_sp==0) then ! lowest grid point already
                call exo_trans(3, 3, h_s,hsp_s, htrans)
            elseif (indE_sp==1) then ! depreciation of HC
                call exo_trans(3, 1, h_s,hsp_s, htrans)
            end if

        elseif (indE_h==1) then! employed today

            if (indE_sp==0) then ! lowest grid point already
                call exo_trans(2, 3, h_s,hsp_s, htrans)
            else ! depreciation of HC
                call exo_trans(2, 1, h_s,hsp_s, htrans)
            end if

        end if

        !both members eligible
        EVFp_XE_BB=SUM(htrans*VFp_XE_BB)
        !only member 1 eligible
        EVFp_XE_BX=SUM(htrans*VFp_XE_BX)
        !only member 2 eligible
        EVFp_XE_XB=SUM(htrans*VFp_XE_XB)
        !no member eligible
        EVFp_XE_XX=SUM(htrans*VFp_XE_XX)
        

        ! second step - none has job (offer)
        if (indE_h==0) then ! not employed today

            if (indE_sp==0) then ! lowest grid point already
                call exo_trans(3, 3, h_s,hsp_s, htrans)
            elseif (indE_sp==1) then ! depreciation of HC
                call exo_trans(3, 2, h_s,hsp_s, htrans)
            end if

        elseif (indE_h==1) then! employed today

            if (indE_sp==0) then ! lowest grid point already
                call exo_trans(2, 3, h_s,hsp_s, htrans)
            else ! depreciation of HC
                call exo_trans(2, 2, h_s,hsp_s, htrans)
            end if

        end if

        !both members eligible
        EVFp_XX_BB=SUM(htrans*VFp_XX_BB)
        !only member 1 eligible
        EVFp_XX_BX=SUM(htrans*VFp_XX_BX)
        !only member 2 eligible
        EVFp_XX_XB=SUM(htrans*VFp_XX_XB)
        !no member eligible
        EVFp_XX_XX=SUM(htrans*VFp_XX_XX)

        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                
        ! third step: taking expectation over future job offer / loss
        EVF = pb_EE*(pb_Eh_Bh*pb_Esp_Bsp*EVFp_EE_BB + (1d0-pb_Eh_Bh)*pb_Esp_Bsp*EVFp_EE_XB + pb_Eh_Bh*(1d0-pb_Esp_Bsp)*EVFp_EE_BX + (1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*EVFp_EE_XX)&
            + pb_EX*(pb_Eh_Bh*pb_Xsp_Bsp*EVFp_EX_BB + (1d0-pb_Eh_Bh)*pb_Xsp_Bsp*EVFp_EX_XB + pb_Eh_Bh*(1d0-pb_Xsp_Bsp)*EVFp_EX_BX + (1d0-pb_Eh_Bh)*(1d0-pb_Xsp_Bsp)*EVFp_EX_XX)&
            + pb_XE*(pb_Xh_Bh*pb_Esp_Bsp*EVFp_XE_BB + (1d0-pb_Xh_Bh)*pb_Esp_Bsp*EVFp_XE_XB + pb_Xh_Bh*(1d0-pb_Esp_Bsp)*EVFp_XE_BX + (1d0-pb_Xh_Bh)*(1d0-pb_Esp_Bsp)*EVFp_XE_XX)&
            + pb_XX*(pb_Xh_Bh*pb_Xsp_Bsp*EVFp_XX_BB + (1d0-pb_Xh_Bh)*pb_Xsp_Bsp*EVFp_XX_XB + pb_Xh_Bh*(1d0-pb_Xsp_Bsp)*EVFp_XX_BX + (1d0-pb_Xh_Bh)*(1d0-pb_Xsp_Bsp)*EVFp_XX_XX)
        
        
    end function EVF
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! transition probabilities for human capital
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! computed as a function of current states and future assets
    subroutine exo_trans(indE_h, indE_sp, h_s,hsp_s, htrans)
    
        use globals

        integer, intent(in) :: indE_h, indE_sp, h_s, hsp_s ! indices for employment status and CURRENT human capital
        real*8, intent(out) :: htrans(HH,HH) ! PBs of future HC states
        real*8 :: htrans_h(1,HH), htrans_sp(1,HH)
        
        ! find future human capital grid points
        ! future HC of head
        if (indE_h==1) then ! EE
            htrans_h(1,:) = htrans_EE(h_s,:)
        elseif (indE_h==2) then ! EX
            htrans_h(1,:) = htrans_EX(h_s,:)
        elseif (indE_h==3) then ! XX
            htrans_h(1,:) = htrans_XX(h_s,:)
        end if
        ! future HC of spouse
        if (indE_sp==1) then ! EE
            htrans_sp(1,:) = htrans_EE(hsp_s,:)
        elseif (indE_sp==2) then ! EX
            htrans_sp(1,:) = htrans_EX(hsp_s,:)
        elseif (indE_sp==3) then ! XX
            htrans_sp(1,:) = htrans_XX(hsp_s,:)
        end if

        htrans = MATMUL(TRANSPOSE(htrans_h),htrans_sp)

        ! print *, "htrans_h= ", htrans_h
        ! print *, "htrans_sp= ", htrans_sp
        ! print *, "htrans= ", htrans
        
    end subroutine exo_trans








    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Upper Envelope step
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    subroutine UEVstep(astate_UEV,VF_UEV,aind_elim)
        
        use Globals
        use Interpolation_Routines
        
        ! main variables
        real*8, dimension(AA), intent(in) :: astate_UEV, VF_UEV ! original endogenous grid and corresponding value function
        integer, dimension(AA), intent(out) :: aind_elim ! contains 1 at all endogenous gridpoints that need to be eliminated
        
        ! aux variables to test non-monotonicities
        integer, allocatable :: monaux(:), aind_mon(:), aind_monaux(:)
        integer, dimension(AA) :: aind_elim_new
        integer :: sum_monaux
        integer :: j, k, ap, app, ahat, AAhat, a        
        ! segmented value functions
        real*8, allocatable :: VFseg1(:), VFseg2(:), VFseg3(:), VFseg1_interp(:), VFseg2_interp(:), &
                        VFseg3_interp(:), astate_elim(:), VF_elim(:), VFseg2_mirr(:), aseg2_mirr(:)
                                
        ! grid to interpolate when constructing upper envelope
        real*8, allocatable :: astate_interp(:), astate_interp1(:), astate_interp2(:), astate_interp3(:)
        integer, allocatable :: ind_invert(:), ind_sort(:),  seg1_max(:), seg2_max(:), seg3_max(:)
        
        
        !print *, 'The UEVstep was here!'
        
        aind_elim = 0 ! keep track of which endogenous gridpoints are eliminated (this is size AA)
        do a=1,(AA-1) ! rewrite the index to account for eliminated gridpoints
            
            AAhat = AA - sum(aind_elim) ! number of non-eliminated gridpoints
            ahat = a - sum(aind_elim(1:a-1)) ! position of current a on reduced grid
            ! reduce grid and VF to not yet eliminated points
            allocate (astate_elim(AAhat))
            allocate (VF_elim(AAhat))
            astate_elim = pack(astate_UEV,aind_elim==0) ! non eliminated gridpoints
            VF_elim = pack(VF_UEV,aind_elim==0) ! VF at non eliminated gridpoints
            
             !print *,a
             !print *,AAhat
             !print *,aind_elim
            
            
            ! auxiliary storages
            allocate (monaux(AAhat)) ! iindex whether gridpoint is in overlapping region
            allocate (aind_mon(AAhat)) ! index which points have to be newly eliminated
            
            if (aind_elim(a)==0) then
            if (astate_elim(ahat)>astate_elim(ahat+1)) then
                ! print *,1
                ! find upper bound of current non-monotonous region (ap) [lowest point of next monotonous region; highest point of current non-monotonous region]
                ap=ahat+1                                                       ! at least one above ahat
                if (ap<AAhat) then
                do while ((astate_elim(ap)>astate_elim(ap+1)).and.(ap<AAhat))
                    ap=ap+1                                                     ! increase ap as long as astate_elim is decreasing
                end do
                endif
                !print *, 'ap = ', ap
                ! find upper bound of next monotonous segment [no counterpart in paper?]
                app = ap
                if (app < AAhat) then
                    do while (astate_elim(app+1)>astate_elim(app))
                        app=app+1
                        if (app == AAhat) then
                            exit
                        endif
                    end do
                endif
                !print *, 'app = ', app
                
                ! identify all gridpoints in overlapping region
                monaux=0
                do k = 1,app
                    if ((astate_elim(k)>=astate_elim(ap)).and.(astate_elim(k)<=astate_elim(ahat)))  monaux(k)=1
                end do
                sum_monaux = sum(monaux)
                !print *, 'monaux = ', monaux
                
                allocate (VFseg1(ahat))
                allocate (VFseg2(ap-ahat+1))
                allocate (VFseg2_mirr(ap-ahat+1))
                allocate (aseg2_mirr(ap-ahat+1))
                allocate (VFseg3(app-ap+1))
                allocate (VFseg1_interp(sum_monaux))
                allocate (VFseg2_interp(sum_monaux))
                allocate (VFseg3_interp(sum_monaux))
                allocate (astate_interp(sum_monaux))
                allocate (ind_sort(sum_monaux))
                allocate (aind_monaux(sum_monaux))
                allocate (ind_invert(sum_monaux))
                allocate (astate_interp1(sum_monaux))
                allocate (astate_interp2(sum_monaux))
                allocate (astate_interp3(sum_monaux)) 
                allocate (seg1_max(sum_monaux))
                allocate (seg2_max(sum_monaux))
                allocate (seg3_max(sum_monaux))
                
                ! define segmented value function
                VFseg1 = VF_elim(1:ahat)
                VFseg2 = VF_elim(ahat:ap)
                VFseg3 = VF_elim(ap:app)

                
                ! construct vector to interpolate on
                astate_interp = pack(astate_elim,monaux==1)
                !print *, 'astate_interp (check pack) = ', astate_interp
                
                ! dummy variables indicating the segement that the endogenous grid point belongs to
                
                astate_interp1 = 0
                astate_interp2 = 0
                astate_interp3 = 0 
                                
                ! is this the most efficient way to find those?
                do k=1,ahat
                    do j=1,sum_monaux
                         if(astate_elim(k) == astate_interp(j)) astate_interp1(j) = 1
                    end do 
                end do
                
                do k=ahat,ap
                    do j=1,sum_monaux
                         if(astate_elim(k) == astate_interp(j)) astate_interp2(j) = 1
                    end do 
                end do
                                
                 do k=ap,app
                    do j=1,sum_monaux
                         if(astate_elim(k) == astate_interp(j)) astate_interp3(j) = 1
                    end do 
                end do
                                
                ! sort in ascending order
                ind_sort = 0
                call hpsort_eps_epw(sum_monaux,astate_interp,ind_sort,1d-16)
                !print *, 'astate_interp = ', astate_interp
                !print *, 'ind_sort = ', ind_sort
                
                astate_interp1 = astate_interp1(ind_sort) 
                astate_interp2 = astate_interp2(ind_sort)
                astate_interp3 = astate_interp3(ind_sort)
                
                ind_invert = (/ (j, j=1,sum_monaux) /)
                ind_invert = ind_invert(ind_sort)
                                
                ! sort second segment in increasing order
                do j=0,(ap-ahat)
                    aseg2_mirr(j+1)=astate_elim(ap-j)
                    VFseg2_mirr(j+1)=VFseg2(ap-ahat+1-j)
                end do
                
                ! print *, ahat
                ! print *, ap
                ! print *, app
                !  print *, astate_elim(1:ahat)
                !  print *,VFseg1
                !  print *, aseg2_mirr
                !  print *,VFseg2_mirr
                !  print *, astate_elim(ap:app)
                ! print *,VFseg3
                
                ! interpolate segments 
                if (ahat==1) then
                    VFseg1_interp=VFseg1(1)
                else
                    call LinInterp_Extrap(ahat,astate_elim(1:ahat),VFseg1,sum_monaux,astate_interp,VFseg1_interp)
                endif
                call LinInterp_Extrap(ap-ahat+1,aseg2_mirr,VFseg2_mirr,sum_monaux,astate_interp,VFseg2_interp)
                
                if (app<=ap) then
                    VFseg3_interp=VFseg3(1)
                else
                    call LinInterp_Extrap(app-ap+1,astate_elim(ap:app),VFseg3,sum_monaux,astate_interp,VFseg3_interp)
                endif
                
                 ! print *, astate_interp
                 ! print *,VFseg1_interp
                 ! print *,VFseg2_interp
                 ! print *,VFseg3_interp
                
                ! for region astate_interp, find highest VF across segments
                seg1_max = 0 
                seg2_max = 0
                seg3_max = 0
                
                where (VFseg1_interp >= VFseg2_interp .and. VFseg1_interp >= VFseg3_interp) seg1_max = 1
                where (VFseg2_interp >= VFseg1_interp .and. VFseg2_interp >= VFseg3_interp) seg2_max = 1
                where (VFseg3_interp >= VFseg1_interp .and. VFseg3_interp >= VFseg2_interp) seg3_max = 1
                
                !print *, 'astate_interp = ', astate_interp
                !print *, 'VFseg1_interp = ', VFseg1_interp
                !print *, 'VFseg2_interp = ', VFseg2_interp
                !print *, 'VFseg3_interp = ', VFseg3_interp
            
                ! find index of eliminated elements on the original (not-sorted) astate_interp
                do k=1,sum_monaux
                    if (astate_interp1(k) == 1 .and. seg1_max(k) == 0) then
                        aind_monaux(ind_invert(k)) = 1  
                    elseif (astate_interp2(k) == 1 .and. seg2_max(k) == 0) then
                        aind_monaux(ind_invert(k)) = 1 
                    elseif (astate_interp3(k) == 1 .and. seg3_max(k) == 0) then
                        aind_monaux(ind_invert(k)) = 1 
                    end if 
                end do
                
                ! translate into indices on the astate_elim grid
                aind_mon = 0 ! becomes 1 if a point on current astate_elim has to be newly eliminated
                j=1
                do k=1,app ! loop over latest grid
                    if (monaux(k) == 1) then ! check if gridpoint was in overlapping region --> potentially eliminated
                        aind_mon(k) = aind_monaux(j) ! set aind_mon to one if newly eliminated, else stays zero 
                        j=j+1 ! keeping track which of the overlapping points we are at
                    end if
                end do
                
                deallocate (seg1_max)
                deallocate (seg2_max)
                deallocate (seg3_max)
                deallocate (ind_sort)
                deallocate (aind_monaux)
                deallocate (ind_invert)
                deallocate (astate_interp1)
                deallocate (astate_interp2)
                deallocate (astate_interp3) 
                deallocate (astate_interp)
                deallocate (VFseg1)
                deallocate (VFseg2)
                deallocate (VFseg2_mirr)
                deallocate (aseg2_mirr)
                deallocate (VFseg3)
                deallocate (VFseg1_interp)
                deallocate (VFseg2_interp)
                deallocate (VFseg3_interp)
                
                ! adjust the index of already eliminated elements (aind_elim)
                aind_elim_new = aind_elim ! might be unneccessary?
                j = sum(aind_elim(1:a-1)) !how many points previously eliminated --> k+j is index on aind_elim
                do k=1,AAhat
                    if (astate_elim(k) <= astate_elim(ahat) .AND. astate_elim(k) >= astate_elim(ap)) then
                    if (aind_elim(k+j)==0) then ! not eliminated in previous rounds                         
                        if (aind_mon(k) == 1) then ! if newly eliminated, set indicator to one
                            aind_elim_new(k+j) = 1
                        end if
                    end if
                    endif
                end do
                aind_elim = aind_elim_new !update index of eliminated gridpoints
            end if
            end if
            
            
            
            deallocate (astate_elim)
            deallocate (VF_elim)
            deallocate (monaux)
            deallocate (aind_mon)
            
            !print *, 'End of loop a = ', a
            
        end do
        
    end subroutine UEVstep
    
    ! probability of finding a job (conditional on "searching")
    function ptheta(theta)
        use globals

        implicit none
        real*8, intent(in) :: theta
        real*8 :: ptheta
        
        ptheta = min(xi * (theta**(1d0-alpha)),1d0)

    end function ptheta

    ! matches per vacancy
    function qtheta(theta)
        use globals

        implicit none
        real*8, intent(in) :: theta
        real*8 :: qtheta
        
        qtheta = min(xi * (theta**(-alpha)), 1d0/theta)

    end function qtheta

    ! matches per vacancy
    subroutine find_theta(lambda_1, lambda_2,EJ_EE_1,EJ_EX_1,EJ_EE_2,EJ_EX_2,theta_1, theta_2)
        
        use globals

        implicit none
        real*8, intent(in) :: lambda_1, lambda_2,EJ_EE_1,EJ_EX_1,EJ_EE_2,EJ_EX_2
        real*8, intent(out):: theta_1, theta_2
        real*8 :: tol, err, t1_up, t1_low, q_2
        integer :: maxit, it
    
        tol = 1d-12
        maxit = 300
        t1_up = 100d0*(1d0/xi)**(1d0/(1d0-alpha)) ! arbirtary multiple of market tightness implying p(theta)=1
        t1_low = 0d0
    
        loopit: do it = 1, maxit
    
            theta_1 = 0.5d0*(t1_up+t1_low)
            
            ! version with no restrictionns on maximum number of matches
            ! theta_2 = ( (kappa/xi)/(lambda_1*xi*theta_1**(1d0-alpha)*EJ_EE_2 + (1d0-lambda_1*xi*theta_1**(1d0-alpha))*EJ_EX_2) )**(-1d0/alpha)
            ! err = xi*theta_1**(-alpha) * ( lambda_2*xi*theta_2**(1d0-alpha)*EJ_EE_1 + (1d0-lambda_2*xi*theta_2**(1d0-alpha))*EJ_Ex_1) - kappa
    
            ! version restricting maximum number of matches to number of unemployed: m(u,v)=min(xi*u^(alpha)v^(1-alpha),u)
            q_2=kappa/(lambda_1*min(xi*theta_1**(1d0-alpha),1d0)*EJ_EE_2 + (1d0-lambda_1*min(xi*theta_1**(1d0-alpha),1d0))*EJ_EX_2) ! get vacancy filling rate clearing free entry for firm of spouse for given theta1
            !invert, xi**(1d0/(1d0-alpha)) is threshold for which part of the min operator is the relevant one
            if (q_2>xi**(1d0/(1d0-alpha))) then
                theta_2 = (q_2/xi)**(-1d0/alpha)
            else
                theta_2 = 1d0/q_2
            end if
            err = min(xi*theta_1**(-alpha),1d0/theta_1) * ( lambda_2*min(xi*theta_2**(1d0-alpha),1d0)*EJ_EE_1 + (1d0-lambda_2*min(xi*theta_2**(1d0-alpha),1d0))*EJ_Ex_1) - kappa
            
            ! update guess for theta1
            if (abs(err) > tol ) then
                if ( err > 0d0 ) then
                    t1_low = theta_1
                else
                    t1_up = theta_1
                end if
            else
                exit loopit
            endif
            
            if (it==maxit) then
                if (theta_1<=1e-12) then
                    write (*,*) "Maximum number of iterations reached.  But theta_1 =", theta_1," and theta_2 =", theta_2
                    write (*,*) "EJ_EE_1 =", EJ_EE_1, "     EJ_EX_1 =", EJ_EX_1, "    EJ_EE_2 =", EJ_EE_2, "    EJ_EX_2 =", EJ_EX_2
                else
                    write (*,*) "Maximum number of iterations reached.  Gap to upper bound:", theta_1-100d0*(1d0/xi)**(1d0/(1d0-alpha))
                    write (*,*) "EJ_EE_1 =", EJ_EE_1, "     EJ_EX_1 =", EJ_EX_1, "    EJ_EE_2 =", EJ_EE_2, "    EJ_EX_2 =", EJ_EX_2
                    stop
                endif
            endif
            
        end do loopit
        
    end subroutine find_theta
    
end module
