!#######################################################################
! Module Globals
! Define parameters and declare global variables
!#######################################################################

module Globals

    implicit none
    
    
    ! 1) loops & state space
    
    ! age (same for both spouses)
    integer, parameter :: TT = 480, TD=600
    
    ! labor market states (joint)
    integer, parameter :: JK = 16
    
    ! human capital (same process for both)
    integer, parameter :: HH = 12
    
    ! assets (joint)
    integer, parameter :: AA = 50
    
    ! number of HHs in simulation
    integer, parameter :: NN = 160000
    
    ! do-loops
    integer :: n, t


    ! 2) preference parameters 
    
    ! Risk aversion, discount factor
    real*8, parameter :: gam = 1.75d0      
    real*8 :: beta 

    ! (Dis-)utility of search/work
    real*8:: eps1, eps2, eps3, psi_search, psi_scale
    
    real*8, dimension(TT) :: psi_EE, psi_EU, psi_UE, psi_EN, psi_NE
    real*8, dimension(TT) :: psi_UU, psi_UN, psi_NU, psi_NN
    real*8, dimension(TT) :: psi_SS, psi_SE, psi_ES, psi_SN, psi_NS, psi_SU, psi_US
    

    ! 3) Extreme Value Shocks
    
    real*8, parameter :: sigma_eps=0.1d0
    
    real*8, parameter :: corr_bmin = -1000d0, corr_bmax = 1000d0 ! bound on how much we correct the VF for extreme value shocks 
        

    ! 4) arrival rates, separation probability
    
    ! separation probabilities
    real*8, parameter :: delta_lev = 0.05d0, delta_exp = -0.65d0
    real*8, dimension(TT,HH) :: delta 
    
    ! arrival rates
    real*8 :: lambda_u, lambda_n, lambda_s         
    real*8, dimension(AA,HH,HH,TT) :: lambda_UE_E, lambda_NE_E, lambda_SE_E, lambda_UE_X, lambda_NE_X, lambda_SE_X
    real*8, dimension(AA,HH,HH,TT) ::  lambda_UU, lambda_UN, lambda_NU, lambda_NN
    real*8, dimension(AA,HH,HH,TT) ::  lambda_SS, lambda_SU, lambda_US, lambda_SN, lambda_NS


    ! 5) wages + UI benefits for head and spouse
    
    ! 1 = $10000

    ! wages
    real*8, dimension(HH)   :: wage
    
    ! benefits
    real*8, dimension(HH)   :: ben
    real*8, parameter :: ben_rep = 0.5d0
    real*8, parameter :: phiUS = 1d0/6d0 ! PB of losing benefits
    

    ! 6) social security taxes and pensions
    
    real*8, parameter :: tau_ss = 0.0d0     ! not used in the end, but coded up, so we declare it
    real*8, dimension(HH,HH) :: pen
    real*8, parameter :: pen_rep = 0d0      ! not used in the end, but kept to not mess up ordering of parameters

    ! 7) human capital formation
    
    ! probabilities of moving up or down
    real*8, parameter ::  phidown_exp = 0d0, phidown_emp_exp = 0d0
    real*8 :: phidown_lev, phiup_lev, phiup_exp, phidown_emp_lev
    real*8, dimension(HH) :: phiup, phidown, phidown_emp 
    
    ! grid (bounds)
    real*8, parameter  :: hmin = 0.1d0, hmax = 1.2d0
    real*8, dimension(HH) :: hgrid

    ! transition matrices
    real*8, dimension(HH,HH) :: htrans_EE, htrans_EX, htrans_XX
    
    ! initial distribution over HC states
    
    real*8, dimension(HH) :: HC_dist


    ! 8) asset grid
    
    real*8, parameter :: Amin = 0d0
    real*8, parameter :: Amax = 100d0
    real*8 :: assgrid(AA)

    ! 9) asset return
    
    real*8, parameter :: r = 0.0017d0
    
    
    ! 10) Value Functions and Optimization
    
    ! Efficiency: Outer loop last entry in matrix, the longer loops should be inner loops 
    
    real*8, dimension(AA,HH,HH,TT) :: VF_EE 
    
    real*8, dimension(AA,HH,HH,TT) :: VF_EU, VF_EN, VF_ES
    
    real*8, dimension(AA,HH,HH,TT) :: VF_UE, VF_NE, VF_SE
    
    real*8, dimension(AA,HH,HH,TT) ::  VF_UU, VF_UN, VF_NU, VF_NN

    real*8, dimension(AA,HH,HH,TT) ::  VF_SS, VF_SU, VF_US, VF_SN, VF_NS
    

    ! 11) Value Function during retirement 
    
    real*8, dimension(AA,HH,HH,TD-TT) :: VF_R, cpol_R, apol_R
    real*8, dimension(AA,HH,HH) :: dVF_R

    
    ! 12) Policy Functions
    
    real*8, dimension(AA,HH,HH,TT) :: cpol_EE, apol_EE
    
    real*8, dimension(AA,HH,HH,TT) :: cpol_EU, cpol_EN, apol_EU, apol_EN, cpol_ES, apol_ES
     
    real*8, dimension(AA,HH,HH,TT) :: cpol_UE, cpol_NE, apol_UE, apol_NE, cpol_SE, apol_SE
    
    real*8, dimension(AA,HH,HH,TT) :: cpol_UU, cpol_UN, cpol_NU, cpol_NN, &
                                      apol_UU, apol_UN,  apol_NU, apol_NN

    real*8, dimension(AA,HH,HH,TT) :: cpol_SS, cpol_SU, cpol_US, cpol_SN, cpol_NS, &
                                      apol_SS, apol_SU, apol_US, apol_SN, apol_NS
    
                                      
    ! 13) decision probabilities
    
    ! both eligible
    real*8, dimension(AA,HH,HH,TT) :: pi_EE_BB_EE, pi_EE_BB_EU, pi_EE_BB_UE, pi_EE_BB_EN, pi_EE_BB_NE, pi_EE_BB_UU, pi_EE_BB_UN, pi_EE_BB_NU, pi_EE_BB_NN
    
    real*8, dimension(AA,HH,HH,TT) :: pi_EX_BB_EU, pi_EX_BB_EN, pi_EX_BB_UU, pi_EX_BB_UN, pi_EX_BB_NU, pi_EX_BB_NN
    
    real*8, dimension(AA,HH,HH,TT) :: pi_XE_BB_UE, pi_XE_BB_NE, pi_XE_BB_UU, pi_XE_BB_UN, pi_XE_BB_NU, pi_XE_BB_NN
    
    real*8, dimension(AA,HH,HH,TT) ::  pi_XX_BB_UU, pi_XX_BB_UN, pi_XX_BB_NU, pi_XX_BB_NN
    
    ! 1 eligible
    real*8, dimension(AA,HH,HH,TT) :: pi_EE_BX_EE, pi_EE_BX_ES, pi_EE_BX_UE, pi_EE_BX_EN, pi_EE_BX_NE, pi_EE_BX_US, pi_EE_BX_UN, pi_EE_BX_NS, pi_EE_BX_NN
    
    real*8, dimension(AA,HH,HH,TT) :: pi_EX_BX_ES, pi_EX_BX_EN, pi_EX_BX_US, pi_EX_BX_UN, pi_EX_BX_NS, pi_EX_BX_NN
    
    real*8, dimension(AA,HH,HH,TT) :: pi_XE_BX_UE, pi_XE_BX_NE, pi_XE_BX_US, pi_XE_BX_UN, pi_XE_BX_NS, pi_XE_BX_NN
    
    real*8, dimension(AA,HH,HH,TT) ::  pi_XX_BX_US, pi_XX_BX_UN, pi_XX_BX_NS, pi_XX_BX_NN
    
    ! 2 eligible
    real*8, dimension(AA,HH,HH,TT) :: pi_EE_XB_EE, pi_EE_XB_EU, pi_EE_XB_SE, pi_EE_XB_EN, pi_EE_XB_NE, pi_EE_XB_SU, pi_EE_XB_SN, pi_EE_XB_NU, pi_EE_XB_NN
    
    real*8, dimension(AA,HH,HH,TT) :: pi_EX_XB_EU, pi_EX_XB_EN, pi_EX_XB_SU, pi_EX_XB_SN, pi_EX_XB_NU, pi_EX_XB_NN
    
    real*8, dimension(AA,HH,HH,TT) :: pi_XE_XB_SE, pi_XE_XB_NE, pi_XE_XB_SU, pi_XE_XB_SN, pi_XE_XB_NU, pi_XE_XB_NN
    
    real*8, dimension(AA,HH,HH,TT) ::  pi_XX_XB_SU, pi_XX_XB_SN, pi_XX_XB_NU, pi_XX_XB_NN
    
    ! none eligible
    real*8, dimension(AA,HH,HH,TT) :: pi_EE_XX_EE, pi_EE_XX_ES, pi_EE_XX_SE, pi_EE_XX_EN, pi_EE_XX_NE, pi_EE_XX_SS, pi_EE_XX_SN, pi_EE_XX_NS, pi_EE_XX_NN
    
    real*8, dimension(AA,HH,HH,TT) :: pi_EX_XX_ES, pi_EX_XX_EN, pi_EX_XX_SS, pi_EX_XX_SN, pi_EX_XX_NS, pi_EX_XX_NN
    
    real*8, dimension(AA,HH,HH,TT) :: pi_XE_XX_SE, pi_XE_XX_NE, pi_XE_XX_SS, pi_XE_XX_SN, pi_XE_XX_NS, pi_XE_XX_NN
    
    real*8, dimension(AA,HH,HH,TT) ::  pi_XX_XX_SS, pi_XX_XX_SN, pi_XX_XX_NS, pi_XX_XX_NN
    

    ! 14) variables for simulation
    
    ! initial distributions
    real*8, dimension(16) :: distinit_LS ! distribution over initial joint labor status, for definition of indices see Simulation.f90
    real*8, dimension(AA,HH,HH) :: distinit_EE
    real*8, dimension(AA,HH,HH) :: distinit_EU, distinit_EN, distinit_UE, distinit_NE, distinit_ES, distinit_SE
    real*8, dimension(AA,HH,HH) :: distinit_UU, distinit_UN,  distinit_NU, distinit_NN, distinit_SS, distinit_SN,  distinit_NS, distinit_SU,  distinit_US

    ! keeping track of HH states
    integer, dimension(NN,TT) :: sim_LS, sim_hh, sim_hsp, sim_asset, sim_LS_h, sim_LS_sp
    real*8, dimension(NN,TT) :: sim_asset_level, sim_inc_h, sim_inc_sp
    real*8, allocatable :: emp_spells(:,:)

    ! transition matrices
    real*8, dimension(16,16) :: trans_join
    real*8, dimension(4,4) :: trans_h, trans_sp
    real*8, dimension(3,3) :: trans_all_3
    real*8, dimension(16,16,TT) :: trans_join_age
    real*8, dimension(4,4,TT) :: trans_h_age, trans_sp_age
    
    real*8, dimension(16,16) :: trans_join_N
    real*8, dimension(4,4) :: trans_h_N, trans_sp_N
    real*8, dimension(3,3) :: trans_all_3_N
    real*8, dimension(16,16,TT) :: trans_join_age_N
    real*8, dimension(4,4,TT) :: trans_h_age_N, trans_sp_age_N

    real*8, dimension(3,3,4) :: trans_group_3, trans_group_3_N
    real*8, dimension(4,4,4) :: trans_group_N, trans_group


    ! 15) statistics for calibration
    real*8 :: ass_agg, ass_agg_med ! aggregate assets
    real*8, dimension(TT) :: ass_life ! asset lifecycle
    real*8, dimension(TT) :: inc_life ! income lifecycle
    real*8, dimension(TT) :: inc_life_std ! standard deviation income lifecycle
    real*8, dimension(16) :: dist_LS_join !distribution over joint labor states
    real*8, dimension(16,4) :: dist_LS_join_groups !distribution over joint labor states
    real*8, dimension(4) :: dist_LS_indiv !distribution over individual labor states

    real*8, dimension(6) :: dist_LS_join_cum
    real*8, dimension(6,4) :: dist_LS_join_groups_cum
    real*8, dimension(4) :: ass_groups, ass_group_med, inc_groups, incSD_groups
    real*8 :: inc_agg, incSD_agg

    real*8 :: dw_unemp_2, dw_unemp_6, dw_unemp_12, dw_unemp_24, dw_job_2, dw_job_5, dw_job_8
    real*8 :: sep_1, sep_15, sep_5


    ! 16) ouput exercises


    ! new exercise, old baseline
    integer, dimension(4,4) :: AWE_o_base ! basic old
    integer, dimension(4,4) :: AWE_o_lambda_age ! arrival rates (age only)
    integer, dimension(4,4) :: AWE_o_lambda ! arrival rates 
    integer, dimension(4,4) :: AWE_o_hout ! HC N member, excl effect on lambda
    integer, dimension(4,4) :: AWE_o_hout_lambda ! HC N member, incl effect on lambda
    integer, dimension(4,4) :: AWE_o_hemp ! HC E member, excl effect on lambda and delta
    integer, dimension(4,4) :: AWE_o_hemp_lambda ! HC E member
    integer, dimension(4,4) :: AWE_o_ass ! assets, excl effect on lambda
    integer, dimension(4,4) :: AWE_o_ass_lambda ! assets
    integer, dimension(4,4) :: AWE_o_age ! age on policies
    integer, dimension(4,4) :: AWE_o_age_lambda ! age on policies
    integer, dimension(4,4) :: AWE_o_all ! age on policies
    integer, dimension(4,4) :: AWE_o_all_lambda ! age on policies
    integer, dimension(4,4) :: AWE_o_sanity ! age on policies

    ! new exercise, young baseline
    integer, dimension(4,4) :: AWE_y_base ! basic old
    integer, dimension(4,4) :: AWE_y_lambda_age ! arrival rates (age only)
    integer, dimension(4,4) :: AWE_y_lambda ! arrival rates (age only)
    integer, dimension(4,4) :: AWE_y_hout ! HC N member, excl effect on lambda
    integer, dimension(4,4) :: AWE_y_hout_lambda ! HC N member, incl effect on lambda
    integer, dimension(4,4) :: AWE_y_hemp ! HC E member, excl effect on lambda and delta
    integer, dimension(4,4) :: AWE_y_hemp_lambda ! HC E member
    integer, dimension(4,4) :: AWE_y_ass ! assets, excl effect on lambda
    integer, dimension(4,4) :: AWE_y_ass_lambda ! assets
    integer, dimension(4,4) :: AWE_y_age ! age on policies
    integer, dimension(4,4) :: AWE_y_age_lambda ! age on policies
    integer, dimension(4,4) :: AWE_y_all ! age on policies
    integer, dimension(4,4) :: AWE_y_all_lambda ! age on policies
    integer, dimension(4,4) :: AWE_y_sanity ! age on policies

    !! non-stochastic exercises (weighted)
    ! new exercise, old baseline
    real*8, dimension(3,2) :: AWE_nsw_o_base ! basic old
    real*8, dimension(3,2) :: AWE_nsw_o_lambda_age ! arrival rates (age only)
    real*8, dimension(3,2) :: AWE_nsw_o_lambda ! arrival rates 
    real*8, dimension(3,2) :: AWE_nsw_o_hout ! HC N member, excl effect on lambda
    real*8, dimension(3,2) :: AWE_nsw_o_hout_lambda ! HC N member, incl effect on lambda
    real*8, dimension(3,2) :: AWE_nsw_o_hemp ! HC E member, excl effect on lambda and delta
    real*8, dimension(3,2) :: AWE_nsw_o_hemp_lambda ! HC E member
    real*8, dimension(3,2) :: AWE_nsw_o_ass ! assets, excl effect on lambda
    real*8, dimension(3,2) :: AWE_nsw_o_ass_lambda ! assets
    real*8, dimension(3,2) :: AWE_nsw_o_age ! age on policies
    real*8, dimension(3,2) :: AWE_nsw_o_age_lambda ! age on policies
    real*8, dimension(3,2) :: AWE_nsw_o_all ! age on policies
    real*8, dimension(3,2) :: AWE_nsw_o_all_lambda ! age on policies
    real*8, dimension(3,2) :: AWE_nsw_o_sanity ! age on policies

    ! new exercise, young baseline
    real*8, dimension(3,2) :: AWE_nsw_y_base ! basic old
    real*8, dimension(3,2) :: AWE_nsw_y_lambda_age ! arrival rates (age only)
    real*8, dimension(3,2) :: AWE_nsw_y_lambda ! arrival rates (age only)
    real*8, dimension(3,2) :: AWE_nsw_y_hout ! HC N member, excl effect on lambda
    real*8, dimension(3,2) :: AWE_nsw_y_hout_lambda ! HC N member, incl effect on lambda
    real*8, dimension(3,2) :: AWE_nsw_y_hemp ! HC E member, excl effect on lambda and delta
    real*8, dimension(3,2) :: AWE_nsw_y_hemp_lambda ! HC E member
    real*8, dimension(3,2) :: AWE_nsw_y_ass ! assets, excl effect on lambda
    real*8, dimension(3,2) :: AWE_nsw_y_ass_lambda ! assets
    real*8, dimension(3,2) :: AWE_nsw_y_age ! age on policies
    real*8, dimension(3,2) :: AWE_nsw_y_age_lambda ! age on policies
    real*8, dimension(3,2) :: AWE_nsw_y_all ! age on policies
    real*8, dimension(3,2) :: AWE_nsw_y_all_lambda ! age on policies
    real*8, dimension(3,2) :: AWE_nsw_y_sanity ! age on policies

    !! non-stochastic exercises (un-weighted)
    ! new exercise, old baseline
    real*8, dimension(3,2) :: AWE_nsuw_o_base ! basic old
    real*8, dimension(3,2) :: AWE_nsuw_o_lambda_age ! arrival rates (age only)
    real*8, dimension(3,2) :: AWE_nsuw_o_lambda ! arrival rates 
    real*8, dimension(3,2) :: AWE_nsuw_o_hout ! HC N member, excl effect on lambda
    real*8, dimension(3,2) :: AWE_nsuw_o_hout_lambda ! HC N member, incl effect on lambda
    real*8, dimension(3,2) :: AWE_nsuw_o_hemp ! HC E member, excl effect on lambda and delta
    real*8, dimension(3,2) :: AWE_nsuw_o_hemp_lambda ! HC E member
    real*8, dimension(3,2) :: AWE_nsuw_o_ass ! assets, excl effect on lambda
    real*8, dimension(3,2) :: AWE_nsuw_o_ass_lambda ! assets
    real*8, dimension(3,2) :: AWE_nsuw_o_age ! age on policies
    real*8, dimension(3,2) :: AWE_nsuw_o_age_lambda ! age on policies
    real*8, dimension(3,2) :: AWE_nsuw_o_all ! age on policies
    real*8, dimension(3,2) :: AWE_nsuw_o_all_lambda ! age on policies
    real*8, dimension(3,2) :: AWE_nsuw_o_sanity ! age on policies

    ! new exercise, young baseline
    real*8, dimension(3,2) :: AWE_nsuw_y_base ! basic old
    real*8, dimension(3,2) :: AWE_nsuw_y_lambda_age ! arrival rates (age only)
    real*8, dimension(3,2) :: AWE_nsuw_y_lambda ! arrival rates (age only)
    real*8, dimension(3,2) :: AWE_nsuw_y_hout ! HC N member, excl effect on lambda
    real*8, dimension(3,2) :: AWE_nsuw_y_hout_lambda ! HC N member, incl effect on lambda
    real*8, dimension(3,2) :: AWE_nsuw_y_hemp ! HC E member, excl effect on lambda and delta
    real*8, dimension(3,2) :: AWE_nsuw_y_hemp_lambda ! HC E member
    real*8, dimension(3,2) :: AWE_nsuw_y_ass ! assets, excl effect on lambda
    real*8, dimension(3,2) :: AWE_nsuw_y_ass_lambda ! assets
    real*8, dimension(3,2) :: AWE_nsuw_y_age ! age on policies
    real*8, dimension(3,2) :: AWE_nsuw_y_age_lambda ! age on policies
    real*8, dimension(3,2) :: AWE_nsuw_y_all ! age on policies
    real*8, dimension(3,2) :: AWE_nsuw_y_all_lambda ! age on policies
    real*8, dimension(3,2) :: AWE_nsuw_y_sanity ! age on policies


    ! 17) firm parameters
    real*8, parameter :: alpha = 0.5d0 ! matching elasticity
    real*8, parameter :: xi = 1d0 ! matching normalization
    real*8:: kappa ! vacancy posting cost
    real*8, parameter :: chi = 0.7d0 ! fraction of output for HH 

    ! 18) firm value functions
    real*8, dimension(AA,HH,HH,TT) :: J_EE  
    real*8, dimension(AA,HH,HH,TT) :: J_EU, J_EN, J_ES
    
    ! 19) posted market tightness
    real*8, dimension(AA,HH,HH,TT) :: theta_UE, theta_NE, theta_SE   
    real*8, dimension(AA,HH,HH,TT) ::  theta_UU, theta_UN, theta_NU, theta_NN
    real*8, dimension(AA,HH,HH,TT) ::  theta_SS, theta_SU, theta_US, theta_SN, theta_NS
    
    ! 20) firm flow profit
    real*8, dimension(HH) :: prof


contains
    
    ! utility function (scalar)
    function util(c)

        implicit none

        ! Variable declarations
        real*8 :: util, caux
        real*8, intent(in) :: c
        
        ! Function
        caux = c
        if(c<=0d0) caux=1d-8
        
        if (gam/=1d0) then
            util = (caux**(1d0-gam)-1d0)/(1d0-gam)
        else
            util = log(caux)
        end if 

    end function util

    ! marginal utility (scalar)
    function utilp(c)

        implicit none

        ! Variable declarations
        real*8 :: utilp, caux
        real*8, intent(in) :: c
        
        ! Function
        caux = c
        if(c<=0d0) caux=1d-8
        
        utilp = caux**(-gam)

    end function utilp
    
    ! utility function (vector)
    function util_vec(c)

        implicit none

        ! Variable declarations
        real*8, intent(in) :: c(:)
        real*8 :: util_vec(size(c)), caux(size(c))
        
        ! Function
        caux = c
        where (c<=0d0 ) caux = 1d-8
        
        if (gam/=1d0) then
            util_vec = (caux**(1d0-gam)-1d0)/(1d0-gam)
        else
            util_vec = log(caux)
        end if 

    end function util_vec

    ! marginal utility (vector)
    function utilp_vec(c)

        implicit none

        ! Variable declarations
        real*8, intent(in) :: c(:)
        real*8 :: utilp_vec(size(c)), caux(size(c))
        
        ! Function
        caux = c
        where (c<=0d0 ) caux = 1d-8
        
        utilp_vec = caux**(-gam)

    end function utilp_vec
    
end module
