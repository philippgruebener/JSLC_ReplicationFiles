!#######################################################################
! Module Simulation
!#######################################################################

module Simulation
    
    use Interpolation_Routines
    use Store_Results
    
    implicit none
    
    ! for the entire simulation, the indices for labor market status are as follows:
    ! 1 - EE, 2 - EU, 3 - UE, 4 - EN, 5 - NE, 6 - UU, 7 - UN, 8 - NU, 9 - NN  
    ! 10 - SS, 11 - SN, 12 - NS, 13 - SU, 14 - US, 15 - SE, 16 - ES
    
    ! for head/spouse individually
    ! 1 - E, 2 - U, 3 - N, 4 - S
    
contains

    subroutine simulate()
    
        use Globals
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Local variable declarations
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        real*8, dimension(16) :: distinit_LS_cumsum !cumulative initial distribution over labor market states
        integer, dimension(16) :: LSinit ! help finding position of initial draw
        
        ! reshaped initial distributions
        real*8, dimension(AA*HH*HH) :: distinit_EE_vec
        real*8, dimension(AA*HH*HH) :: distinit_EU_vec, distinit_EN_vec, distinit_UE_vec, distinit_NE_vec, distinit_SE_vec, distinit_ES_vec
        real*8, dimension(AA*HH*HH) :: distinit_UU_vec, distinit_UN_vec,  distinit_NU_vec, distinit_NN_vec
        real*8, dimension(AA*HH*HH) :: distinit_SS_vec, distinit_SN_vec,  distinit_NS_vec, distinit_SU_vec, distinit_US_vec
        ! cumulated initial distributions
        real*8, dimension(AA*HH*HH) :: distinit_EE_cum
        real*8, dimension(AA*HH*HH) :: distinit_EU_cum, distinit_EN_cum, distinit_UE_cum, distinit_NE_cum, distinit_ES_cum, distinit_SE_cum
        real*8, dimension(AA*HH*HH) :: distinit_UU_cum, distinit_UN_cum,  distinit_NU_cum, distinit_NN_cum
        real*8, dimension(AA*HH*HH) :: distinit_SS_cum, distinit_SN_cum,  distinit_NS_cum, distinit_SU_cum, distinit_US_cum
        ! help finding position of initial draw
        integer, dimension(AA*HH*HH) :: EEinit
        integer, dimension(AA*HH*HH) :: EUinit, ENinit, UEinit, NEinit, SEinit, ESinit
        integer, dimension(AA*HH*HH) :: UUinit, UNinit,  NUinit, NNinit, SSinit, SNinit,  NSinit, SUinit, USinit
        ! help computing std of income
        real*8, allocatable:: stack_inc(:)
        integer, allocatable:: stack_LS(:)

        ! storage employment spells
        real*8, allocatable :: emp_spells_large(:,:)
        real*8 :: w_count
        integer :: ind_spell, ind_find, find
        integer :: aux_time
        
        real*8 :: sep_h, sep_sp
        real*8 :: draw !random number draw
        integer :: j,k ! loop index
        real*8 :: init ! initial position
        real*8, parameter :: AAr = dble(AA), HHr = dble(HH)! real sizes
        integer, parameter :: y10 = TT/4 ! equivalent of 10 years
        
        ! helper asset median
        real*8, allocatable :: ass_vec(:)
        real*8, allocatable :: ass_vec_group(:)
        integer, allocatable :: ind_sort(:)
        integer, allocatable :: ind_sort_group(:)

        ! allocate variables
        allocate (stack_inc(2*NN))
        allocate (stack_LS(2*NN))
        allocate (emp_spells_large(2*NN*TT,9))
        allocate (ass_vec(NN*TT))
        allocate (ass_vec_group(NN*y10))
        allocate (ind_sort(NN*TT))
        allocate (ind_sort_group(NN*y10))
        
        ! initialize random number
        call srand(123)
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! initial states of simulated HHs
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        ! get initial joint labor market status (LS)
        
        distinit_LS_cumsum = cumsum(distinit_LS)! cumulative LS probabilities

        do n=1,NN
            draw=rand()
            LSinit = merge( 1, 0, distinit_LS_cumsum<draw )
            sim_LS(n,1)=findpos(LSinit,0,0)        
        end do
        
        ! vectorize and cumulate initial distributions conditional on LS
        distinit_EE_vec = reshape(distinit_EE,(/AA*HH*HH/))
        distinit_EU_vec = reshape(distinit_EU,(/AA*HH*HH/))
        distinit_EN_vec = reshape(distinit_EN,(/AA*HH*HH/))
        distinit_UE_vec = reshape(distinit_UE,(/AA*HH*HH/))
        distinit_NE_vec = reshape(distinit_NE,(/AA*HH*HH/))
        distinit_UU_vec = reshape(distinit_UU,(/AA*HH*HH/))
        distinit_UN_vec = reshape(distinit_UN,(/AA*HH*HH/))
        distinit_NU_vec = reshape(distinit_NU,(/AA*HH*HH/))
        distinit_NN_vec = reshape(distinit_NN,(/AA*HH*HH/))
        distinit_SE_vec = reshape(distinit_SE,(/AA*HH*HH/))
        distinit_ES_vec = reshape(distinit_ES,(/AA*HH*HH/))
        distinit_SS_vec = reshape(distinit_SS,(/AA*HH*HH/))
        distinit_SN_vec = reshape(distinit_SN,(/AA*HH*HH/))
        distinit_NS_vec = reshape(distinit_NS,(/AA*HH*HH/))
        distinit_SU_vec = reshape(distinit_SU,(/AA*HH*HH/))
        distinit_US_vec = reshape(distinit_US,(/AA*HH*HH/))

        
        distinit_EE_cum = cumsum(distinit_EE_vec)
        distinit_EU_cum = cumsum(distinit_EU_vec)
        distinit_EN_cum = cumsum(distinit_EN_vec)
        distinit_UE_cum = cumsum(distinit_UE_vec)
        distinit_NE_cum = cumsum(distinit_NE_vec)
        distinit_UU_cum = cumsum(distinit_UU_vec)
        distinit_UN_cum = cumsum(distinit_UN_vec)
        distinit_NU_cum = cumsum(distinit_NU_vec)
        distinit_NN_cum = cumsum(distinit_NN_vec)
        distinit_SS_cum = cumsum(distinit_SS_vec)
        distinit_SN_cum = cumsum(distinit_SN_vec)
        distinit_NS_cum = cumsum(distinit_NS_vec)
        distinit_SU_cum = cumsum(distinit_SU_vec)
        distinit_US_cum = cumsum(distinit_US_vec)
        distinit_SE_cum = cumsum(distinit_SE_vec)
        distinit_ES_cum = cumsum(distinit_ES_vec)
        
        ! draw for each household the initial state conditional on LS
        
        do n=1,NN
        
             draw=rand()
             
             if (sim_LS(n,1)==1) then ! 1 - EE
             
                EEinit = merge( 1, 0, distinit_EE_cum<draw )
                init = dble(findpos(EEinit,0,0)) 
                sim_hsp(n,1) = ceiling(init/(AAr*HHr))
                sim_hh(n,1) = ceiling((init-(AAr*HHr*(sim_hsp(n,1)-1)))/(AAr))
                sim_asset(n,1) = int(init-(AAr*HHr*(sim_hsp(n,1)-1))-(AAr*(sim_hh(n,1)-1)))
                sim_asset_level(n,1) = assgrid(sim_asset(n,1))
                sim_inc_h(n,1) = wage(sim_hh(n,1))
                sim_inc_sp(n,1) = wage(sim_hsp(n,1))

             elseif(sim_LS(n,1)==2) then ! 2 - EU
                
                EUinit = merge( 1, 0, distinit_EU_cum<draw )
                init = dble(findpos(EUinit,0,0)) 
                sim_hsp(n,1) = ceiling(init/(AAr*HHr))
                sim_hh(n,1) = ceiling((init-(AAr*HHr*(sim_hsp(n,1)-1)))/(AAr))
                sim_asset(n,1) = int(init-(AAr*HHr*(sim_hsp(n,1)-1))-(AAr*(sim_hh(n,1)-1)))
                sim_asset_level(n,1) = assgrid(sim_asset(n,1))
                sim_inc_h(n,1) = wage(sim_hh(n,1))
                sim_inc_sp(n,1) = 0d0

             elseif(sim_LS(n,1)==3) then ! 3 - UE
                
                UEinit = merge( 1, 0, distinit_UE_cum<draw )
                init = dble(findpos(UEinit,0,0)) 
                sim_hsp(n,1) = ceiling(init/(AAr*HHr))
                sim_hh(n,1) = ceiling((init-(AAr*HHr*(sim_hsp(n,1)-1)))/(AAr))
                sim_asset(n,1) = int(init-(AAr*HHr*(sim_hsp(n,1)-1))-(AAr*(sim_hh(n,1)-1)))
                sim_asset_level(n,1) = assgrid(sim_asset(n,1))
                sim_inc_h(n,1) = 0d0
                sim_inc_sp(n,1) = wage(sim_hsp(n,1))

             elseif(sim_LS(n,1)==4) then ! 4 - EN
                
                ENinit = merge( 1, 0, distinit_EN_cum<draw )
                init = dble(findpos(ENinit,0,0)) 
                sim_hsp(n,1) = ceiling(init/(AAr*HHr))
                sim_hh(n,1) = ceiling((init-(AAr*HHr*(sim_hsp(n,1)-1)))/(AAr))
                sim_asset(n,1) = int(init-(AAr*HHr*(sim_hsp(n,1)-1))-(AAr*(sim_hh(n,1)-1)))
                sim_asset_level(n,1) = assgrid(sim_asset(n,1))
                sim_inc_h(n,1) = wage(sim_hh(n,1))
                sim_inc_sp(n,1) = 0d0

             elseif(sim_LS(n,1)==5) then ! 5 - NE
                
                NEinit = merge( 1, 0, distinit_NE_cum<draw )
                init = dble(findpos(NEinit,0,0)) 
                sim_hsp(n,1) = ceiling(init/(AAr*HHr))
                sim_hh(n,1) = ceiling((init-(AAr*HHr*(sim_hsp(n,1)-1)))/(AAr))
                sim_asset(n,1) = int(init-(AAr*HHr*(sim_hsp(n,1)-1))-(AAr*(sim_hh(n,1)-1)))
                sim_asset_level(n,1) = assgrid(sim_asset(n,1))
                sim_inc_h(n,1) = 0d0
                sim_inc_sp(n,1) = wage(sim_hsp(n,1))

             elseif(sim_LS(n,1)==6) then ! 6 - UU
                
                UUinit = merge( 1, 0, distinit_UU_cum<draw )
                init = dble(findpos(UUinit,0,0)) 
                sim_hsp(n,1) = ceiling(init/(AAr*HHr))
                sim_hh(n,1) = ceiling((init-(AAr*HHr*(sim_hsp(n,1)-1)))/(AAr))
                sim_asset(n,1) = int(init-(AAr*HHr*(sim_hsp(n,1)-1))-(AAr*(sim_hh(n,1)-1)))
                sim_asset_level(n,1) = assgrid(sim_asset(n,1))
                sim_inc_h(n,1) = 0d0
                sim_inc_sp(n,1) = 0d0

             elseif(sim_LS(n,1)==7) then ! 7 - UN
                
                UNinit = merge( 1, 0, distinit_UN_cum<draw )
                init = dble(findpos(UUinit,0,0)) 
                sim_hsp(n,1) = ceiling(init/(AAr*HHr))
                sim_hh(n,1) = ceiling((init-(AAr*HHr*(sim_hsp(n,1)-1)))/(AAr))
                sim_asset(n,1) = int(init-(AAr*HHr*(sim_hsp(n,1)-1))-(AAr*(sim_hh(n,1)-1)))
                sim_asset_level(n,1) = assgrid(sim_asset(n,1))
                sim_inc_h(n,1) = 0d0
                sim_inc_sp(n,1) = 0d0

             elseif(sim_LS(n,1)==8) then ! 8 - NU
                
                NUinit = merge( 1, 0, distinit_NU_cum<draw )
                init = dble(findpos(NUinit,0,0)) 
                sim_hsp(n,1) = ceiling(init/(AAr*HHr))
                sim_hh(n,1) = ceiling((init-(AAr*HHr*(sim_hsp(n,1)-1)))/(AAr))
                sim_asset(n,1) = int(init-(AAr*HHr*(sim_hsp(n,1)-1))-(AAr*(sim_hh(n,1)-1)))
                sim_asset_level(n,1) = assgrid(sim_asset(n,1))
                sim_inc_h(n,1) = 0d0
                sim_inc_sp(n,1) = 0d0

             elseif(sim_LS(n,1)==9) then ! 9 - NN  
                
                NNinit = merge( 1, 0, distinit_NN_cum<draw )
                init = dble(findpos(NNinit,0,0)) 
                sim_hsp(n,1) = ceiling(init/(AAr*HHr))
                sim_hh(n,1) = ceiling((init-(AAr*HHr*(sim_hsp(n,1)-1)))/(AAr))
                sim_asset(n,1) = int(init-(AAr*HHr*(sim_hsp(n,1)-1))-(AAr*(sim_hh(n,1)-1)))
                sim_asset_level(n,1) = assgrid(sim_asset(n,1))
                sim_inc_h(n,1) = 0d0
                sim_inc_sp(n,1) = 0d0

            elseif(sim_LS(n,1)==10) then ! 10 - SS  
                
                SSinit = merge( 1, 0, distinit_SS_cum<draw )
                init = dble(findpos(SSinit,0,0)) 
                sim_hsp(n,1) = ceiling(init/(AAr*HHr))
                sim_hh(n,1) = ceiling((init-(AAr*HHr*(sim_hsp(n,1)-1)))/(AAr))
                sim_asset(n,1) = int(init-(AAr*HHr*(sim_hsp(n,1)-1))-(AAr*(sim_hh(n,1)-1)))
                sim_asset_level(n,1) = assgrid(sim_asset(n,1))
                sim_inc_h(n,1) = 0d0
                sim_inc_sp(n,1) = 0d0

            elseif(sim_LS(n,1)==11) then ! 11 - SN  
                
                SNinit = merge( 1, 0, distinit_SN_cum<draw )
                init = dble(findpos(SNinit,0,0)) 
                sim_hsp(n,1) = ceiling(init/(AAr*HHr))
                sim_hh(n,1) = ceiling((init-(AAr*HHr*(sim_hsp(n,1)-1)))/(AAr))
                sim_asset(n,1) = int(init-(AAr*HHr*(sim_hsp(n,1)-1))-(AAr*(sim_hh(n,1)-1)))
                sim_asset_level(n,1) = assgrid(sim_asset(n,1))
                sim_inc_h(n,1) = 0d0
                sim_inc_sp(n,1) = 0d0

            elseif(sim_LS(n,1)==12) then ! 12 - NS  
                
                NSinit = merge( 1, 0, distinit_NS_cum<draw )
                init = dble(findpos(NSinit,0,0)) 
                sim_hsp(n,1) = ceiling(init/(AAr*HHr))
                sim_hh(n,1) = ceiling((init-(AAr*HHr*(sim_hsp(n,1)-1)))/(AAr))
                sim_asset(n,1) = int(init-(AAr*HHr*(sim_hsp(n,1)-1))-(AAr*(sim_hh(n,1)-1)))
                sim_asset_level(n,1) = assgrid(sim_asset(n,1))
                sim_inc_h(n,1) = 0d0
                sim_inc_sp(n,1) = 0d0

            elseif(sim_LS(n,1)==13) then ! 13 - SU  
                
                SUinit = merge( 1, 0, distinit_SU_cum<draw )
                init = dble(findpos(SUinit,0,0)) 
                sim_hsp(n,1) = ceiling(init/(AAr*HHr))
                sim_hh(n,1) = ceiling((init-(AAr*HHr*(sim_hsp(n,1)-1)))/(AAr))
                sim_asset(n,1) = int(init-(AAr*HHr*(sim_hsp(n,1)-1))-(AAr*(sim_hh(n,1)-1)))
                sim_asset_level(n,1) = assgrid(sim_asset(n,1))
                sim_inc_h(n,1) = 0d0
                sim_inc_sp(n,1) = 0d0

            elseif(sim_LS(n,1)==14) then ! 14 - US  
                
                USinit = merge( 1, 0, distinit_US_cum<draw )
                init = dble(findpos(USinit,0,0)) 
                sim_hsp(n,1) = ceiling(init/(AAr*HHr))
                sim_hh(n,1) = ceiling((init-(AAr*HHr*(sim_hsp(n,1)-1)))/(AAr))
                sim_asset(n,1) = int(init-(AAr*HHr*(sim_hsp(n,1)-1))-(AAr*(sim_hh(n,1)-1)))
                sim_asset_level(n,1) = assgrid(sim_asset(n,1))
                sim_inc_h(n,1) = 0d0
                sim_inc_sp(n,1) = 0d0

            elseif(sim_LS(n,1)==15) then ! 15 - SE
                
                SEinit = merge( 1, 0, distinit_SE_cum<draw )
                init = dble(findpos(SEinit,0,0)) 
                sim_hsp(n,1) = ceiling(init/(AAr*HHr))
                sim_hh(n,1) = ceiling((init-(AAr*HHr*(sim_hsp(n,1)-1)))/(AAr))
                sim_asset(n,1) = int(init-(AAr*HHr*(sim_hsp(n,1)-1))-(AAr*(sim_hh(n,1)-1)))
                sim_asset_level(n,1) = assgrid(sim_asset(n,1))
                sim_inc_h(n,1) = 0d0
                sim_inc_sp(n,1) = wage(sim_hsp(n,1))
            
            elseif(sim_LS(n,1)==16) then ! 16 - ES
                
                ESinit = merge( 1, 0, distinit_ES_cum<draw )
                init = dble(findpos(ESinit,0,0)) 
                sim_hsp(n,1) = ceiling(init/(AAr*HHr))
                sim_hh(n,1) = ceiling((init-(AAr*HHr*(sim_hsp(n,1)-1)))/(AAr))
                sim_asset(n,1) = int(init-(AAr*HHr*(sim_hsp(n,1)-1))-(AAr*(sim_hh(n,1)-1)))
                sim_asset_level(n,1) = assgrid(sim_asset(n,1))
                sim_inc_h(n,1) = wage(sim_hh(n,1))
                sim_inc_sp(n,1) = 0d0

             end if  
                 
        end do
        
        
        !!!!!!!!!!!!!!!!!!!!!
        ! simulate life cycle
        !!!!!!!!!!!!!!!!!!!!!
        do n=1,NN
            
            do t=2,TT
                
                if (sim_LS(n,t-1)==1) then ! 1 - EE
                    
                    ! asset policy
                    sim_asset(n,t) = ap_ind(apol_EE(sim_asset(n,t-1),sim_hh(n,t-1),sim_hsp(n,t-1),t-1)) 
                    sim_asset_level(n,t) = assgrid(sim_asset(n,t))

                    ! human capital
                    call HCsim(sim_hh(n,t-1), sim_hsp(n,t-1), 1, 1, delta(t-1,sim_hh(n,t-1)), delta(t-1,sim_hsp(n,t-1)),sim_hh(n,t), sim_hsp(n,t), sep_h, sep_sp)
                    
                    ! match quality and labor market status (jointly!)
                    call LS_sim(sim_asset(n,t), sim_hh(n,t), sim_hsp(n,t), t-1, (1d0-sep_h), (1d0-sep_sp), 0d0, 1d0, 0d0, 1d0, sim_LS(n,t))
                    
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=2).and.(sim_LS(n,t)/=4).and.(sim_LS(n,t)/=16)) then
                        sim_inc_h(n,t) = 0d0
                      else
                        sim_inc_h(n,t) = wage(sim_hh(n,t))
                      end if
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=3).and.(sim_LS(n,t)/=5).and.(sim_LS(n,t)/=15))then
                        sim_inc_sp(n,t) = 0d0
                      else
                        sim_inc_sp(n,t) = wage(sim_hsp(n,t))
                      end if
                    
                elseif(sim_LS(n,t-1)==2) then ! 2 - EU
                    
                    ! asset policy
                    sim_asset(n,t) = ap_ind(apol_EU(sim_asset(n,t-1),sim_hh(n,t-1),sim_hsp(n,t-1),t-1))                    
                    sim_asset_level(n,t) = assgrid(sim_asset(n,t))

                    ! human capital
                    call HCsim(sim_hh(n,t-1), sim_hsp(n,t-1), 1, 0, delta(t-1,sim_hh(n,t-1)), 0d0, sim_hh(n,t), sim_hsp(n,t), sep_h, sep_sp)
                    
                    ! match quality and labor market status (jointly!)
                    call LS_sim(sim_asset(n,t), sim_hh(n,t), sim_hsp(n,t), t-1, (1d0-sep_h), (1d0-sep_h)*lambda_UE_E(sim_asset(n,t),sim_hsp(n,t-1),sim_hh(n,t-1),t-1)+(sep_h)*lambda_UE_X(sim_asset(n,t),sim_hsp(n,t-1),sim_hh(n,t-1),t-1), 0d0, 1d0, (1d0-phiUS), (1d0-phiUS), sim_LS(n,t))
                    
                              if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=2).and.(sim_LS(n,t)/=4).and.(sim_LS(n,t)/=16)) then
                        sim_inc_h(n,t) = 0d0
                      else
                        sim_inc_h(n,t) = wage(sim_hh(n,t))
                      end if
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=3).and.(sim_LS(n,t)/=5).and.(sim_LS(n,t)/=15))then
                        sim_inc_sp(n,t) = 0d0
                      else
                        sim_inc_sp(n,t) = wage(sim_hsp(n,t))
                      end if

                elseif(sim_LS(n,t-1)==3) then ! 3 - UE
                    
                    ! asset policy
                    sim_asset(n,t) = ap_ind(apol_UE(sim_asset(n,t-1),sim_hh(n,t-1),sim_hsp(n,t-1),t-1))                    
                    sim_asset_level(n,t) = assgrid(sim_asset(n,t))

                    ! human capital
                    call HCsim(sim_hh(n,t-1), sim_hsp(n,t-1), 0, 1, 0d0, delta(t-1,sim_hsp(n,t-1)), sim_hh(n,t), sim_hsp(n,t), sep_h, sep_sp)
                    
                    ! match quality and labor market status (jointly!)
                    call LS_sim(sim_asset(n,t), sim_hh(n,t), sim_hsp(n,t), t-1, (1d0-sep_sp)*lambda_UE_E(sim_asset(n,t),sim_hh(n,t-1),sim_hsp(n,t-1),t-1)+(sep_sp)*lambda_UE_X(sim_asset(n,t),sim_hh(n,t-1),sim_hsp(n,t-1),t-1), (1d0-sep_sp), (1d0-phiUS), (1d0-phiUS), 0d0, 1d0, sim_LS(n,t))
                     
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=2).and.(sim_LS(n,t)/=4).and.(sim_LS(n,t)/=16)) then
                        sim_inc_h(n,t) = 0d0
                      else
                        sim_inc_h(n,t) = wage(sim_hh(n,t))
                      end if
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=3).and.(sim_LS(n,t)/=5).and.(sim_LS(n,t)/=15))then
                        sim_inc_sp(n,t) = 0d0
                      else
                        sim_inc_sp(n,t) = wage(sim_hsp(n,t))
                      end if

                elseif(sim_LS(n,t-1)==4) then ! 4 - EN
                    
                    ! asset policy
                    sim_asset(n,t) = ap_ind(apol_EN(sim_asset(n,t-1),sim_hh(n,t-1),sim_hsp(n,t-1),t-1))                    
                    sim_asset_level(n,t) = assgrid(sim_asset(n,t))

                    ! human capital
                    call HCsim(sim_hh(n,t-1), sim_hsp(n,t-1), 1, 0, delta(t-1,sim_hh(n,t-1)), 0d0, sim_hh(n,t), sim_hsp(n,t), sep_h, sep_sp)
                    
                    ! match quality and labor market status (jointly!)
                    call LS_sim(sim_asset(n,t), sim_hh(n,t), sim_hsp(n,t), t-1, (1d0-sep_h), (1d0-sep_h)*lambda_NE_E(sim_asset(n,t),sim_hsp(n,t-1),sim_hh(n,t-1),t-1)+(sep_h)*lambda_NE_X(sim_asset(n,t),sim_hsp(n,t-1),sim_hh(n,t-1),t-1), 0d0, 1d0, 0d0, 0d0, sim_LS(n,t))
                     
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=2).and.(sim_LS(n,t)/=4).and.(sim_LS(n,t)/=16)) then
                        sim_inc_h(n,t) = 0d0
                      else
                        sim_inc_h(n,t) = wage(sim_hh(n,t))
                      end if
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=3).and.(sim_LS(n,t)/=5).and.(sim_LS(n,t)/=15))then
                        sim_inc_sp(n,t) = 0d0
                      else
                        sim_inc_sp(n,t) = wage(sim_hsp(n,t))
                      end if

                elseif(sim_LS(n,t-1)==5) then ! 5 - NE
                    
                    ! asset policy
                    sim_asset(n,t) = ap_ind(apol_NE(sim_asset(n,t-1),sim_hh(n,t-1),sim_hsp(n,t-1),t-1))                    
                    sim_asset_level(n,t) = assgrid(sim_asset(n,t))

                    ! human capital
                    call HCsim(sim_hh(n,t-1), sim_hsp(n,t-1), 0, 1, 0d0, delta(t-1,sim_hsp(n,t-1)), sim_hh(n,t), sim_hsp(n,t),sep_h, sep_sp)
                    
                    ! match quality and labor market status (jointly!)
                    call LS_sim(sim_asset(n,t), sim_hh(n,t), sim_hsp(n,t), t-1, (1d0-sep_sp)*lambda_NE_E(sim_asset(n,t),sim_hh(n,t-1),sim_hsp(n,t-1),t-1)+(sep_sp)*lambda_NE_X(sim_asset(n,t),sim_hh(n,t-1),sim_hsp(n,t-1),t-1), (1d0-sep_sp), 0d0, 0d0, 0d0, 1d0, sim_LS(n,t))
                    
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=2).and.(sim_LS(n,t)/=4).and.(sim_LS(n,t)/=16)) then
                        sim_inc_h(n,t) = 0d0
                      else
                        sim_inc_h(n,t) = wage(sim_hh(n,t))
                      end if
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=3).and.(sim_LS(n,t)/=5).and.(sim_LS(n,t)/=15))then
                        sim_inc_sp(n,t) = 0d0
                      else
                        sim_inc_sp(n,t) = wage(sim_hsp(n,t))
                      end if

                elseif(sim_LS(n,t-1)==6) then ! 6 - UU
                    
                    ! asset policy
                    sim_asset(n,t) = ap_ind(apol_UU(sim_asset(n,t-1),sim_hh(n,t-1),sim_hsp(n,t-1),t-1))                    
                    sim_asset_level(n,t) = assgrid(sim_asset(n,t))

                    ! human capital
                    call HCsim(sim_hh(n,t-1), sim_hsp(n,t-1), 0, 0, 0d0, 0d0, sim_hh(n,t), sim_hsp(n,t),sep_h, sep_sp)
                    
                    ! match quality and labor market status (jointly!)
                    call LS_sim(sim_asset(n,t), sim_hh(n,t), sim_hsp(n,t), t-1, lambda_UU(sim_asset(n,t),sim_hh(n,t-1),sim_hsp(n,t-1),t-1), lambda_UU(sim_asset(n,t),sim_hsp(n,t-1),sim_hh(n,t-1),t-1), (1d0-phiUS), (1d0-phiUS), (1d0-phiUS), (1d0-phiUS), sim_LS(n,t))
                     
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=2).and.(sim_LS(n,t)/=4).and.(sim_LS(n,t)/=16)) then
                        sim_inc_h(n,t) = 0d0
                      else
                        sim_inc_h(n,t) = wage(sim_hh(n,t))
                      end if
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=3).and.(sim_LS(n,t)/=5).and.(sim_LS(n,t)/=15))then
                        sim_inc_sp(n,t) = 0d0
                      else
                        sim_inc_sp(n,t) = wage(sim_hsp(n,t))
                      end if

                elseif(sim_LS(n,t-1)==7) then ! 7 - UN
                    
                    ! asset policy
                    sim_asset(n,t) = ap_ind(apol_UN(sim_asset(n,t-1),sim_hh(n,t-1),sim_hsp(n,t-1),t-1))                    
                    sim_asset_level(n,t) = assgrid(sim_asset(n,t))

                    ! human capital
                    call HCsim(sim_hh(n,t-1), sim_hsp(n,t-1), 0, 0, 0d0, 0d0, sim_hh(n,t), sim_hsp(n,t),sep_h, sep_sp)
                    
                    ! match quality and labor market status (jointly!)
                    call LS_sim(sim_asset(n,t), sim_hh(n,t), sim_hsp(n,t), t-1, lambda_UN(sim_asset(n,t),sim_hh(n,t-1),sim_hsp(n,t-1),t-1), lambda_NU(sim_asset(n,t),sim_hsp(n,t-1),sim_hh(n,t-1),t-1), (1d0-phiUS), (1d0-phiUS), 0d0, 0d0, sim_LS(n,t))
                    
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=2).and.(sim_LS(n,t)/=4).and.(sim_LS(n,t)/=16)) then
                        sim_inc_h(n,t) = 0d0
                      else
                        sim_inc_h(n,t) = wage(sim_hh(n,t))
                      end if
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=3).and.(sim_LS(n,t)/=5).and.(sim_LS(n,t)/=15))then
                        sim_inc_sp(n,t) = 0d0
                      else
                        sim_inc_sp(n,t) = wage(sim_hsp(n,t))
                      end if

                elseif(sim_LS(n,t-1)==8) then ! 8 - NU
                    
                    ! asset policy
                    sim_asset(n,t) = ap_ind(apol_NU(sim_asset(n,t-1),sim_hh(n,t-1),sim_hsp(n,t-1),t-1))                    
                    sim_asset_level(n,t) = assgrid(sim_asset(n,t))

                    ! human capital
                    call HCsim(sim_hh(n,t-1), sim_hsp(n,t-1), 0, 0, 0d0, 0d0, sim_hh(n,t), sim_hsp(n,t) ,sep_h, sep_sp)
                    
                    ! match quality and labor market status (jointly!)
                    call LS_sim(sim_asset(n,t), sim_hh(n,t), sim_hsp(n,t), t-1, lambda_NU(sim_asset(n,t),sim_hh(n,t-1),sim_hsp(n,t-1),t-1), lambda_UN(sim_asset(n,t),sim_hsp(n,t-1),sim_hh(n,t-1),t-1), 0d0, 0d0, (1d0-phiUS), (1d0-phiUS), sim_LS(n,t))
                    
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=2).and.(sim_LS(n,t)/=4).and.(sim_LS(n,t)/=16)) then
                        sim_inc_h(n,t) = 0d0
                      else
                        sim_inc_h(n,t) = wage(sim_hh(n,t))
                      end if
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=3).and.(sim_LS(n,t)/=5).and.(sim_LS(n,t)/=15))then
                        sim_inc_sp(n,t) = 0d0
                      else
                        sim_inc_sp(n,t) = wage(sim_hsp(n,t))
                      end if

                elseif(sim_LS(n,t-1)==9) then ! 9 - NN  
                    
                    ! asset policy
                    sim_asset(n,t) = ap_ind(apol_NN(sim_asset(n,t-1),sim_hh(n,t-1),sim_hsp(n,t-1),t-1))                    
                    sim_asset_level(n,t) = assgrid(sim_asset(n,t))

                    ! human capital
                    call HCsim(sim_hh(n,t-1), sim_hsp(n,t-1), 0, 0, 0d0, 0d0, sim_hh(n,t), sim_hsp(n,t) ,sep_h, sep_sp)
                    
                    ! match quality and labor market status (jointly!)
                    call LS_sim(sim_asset(n,t), sim_hh(n,t), sim_hsp(n,t), t-1, lambda_NN(sim_asset(n,t),sim_hh(n,t-1),sim_hsp(n,t-1),t-1), lambda_NN(sim_asset(n,t),sim_hsp(n,t-1),sim_hh(n,t-1),t-1), 0d0, 0d0, 0d0, 0d0, sim_LS(n,t))
                    
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=2).and.(sim_LS(n,t)/=4).and.(sim_LS(n,t)/=16)) then
                        sim_inc_h(n,t) = 0d0
                      else
                        sim_inc_h(n,t) = wage(sim_hh(n,t))
                      end if
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=3).and.(sim_LS(n,t)/=5).and.(sim_LS(n,t)/=15))then
                        sim_inc_sp(n,t) = 0d0
                      else
                        sim_inc_sp(n,t) = wage(sim_hsp(n,t))
                      end if
                    
                    
                elseif(sim_LS(n,t-1)==10) then ! 10 - SS  
                    
                    ! asset policy
                    sim_asset(n,t) = ap_ind(apol_SS(sim_asset(n,t-1),sim_hh(n,t-1),sim_hsp(n,t-1),t-1))                    
                    sim_asset_level(n,t) = assgrid(sim_asset(n,t))

                    ! human capital
                    call HCsim(sim_hh(n,t-1), sim_hsp(n,t-1), 0, 0, 0d0, 0d0, sim_hh(n,t), sim_hsp(n,t) ,sep_h, sep_sp)
                    
                    ! match quality and labor market status (jointly!)
                    call LS_sim(sim_asset(n,t), sim_hh(n,t), sim_hsp(n,t), t-1, lambda_SS(sim_asset(n,t),sim_hh(n,t-1),sim_hsp(n,t-1),t-1), lambda_SS(sim_asset(n,t),sim_hsp(n,t-1),sim_hh(n,t-1),t-1), 0d0, 0d0, 0d0, 0d0, sim_LS(n,t))
                    
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=2).and.(sim_LS(n,t)/=4).and.(sim_LS(n,t)/=16)) then
                        sim_inc_h(n,t) = 0d0
                      else
                        sim_inc_h(n,t) = wage(sim_hh(n,t))
                      end if
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=3).and.(sim_LS(n,t)/=5).and.(sim_LS(n,t)/=15))then
                        sim_inc_sp(n,t) = 0d0
                      else
                        sim_inc_sp(n,t) = wage(sim_hsp(n,t))
                      end if

                elseif(sim_LS(n,t-1)==11) then ! 11 - SN  
                    
                    ! asset policy
                    sim_asset(n,t) = ap_ind(apol_SN(sim_asset(n,t-1),sim_hh(n,t-1),sim_hsp(n,t-1),t-1))                    
                    sim_asset_level(n,t) = assgrid(sim_asset(n,t))

                    ! human capital
                    call HCsim(sim_hh(n,t-1), sim_hsp(n,t-1), 0, 0, 0d0, 0d0, sim_hh(n,t), sim_hsp(n,t) ,sep_h, sep_sp)
                    
                    ! match quality and labor market status (jointly!)
                    call LS_sim(sim_asset(n,t), sim_hh(n,t), sim_hsp(n,t), t-1, lambda_SN(sim_asset(n,t),sim_hh(n,t-1),sim_hsp(n,t-1),t-1), lambda_NS(sim_asset(n,t),sim_hsp(n,t-1),sim_hh(n,t-1),t-1), 0d0, 0d0, 0d0, 0d0, sim_LS(n,t))
                    
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=2).and.(sim_LS(n,t)/=4).and.(sim_LS(n,t)/=16)) then
                        sim_inc_h(n,t) = 0d0
                      else
                        sim_inc_h(n,t) = wage(sim_hh(n,t))
                      end if
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=3).and.(sim_LS(n,t)/=5).and.(sim_LS(n,t)/=15))then
                        sim_inc_sp(n,t) = 0d0
                      else
                        sim_inc_sp(n,t) = wage(sim_hsp(n,t))
                      end if

                elseif(sim_LS(n,t-1)==12) then ! 12 - NS  
                    
                    ! asset policy
                    sim_asset(n,t) = ap_ind(apol_NS(sim_asset(n,t-1),sim_hh(n,t-1),sim_hsp(n,t-1),t-1))                    
                    sim_asset_level(n,t) = assgrid(sim_asset(n,t))

                    ! human capital
                    call HCsim(sim_hh(n,t-1), sim_hsp(n,t-1), 0, 0, 0d0, 0d0, sim_hh(n,t), sim_hsp(n,t) ,sep_h, sep_sp)
                    
                    ! match quality and labor market status (jointly!)
                    call LS_sim(sim_asset(n,t), sim_hh(n,t), sim_hsp(n,t), t-1, lambda_NS(sim_asset(n,t),sim_hh(n,t-1),sim_hsp(n,t-1),t-1), lambda_SN(sim_asset(n,t),sim_hsp(n,t-1),sim_hh(n,t-1),t-1), 0d0, 0d0, 0d0, 0d0, sim_LS(n,t))
                    
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=2).and.(sim_LS(n,t)/=4).and.(sim_LS(n,t)/=16)) then
                        sim_inc_h(n,t) = 0d0
                      else
                        sim_inc_h(n,t) = wage(sim_hh(n,t))
                      end if
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=3).and.(sim_LS(n,t)/=5).and.(sim_LS(n,t)/=15))then
                        sim_inc_sp(n,t) = 0d0
                      else
                        sim_inc_sp(n,t) = wage(sim_hsp(n,t))
                      end if

                elseif(sim_LS(n,t-1)==13) then ! 13 - SU  
                    
                    ! asset policy
                    sim_asset(n,t) = ap_ind(apol_SU(sim_asset(n,t-1),sim_hh(n,t-1),sim_hsp(n,t-1),t-1))                    
                    sim_asset_level(n,t) = assgrid(sim_asset(n,t))

                    ! human capital
                    call HCsim(sim_hh(n,t-1), sim_hsp(n,t-1), 0, 0, 0d0, 0d0, sim_hh(n,t), sim_hsp(n,t) ,sep_h, sep_sp)
                    
                    ! match quality and labor market status (jointly!)
                    call LS_sim(sim_asset(n,t), sim_hh(n,t), sim_hsp(n,t), t-1, lambda_SU(sim_asset(n,t),sim_hh(n,t-1),sim_hsp(n,t-1),t-1), lambda_US(sim_asset(n,t),sim_hsp(n,t-1),sim_hh(n,t-1),t-1), 0d0, 0d0, (1d0-phiUS), (1d0-phiUS), sim_LS(n,t))
                    
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=2).and.(sim_LS(n,t)/=4).and.(sim_LS(n,t)/=16)) then
                        sim_inc_h(n,t) = 0d0
                      else
                        sim_inc_h(n,t) = wage(sim_hh(n,t))
                      end if
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=3).and.(sim_LS(n,t)/=5).and.(sim_LS(n,t)/=15))then
                        sim_inc_sp(n,t) = 0d0
                      else
                        sim_inc_sp(n,t) = wage(sim_hsp(n,t))
                      end if

                elseif(sim_LS(n,t-1)==14) then ! 14 - US  
                    
                    ! asset policy
                    sim_asset(n,t) = ap_ind(apol_US(sim_asset(n,t-1),sim_hh(n,t-1),sim_hsp(n,t-1),t-1))                    
                    sim_asset_level(n,t) = assgrid(sim_asset(n,t))

                    ! human capital
                    call HCsim(sim_hh(n,t-1), sim_hsp(n,t-1), 0, 0, 0d0, 0d0, sim_hh(n,t), sim_hsp(n,t) ,sep_h, sep_sp)
                    
                    ! match quality and labor market status (jointly!)
                    call LS_sim(sim_asset(n,t), sim_hh(n,t), sim_hsp(n,t), t-1, lambda_US(sim_asset(n,t),sim_hh(n,t-1),sim_hsp(n,t-1),t-1), lambda_SU(sim_asset(n,t),sim_hsp(n,t-1),sim_hh(n,t-1),t-1), (1d0-phiUS), (1d0-phiUS), 0d0, 0d0, sim_LS(n,t))
                    
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=2).and.(sim_LS(n,t)/=4).and.(sim_LS(n,t)/=16)) then
                        sim_inc_h(n,t) = 0d0
                      else
                        sim_inc_h(n,t) = wage(sim_hh(n,t))
                      end if
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=3).and.(sim_LS(n,t)/=5).and.(sim_LS(n,t)/=15))then
                        sim_inc_sp(n,t) = 0d0
                      else
                        sim_inc_sp(n,t) = wage(sim_hsp(n,t))
                      end if

                elseif(sim_LS(n,t-1)==15) then ! 15 - SE  
                    
                    ! asset policy
                    sim_asset(n,t) = ap_ind(apol_SE(sim_asset(n,t-1),sim_hh(n,t-1),sim_hsp(n,t-1),t-1))                    
                    sim_asset_level(n,t) = assgrid(sim_asset(n,t))

                    ! human capital
                    call HCsim(sim_hh(n,t-1), sim_hsp(n,t-1), 0, 1, 0d0, delta(t-1,sim_hsp(n,t-1)), sim_hh(n,t), sim_hsp(n,t), sep_h, sep_sp)
                    
                    ! match quality and labor market status (jointly!)
                    call LS_sim(sim_asset(n,t), sim_hh(n,t), sim_hsp(n,t), t-1, (1d0-sep_sp)*lambda_SE_E(sim_asset(n,t),sim_hh(n,t-1),sim_hsp(n,t-1),t-1)+(sep_sp)*lambda_SE_X(sim_asset(n,t),sim_hh(n,t-1),sim_hsp(n,t-1),t-1), (1d0-sep_sp), 0d0, 0d0, 0d0, 1d0, sim_LS(n,t))
                    
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=2).and.(sim_LS(n,t)/=4).and.(sim_LS(n,t)/=16)) then
                        sim_inc_h(n,t) = 0d0
                      else
                        sim_inc_h(n,t) = wage(sim_hh(n,t))
                      end if
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=3).and.(sim_LS(n,t)/=5).and.(sim_LS(n,t)/=15))then
                        sim_inc_sp(n,t) = 0d0
                      else
                        sim_inc_sp(n,t) = wage(sim_hsp(n,t))
                      end if

                elseif(sim_LS(n,t-1)==16) then ! 16 - ES  
                    
                    ! asset policy
                    sim_asset(n,t) = ap_ind(apol_ES(sim_asset(n,t-1),sim_hh(n,t-1),sim_hsp(n,t-1),t-1))                    
                    sim_asset_level(n,t) = assgrid(sim_asset(n,t))

                    ! human capital
                    call HCsim(sim_hh(n,t-1), sim_hsp(n,t-1), 1, 0, delta(t-1,sim_hh(n,t-1)), 0d0, sim_hh(n,t), sim_hsp(n,t), sep_h, sep_sp)
                    
                    ! match quality and labor market status (jointly!)
                    call LS_sim(sim_asset(n,t), sim_hh(n,t), sim_hsp(n,t), t-1, (1d0-sep_h), (1d0-sep_h)*lambda_SE_E(sim_asset(n,t),sim_hsp(n,t-1),sim_hh(n,t-1),t-1)+(sep_h)*lambda_SE_X(sim_asset(n,t),sim_hsp(n,t-1),sim_hh(n,t-1),t-1), 0d0, 1d0, 0d0, 0d0, sim_LS(n,t))
                    
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=2).and.(sim_LS(n,t)/=4).and.(sim_LS(n,t)/=16)) then
                        sim_inc_h(n,t) = 0d0
                      else
                        sim_inc_h(n,t) = wage(sim_hh(n,t))
                      end if
                    if ((sim_LS(n,t)/=1).and.(sim_LS(n,t)/=3).and.(sim_LS(n,t)/=5).and.(sim_LS(n,t)/=15))then
                        sim_inc_sp(n,t) = 0d0
                      else
                        sim_inc_sp(n,t) = wage(sim_hsp(n,t))
                      end if

                end if  
                
            end do
        
        end do
        
        ! for reference: indexation of LS:
        ! 1 - EE, 2 - EU, 3 - UE, 4 - EN, 5 - NE, 6 - UU, 7 - UN, 8 - NU, 9 - NN
        ! 10 - SS, 11 - SN, 12 - NS, 13 - SU, 14 - US, 15 - SE, 16 - ES

        ! individual LS
        sim_LS_h = 0
        sim_LS_sp = 0
        
        where((sim_LS==1).or.(sim_LS==2).or.(sim_LS==4).or.(sim_LS==16)) sim_LS_h=1
        where((sim_LS==3).or.(sim_LS==6).or.(sim_LS==7).or.(sim_LS==14)) sim_LS_h=2
        where((sim_LS==5).or.(sim_LS==8).or.(sim_LS==9).or.(sim_LS==12)) sim_LS_h=3
        where((sim_LS==10).or.(sim_LS==11).or.(sim_LS==13).or.(sim_LS==15)) sim_LS_h=4

        
        where((sim_LS==1).or.(sim_LS==3).or.(sim_LS==5).or.(sim_LS==15)) sim_LS_sp=1
        where((sim_LS==2).or.(sim_LS==6).or.(sim_LS==8).or.(sim_LS==13)) sim_LS_sp=2
        where((sim_LS==4).or.(sim_LS==7).or.(sim_LS==9).or.(sim_LS==11)) sim_LS_sp=3
        where((sim_LS==10).or.(sim_LS==12).or.(sim_LS==14).or.(sim_LS==16)) sim_LS_sp=4

        
        
        !!!!!!!!!!!!!!!!!!!!!!
        ! transition matrices
        !!!!!!!!!!!!!!!!!!!!!!
        
        trans_join =0d0
        trans_h = 0d0
        trans_sp = 0d0
        trans_join_age = 0d0
        trans_h_age = 0d0
        trans_sp_age = 0d0

        trans_join_N =0d0
        trans_h_N = 0d0
        trans_sp_N = 0d0
        trans_join_age_N = 0d0
        trans_h_age_N = 0d0
        trans_sp_age_N = 0d0
    
        ! count transitions
        do n=1,NN
            
            do t=2,TT
            
                trans_join_N(sim_LS(n,t-1),sim_LS(n,t)) = trans_join_N(sim_LS(n,t-1),sim_LS(n,t)) + 1d0
                trans_join_age_N(sim_LS(n,t-1),sim_LS(n,t),t) = trans_join_age_N(sim_LS(n,t-1),sim_LS(n,t),t) + 1d0
                
                trans_h_N(sim_LS_h(n,t-1),sim_LS_h(n,t)) = trans_h_N(sim_LS_h(n,t-1),sim_LS_h(n,t)) + 1d0
                trans_sp_N(sim_LS_sp(n,t-1),sim_LS_sp(n,t)) = trans_sp_N(sim_LS_sp(n,t-1),sim_LS_sp(n,t)) + 1d0
                trans_h_age_N(sim_LS_h(n,t-1),sim_LS_h(n,t),t) = trans_h_age_N(sim_LS_h(n,t-1),sim_LS_h(n,t),t) + 1d0
                trans_sp_age_N(sim_LS_sp(n,t-1),sim_LS_sp(n,t),t) = trans_sp_age_N(sim_LS_sp(n,t-1),sim_LS_sp(n,t),t) + 1d0
        
            end do
        
        end do
        
        ! transform to probabilities
        do j=1,16
            trans_join(j,:) = trans_join_N(j,:) / sum(trans_join_N(j,:),1)
            do t=2,TT
                if (sum(trans_join_age_N(j,:,t),1)>0d0) then
                    trans_join_age(j,:,t) = trans_join_age_N(j,:,t) / sum(trans_join_age_N(j,:,t),1)
                end if
            end do
        end do
        
        do j=1,4
            trans_h(j,:) = trans_h_N(j,:) / sum(trans_h_N(j,:),1)
            trans_sp(j,:) = trans_sp_N(j,:) / sum(trans_sp_N(j,:),1)

            do t=2,TT
                if (sum(trans_h_age_N(j,:,t),1)>0d0) then
                    trans_h_age(j,:,t) = trans_h_age_N(j,:,t) / sum(trans_h_age_N(j,:,t),1)
                end if
                if (sum(trans_sp_age_N(j,:,t),1)>0d0) then
                    trans_sp_age(j,:,t) = trans_sp_age_N(j,:,t) / sum(trans_sp_age_N(j,:,t),1)
                end if
            end do
        end do
        
        ! average transition PBs, 3 states
        trans_all_3_N(1,:) = [trans_h_N(1,1) + trans_sp_N(1,1),  trans_h_N(1,2) + trans_sp_N(1,2)+trans_h_N(1,4) + trans_sp_N(1,4), trans_h_N(1,3) + trans_sp_N(1,3)]
        trans_all_3_N(2,:) = [trans_h_N(2,1) + trans_sp_N(2,1) + trans_h_N(4,1) + trans_sp_N(4,1),  trans_h_N(2,2) + trans_sp_N(2,2)+trans_h_N(2,4) + trans_sp_N(2,4) +&
                            & trans_h_N(4,2) + trans_sp_N(4,2)+trans_h_N(4,4) + trans_sp_N(4,4), trans_h_N(2,3) + trans_sp_N(2,3) + trans_h_N(4,3) + trans_sp_N(4,3)]
        trans_all_3_N(3,:) = [trans_h_N(3,1) + trans_sp_N(3,1),  trans_h_N(3,2) + trans_sp_N(3,2)+trans_h_N(3,4) + trans_sp_N(3,4), trans_h_N(3,3) + trans_sp_N(3,3)]

        do j=1,3
            trans_all_3(j,:) = trans_all_3_N(j,:) / sum(trans_all_3_N(j,:),1)
        end do

        ! transition PBs by age groups
        trans_group_N(:,:,1) = sum(trans_h_age_N(:,:,1:y10),3)+sum(trans_sp_age_N(:,:,1:y10),3)
        trans_group_N(:,:,2) = sum(trans_h_age_N(:,:,y10+1:2*y10),3)+sum(trans_sp_age_N(:,:,y10+1:2*y10),3)
        trans_group_N(:,:,3) = sum(trans_h_age_N(:,:,2*y10+1:3*y10),3)+sum(trans_sp_age_N(:,:,2*y10+1:3*y10),3)
        trans_group_N(:,:,4) = sum(trans_h_age_N(:,:,3*y10+1:TT),3)+sum(trans_sp_age_N(:,:,3*y10+1:TT),3)

        do j=1,4
            trans_group(j,:,1) = trans_group_N(j,:,1) / sum(trans_group_N(j,:,1),1)
            trans_group(j,:,2) = trans_group_N(j,:,2) / sum(trans_group_N(j,:,2),1)
            trans_group(j,:,3) = trans_group_N(j,:,3) / sum(trans_group_N(j,:,3),1)
            trans_group(j,:,4) = trans_group_N(j,:,4) / sum(trans_group_N(j,:,4),1)
        end do

        ! average transition PBs, 3 states
        do j = 1, 4
            trans_group_3_N(1,:,j) = [trans_group_N(1,1,j),  trans_group_N(1,2,j) + trans_group_N(1,4,j), trans_group_N(1,3,j)]
            trans_group_3_N(2,:,j) = [trans_group_N(2,1,j) + trans_group_N(4,1,j),  trans_group_N(2,2,j) + trans_group_N(2,4,j) +&
                                & trans_group_N(4,2,j)+trans_group_N(4,4,j), trans_group_N(2,3,j) + trans_group_N(4,3,j)]
            trans_group_3_N(3,:,j) = [trans_group_N(3,1,j),  trans_group_N(3,2,j)+trans_group_N(3,4,j), trans_group_N(3,3,j)]
        enddo

        do j=1,3
            trans_group_3(j,:,1) = trans_group_3_N(j,:,1) / sum(trans_group_3_N(j,:,1),1)
            trans_group_3(j,:,2) = trans_group_3_N(j,:,2) / sum(trans_group_3_N(j,:,2),1)
            trans_group_3(j,:,3) = trans_group_3_N(j,:,3) / sum(trans_group_3_N(j,:,3),1)
            trans_group_3(j,:,4) = trans_group_3_N(j,:,4) / sum(trans_group_3_N(j,:,4),1)
        end do

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! statistics for calibration
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ! per capita savings: mean
        ass_agg = sum(sim_asset_level)/dble(NN*TT)
        
        ! per capita savings: median
        ass_vec = reshape( sim_asset_level, (/ NN*TT /))
        ind_sort = 0
        call hpsort_eps_epw(NN*TT, ass_vec, ind_sort, 1d-16)
        ass_agg_med = ass_vec(NN*TT/2)
        

        ! assets life-cycle: mean
        ass_life = sum(sim_asset_level,1)/dble(NN)

        ass_groups(1) = sum(sim_asset_level(:,1:y10))/dble(NN*y10)
        ass_groups(2) = sum(sim_asset_level(:,y10+1:2*y10))/dble(NN*y10)
        ass_groups(3) = sum(sim_asset_level(:,2*y10+1:3*y10))/dble(NN*y10)
        ass_groups(4) = sum(sim_asset_level(:,3*y10+1:TT))/dble(NN*y10)
        
        
        ! assets life-cycle: median
        ass_vec_group = reshape( sim_asset_level(:,1:y10), (/ NN*y10 /))
        ind_sort_group = 0
        call hpsort_eps_epw(NN*y10, ass_vec_group, ind_sort_group, 1d-16)
        ass_group_med(1) = ass_vec_group(NN*y10/2)
        
        ass_vec_group = reshape( sim_asset_level(:,y10+1:2*y10), (/ NN*y10 /))
        ind_sort_group = 0
        call hpsort_eps_epw(NN*y10, ass_vec_group, ind_sort_group, 1d-16)
        ass_group_med(2) = ass_vec_group(NN*y10/2)
        
        ass_vec_group = reshape( sim_asset_level(:,2*y10+1:3*y10), (/ NN*y10 /))
        ind_sort_group = 0
        call hpsort_eps_epw(NN*y10, ass_vec_group, ind_sort_group, 1d-16)
        ass_group_med(3) = ass_vec_group(NN*y10/2)
        
        ass_vec_group = reshape( sim_asset_level(:,3*y10+1:TT), (/ NN*y10 /))
        ind_sort_group = 0
        call hpsort_eps_epw(NN*y10, ass_vec_group, ind_sort_group, 1d-16)
        ass_group_med(4) = ass_vec_group(NN*y10/2)

        !labor market states distribution
        do j=1,16
            dist_LS_join(j)=dble(count((sim_LS==j)))
        end do
        dist_LS_join = dist_LS_join / dble(NN*TT)

        do k=1,4
            do j=1,16
                dist_LS_join_groups(j,k)=dble(count((sim_LS(:,y10*(k-1)+1:k*y10)==j)))
            end do
        end do
        dist_LS_join_groups = dist_LS_join_groups / dble(NN*TT/4)


        do j=1,4
            dist_LS_indiv(j)=dble(count((sim_LS_h==j))) + dble(count((sim_LS_sp==j)))
        end do
        dist_LS_indiv = dist_LS_indiv / dble(2*NN*TT)



        ! income life-cycle
        inc_life = (sum(sim_inc_h,1,(sim_LS_h==1))+sum(sim_inc_sp,1,(sim_LS_sp==1)))/dble(count((sim_LS_h==1),1)+count((sim_LS_sp==1),1))

        do t = 1,TT
            stack_inc(1:NN)=sim_inc_sp(:,t)
            stack_inc(NN+1:2*NN)=sim_inc_h(:,t)
            stack_LS(1:NN)=sim_LS_sp(:,t)
            stack_LS(NN+1:2*NN)=sim_LS_h(:,t)
            inc_life_std(t)=std(stack_inc, (stack_LS==1))
        end do

        inc_agg=(sum(sim_inc_h)+sum(sim_inc_sp))/dble(count(sim_LS_h==1)+count(sim_LS_sp==1))
        inc_groups(1)=(sum(sim_inc_h(:,1:y10))+sum(sim_inc_sp(:,1:y10)))/dble(count(sim_LS_h(:,1:y10)==1)+count(sim_LS_sp(:,1:y10)==1))
        inc_groups(2)=(sum(sim_inc_h(:,y10+1:2*y10))+sum(sim_inc_sp(:,y10+1:2*y10)))/dble(count(sim_LS_h(:,y10+1:2*y10)==1)+count(sim_LS_sp(:,y10+1:2*y10)==1))
        inc_groups(3)=(sum(sim_inc_h(:,2*y10+1:3*y10))+sum(sim_inc_sp(:,2*y10+1:3*y10)))/dble(count(sim_LS_h(:,2*y10+1:3*y10)==1)+count(sim_LS_sp(:,2*y10+1:3*y10)==1))
        inc_groups(4)=(sum(sim_inc_h(:,3*y10+1:TT))+sum(sim_inc_sp(:,3*y10+1:TT)))/dble(count(sim_LS_h(:,3*y10+1:TT)==1)+count(sim_LS_sp(:,3*y10+1:TT)==1))

        incSD_agg = sum(inc_life_std)/dble(TT)
        incSD_groups(1) = sum(inc_life_std(1:y10))/dble(y10)
        incSD_groups(2) = sum(inc_life_std(y10+1:2*y10))/dble(y10)
        incSD_groups(3) = sum(inc_life_std(2*y10+1:3*y10))/dble(y10)
        incSD_groups(4) = sum(inc_life_std(3*y10+1:TT))/dble(y10)

        ! pooled labor states
        dist_LS_join_cum(1)=dist_LS_join(1)
        dist_LS_join_cum(2)=dist_LS_join(2)+dist_LS_join(3)+dist_LS_join(15)+dist_LS_join(16)
        dist_LS_join_cum(3)=dist_LS_join(4)+dist_LS_join(5)
        dist_LS_join_cum(4)=dist_LS_join(6)+dist_LS_join(10)+dist_LS_join(13)+dist_LS_join(14)
        dist_LS_join_cum(5)=dist_LS_join(7)+dist_LS_join(8)+dist_LS_join(11)+dist_LS_join(12)
        dist_LS_join_cum(6)=dist_LS_join(9)

        do k = 1,4
            dist_LS_join_groups_cum(1,k)=dist_LS_join_groups(1,k)
            dist_LS_join_groups_cum(2,k)=dist_LS_join_groups(2,k)+dist_LS_join_groups(3,k)+dist_LS_join_groups(15,k)+dist_LS_join_groups(16,k)
            dist_LS_join_groups_cum(3,k)=dist_LS_join_groups(4,k)+dist_LS_join_groups(5,k)
            dist_LS_join_groups_cum(4,k)=dist_LS_join_groups(6,k)+dist_LS_join_groups(10,k)+dist_LS_join_groups(13,k)+dist_LS_join_groups(14,k)
            dist_LS_join_groups_cum(5,k)=dist_LS_join_groups(7,k)+dist_LS_join_groups(8,k)+dist_LS_join_groups(11,k)+dist_LS_join_groups(12,k)
            dist_LS_join_groups_cum(6,k)=dist_LS_join_groups(9,k)
        end do


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Duration Dependencies
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ! collect data on employment spells
        ind_spell = 0
        emp_spells_large = 0d0

        do n=1,NN
            
            do t=1,TT

                ! employment spells household head
                aux_time = 0
                if (t == 1) then
                    aux_time = 1
                elseif (sim_LS_h(n,t-1).ne.1) then
                    aux_time = 1
                endif
                if ( (sim_LS_h(n,t).eq.1).AND.(aux_time == 1) ) then
                    
                    ind_spell = ind_spell + 1 ! index of current spell

                    emp_spells_large(ind_spell,1) = dble(n) ! individuum
                    emp_spells_large(ind_spell,2) = dble(t) ! start of spell
                    emp_spells_large(ind_spell,3) = sim_inc_h(n,t) ! starting wage new job

                    ! end of previous job
                    ind_find = 1
                    find = 0
                    do while((find.eq.0).AND.(t-ind_find>0))
                        
                        if ( sim_LS_h(n,t-ind_find)==1 ) then
                            
                            ! last period of previous job
                            emp_spells_large(ind_spell,4) = dble(t-ind_find)
                            ! final wage at previous job
                            emp_spells_large(ind_spell,5) = sim_inc_h(n,t-ind_find)
                            
                            find = 1
                        end if

                        ind_find = ind_find + 1

                    end do

                    ! end of new job
                    ind_find = 1
                    find = 0
                    do while((find.eq.0).AND.(t+ind_find<TT+1))
                        
                        if ( sim_LS_h(n,t+ind_find).ne.1 ) then
                            
                            ! last period of new job
                            emp_spells_large(ind_spell,6) = dble(t+ind_find-1)
                                                    
                            find = 1
                        end if

                        ind_find = ind_find + 1

                    end do
                    
                    ! all those going until retirement
                    if ( emp_spells_large(ind_spell,6)==0d0 ) emp_spells_large(ind_spell,6) = dble(TT)

                    ! wage trajectory at new job
                    if ( emp_spells_large(ind_spell,6)-emp_spells_large(ind_spell,2) > 23d0 ) then
                        emp_spells_large(ind_spell,7) = sim_inc_h(n,t+24) ! wage after two years
                    end if

                    if ( emp_spells_large(ind_spell,6)-emp_spells_large(ind_spell,2) > 59d0 ) then
                        emp_spells_large(ind_spell,8) = sim_inc_h(n,t+60) ! wage after five years
                    end if

                    if ( emp_spells_large(ind_spell,6)-emp_spells_large(ind_spell,2) > 95d0 ) then
                        emp_spells_large(ind_spell,9) = sim_inc_h(n,t+96) ! wage after eight years
                    end if

                end if 


                aux_time = 0
                if (t == 1) then
                    aux_time = 1
                elseif (sim_LS_sp(n,t-1).ne.1) then
                    aux_time = 1
                endif
                ! employment spells spouse
                if ( (sim_LS_sp(n,t).eq.1).AND.(aux_time == 1) ) then
                    
                    ind_spell = ind_spell + 1 ! index of current spell

                    emp_spells_large(ind_spell,1) = dble(n) ! individuum
                    emp_spells_large(ind_spell,2) = dble(t) ! start of spell
                    emp_spells_large(ind_spell,3) = sim_inc_sp(n,t) ! starting wage new job

                    ! end of previous job
                    ind_find = 1
                    find = 0
                    do while((find.eq.0).AND.(t-ind_find>0))
                        
                        if ( sim_LS_sp(n,t-ind_find)==1 ) then
                            
                            ! last period of previous job
                            emp_spells_large(ind_spell,4) = dble(t-ind_find)
                            ! final wage at previous job
                            emp_spells_large(ind_spell,5) = sim_inc_sp(n,t-ind_find)
                            
                            find = 1
                        end if

                        ind_find = ind_find + 1

                    end do

                    ! end of new job
                    ind_find = 1
                    find = 0
                    do while((find.eq.0).AND.(t+ind_find<TT+1))
                        
                        if ( sim_LS_sp(n,t+ind_find).ne.1 ) then
                            
                            ! last period of previous job
                            emp_spells_large(ind_spell,6) = dble(t+ind_find-1)
                                                    
                            find = 1
                        end if

                        ind_find = ind_find + 1

                    end do

                    ! all those going until retirement
                    if ( emp_spells_large(ind_spell,6)==0d0 ) emp_spells_large(ind_spell,6) = dble(TT)
                    
                    ! wage trajectory at new job
                    if ( emp_spells_large(ind_spell,6)-emp_spells_large(ind_spell,2) > 23d0 ) then
                        emp_spells_large(ind_spell,7) = sim_inc_sp(n,t+24) ! wage after two years
                    end if

                    if ( emp_spells_large(ind_spell,6)-emp_spells_large(ind_spell,2) > 59d0 ) then
                        emp_spells_large(ind_spell,8) = sim_inc_sp(n,t+60) ! wage after five years
                    end if

                    if ( emp_spells_large(ind_spell,6)-emp_spells_large(ind_spell,2) > 95d0 ) then
                        emp_spells_large(ind_spell,9) = sim_inc_sp(n,t+96) ! wage after eight years
                    end if

                end if 


            end do

        end do

        allocate (emp_spells(ind_spell,9))
        emp_spells = emp_spells_large(1:ind_spell,:)

        ! wage losses from unemployment spells

        ! average wage loss - up to 3 months of U
        dw_unemp_2 = 0d0
        w_count = 0d0
        do n=1,ind_spell

            if ( (emp_spells_large(n,2)-emp_spells_large(n,4)>=2d0).AND.(emp_spells_large(n,2)-emp_spells_large(n,4)<=4d0).AND.(emp_spells_large(n,5).ne.0d0) ) then
                w_count = w_count + 1d0
                dw_unemp_2 = dw_unemp_2 + (log(emp_spells_large(n,3)) - log(emp_spells_large(n,5)))
            end if
        end do
        dw_unemp_2 = dw_unemp_2 / w_count

        ! average wage loss - 6 months of U
        dw_unemp_6 = 0d0 ! unused
        ! w_count = 0d0
        ! do n=1,ind_spell

        !     if ( (emp_spells_large(n,2)-emp_spells_large(n,4)==7d0).AND.(emp_spells_large(n,5).ne.0d0) ) then
        !         w_count = w_count + 1d0
        !         dw_unemp_6 = dw_unemp_6 + (log(emp_spells_large(n,3)) - log(emp_spells_large(n,5)))
        !     end if
        ! end do
        ! dw_unemp_6 = dw_unemp_6 / w_count

        ! average wage loss - 12 months of U
        dw_unemp_12 = 0d0
        w_count = 0d0
        do n=1,ind_spell

            if ( (emp_spells_large(n,2)-emp_spells_large(n,4)>=5d0).AND.(emp_spells_large(n,2)-emp_spells_large(n,4)<=13d0).AND.(emp_spells_large(n,5).ne.0d0) ) then
                w_count = w_count + 1d0
                dw_unemp_12 = dw_unemp_12 + (log(emp_spells_large(n,3)) - log(emp_spells_large(n,5)))
            end if
        end do
        dw_unemp_12 = dw_unemp_12 / w_count

        ! average wage loss - 24 months of U
        dw_unemp_24 = 0d0
        w_count = 0d0
        do n=1,ind_spell

            if ( (emp_spells_large(n,2)-emp_spells_large(n,4)>=14d0).AND.(emp_spells_large(n,2)-emp_spells_large(n,4)<=25d0).AND.(emp_spells_large(n,5).ne.0d0) ) then
                w_count = w_count + 1d0
                dw_unemp_24 = dw_unemp_24 + (log(emp_spells_large(n,3)) - log(emp_spells_large(n,5)))
            end if
        end do
        dw_unemp_24 = dw_unemp_24 / w_count


        ! wage growth on the job

        ! average wage growth - 2 years
        dw_job_2 = 0d0
        w_count = 0d0
        do n=1,ind_spell

            if ( emp_spells_large(n,6)-emp_spells_large(n,2)>23d0 ) then
                w_count = w_count + 1d0
                dw_job_2 = dw_job_2 + (log(emp_spells_large(n,7)) - log(emp_spells_large(n,3)))
            end if
        end do
        dw_job_2 = dw_job_2 / w_count

        ! average wage growth - 5 years
        dw_job_5 = 0d0
        w_count = 0d0
        do n=1,ind_spell

            if ( emp_spells_large(n,6)-emp_spells_large(n,2)>59d0 ) then
                w_count = w_count + 1d0
                dw_job_5 = dw_job_5 + (log(emp_spells_large(n,8)) - log(emp_spells_large(n,3)))
            end if
        end do
        dw_job_5 = dw_job_5 / w_count

        ! average wage growth - 8 years
        dw_job_8 = 0d0
        w_count = 0d0
        do n=1,ind_spell

            if ( emp_spells_large(n,6)-emp_spells_large(n,2)>95d0 ) then
                w_count = w_count + 1d0
                dw_job_8 = dw_job_8 + (log(emp_spells_large(n,9)) - log(emp_spells_large(n,3)))
            end if
        end do
        dw_job_8 = dw_job_8 / w_count


        ! separation rates by duration
        ! ???


        !store results
        call store_simulation() 
        call store_calibration()

        deallocate (emp_spells) 

        deallocate (stack_inc)
        deallocate (stack_LS)
        deallocate (emp_spells_large)
        deallocate (ass_vec)
        deallocate (ass_vec_group)
        deallocate (ind_sort)
        deallocate (ind_sort_group)

    
    end subroutine
    




    ! cumulative sum
    function cumsum(array)
        
        real*8, intent(in) :: array(:)
        real*8, dimension(size(array)) :: cumsum
        integer :: j
        
        cumsum = 0d0
        
        cumsum(1)=array(1)
        
        do j=2,size(array)
            cumsum(j)=cumsum(j-1)+array(j)
        end do
        
    end function

    ! standard deviation
    function std(array, cond)
        
        real*8, intent(in) :: array(:)
        logical, intent(in) :: cond(:)
        real*8:: std, help, mean
        integer :: j
        
        help = 0d0
        mean = sum(array,1,cond)/dble(count(cond))

        do j=1,size(array)
            if (cond(j))then
                help=help+(array(j)-mean)**2d0
              end if
        end do

        std=sqrt(help/(dble(count(cond))-1d0))
        
    end function

    ! find position in array
    function findpos(array, x, back)
        
        integer, intent(in) :: array(:)
        integer, intent(in) :: x, back
        integer :: j
        integer :: findpos 
        
        findpos=0
        
        if (back==0) then
            do j=1,size(array)
                if (array(j)==x) then
                    findpos=j
                    EXIT
                end if
            end do
        elseif (back==1) then
            do j=size(array),1,-1
                if (array(j)==x) then
                    findpos=j
                    EXIT
                end if
            end do
        else
            print *, 'Choose which way to search!'
        end if 
        
    end function findpos
    
    ! simulation step human capital
    subroutine HCsim(hh_prev, hsp_prev, hemp, hspemp, delta_h, delta_sp, hh_new, hsp_new, sep_h, sep_sp)

        use Globals
        
        real*8 :: zug
        real*8, intent(in) :: delta_h, delta_sp !separation PBs
        integer, intent(in) :: hh_prev, hsp_prev ! previous HC state
        integer, intent(in) :: hemp, hspemp ! indicator for employment (set 1) in previous period
        integer, intent(out) :: hh_new, hsp_new ! updated HC state
        real*8, intent(out) :: sep_h, sep_sp ! simulated separation
        real*8, dimension(HH) :: dist_hp
        integer, dimension(HH) :: find_HC

        ! simulate separation
        sep_h = 0d0
        sep_sp = 0d0

        if (hemp==1) then
            zug=rand()
            if (zug<delta_h) then
                sep_h = 1d0
            endif
        endif

        if (hspemp==1) then
            zug=rand()
            if (zug<delta_sp) then
                sep_sp = 1d0
            endif
        endif

        ! simulate human capital for the head
        if (hemp==1) then
            if (sep_h==1d0) then
                dist_hp = cumsum(htrans_EX(hh_prev,:))
            else
                dist_hp = cumsum(htrans_EE(hh_prev,:))
            end if
        else
            dist_hp = cumsum(htrans_XX(hh_prev,:))
        end if 
        zug=rand() 
        find_HC = merge( 1, 0, dist_hp<zug )
        hh_new = findpos(find_HC,0,0) 

        
        ! simulate human capital for the spouse
        if (hspemp==1) then
            if (sep_sp==1d0) then
                dist_hp = cumsum(htrans_EX(hsp_prev,:))
            else
                dist_hp = cumsum(htrans_EE(hsp_prev,:))
            end if
        else
            dist_hp = cumsum(htrans_XX(hsp_prev,:))
        end if 
        zug=rand() 
        find_HC = merge( 1, 0, dist_hp<zug )
        hsp_new = findpos(find_HC,0,0) 
        
    end subroutine HCsim
    
    ! simulation step future assets
    function ap_ind(ap)

        use Globals
        
        real*8, intent(in) :: ap
        integer :: ap_ind
        
        real*8 :: zug, PB_below
        integer :: ind_below
        integer, dimension(AA) :: find_below
        
        if (ap>assgrid(AA)) then
        
            ! special case if choosing outside of grid: allocate to highest gridpoint
            ap_ind=AA
            
        else
            
            ! find gridpoint below asset choice
            find_below = merge( 1, 0, assgrid <= ap )
            ind_below = findpos(find_below,1,1) 
            
            ! compute PB of chosing lower gridpoint
            PB_below = (assgrid(ind_below+1)-ap) /(assgrid(ind_below+1) - assgrid(ind_below))
            
            zug=rand() !make a draw
            
            !allocate to gridpoint
            if (zug<=PB_below) then
                ap_ind = ind_below
            else
                ap_ind = ind_below + 1
            end if 
            
        end if 
        
        
    end function ap_ind
    
    subroutine LS_sim(a_p, h_new, hsp_new, t_s, PB_emp_h, PB_emp_sp, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, LS_new)

        use Globals
        
        integer, intent(in) :: a_p, h_new, hsp_new, t_s ! indices for states
        real*8, intent(in) :: PB_emp_h, PB_emp_sp
        real*8, intent(in) :: pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp

        integer, intent(out) :: LS_new ! index of future labor state
        
        real*8 :: zug
        integer, dimension(16) :: find_LS
        real*8, dimension(16) :: PB_LS, PB_LS_cum ! 1 - EE, 2 - EU, 3 - UE, 4 - EN, 5 - NE, 6 - UU, 7 - UN, 8 - NU, 9 - NN  
                                                  ! 10 - SS, 11 - SN, 12 - NS, 13 - SU, 14 - US, 15 - SE, 16 - ES
    
        !!!!!!!!!!!!!!!!!!!!!
        ! find LS transition
        !!!!!!!!!!!!!!!!!!!!!
        !pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp,
        
        ! fill distribution
        ! 1 - EE
        PB_LS(1)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_EE(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*pb_Esp_Bsp*pi_EE_XB_EE(a_p, h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_EE(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_EE(a_p, h_new, hsp_new, t_s+1))

        ! 2 - EU
        PB_LS(2)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_EU(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*pb_Esp_Bsp*pi_EE_XB_EU(a_p, h_new, hsp_new, t_s+1)) &
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                (pb_Eh_Bh*pb_Xsp_Bsp*pi_EX_BB_EU(a_p,h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*pb_Xsp_Bsp*pi_EX_XB_EU(a_p, h_new, hsp_new, t_s+1)) 

        ! 3 - UE
        PB_LS(3)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_UE(a_p,  h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_UE(a_p,  h_new, hsp_new, t_s+1)) &
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                (pb_Xh_Bh*pb_Esp_Bsp*pi_XE_BB_UE(a_p, h_new, hsp_new, t_s+1) &
                +pb_Xh_Bh*(1d0-pb_Esp_Bsp)*pi_XE_BX_UE(a_p, h_new, hsp_new, t_s+1)) 
                
        ! 4 - EN
        PB_LS(4)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_EN(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*pb_Esp_Bsp*pi_EE_XB_EN(a_p, h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_EN(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_EN(a_p, h_new, hsp_new, t_s+1))&
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                (pb_Eh_Bh*pb_Xsp_Bsp*pi_EX_BB_EN(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*pb_Xsp_Bsp*pi_EX_XB_EN(a_p, h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Xsp_Bsp)*pi_EX_BX_EN(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Xsp_Bsp)*pi_EX_XX_EN(a_p, h_new, hsp_new, t_s+1))

        ! 5 - NE
        PB_LS(5)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_NE(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*pb_Esp_Bsp*pi_EE_XB_NE(a_p, h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_NE(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_NE(a_p, h_new, hsp_new, t_s+1))&
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                (pb_Xh_Bh*pb_Esp_Bsp*pi_XE_BB_NE(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*pb_Esp_Bsp*pi_XE_XB_NE(a_p, h_new, hsp_new, t_s+1) &
                +pb_Xh_Bh*(1d0-pb_Esp_Bsp)*pi_XE_BX_NE(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(1d0-pb_Esp_Bsp)*pi_XE_XX_NE(a_p, h_new, hsp_new, t_s+1))
        
        ! 6 - UU
        PB_LS(6)=(PB_emp_h * PB_emp_sp) * pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_UU(a_p, h_new, hsp_new, t_s+1) &
                + ((1d0-PB_emp_h) * PB_emp_sp) * pb_Xh_Bh*pb_Esp_Bsp*pi_XE_BB_UU(a_p, h_new, hsp_new, t_s+1) &
                + (PB_emp_h * (1d0-PB_emp_sp)) * pb_Eh_Bh*pb_Xsp_Bsp*pi_EX_BB_UU(a_p, h_new, hsp_new, t_s+1) &
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * pb_Xh_Bh*pb_Xsp_Bsp*pi_XX_BB_UU(a_p, h_new, hsp_new, t_s+1)
        
        ! 7 - UN
        PB_LS(7)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_UN(a_p, h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_UN(a_p, h_new, hsp_new, t_s+1)) &
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                (pb_Xh_Bh*pb_Esp_Bsp*pi_XE_BB_UN(a_p, h_new, hsp_new, t_s+1) &
                +pb_Xh_Bh*(1d0-pb_Esp_Bsp)*pi_XE_BX_UN(a_p, h_new, hsp_new, t_s+1)) &
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                (pb_Eh_Bh*pb_Xsp_Bsp*pi_EX_BB_UN(a_p, h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Xsp_Bsp)*pi_EX_BX_UN(a_p, h_new, hsp_new, t_s+1)) &
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * &
                (pb_Xh_Bh*pb_Xsp_Bsp*pi_XX_BB_UN(a_p, h_new, hsp_new, t_s+1) &
                +pb_Xh_Bh*(1d0-pb_Xsp_Bsp)*pi_XX_BX_UN(a_p, h_new, hsp_new, t_s+1)) 
                       
        ! 8 - NU
        PB_LS(8)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_NU(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(pb_Esp_Bsp)*pi_EE_XB_NU(a_p, h_new, hsp_new, t_s+1)) &
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                (pb_Xh_Bh*pb_Esp_Bsp*pi_XE_BB_NU(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(pb_Esp_Bsp)*pi_XE_XB_NU(a_p, h_new, hsp_new, t_s+1)) &
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                (pb_Eh_Bh*pb_Xsp_Bsp*pi_EX_BB_NU(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(pb_Xsp_Bsp)*pi_EX_XB_NU(a_p, h_new, hsp_new, t_s+1)) &
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * &
                (pb_Xh_Bh*pb_Xsp_Bsp*pi_XX_BB_NU(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(pb_Xsp_Bsp)*pi_XX_XB_NU(a_p,  h_new, hsp_new, t_s+1)) 
                 
        ! 9 - NN
        PB_LS(9)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_NN(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*pb_Esp_Bsp*pi_EE_XB_NN(a_p, h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_NN(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_NN(a_p, h_new, hsp_new, t_s+1)) &
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                (pb_Xh_Bh*pb_Esp_Bsp*pi_XE_BB_NN(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*pb_Esp_Bsp*pi_XE_XB_NN(a_p, h_new, hsp_new, t_s+1) &
                +pb_Xh_Bh*(1d0-pb_Esp_Bsp)*pi_XE_BX_NN(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(1d0-pb_Esp_Bsp)*pi_XE_XX_NN(a_p, h_new, hsp_new, t_s+1)) &
                + (PB_emp_h * (1d0-PB_emp_sp))  * &
                (pb_Eh_Bh*pb_Xsp_Bsp*pi_EX_BB_NN(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*pb_Xsp_Bsp*pi_EX_XB_NN(a_p, h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Xsp_Bsp)*pi_EX_BX_NN(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Xsp_Bsp)*pi_EX_XX_NN(a_p, h_new, hsp_new, t_s+1)) &
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * &
                (pb_Xh_Bh*pb_Xsp_Bsp*pi_XX_BB_NN(a_p, h_new, hsp_new, t_s+1) &
                +pb_Xh_Bh*(1d0-pb_Xsp_Bsp)*pi_XX_BX_NN(a_p,  h_new, hsp_new, t_s+1) & 
                +(1d0-pb_Xh_Bh)*pb_Xsp_Bsp*pi_XX_XB_NN(a_p,  h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(1d0-pb_Xsp_Bsp)*pi_XX_XX_NN(a_p, h_new, hsp_new, t_s+1))  
                 
                
        ! 10 - SS
        PB_LS(10)=(PB_emp_h * PB_emp_sp) * &
                ((1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_SS(a_p, h_new, hsp_new, t_s+1))&
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                ((1d0-pb_Xh_Bh)*(1d0-pb_Esp_Bsp)*pi_XE_XX_SS(a_p, h_new, hsp_new, t_s+1))&
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                ((1d0-pb_Eh_Bh)*(1d0-pb_Xsp_Bsp)*pi_EX_XX_SS(a_p, h_new, hsp_new, t_s+1))&
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * &
                ((1d0-pb_Xh_Bh)*(1d0-pb_Xsp_Bsp)*pi_XX_XX_SS(a_p, h_new, hsp_new, t_s+1)) 
        
        ! 11 - SN
        PB_LS(11)=(PB_emp_h * PB_emp_sp) * &
                ((1d0-pb_Eh_Bh)*pb_Esp_Bsp*pi_EE_XB_SN(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_SN(a_p, h_new, hsp_new, t_s+1))&
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                ((1d0-pb_Xh_Bh)*pb_Esp_Bsp*pi_XE_XB_SN(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(1d0-pb_Esp_Bsp)*pi_XE_XX_SN(a_p, h_new, hsp_new, t_s+1))&
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                ((1d0-pb_Eh_Bh)*pb_Xsp_Bsp*pi_EX_XB_SN(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Xsp_Bsp)*pi_EX_XX_SN(a_p, h_new, hsp_new, t_s+1))&
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * &
                ((1d0-pb_Xh_Bh)*pb_Xsp_Bsp*pi_XX_XB_SN(a_p,h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(1d0-pb_Xsp_Bsp)*pi_XX_XX_SN(a_p, h_new, hsp_new, t_s+1)) 
        
        ! 12 - NS
        PB_LS(12)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_NS(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_NS(a_p, h_new, hsp_new, t_s+1))&
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                (pb_Xh_Bh*(1d0-pb_Esp_Bsp)*pi_XE_BX_NS(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(1d0-pb_Esp_Bsp)*pi_XE_XX_NS(a_p, h_new, hsp_new, t_s+1))&
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                (pb_Eh_Bh*(1d0-pb_Xsp_Bsp)*pi_EX_BX_NS(a_p,  h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Xsp_Bsp)*pi_EX_XX_NS(a_p,  h_new, hsp_new, t_s+1))&
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * &
                (pb_Xh_Bh*(1d0-pb_Xsp_Bsp)*pi_XX_BX_NS(a_p,  h_new, hsp_new, t_s+1) & 
                +(1d0-pb_Xh_Bh)*(1d0-pb_Xsp_Bsp)*pi_XX_XX_NS(a_p, h_new, hsp_new, t_s+1)) 
        
        ! 13 - SU
        PB_LS(13)=(PB_emp_h * PB_emp_sp) * &
                (1d0-pb_Eh_Bh)*pb_Esp_Bsp*pi_EE_XB_SU(a_p, h_new, hsp_new, t_s+1)&
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                (1d0-pb_Xh_Bh)*pb_Esp_Bsp*pi_XE_XB_SU(a_p, h_new, hsp_new, t_s+1)  &
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                (1d0-pb_Eh_Bh)*pb_Xsp_Bsp*pi_EX_XB_SU(a_p, h_new, hsp_new, t_s+1)  &
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * &
                (1d0-pb_Xh_Bh)*pb_Xsp_Bsp*pi_XX_XB_SU(a_p, h_new, hsp_new, t_s+1) 
         
        
        
        ! 14 - US
        PB_LS(14)=(PB_emp_h * PB_emp_sp) * &
                 pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_US(a_p, h_new, hsp_new, t_s+1) &
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                pb_Xh_Bh*(1d0-pb_Esp_Bsp)*pi_XE_BX_US(a_p, h_new, hsp_new, t_s+1) &
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                 pb_Eh_Bh*(1d0-pb_Xsp_Bsp)*pi_EX_BX_US(a_p, h_new, hsp_new, t_s+1) &
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * &
                pb_Xh_Bh*(1d0-pb_Xsp_Bsp)*pi_XX_BX_US(a_p, h_new, hsp_new, t_s+1) 
         
         
         
        ! 15 - SE
        PB_LS(15)=(PB_emp_h * PB_emp_sp) * &
                ((1d0-pb_Eh_Bh)*pb_Esp_Bsp*pi_EE_XB_SE(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_SE(a_p, h_new, hsp_new, t_s+1))&
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                ((1d0-pb_Xh_Bh)*pb_Esp_Bsp*pi_XE_XB_SE(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(1d0-pb_Esp_Bsp)*pi_XE_XX_SE(a_p,  h_new, hsp_new, t_s+1))
                
   

        ! 16 - ES
        PB_LS(16)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_ES(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_ES(a_p, h_new, hsp_new, t_s+1))&
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                (pb_Eh_Bh*(1d0-pb_Xsp_Bsp)*pi_EX_BX_ES(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Xsp_Bsp)*pi_EX_XX_ES(a_p, h_new, hsp_new, t_s+1))

        


        PB_LS_cum = cumsum(PB_LS) 

   !     if(PB_LS_cum(16)< 1d0) print *, 'Error in LS transitions', PB_LS_cum(16)
    

        zug=rand() 
        find_LS = merge( 1, 0, PB_LS_cum<zug )
        LS_new = findpos(find_LS,0,0)  
        
   !     if(LS_new == 0) print *, 'new LS is zero', PB_LS       
                
    end subroutine LS_sim
    
end module
