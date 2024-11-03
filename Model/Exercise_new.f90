!#######################################################################
! Module Exercise
!#######################################################################

module Exercise_new
    
    use Interpolation_Routines
    use Store_Results
    
    implicit none
    
    ! for the entire simulation, the indices for labor market status are as follows:
    ! 1 - EE, 2 - EU, 3 - UE, 4 - EN, 5 - NE, 6 - UU, 7 - UN, 8 - NU, 9 - NN  
    ! 10 - SS, 11 - SN, 12 - NS, 13 - SU, 14 - US, 15 - SE, 16 - ES
    
    ! for head/spouse individually
    ! 1 - E, 2 - U, 3 - N, 4 - S
    
contains

    subroutine exercise_sim_new()

        use Globals
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Local variable declarations
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! number of relevant observations
        integer :: NN_ENyoung, NN_ENold, NN_NEyoung, NN_NEold
        ! location of relevant observations
        integer, allocatable :: t_ENyoung(:), n_ENyoung(:), t_ENold(:), n_ENold(:), t_NEyoung(:), n_NEyoung(:), t_NEold(:), n_NEold(:)
        ! states of relevant observations
        integer, allocatable :: hout_ENyoung(:), hemp_ENyoung(:), hout_ENold(:), hemp_ENold(:), hout_NEyoung(:), hemp_NEyoung(:), hout_NEold(:), hemp_NEold(:)
        real*8, allocatable :: ass_ENyoung(:), ass_ENold(:), ass_NEyoung(:), ass_NEold(:),lambda_ENyoung_E(:),lambda_ENold_E(:),lambda_NEyoung_E(:),lambda_NEold_E(:),lambda_ENyoung_X(:),lambda_ENold_X(:),lambda_NEyoung_X(:),lambda_NEold_X(:)
        integer :: hemp_diff, hout_diff 
        real*8 :: hemp_y, hemp_o, hout_y, hout_o, ass_y, ass_o, lambda_y_E, lambda_o_E, lambda_y_X, lambda_o_X, ass_ratio, lambda_ratio_E, lambda_ratio_X
        ! simulation variables
        real*8, allocatable :: step_hh_EN(:), step_hsp_EN(:), step_jobh_EN(:), step_jobsp_EN(:), step_LS_EN(:)
        real*8, allocatable :: step_hh_NE(:), step_hsp_NE(:), step_jobh_NE(:), step_jobsp_NE(:), step_LS_NE(:)

        ! helper variables
        integer :: ii, iEN, iNE

        ! for simulation function
        integer :: AWE11, AWE12, AWE13, AWE14, AWE21, AWE22, AWE23, AWE24, AWE31, AWE32, AWE33, AWE34, AWE41, AWE42, AWE43, AWE44 
        integer :: a_p, hh_s, hsp_s, t_s, a_s ! indices for current states (future a)
        integer :: a_p_arrv, hh_s_arrv, hsp_s_arrv, t_s_arrv, a_s_arrv ! indices for current states (future a) - arrival rates
        real*8 :: sep, lambda_E, lambda_X ! job finding / job loss PBs
        real*8 :: pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp ! eligibility PBs conditional on employed / unemployed
        real*8 :: step_hh, step_hsp, step_jobh, step_jobsp, step_LS ! fixed random draws for future states
        integer :: LS_new, indEN ! index of future labor state
        

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Prepare Exercises
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ! count relevant observations
        NN_ENyoung = count(sim_LS(:,1:(TT/4-1))==4)
        NN_ENold = count(sim_LS(:,((3*TT/4)+1):(TT-1))==4)
        NN_NEyoung = count(sim_LS(:,1:(TT/4-1))==5)
        NN_NEold = count(sim_LS(:,((3*TT/4)+1):(TT-1))==5)

        open (unit=100,file="Output/exercise_new_N.txt",action="write",status="replace")
        write (100,*) NN_ENyoung, NN_NEyoung, NN_ENold, NN_NEold
        close(100)

        ! allocate storage space
        allocate (t_ENyoung(NN_ENyoung))
        allocate (n_ENyoung(NN_ENyoung))
        allocate (t_ENold(NN_ENold))
        allocate (n_ENold(NN_ENold))
        allocate (t_NEyoung(NN_NEyoung))
        allocate (n_NEyoung(NN_NEyoung))
        allocate (t_NEold(NN_NEold))
        allocate (n_NEold(NN_NEold))

        allocate (hemp_ENyoung(NN_ENyoung))
        allocate (hout_ENyoung(NN_ENyoung))
        allocate (hemp_ENold(NN_ENold))
        allocate (hout_ENold(NN_ENold))
        allocate (hemp_NEyoung(NN_NEyoung))
        allocate (hout_NEyoung(NN_NEyoung))
        allocate (hemp_NEold(NN_NEold))
        allocate (hout_NEold(NN_NEold))
        allocate (ass_ENyoung(NN_ENyoung))
        allocate (ass_ENold(NN_ENold))
        allocate (ass_NEyoung(NN_NEyoung))
        allocate (ass_NEold(NN_NEold))
        allocate (lambda_ENyoung_E(NN_ENyoung))
        allocate (lambda_ENold_E(NN_ENold))
        allocate (lambda_NEyoung_E(NN_NEyoung))
        allocate (lambda_NEold_E(NN_NEold))
        allocate (lambda_ENyoung_X(NN_ENyoung))
        allocate (lambda_ENold_X(NN_ENold))
        allocate (lambda_NEyoung_X(NN_NEyoung))
        allocate (lambda_NEold_X(NN_NEold))

        allocate (step_hh_EN(max(NN_ENold,NN_ENyoung)))
        allocate (step_hsp_EN(max(NN_ENold,NN_ENyoung)))
        allocate (step_jobh_EN(max(NN_ENold,NN_ENyoung)))
        allocate (step_jobsp_EN(max(NN_ENold,NN_ENyoung)))
        allocate (step_LS_EN(max(NN_ENold,NN_ENyoung)))

        allocate (step_hh_NE(max(NN_NEold,NN_NEyoung)))
        allocate (step_hsp_NE(max(NN_NEold,NN_NEyoung)))
        allocate (step_jobh_NE(max(NN_NEold,NN_NEyoung)))
        allocate (step_jobsp_NE(max(NN_NEold,NN_NEyoung)))
        allocate (step_LS_NE(max(NN_NEold,NN_NEyoung)))

        ! random number seed
        call srand(291)

        ! get position and states of relevant young observations
        iEN=1
        iNE=1
        do n=1,NN
            do t=1,(TT/4-1)
                if (sim_LS(n,t)==4)then
                    t_ENyoung(iEN) = t
                    n_ENyoung(iEN) = n
                    hout_ENyoung(iEN) = sim_hsp(n,t)
                    hemp_ENyoung(iEN) = sim_hh(n,t)
                    ass_ENyoung(iEN) = assgrid(sim_asset(n,t))

                    a_p = ap_ind(apol_EN(sim_asset(n,t),sim_hh(n,t),sim_hsp(n,t),t)) ! asset choice
                    lambda_ENyoung_E(iEN) = lambda_NE_E(a_p,sim_hsp(n,t),sim_hh(n,t),t)
                    lambda_ENyoung_X(iEN) = lambda_NE_X(a_p,sim_hsp(n,t),sim_hh(n,t),t)

                    iEN=iEN+1
                elseif (sim_LS(n,t)==5) then 
                    t_NEyoung(iNE) = t
                    n_NEyoung(iNE) = n
                    hout_NEyoung(iNE) = sim_hh(n,t)
                    hemp_NEyoung(iNE) = sim_hsp(n,t)
                    ass_NEyoung(iNE) = assgrid(sim_asset(n,t))

                    a_p = ap_ind(apol_NE(sim_asset(n,t),sim_hh(n,t),sim_hsp(n,t),t)) ! asset choice
                    lambda_NEyoung_E(iNE) = lambda_NE_E(a_p,sim_hh(n,t),sim_hsp(n,t),t)! PB head has job next period
                    lambda_NEyoung_X(iNE) = lambda_NE_X(a_p,sim_hh(n,t),sim_hsp(n,t),t)! PB head has job next period

                    iNE=iNE+1
                end if
            end do
        end do

        ! get position and states of relevant old observations
        iEN=1
        iNE=1
        do n=1,NN
            do t=((3*TT/4)+1),(TT-1)
                if (sim_LS(n,t)==4)then
                    t_ENold(iEN) = t
                    n_ENold(iEN) = n
                    hout_ENold(iEN) = sim_hsp(n,t)
                    hemp_ENold(iEN) = sim_hh(n,t)
                    ass_ENold(iEN) = assgrid(sim_asset(n,t))

                    a_p = ap_ind(apol_EN(sim_asset(n,t),sim_hh(n,t),sim_hsp(n,t),t)) ! asset choice
                    lambda_ENold_E(iEN) = lambda_NE_E(a_p,sim_hsp(n,t),sim_hh(n,t),t)
                    lambda_ENold_X(iEN) = lambda_NE_X(a_p,sim_hsp(n,t),sim_hh(n,t),t)

                    iEN=iEN+1
                elseif (sim_LS(n,t)==5) then 
                    t_NEold(iNE) = t
                    n_NEold(iNE) = n
                    hout_NEold(iNE) = sim_hh(n,t)
                    hemp_NEold(iNE) = sim_hsp(n,t)
                    ass_NEold(iNE) = assgrid(sim_asset(n,t))

                    a_p = ap_ind(apol_NE(sim_asset(n,t),sim_hh(n,t),sim_hsp(n,t),t)) ! asset choice
                    lambda_NEold_E(iNE) = lambda_NE_E(a_p,sim_hh(n,t),sim_hsp(n,t),t)! PB head has job next period
                    lambda_NEold_X(iNE) = lambda_NE_X(a_p,sim_hh(n,t),sim_hsp(n,t),t)! PB head has job next period

                    iNE=iNE+1
                end if
            end do
        end do

        ! compute averages and difference
        hemp_y = (sum(dble(hemp_ENyoung)) + sum(dble(hemp_NEyoung)))/(dble(NN_ENyoung)+dble(NN_NEyoung))
        hout_y = (sum(dble(hout_ENyoung)) + sum(dble(hout_NEyoung)))/(dble(NN_ENyoung)+dble(NN_NEyoung))
        ass_y = (sum((ass_ENyoung)) + sum((ass_NEyoung)))/(dble(NN_ENyoung)+dble(NN_NEyoung))
        lambda_y_E = (sum((lambda_ENyoung_E)) + sum((lambda_ENyoung_E)))/(dble(NN_ENyoung)+dble(NN_NEyoung))
        lambda_y_X = (sum((lambda_ENyoung_X)) + sum((lambda_ENyoung_X)))/(dble(NN_ENyoung)+dble(NN_NEyoung))

        hemp_o = (sum(dble(hemp_ENold)) + sum(dble(hemp_NEold)))/(dble(NN_ENold)+dble(NN_NEold))
        hout_o = (sum(dble(hout_ENold)) + sum(dble(hout_NEold)))/(dble(NN_ENold)+dble(NN_NEold))
        ass_o = (sum((ass_ENold)) + sum((ass_NEold)))/(dble(NN_ENold)+dble(NN_NEold))
        lambda_o_E = (sum((lambda_ENold_E)) + sum((lambda_ENold_E)))/(dble(NN_ENold)+dble(NN_NEold))
        lambda_o_X = (sum((lambda_ENold_X)) + sum((lambda_ENold_X)))/(dble(NN_ENold)+dble(NN_NEold))

        ass_ratio = ass_o/ass_y
        lambda_ratio_E = lambda_o_E/lambda_y_E
        lambda_ratio_X = lambda_o_X/lambda_y_X

        if ( ((hemp_o-hemp_y).le.0d0).AND.((hemp_o-hemp_y).ge.-1d0) ) then
            hemp_diff = -1
        else if ( ((hemp_o-hemp_y).ge.0d0).AND.((hemp_o-hemp_y).le.1d0) ) then
            hemp_diff = 1
        else
            hemp_diff=NINT(hemp_o-hemp_y)
        end if

        if ( ((hout_o-hout_y).le.0d0).AND.((hout_o-hout_y).ge.-1d0) ) then
            hout_diff = -1
        else if ( ((hout_o-hout_y).ge.0d0).AND.((hout_o-hout_y).le.1d0) ) then
            hout_diff = 1
        else
            hout_diff=NINT(hout_o-hout_y)
        end if

        print *, 'hemp_y',hemp_y
        print *, 'hout_y',hout_y
        print *, 'ass_y',ass_y
        print *, 'lambda_y_E',lambda_y_E
        print *, 'lambda_y_X',lambda_y_X
        print *, 'hemp_o',hemp_o
        print *, 'hout_o',hout_o
        print *, 'ass_o',ass_o
        print *, 'lambda_o_E',lambda_o_E
        print *, 'lambda_o_X',lambda_o_X
        print *, 'hemp_diff',hemp_diff
        print *, 'hout_diff',hout_diff
        print *, 'ass_ratio',ass_ratio
        print *, 'lambda_ratio_E',lambda_ratio_E
        print *, 'lambda_ratio_X',lambda_ratio_X

        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'differences done!'
        close(101)
        
        ! draw state transitions

        do ii=1,(max(NN_ENold,NN_ENyoung))

            ! human capital head
            step_hh_EN(ii) = rand()
            
            ! human capital spouse
            step_hsp_EN(ii) = rand()

            ! job offer head
            step_jobh_EN(ii) = rand()

            ! job offer spouse
            step_jobsp_EN(ii) = rand()

            ! LS
            step_LS_EN(ii) = rand()
            
        end do

        do ii=1,(max(NN_NEold,NN_NEyoung))

            ! human capital head
            step_hh_NE(ii) = rand()
            
            ! human capital spouse
            step_hsp_NE(ii) = rand()

            ! job offer head
            step_jobh_NE(ii) = rand()

            ! job offer spouse
            step_jobsp_NE(ii) = rand()

            ! LS
            step_LS_NE(ii) = rand()
            
        end do

        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'draws done!'
        close(101)


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE old
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_o_base=0

        do ii=1,NN_ENold

            
            hh_s = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state
            hsp_s = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state
            t_s = t_ENold(ii)! relevant age
            a_s = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice
            indEN = 1
            sep = (delta(t_s,hh_s))! PB head has job next period
            lambda_E = lambda_NE_E(a_p,hsp_s,hh_s,t_s)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p,hsp_s,hh_s,t_s)! PB spouse has job next period
            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition

            

            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_o_base(1,1) = AWE_o_base(1,1) + AWE11
            AWE_o_base(1,2) = AWE_o_base(1,2) + AWE12
            AWE_o_base(1,3) = AWE_o_base(1,3) + AWE13
            AWE_o_base(1,4) = AWE_o_base(1,4) + AWE14
            AWE_o_base(2,1) = AWE_o_base(2,1) + AWE21
            AWE_o_base(2,2) = AWE_o_base(2,2) + AWE22
            AWE_o_base(2,3) = AWE_o_base(2,3) + AWE23
            AWE_o_base(2,4) = AWE_o_base(2,4) + AWE24
            AWE_o_base(3,1) = AWE_o_base(3,1) + AWE31
            AWE_o_base(3,2) = AWE_o_base(3,2) + AWE32
            AWE_o_base(3,3) = AWE_o_base(3,3) + AWE33
            AWE_o_base(3,4) = AWE_o_base(3,4) + AWE34
            AWE_o_base(4,1) = AWE_o_base(4,1) + AWE41
            AWE_o_base(4,2) = AWE_o_base(4,2) + AWE42
            AWE_o_base(4,3) = AWE_o_base(4,3) + AWE43
            AWE_o_base(4,4) = AWE_o_base(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o base EN done!'
        close(101)
        do ii=1,NN_NEold

            
            hh_s = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state
            hsp_s = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state
            t_s = t_NEold(ii)! relevant age
            a_s = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p,hh_s,hsp_s,t_s)! PB head has job next period
            lambda_X = lambda_NE_X(a_p,hh_s,hsp_s,t_s)! PB head has job next period
            sep = (delta(t_s,hsp_s))! PB spouse has job next period
            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition

            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 
            
            AWE_o_base(1,1) = AWE_o_base(1,1) + AWE11
            AWE_o_base(1,2) = AWE_o_base(1,2) + AWE12
            AWE_o_base(1,3) = AWE_o_base(1,3) + AWE13
            AWE_o_base(1,4) = AWE_o_base(1,4) + AWE14
            AWE_o_base(2,1) = AWE_o_base(2,1) + AWE21
            AWE_o_base(2,2) = AWE_o_base(2,2) + AWE22
            AWE_o_base(2,3) = AWE_o_base(2,3) + AWE23
            AWE_o_base(2,4) = AWE_o_base(2,4) + AWE24
            AWE_o_base(3,1) = AWE_o_base(3,1) + AWE31
            AWE_o_base(3,2) = AWE_o_base(3,2) + AWE32
            AWE_o_base(3,3) = AWE_o_base(3,3) + AWE33
            AWE_o_base(3,4) = AWE_o_base(3,4) + AWE34
            AWE_o_base(4,1) = AWE_o_base(4,1) + AWE41
            AWE_o_base(4,2) = AWE_o_base(4,2) + AWE42
            AWE_o_base(4,3) = AWE_o_base(4,3) + AWE43
            AWE_o_base(4,4) = AWE_o_base(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o base NE done!'
        close(101)


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE arrival rates (age)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_o_lambda_age=0

        do ii=1,NN_ENold

            
            hh_s = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state
            hsp_s = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state
            t_s = t_ENold(ii)! relevant age
            a_s = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENold(ii)-(30*12)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_ENold(ii))) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_o_lambda_age(1,1) = AWE_o_lambda_age(1,1) + AWE11
            AWE_o_lambda_age(1,2) = AWE_o_lambda_age(1,2) + AWE12
            AWE_o_lambda_age(1,3) = AWE_o_lambda_age(1,3) + AWE13
            AWE_o_lambda_age(1,4) = AWE_o_lambda_age(1,4) + AWE14
            AWE_o_lambda_age(2,1) = AWE_o_lambda_age(2,1) + AWE21
            AWE_o_lambda_age(2,2) = AWE_o_lambda_age(2,2) + AWE22
            AWE_o_lambda_age(2,3) = AWE_o_lambda_age(2,3) + AWE23
            AWE_o_lambda_age(2,4) = AWE_o_lambda_age(2,4) + AWE24
            AWE_o_lambda_age(3,1) = AWE_o_lambda_age(3,1) + AWE31
            AWE_o_lambda_age(3,2) = AWE_o_lambda_age(3,2) + AWE32
            AWE_o_lambda_age(3,3) = AWE_o_lambda_age(3,3) + AWE33
            AWE_o_lambda_age(3,4) = AWE_o_lambda_age(3,4) + AWE34
            AWE_o_lambda_age(4,1) = AWE_o_lambda_age(4,1) + AWE41
            AWE_o_lambda_age(4,2) = AWE_o_lambda_age(4,2) + AWE42
            AWE_o_lambda_age(4,3) = AWE_o_lambda_age(4,3) + AWE43
            AWE_o_lambda_age(4,4) = AWE_o_lambda_age(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o lambda age EN done!'
        close(101)
        do ii=1,NN_NEold

            
            hh_s = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state
            hsp_s = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state
            t_s = t_NEold(ii)! relevant age
            a_s = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEold(ii)-(30*12)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_NEold(ii))) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_o_lambda_age(1,1) = AWE_o_lambda_age(1,1) + AWE11
            AWE_o_lambda_age(1,2) = AWE_o_lambda_age(1,2) + AWE12
            AWE_o_lambda_age(1,3) = AWE_o_lambda_age(1,3) + AWE13
            AWE_o_lambda_age(1,4) = AWE_o_lambda_age(1,4) + AWE14
            AWE_o_lambda_age(2,1) = AWE_o_lambda_age(2,1) + AWE21
            AWE_o_lambda_age(2,2) = AWE_o_lambda_age(2,2) + AWE22
            AWE_o_lambda_age(2,3) = AWE_o_lambda_age(2,3) + AWE23
            AWE_o_lambda_age(2,4) = AWE_o_lambda_age(2,4) + AWE24
            AWE_o_lambda_age(3,1) = AWE_o_lambda_age(3,1) + AWE31
            AWE_o_lambda_age(3,2) = AWE_o_lambda_age(3,2) + AWE32
            AWE_o_lambda_age(3,3) = AWE_o_lambda_age(3,3) + AWE33
            AWE_o_lambda_age(3,4) = AWE_o_lambda_age(3,4) + AWE34
            AWE_o_lambda_age(4,1) = AWE_o_lambda_age(4,1) + AWE41
            AWE_o_lambda_age(4,2) = AWE_o_lambda_age(4,2) + AWE42
            AWE_o_lambda_age(4,3) = AWE_o_lambda_age(4,3) + AWE43
            AWE_o_lambda_age(4,4) = AWE_o_lambda_age(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o lambda age NE done!'
        close(101)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE arrival rates
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_o_lambda=0

        do ii=1,NN_ENold

            
            hh_s = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state
            hsp_s = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state
            t_s = t_ENold(ii)! relevant age
            a_s = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENold(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_ENold(ii))) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = min(max(lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)/lambda_ratio_E,0d0),1d0)! PB spouse has job next period
            lambda_X = min(max(lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)/lambda_ratio_X,0d0),1d0)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_o_lambda(1,1) = AWE_o_lambda(1,1) + AWE11
            AWE_o_lambda(1,2) = AWE_o_lambda(1,2) + AWE12
            AWE_o_lambda(1,3) = AWE_o_lambda(1,3) + AWE13
            AWE_o_lambda(1,4) = AWE_o_lambda(1,4) + AWE14
            AWE_o_lambda(2,1) = AWE_o_lambda(2,1) + AWE21
            AWE_o_lambda(2,2) = AWE_o_lambda(2,2) + AWE22
            AWE_o_lambda(2,3) = AWE_o_lambda(2,3) + AWE23
            AWE_o_lambda(2,4) = AWE_o_lambda(2,4) + AWE24
            AWE_o_lambda(3,1) = AWE_o_lambda(3,1) + AWE31
            AWE_o_lambda(3,2) = AWE_o_lambda(3,2) + AWE32
            AWE_o_lambda(3,3) = AWE_o_lambda(3,3) + AWE33
            AWE_o_lambda(3,4) = AWE_o_lambda(3,4) + AWE34
            AWE_o_lambda(4,1) = AWE_o_lambda(4,1) + AWE41
            AWE_o_lambda(4,2) = AWE_o_lambda(4,2) + AWE42
            AWE_o_lambda(4,3) = AWE_o_lambda(4,3) + AWE43
            AWE_o_lambda(4,4) = AWE_o_lambda(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o lambda EN done!'
        close(101)
        do ii=1,NN_NEold

            
            hh_s = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state
            hsp_s = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state
            t_s = t_NEold(ii)! relevant age
            a_s = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEold(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_NEold(ii))) ! asset choice
            indEN = 2
            lambda_E = min(max(lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)/lambda_ratio_E,0d0),1d0) ! PB head has job next period
            lambda_X = min(max(lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)/lambda_ratio_X,0d0),1d0) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_o_lambda(1,1) = AWE_o_lambda(1,1) + AWE11
            AWE_o_lambda(1,2) = AWE_o_lambda(1,2) + AWE12
            AWE_o_lambda(1,3) = AWE_o_lambda(1,3) + AWE13
            AWE_o_lambda(1,4) = AWE_o_lambda(1,4) + AWE14
            AWE_o_lambda(2,1) = AWE_o_lambda(2,1) + AWE21
            AWE_o_lambda(2,2) = AWE_o_lambda(2,2) + AWE22
            AWE_o_lambda(2,3) = AWE_o_lambda(2,3) + AWE23
            AWE_o_lambda(2,4) = AWE_o_lambda(2,4) + AWE24
            AWE_o_lambda(3,1) = AWE_o_lambda(3,1) + AWE31
            AWE_o_lambda(3,2) = AWE_o_lambda(3,2) + AWE32
            AWE_o_lambda(3,3) = AWE_o_lambda(3,3) + AWE33
            AWE_o_lambda(3,4) = AWE_o_lambda(3,4) + AWE34
            AWE_o_lambda(4,1) = AWE_o_lambda(4,1) + AWE41
            AWE_o_lambda(4,2) = AWE_o_lambda(4,2) + AWE42
            AWE_o_lambda(4,3) = AWE_o_lambda(4,3) + AWE43
            AWE_o_lambda(4,4) = AWE_o_lambda(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o lambda NE done!'
        close(101)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE hout (without lambda)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_o_hout=0

        do ii=1,NN_ENold

            
            hh_s = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state
            hsp_s = min(max(sim_hsp(n_ENold(ii),t_ENold(ii))-hout_diff,1),HH)! relevant hsp state
            t_s = t_ENold(ii)! relevant age
            a_s = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENold(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_ENold(ii))) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period


            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_o_hout(1,1) = AWE_o_hout(1,1) + AWE11
            AWE_o_hout(1,2) = AWE_o_hout(1,2) + AWE12
            AWE_o_hout(1,3) = AWE_o_hout(1,3) + AWE13
            AWE_o_hout(1,4) = AWE_o_hout(1,4) + AWE14
            AWE_o_hout(2,1) = AWE_o_hout(2,1) + AWE21
            AWE_o_hout(2,2) = AWE_o_hout(2,2) + AWE22
            AWE_o_hout(2,3) = AWE_o_hout(2,3) + AWE23
            AWE_o_hout(2,4) = AWE_o_hout(2,4) + AWE24
            AWE_o_hout(3,1) = AWE_o_hout(3,1) + AWE31
            AWE_o_hout(3,2) = AWE_o_hout(3,2) + AWE32
            AWE_o_hout(3,3) = AWE_o_hout(3,3) + AWE33
            AWE_o_hout(3,4) = AWE_o_hout(3,4) + AWE34
            AWE_o_hout(4,1) = AWE_o_hout(4,1) + AWE41
            AWE_o_hout(4,2) = AWE_o_hout(4,2) + AWE42
            AWE_o_hout(4,3) = AWE_o_hout(4,3) + AWE43
            AWE_o_hout(4,4) = AWE_o_hout(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o hout EN done!'
        close(101)
        do ii=1,NN_NEold

            
            hh_s = min(max(sim_hh(n_NEold(ii),t_NEold(ii))-hout_diff,1),HH)! relevant hh state
            hsp_s = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state
            t_s = t_NEold(ii)! relevant age
            a_s = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEold(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_NEold(ii))) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period
            
            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 )  

            AWE_o_hout(1,1) = AWE_o_hout(1,1) + AWE11
            AWE_o_hout(1,2) = AWE_o_hout(1,2) + AWE12
            AWE_o_hout(1,3) = AWE_o_hout(1,3) + AWE13
            AWE_o_hout(1,4) = AWE_o_hout(1,4) + AWE14
            AWE_o_hout(2,1) = AWE_o_hout(2,1) + AWE21
            AWE_o_hout(2,2) = AWE_o_hout(2,2) + AWE22
            AWE_o_hout(2,3) = AWE_o_hout(2,3) + AWE23
            AWE_o_hout(2,4) = AWE_o_hout(2,4) + AWE24
            AWE_o_hout(3,1) = AWE_o_hout(3,1) + AWE31
            AWE_o_hout(3,2) = AWE_o_hout(3,2) + AWE32
            AWE_o_hout(3,3) = AWE_o_hout(3,3) + AWE33
            AWE_o_hout(3,4) = AWE_o_hout(3,4) + AWE34
            AWE_o_hout(4,1) = AWE_o_hout(4,1) + AWE41
            AWE_o_hout(4,2) = AWE_o_hout(4,2) + AWE42
            AWE_o_hout(4,3) = AWE_o_hout(4,3) + AWE43
            AWE_o_hout(4,4) = AWE_o_hout(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o hout NE done!'
        close(101)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE hout (with lambda)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_o_hout_lambda=0

        do ii=1,NN_ENold

            
            hh_s = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state
            hsp_s = min(max(sim_hsp(n_ENold(ii),t_ENold(ii))-hout_diff,1),HH)! relevant hsp state
            t_s = t_ENold(ii)! relevant age
            a_s = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = min(max(sim_hsp(n_ENold(ii),t_ENold(ii))-hout_diff,1),HH)! relevant hsp state for arrival rates
            t_s_arrv = t_ENold(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_ENold(ii))) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period


            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_o_hout_lambda(1,1) = AWE_o_hout_lambda(1,1) + AWE11
            AWE_o_hout_lambda(1,2) = AWE_o_hout_lambda(1,2) + AWE12
            AWE_o_hout_lambda(1,3) = AWE_o_hout_lambda(1,3) + AWE13
            AWE_o_hout_lambda(1,4) = AWE_o_hout_lambda(1,4) + AWE14
            AWE_o_hout_lambda(2,1) = AWE_o_hout_lambda(2,1) + AWE21
            AWE_o_hout_lambda(2,2) = AWE_o_hout_lambda(2,2) + AWE22
            AWE_o_hout_lambda(2,3) = AWE_o_hout_lambda(2,3) + AWE23
            AWE_o_hout_lambda(2,4) = AWE_o_hout_lambda(2,4) + AWE24
            AWE_o_hout_lambda(3,1) = AWE_o_hout_lambda(3,1) + AWE31
            AWE_o_hout_lambda(3,2) = AWE_o_hout_lambda(3,2) + AWE32
            AWE_o_hout_lambda(3,3) = AWE_o_hout_lambda(3,3) + AWE33
            AWE_o_hout_lambda(3,4) = AWE_o_hout_lambda(3,4) + AWE34
            AWE_o_hout_lambda(4,1) = AWE_o_hout_lambda(4,1) + AWE41
            AWE_o_hout_lambda(4,2) = AWE_o_hout_lambda(4,2) + AWE42
            AWE_o_hout_lambda(4,3) = AWE_o_hout_lambda(4,3) + AWE43
            AWE_o_hout_lambda(4,4) = AWE_o_hout_lambda(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o hout lambda EN done!'
        close(101)
        do ii=1,NN_NEold

            
            hh_s = min(max(sim_hh(n_NEold(ii),t_NEold(ii))-hout_diff,1),HH)! relevant hh state
            hsp_s = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state
            t_s = t_NEold(ii)! relevant age
            a_s = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = min(max(sim_hh(n_NEold(ii),t_NEold(ii))-hout_diff,1),HH)! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEold(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_NEold(ii))) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period


            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 )  

            AWE_o_hout_lambda(1,1) = AWE_o_hout_lambda(1,1) + AWE11
            AWE_o_hout_lambda(1,2) = AWE_o_hout_lambda(1,2) + AWE12
            AWE_o_hout_lambda(1,3) = AWE_o_hout_lambda(1,3) + AWE13
            AWE_o_hout_lambda(1,4) = AWE_o_hout_lambda(1,4) + AWE14
            AWE_o_hout_lambda(2,1) = AWE_o_hout_lambda(2,1) + AWE21
            AWE_o_hout_lambda(2,2) = AWE_o_hout_lambda(2,2) + AWE22
            AWE_o_hout_lambda(2,3) = AWE_o_hout_lambda(2,3) + AWE23
            AWE_o_hout_lambda(2,4) = AWE_o_hout_lambda(2,4) + AWE24
            AWE_o_hout_lambda(3,1) = AWE_o_hout_lambda(3,1) + AWE31
            AWE_o_hout_lambda(3,2) = AWE_o_hout_lambda(3,2) + AWE32
            AWE_o_hout_lambda(3,3) = AWE_o_hout_lambda(3,3) + AWE33
            AWE_o_hout_lambda(3,4) = AWE_o_hout_lambda(3,4) + AWE34
            AWE_o_hout_lambda(4,1) = AWE_o_hout_lambda(4,1) + AWE41
            AWE_o_hout_lambda(4,2) = AWE_o_hout_lambda(4,2) + AWE42
            AWE_o_hout_lambda(4,3) = AWE_o_hout_lambda(4,3) + AWE43
            AWE_o_hout_lambda(4,4) = AWE_o_hout_lambda(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o hout lambda NE done!'
        close(101)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE hemp
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_o_hemp=0

        do ii=1,NN_ENold

            
            hh_s = min(max(sim_hh(n_ENold(ii),t_ENold(ii))-hemp_diff,1),HH)! relevant hh state
            hsp_s = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state
            t_s = t_ENold(ii)! relevant age
            a_s = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice
            
            hh_s_arrv = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENold(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_o_hemp(1,1) = AWE_o_hemp(1,1) + AWE11
            AWE_o_hemp(1,2) = AWE_o_hemp(1,2) + AWE12
            AWE_o_hemp(1,3) = AWE_o_hemp(1,3) + AWE13
            AWE_o_hemp(1,4) = AWE_o_hemp(1,4) + AWE14
            AWE_o_hemp(2,1) = AWE_o_hemp(2,1) + AWE21
            AWE_o_hemp(2,2) = AWE_o_hemp(2,2) + AWE22
            AWE_o_hemp(2,3) = AWE_o_hemp(2,3) + AWE23
            AWE_o_hemp(2,4) = AWE_o_hemp(2,4) + AWE24
            AWE_o_hemp(3,1) = AWE_o_hemp(3,1) + AWE31
            AWE_o_hemp(3,2) = AWE_o_hemp(3,2) + AWE32
            AWE_o_hemp(3,3) = AWE_o_hemp(3,3) + AWE33
            AWE_o_hemp(3,4) = AWE_o_hemp(3,4) + AWE34
            AWE_o_hemp(4,1) = AWE_o_hemp(4,1) + AWE41
            AWE_o_hemp(4,2) = AWE_o_hemp(4,2) + AWE42
            AWE_o_hemp(4,3) = AWE_o_hemp(4,3) + AWE43
            AWE_o_hemp(4,4) = AWE_o_hemp(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o hemp EN done!'
        close(101)
        do ii=1,NN_NEold

            
            hh_s = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state
            hsp_s = min(max(sim_hsp(n_NEold(ii),t_NEold(ii))-hemp_diff,1),HH)! relevant hsp state
            t_s = t_NEold(ii)! relevant age
            a_s = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice
            
            hh_s_arrv = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEold(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_o_hemp(1,1) = AWE_o_hemp(1,1) + AWE11
            AWE_o_hemp(1,2) = AWE_o_hemp(1,2) + AWE12
            AWE_o_hemp(1,3) = AWE_o_hemp(1,3) + AWE13
            AWE_o_hemp(1,4) = AWE_o_hemp(1,4) + AWE14
            AWE_o_hemp(2,1) = AWE_o_hemp(2,1) + AWE21
            AWE_o_hemp(2,2) = AWE_o_hemp(2,2) + AWE22
            AWE_o_hemp(2,3) = AWE_o_hemp(2,3) + AWE23
            AWE_o_hemp(2,4) = AWE_o_hemp(2,4) + AWE24
            AWE_o_hemp(3,1) = AWE_o_hemp(3,1) + AWE31
            AWE_o_hemp(3,2) = AWE_o_hemp(3,2) + AWE32
            AWE_o_hemp(3,3) = AWE_o_hemp(3,3) + AWE33
            AWE_o_hemp(3,4) = AWE_o_hemp(3,4) + AWE34
            AWE_o_hemp(4,1) = AWE_o_hemp(4,1) + AWE41
            AWE_o_hemp(4,2) = AWE_o_hemp(4,2) + AWE42
            AWE_o_hemp(4,3) = AWE_o_hemp(4,3) + AWE43
            AWE_o_hemp(4,4) = AWE_o_hemp(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o hemp NE done!'
        close(101)
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE hemp (with lambda)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_o_hemp_lambda=0

        do ii=1,NN_ENold

            
            hh_s = min(max(sim_hh(n_ENold(ii),t_ENold(ii))-hemp_diff,1),HH)! relevant hh state
            hsp_s = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state
            t_s = t_ENold(ii)! relevant age
            a_s = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice
            
            hh_s_arrv = min(max(sim_hh(n_ENold(ii),t_ENold(ii))-hemp_diff,1),HH)! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENold(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_o_hemp_lambda(1,1) = AWE_o_hemp_lambda(1,1) + AWE11
            AWE_o_hemp_lambda(1,2) = AWE_o_hemp_lambda(1,2) + AWE12
            AWE_o_hemp_lambda(1,3) = AWE_o_hemp_lambda(1,3) + AWE13
            AWE_o_hemp_lambda(1,4) = AWE_o_hemp_lambda(1,4) + AWE14
            AWE_o_hemp_lambda(2,1) = AWE_o_hemp_lambda(2,1) + AWE21
            AWE_o_hemp_lambda(2,2) = AWE_o_hemp_lambda(2,2) + AWE22
            AWE_o_hemp_lambda(2,3) = AWE_o_hemp_lambda(2,3) + AWE23
            AWE_o_hemp_lambda(2,4) = AWE_o_hemp_lambda(2,4) + AWE24
            AWE_o_hemp_lambda(3,1) = AWE_o_hemp_lambda(3,1) + AWE31
            AWE_o_hemp_lambda(3,2) = AWE_o_hemp_lambda(3,2) + AWE32
            AWE_o_hemp_lambda(3,3) = AWE_o_hemp_lambda(3,3) + AWE33
            AWE_o_hemp_lambda(3,4) = AWE_o_hemp_lambda(3,4) + AWE34
            AWE_o_hemp_lambda(4,1) = AWE_o_hemp_lambda(4,1) + AWE41
            AWE_o_hemp_lambda(4,2) = AWE_o_hemp_lambda(4,2) + AWE42
            AWE_o_hemp_lambda(4,3) = AWE_o_hemp_lambda(4,3) + AWE43
            AWE_o_hemp_lambda(4,4) = AWE_o_hemp_lambda(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o hemp EN done!'
        close(101)
        do ii=1,NN_NEold

            
            hh_s = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state
            hsp_s = min(max(sim_hsp(n_NEold(ii),t_NEold(ii))-hemp_diff,1),HH)! relevant hsp state
            t_s = t_NEold(ii)! relevant age
            a_s = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice
            
            hh_s_arrv = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = min(max(sim_hsp(n_NEold(ii),t_NEold(ii))-hemp_diff,1),HH)! relevant hsp state for arrival rates
            t_s_arrv = t_NEold(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_o_hemp_lambda(1,1) = AWE_o_hemp_lambda(1,1) + AWE11
            AWE_o_hemp_lambda(1,2) = AWE_o_hemp_lambda(1,2) + AWE12
            AWE_o_hemp_lambda(1,3) = AWE_o_hemp_lambda(1,3) + AWE13
            AWE_o_hemp_lambda(1,4) = AWE_o_hemp_lambda(1,4) + AWE14
            AWE_o_hemp_lambda(2,1) = AWE_o_hemp_lambda(2,1) + AWE21
            AWE_o_hemp_lambda(2,2) = AWE_o_hemp_lambda(2,2) + AWE22
            AWE_o_hemp_lambda(2,3) = AWE_o_hemp_lambda(2,3) + AWE23
            AWE_o_hemp_lambda(2,4) = AWE_o_hemp_lambda(2,4) + AWE24
            AWE_o_hemp_lambda(3,1) = AWE_o_hemp_lambda(3,1) + AWE31
            AWE_o_hemp_lambda(3,2) = AWE_o_hemp_lambda(3,2) + AWE32
            AWE_o_hemp_lambda(3,3) = AWE_o_hemp_lambda(3,3) + AWE33
            AWE_o_hemp_lambda(3,4) = AWE_o_hemp_lambda(3,4) + AWE34
            AWE_o_hemp_lambda(4,1) = AWE_o_hemp_lambda(4,1) + AWE41
            AWE_o_hemp_lambda(4,2) = AWE_o_hemp_lambda(4,2) + AWE42
            AWE_o_hemp_lambda(4,3) = AWE_o_hemp_lambda(4,3) + AWE43
            AWE_o_hemp_lambda(4,4) = AWE_o_hemp_lambda(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o hemp NE done!'
        close(101)
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE age
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_o_age=0

        do ii=1,NN_ENold

            
            hh_s = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state
            hsp_s = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state
            t_s = t_ENold(ii)-(30*12)! relevant age
            a_s = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice
            
            hh_s_arrv = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENold(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 
             
            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 
       

            AWE_o_age(1,1) = AWE_o_age(1,1) + AWE11
            AWE_o_age(1,2) = AWE_o_age(1,2) + AWE12
            AWE_o_age(1,3) = AWE_o_age(1,3) + AWE13
            AWE_o_age(1,4) = AWE_o_age(1,4) + AWE14
            AWE_o_age(2,1) = AWE_o_age(2,1) + AWE21
            AWE_o_age(2,2) = AWE_o_age(2,2) + AWE22
            AWE_o_age(2,3) = AWE_o_age(2,3) + AWE23
            AWE_o_age(2,4) = AWE_o_age(2,4) + AWE24
            AWE_o_age(3,1) = AWE_o_age(3,1) + AWE31
            AWE_o_age(3,2) = AWE_o_age(3,2) + AWE32
            AWE_o_age(3,3) = AWE_o_age(3,3) + AWE33
            AWE_o_age(3,4) = AWE_o_age(3,4) + AWE34
            AWE_o_age(4,1) = AWE_o_age(4,1) + AWE41
            AWE_o_age(4,2) = AWE_o_age(4,2) + AWE42
            AWE_o_age(4,3) = AWE_o_age(4,3) + AWE43
            AWE_o_age(4,4) = AWE_o_age(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o age EN done!'
        close(101)
        do ii=1,NN_NEold

            
            hh_s = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state
            hsp_s = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state
            t_s = t_NEold(ii)-(30*12)! relevant age
            a_s = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice
            
            hh_s_arrv = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEold(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period
            
            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_o_age(1,1) = AWE_o_age(1,1) + AWE11
            AWE_o_age(1,2) = AWE_o_age(1,2) + AWE12
            AWE_o_age(1,3) = AWE_o_age(1,3) + AWE13
            AWE_o_age(1,4) = AWE_o_age(1,4) + AWE14
            AWE_o_age(2,1) = AWE_o_age(2,1) + AWE21
            AWE_o_age(2,2) = AWE_o_age(2,2) + AWE22
            AWE_o_age(2,3) = AWE_o_age(2,3) + AWE23
            AWE_o_age(2,4) = AWE_o_age(2,4) + AWE24
            AWE_o_age(3,1) = AWE_o_age(3,1) + AWE31
            AWE_o_age(3,2) = AWE_o_age(3,2) + AWE32
            AWE_o_age(3,3) = AWE_o_age(3,3) + AWE33
            AWE_o_age(3,4) = AWE_o_age(3,4) + AWE34
            AWE_o_age(4,1) = AWE_o_age(4,1) + AWE41
            AWE_o_age(4,2) = AWE_o_age(4,2) + AWE42
            AWE_o_age(4,3) = AWE_o_age(4,3) + AWE43
            AWE_o_age(4,4) = AWE_o_age(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o age NE done!'
        close(101)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE age (with lambda)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_o_age_lambda=0

        do ii=1,NN_ENold

            
            hh_s = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state
            hsp_s = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state
            t_s = t_ENold(ii)-(30*12)! relevant age
            a_s = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice
            
            hh_s_arrv = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENold(ii)-(30*12)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 
             
            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 
       

            AWE_o_age_lambda(1,1) = AWE_o_age_lambda(1,1) + AWE11
            AWE_o_age_lambda(1,2) = AWE_o_age_lambda(1,2) + AWE12
            AWE_o_age_lambda(1,3) = AWE_o_age_lambda(1,3) + AWE13
            AWE_o_age_lambda(1,4) = AWE_o_age_lambda(1,4) + AWE14
            AWE_o_age_lambda(2,1) = AWE_o_age_lambda(2,1) + AWE21
            AWE_o_age_lambda(2,2) = AWE_o_age_lambda(2,2) + AWE22
            AWE_o_age_lambda(2,3) = AWE_o_age_lambda(2,3) + AWE23
            AWE_o_age_lambda(2,4) = AWE_o_age_lambda(2,4) + AWE24
            AWE_o_age_lambda(3,1) = AWE_o_age_lambda(3,1) + AWE31
            AWE_o_age_lambda(3,2) = AWE_o_age_lambda(3,2) + AWE32
            AWE_o_age_lambda(3,3) = AWE_o_age_lambda(3,3) + AWE33
            AWE_o_age_lambda(3,4) = AWE_o_age_lambda(3,4) + AWE34
            AWE_o_age_lambda(4,1) = AWE_o_age_lambda(4,1) + AWE41
            AWE_o_age_lambda(4,2) = AWE_o_age_lambda(4,2) + AWE42
            AWE_o_age_lambda(4,3) = AWE_o_age_lambda(4,3) + AWE43
            AWE_o_age_lambda(4,4) = AWE_o_age_lambda(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o age EN done!'
        close(101)
        do ii=1,NN_NEold

            
            hh_s = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state
            hsp_s = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state
            t_s = t_NEold(ii)-(30*12)! relevant age
            a_s = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice
            
            hh_s_arrv = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEold(ii)-(30*12)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period
            
            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_o_age_lambda(1,1) = AWE_o_age_lambda(1,1) + AWE11
            AWE_o_age_lambda(1,2) = AWE_o_age_lambda(1,2) + AWE12
            AWE_o_age_lambda(1,3) = AWE_o_age_lambda(1,3) + AWE13
            AWE_o_age_lambda(1,4) = AWE_o_age_lambda(1,4) + AWE14
            AWE_o_age_lambda(2,1) = AWE_o_age_lambda(2,1) + AWE21
            AWE_o_age_lambda(2,2) = AWE_o_age_lambda(2,2) + AWE22
            AWE_o_age_lambda(2,3) = AWE_o_age_lambda(2,3) + AWE23
            AWE_o_age_lambda(2,4) = AWE_o_age_lambda(2,4) + AWE24
            AWE_o_age_lambda(3,1) = AWE_o_age_lambda(3,1) + AWE31
            AWE_o_age_lambda(3,2) = AWE_o_age_lambda(3,2) + AWE32
            AWE_o_age_lambda(3,3) = AWE_o_age_lambda(3,3) + AWE33
            AWE_o_age_lambda(3,4) = AWE_o_age_lambda(3,4) + AWE34
            AWE_o_age_lambda(4,1) = AWE_o_age_lambda(4,1) + AWE41
            AWE_o_age_lambda(4,2) = AWE_o_age_lambda(4,2) + AWE42
            AWE_o_age_lambda(4,3) = AWE_o_age_lambda(4,3) + AWE43
            AWE_o_age_lambda(4,4) = AWE_o_age_lambda(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o age NE done!'
        close(101)


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE assets ratio
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_o_ass=0

        do ii=1,NN_ENold

            
            hh_s = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state
            hsp_s = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state
            t_s = t_ENold(ii)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_ENold(ii),t_ENold(ii)))/ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENold(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_o_ass(1,1) = AWE_o_ass(1,1) + AWE11
            AWE_o_ass(1,2) = AWE_o_ass(1,2) + AWE12
            AWE_o_ass(1,3) = AWE_o_ass(1,3) + AWE13
            AWE_o_ass(1,4) = AWE_o_ass(1,4) + AWE14
            AWE_o_ass(2,1) = AWE_o_ass(2,1) + AWE21
            AWE_o_ass(2,2) = AWE_o_ass(2,2) + AWE22
            AWE_o_ass(2,3) = AWE_o_ass(2,3) + AWE23
            AWE_o_ass(2,4) = AWE_o_ass(2,4) + AWE24
            AWE_o_ass(3,1) = AWE_o_ass(3,1) + AWE31
            AWE_o_ass(3,2) = AWE_o_ass(3,2) + AWE32
            AWE_o_ass(3,3) = AWE_o_ass(3,3) + AWE33
            AWE_o_ass(3,4) = AWE_o_ass(3,4) + AWE34
            AWE_o_ass(4,1) = AWE_o_ass(4,1) + AWE41
            AWE_o_ass(4,2) = AWE_o_ass(4,2) + AWE42
            AWE_o_ass(4,3) = AWE_o_ass(4,3) + AWE43
            AWE_o_ass(4,4) = AWE_o_ass(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o ass EN done!'
        close(101)
        do ii=1,NN_NEold

            
            hh_s = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state
            hsp_s = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state
            t_s = t_NEold(ii)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_NEold(ii),t_NEold(ii)))/ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEold(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 )  

            AWE_o_ass(1,1) = AWE_o_ass(1,1) + AWE11
            AWE_o_ass(1,2) = AWE_o_ass(1,2) + AWE12
            AWE_o_ass(1,3) = AWE_o_ass(1,3) + AWE13
            AWE_o_ass(1,4) = AWE_o_ass(1,4) + AWE14
            AWE_o_ass(2,1) = AWE_o_ass(2,1) + AWE21
            AWE_o_ass(2,2) = AWE_o_ass(2,2) + AWE22
            AWE_o_ass(2,3) = AWE_o_ass(2,3) + AWE23
            AWE_o_ass(2,4) = AWE_o_ass(2,4) + AWE24
            AWE_o_ass(3,1) = AWE_o_ass(3,1) + AWE31
            AWE_o_ass(3,2) = AWE_o_ass(3,2) + AWE32
            AWE_o_ass(3,3) = AWE_o_ass(3,3) + AWE33
            AWE_o_ass(3,4) = AWE_o_ass(3,4) + AWE34
            AWE_o_ass(4,1) = AWE_o_ass(4,1) + AWE41
            AWE_o_ass(4,2) = AWE_o_ass(4,2) + AWE42
            AWE_o_ass(4,3) = AWE_o_ass(4,3) + AWE43
            AWE_o_ass(4,4) = AWE_o_ass(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o ass NE done!'
        close(101)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE assets ratio (with lambda)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_o_ass_lambda=0

        do ii=1,NN_ENold

            
            hh_s = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state
            hsp_s = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state
            t_s = t_ENold(ii)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_ENold(ii),t_ENold(ii)))/ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENold(ii)! relevant age for arrival rates
            a_s_arrv = ap_ind(max(assgrid(sim_asset(n_ENold(ii),t_ENold(ii)))/ass_ratio,0d0))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_o_ass_lambda(1,1) = AWE_o_ass_lambda(1,1) + AWE11
            AWE_o_ass_lambda(1,2) = AWE_o_ass_lambda(1,2) + AWE12
            AWE_o_ass_lambda(1,3) = AWE_o_ass_lambda(1,3) + AWE13
            AWE_o_ass_lambda(1,4) = AWE_o_ass_lambda(1,4) + AWE14
            AWE_o_ass_lambda(2,1) = AWE_o_ass_lambda(2,1) + AWE21
            AWE_o_ass_lambda(2,2) = AWE_o_ass_lambda(2,2) + AWE22
            AWE_o_ass_lambda(2,3) = AWE_o_ass_lambda(2,3) + AWE23
            AWE_o_ass_lambda(2,4) = AWE_o_ass_lambda(2,4) + AWE24
            AWE_o_ass_lambda(3,1) = AWE_o_ass_lambda(3,1) + AWE31
            AWE_o_ass_lambda(3,2) = AWE_o_ass_lambda(3,2) + AWE32
            AWE_o_ass_lambda(3,3) = AWE_o_ass_lambda(3,3) + AWE33
            AWE_o_ass_lambda(3,4) = AWE_o_ass_lambda(3,4) + AWE34
            AWE_o_ass_lambda(4,1) = AWE_o_ass_lambda(4,1) + AWE41
            AWE_o_ass_lambda(4,2) = AWE_o_ass_lambda(4,2) + AWE42
            AWE_o_ass_lambda(4,3) = AWE_o_ass_lambda(4,3) + AWE43
            AWE_o_ass_lambda(4,4) = AWE_o_ass_lambda(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o ass EN done!'
        close(101)
        do ii=1,NN_NEold

            
            hh_s = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state
            hsp_s = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state
            t_s = t_NEold(ii)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_NEold(ii),t_NEold(ii)))/ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEold(ii)! relevant age for arrival rates
            a_s_arrv = ap_ind(max(assgrid(sim_asset(n_NEold(ii),t_NEold(ii)))/ass_ratio,0d0))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_o_ass_lambda(1,1) = AWE_o_ass_lambda(1,1) + AWE11
            AWE_o_ass_lambda(1,2) = AWE_o_ass_lambda(1,2) + AWE12
            AWE_o_ass_lambda(1,3) = AWE_o_ass_lambda(1,3) + AWE13
            AWE_o_ass_lambda(1,4) = AWE_o_ass_lambda(1,4) + AWE14
            AWE_o_ass_lambda(2,1) = AWE_o_ass_lambda(2,1) + AWE21
            AWE_o_ass_lambda(2,2) = AWE_o_ass_lambda(2,2) + AWE22
            AWE_o_ass_lambda(2,3) = AWE_o_ass_lambda(2,3) + AWE23
            AWE_o_ass_lambda(2,4) = AWE_o_ass_lambda(2,4) + AWE24
            AWE_o_ass_lambda(3,1) = AWE_o_ass_lambda(3,1) + AWE31
            AWE_o_ass_lambda(3,2) = AWE_o_ass_lambda(3,2) + AWE32
            AWE_o_ass_lambda(3,3) = AWE_o_ass_lambda(3,3) + AWE33
            AWE_o_ass_lambda(3,4) = AWE_o_ass_lambda(3,4) + AWE34
            AWE_o_ass_lambda(4,1) = AWE_o_ass_lambda(4,1) + AWE41
            AWE_o_ass_lambda(4,2) = AWE_o_ass_lambda(4,2) + AWE42
            AWE_o_ass_lambda(4,3) = AWE_o_ass_lambda(4,3) + AWE43
            AWE_o_ass_lambda(4,4) = AWE_o_ass_lambda(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o ass NE done!'
        close(101)


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE all
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_o_all=0

        do ii=1,NN_ENold

            
            hh_s = min(max(sim_hh(n_ENold(ii),t_ENold(ii))-hemp_diff,1),HH)! relevant hh state
            hsp_s = min(max(sim_hsp(n_ENold(ii),t_ENold(ii))-hout_diff,1),HH)! relevant hsp state
            t_s = t_ENold(ii)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_ENold(ii),t_ENold(ii)))/ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_ENold(ii),t_ENold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENold(ii),t_ENold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENold(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENold(ii),t_ENold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_o_all(1,1) = AWE_o_all(1,1) + AWE11
            AWE_o_all(1,2) = AWE_o_all(1,2) + AWE12
            AWE_o_all(1,3) = AWE_o_all(1,3) + AWE13
            AWE_o_all(1,4) = AWE_o_all(1,4) + AWE14
            AWE_o_all(2,1) = AWE_o_all(2,1) + AWE21
            AWE_o_all(2,2) = AWE_o_all(2,2) + AWE22
            AWE_o_all(2,3) = AWE_o_all(2,3) + AWE23
            AWE_o_all(2,4) = AWE_o_all(2,4) + AWE24
            AWE_o_all(3,1) = AWE_o_all(3,1) + AWE31
            AWE_o_all(3,2) = AWE_o_all(3,2) + AWE32
            AWE_o_all(3,3) = AWE_o_all(3,3) + AWE33
            AWE_o_all(3,4) = AWE_o_all(3,4) + AWE34
            AWE_o_all(4,1) = AWE_o_all(4,1) + AWE41
            AWE_o_all(4,2) = AWE_o_all(4,2) + AWE42
            AWE_o_all(4,3) = AWE_o_all(4,3) + AWE43
            AWE_o_all(4,4) = AWE_o_all(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o ass EN done!'
        close(101)
        do ii=1,NN_NEold

            
            hh_s = min(max(sim_hh(n_NEold(ii),t_NEold(ii))-hout_diff,1),HH)! relevant hh state
            hsp_s = min(max(sim_hsp(n_NEold(ii),t_NEold(ii))-hemp_diff,1),HH)! relevant hsp state
            t_s = t_NEold(ii)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_NEold(ii),t_NEold(ii)))/ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_NEold(ii),t_NEold(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEold(ii),t_NEold(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEold(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEold(ii),t_NEold(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_o_all(1,1) = AWE_o_all(1,1) + AWE11
            AWE_o_all(1,2) = AWE_o_all(1,2) + AWE12
            AWE_o_all(1,3) = AWE_o_all(1,3) + AWE13
            AWE_o_all(1,4) = AWE_o_all(1,4) + AWE14
            AWE_o_all(2,1) = AWE_o_all(2,1) + AWE21
            AWE_o_all(2,2) = AWE_o_all(2,2) + AWE22
            AWE_o_all(2,3) = AWE_o_all(2,3) + AWE23
            AWE_o_all(2,4) = AWE_o_all(2,4) + AWE24
            AWE_o_all(3,1) = AWE_o_all(3,1) + AWE31
            AWE_o_all(3,2) = AWE_o_all(3,2) + AWE32
            AWE_o_all(3,3) = AWE_o_all(3,3) + AWE33
            AWE_o_all(3,4) = AWE_o_all(3,4) + AWE34
            AWE_o_all(4,1) = AWE_o_all(4,1) + AWE41
            AWE_o_all(4,2) = AWE_o_all(4,2) + AWE42
            AWE_o_all(4,3) = AWE_o_all(4,3) + AWE43
            AWE_o_all(4,4) = AWE_o_all(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o ass NE done!'
        close(101)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE all (with lambda)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_o_all_lambda=0

        do ii=1,NN_ENold

            
            hh_s = min(max(sim_hh(n_ENold(ii),t_ENold(ii))-hemp_diff,1),HH)! relevant hh state
            hsp_s = min(max(sim_hsp(n_ENold(ii),t_ENold(ii))-hout_diff,1),HH)! relevant hsp state
            t_s = t_ENold(ii)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_ENold(ii),t_ENold(ii)))/ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = min(max(sim_hh(n_ENold(ii),t_ENold(ii))-hemp_diff,1),HH)! relevant hh state for arrival rates
            hsp_s_arrv = min(max(sim_hsp(n_ENold(ii),t_ENold(ii))-hout_diff,1),HH)! relevant hsp state for arrival rates
            t_s_arrv = t_ENold(ii)! relevant age for arrival rates
            a_s_arrv = ap_ind(max(assgrid(sim_asset(n_ENold(ii),t_ENold(ii)))/ass_ratio,0d0))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_o_all_lambda(1,1) = AWE_o_all_lambda(1,1) + AWE11
            AWE_o_all_lambda(1,2) = AWE_o_all_lambda(1,2) + AWE12
            AWE_o_all_lambda(1,3) = AWE_o_all_lambda(1,3) + AWE13
            AWE_o_all_lambda(1,4) = AWE_o_all_lambda(1,4) + AWE14
            AWE_o_all_lambda(2,1) = AWE_o_all_lambda(2,1) + AWE21
            AWE_o_all_lambda(2,2) = AWE_o_all_lambda(2,2) + AWE22
            AWE_o_all_lambda(2,3) = AWE_o_all_lambda(2,3) + AWE23
            AWE_o_all_lambda(2,4) = AWE_o_all_lambda(2,4) + AWE24
            AWE_o_all_lambda(3,1) = AWE_o_all_lambda(3,1) + AWE31
            AWE_o_all_lambda(3,2) = AWE_o_all_lambda(3,2) + AWE32
            AWE_o_all_lambda(3,3) = AWE_o_all_lambda(3,3) + AWE33
            AWE_o_all_lambda(3,4) = AWE_o_all_lambda(3,4) + AWE34
            AWE_o_all_lambda(4,1) = AWE_o_all_lambda(4,1) + AWE41
            AWE_o_all_lambda(4,2) = AWE_o_all_lambda(4,2) + AWE42
            AWE_o_all_lambda(4,3) = AWE_o_all_lambda(4,3) + AWE43
            AWE_o_all_lambda(4,4) = AWE_o_all_lambda(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o ass EN done!'
        close(101)
        do ii=1,NN_NEold

            
            hh_s = min(max(sim_hh(n_NEold(ii),t_NEold(ii))-hout_diff,1),HH)! relevant hh state
            hsp_s = min(max(sim_hsp(n_NEold(ii),t_NEold(ii))-hemp_diff,1),HH)! relevant hsp state
            t_s = t_NEold(ii)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_NEold(ii),t_NEold(ii)))/ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = min(max(sim_hh(n_NEold(ii),t_NEold(ii))-hout_diff,1),HH)! relevant hh state for arrival rates
            hsp_s_arrv = min(max(sim_hsp(n_NEold(ii),t_NEold(ii))-hemp_diff,1),HH)! relevant hsp state for arrival rates
            t_s_arrv = t_NEold(ii)! relevant age for arrival rates
            a_s_arrv = ap_ind(max(assgrid(sim_asset(n_NEold(ii),t_NEold(ii)))/ass_ratio,0d0))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_o_all_lambda(1,1) = AWE_o_all_lambda(1,1) + AWE11
            AWE_o_all_lambda(1,2) = AWE_o_all_lambda(1,2) + AWE12
            AWE_o_all_lambda(1,3) = AWE_o_all_lambda(1,3) + AWE13
            AWE_o_all_lambda(1,4) = AWE_o_all_lambda(1,4) + AWE14
            AWE_o_all_lambda(2,1) = AWE_o_all_lambda(2,1) + AWE21
            AWE_o_all_lambda(2,2) = AWE_o_all_lambda(2,2) + AWE22
            AWE_o_all_lambda(2,3) = AWE_o_all_lambda(2,3) + AWE23
            AWE_o_all_lambda(2,4) = AWE_o_all_lambda(2,4) + AWE24
            AWE_o_all_lambda(3,1) = AWE_o_all_lambda(3,1) + AWE31
            AWE_o_all_lambda(3,2) = AWE_o_all_lambda(3,2) + AWE32
            AWE_o_all_lambda(3,3) = AWE_o_all_lambda(3,3) + AWE33
            AWE_o_all_lambda(3,4) = AWE_o_all_lambda(3,4) + AWE34
            AWE_o_all_lambda(4,1) = AWE_o_all_lambda(4,1) + AWE41
            AWE_o_all_lambda(4,2) = AWE_o_all_lambda(4,2) + AWE42
            AWE_o_all_lambda(4,3) = AWE_o_all_lambda(4,3) + AWE43
            AWE_o_all_lambda(4,4) = AWE_o_all_lambda(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o ass NE done!'
        close(101)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE sanity
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_o_sanity=0

        do ii=1,NN_ENold

            
            hh_s = min(max(sim_hh(n_ENold(ii),t_ENold(ii))-hemp_diff,1),HH)! relevant hh state
            hsp_s = min(max(sim_hsp(n_ENold(ii),t_ENold(ii))-hout_diff,1),HH)! relevant hsp state
            t_s = t_ENold(ii)-(30*12)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_ENold(ii),t_ENold(ii)))/ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = min(max(sim_hh(n_ENold(ii),t_ENold(ii))-hemp_diff,1),HH)! relevant hh state for arrival rates
            hsp_s_arrv = min(max(sim_hsp(n_ENold(ii),t_ENold(ii))-hout_diff,1),HH)! relevant hsp state for arrival rates
            t_s_arrv = t_ENold(ii)-(30*12)! relevant age for arrival rates
            a_s_arrv = ap_ind(max(assgrid(sim_asset(n_ENold(ii),t_ENold(ii)))/ass_ratio,0d0))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_o_sanity(1,1) = AWE_o_sanity(1,1) + AWE11
            AWE_o_sanity(1,2) = AWE_o_sanity(1,2) + AWE12
            AWE_o_sanity(1,3) = AWE_o_sanity(1,3) + AWE13
            AWE_o_sanity(1,4) = AWE_o_sanity(1,4) + AWE14
            AWE_o_sanity(2,1) = AWE_o_sanity(2,1) + AWE21
            AWE_o_sanity(2,2) = AWE_o_sanity(2,2) + AWE22
            AWE_o_sanity(2,3) = AWE_o_sanity(2,3) + AWE23
            AWE_o_sanity(2,4) = AWE_o_sanity(2,4) + AWE24
            AWE_o_sanity(3,1) = AWE_o_sanity(3,1) + AWE31
            AWE_o_sanity(3,2) = AWE_o_sanity(3,2) + AWE32
            AWE_o_sanity(3,3) = AWE_o_sanity(3,3) + AWE33
            AWE_o_sanity(3,4) = AWE_o_sanity(3,4) + AWE34
            AWE_o_sanity(4,1) = AWE_o_sanity(4,1) + AWE41
            AWE_o_sanity(4,2) = AWE_o_sanity(4,2) + AWE42
            AWE_o_sanity(4,3) = AWE_o_sanity(4,3) + AWE43
            AWE_o_sanity(4,4) = AWE_o_sanity(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o ass EN done!'
        close(101)
        do ii=1,NN_NEold

            
            hh_s = min(max(sim_hh(n_NEold(ii),t_NEold(ii))-hout_diff,1),HH)! relevant hh state
            hsp_s = min(max(sim_hsp(n_NEold(ii),t_NEold(ii))-hemp_diff,1),HH)! relevant hsp state
            t_s = t_NEold(ii)-(30*12)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_NEold(ii),t_NEold(ii)))/ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = min(max(sim_hh(n_NEold(ii),t_NEold(ii))-hout_diff,1),HH)! relevant hh state for arrival rates
            hsp_s_arrv = min(max(sim_hsp(n_NEold(ii),t_NEold(ii))-hemp_diff,1),HH)! relevant hsp state for arrival rates
            t_s_arrv = t_NEold(ii)-(30*12)! relevant age for arrival rates
            a_s_arrv = ap_ind(max(assgrid(sim_asset(n_NEold(ii),t_NEold(ii)))/ass_ratio,0d0))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_o_sanity(1,1) = AWE_o_sanity(1,1) + AWE11
            AWE_o_sanity(1,2) = AWE_o_sanity(1,2) + AWE12
            AWE_o_sanity(1,3) = AWE_o_sanity(1,3) + AWE13
            AWE_o_sanity(1,4) = AWE_o_sanity(1,4) + AWE14
            AWE_o_sanity(2,1) = AWE_o_sanity(2,1) + AWE21
            AWE_o_sanity(2,2) = AWE_o_sanity(2,2) + AWE22
            AWE_o_sanity(2,3) = AWE_o_sanity(2,3) + AWE23
            AWE_o_sanity(2,4) = AWE_o_sanity(2,4) + AWE24
            AWE_o_sanity(3,1) = AWE_o_sanity(3,1) + AWE31
            AWE_o_sanity(3,2) = AWE_o_sanity(3,2) + AWE32
            AWE_o_sanity(3,3) = AWE_o_sanity(3,3) + AWE33
            AWE_o_sanity(3,4) = AWE_o_sanity(3,4) + AWE34
            AWE_o_sanity(4,1) = AWE_o_sanity(4,1) + AWE41
            AWE_o_sanity(4,2) = AWE_o_sanity(4,2) + AWE42
            AWE_o_sanity(4,3) = AWE_o_sanity(4,3) + AWE43
            AWE_o_sanity(4,4) = AWE_o_sanity(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'o ass NE done!'
        close(101)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE young
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
        AWE_y_base=0

        do ii=1,NN_ENyoung

            
            hh_s = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state
            hsp_s = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state
            t_s = t_ENyoung(ii)! relevant age
            a_s = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice
            indEN = 1
            sep = (delta(t_s,hh_s))! PB head has job next period
            lambda_E = lambda_NE_E(a_p,hsp_s,hh_s,t_s)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p,hsp_s,hh_s,t_s)! PB spouse has job next period
            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition

            
            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_y_base(1,1) = AWE_y_base(1,1) + AWE11
            AWE_y_base(1,2) = AWE_y_base(1,2) + AWE12
            AWE_y_base(1,3) = AWE_y_base(1,3) + AWE13
            AWE_y_base(1,4) = AWE_y_base(1,4) + AWE14
            AWE_y_base(2,1) = AWE_y_base(2,1) + AWE21
            AWE_y_base(2,2) = AWE_y_base(2,2) + AWE22
            AWE_y_base(2,3) = AWE_y_base(2,3) + AWE23
            AWE_y_base(2,4) = AWE_y_base(2,4) + AWE24
            AWE_y_base(3,1) = AWE_y_base(3,1) + AWE31
            AWE_y_base(3,2) = AWE_y_base(3,2) + AWE32
            AWE_y_base(3,3) = AWE_y_base(3,3) + AWE33
            AWE_y_base(3,4) = AWE_y_base(3,4) + AWE34
            AWE_y_base(4,1) = AWE_y_base(4,1) + AWE41
            AWE_y_base(4,2) = AWE_y_base(4,2) + AWE42
            AWE_y_base(4,3) = AWE_y_base(4,3) + AWE43
            AWE_y_base(4,4) = AWE_y_base(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y base EN done!'
        close(101)
        do ii=1,NN_NEyoung

            
            hh_s = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state
            hsp_s = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state
            t_s = t_NEyoung(ii)! relevant age
            a_s = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p,hh_s,hsp_s,t_s)! PB head has job next period
            lambda_X = lambda_NE_X(a_p,hh_s,hsp_s,t_s)! PB head has job next period
            sep = (delta(t_s,hsp_s))! PB spouse has job next period
            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 
 
            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_y_base(1,1) = AWE_y_base(1,1) + AWE11
            AWE_y_base(1,2) = AWE_y_base(1,2) + AWE12
            AWE_y_base(1,3) = AWE_y_base(1,3) + AWE13
            AWE_y_base(1,4) = AWE_y_base(1,4) + AWE14
            AWE_y_base(2,1) = AWE_y_base(2,1) + AWE21
            AWE_y_base(2,2) = AWE_y_base(2,2) + AWE22
            AWE_y_base(2,3) = AWE_y_base(2,3) + AWE23
            AWE_y_base(2,4) = AWE_y_base(2,4) + AWE24
            AWE_y_base(3,1) = AWE_y_base(3,1) + AWE31
            AWE_y_base(3,2) = AWE_y_base(3,2) + AWE32
            AWE_y_base(3,3) = AWE_y_base(3,3) + AWE33
            AWE_y_base(3,4) = AWE_y_base(3,4) + AWE34
            AWE_y_base(4,1) = AWE_y_base(4,1) + AWE41
            AWE_y_base(4,2) = AWE_y_base(4,2) + AWE42
            AWE_y_base(4,3) = AWE_y_base(4,3) + AWE43
            AWE_y_base(4,4) = AWE_y_base(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y base NE done!'
        close(101)
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE arrival rates (age) - young baseline
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_y_lambda_age=0

        do ii=1,NN_ENyoung

            
            hh_s = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state
            hsp_s = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state
            t_s = t_ENyoung(ii)! relevant age
            a_s = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENyoung(ii)+(30*12)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_ENyoung(ii))) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_y_lambda_age(1,1) = AWE_y_lambda_age(1,1) + AWE11
            AWE_y_lambda_age(1,2) = AWE_y_lambda_age(1,2) + AWE12
            AWE_y_lambda_age(1,3) = AWE_y_lambda_age(1,3) + AWE13
            AWE_y_lambda_age(1,4) = AWE_y_lambda_age(1,4) + AWE14
            AWE_y_lambda_age(2,1) = AWE_y_lambda_age(2,1) + AWE21
            AWE_y_lambda_age(2,2) = AWE_y_lambda_age(2,2) + AWE22
            AWE_y_lambda_age(2,3) = AWE_y_lambda_age(2,3) + AWE23
            AWE_y_lambda_age(2,4) = AWE_y_lambda_age(2,4) + AWE24
            AWE_y_lambda_age(3,1) = AWE_y_lambda_age(3,1) + AWE31
            AWE_y_lambda_age(3,2) = AWE_y_lambda_age(3,2) + AWE32
            AWE_y_lambda_age(3,3) = AWE_y_lambda_age(3,3) + AWE33
            AWE_y_lambda_age(3,4) = AWE_y_lambda_age(3,4) + AWE34
            AWE_y_lambda_age(4,1) = AWE_y_lambda_age(4,1) + AWE41
            AWE_y_lambda_age(4,2) = AWE_y_lambda_age(4,2) + AWE42
            AWE_y_lambda_age(4,3) = AWE_y_lambda_age(4,3) + AWE43
            AWE_y_lambda_age(4,4) = AWE_y_lambda_age(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y lambda age EN done!'
        close(101)
        do ii=1,NN_NEyoung

            
            hh_s = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state
            hsp_s = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state
            t_s = t_NEyoung(ii)! relevant age
            a_s = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEyoung(ii)+(30*12)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_NEyoung(ii))) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_y_lambda_age(1,1) = AWE_y_lambda_age(1,1) + AWE11
            AWE_y_lambda_age(1,2) = AWE_y_lambda_age(1,2) + AWE12
            AWE_y_lambda_age(1,3) = AWE_y_lambda_age(1,3) + AWE13
            AWE_y_lambda_age(1,4) = AWE_y_lambda_age(1,4) + AWE14
            AWE_y_lambda_age(2,1) = AWE_y_lambda_age(2,1) + AWE21
            AWE_y_lambda_age(2,2) = AWE_y_lambda_age(2,2) + AWE22
            AWE_y_lambda_age(2,3) = AWE_y_lambda_age(2,3) + AWE23
            AWE_y_lambda_age(2,4) = AWE_y_lambda_age(2,4) + AWE24
            AWE_y_lambda_age(3,1) = AWE_y_lambda_age(3,1) + AWE31
            AWE_y_lambda_age(3,2) = AWE_y_lambda_age(3,2) + AWE32
            AWE_y_lambda_age(3,3) = AWE_y_lambda_age(3,3) + AWE33
            AWE_y_lambda_age(3,4) = AWE_y_lambda_age(3,4) + AWE34
            AWE_y_lambda_age(4,1) = AWE_y_lambda_age(4,1) + AWE41
            AWE_y_lambda_age(4,2) = AWE_y_lambda_age(4,2) + AWE42
            AWE_y_lambda_age(4,3) = AWE_y_lambda_age(4,3) + AWE43
            AWE_y_lambda_age(4,4) = AWE_y_lambda_age(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y lambda age NE done!'
        close(101)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE arrival rates - young baseline
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_y_lambda=0

        do ii=1,NN_ENyoung

            
            hh_s = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state
            hsp_s = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state
            t_s = t_ENyoung(ii)! relevant age
            a_s = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENyoung(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_ENyoung(ii))) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = min(max(lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)*lambda_ratio_E,0d0),1d0)! PB spouse has job next period
            lambda_X = min(max(lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)*lambda_ratio_X,0d0),1d0)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_y_lambda(1,1) = AWE_y_lambda(1,1) + AWE11
            AWE_y_lambda(1,2) = AWE_y_lambda(1,2) + AWE12
            AWE_y_lambda(1,3) = AWE_y_lambda(1,3) + AWE13
            AWE_y_lambda(1,4) = AWE_y_lambda(1,4) + AWE14
            AWE_y_lambda(2,1) = AWE_y_lambda(2,1) + AWE21
            AWE_y_lambda(2,2) = AWE_y_lambda(2,2) + AWE22
            AWE_y_lambda(2,3) = AWE_y_lambda(2,3) + AWE23
            AWE_y_lambda(2,4) = AWE_y_lambda(2,4) + AWE24
            AWE_y_lambda(3,1) = AWE_y_lambda(3,1) + AWE31
            AWE_y_lambda(3,2) = AWE_y_lambda(3,2) + AWE32
            AWE_y_lambda(3,3) = AWE_y_lambda(3,3) + AWE33
            AWE_y_lambda(3,4) = AWE_y_lambda(3,4) + AWE34
            AWE_y_lambda(4,1) = AWE_y_lambda(4,1) + AWE41
            AWE_y_lambda(4,2) = AWE_y_lambda(4,2) + AWE42
            AWE_y_lambda(4,3) = AWE_y_lambda(4,3) + AWE43
            AWE_y_lambda(4,4) = AWE_y_lambda(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y lambda EN done!'
        close(101)
        do ii=1,NN_NEyoung

            
            hh_s = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state
            hsp_s = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state
            t_s = t_NEyoung(ii)! relevant age
            a_s = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEyoung(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_NEyoung(ii))) ! asset choice
            indEN = 2
            lambda_E = min(max(lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)*lambda_ratio_E,0d0),1d0) ! PB head has job next period
            lambda_X = min(max(lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)*lambda_ratio_X,0d0),1d0) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_y_lambda(1,1) = AWE_y_lambda(1,1) + AWE11
            AWE_y_lambda(1,2) = AWE_y_lambda(1,2) + AWE12
            AWE_y_lambda(1,3) = AWE_y_lambda(1,3) + AWE13
            AWE_y_lambda(1,4) = AWE_y_lambda(1,4) + AWE14
            AWE_y_lambda(2,1) = AWE_y_lambda(2,1) + AWE21
            AWE_y_lambda(2,2) = AWE_y_lambda(2,2) + AWE22
            AWE_y_lambda(2,3) = AWE_y_lambda(2,3) + AWE23
            AWE_y_lambda(2,4) = AWE_y_lambda(2,4) + AWE24
            AWE_y_lambda(3,1) = AWE_y_lambda(3,1) + AWE31
            AWE_y_lambda(3,2) = AWE_y_lambda(3,2) + AWE32
            AWE_y_lambda(3,3) = AWE_y_lambda(3,3) + AWE33
            AWE_y_lambda(3,4) = AWE_y_lambda(3,4) + AWE34
            AWE_y_lambda(4,1) = AWE_y_lambda(4,1) + AWE41
            AWE_y_lambda(4,2) = AWE_y_lambda(4,2) + AWE42
            AWE_y_lambda(4,3) = AWE_y_lambda(4,3) + AWE43
            AWE_y_lambda(4,4) = AWE_y_lambda(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y lambda NE done!'
        close(101)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE hout (without lambda) - young baseline
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_y_hout=0

        do ii=1,NN_ENyoung

            
            hh_s = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state
            hsp_s = min(max(sim_hsp(n_ENyoung(ii),t_ENyoung(ii))+hout_diff,1),HH)! relevant hsp state
            t_s = t_ENyoung(ii)! relevant age
            a_s = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENyoung(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_ENyoung(ii))) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period


            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_y_hout(1,1) = AWE_y_hout(1,1) + AWE11
            AWE_y_hout(1,2) = AWE_y_hout(1,2) + AWE12
            AWE_y_hout(1,3) = AWE_y_hout(1,3) + AWE13
            AWE_y_hout(1,4) = AWE_y_hout(1,4) + AWE14
            AWE_y_hout(2,1) = AWE_y_hout(2,1) + AWE21
            AWE_y_hout(2,2) = AWE_y_hout(2,2) + AWE22
            AWE_y_hout(2,3) = AWE_y_hout(2,3) + AWE23
            AWE_y_hout(2,4) = AWE_y_hout(2,4) + AWE24
            AWE_y_hout(3,1) = AWE_y_hout(3,1) + AWE31
            AWE_y_hout(3,2) = AWE_y_hout(3,2) + AWE32
            AWE_y_hout(3,3) = AWE_y_hout(3,3) + AWE33
            AWE_y_hout(3,4) = AWE_y_hout(3,4) + AWE34
            AWE_y_hout(4,1) = AWE_y_hout(4,1) + AWE41
            AWE_y_hout(4,2) = AWE_y_hout(4,2) + AWE42
            AWE_y_hout(4,3) = AWE_y_hout(4,3) + AWE43
            AWE_y_hout(4,4) = AWE_y_hout(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y hout EN done!'
        close(101)
        do ii=1,NN_NEyoung

            
            hh_s = min(max(sim_hh(n_NEyoung(ii),t_NEyoung(ii))+hout_diff,1),HH)! relevant hh state
            hsp_s = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state
            t_s = t_NEyoung(ii)! relevant age
            a_s = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEyoung(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_NEyoung(ii))) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_y_hout(1,1) = AWE_y_hout(1,1) + AWE11
            AWE_y_hout(1,2) = AWE_y_hout(1,2) + AWE12
            AWE_y_hout(1,3) = AWE_y_hout(1,3) + AWE13
            AWE_y_hout(1,4) = AWE_y_hout(1,4) + AWE14
            AWE_y_hout(2,1) = AWE_y_hout(2,1) + AWE21
            AWE_y_hout(2,2) = AWE_y_hout(2,2) + AWE22
            AWE_y_hout(2,3) = AWE_y_hout(2,3) + AWE23
            AWE_y_hout(2,4) = AWE_y_hout(2,4) + AWE24
            AWE_y_hout(3,1) = AWE_y_hout(3,1) + AWE31
            AWE_y_hout(3,2) = AWE_y_hout(3,2) + AWE32
            AWE_y_hout(3,3) = AWE_y_hout(3,3) + AWE33
            AWE_y_hout(3,4) = AWE_y_hout(3,4) + AWE34
            AWE_y_hout(4,1) = AWE_y_hout(4,1) + AWE41
            AWE_y_hout(4,2) = AWE_y_hout(4,2) + AWE42
            AWE_y_hout(4,3) = AWE_y_hout(4,3) + AWE43
            AWE_y_hout(4,4) = AWE_y_hout(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y hout NE done!'
        close(101)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE hout (with lambda) - baseline young
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_y_hout_lambda=0

        do ii=1,NN_ENyoung

            
            hh_s = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state
            hsp_s = min(max(sim_hsp(n_ENyoung(ii),t_ENyoung(ii))+hout_diff,1),HH)! relevant hsp state
            t_s = t_ENyoung(ii)! relevant age
            a_s = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = min(max(sim_hsp(n_ENyoung(ii),t_ENyoung(ii))+hout_diff,1),HH)! relevant hsp state for arrival rates
            t_s_arrv = t_ENyoung(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_ENyoung(ii))) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period


            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_y_hout_lambda(1,1) = AWE_y_hout_lambda(1,1) + AWE11
            AWE_y_hout_lambda(1,2) = AWE_y_hout_lambda(1,2) + AWE12
            AWE_y_hout_lambda(1,3) = AWE_y_hout_lambda(1,3) + AWE13
            AWE_y_hout_lambda(1,4) = AWE_y_hout_lambda(1,4) + AWE14
            AWE_y_hout_lambda(2,1) = AWE_y_hout_lambda(2,1) + AWE21
            AWE_y_hout_lambda(2,2) = AWE_y_hout_lambda(2,2) + AWE22
            AWE_y_hout_lambda(2,3) = AWE_y_hout_lambda(2,3) + AWE23
            AWE_y_hout_lambda(2,4) = AWE_y_hout_lambda(2,4) + AWE24
            AWE_y_hout_lambda(3,1) = AWE_y_hout_lambda(3,1) + AWE31
            AWE_y_hout_lambda(3,2) = AWE_y_hout_lambda(3,2) + AWE32
            AWE_y_hout_lambda(3,3) = AWE_y_hout_lambda(3,3) + AWE33
            AWE_y_hout_lambda(3,4) = AWE_y_hout_lambda(3,4) + AWE34
            AWE_y_hout_lambda(4,1) = AWE_y_hout_lambda(4,1) + AWE41
            AWE_y_hout_lambda(4,2) = AWE_y_hout_lambda(4,2) + AWE42
            AWE_y_hout_lambda(4,3) = AWE_y_hout_lambda(4,3) + AWE43
            AWE_y_hout_lambda(4,4) = AWE_y_hout_lambda(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y hout lambda EN done!'
        close(101)
        do ii=1,NN_NEyoung

            
            hh_s = min(max(sim_hh(n_NEyoung(ii),t_NEyoung(ii))+hout_diff,1),HH)! relevant hh state
            hsp_s = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state
            t_s = t_NEyoung(ii)! relevant age
            a_s = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = min(max(sim_hh(n_NEyoung(ii),t_NEyoung(ii))+hout_diff,1),HH)! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEyoung(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_NEyoung(ii))) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_y_hout_lambda(1,1) = AWE_y_hout_lambda(1,1) + AWE11
            AWE_y_hout_lambda(1,2) = AWE_y_hout_lambda(1,2) + AWE12
            AWE_y_hout_lambda(1,3) = AWE_y_hout_lambda(1,3) + AWE13
            AWE_y_hout_lambda(1,4) = AWE_y_hout_lambda(1,4) + AWE14
            AWE_y_hout_lambda(2,1) = AWE_y_hout_lambda(2,1) + AWE21
            AWE_y_hout_lambda(2,2) = AWE_y_hout_lambda(2,2) + AWE22
            AWE_y_hout_lambda(2,3) = AWE_y_hout_lambda(2,3) + AWE23
            AWE_y_hout_lambda(2,4) = AWE_y_hout_lambda(2,4) + AWE24
            AWE_y_hout_lambda(3,1) = AWE_y_hout_lambda(3,1) + AWE31
            AWE_y_hout_lambda(3,2) = AWE_y_hout_lambda(3,2) + AWE32
            AWE_y_hout_lambda(3,3) = AWE_y_hout_lambda(3,3) + AWE33
            AWE_y_hout_lambda(3,4) = AWE_y_hout_lambda(3,4) + AWE34
            AWE_y_hout_lambda(4,1) = AWE_y_hout_lambda(4,1) + AWE41
            AWE_y_hout_lambda(4,2) = AWE_y_hout_lambda(4,2) + AWE42
            AWE_y_hout_lambda(4,3) = AWE_y_hout_lambda(4,3) + AWE43
            AWE_y_hout_lambda(4,4) = AWE_y_hout_lambda(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y hout lambda NE done!'
        close(101)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE hemp - young baseline
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_y_hemp=0

        do ii=1,NN_ENyoung

            
            hh_s = min(max(sim_hh(n_ENyoung(ii),t_ENyoung(ii))+hemp_diff,1),HH)! relevant hh state
            hsp_s = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state
            t_s = t_ENyoung(ii)! relevant age
            a_s = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice
            
            hh_s_arrv = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENyoung(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_y_hemp(1,1) = AWE_y_hemp(1,1) + AWE11
            AWE_y_hemp(1,2) = AWE_y_hemp(1,2) + AWE12
            AWE_y_hemp(1,3) = AWE_y_hemp(1,3) + AWE13
            AWE_y_hemp(1,4) = AWE_y_hemp(1,4) + AWE14
            AWE_y_hemp(2,1) = AWE_y_hemp(2,1) + AWE21
            AWE_y_hemp(2,2) = AWE_y_hemp(2,2) + AWE22
            AWE_y_hemp(2,3) = AWE_y_hemp(2,3) + AWE23
            AWE_y_hemp(2,4) = AWE_y_hemp(2,4) + AWE24
            AWE_y_hemp(3,1) = AWE_y_hemp(3,1) + AWE31
            AWE_y_hemp(3,2) = AWE_y_hemp(3,2) + AWE32
            AWE_y_hemp(3,3) = AWE_y_hemp(3,3) + AWE33
            AWE_y_hemp(3,4) = AWE_y_hemp(3,4) + AWE34
            AWE_y_hemp(4,1) = AWE_y_hemp(4,1) + AWE41
            AWE_y_hemp(4,2) = AWE_y_hemp(4,2) + AWE42
            AWE_y_hemp(4,3) = AWE_y_hemp(4,3) + AWE43
            AWE_y_hemp(4,4) = AWE_y_hemp(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y hemp EN done!'
        close(101)
        do ii=1,NN_NEyoung

            
            hh_s = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state
            hsp_s = min(max(sim_hsp(n_NEyoung(ii),t_NEyoung(ii))+hemp_diff,1),HH)! relevant hsp state
            t_s = t_NEyoung(ii)! relevant age
            a_s = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice
            
            hh_s_arrv = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEyoung(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_y_hemp(1,1) = AWE_y_hemp(1,1) + AWE11
            AWE_y_hemp(1,2) = AWE_y_hemp(1,2) + AWE12
            AWE_y_hemp(1,3) = AWE_y_hemp(1,3) + AWE13
            AWE_y_hemp(1,4) = AWE_y_hemp(1,4) + AWE14
            AWE_y_hemp(2,1) = AWE_y_hemp(2,1) + AWE21
            AWE_y_hemp(2,2) = AWE_y_hemp(2,2) + AWE22
            AWE_y_hemp(2,3) = AWE_y_hemp(2,3) + AWE23
            AWE_y_hemp(2,4) = AWE_y_hemp(2,4) + AWE24
            AWE_y_hemp(3,1) = AWE_y_hemp(3,1) + AWE31
            AWE_y_hemp(3,2) = AWE_y_hemp(3,2) + AWE32
            AWE_y_hemp(3,3) = AWE_y_hemp(3,3) + AWE33
            AWE_y_hemp(3,4) = AWE_y_hemp(3,4) + AWE34
            AWE_y_hemp(4,1) = AWE_y_hemp(4,1) + AWE41
            AWE_y_hemp(4,2) = AWE_y_hemp(4,2) + AWE42
            AWE_y_hemp(4,3) = AWE_y_hemp(4,3) + AWE43
            AWE_y_hemp(4,4) = AWE_y_hemp(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y hemp NE done!'
        close(101)
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE hemp (with lambda) - young baseline
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_y_hemp_lambda=0

        do ii=1,NN_ENyoung

            
            hh_s = min(max(sim_hh(n_ENyoung(ii),t_ENyoung(ii))+hemp_diff,1),HH)! relevant hh state
            hsp_s = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state
            t_s = t_ENyoung(ii)! relevant age
            a_s = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice
            
            hh_s_arrv = min(max(sim_hh(n_ENyoung(ii),t_ENyoung(ii))+hemp_diff,1),HH)! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENyoung(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_y_hemp_lambda(1,1) = AWE_y_hemp_lambda(1,1) + AWE11
            AWE_y_hemp_lambda(1,2) = AWE_y_hemp_lambda(1,2) + AWE12
            AWE_y_hemp_lambda(1,3) = AWE_y_hemp_lambda(1,3) + AWE13
            AWE_y_hemp_lambda(1,4) = AWE_y_hemp_lambda(1,4) + AWE14
            AWE_y_hemp_lambda(2,1) = AWE_y_hemp_lambda(2,1) + AWE21
            AWE_y_hemp_lambda(2,2) = AWE_y_hemp_lambda(2,2) + AWE22
            AWE_y_hemp_lambda(2,3) = AWE_y_hemp_lambda(2,3) + AWE23
            AWE_y_hemp_lambda(2,4) = AWE_y_hemp_lambda(2,4) + AWE24
            AWE_y_hemp_lambda(3,1) = AWE_y_hemp_lambda(3,1) + AWE31
            AWE_y_hemp_lambda(3,2) = AWE_y_hemp_lambda(3,2) + AWE32
            AWE_y_hemp_lambda(3,3) = AWE_y_hemp_lambda(3,3) + AWE33
            AWE_y_hemp_lambda(3,4) = AWE_y_hemp_lambda(3,4) + AWE34
            AWE_y_hemp_lambda(4,1) = AWE_y_hemp_lambda(4,1) + AWE41
            AWE_y_hemp_lambda(4,2) = AWE_y_hemp_lambda(4,2) + AWE42
            AWE_y_hemp_lambda(4,3) = AWE_y_hemp_lambda(4,3) + AWE43
            AWE_y_hemp_lambda(4,4) = AWE_y_hemp_lambda(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y hemp EN done!'
        close(101)
        do ii=1,NN_NEyoung

            
            hh_s = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state
            hsp_s = min(max(sim_hsp(n_NEyoung(ii),t_NEyoung(ii))+hemp_diff,1),HH)! relevant hsp state
            t_s = t_NEyoung(ii)! relevant age
            a_s = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice
            
            hh_s_arrv = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = min(max(sim_hsp(n_NEyoung(ii),t_NEyoung(ii))+hemp_diff,1),HH)! relevant hsp state for arrival rates
            t_s_arrv = t_NEyoung(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_y_hemp_lambda(1,1) = AWE_y_hemp_lambda(1,1) + AWE11
            AWE_y_hemp_lambda(1,2) = AWE_y_hemp_lambda(1,2) + AWE12
            AWE_y_hemp_lambda(1,3) = AWE_y_hemp_lambda(1,3) + AWE13
            AWE_y_hemp_lambda(1,4) = AWE_y_hemp_lambda(1,4) + AWE14
            AWE_y_hemp_lambda(2,1) = AWE_y_hemp_lambda(2,1) + AWE21
            AWE_y_hemp_lambda(2,2) = AWE_y_hemp_lambda(2,2) + AWE22
            AWE_y_hemp_lambda(2,3) = AWE_y_hemp_lambda(2,3) + AWE23
            AWE_y_hemp_lambda(2,4) = AWE_y_hemp_lambda(2,4) + AWE24
            AWE_y_hemp_lambda(3,1) = AWE_y_hemp_lambda(3,1) + AWE31
            AWE_y_hemp_lambda(3,2) = AWE_y_hemp_lambda(3,2) + AWE32
            AWE_y_hemp_lambda(3,3) = AWE_y_hemp_lambda(3,3) + AWE33
            AWE_y_hemp_lambda(3,4) = AWE_y_hemp_lambda(3,4) + AWE34
            AWE_y_hemp_lambda(4,1) = AWE_y_hemp_lambda(4,1) + AWE41
            AWE_y_hemp_lambda(4,2) = AWE_y_hemp_lambda(4,2) + AWE42
            AWE_y_hemp_lambda(4,3) = AWE_y_hemp_lambda(4,3) + AWE43
            AWE_y_hemp_lambda(4,4) = AWE_y_hemp_lambda(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y hemp NE done!'
        close(101)
        
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE age
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_y_age=0

        do ii=1,NN_ENyoung

            
            hh_s = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state
            hsp_s = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state
            t_s = t_ENyoung(ii)+(30*12)! relevant age
            a_s = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice
            
            hh_s_arrv = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENyoung(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 
             
            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 
       

            AWE_y_age(1,1) = AWE_y_age(1,1) + AWE11
            AWE_y_age(1,2) = AWE_y_age(1,2) + AWE12
            AWE_y_age(1,3) = AWE_y_age(1,3) + AWE13
            AWE_y_age(1,4) = AWE_y_age(1,4) + AWE14
            AWE_y_age(2,1) = AWE_y_age(2,1) + AWE21
            AWE_y_age(2,2) = AWE_y_age(2,2) + AWE22
            AWE_y_age(2,3) = AWE_y_age(2,3) + AWE23
            AWE_y_age(2,4) = AWE_y_age(2,4) + AWE24
            AWE_y_age(3,1) = AWE_y_age(3,1) + AWE31
            AWE_y_age(3,2) = AWE_y_age(3,2) + AWE32
            AWE_y_age(3,3) = AWE_y_age(3,3) + AWE33
            AWE_y_age(3,4) = AWE_y_age(3,4) + AWE34
            AWE_y_age(4,1) = AWE_y_age(4,1) + AWE41
            AWE_y_age(4,2) = AWE_y_age(4,2) + AWE42
            AWE_y_age(4,3) = AWE_y_age(4,3) + AWE43
            AWE_y_age(4,4) = AWE_y_age(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y age EN done!'
        close(101)
        do ii=1,NN_NEyoung

            
            hh_s = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state
            hsp_s = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state
            t_s = t_NEyoung(ii)+(30*12)! relevant age
            a_s = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice
            
            hh_s_arrv = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEyoung(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period
            
            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_y_age(1,1) = AWE_y_age(1,1) + AWE11
            AWE_y_age(1,2) = AWE_y_age(1,2) + AWE12
            AWE_y_age(1,3) = AWE_y_age(1,3) + AWE13
            AWE_y_age(1,4) = AWE_y_age(1,4) + AWE14
            AWE_y_age(2,1) = AWE_y_age(2,1) + AWE21
            AWE_y_age(2,2) = AWE_y_age(2,2) + AWE22
            AWE_y_age(2,3) = AWE_y_age(2,3) + AWE23
            AWE_y_age(2,4) = AWE_y_age(2,4) + AWE24
            AWE_y_age(3,1) = AWE_y_age(3,1) + AWE31
            AWE_y_age(3,2) = AWE_y_age(3,2) + AWE32
            AWE_y_age(3,3) = AWE_y_age(3,3) + AWE33
            AWE_y_age(3,4) = AWE_y_age(3,4) + AWE34
            AWE_y_age(4,1) = AWE_y_age(4,1) + AWE41
            AWE_y_age(4,2) = AWE_y_age(4,2) + AWE42
            AWE_y_age(4,3) = AWE_y_age(4,3) + AWE43
            AWE_y_age(4,4) = AWE_y_age(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y age NE done!'
        close(101)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE age (with lambda)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_y_age_lambda=0

        do ii=1,NN_ENyoung

            
            hh_s = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state
            hsp_s = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state
            t_s = t_ENyoung(ii)+(30*12)! relevant age
            a_s = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice
            
            hh_s_arrv = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENyoung(ii)+(30*12)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 
             
            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 
       

            AWE_y_age_lambda(1,1) = AWE_y_age_lambda(1,1) + AWE11
            AWE_y_age_lambda(1,2) = AWE_y_age_lambda(1,2) + AWE12
            AWE_y_age_lambda(1,3) = AWE_y_age_lambda(1,3) + AWE13
            AWE_y_age_lambda(1,4) = AWE_y_age_lambda(1,4) + AWE14
            AWE_y_age_lambda(2,1) = AWE_y_age_lambda(2,1) + AWE21
            AWE_y_age_lambda(2,2) = AWE_y_age_lambda(2,2) + AWE22
            AWE_y_age_lambda(2,3) = AWE_y_age_lambda(2,3) + AWE23
            AWE_y_age_lambda(2,4) = AWE_y_age_lambda(2,4) + AWE24
            AWE_y_age_lambda(3,1) = AWE_y_age_lambda(3,1) + AWE31
            AWE_y_age_lambda(3,2) = AWE_y_age_lambda(3,2) + AWE32
            AWE_y_age_lambda(3,3) = AWE_y_age_lambda(3,3) + AWE33
            AWE_y_age_lambda(3,4) = AWE_y_age_lambda(3,4) + AWE34
            AWE_y_age_lambda(4,1) = AWE_y_age_lambda(4,1) + AWE41
            AWE_y_age_lambda(4,2) = AWE_y_age_lambda(4,2) + AWE42
            AWE_y_age_lambda(4,3) = AWE_y_age_lambda(4,3) + AWE43
            AWE_y_age_lambda(4,4) = AWE_y_age_lambda(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y age EN done!'
        close(101)
        do ii=1,NN_NEyoung

            
            hh_s = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state
            hsp_s = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state
            t_s = t_NEyoung(ii)+(30*12)! relevant age
            a_s = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice
            
            hh_s_arrv = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEyoung(ii)+(30*12)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period
            
            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_y_age_lambda(1,1) = AWE_y_age_lambda(1,1) + AWE11
            AWE_y_age_lambda(1,2) = AWE_y_age_lambda(1,2) + AWE12
            AWE_y_age_lambda(1,3) = AWE_y_age_lambda(1,3) + AWE13
            AWE_y_age_lambda(1,4) = AWE_y_age_lambda(1,4) + AWE14
            AWE_y_age_lambda(2,1) = AWE_y_age_lambda(2,1) + AWE21
            AWE_y_age_lambda(2,2) = AWE_y_age_lambda(2,2) + AWE22
            AWE_y_age_lambda(2,3) = AWE_y_age_lambda(2,3) + AWE23
            AWE_y_age_lambda(2,4) = AWE_y_age_lambda(2,4) + AWE24
            AWE_y_age_lambda(3,1) = AWE_y_age_lambda(3,1) + AWE31
            AWE_y_age_lambda(3,2) = AWE_y_age_lambda(3,2) + AWE32
            AWE_y_age_lambda(3,3) = AWE_y_age_lambda(3,3) + AWE33
            AWE_y_age_lambda(3,4) = AWE_y_age_lambda(3,4) + AWE34
            AWE_y_age_lambda(4,1) = AWE_y_age_lambda(4,1) + AWE41
            AWE_y_age_lambda(4,2) = AWE_y_age_lambda(4,2) + AWE42
            AWE_y_age_lambda(4,3) = AWE_y_age_lambda(4,3) + AWE43
            AWE_y_age_lambda(4,4) = AWE_y_age_lambda(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y age NE done!'
        close(101)


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE assets
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_y_ass=0

        do ii=1,NN_ENyoung

            
            hh_s = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state
            hsp_s = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state
            t_s = t_ENyoung(ii)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_ENyoung(ii),t_ENyoung(ii)))*ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENyoung(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_y_ass(1,1) = AWE_y_ass(1,1) + AWE11
            AWE_y_ass(1,2) = AWE_y_ass(1,2) + AWE12
            AWE_y_ass(1,3) = AWE_y_ass(1,3) + AWE13
            AWE_y_ass(1,4) = AWE_y_ass(1,4) + AWE14
            AWE_y_ass(2,1) = AWE_y_ass(2,1) + AWE21
            AWE_y_ass(2,2) = AWE_y_ass(2,2) + AWE22
            AWE_y_ass(2,3) = AWE_y_ass(2,3) + AWE23
            AWE_y_ass(2,4) = AWE_y_ass(2,4) + AWE24
            AWE_y_ass(3,1) = AWE_y_ass(3,1) + AWE31
            AWE_y_ass(3,2) = AWE_y_ass(3,2) + AWE32
            AWE_y_ass(3,3) = AWE_y_ass(3,3) + AWE33
            AWE_y_ass(3,4) = AWE_y_ass(3,4) + AWE34
            AWE_y_ass(4,1) = AWE_y_ass(4,1) + AWE41
            AWE_y_ass(4,2) = AWE_y_ass(4,2) + AWE42
            AWE_y_ass(4,3) = AWE_y_ass(4,3) + AWE43
            AWE_y_ass(4,4) = AWE_y_ass(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y ass EN done!'
        close(101)
        do ii=1,NN_NEyoung

            
            hh_s = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state
            hsp_s = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state
            t_s = t_NEyoung(ii)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_NEyoung(ii),t_NEyoung(ii)))*ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEyoung(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_y_ass(1,1) = AWE_y_ass(1,1) + AWE11
            AWE_y_ass(1,2) = AWE_y_ass(1,2) + AWE12
            AWE_y_ass(1,3) = AWE_y_ass(1,3) + AWE13
            AWE_y_ass(1,4) = AWE_y_ass(1,4) + AWE14
            AWE_y_ass(2,1) = AWE_y_ass(2,1) + AWE21
            AWE_y_ass(2,2) = AWE_y_ass(2,2) + AWE22
            AWE_y_ass(2,3) = AWE_y_ass(2,3) + AWE23
            AWE_y_ass(2,4) = AWE_y_ass(2,4) + AWE24
            AWE_y_ass(3,1) = AWE_y_ass(3,1) + AWE31
            AWE_y_ass(3,2) = AWE_y_ass(3,2) + AWE32
            AWE_y_ass(3,3) = AWE_y_ass(3,3) + AWE33
            AWE_y_ass(3,4) = AWE_y_ass(3,4) + AWE34
            AWE_y_ass(4,1) = AWE_y_ass(4,1) + AWE41
            AWE_y_ass(4,2) = AWE_y_ass(4,2) + AWE42
            AWE_y_ass(4,3) = AWE_y_ass(4,3) + AWE43
            AWE_y_ass(4,4) = AWE_y_ass(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y ass NE done!'
        close(101)

        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE assets (with lambda)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_y_ass_lambda=0

        do ii=1,NN_ENyoung

            
            hh_s = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state
            hsp_s = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state
            t_s = t_ENyoung(ii)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_ENyoung(ii),t_ENyoung(ii)))*ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENyoung(ii)! relevant age for arrival rates
            a_s_arrv = ap_ind(max(assgrid(sim_asset(n_ENyoung(ii),t_ENyoung(ii)))*ass_ratio,0d0))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_y_ass_lambda(1,1) = AWE_y_ass_lambda(1,1) + AWE11
            AWE_y_ass_lambda(1,2) = AWE_y_ass_lambda(1,2) + AWE12
            AWE_y_ass_lambda(1,3) = AWE_y_ass_lambda(1,3) + AWE13
            AWE_y_ass_lambda(1,4) = AWE_y_ass_lambda(1,4) + AWE14
            AWE_y_ass_lambda(2,1) = AWE_y_ass_lambda(2,1) + AWE21
            AWE_y_ass_lambda(2,2) = AWE_y_ass_lambda(2,2) + AWE22
            AWE_y_ass_lambda(2,3) = AWE_y_ass_lambda(2,3) + AWE23
            AWE_y_ass_lambda(2,4) = AWE_y_ass_lambda(2,4) + AWE24
            AWE_y_ass_lambda(3,1) = AWE_y_ass_lambda(3,1) + AWE31
            AWE_y_ass_lambda(3,2) = AWE_y_ass_lambda(3,2) + AWE32
            AWE_y_ass_lambda(3,3) = AWE_y_ass_lambda(3,3) + AWE33
            AWE_y_ass_lambda(3,4) = AWE_y_ass_lambda(3,4) + AWE34
            AWE_y_ass_lambda(4,1) = AWE_y_ass_lambda(4,1) + AWE41
            AWE_y_ass_lambda(4,2) = AWE_y_ass_lambda(4,2) + AWE42
            AWE_y_ass_lambda(4,3) = AWE_y_ass_lambda(4,3) + AWE43
            AWE_y_ass_lambda(4,4) = AWE_y_ass_lambda(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y ass EN done!'
        close(101)
        do ii=1,NN_NEyoung

            
            hh_s = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state
            hsp_s = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state
            t_s = t_NEyoung(ii)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_NEyoung(ii),t_NEyoung(ii)))*ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEyoung(ii)! relevant age for arrival rates
            a_s_arrv = ap_ind(max(assgrid(sim_asset(n_NEyoung(ii),t_NEyoung(ii)))*ass_ratio,0d0))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_y_ass_lambda(1,1) = AWE_y_ass_lambda(1,1) + AWE11
            AWE_y_ass_lambda(1,2) = AWE_y_ass_lambda(1,2) + AWE12
            AWE_y_ass_lambda(1,3) = AWE_y_ass_lambda(1,3) + AWE13
            AWE_y_ass_lambda(1,4) = AWE_y_ass_lambda(1,4) + AWE14
            AWE_y_ass_lambda(2,1) = AWE_y_ass_lambda(2,1) + AWE21
            AWE_y_ass_lambda(2,2) = AWE_y_ass_lambda(2,2) + AWE22
            AWE_y_ass_lambda(2,3) = AWE_y_ass_lambda(2,3) + AWE23
            AWE_y_ass_lambda(2,4) = AWE_y_ass_lambda(2,4) + AWE24
            AWE_y_ass_lambda(3,1) = AWE_y_ass_lambda(3,1) + AWE31
            AWE_y_ass_lambda(3,2) = AWE_y_ass_lambda(3,2) + AWE32
            AWE_y_ass_lambda(3,3) = AWE_y_ass_lambda(3,3) + AWE33
            AWE_y_ass_lambda(3,4) = AWE_y_ass_lambda(3,4) + AWE34
            AWE_y_ass_lambda(4,1) = AWE_y_ass_lambda(4,1) + AWE41
            AWE_y_ass_lambda(4,2) = AWE_y_ass_lambda(4,2) + AWE42
            AWE_y_ass_lambda(4,3) = AWE_y_ass_lambda(4,3) + AWE43
            AWE_y_ass_lambda(4,4) = AWE_y_ass_lambda(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y ass NE done!'
        close(101)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE all
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_y_all=0

        do ii=1,NN_ENyoung

            
            hh_s = min(max(sim_hh(n_ENyoung(ii),t_ENyoung(ii))+hemp_diff,1),HH)! relevant hh state
            hsp_s = min(max(sim_hsp(n_ENyoung(ii),t_ENyoung(ii))+hout_diff,1),HH)! relevant hsp state
            t_s = t_ENyoung(ii)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_ENyoung(ii),t_ENyoung(ii)))*ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_ENyoung(ii),t_ENyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_ENyoung(ii),t_ENyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_ENyoung(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_ENyoung(ii),t_ENyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_y_all(1,1) = AWE_y_all(1,1) + AWE11
            AWE_y_all(1,2) = AWE_y_all(1,2) + AWE12
            AWE_y_all(1,3) = AWE_y_all(1,3) + AWE13
            AWE_y_all(1,4) = AWE_y_all(1,4) + AWE14
            AWE_y_all(2,1) = AWE_y_all(2,1) + AWE21
            AWE_y_all(2,2) = AWE_y_all(2,2) + AWE22
            AWE_y_all(2,3) = AWE_y_all(2,3) + AWE23
            AWE_y_all(2,4) = AWE_y_all(2,4) + AWE24
            AWE_y_all(3,1) = AWE_y_all(3,1) + AWE31
            AWE_y_all(3,2) = AWE_y_all(3,2) + AWE32
            AWE_y_all(3,3) = AWE_y_all(3,3) + AWE33
            AWE_y_all(3,4) = AWE_y_all(3,4) + AWE34
            AWE_y_all(4,1) = AWE_y_all(4,1) + AWE41
            AWE_y_all(4,2) = AWE_y_all(4,2) + AWE42
            AWE_y_all(4,3) = AWE_y_all(4,3) + AWE43
            AWE_y_all(4,4) = AWE_y_all(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y ass EN done!'
        close(101)
        do ii=1,NN_NEyoung

            
            hh_s = min(max(sim_hh(n_NEyoung(ii),t_NEyoung(ii))+hout_diff,1),HH)! relevant hh state
            hsp_s = min(max(sim_hsp(n_NEyoung(ii),t_NEyoung(ii))+hemp_diff,1),HH)! relevant hsp state
            t_s = t_NEyoung(ii)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_NEyoung(ii),t_NEyoung(ii)))*ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = sim_hh(n_NEyoung(ii),t_NEyoung(ii))! relevant hh state for arrival rates
            hsp_s_arrv = sim_hsp(n_NEyoung(ii),t_NEyoung(ii))! relevant hsp state for arrival rates
            t_s_arrv = t_NEyoung(ii)! relevant age for arrival rates
            a_s_arrv = sim_asset(n_NEyoung(ii),t_NEyoung(ii))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_y_all(1,1) = AWE_y_all(1,1) + AWE11
            AWE_y_all(1,2) = AWE_y_all(1,2) + AWE12
            AWE_y_all(1,3) = AWE_y_all(1,3) + AWE13
            AWE_y_all(1,4) = AWE_y_all(1,4) + AWE14
            AWE_y_all(2,1) = AWE_y_all(2,1) + AWE21
            AWE_y_all(2,2) = AWE_y_all(2,2) + AWE22
            AWE_y_all(2,3) = AWE_y_all(2,3) + AWE23
            AWE_y_all(2,4) = AWE_y_all(2,4) + AWE24
            AWE_y_all(3,1) = AWE_y_all(3,1) + AWE31
            AWE_y_all(3,2) = AWE_y_all(3,2) + AWE32
            AWE_y_all(3,3) = AWE_y_all(3,3) + AWE33
            AWE_y_all(3,4) = AWE_y_all(3,4) + AWE34
            AWE_y_all(4,1) = AWE_y_all(4,1) + AWE41
            AWE_y_all(4,2) = AWE_y_all(4,2) + AWE42
            AWE_y_all(4,3) = AWE_y_all(4,3) + AWE43
            AWE_y_all(4,4) = AWE_y_all(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y ass NE done!'
        close(101)

        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE all (with lambda)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_y_all_lambda=0

        do ii=1,NN_ENyoung

            
            hh_s = min(max(sim_hh(n_ENyoung(ii),t_ENyoung(ii))+hemp_diff,1),HH)! relevant hh state
            hsp_s = min(max(sim_hsp(n_ENyoung(ii),t_ENyoung(ii))+hout_diff,1),HH)! relevant hsp state
            t_s = t_ENyoung(ii)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_ENyoung(ii),t_ENyoung(ii)))*ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = min(max(sim_hh(n_ENyoung(ii),t_ENyoung(ii))+hemp_diff,1),HH)! relevant hh state for arrival rates
            hsp_s_arrv = min(max(sim_hsp(n_ENyoung(ii),t_ENyoung(ii))+hout_diff,1),HH)! relevant hsp state for arrival rates
            t_s_arrv = t_ENyoung(ii)! relevant age for arrival rates
            a_s_arrv = ap_ind(max(assgrid(sim_asset(n_ENyoung(ii),t_ENyoung(ii)))*ass_ratio,0d0))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_y_all_lambda(1,1) = AWE_y_all_lambda(1,1) + AWE11
            AWE_y_all_lambda(1,2) = AWE_y_all_lambda(1,2) + AWE12
            AWE_y_all_lambda(1,3) = AWE_y_all_lambda(1,3) + AWE13
            AWE_y_all_lambda(1,4) = AWE_y_all_lambda(1,4) + AWE14
            AWE_y_all_lambda(2,1) = AWE_y_all_lambda(2,1) + AWE21
            AWE_y_all_lambda(2,2) = AWE_y_all_lambda(2,2) + AWE22
            AWE_y_all_lambda(2,3) = AWE_y_all_lambda(2,3) + AWE23
            AWE_y_all_lambda(2,4) = AWE_y_all_lambda(2,4) + AWE24
            AWE_y_all_lambda(3,1) = AWE_y_all_lambda(3,1) + AWE31
            AWE_y_all_lambda(3,2) = AWE_y_all_lambda(3,2) + AWE32
            AWE_y_all_lambda(3,3) = AWE_y_all_lambda(3,3) + AWE33
            AWE_y_all_lambda(3,4) = AWE_y_all_lambda(3,4) + AWE34
            AWE_y_all_lambda(4,1) = AWE_y_all_lambda(4,1) + AWE41
            AWE_y_all_lambda(4,2) = AWE_y_all_lambda(4,2) + AWE42
            AWE_y_all_lambda(4,3) = AWE_y_all_lambda(4,3) + AWE43
            AWE_y_all_lambda(4,4) = AWE_y_all_lambda(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y ass EN done!'
        close(101)
        do ii=1,NN_NEyoung

            
            hh_s = min(max(sim_hh(n_NEyoung(ii),t_NEyoung(ii))+hout_diff,1),HH)! relevant hh state
            hsp_s = min(max(sim_hsp(n_NEyoung(ii),t_NEyoung(ii))+hemp_diff,1),HH)! relevant hsp state
            t_s = t_NEyoung(ii)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_NEyoung(ii),t_NEyoung(ii)))*ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = min(max(sim_hh(n_NEyoung(ii),t_NEyoung(ii))+hout_diff,1),HH)! relevant hh state for arrival rates
            hsp_s_arrv = min(max(sim_hsp(n_NEyoung(ii),t_NEyoung(ii))+hemp_diff,1),HH)! relevant hsp state for arrival rates
            t_s_arrv = t_NEyoung(ii)! relevant age for arrival rates
            a_s_arrv = ap_ind(max(assgrid(sim_asset(n_NEyoung(ii),t_NEyoung(ii)))*ass_ratio,0d0))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_y_all_lambda(1,1) = AWE_y_all_lambda(1,1) + AWE11
            AWE_y_all_lambda(1,2) = AWE_y_all_lambda(1,2) + AWE12
            AWE_y_all_lambda(1,3) = AWE_y_all_lambda(1,3) + AWE13
            AWE_y_all_lambda(1,4) = AWE_y_all_lambda(1,4) + AWE14
            AWE_y_all_lambda(2,1) = AWE_y_all_lambda(2,1) + AWE21
            AWE_y_all_lambda(2,2) = AWE_y_all_lambda(2,2) + AWE22
            AWE_y_all_lambda(2,3) = AWE_y_all_lambda(2,3) + AWE23
            AWE_y_all_lambda(2,4) = AWE_y_all_lambda(2,4) + AWE24
            AWE_y_all_lambda(3,1) = AWE_y_all_lambda(3,1) + AWE31
            AWE_y_all_lambda(3,2) = AWE_y_all_lambda(3,2) + AWE32
            AWE_y_all_lambda(3,3) = AWE_y_all_lambda(3,3) + AWE33
            AWE_y_all_lambda(3,4) = AWE_y_all_lambda(3,4) + AWE34
            AWE_y_all_lambda(4,1) = AWE_y_all_lambda(4,1) + AWE41
            AWE_y_all_lambda(4,2) = AWE_y_all_lambda(4,2) + AWE42
            AWE_y_all_lambda(4,3) = AWE_y_all_lambda(4,3) + AWE43
            AWE_y_all_lambda(4,4) = AWE_y_all_lambda(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y ass NE done!'
        close(101)

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! AWE all (with lambda)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        AWE_y_sanity=0

        do ii=1,NN_ENyoung

            
            hh_s = min(max(sim_hh(n_ENyoung(ii),t_ENyoung(ii))+hemp_diff,1),HH)! relevant hh state
            hsp_s = min(max(sim_hsp(n_ENyoung(ii),t_ENyoung(ii))+hout_diff,1),HH)! relevant hsp state
            t_s = t_ENyoung(ii)+(30*12)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_ENyoung(ii),t_ENyoung(ii)))*ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_EN(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = min(max(sim_hh(n_ENyoung(ii),t_ENyoung(ii))+hemp_diff,1),HH)! relevant hh state for arrival rates
            hsp_s_arrv = min(max(sim_hsp(n_ENyoung(ii),t_ENyoung(ii))+hout_diff,1),HH)! relevant hsp state for arrival rates
            t_s_arrv = t_ENyoung(ii)+(30*12)! relevant age for arrival rates
            a_s_arrv = ap_ind(max(assgrid(sim_asset(n_ENyoung(ii),t_ENyoung(ii)))*ass_ratio,0d0))! relevant assets
            a_p_arrv = ap_ind(apol_EN(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 1
            sep = (delta(t_s_arrv,hh_s_arrv))! PB head has job next period
            lambda_E = lambda_NE_E(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hsp_s_arrv,hh_s_arrv,t_s_arrv)! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 1d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 0d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_EN(ii)! draw for h transition head
            step_hsp = step_hsp_EN(ii)! draw for h transition spouse
            step_jobh = step_jobh_EN(ii)! draw for job offer head
            step_jobsp = step_jobsp_EN(ii)! draw for job offer spouse
            step_LS = step_LS_EN(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==3 )
            AWE13 = merge( 1, 0, LS_new==5 )
            AWE14 = merge( 1, 0, LS_new==15 )
            AWE21 = merge( 1, 0, LS_new==2 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==8 ) 
            AWE24 = merge( 1, 0, LS_new==13 ) 
            AWE31 = merge( 1, 0, LS_new==4 ) 
            AWE32 = merge( 1, 0, LS_new==7 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==11 ) 
            AWE41 = merge( 1, 0, LS_new==16 ) 
            AWE42 = merge( 1, 0, LS_new==14 ) 
            AWE43 = merge( 1, 0, LS_new==12 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 


            AWE_y_sanity(1,1) = AWE_y_sanity(1,1) + AWE11
            AWE_y_sanity(1,2) = AWE_y_sanity(1,2) + AWE12
            AWE_y_sanity(1,3) = AWE_y_sanity(1,3) + AWE13
            AWE_y_sanity(1,4) = AWE_y_sanity(1,4) + AWE14
            AWE_y_sanity(2,1) = AWE_y_sanity(2,1) + AWE21
            AWE_y_sanity(2,2) = AWE_y_sanity(2,2) + AWE22
            AWE_y_sanity(2,3) = AWE_y_sanity(2,3) + AWE23
            AWE_y_sanity(2,4) = AWE_y_sanity(2,4) + AWE24
            AWE_y_sanity(3,1) = AWE_y_sanity(3,1) + AWE31
            AWE_y_sanity(3,2) = AWE_y_sanity(3,2) + AWE32
            AWE_y_sanity(3,3) = AWE_y_sanity(3,3) + AWE33
            AWE_y_sanity(3,4) = AWE_y_sanity(3,4) + AWE34
            AWE_y_sanity(4,1) = AWE_y_sanity(4,1) + AWE41
            AWE_y_sanity(4,2) = AWE_y_sanity(4,2) + AWE42
            AWE_y_sanity(4,3) = AWE_y_sanity(4,3) + AWE43
            AWE_y_sanity(4,4) = AWE_y_sanity(4,4) + AWE44
        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y ass EN done!'
        close(101)
        do ii=1,NN_NEyoung

            
            hh_s = min(max(sim_hh(n_NEyoung(ii),t_NEyoung(ii))+hout_diff,1),HH)! relevant hh state
            hsp_s = min(max(sim_hsp(n_NEyoung(ii),t_NEyoung(ii))+hemp_diff,1),HH)! relevant hsp state
            t_s = t_NEyoung(ii)+(30*12)! relevant age
            a_s = ap_ind(max(assgrid(sim_asset(n_NEyoung(ii),t_NEyoung(ii)))*ass_ratio,0d0))! relevant assets
            a_p = ap_ind(apol_NE(a_s,hh_s,hsp_s,t_s)) ! asset choice

            hh_s_arrv = min(max(sim_hh(n_NEyoung(ii),t_NEyoung(ii))+hout_diff,1),HH)! relevant hh state for arrival rates
            hsp_s_arrv = min(max(sim_hsp(n_NEyoung(ii),t_NEyoung(ii))+hemp_diff,1),HH)! relevant hsp state for arrival rates
            t_s_arrv = t_NEyoung(ii)+(30*12)! relevant age for arrival rates
            a_s_arrv = ap_ind(max(assgrid(sim_asset(n_NEyoung(ii),t_NEyoung(ii)))*ass_ratio,0d0))! relevant assets
            a_p_arrv = ap_ind(apol_NE(a_s_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv)) ! asset choice
            indEN = 2
            lambda_E = lambda_NE_E(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            lambda_X = lambda_NE_X(a_p_arrv,hh_s_arrv,hsp_s_arrv,t_s_arrv) ! PB head has job next period
            sep = (delta(t_s_arrv,hsp_s_arrv))! PB spouse has job next period

            pb_Eh_Bh = 0d0! indicator eligible head if head has job
            pb_Xh_Bh = 0d0! indicator eligible head if head not has job
            pb_Esp_Bsp = 0d0! indicator eligible spouse if spouse has job
            pb_Xsp_Bsp = 1d0! indicator eligible spouse if spouse not has job
            step_hh = step_hh_NE(ii)! draw for h transition head
            step_hsp = step_hsp_NE(ii)! draw for h transition spouse
            step_jobh = step_jobh_NE(ii)! draw for job offer head
            step_jobsp = step_jobsp_NE(ii)! draw for job offer spouse
            step_LS = step_LS_NE(ii)! draw for LS transition


            call sim_step(a_p, 1, 0, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new) 

            AWE11 = merge( 1, 0, LS_new==1 )
            AWE12 = merge( 1, 0, LS_new==2 )
            AWE13 = merge( 1, 0, LS_new==4 )
            AWE14 = merge( 1, 0, LS_new==16 )
            AWE21 = merge( 1, 0, LS_new==3 )
            AWE22 = merge( 1, 0, LS_new==6 ) 
            AWE23 = merge( 1, 0, LS_new==7 ) 
            AWE24 = merge( 1, 0, LS_new==14 ) 
            AWE31 = merge( 1, 0, LS_new==5 ) 
            AWE32 = merge( 1, 0, LS_new==8 ) 
            AWE33 = merge( 1, 0, LS_new==9 ) 
            AWE34 = merge( 1, 0, LS_new==12 ) 
            AWE41 = merge( 1, 0, LS_new==15 ) 
            AWE42 = merge( 1, 0, LS_new==13 ) 
            AWE43 = merge( 1, 0, LS_new==11 ) 
            AWE44 = merge( 1, 0, LS_new==10 ) 

            AWE_y_sanity(1,1) = AWE_y_sanity(1,1) + AWE11
            AWE_y_sanity(1,2) = AWE_y_sanity(1,2) + AWE12
            AWE_y_sanity(1,3) = AWE_y_sanity(1,3) + AWE13
            AWE_y_sanity(1,4) = AWE_y_sanity(1,4) + AWE14
            AWE_y_sanity(2,1) = AWE_y_sanity(2,1) + AWE21
            AWE_y_sanity(2,2) = AWE_y_sanity(2,2) + AWE22
            AWE_y_sanity(2,3) = AWE_y_sanity(2,3) + AWE23
            AWE_y_sanity(2,4) = AWE_y_sanity(2,4) + AWE24
            AWE_y_sanity(3,1) = AWE_y_sanity(3,1) + AWE31
            AWE_y_sanity(3,2) = AWE_y_sanity(3,2) + AWE32
            AWE_y_sanity(3,3) = AWE_y_sanity(3,3) + AWE33
            AWE_y_sanity(3,4) = AWE_y_sanity(3,4) + AWE34
            AWE_y_sanity(4,1) = AWE_y_sanity(4,1) + AWE41
            AWE_y_sanity(4,2) = AWE_y_sanity(4,2) + AWE42
            AWE_y_sanity(4,3) = AWE_y_sanity(4,3) + AWE43
            AWE_y_sanity(4,4) = AWE_y_sanity(4,4) + AWE44

        end do
        open (unit=101,file="Output/progress_ex_new.txt",action="write",status="replace")
        write (101,*) 'y ass NE done!'
        close(101)
        
        


        call store_exercise_new()
        
        
        deallocate (t_ENyoung)
        deallocate (n_ENyoung)
        deallocate (t_ENold)
        deallocate (n_ENold)
        deallocate (t_NEyoung)
        deallocate (n_NEyoung)
        deallocate (t_NEold)
        deallocate (n_NEold)

        deallocate (hemp_ENyoung)
        deallocate (hout_ENyoung)
        deallocate (hemp_ENold)
        deallocate (hout_ENold)
        deallocate (hemp_NEyoung)
        deallocate (hout_NEyoung)
        deallocate (hemp_NEold)
        deallocate (hout_NEold)
        deallocate (ass_ENyoung)
        deallocate (ass_ENold)
        deallocate (ass_NEyoung)
        deallocate (ass_NEold)
        deallocate (lambda_ENyoung_E)
        deallocate (lambda_ENold_E)
        deallocate (lambda_NEyoung_E)
        deallocate (lambda_NEold_E)
        deallocate (lambda_ENyoung_X)
        deallocate (lambda_ENold_X)
        deallocate (lambda_NEyoung_X)
        deallocate (lambda_NEold_X)
        

        deallocate (step_hh_EN)
        deallocate (step_hsp_EN)
        deallocate (step_jobh_EN)
        deallocate (step_jobsp_EN)
        deallocate (step_LS_EN)

        deallocate (step_hh_NE)
        deallocate (step_hsp_NE)
        deallocate (step_jobh_NE)
        deallocate (step_jobsp_NE)
        deallocate (step_LS_NE)

    end subroutine exercise_sim_new



subroutine sim_step(a_p, indE_h, indE_sp, hh_s, hsp_s, t_s, indEN, sep, lambda_E, lambda_X, pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp, step_hh, step_hsp, step_jobh, step_jobsp, step_LS, LS_new)
                     
         use Globals            
        
        integer, intent(in) :: a_p, hh_s, hsp_s, t_s, indE_h, indE_sp, indEN ! indices for current states (future a)
        real*8, intent(in) :: sep, lambda_E, lambda_X ! job finding / job loss PBs
        real*8, intent(in) :: pb_Eh_Bh, pb_Xh_Bh, pb_Esp_Bsp, pb_Xsp_Bsp ! eligibility PBs conditional on employed / unemployed

        real*8, intent(in) :: step_hh, step_hsp, step_jobh, step_jobsp, step_LS ! fixed random draws for future states
        integer, intent(out) :: LS_new ! index of future labor state
        
        integer :: h_new, hsp_new
        real*8 :: PB_emp_h, PB_emp_sp ! job finding / job loss indicator
        real*8 :: dist_hp(HH)
        integer, dimension(HH) :: find_HC


        integer, dimension(16) :: find_LS
        real*8, dimension(16) :: PB_LS, PB_LS_cum
        ! 1 - EE, 2 - EU, 3 - UE, 4 - EN, 5 - NE, 6 - UU, 7 - UN, 8 - NU, 9 - NN  
        ! 10 - SS, 11 - SN, 12 - NS, 13 - SU, 14 - US, 15 - SE, 16 - ES
        
        !!!!!!!!!!!!!!!!!!!!!
        ! decide job offers
        !!!!!!!!!!!!!!!!!!!!!

        ! EN couple
        if (indEN==1) then

            ! head is separated
            if (step_jobh<=sep) then
                PB_emp_h = 0d0

                if (step_jobsp<=lambda_X) then
                    PB_emp_sp = 1d0
                else
                    PB_emp_sp = 0d0
                end if

            else !head is not separated
                PB_emp_h = 1d0

                if (step_jobsp<=lambda_E) then
                    PB_emp_sp = 1d0
                else
                    PB_emp_sp = 0d0
                end if

            end if

        elseif (indEN==2) then ! NE couple

                ! spouse is separated
                if (step_jobsp<=sep) then
                    PB_emp_sp = 0d0
    
                    if (step_jobh<=lambda_X) then
                        PB_emp_h = 1d0
                    else
                        PB_emp_h = 0d0
                    end if
    
                else ! spouse is not separated
                    PB_emp_sp = 1d0
    
                    if (step_jobh<=lambda_E) then
                        PB_emp_h = 1d0
                    else
                        PB_emp_h = 0d0
                    end if
    
                end if

        end if

        !!!!!!!!!!!!!!!!!!!!!!!!
        ! find future human capital
        !!!!!!!!!!!!!!!!!!!!!!!!
        ! for the head
        if (indE_h==1) then
            if (PB_emp_h==1d0) then
                dist_hp = cumsum(htrans_EE(hh_s,:))
            else
                dist_hp = cumsum(htrans_EX(hh_s,:))
            end if
        else
            dist_hp = cumsum(htrans_XX(hh_s,:))
        end if 
        find_HC = merge( 1, 0, dist_hp<step_hh )
        h_new = findpos(find_HC,0,0) 

        !for the spouse
        if (indE_sp==1) then
            if (PB_emp_sp==1d0) then
                dist_hp = cumsum(htrans_EE(hsp_s,:))
            else
                dist_hp = cumsum(htrans_EX(hsp_s,:))
            end if
        else
            dist_hp = cumsum(htrans_XX(hsp_s,:))
        end if 
        find_HC = merge( 1, 0, dist_hp<step_hsp )
        hsp_new = findpos(find_HC,0,0) 
        

        !!!!!!!!!!!!!!!!!!!!!
        ! find LS transition
        !!!!!!!!!!!!!!!!!!!!!
        
        ! fill distribution
        ! 1 - EE
        PB_LS(1)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_EE(a_p,   h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*pb_Esp_Bsp*pi_EE_XB_EE(a_p,   h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_EE(a_p,   h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_EE(a_p,   h_new, hsp_new, t_s+1))

        ! 2 - EU
        PB_LS(2)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_EU(a_p,   h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*pb_Esp_Bsp*pi_EE_XB_EU(a_p,   h_new, hsp_new, t_s+1)) &
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                (pb_Eh_Bh*pb_Xsp_Bsp*pi_EX_BB_EU(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*pb_Xsp_Bsp*pi_EX_XB_EU(a_p,  h_new, hsp_new, t_s+1)) 

        ! 3 - UE
        PB_LS(3)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_UE(a_p,   h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_UE(a_p,   h_new, hsp_new, t_s+1)) &
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                (pb_Xh_Bh*pb_Esp_Bsp*pi_XE_BB_UE(a_p, h_new, hsp_new, t_s+1) &
                +pb_Xh_Bh*(1d0-pb_Esp_Bsp)*pi_XE_BX_UE(a_p,  h_new, hsp_new, t_s+1)) 
                
        ! 4 - EN
        PB_LS(4)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_EN(a_p,   h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*pb_Esp_Bsp*pi_EE_XB_EN(a_p,   h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_EN(a_p,   h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_EN(a_p,   h_new, hsp_new, t_s+1))&
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                (pb_Eh_Bh*pb_Xsp_Bsp*pi_EX_BB_EN(a_p,  h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*pb_Xsp_Bsp*pi_EX_XB_EN(a_p,  h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Xsp_Bsp)*pi_EX_BX_EN(a_p,  h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Xsp_Bsp)*pi_EX_XX_EN(a_p,  h_new, hsp_new, t_s+1))

        ! 5 - NE
        PB_LS(5)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_NE(a_p,   h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*pb_Esp_Bsp*pi_EE_XB_NE(a_p,   h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_NE(a_p,   h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_NE(a_p,   h_new, hsp_new, t_s+1))&
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                (pb_Xh_Bh*pb_Esp_Bsp*pi_XE_BB_NE(a_p,  h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*pb_Esp_Bsp*pi_XE_XB_NE(a_p,  h_new, hsp_new, t_s+1) &
                +pb_Xh_Bh*(1d0-pb_Esp_Bsp)*pi_XE_BX_NE(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(1d0-pb_Esp_Bsp)*pi_XE_XX_NE(a_p, h_new, hsp_new, t_s+1))
        
        ! 6 - UU
        PB_LS(6)=(PB_emp_h * PB_emp_sp) * pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_UU(a_p,   h_new, hsp_new, t_s+1) &
                + ((1d0-PB_emp_h) * PB_emp_sp) * pb_Xh_Bh*pb_Esp_Bsp*pi_XE_BB_UU(a_p,  h_new, hsp_new, t_s+1) &
                + (PB_emp_h * (1d0-PB_emp_sp)) * pb_Eh_Bh*pb_Xsp_Bsp*pi_EX_BB_UU(a_p,  h_new, hsp_new, t_s+1) &
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * pb_Xh_Bh*pb_Xsp_Bsp*pi_XX_BB_UU(a_p, h_new, hsp_new, t_s+1)
        
        ! 7 - UN
        PB_LS(7)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_UN(a_p,   h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_UN(a_p,   h_new, hsp_new, t_s+1)) &
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                (pb_Xh_Bh*pb_Esp_Bsp*pi_XE_BB_UN(a_p,  h_new, hsp_new, t_s+1) &
                +pb_Xh_Bh*(1d0-pb_Esp_Bsp)*pi_XE_BX_UN(a_p,  h_new, hsp_new, t_s+1)) &
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                (pb_Eh_Bh*pb_Xsp_Bsp*pi_EX_BB_UN(a_p, h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Xsp_Bsp)*pi_EX_BX_UN(a_p,  h_new, hsp_new, t_s+1)) &
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * &
                (pb_Xh_Bh*pb_Xsp_Bsp*pi_XX_BB_UN(a_p, h_new, hsp_new, t_s+1) &
                +pb_Xh_Bh*(1d0-pb_Xsp_Bsp)*pi_XX_BX_UN(a_p, h_new, hsp_new, t_s+1)) 
                       
        ! 8 - NU
        PB_LS(8)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_NU(a_p,   h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(pb_Esp_Bsp)*pi_EE_XB_NU(a_p,   h_new, hsp_new, t_s+1)) &
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                (pb_Xh_Bh*pb_Esp_Bsp*pi_XE_BB_NU(a_p,  h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(pb_Esp_Bsp)*pi_XE_XB_NU(a_p,  h_new, hsp_new, t_s+1)) &
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                (pb_Eh_Bh*pb_Xsp_Bsp*pi_EX_BB_NU(a_p,  h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(pb_Xsp_Bsp)*pi_EX_XB_NU(a_p,  h_new, hsp_new, t_s+1)) &
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * &
                (pb_Xh_Bh*pb_Xsp_Bsp*pi_XX_BB_NU(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(pb_Xsp_Bsp)*pi_XX_XB_NU(a_p,  h_new, hsp_new, t_s+1)) 
                 
        ! 9 - NN
        PB_LS(9)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*pb_Esp_Bsp*pi_EE_BB_NN(a_p,   h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*pb_Esp_Bsp*pi_EE_XB_NN(a_p,   h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_NN(a_p,   h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_NN(a_p,   h_new, hsp_new, t_s+1)) &
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                (pb_Xh_Bh*pb_Esp_Bsp*pi_XE_BB_NN(a_p,   h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*pb_Esp_Bsp*pi_XE_XB_NN(a_p,  h_new, hsp_new, t_s+1) &
                +pb_Xh_Bh*(1d0-pb_Esp_Bsp)*pi_XE_BX_NN(a_p,  h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(1d0-pb_Esp_Bsp)*pi_XE_XX_NN(a_p,  h_new, hsp_new, t_s+1)) &
                + (PB_emp_h * (1d0-PB_emp_sp))  * &
                (pb_Eh_Bh*pb_Xsp_Bsp*pi_EX_BB_NN(a_p,   h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*pb_Xsp_Bsp*pi_EX_XB_NN(a_p,   h_new, hsp_new, t_s+1) &
                +pb_Eh_Bh*(1d0-pb_Xsp_Bsp)*pi_EX_BX_NN(a_p,  h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Xsp_Bsp)*pi_EX_XX_NN(a_p,  h_new, hsp_new, t_s+1)) &
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * &
                (pb_Xh_Bh*pb_Xsp_Bsp*pi_XX_BB_NN(a_p, h_new, hsp_new, t_s+1) &
                +pb_Xh_Bh*(1d0-pb_Xsp_Bsp)*pi_XX_BX_NN(a_p,  h_new, hsp_new, t_s+1) & 
                +(1d0-pb_Xh_Bh)*pb_Xsp_Bsp*pi_XX_XB_NN(a_p,  h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(1d0-pb_Xsp_Bsp)*pi_XX_XX_NN(a_p, h_new, hsp_new, t_s+1))  
                 
                
        ! 10 - SS
        PB_LS(10)=(PB_emp_h * PB_emp_sp) * &
                ((1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_SS(a_p,   h_new, hsp_new, t_s+1))&
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                ((1d0-pb_Xh_Bh)*(1d0-pb_Esp_Bsp)*pi_XE_XX_SS(a_p,  h_new, hsp_new, t_s+1))&
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                ((1d0-pb_Eh_Bh)*(1d0-pb_Xsp_Bsp)*pi_EX_XX_SS(a_p,  h_new, hsp_new, t_s+1))&
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * &
                ((1d0-pb_Xh_Bh)*(1d0-pb_Xsp_Bsp)*pi_XX_XX_SS(a_p, h_new, hsp_new, t_s+1)) 
        
        ! 11 - SN
        PB_LS(11)=(PB_emp_h * PB_emp_sp) * &
                ((1d0-pb_Eh_Bh)*pb_Esp_Bsp*pi_EE_XB_SN(a_p,   h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_SN(a_p,   h_new, hsp_new, t_s+1))&
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                ((1d0-pb_Xh_Bh)*pb_Esp_Bsp*pi_XE_XB_SN(a_p, h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(1d0-pb_Esp_Bsp)*pi_XE_XX_SN(a_p,  h_new, hsp_new, t_s+1))&
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                ((1d0-pb_Eh_Bh)*pb_Xsp_Bsp*pi_EX_XB_SN(a_p,  h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Xsp_Bsp)*pi_EX_XX_SN(a_p,  h_new, hsp_new, t_s+1))&
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * &
                ((1d0-pb_Xh_Bh)*pb_Xsp_Bsp*pi_XX_XB_SN(a_p,h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(1d0-pb_Xsp_Bsp)*pi_XX_XX_SN(a_p, h_new, hsp_new, t_s+1)) 
        
        ! 12 - NS
        PB_LS(12)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_NS(a_p,   h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_NS(a_p,   h_new, hsp_new, t_s+1))&
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                (pb_Xh_Bh*(1d0-pb_Esp_Bsp)*pi_XE_BX_NS(a_p,  h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(1d0-pb_Esp_Bsp)*pi_XE_XX_NS(a_p,  h_new, hsp_new, t_s+1))&
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                (pb_Eh_Bh*(1d0-pb_Xsp_Bsp)*pi_EX_BX_NS(a_p,   h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Xsp_Bsp)*pi_EX_XX_NS(a_p,   h_new, hsp_new, t_s+1))&
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * &
                (pb_Xh_Bh*(1d0-pb_Xsp_Bsp)*pi_XX_BX_NS(a_p,  h_new, hsp_new, t_s+1) & 
                +(1d0-pb_Xh_Bh)*(1d0-pb_Xsp_Bsp)*pi_XX_XX_NS(a_p, h_new, hsp_new, t_s+1)) 
        
        ! 13 - SU
        PB_LS(13)=(PB_emp_h * PB_emp_sp) * &
                (1d0-pb_Eh_Bh)*pb_Esp_Bsp*pi_EE_XB_SU(a_p,   h_new, hsp_new, t_s+1)&
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                (1d0-pb_Xh_Bh)*pb_Esp_Bsp*pi_XE_XB_SU(a_p,  h_new, hsp_new, t_s+1)  &
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                (1d0-pb_Eh_Bh)*pb_Xsp_Bsp*pi_EX_XB_SU(a_p,  h_new, hsp_new, t_s+1)  &
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * &
                (1d0-pb_Xh_Bh)*pb_Xsp_Bsp*pi_XX_XB_SU(a_p, h_new, hsp_new, t_s+1) 
         
        
        
        ! 14 - US
        PB_LS(14)=(PB_emp_h * PB_emp_sp) * &
                 pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_US(a_p,   h_new, hsp_new, t_s+1) &
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                pb_Xh_Bh*(1d0-pb_Esp_Bsp)*pi_XE_BX_US(a_p, h_new, hsp_new, t_s+1) &
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                 pb_Eh_Bh*(1d0-pb_Xsp_Bsp)*pi_EX_BX_US(a_p,  h_new, hsp_new, t_s+1) &
                + ((1d0-PB_emp_h) * (1d0-PB_emp_sp)) * &
                pb_Xh_Bh*(1d0-pb_Xsp_Bsp)*pi_XX_BX_US(a_p, h_new, hsp_new, t_s+1) 
         
         
         
        ! 15 - SE
        PB_LS(15)=(PB_emp_h * PB_emp_sp) * &
                ((1d0-pb_Eh_Bh)*pb_Esp_Bsp*pi_EE_XB_SE(a_p,   h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_SE(a_p,   h_new, hsp_new, t_s+1))&
                + ((1d0-PB_emp_h) * PB_emp_sp) * &
                ((1d0-pb_Xh_Bh)*pb_Esp_Bsp*pi_XE_XB_SE(a_p,  h_new, hsp_new, t_s+1) &
                +(1d0-pb_Xh_Bh)*(1d0-pb_Esp_Bsp)*pi_XE_XX_SE(a_p,   h_new, hsp_new, t_s+1))
                
   

        ! 16 - ES
        PB_LS(16)=(PB_emp_h * PB_emp_sp) * &
                (pb_Eh_Bh*(1d0-pb_Esp_Bsp)*pi_EE_BX_ES(a_p,   h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Esp_Bsp)*pi_EE_XX_ES(a_p,   h_new, hsp_new, t_s+1))&
                + (PB_emp_h * (1d0-PB_emp_sp)) * &
                (pb_Eh_Bh*(1d0-pb_Xsp_Bsp)*pi_EX_BX_ES(a_p,  h_new, hsp_new, t_s+1) &
                +(1d0-pb_Eh_Bh)*(1d0-pb_Xsp_Bsp)*pi_EX_XX_ES(a_p,  h_new, hsp_new, t_s+1))

        


        PB_LS_cum = cumsum(PB_LS) 

   !     if(PB_LS_cum(16)< 1d0) print *, 'Error in LS transitions', PB_LS_cum(16)
    
        find_LS = merge( 1, 0, PB_LS_cum<step_LS )
        LS_new = findpos(find_LS,0,0)  
        
   !     if(LS_new == 0) print *, 'new LS is zero', PB_LS       
                 
    end subroutine sim_step

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

end module
