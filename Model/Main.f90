!#######################################################################
! Main program file for "Joint Search over the Life Cycle"
! Annika Bacher, Philipp Gruebener, Lukas Nord
!######################################################################

include "Globals.f90"
include "Sort.f90"
include "Interpolation_Routines.f90"
include "Initialization.f90"
include "Household_Problem.f90"
include "Simulation.f90"
include "Exercise_new.f90"

program Main

    use Globals
    use Initialization
    use Household_Problem
    use Simulation
    use Interpolation_Routines
    use Exercise_new
    
    implicit none

    ! set internally calibrated parameters
    beta = 0.9955d0
    eps1 = 2.2d0
    eps2 = 1.7d0
    eps3 = 0.2d0
    psi_search = 0.58d0 
    psi_scale = 1.35d0  
    lambda_u = 0.3d0
    lambda_n = 0.2d0
    lambda_s = lambda_u
    phidown_lev = 0.05d0
    phiup_lev = 0.1d0
    phiup_exp = -1.8d0
    kappa = 8d0
    phidown_emp_lev = 0d0
    
    ! Initialization
    call initialize()
    
    ! Solve model
    call solve_model()
    
    ! Simulate model
     call simulate()
     
     ! Conduct exercise
     call exercise_sim_new()

end program