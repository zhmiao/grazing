module structure
  ! Structure of the program

  ! use parameter module
  ! use parameter_var
  
  ! ================================================

  ! the first type variable LANDCELL {{{
  type  LANDCELL
    ! for rainfall {{{
    real                               :: RAINFALL_mu                     ! normal random deviat rain fall amount in each cell
    real                               :: RAINFALL                        ! rain fall amount in each cell }}}

    ! for plant growth {{{
    integer                            :: RAIN_SEA                        ! Rain season
    real, dimension(:), allocatable    :: GROW_DAYS                       ! plant growing days each year in each cell
    real, dimension(:), allocatable    :: GROW_DAYS_CO                    ! Growth days coefficient
    real                               :: DAY_RAIN                        ! (mm^2) Daily rainfall use efficiency
    ! real                               :: MAX_DAILY_GROWTH                ! maximum daily grass growth
    real, dimension(:), allocatable    :: TOT_BIO_SPP                     ! (g) total aboveground plant biomass of different species
    real                               :: TOT_BIOMASS                     ! (g) total biomass
    real, dimension(:), allocatable    :: AV_BIO_SPP                      ! (g) available aboveground plant biomass of different species
    real, dimension(:), allocatable    :: AV_BIO_SPP_P                    ! (g) Stores species available biomass for future modifications
    real                               :: AV_BIOMASS                      ! (g) total available biomass
    real                               :: AV_BIOMASS_P                    ! (g) Stores cell level total availablr biomass for future modifications
    real, dimension(:), allocatable    :: UAV_BIO_SPP                     ! (g) unavailable aboveground plant biomass of different species
    real                               :: UAV_BIOMASS                     ! (g) total unavailable biomass
    real, dimension(:), allocatable    :: SPP_K                           ! (g) grass growth carrying capacity for each plant species
    real, dimension(:), allocatable    :: SPP_K_CO                        ! Carrying capacit coefficient
    real, dimension(:), allocatable    :: G_R                             ! Actual grwoth rate
    real, dimension(:), allocatable    :: R_MAX_CO                        ! R_MAX coefficient
    real, dimension(:), allocatable    :: D_R                             ! Actual decrease rate
    real, dimension(:), allocatable    :: DECREASE_R_CO                   ! DECREASE_R coefficient
    real                               :: TOT_K                           ! (g) total carrying capacity }}}

    ! variables that's not included in the original plant growth model {{{
    real, dimension(:), allocatable    :: SPP_LAI                         ! LAI for each plant species
    real                               :: TOT_LAI                         ! total LAI
    real, dimension(:), allocatable    :: SPP_DEN                         ! Plant density for each plant species
    real, dimension(:), allocatable    :: SPP_RES                         ! Plant respiration for each plant species
    real, dimension(:), allocatable    :: SPP_MOR                         ! Mortality rate for each plant species
    real, dimension(:), allocatable    :: SPP_GRO                         ! Growth rate for each plant species
    real, dimension(:), allocatable    :: SPP_PS                          ! Photothesis amount for each plant species
    real, dimension(:), allocatable    :: SPP_RI                          ! Rainfall interception fraction for each plant species
    real, dimension(:), allocatable    :: SPP_CC                          ! Carbon conversion rate from photosynthesis for each plant species
    real, dimension(:), allocatable    :: SPP_NU                          ! N uptake for each plant species
    real, dimension(:), allocatable    :: SPP_RT                          ! Root to shoot ratio for each plant species

    real                               :: AN_POOL                         ! Available N pool
    real                               :: LIT_POOL_D                      ! (g) Litter pool change caused by grazing animal
    real                               :: LIT_N                           ! Organic N in litter

    real                               :: POT_ETP                         ! Potential evapotransipration
    real                               :: POT_EVP                         ! Potential soil evaporation
    real, dimension(:), allocatable    :: SPP_TRP                         ! Potential transpiration for each plant species }}}

    ! for grazing !{{{
    real                               :: GR_SW                           ! Used to control whether there is grazing
    real, dimension(:,:), allocatable  :: SPP_GRAZED                      ! (g) daily grazed amount from animal i of plant j
    real, dimension(:,:), allocatable  :: SPP_DETACH                      ! (g) daily detched amount from animal i of plant j
    real, dimension(:), allocatable    :: SD                              ! stocking density for each animal species
    real                               :: TOT_SD                          ! total stocking density
    integer, dimension(:), allocatable :: SS_PR_CLA                       ! site preference class for each animal species
    integer                            :: SS_PR_SCR                       ! site preference score }}}

    ! geographical attributes !{{{
    real                               :: SLOPE                           ! slope
    real                               :: WATER_DIST                      ! distance to water resource
    real                               :: SNOW_COV                        ! snow cover
    real                               :: SOIL_COM                        ! soil compactness
    real                               :: SOIL_DCOM                       ! change in soil compactness }}}

  end type LANDCELL
  ! define the LAND vairable
  type(LANDCELL), dimension(:,:), allocatable :: CELL !}}}

  ! ================================================

  ! Row attributes {{{
  type  ROW_ATT
    real                          ::        AVG_RAINFALL_mu          ! mean of average annual rain fall each row
    real                          ::        AVG_RAINFALL             ! actual annual rain fall generated using a normal distribution
  end type ROW_ATT
  ! define one variable with one dimension
  type(ROW_ATT), dimension(:), allocatable :: ROW !}}}

  ! ================================================

  ! Rainfall parameter !{{{
  real, parameter                 :: AVG_N=671, AVG_S=529            ! northern most and southern most average annual rainfall }}}

  ! Global plant variables !{{{
  real, dimension(:), allocatable :: K_CO                            ! carrying capacity coefficient (default value=80.872)
  real, dimension(:), allocatable :: DECREASE_R                      ! biomass decrese rate during dry season (default value=1.79)
  real, dimension(:), allocatable :: R_MAX                           ! maximum grass growth rate (default value=0.039)
  real, dimension(:), allocatable :: SPP_BIOMASS                     ! (g) Global total biomass for each plant species
  real                            :: TOT_BIOMASS                     ! (g) Global total plant biomass
  real                            :: AV_BIOMASS                      ! (g) Global available plant biomass
  real                            :: AV_BIOMASS_P                    ! (g) Stores global available biomass for future modifications
  real                            :: UAV_BIOMASS                     ! (g) Global unavailable plant biomass
  real                            :: TOT_K                           ! (g) Global total plant carrying capacity }}}
  real, dimension(:), allocatable :: SPP_N_CON                       ! (gN/gPlant) Nitrogen concentration for each plant species
  real                            :: TOT_N_CON                       ! (gN/gPlant) Total Nitrogen concentration
  real, dimension(:), allocatable :: SPP_CN                          ! Plant C to N ratio

  ! Global geographical variables !{{{
  real                            :: SOIL_ALB                        ! Soil albedo (0-0.1)
  real                            :: SOLA_RAD                        ! Solar radiation (Langleys)
  real                            :: AVG_TEMP                        ! Average temperature (K) }}}

  ! Effects functions !{{{
  real, dimension(:), allocatable :: RES_RATE                        ! Respiration rate for each plant species }}}


end module structure
