      MODULE mod_ncparam
!
!git $Id$
!svn $Id: mod_ncparam.F 1215 2024-02-09 23:11:09Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This MODULE contains all the variables associated with input and    !
!  output  NetCDF  files.  The IO model is very generic and easy to    !
!  change or expand.  The NetCDF files can be in any language.  All    !
!  the IO information is managed using the following variables:        !
!                                                                      !
!  Vname      Input/output variables names and attributes:             !
!               Vname(1,*)  => field variable name.                    !
!               Vname(2,*)  => long-name attribute.                    !
!               Vname(3,*)  => units attribute.                        !
!               Vname(4,*)  => field attribute.                        !
!               Vname(5,*)  => associated time variable name.          !
!               Vname(6,*)  => standard-name attribute.                !
!  Tname      Input/output associated time variables names.            !
!                                                                      !
!  Cinfo      Input/output file names associated with each field       !
!                                                                      !
!  Linfo      Input/output fields logical information:                 !
!               Linfo(1,*)  => switch indicating grided data.          !
!               Linfo(2,*)  => switch indicating time cycling.         !
!               Linfo(3,*)  => switch indicating only one-time         !
!                              record available.                       !
!               Linfo(4,*)  => switch indicating special record        !
!                              processing (like tides)                 !
!               Linfo(5,*)  => switch to indicate processing the first !
!                              record of a file or multi-file.         !
!               Linfo(6,*)  => switch to indicate processing the last  !
!                              record of a file or multi-file.         !
!                                                                      !
!  Iinfo      Input/output fields integer information:                 !
!               Iinfo( 1,*) => variable grid type.                     !
!               Iinfo( 2,*) => field variable NetCDF ID.               !
!               Iinfo( 3,*) => associated time variable NetCDF ID.     !
!               Iinfo( 4,*) => number of time records.                 !
!               Iinfo( 5,*) => size of first  spatial dimension.       !
!               Iinfo( 6,*) => size of second spatial dimension.       !
!               Iinfo( 7,*) => size of third  spatial dimension.       !
!               Iinfo( 8,*) => rolling two-time levels index.          !
!               Iinfo( 9,*) => latest processed time record.           !
!               Iinfo(10,*) => number of field multi-files.            !
!               Iinfo(11,*) => number of variable dimensions in file.  !
!                                                                      !
!  Finfo      Input/output field floating-point information:           !
!               Finfo( 1,*) => Starting time (days) of data.           !
!               Finfo( 2,*) => Ending time (days) of data.             !
!               Finfo( 3,*) => Data time lower bound (days) enclosing  !
!                                model starting time.                  !
!               Finfo( 4,*) => Data time upper bound (days) enclosing  !
!                                model starting time.                  !
!               Finfo( 5,*) => length (days) of time cycling.          !
!               Finfo( 6,*) => Scale to convert time to day units.     !
!               Finfo( 7,*) => latest monotonic time (sec).            !
!               Finfo( 8,*) => minimum value for current data.         !
!               Finfo( 9,*) => maximum value for current data.         !
!               Finfo(10,*) => value of scale_factor attribute if any. !
!  Fscale     Scale to convert input data to model units.              !
!  Fpoint     Latest two-time records of input point data.             !
!  Tintrp     Time (sec) of latest field snapshots used for            !
!               interpolation.                                         !
!  Vtime      Latest two-time values of processed input data.          !
!                                                                      !
!=======================================================================
!
      USE mod_param
!
      implicit none
!
      PUBLIC :: allocate_ncparam
      PUBLIC :: deallocate_ncparam
      PUBLIC :: initialize_ncparam
!
!-----------------------------------------------------------------------
!  Define variables in module.
!-----------------------------------------------------------------------
!
!  Set type of IO NetCDF files interface library
!
      integer, parameter :: io_nf90 = 1   ! standard NetCDF-3, NetCDF-4
      integer, parameter :: io_pio  = 2   ! Parallel-IO, SCORPIO
!
      integer :: inp_lib                  ! input  NetCDF library
      integer :: out_lib                  ! output NetCDF library
!
!  Maximum number of variables in a generic NetCDF file (MV) and
!  maximum number of variables in information arrays (NV).
!
      integer, parameter :: MV = 1400
      integer, parameter :: NV = 1400
!
!  Maximum number of dimension IDs stored in local array DimIDs used to
!  defined variable dimensions in output NetCDF files.
!
      integer, parameter :: nDimID = 33
!
!  Number of horizontal interior and boundary water points.
!
      integer, allocatable :: Nxyp(:)          ! PSI water points
      integer, allocatable :: Nxyr(:)          ! RHO water points
      integer, allocatable :: Nxyu(:)          ! U water points
      integer, allocatable :: Nxyv(:)          ! V water points
!
!  Number of horizontal interior water points only.
!
      integer, allocatable :: NwaterR(:)       ! RHO water points
      integer, allocatable :: NwaterU(:)       ! U water points
      integer, allocatable :: NwaterV(:)       ! V water points
!
!  Lower and upper bound ranges for RHO-type variables for processing
!  state vector and observations.
!
      integer, allocatable :: rILB(:)
      integer, allocatable :: rIUB(:)
      integer, allocatable :: rJLB(:)
      integer, allocatable :: rJUB(:)
!
      real(r8), allocatable :: rXmin(:)
      real(r8), allocatable :: rXmax(:)
      real(r8), allocatable :: rYmin(:)
      real(r8), allocatable :: rYmax(:)
!
!  Lower and upper bound ranges for U-type variables for processing
!  state vector and observations.
!
      integer, allocatable :: uILB(:)
      integer, allocatable :: uIUB(:)
      integer, allocatable :: uJLB(:)
      integer, allocatable :: uJUB(:)
!
      real(r8), allocatable :: uXmin(:)
      real(r8), allocatable :: uXmax(:)
      real(r8), allocatable :: uYmin(:)
      real(r8), allocatable :: uYmax(:)
!
!  Lower and upper bound ranges for V-type variables for processing
!  state vector and observations.
!
      integer, allocatable :: vILB(:)
      integer, allocatable :: vIUB(:)
      integer, allocatable :: vJLB(:)
      integer, allocatable :: vJUB(:)
!
      real(r8), allocatable :: vXmin(:)
      real(r8), allocatable :: vXmax(:)
      real(r8), allocatable :: vYmin(:)
      real(r8), allocatable :: vYmax(:)
!
!  Switches indicating which variables are written to output files.
!
      logical, allocatable :: Aout(:,:)     ! average file switches
      logical, allocatable :: Dout(:,:)     ! diagnostic file switches
      logical, allocatable :: Hout(:,:)     ! history file switches
      logical, allocatable :: Qout(:,:)     ! quicksave file switches
      logical, allocatable :: Sout(:,:)     ! station file switches
!
!  Grid identification indices.
!
      integer  :: idXgrd = -1   ! XI-grid position
      integer  :: idYgrd = -2   ! ETA-grid position
      integer  :: idZgrd = -3   ! S-grid position
      integer  :: iddpth = -4   ! depth
      integer  :: idglon = -5   ! longitude
      integer  :: idglat = -6   ! latitude
!
!  Input/output identification indices.
!
      integer  :: idangR        ! angle between XI-axis and EAST
      integer  :: idbath        ! time-dependent bathymetry
      integer  :: idBgEr        ! background error at observations
      integer  :: idBgTh        ! threshold for BQC check
      integer  :: idCfra        ! cloud fraction
      integer  :: idCosW        ! COS(omega(k)*t)
      integer  :: idCos2        ! COS(omega(k)*t)*COS(omega(l)*t)
      integer  :: idDano        ! density anomaly
      integer  :: idDiff(2)     ! temperature and salinity diffusion
      integer  :: iddQdT        ! heat flux sensitivity to SST
      integer  :: idEmPf        ! E-P from bulk_flux.F
      integer  :: idevap        ! evaporation rate
      integer  :: idfcor        ! Coriolis parameter
      integer  :: idFsur        ! free-surface
      integer  :: idFsuD        ! detided free-surface
      integer  :: idFsuH        ! free-surface tide harmonics
      integer  :: idGhat(2)     ! KPP nonlocal transport
      integer  :: idgTnc        ! generic tracer nudging coefficients
      integer  :: idJWTy        ! Jerlov water type
      integer  :: idHbbl        ! depth of bottom boundary layer
      integer  :: idHsbl        ! depth of surface boundary layer
      integer  :: idIncr        ! 4D-Var analysis increment
      integer  :: idInno        ! 4D-Var innovation vector
      integer  :: idKhor        ! convolution horizontal diffusion
      integer  :: idKver        ! convolution vertical diffusion
      integer  :: idLatP        ! latitude of PSI-points
      integer  :: idLatR        ! latitude of RHO-points
      integer  :: idLatU        ! latitude of U-points
      integer  :: idLatV        ! latitude of V-points
      integer  :: idLdwn        ! downwelling longwave radiation flux
      integer  :: idLonP        ! longitude of PSI-points
      integer  :: idLonR        ! longitude of RHO-points
      integer  :: idLonU        ! longitude of U-points
      integer  :: idLonV        ! longitude of V-points
      integer  :: idLhea        ! net latent heat flux
      integer  :: idLrad        ! net longwave radiation flux
      integer  :: idMadH        ! ADM interpolation weights
      integer  :: idMOMi        ! Initial model-observation misfit
      integer  :: idMOMf        ! final model-observation misfit
      integer  :: idmskP        ! land/mask on PSI-points
      integer  :: idmskR        ! land/mask on RHO-points
      integer  :: idmskU        ! land/mask on U-points
      integer  :: idmskV        ! land/mask on V-points
      integer  :: idMtke        ! turbulent kinetic energy
      integer  :: idMtls        ! turbulent length scale
      integer  :: idM2nc        ! 2D momentum nudging coefficients
      integer  :: idM3nc        ! 2D momentum nudging coefficients
      integer  :: idNLmf        ! final NLM at observation locations
      integer  :: idNLmi        ! initial NLM at observation locations
      integer  :: idNLmo        ! NLM at observations locations
      integer  :: idNLmp        ! unvetted prior NLM at obs locations
      integer  :: idNLmu        ! unvetted NLM at obs locations
      integer  :: idNobs        ! number of observations
      integer  :: idObsD        ! observations depth
      integer  :: idObsS        ! observations screening scale
      integer  :: idObsT        ! observations time
      integer  :: idObsX        ! observations X-grid location
      integer  :: idObsY        ! observations Y-grid location
      integer  :: idObsZ        ! observations Z-grid location
      integer  :: idOday        ! observations survey time
      integer  :: idOerr        ! observations error
      integer  :: idOlat        ! observations latitude
      integer  :: idOlon        ! observations longitude
      integer  :: idOmet        ! observations meta value
      integer  :: idOpro        ! observations provenance
      integer  :: idOtyp        ! observations type
      integer  :: idOval        ! observations value
      integer  :: idOvel        ! omega vertical velocity
      integer  :: idOvil        ! omega vertical velocity implicit
      integer  :: idPair        ! surface air pressure
      integer  :: idPbar        ! streamfunction
      integer  :: idPwet        ! wet/dry mask on PSI-points
      integer  :: idpmdx        ! inverse grid x-spacing
      integer  :: idpndy        ! inverse grid y-spacing
      integer  :: idpthR        ! depths of RHO-points
      integer  :: idpthU        ! depths of U-points
      integer  :: idpthV        ! depths of V-points
      integer  :: idpthW        ! depths of W-points
      integer  :: idQair        ! surface air humidity
      integer  :: idRdir        ! river runoff direction
      integer  :: idRepo        ! river runoff ETA-positions
      integer  :: idRflg        ! river runoff flag
      integer  :: idRtra        ! river runoff mass transport
      integer  :: idRuct        ! RHS of U-momentum coupling term
      integer  :: idRu2d        ! RHS of 2D U-momentum
      integer  :: idRu3d        ! RHS of total U-momentum
      integer  :: idRvct        ! RHS of V-momentum coupling term
      integer  :: idRv2d        ! RHS of 2D V-momentum
      integer  :: idRv3d        ! RHS of total V-momentum
      integer  :: idRxpo        ! river runoff XI-positions
      integer  :: idRvsh        ! river runoff transport profile
      integer  :: idRwet        ! wet/dry mask on RHO-points
      integer  :: idRzet        ! RHS of free-surface
      integer  :: idrain        ! rainfall rate
      integer  :: idragL        ! bottom linear drag coefficient
      integer  :: idragQ        ! bottom quadratic drag coefficient
      integer  :: idSdif        ! vertical S-diffusion coefficient
      integer  :: idSinW        ! SIN(omega(k)*t)
      integer  :: idSin2        ! SIN(omega(k)*t)*SIN(omega(l)*t)
      integer  :: idSrad        ! net shortwave radiation flux
      integer  :: idSSHc        ! SSH climatology
      integer  :: idSSSc        ! SSS climatology
      integer  :: idSSTc        ! SST climatology
      integer  :: idShea        ! net sensible heat flux
      integer  :: idSWCW        ! SIN(omega(k)*t)*COS(omega(l)*t)
      integer  :: idResi        ! 4D-Var residual vector
      integer  :: idsfwf        ! surface freswater flux
      integer  :: idscoR        ! ADM sensitivity scope RHO-mask
      integer  :: idscoU        ! ADM sensitivity scope U-mask
      integer  :: idscoV        ! ADM sensitivity scope V-mask
      integer  :: idTLmo        ! TLM at observation locations
      integer  :: idTair        ! surface air temperature
      integer  :: idTdif        ! vertical T-diffusion coefficient
      integer  :: idtime        ! ocean time
      integer  :: idtopo        ! time-independent bathymetry
      integer  :: idTref        ! tidal reference date for zero phase
      integer  :: idTper        ! tidal period
      integer  :: idTvan        ! tidal current angle
      integer  :: idTvma        ! maximum tidal current
      integer  :: idTvmi        ! minimum tidal current
      integer  :: idTvph        ! tidal current phase
      integer  :: idTzam        ! tidal elevation amplitude
      integer  :: idTzph        ! tidal elevation phase
      integer  :: idu2dA        ! accumulated 2D U-velocity
      integer  :: idU2rs        ! 2D total U-radiation stress
      integer  :: idU3rs        ! 3D total U-radiation stress
      integer  :: idU2Sd        ! 2D U-Stokes velocity
      integer  :: idU3Sd        ! 3D U-Stokes velocity
      integer  :: idUads        ! 3D U-velocity adjoint sensitivity
      integer  :: idUair        ! surface U-wind
      integer  :: idUaiE        ! Eastward surface U-wind
      integer  :: idUbar        ! 2D U-velocity
      integer  :: idUbas        ! 2D U-velocity adjoint sensitivity
      integer  :: idUbcl        ! 2D U-velocity climatology
      integer  :: idUbcs        ! bottom max U-momentum-wave stress
      integer  :: idUbed        ! bed load U-direction
      integer  :: idUbms        ! bottom U-momentum stress
      integer  :: idUbot        ! bed wave orbital U-velocity
      integer  :: idUbrs        ! bottom U-current stress
      integer  :: idUbtf        ! 2D U-velocity impulse forcing
      integer  :: idUbur        ! bottom U-velocity above bed
      integer  :: idUbws        ! bottom U-wave stress
      integer  :: idUclm        ! 3D U-velocity climatology
      integer  :: idUfx1        ! time averaged U-flux for 2D
      integer  :: idUfx2        ! time averaged U-flux for 3D
      integer  :: idUsms        ! surface U-momentum stress
      integer  :: idUsuE        ! model surface eastward velocity
      integer  :: idUsur        ! surface U-velocity
      integer  :: idUtlf        ! 3D U-velocity impulse forcing
      integer  :: idUvel        ! 3D U-velocity
      integer  :: idUVwc        ! bottom wave-current stress magnitude
      integer  :: idUwav        ! 2D U-velocity Kirby and Chen
      integer  :: idUwet        ! wet/dry mask on U-points
      integer  :: idu2dD        ! detided 2D U-velocity
      integer  :: idu2dH        ! 2D U-velocity tide harmonics
      integer  :: idu2dE        ! 2D eastward velocity at RHO-points
      integer  :: idu3dD        ! detided 3D U-velocity
      integer  :: idu3dH        ! 3D U-velocity tide harmonics
      integer  :: idu3dE        ! 3D eastward velocity at RHO-points
      integer  :: idV2rs        ! 2D total V-radiation stress
      integer  :: idV3rs        ! 3D total V-radiation stress
      integer  :: idV2Sd        ! 2D U-Stokes velocity
      integer  :: idV3Sd        ! 3D U-Stokes velocity
      integer  :: idVads        ! 3D V-velocity adjoint sensitivity
      integer  :: idVair        ! surface V-wind
      integer  :: idVaiN        ! Northward surface V-wind
      integer  :: idVbar        ! 2D V-velocity
      integer  :: idVbas        ! 2D V-velocity adjoint sensitivity
      integer  :: idVbcl        ! 2D V-velocity climatology
      integer  :: idVbcs        ! bottom max V-current-wave stress
      integer  :: idVbed        ! bed load V-direction
      integer  :: idVbms        ! bottom V-momentum stress
      integer  :: idVbot        ! bed wave orbital V-velocity
      integer  :: idVbrs        ! bottom V-current stress
      integer  :: idVbtf        ! 2D V-velocity impulse forcing
      integer  :: idVbvr        ! bottom V-velocity above bed
      integer  :: idVbws        ! bottom V-wave stress
      integer  :: idVclm        ! 3D V-velocity climatology
      integer  :: idVfx1        ! 2D momentum time-averaged V-flux
      integer  :: idVfx2        ! 3D momentum time-averaged V-flux
      integer  :: idVmLS        ! vertical mixing length scale
      integer  :: idVmKK        ! Kinetic energy vertical mixing
      integer  :: idVmKP        ! Length scale vertical mixing
      integer  :: idVsms        ! surface V-momentum stress
      integer  :: idVsuN        ! model surface northward velocity
      integer  :: idVsur        ! surface V-velocity
      integer  :: idVtlf        ! 3D V-velocity impulse forcing
      integer  :: idVvel        ! 3D V-velocity
      integer  :: idVvis        ! vertical viscosity coefficient
      integer  :: idVwav        ! 2D V-velocity Kirby and Chen
      integer  :: idVwet        ! wet/dry mask on V-points
      integer  :: idv2dD        ! detided 2D U-velocity
      integer  :: idv2dH        ! 2D U-velocity tide harmonics
      integer  :: idv2dN        ! 2D northward velocity at RHO-points
      integer  :: idv3dD        ! detided 3D U-velocity
      integer  :: idv3dH        ! 3D U-velocity tide harmonics
      integer  :: idv3dN        ! 3D northward velocity at RHO-points
      integer  :: idW3Sd        ! 3D W-Stokes omega drift velocity
      integer  :: idW3St        ! 3D W-Stokes vertical drift velocity
      integer  :: idW2xx        ! 2D radiation stress, Sxx-component
      integer  :: idW2xy        ! 2D radiation stress, Sxy-component
      integer  :: idW2yy        ! 2D radiation stress, Syy-component
      integer  :: idW3xx        ! 3D radiation stress, Sxx-component
      integer  :: idW3xy        ! 3D radiation stress, Sxy-component
      integer  :: idW3yy        ! 3D radiation stress, Syy-component
      integer  :: idW3zx        ! 3D radiation stress, Szx-component
      integer  :: idW3zy        ! 3D radiation stress, Szy-component
      integer  :: idWads        ! 3D W-velocity adjoint sensitivity
      integer  :: idWamp        ! wind-induced wave amplitude
      integer  :: idWam2        ! wind-induced wave amplitude squared
      integer  :: idWbeh        ! wec Bernoulli head
      integer  :: idWbrk        ! wind-induced wave breaking
      integer  :: idWdib        ! wave dissipation from surface breaking
      integer  :: idWdif        ! wave dissipation from bottom friction
      integer  :: idWdip        ! wind-induced peak wave direction
      integer  :: idWdir        ! wind-induced mean wave direction
      integer  :: idWdis        ! wind-induced wave roller dissipation
      integer  :: idWdiw        ! wave dissipation from whitecapping
      integer  :: idWlen        ! wind-induced mean wave length
      integer  :: idWlep        ! wind-induced peak wave length
      integer  :: idWmsk        ! wet-dry mask
      integer  :: idWorb        ! wind-induced wave orbital velocity
      integer  :: idWptp        ! wind-induced surface wave period
      integer  :: idWpbt        ! wind-induced bottom wave period
      integer  :: idWqsp        ! wec quasi-static pressure
      integer  :: idWrol        ! wave roller action density
      integer  :: idWvds        ! wave directional spread
      integer  :: idWvel        ! true vertical velocity
      integer  :: idWvqp        ! wave spectrum peakedness
      integer  :: idWztw        ! wec quasi-static sea level adjustment
      integer  :: idXgrP        ! x-locations of PSI-points
      integer  :: idXgrR        ! x-locations of RHO-points
      integer  :: idXgrU        ! x-locations of U-points
      integer  :: idXgrV        ! x-locations of V-points
      integer  :: idYgrP        ! y-locations of PSI-points
      integer  :: idYgrR        ! y-locations of RHO-points
      integer  :: idYgrU        ! y-locations of U-points
      integer  :: idYgrV        ! y-locations of V-points
      integer  :: idZoBL        ! bottom roughness length
      integer  :: idZads        ! Free-surface adjoint sensitivity
      integer  :: idZtlf        ! Free-surface impulse forcing
      integer  :: id2dPV        ! 2D potential vorticity
      integer  :: id2dRV        ! 2D relative vorticity
      integer  :: id3dPV        ! 3D potential vorticity
      integer  :: id3dRV        ! 3D relative vorticity
!
!  Last used variable ID counter.
!
      integer  :: last_varid
!
!  Input/output identification tracer indices.
!
      integer, allocatable :: idRtrc(:)    ! river runoff for tracers
      integer, allocatable :: idsurT(:)    ! model surface tracers
      integer, allocatable :: idTads(:)    ! tracers adjoint sentivity
      integer, allocatable :: idTbot(:)    ! bottom flux for tracers
      integer, allocatable :: idTbry(:,:)  ! tracers boundary
      integer, allocatable :: idTclm(:)    ! tracers climatology
      integer, allocatable :: idTnud(:)    ! tracers nudge coefficient
      integer, allocatable :: idTsur(:)    ! surface flux for tracers
      integer, allocatable :: idTtlf(:)    ! tracers impulse forcing
!
!  Boundary conditions identification indices.
!
      integer  :: idU2bc(4)      ! 2D U-velocity boundary conditions
      integer  :: idU3bc(4)      ! 3D U-velocity boundary conditions
      integer  :: idV2bc(4)      ! 2D V-velocity boundary conditions
      integer  :: idV3bc(4)      ! 3D V-velocity boundary conditions
      integer  :: idZbry(4)      ! free-surface boundary conditions
!
!  Time-averaged quadratic terms IDs.
!
      integer  :: idU2av                    ! <ubar*ubar>
      integer  :: idV2av                    ! <vbar*vbar>
      integer  :: idZZav                    ! <zeta*zeta>
      integer  :: idHUav                    ! <Huon>
      integer  :: idHVav                    ! <Hvom>
      integer  :: idUUav                    ! <u*u>
      integer  :: idUVav                    ! <u*v>
      integer  :: idVVav                    ! <v*v>
      integer, allocatable :: iHUTav(:)     ! <Huon*t> for active tracers
      integer, allocatable :: iHVTav(:)     ! <Hvom*t> for active tracers
      integer, allocatable :: idTTav(:)     ! <t*t> for active tracers
      integer, allocatable :: idUTav(:)     ! <u*t> for active tracers
      integer, allocatable :: idVTav(:)     ! <v*t> for active tracers
!
!  Tracer/Momentum Diagnostic variable IDs.
!
      integer, allocatable :: idDtrc(:,:)   ! tracers terms
      integer, allocatable :: idDu2d(:)     ! 2D u-momentum terms
      integer, allocatable :: idDv2d(:)     ! 2D v-momentum terms
      integer, allocatable :: idDu3d(:)     ! 3D u-momentum terms
      integer, allocatable :: idDv3d(:)     ! 3D v-momentum terms
!
!  State variables indices (order is important). Notice that current
!  extra-observations index (isRadial) needs to be initialized to zero
!  here and reset elsewhere to the value provided by the user.
!  Currently, the maximum number of state variables in 3D applications
!  is 9+2*MT. If MT=2 (temperature and salinity), the state variables
!  have the following order:
!
!     1,   2,   3,4,5,      6,      7,  8,9,  10,  11,     12,     13
!  zeta,ubar,vbar,u,v,Tvar(1),Tvar(2),TKE,W,Ustr,Vstr,Tsur(1),Tsur(2)
!
      integer  :: isFsur = 1                ! free-surface
      integer  :: isUbar = 2                ! 2D U-velocity
      integer  :: isVbar = 3                ! 2D V-velocity
      integer  :: isUvel = 4                ! 3D U-velocity
      integer  :: isVvel = 5                ! 3D V-velocity
      integer  :: isWvel                    ! 3D W-velocity
      integer  :: isRadial = 0              ! HF radial-velocity
      integer  :: isUstr                    ! surface u-stress
      integer  :: isVstr                    ! surface v-stress
      integer  :: isMtke                    ! turbulent kinetic energy
      integer, allocatable :: isTsur(:)     ! surface tracer flux
      integer, allocatable :: isTvar(:)     ! tracers
      integer, allocatable :: idBvar(:)     ! LBC variables indices
      integer, allocatable :: idSvar(:)     ! state vector indices
      integer, allocatable :: idSbry(:)     ! state boundaries indices
!
!  Generic state variables lateral boundary indices.
!
      integer  :: isBp2d                    ! 2D PSI-variables
      integer  :: isBr2d                    ! 2D RHO-variables
      integer  :: isBu2d                    ! 2D U-variables
      integer  :: isBv2d                    ! 2D V-variables
      integer  :: isBp3d                    ! 3D PSI-variables
      integer  :: isBr3d                    ! 3D RHO-variables
      integer  :: isBu3d                    ! 3D U-variables
      integer  :: isBv3d                    ! 3D V-variables
      integer  :: isBw3d                    ! 3D W-variables
!
!  Input lateral boundary, climatology, and forcing NetCDF files IDs
!  for each processing field. It facilitates the multi-file option:
!  each field can be in its own file.
!
      integer, allocatable :: BRYncid(:,:)
      integer, allocatable :: CLMncid(:,:)
      integer, allocatable :: FRCncid(:,:)
!
!  Flags to create output files.
!
      integer, allocatable :: idefADJ(:)    ! adjoint file
      integer, allocatable :: idefAVG(:)    ! averages file
      integer, allocatable :: idefDIA(:)    ! diagnostics file
      integer, allocatable :: idefHIS(:)    ! history file
      integer, allocatable :: idefQCK(:)    ! quicksave file
      integer, allocatable :: idefTLM(:)    ! tangent file
!
!  Output NetCDF variables IDs.
!
      integer, allocatable :: idTvar(:)     ! tracers variables
      integer, allocatable :: idTrcD(:)     ! detided tracer variables
      integer, allocatable :: idTrcH(:)     ! tracer variables hamonics
!
!  Input/Output information variables.
!
      logical, allocatable :: Linfo(:,:,:)
!
      integer, parameter :: MaxLen = 160    ! information string length
      integer, allocatable :: Iinfo(:,:,:)
!
      real(dp), allocatable :: Finfo(:,:,:)
      real(dp), allocatable :: Fpoint(:,:,:)
      real(dp), allocatable :: Fscale(:,:)
      real(dp), allocatable :: Tintrp(:,:,:)
      real(dp), allocatable :: Vtime(:,:,:)
!
      character (len=5  )    :: version = '4.2  '
      character (len=40 )    :: varnam(MV)
      character (len=44 )    :: date_str
      character (len=46 )    :: Tname(0:NV)
      character (len=MaxLen) :: history
      character (len=MaxLen) :: Vname(6,0:NV)
      character (len=256), allocatable :: Cinfo(:,:)
!
!  Analyical header file logical and names.
!
      logical :: Lanafile
!
      character (len=256), dimension(39) :: ANANAME
!
!  GIT and SVN revision and repository root URL.
!
      character (len=80 ) :: git_rev, svn_rev
      character (len=256) :: git_url, svn_url
!
      CONTAINS
!
      SUBROUTINE allocate_ncparam
!
!=======================================================================
!                                                                      !
!  This routine allocates several variables in the module that depend  !
!  on the number of nested grids.                                      !
!                                                                      !
!=======================================================================
!
      USE mod_param
!
!  Local variable declarations.
!
      integer :: ng
!
!-----------------------------------------------------------------------
!  Allocate variables.
!-----------------------------------------------------------------------
!
      IF (.not.allocated(Nxyp)) THEN
        allocate ( Nxyp(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Nxyr)) THEN
        allocate ( Nxyr(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Nxyu)) THEN
        allocate ( Nxyu(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Nxyv)) THEN
        allocate ( Nxyv(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(NwaterR)) THEN
        allocate ( NwaterR(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(NwaterU)) THEN
        allocate ( NwaterU(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(NwaterV)) THEN
        allocate ( NwaterV(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(rILB)) THEN
        allocate ( rILB(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(rIUB)) THEN
        allocate ( rIUB(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(rJLB)) THEN
        allocate ( rJLB(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(rJUB)) THEN
        allocate ( rJUB(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(rXmin)) THEN
        allocate ( rXmin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(rXmax)) THEN
        allocate ( rXmax(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(rYmin)) THEN
        allocate ( rYmin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(rYmax)) THEN
        allocate ( rYmax(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(uILB)) THEN
        allocate ( uILB(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(uIUB)) THEN
        allocate ( uIUB(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(uJLB)) THEN
        allocate ( uJLB(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(uJUB)) THEN
        allocate ( uJUB(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(uXmin)) THEN
        allocate ( uXmin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(uXmax)) THEN
        allocate ( uXmax(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(uYmin)) THEN
        allocate ( uYmin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(uYmax)) THEN
        allocate ( uYmax(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(vILB)) THEN
        allocate ( vILB(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(vIUB)) THEN
        allocate ( vIUB(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(vJLB)) THEN
        allocate ( vJLB(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(vJUB)) THEN
        allocate ( vJUB(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(vXmin)) THEN
        allocate ( vXmin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(vXmax)) THEN
        allocate ( vXmax(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(vYmin)) THEN
        allocate ( vYmin(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(vYmax)) THEN
        allocate ( vYmax(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(Aout)) THEN
        allocate ( Aout(NV,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(NV*Ngrids,r8)
      END IF
      IF (.not.allocated(Dout)) THEN
        allocate ( Dout(NV,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(NV*Ngrids,r8)
      END IF
      IF (.not.allocated(Hout)) THEN
        allocate ( Hout(NV,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(NV*Ngrids,r8)
      END IF
      IF (.not.allocated(Qout)) THEN
        allocate ( Qout(NV,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(NV*Ngrids,r8)
      END IF
      IF (.not.allocated(Sout)) THEN
        allocate ( Sout(NV,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(NV*Ngrids,r8)
      END IF
      IF (.not.allocated(idRtrc)) THEN
        allocate ( idRtrc(MT) )
        Dmem(1)=Dmem(1)+REAL(MT,r8)
      END IF
      IF (.not.allocated(idsurT)) THEN
        allocate ( idsurT(MT) )
        Dmem(1)=Dmem(1)+REAL(MT,r8)
      END IF
      IF (.not.allocated(idTads)) THEN
        allocate ( idTads(MT) )
        Dmem(1)=Dmem(1)+REAL(MT,r8)
      END IF
      IF (.not.allocated(idTbot)) THEN
        allocate ( idTbot(MT) )
        Dmem(1)=Dmem(1)+REAL(MT,r8)
      END IF
      IF (.not.allocated(idTbry)) THEN
        allocate ( idTbry(4,MT) )
        Dmem(1)=Dmem(1)+4.0_r8*REAL(MT,r8)
      END IF
      IF (.not.allocated(idTclm)) THEN
        allocate ( idTclm(MT) )
        Dmem(1)=Dmem(1)+REAL(MT,r8)
      END IF
      IF (.not.allocated(idTnud)) THEN
        allocate ( idTnud(MT) )
        Dmem(1)=Dmem(1)+REAL(MT,r8)
      END IF
      IF (.not.allocated(idTsur)) THEN
        allocate ( idTsur(MT) )
        Dmem(1)=Dmem(1)+REAL(MT,r8)
      END IF
      IF (.not.allocated(idTtlf)) THEN
        allocate ( idTtlf(MT) )
        Dmem(1)=Dmem(1)+REAL(MT,r8)
      END IF
      IF (.not.allocated(iHUTav)) THEN
        allocate ( iHUTav(MT) )
        Dmem(1)=Dmem(1)+REAL(MT,r8)
      END IF
      IF (.not.allocated(iHVTav)) THEN
        allocate ( iHVTav(MT) )
        Dmem(1)=Dmem(1)+REAL(MT,r8)
      END IF
      IF (.not.allocated(idTTav)) THEN
        allocate ( idTTav(MT) )
        Dmem(1)=Dmem(1)+REAL(MT,r8)
      END IF
      IF (.not.allocated(idUTav)) THEN
        allocate ( idUTav(MT) )
        Dmem(1)=Dmem(1)+REAL(MT,r8)
      END IF
      IF (.not.allocated(idVTav)) THEN
        allocate ( idVTav(MT) )
        Dmem(1)=Dmem(1)+REAL(MT,r8)
      END IF
      IF (.not.allocated(idDtrc)) THEN
        allocate ( idDtrc(MT,NDT) )
        Dmem(1)=Dmem(1)+REAL(MT*NDT,r8)
      END IF
      IF (.not.allocated(idDu2d)) THEN
        allocate ( idDu2d(NDM2d) )
        Dmem(1)=Dmem(1)+REAL(NDM2d,r8)
      END IF
      IF (.not.allocated(idDv2d)) THEN
        allocate ( idDv2d(NDM2d) )
        Dmem(1)=Dmem(1)+REAL(NDM2d,r8)
      END IF
      IF (.not.allocated(idDu3d)) THEN
        allocate ( idDu3d(NDM3d) )
        Dmem(1)=Dmem(1)+REAL(NDM3d,r8)
      END IF
      IF (.not.allocated(idDv3d)) THEN
        allocate ( idDv3d(NDM3d) )
        Dmem(1)=Dmem(1)+REAL(NDM3d,r8)
      END IF
      IF (.not.allocated(isTsur)) THEN
        allocate ( isTsur(MT) )
        Dmem(1)=Dmem(1)+REAL(MT,r8)
      END IF
      IF (.not.allocated(isTvar)) THEN
        allocate ( isTvar(MT) )
        isTvar=0
        Dmem(1)=Dmem(1)+REAL(MT,r8)
      END IF
      IF (.not.allocated(idBvar)) THEN
        allocate ( idBvar(nLBCvar) )
        idBvar=0
        Dmem(1)=Dmem(1)+REAL(nLBCvar,r8)
      END IF
      IF (.not.allocated(idSvar)) THEN
        allocate ( idSvar(MAXVAL(NSV)) )
        idSvar=0
        Dmem(1)=Dmem(1)+REAL(MAXVAL(NSV),r8)
      END IF
      IF (.not.allocated(idSbry)) THEN
        allocate ( idSbry(MAXVAL(NSV)) )
        idSbry=0
        Dmem(1)=Dmem(1)+REAL(MAXVAL(NSV),r8)
      END IF
      IF (.not.allocated(BRYncid)) THEN
        allocate ( BRYncid(NV,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(NV*Ngrids,r8)
      END IF
      IF (.not.allocated(CLMncid)) THEN
        allocate ( CLMncid(NV,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(NV*Ngrids,r8)
      END IF
      IF (.not.allocated(FRCncid)) THEN
        allocate ( FRCncid(NV,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(NV*Ngrids,r8)
      END IF
      IF (.not.allocated(idefADJ)) THEN
        allocate ( idefADJ(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(idefAVG)) THEN
        allocate ( idefAVG(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(idefDIA)) THEN
        allocate ( idefDIA(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(idefHIS)) THEN
        allocate ( idefHIS(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(idefQCK)) THEN
        allocate ( idefQCK(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(idefTLM)) THEN
        allocate ( idefTLM(Ngrids) )
        Dmem(1)=Dmem(1)+REAL(Ngrids,r8)
      END IF
      IF (.not.allocated(idTvar)) THEN
        allocate ( idTvar(MT) )
        Dmem(1)=Dmem(1)+REAL(MT,r8)
      END IF
      IF (.not.allocated(idTrcD)) THEN
        allocate ( idTrcD(NAT) )
        Dmem(1)=Dmem(1)+REAL(NAT,r8)
      END IF
      IF (.not.allocated(idTrcH)) THEN
        allocate ( idTrcH(NAT) )
        Dmem(1)=Dmem(1)+REAL(NAT,r8)
      END IF
      IF (.not.allocated(Linfo)) THEN
        allocate ( Linfo(6,NV,Ngrids) )
        Dmem(1)=Dmem(1)+6.0_r8*REAL(NV*Ngrids,r8)
      END IF
      IF (.not.allocated(Iinfo)) THEN
        allocate ( Iinfo(11,NV,Ngrids) )
        Dmem(1)=Dmem(1)+10.0_r8*REAL(NV*Ngrids,r8)
      END IF
      IF (.not.allocated(Finfo)) THEN
        allocate ( Finfo(10,NV,Ngrids) )
        Dmem(1)=Dmem(1)+10.0_r8*REAL(NV*Ngrids,r8)
      END IF
      IF (.not.allocated(Fpoint)) THEN
        allocate ( Fpoint(2,NV,Ngrids) )
        Dmem(1)=Dmem(1)+2.0_r8*REAL(NV*Ngrids,r8)
      END IF
      IF (.not.allocated(Fscale)) THEN
        allocate ( Fscale(NV,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(NV*Ngrids,r8)
      END IF
      IF (.not.allocated(Tintrp)) THEN
        allocate ( Tintrp(2,NV,Ngrids) )
        Dmem(1)=Dmem(1)+2.0_r8*REAL(NV*Ngrids,r8)
      END IF
      IF (.not.allocated(Vtime)) THEN
        allocate ( Vtime(MAX(2,MTC),NV,Ngrids) )
        Dmem(1)=Dmem(1)+REAL(MAX(2,MTC)*NV*Ngrids,r8)
      END IF
      IF (.not.allocated(Cinfo)) THEN
        allocate ( Cinfo(NV,Ngrids) )
        Dmem(1)=Dmem(1)+0.125_r8*256.0_r8*REAL(NV*Ngrids,r8)
      END IF
!
      RETURN
      END SUBROUTINE allocate_ncparam
!
      SUBROUTINE deallocate_ncparam
!
!=======================================================================
!                                                                      !
!  This routine deallocates variables in the module.                   !
!  on the number of nested grids.                                      !
!                                                                      !
!=======================================================================
!
!-----------------------------------------------------------------------
!  Deallocate variables in module.
!-----------------------------------------------------------------------
!
      IF (allocated(Nxyp))        deallocate ( Nxyp )
      IF (allocated(Nxyr))        deallocate ( Nxyr )
      IF (allocated(Nxyu))        deallocate ( Nxyu )
      IF (allocated(Nxyv))        deallocate ( Nxyv )
      IF (allocated(NwaterR))     deallocate ( NwaterR )
      IF (allocated(NwaterU))     deallocate ( NwaterU )
      IF (allocated(NwaterV))     deallocate ( NwaterV )
      IF (allocated(rILB))        deallocate ( rILB )
      IF (allocated(rIUB))        deallocate ( rIUB )
      IF (allocated(rJLB))        deallocate ( rJLB )
      IF (allocated(rJUB))        deallocate ( rJUB )
      IF (allocated(rXmin))       deallocate ( rXmin )
      IF (allocated(rXmax))       deallocate ( rXmax )
      IF (allocated(rYmin))       deallocate ( rYmin )
      IF (allocated(rYmax))       deallocate ( rYmax )
      IF (allocated(uILB))        deallocate ( uILB )
      IF (allocated(uIUB))        deallocate ( uIUB )
      IF (allocated(uJLB))        deallocate ( uJLB )
      IF (allocated(uJUB))        deallocate ( uJUB )
      IF (allocated(uXmin))       deallocate ( uXmin )
      IF (allocated(uXmax))       deallocate ( uXmax )
      IF (allocated(uYmin))       deallocate ( uYmin )
      IF (allocated(uYmax))       deallocate ( uYmax )
      IF (allocated(vILB))        deallocate ( vILB )
      IF (allocated(vIUB))        deallocate ( vIUB )
      IF (allocated(vJLB))        deallocate ( vJLB )
      IF (allocated(vJUB))        deallocate ( vJUB )
      IF (allocated(vXmin))       deallocate ( vXmin )
      IF (allocated(vXmax))       deallocate ( vXmax )
      IF (allocated(vYmin))       deallocate ( vYmin )
      IF (allocated(vYmax))       deallocate ( vYmax )
!
      IF (allocated(Aout))        deallocate ( Aout )
      IF (allocated(Dout))        deallocate ( Dout )
      IF (allocated(Hout))        deallocate ( Hout )
      IF (allocated(Qout))        deallocate ( Qout )
      IF (allocated(Sout))        deallocate ( Sout )
!
      IF (allocated(idRtrc))      deallocate ( idRtrc )
      IF (allocated(idsurT))      deallocate ( idsurT )
      IF (allocated(idTads))      deallocate ( idTads )
      IF (allocated(idTbot))      deallocate ( idTbot )
      IF (allocated(idTbry))      deallocate ( idTbry )
      IF (allocated(idTclm))      deallocate ( idTclm )
      IF (allocated(idTnud))      deallocate ( idTnud )
      IF (allocated(idTsur))      deallocate ( idTsur )
      IF (allocated(idTtlf))      deallocate ( idTtlf )
!
      IF (allocated(iHUTav))      deallocate ( iHUTav )
      IF (allocated(iHVTav))      deallocate ( iHVTav )
      IF (allocated(idTTav))      deallocate ( idTTav )
      IF (allocated(idUTav))      deallocate ( idUTav )
      IF (allocated(idVTav))      deallocate ( idVTav )
!
      IF (allocated(idDtrc))      deallocate ( idDtrc )
      IF (allocated(idDu2d))      deallocate ( idDu2d )
      IF (allocated(idDv2d))      deallocate ( idDv2d )
      IF (allocated(idDu3d))      deallocate ( idDu3d )
      IF (allocated(idDv3d))      deallocate ( idDv3d )
!
      IF (allocated(isTsur))      deallocate ( isTsur )
      IF (allocated(isTvar))      deallocate ( isTvar )
      IF (allocated(idBvar))      deallocate ( idBvar )
      IF (allocated(idSvar))      deallocate ( idSvar )
      IF (allocated(idSbry))      deallocate ( idSbry )
!
      IF (allocated(BRYncid))     deallocate ( BRYncid )
      IF (allocated(CLMncid))     deallocate ( CLMncid )
      IF (allocated(FRCncid))     deallocate ( FRCncid )
!
      IF (allocated(idefADJ))     deallocate ( idefADJ )
      IF (allocated(idefAVG))     deallocate ( idefAVG )
      IF (allocated(idefDIA))     deallocate ( idefDIA )
      IF (allocated(idefHIS))     deallocate ( idefHIS )
      IF (allocated(idefQCK))     deallocate ( idefQCK )
      IF (allocated(idefTLM))     deallocate ( idefTLM )
!
      IF (allocated(idTvar))      deallocate ( idTvar )
      IF (allocated(idTrcD))      deallocate ( idTrcD )
      IF (allocated(idTrcH))      deallocate ( idTrcH )
!
      IF (allocated(Linfo))       deallocate ( Linfo )
      IF (allocated(Iinfo))       deallocate ( Iinfo )
      IF (allocated(Finfo))       deallocate ( Finfo )
      IF (allocated(Fpoint))      deallocate ( Fpoint )
      IF (allocated(Fscale))      deallocate ( Fscale )
      IF (allocated(Tintrp))      deallocate ( Tintrp )
      IF (allocated(Vtime))       deallocate ( Vtime )
      IF (allocated(Cinfo))       deallocate ( Cinfo )
!
      RETURN
      END SUBROUTINE deallocate_ncparam
!
      SUBROUTINE initialize_ncparam
!
!=======================================================================
!                                                                      !
!  This routine initializes all variables in module "mod_ncparam" for  !
!  all nested grids.                                                   !
!                                                                      !
!=======================================================================
!
      USE mod_parallel
      USE mod_iounits
      USE mod_scalars
!
      USE get_metadata_mod, ONLY : io_metadata
      USE strings_mod,      ONLY : FoundError, StandardName
!
!  Local variable declarations.
!
      logical :: FirstPass, Ldone, load
!
      integer :: Lvar, Ntiles, i, ic, ic_brylast, j, ng
      integer :: gtype, tile, varid
!
      real(dp), parameter :: spv = 0.0_dp
      real(dp) :: add_offset, scale
!
      character (len=MaxLen) :: string
      character (len=MaxLen), dimension(8) :: Vinfo
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Modules/mod_ncparam_modified.F"//"initialize_ncparam"
!
!-----------------------------------------------------------------------
!  Initialize several variables.
!-----------------------------------------------------------------------
!
!  Initialize DOMAIN structure.
!
      DO ng=1,Ngrids
        DOMAIN(ng) % Eastern_Edge  = .FALSE.
        DOMAIN(ng) % Western_Edge  = .FALSE.
        DOMAIN(ng) % Northern_Edge = .FALSE.
        DOMAIN(ng) % Southern_Edge = .FALSE.
        DOMAIN(ng) % NorthEast_Corner = .FALSE.
        DOMAIN(ng) % NorthWest_Corner = .FALSE.
        DOMAIN(ng) % SouthEast_Corner = .FALSE.
        DOMAIN(ng) % SouthWest_Corner = .FALSE.
        DOMAIN(ng) % NorthEast_Test = .FALSE.
        DOMAIN(ng) % NorthWest_Test = .FALSE.
        DOMAIN(ng) % SouthEast_Test = .FALSE.
        DOMAIN(ng) % SouthWest_Test = .FALSE.
        DOMAIN(ng) % Xmin_psi = spv
        DOMAIN(ng) % Xmax_psi = spv
        DOMAIN(ng) % Ymin_psi = spv
        DOMAIN(ng) % Ymax_psi = spv
        DOMAIN(ng) % Xmin_rho = spv
        DOMAIN(ng) % Xmax_rho = spv
        DOMAIN(ng) % Ymin_rho = spv
        DOMAIN(ng) % Ymax_rho = spv
        DOMAIN(ng) % Xmin_u   = spv
        DOMAIN(ng) % Xmax_u   = spv
        DOMAIN(ng) % Ymin_u   = spv
        DOMAIN(ng) % Ymax_u   = spv
        DOMAIN(ng) % Xmin_v   = spv
        DOMAIN(ng) % Xmax_v   = spv
        DOMAIN(ng) % Ymin_v   = spv
        DOMAIN(ng) % Ymax_v   = spv
      END DO
!
!  Initialize NetCDF files creation flags.
!
      DO ng=1,Ngrids
        idefADJ(ng)=-1
        idefAVG(ng)=-1
        idefDIA(ng)=-1
        idefHIS(ng)=-1
        idefQCK(ng)=-1
        idefTLM(ng)=-1
      END DO
!
!  Analytical files switch and names.
!
      Lanafile=.TRUE.
      DO i=1,SIZE(ANANAME)
        DO j=1,LEN(ANANAME(1))
          ANANAME(i)(j:j)=' '
        END DO
      END DO
!
!  Set indices for state some state variables.
!
      ic=5
      DO i=1,MT
        ic=ic+1
        isTvar(i)=ic
      END DO
      ic_brylast=ic
      ic=ic+1
      isWvel=ic                               ! no LBC index
!
!  Set generic lateral boundary indices for LBC structure.  Use the same
!  values of the state variables at the same C-grid location.  Generic
!  indices are used for testing periodicity.  The PSI-variables and
!  W-variables are assigned the same value as the RHO-variables.
!
      isBp2d=isFsur                           ! 2D PSI-variables
      isBr2d=isFsur                           ! 2D RHO-variables
      isBu2d=isUbar                           ! 2D U-variables
      isBv2d=isVbar                           ! 2D V-variables
      isBp3d=isTvar(1)                        ! 3D PSI-variables
      isBr3d=isTvar(1)                        ! 3D RHO-variables
      isBu3d=isUvel                           ! 3D U-variables
      isBv3d=isVvel                           ! 3D V-variables
      isBw3d=isTvar(1)                        ! 3D W-variables
!
!  Initialize IO information variables.
!
      DO ng=1,Ngrids
        DO i=1,NV
          Linfo(1,i,ng)=.FALSE.
          Linfo(2,i,ng)=.FALSE.
          Linfo(3,i,ng)=.FALSE.
          Linfo(4,i,ng)=.FALSE.
          Linfo(5,i,ng)=.FALSE.
          Linfo(6,i,ng)=.FALSE.
          Aout(i,ng)=.FALSE.
          Dout(i,ng)=.FALSE.
          Hout(i,ng)=.FALSE.
          Qout(i,ng)=.FALSE.
          Sout(i,ng)=.FALSE.
          Iinfo(1,i,ng)=0
          Iinfo(2,i,ng)=-1
          Iinfo(3,i,ng)=-1
          Iinfo(4,i,ng)=0
          Iinfo(5,i,ng)=0
          Iinfo(6,i,ng)=0
          Iinfo(7,i,ng)=0
          Iinfo(8,i,ng)=2
          Iinfo(9,i,ng)=0
          Iinfo(10,i,ng)=0
          Iinfo(11,i,ng)=0
          Finfo(1,i,ng)=0.0_r8
          Finfo(2,i,ng)=0.0_r8
          Finfo(3,i,ng)=0.0_r8
          Finfo(5,i,ng)=0.0_r8
          Finfo(6,i,ng)=0.0_r8
          Finfo(7,i,ng)=0.0_r8
          Finfo(10,i,ng)=1.0_r8
          Fscale(i,ng)=1.0_r8
          Fpoint(1,i,ng)=0.0_r8
          Fpoint(2,i,ng)=0.0_r8
          Tintrp(1,i,ng)=0.0_dp
          Tintrp(2,i,ng)=0.0_dp
          Vtime(1,i,ng)=0.0_dp
          Vtime(2,i,ng)=0.0_dp
          BRYncid(i,ng)=-1
          CLMncid(i,ng)=-1
          FRCncid(i,ng)=-1
        END DO
      END DO
!
!  Set GIT and SVN Repository Root URL and revision.  Their values are
!  assigned in the 'makefile' to the CPPFLAGS macro.
!
      git_url="https://github.com/myroms/roms.git"
      git_rev="91fda4bb549c6cd2f0695939e6dccf351b9495dd"
!
      svn_url="https://www.myroms.org/svn/src/trunk"
      svn_rev="1215"
!
!-----------------------------------------------------------------------
!  Process ROMS Input/Output metadata file and define variables
!  information arrays.
!-----------------------------------------------------------------------
!
      varid=0
      Ldone=.FALSE.
      FirstPass=.TRUE.                  ! initialize metadata processing
!
      METADATA_LOOP : DO WHILE (.not.Ldone)
!
!  Load I/O metadata information.
!
        Ldone = io_metadata(FirstPass, Vinfo, scale, add_offset)
        IF (FoundError(exit_flag, NoError, 1424, MyFile)) RETURN
        IF (Ldone) EXIT
!
!  Determine staggered C-grid variable.
!
        SELECT CASE (TRIM(ADJUSTL(Vinfo(7))))
          CASE ('p2dvar')
            gtype=p2dvar
          CASE ('r2dvar')
            gtype=r2dvar
          CASE ('u2dvar')
            gtype=u2dvar
          CASE ('v2dvar')
            gtype=v2dvar
          CASE ('p3dvar')
            gtype=p3dvar
          CASE ('r3dvar')
            gtype=r3dvar
          CASE ('u3dvar')
            gtype=u3dvar
          CASE ('v3dvar')
            gtype=v3dvar
          CASE ('w3dvar')
            gtype=w3dvar
          CASE ('b3dvar')
            gtype=b3dvar
          CASE DEFAULT
            gtype=0
        END SELECT
!
!  Assign identification indices.
!
        load=.TRUE.
        varid=varid+1
        SELECT CASE (TRIM(ADJUSTL(Vinfo(8))))
          CASE ('idangR')
            idangR=varid
          CASE ('idbath')
            idbath=varid
          CASE ('idfcor')
            idfcor=varid
          CASE ('idtopo')
            idtopo=varid
          CASE ('idpmdx')
            idpmdx=varid
          CASE ('idpndy')
            idpndy=varid
          CASE ('idLonP')
            idLonP=varid
          CASE ('idLatP')
            idLatP=varid
          CASE ('idLonR')
            idLonR=varid
          CASE ('idLatR')
            idLatR=varid
          CASE ('idLonU')
            idLonU=varid
          CASE ('idLatU')
            idLatU=varid
          CASE ('idLonV')
            idLonV=varid
          CASE ('idLatV')
            idLatV=varid
          CASE ('idmskP')
            idmskP=varid
          CASE ('idmskR')
            idmskR=varid
          CASE ('idmskU')
            idmskU=varid
          CASE ('idmskV')
            idmskV=varid
          CASE ('idscoR')
            idscoR=varid
          CASE ('idscoU')
            idscoU=varid
          CASE ('idscoV')
            idscoV=varid
          CASE ('idXgrP')
            idXgrP=varid
          CASE ('idYgrP')
            idYgrP=varid
          CASE ('idXgrR')
            idXgrR=varid
          CASE ('idYgrR')
            idYgrR=varid
          CASE ('idXgrU')
            idXgrU=varid
          CASE ('idYgrU')
            idYgrU=varid
          CASE ('idXgrV')
            idXgrV=varid
          CASE ('idYgrV')
            idYgrV=varid
          CASE ('idpthR')
            idpthR=varid
          CASE ('idpthU')
            idpthU=varid
          CASE ('idpthV')
            idpthV=varid
          CASE ('idpthW')
            idpthW=varid
          CASE ('idtime')
            idtime=varid
          CASE ('idFsur')
            idFsur=varid
          CASE ('idRzet')
            idRzet=varid
          CASE ('idUbar')
            idUbar=varid
          CASE ('idu2dE')
            idu2dE=varid
          CASE ('idRu2d')
            idRu2d=varid
          CASE ('idVbar')
            idVbar=varid
          CASE ('idv2dN')
            idv2dN=varid
          CASE ('idRv2d')
            idRv2d=varid
          CASE ('idUsur')
            idUsur=varid
          CASE ('idUsuE')
            idUsuE=varid
          CASE ('idUvel')
            idUvel=varid
          CASE ('idu3dE')
            idu3dE=varid
          CASE ('idRu3d')
            idRu3d=varid
          CASE ('idVsur')
            idVsur=varid
          CASE ('idVsuN')
            idVsuN=varid
          CASE ('idVvel')
            idVvel=varid
          CASE ('idv3dN')
            idv3dN=varid
          CASE ('idRv3d')
            idRv3d=varid
          CASE ('idWvel')
            idWvel=varid
          CASE ('idOvil')
            idOvil=varid
          CASE ('idOvel')
            idOvel=varid
          CASE ('idDano')
            idDano=varid
          CASE ('idsurT(itemp)')
            idsurT(itemp)=varid
          CASE ('idsurT(isalt)')
            idsurT(isalt)=varid
          CASE ('idTvar(itemp)')
            idTvar(itemp)=varid
          CASE ('idTvar(isalt)')
            idTvar(isalt)=varid
          CASE ('idUsms')
            idUsms=varid
          CASE ('idVsms')
            idVsms=varid
          CASE ('idUbms')
            idUbms=varid
          CASE ('idVbms')
            idVbms=varid
          CASE ('idUbws')
            idUbws=varid
          CASE ('idUbcs')
            idUbcs=varid
          CASE ('idVbws')
            idVbws=varid
          CASE ('idVbcs')
            idVbcs=varid
          CASE ('idUVwc')
            idUVwc=varid
          CASE ('idTsur(itemp)')
            idTsur(itemp)=varid
          CASE ('iddQdT')
            iddQdT=varid
          CASE ('idsfwf')
            idsfwf=varid
          CASE ('idTsur(isalt)')
            idTsur(isalt)=varid
          CASE ('idTbot(itemp)')
            idTbot(itemp)=varid
          CASE ('idTbot(isalt)')
            idTbot(isalt)=varid
          CASE ('idGhat(itemp)')
            idGhat(itemp)=varid
          CASE ('idGhat(isalt)')
            idGhat(isalt)=varid
          CASE ('idMtke')
            idMtke=varid
          CASE ('idMtls')
            idMtls=varid
          CASE ('idVvis')
            idVvis=varid
          CASE ('idTdif')
            idTdif=varid
            idDiff(itemp)=idTdif
          CASE ('idSdif')
            idSdif=varid
            idDiff(isalt)=idSdif
          CASE ('idVmLS')
            idVmLS=varid
          CASE ('idVmKK')
            idVmKK=varid
          CASE ('idVmKP')
            idVmKP=varid
          CASE ('idZbry(iwest)')
            idZbry(iwest)=varid
          CASE ('idZbry(ieast)')
            idZbry(ieast)=varid
          CASE ('idZbry(isouth)')
            idZbry(isouth)=varid
          CASE ('idZbry(inorth)')
            idZbry(inorth)=varid
          CASE ('idU2bc(iwest)')
            idU2bc(iwest)=varid
          CASE ('idU2bc(ieast)')
            idU2bc(ieast)=varid
          CASE ('idU2bc(isouth)')
            idU2bc(isouth)=varid
          CASE ('idU2bc(inorth)')
            idU2bc(inorth)=varid
          CASE ('idV2bc(iwest)')
            idV2bc(iwest)=varid
          CASE ('idV2bc(ieast)')
            idV2bc(ieast)=varid
          CASE ('idV2bc(isouth)')
            idV2bc(isouth)=varid
          CASE ('idV2bc(inorth)')
            idV2bc(inorth)=varid
          CASE ('idU3bc(iwest)')
            idU3bc(iwest)=varid
          CASE ('idU3bc(ieast)')
            idU3bc(ieast)=varid
          CASE ('idU3bc(isouth)')
            idU3bc(isouth)=varid
          CASE ('idU3bc(inorth)')
            idU3bc(inorth)=varid
          CASE ('idV3bc(iwest)')
            idV3bc(iwest)=varid
          CASE ('idV3bc(ieast)')
            idV3bc(ieast)=varid
          CASE ('idV3bc(isouth)')
            idV3bc(isouth)=varid
          CASE ('idV3bc(inorth)')
            idV3bc(inorth)=varid
          CASE ('idTbry(iwest,itemp)')
            idTbry(iwest,itemp)=varid
          CASE ('idTbry(ieast,itemp)')
            idTbry(ieast,itemp)=varid
          CASE ('idTbry(isouth,itemp)')
            idTbry(isouth,itemp)=varid
          CASE ('idTbry(inorth,itemp)')
            idTbry(inorth,itemp)=varid
          CASE ('idTbry(iwest,isalt)')
            idTbry(iwest,isalt)=varid
          CASE ('idTbry(ieast,isalt)')
            idTbry(ieast,isalt)=varid
          CASE ('idTbry(isouth,isalt)')
            idTbry(isouth,isalt)=varid
          CASE ('idTbry(inorth,isalt)')
            idTbry(inorth,isalt)=varid
          CASE ('idPwet')
            idPwet=varid
          CASE ('idRwet')
            idRwet=varid
          CASE ('idUwet')
            idUwet=varid
          CASE ('idVwet')
            idVwet=varid
          CASE ('idPair')
            idPair=varid
          CASE ('idTair')
            idTair=varid
          CASE ('idQair')
            idQair=varid
          CASE ('idCfra')
            idCfra=varid
          CASE ('idSrad')
            idSrad=varid
          CASE ('idLdwn')
            idLdwn=varid
          CASE ('idLrad')
            idLrad=varid
          CASE ('idLhea')
            idLhea=varid
          CASE ('idShea')
            idShea=varid
          CASE ('idrain')
            idrain=varid
          CASE ('idEmPf')
            idEmPf=varid
          CASE ('idevap')
            idevap=varid
          CASE ('idUair')
            idUair=varid
          CASE ('idVair')
            idVair=varid
          CASE ('idUaiE')
            idUaiE=varid
          CASE ('idVaiN')
            idVaiN=varid
          CASE ('idWvds')
            idWvds=varid
          CASE ('idWvqp')
            idWvqp=varid
          CASE ('idUwav')
            idUwav=varid
          CASE ('idVwav')
            idVwav=varid
          CASE ('idWamp')
            idWamp=varid
          CASE ('idWam2')
            idWam2=varid
          CASE ('idWbrk')
            idWbrk=varid
          CASE ('idWdib')
            idWdib=varid
          CASE ('idWdif')
            idWdif=varid
          CASE ('idWdis')
            idWdis=varid
          CASE ('idWdir')
            idWdir=varid
          CASE ('idWdip')
            idWdip=varid
          CASE ('idWdiw')
            idWdiw=varid
          CASE ('idWztw')
            idWztw=varid
          CASE ('idWqsp')
            idWqsp=varid
          CASE ('idWbeh')
            idWbeh=varid
          CASE ('idWlen')
            idWlen=varid
          CASE ('idWlep')
            idWlep=varid
          CASE ('idWptp')
            idWptp=varid
          CASE ('idWpbt')
            idWpbt=varid
          CASE ('idWorb')
            idWorb=varid
          CASE ('idWrol')
            idWrol=varid
          CASE ('idW2xx')
            idW2xx=varid
          CASE ('idW2xy')
            idW2xy=varid
          CASE ('idW2yy')
            idW2yy=varid
          CASE ('idW3xx')
            idW3xx=varid
          CASE ('idW3xy')
            idW3xy=varid
          CASE ('idW3yy')
            idW3yy=varid
          CASE ('idW3zx')
            idW3zx=varid
          CASE ('idW3zy')
            idW3zy=varid
          CASE ('idU2rs')
            idU2rs=varid
          CASE ('idV2rs')
            idV2rs=varid
          CASE ('idU2Sd')
            idU2Sd=varid
          CASE ('idV2Sd')
            idV2Sd=varid
          CASE ('idU3rs')
            idU3rs=varid
          CASE ('idV3rs')
            idV3rs=varid
          CASE ('idU3Sd')
            idU3Sd=varid
          CASE ('idV3Sd')
            idV3Sd=varid
          CASE ('idW3Sd')
            idW3Sd=varid
          CASE ('idW3St')
            idW3St=varid
          CASE ('idTref')
            idTref=varid
          CASE ('idTper')
            idTper=varid
          CASE ('idTzam')
            idTzam=varid
          CASE ('idTzph')
            idTzph=varid
          CASE ('idTvph')
            idTvph=varid
          CASE ('idTvan')
            idTvan=varid
          CASE ('idTvma')
            idTvma=varid
          CASE ('idTvmi')
            idTvmi=varid
          CASE ('idRxpo')
            idRxpo=varid
          CASE ('idRepo')
            idRepo=varid
          CASE ('idRdir')
            idRdir=varid
          CASE ('idRvsh')
            idRvsh=varid
          CASE ('idRtra')
            idRtra=varid
          CASE ('idRflg')
            idRflg=varid
          CASE ('idRtrc(itemp)')
            idRtrc(itemp)=varid
          CASE ('idRtrc(isalt)')
            idRtrc(isalt)=varid
          CASE ('idHsbl')
            idHsbl=varid
          CASE ('idHbbl')
            idHbbl=varid
          CASE ('idUbot')
            idUbot=varid
          CASE ('idVbot')
            idVbot=varid
          CASE ('idUbur')
            idUbur=varid
          CASE ('idVbvr')
            idVbvr=varid
          CASE ('idUbrs')
            idUbrs=varid
          CASE ('idVbrs')
            idVbrs=varid
          CASE ('idSSHc')
            idSSHc=varid
          CASE ('idUbcl')
            idUbcl=varid
          CASE ('idVbcl')
            idVbcl=varid
          CASE ('idUclm')
            idUclm=varid
          CASE ('idVclm')
            idVclm=varid
          CASE ('idSSSc')
            idSSSc=varid
          CASE ('idSSTc')
            idSSTc=varid
          CASE ('idM2nc')
            idM2nc=varid
          CASE ('idM3nc')
            idM3nc=varid
          CASE ('idgTnc')
            idgTnc=varid
          CASE ('idU2av')
            idU2av=varid
          CASE ('idV2av')
            idV2av=varid
          CASE ('idZZav')
            idZZav=varid
          CASE ('idTTav(itrc)')
            load=.TRUE.
          CASE ('iHUTav(itrc)')
            load=.TRUE.
          CASE ('iHVTav(itrc)')
            load=.TRUE.
          CASE ('idUTav(itrc)')
            load=.TRUE.
          CASE ('idVTav(itrc)')
            load=.TRUE.
          CASE ('idHUav')
            idHUav=varid
          CASE ('idHVav')
            idHVav=varid
          CASE ('idUUav')
            idUUav=varid
          CASE ('idUVav')
            idUVav=varid
          CASE ('idVVav')
            idVVav=varid
          CASE ('id2dPV')
            id2dPV=varid
          CASE ('id2dRV')
            id2dRV=varid
          CASE ('id3dPV')
            id3dPV=varid
          CASE ('id3dRV')
            id3dRV=varid
          CASE ('idDu2d(M2pgrd)')
            idDu2d(M2pgrd)=varid
          CASE ('idDv2d(M2pgrd)')
            idDv2d(M2pgrd)=varid
          CASE ('idDu2d(M2sstr)')
            idDu2d(M2sstr)=varid
          CASE ('idDu2d(M2bstr)')
            idDu2d(M2bstr)=varid
          CASE ('idDv2d(M2sstr)')
            idDv2d(M2sstr)=varid
          CASE ('idDv2d(M2bstr)')
            idDv2d(M2bstr)=varid
          CASE ('idDu2d(M2rate)')
            idDu2d(M2rate)=varid
          CASE ('idDv2d(M2rate)')
            idDv2d(M2rate)=varid
          CASE ('idDu2d(M2xadv)')
            idDu2d(M2xadv)=varid
          CASE ('idDu2d(M2yadv)')
            idDu2d(M2yadv)=varid
          CASE ('idDu2d(M2hadv)')
            idDu2d(M2hadv)=varid
          CASE ('idDv2d(M2xadv)')
            idDv2d(M2xadv)=varid
          CASE ('idDv2d(M2yadv)')
            idDv2d(M2yadv)=varid
          CASE ('idDv2d(M2hadv)')
            idDv2d(M2hadv)=varid
          CASE ('idDu2d(M2fcor)')
            idDu2d(M2fcor)=varid
          CASE ('idDv2d(M2fcor)')
            idDv2d(M2fcor)=varid
          CASE ('idDu2d(M2hvis)')
            idDu2d(M2hvis)=varid
          CASE ('idDu2d(M2xvis)')
            idDu2d(M2xvis)=varid
          CASE ('idDu2d(M2yvis)')
            idDu2d(M2yvis)=varid
          CASE ('idDv2d(M2hvis)')
            idDv2d(M2hvis)=varid
          CASE ('idDv2d(M2xvis)')
            idDv2d(M2xvis)=varid
          CASE ('idDv2d(M2yvis)')
            idDv2d(M2yvis)=varid
          CASE ('idDu3d(M3pgrd)')
            idDu3d(M3pgrd)=varid
          CASE ('idDv3d(M3pgrd)')
            idDv3d(M3pgrd)=varid
          CASE ('idDu3d(M3vvis)')
            idDu3d(M3vvis)=varid
          CASE ('idDv3d(M3vvis)')
            idDv3d(M3vvis)=varid
          CASE ('idDu3d(M3rate)')
            idDu3d(M3rate)=varid
          CASE ('idDv3d(M3rate)')
            idDv3d(M3rate)=varid
          CASE ('idDu3d(M3xadv)')
            idDu3d(M3xadv)=varid
          CASE ('idDu3d(M3yadv)')
            idDu3d(M3yadv)=varid
          CASE ('idDu3d(M3hadv)')
            idDu3d(M3hadv)=varid
          CASE ('idDv3d(M3xadv)')
            idDv3d(M3xadv)=varid
          CASE ('idDv3d(M3yadv)')
            idDv3d(M3yadv)=varid
          CASE ('idDv3d(M3hadv)')
            idDv3d(M3hadv)=varid
          CASE ('idDu3d(M3vadv)')
            idDu3d(M3vadv)=varid
          CASE ('idDv3d(M3vadv)')
            idDv3d(M3vadv)=varid
          CASE ('idDu3d(M3fcor)')
            idDu3d(M3fcor)=varid
          CASE ('idDv3d(M3fcor)')
            idDv3d(M3fcor)=varid
          CASE ('idDu3d(M3hvis)')
            idDu3d(M3hvis)=varid
          CASE ('idDu3d(M3xvis)')
            idDu3d(M3xvis)=varid
          CASE ('idDu3d(M3yvis)')
            idDu3d(M3yvis)=varid
          CASE ('idDv3d(M3hvis)')
            idDv3d(M3hvis)=varid
          CASE ('idDv3d(M3xvis)')
            idDv3d(M3xvis)=varid
          CASE ('idDv3d(M3yvis)')
            idDv3d(M3yvis)=varid
          CASE ('idDtrc(iTrate)')
            load=.TRUE.
          CASE ('idDtrc(iThadv)')
            load=.TRUE.
          CASE ('idDtrc(iTxadv)')
            load=.TRUE.
          CASE ('idDtrc(iTyadv)')
            load=.TRUE.
          CASE ('idDtrc(iTvadv)')
            load=.TRUE.
          CASE ('idDtrc(iThdif)')
            load=.TRUE.
          CASE ('idDtrc(iTxdif)')
            load=.TRUE.
          CASE ('idDtrc(iTydif)')
            load=.TRUE.
          CASE ('idDtrc(iTsdif)')
            load=.TRUE.
          CASE ('idDtrc(iTvdif)')
            load=.TRUE.
          CASE DEFAULT
            load=.FALSE.
        END SELECT
!
!  Load variable data into information arrays.
!
        IF (load) THEN
          load=.FALSE.
          IF (varid.gt.MV) THEN
            WRITE (stdout,10) MV, varid
            STOP
          END IF
          DO i=1,6
            Vname(i,varid)=TRIM(ADJUSTL(Vinfo(i)))
          END DO
          DO ng=1,Ngrids
            Iinfo(1,varid,ng)=gtype
            Fscale(varid,ng)=scale
          END DO
!
!  Adjust information for tracer diagnostic variables.  This needs to be
!  done last because it needs all the tracers variable names.
!
          SELECT CASE (Vinfo(1))
            CASE ('_rate')
              varid=varid-1
              DO i=1,MT
                varid=varid+1
                idDtrc(i,iTrate)=varid
                DO ng=1,Ngrids
                  Fscale(varid,ng)=scale
                  Iinfo(1,varid,ng)=gtype
                END DO
                WRITE (Vname(1,varid),'(a,a)')                          &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(1)))
                WRITE (Vname(2,varid),'(a,", ",a)')                     &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(2)))
                WRITE (Vname(3,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(3,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(3)))
                WRITE (Vname(4,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(vinfo(4)))
                WRITE (Vname(5,varid),'(a)')                            &
     &                TRIM(ADJUSTL(Vinfo(5)))
                CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),  &
     &                             prefix='sea_water_',                 &
     &                             suffix=Vinfo(6))
              END DO
            CASE ('_hadv')
              varid=varid-1
              DO i=1,MT
                varid=varid+1
                idDtrc(i,iThadv)=varid
                DO ng=1,Ngrids
                  Fscale(varid,ng)=scale
                  Iinfo(1,varid,ng)=gtype
                END DO
                WRITE (Vname(1,varid),'(a,a)')                          &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(1)))
                WRITE (Vname(2,varid),'(a,", ",a)')                     &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(2)))
                WRITE (Vname(3,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(3,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(3)))
                WRITE (Vname(4,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(vinfo(4)))
                WRITE (Vname(5,varid),'(a)')                            &
     &                TRIM(ADJUSTL(Vinfo(5)))
                CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),  &
     &                             prefix='sea_water_',                 &
     &                             suffix=Vinfo(6))
             END DO
            CASE ('_xadv')
              varid=varid-1
              DO i=1,MT
                varid=varid+1
                idDtrc(i,iTxadv)=varid
                DO ng=1,Ngrids
                  Fscale(varid,ng)=scale
                  Iinfo(1,varid,ng)=gtype
                END DO
                WRITE (Vname(1,varid),'(a,a)')                          &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(1)))
                WRITE (Vname(2,varid),'(a,", ",a)')                     &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(2)))
                WRITE (Vname(3,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(3,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(3)))
                WRITE (Vname(4,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(vinfo(4)))
                WRITE (Vname(5,varid),'(a)')                            &
     &                TRIM(ADJUSTL(Vinfo(5)))
                CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),  &
     &                             prefix='sea_water_',                 &
     &                             suffix=Vinfo(6))
              END DO
            CASE ('_yadv')
              varid=varid-1
              DO i=1,MT
                varid=varid+1
                idDtrc(i,iTyadv)=varid
                DO ng=1,Ngrids
                  Fscale(varid,ng)=scale
                  Iinfo(1,varid,ng)=gtype
                END DO
                WRITE (Vname(1,varid),'(a,a)')                          &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(1)))
                WRITE (Vname(2,varid),'(a,", ",a)')                     &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(2)))
                WRITE (Vname(3,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(3,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(3)))
                WRITE (Vname(4,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(vinfo(4)))
                WRITE (Vname(5,varid),'(a)')                            &
     &                TRIM(ADJUSTL(Vinfo(5)))
                CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),  &
     &                             prefix='sea_water_',                 &
     &                             suffix=Vinfo(6))
              END DO
            CASE ('_vadv')
              varid=varid-1
              DO i=1,MT
                varid=varid+1
                idDtrc(i,iTvadv)=varid
                DO ng=1,Ngrids
                  Fscale(varid,ng)=scale
                  Iinfo(1,varid,ng)=gtype
                END DO
                WRITE (Vname(1,varid),'(a,a)')                          &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(1)))
                WRITE (Vname(2,varid),'(a,", ",a)')                     &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(2)))
                WRITE (Vname(3,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(3,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(3)))
                WRITE (Vname(4,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(vinfo(4)))
                WRITE (Vname(5,varid),'(a)')                            &
     &                TRIM(ADJUSTL(Vinfo(5)))
                CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),  &
     &                             prefix='sea_water_',                 &
     &                             suffix=Vinfo(6))
              END DO
            CASE ('_hdiff')
              varid=varid-1
              DO i=1,MT
                varid=varid+1
                idDtrc(i,iThdif)=varid
                DO ng=1,Ngrids
                  Fscale(varid,ng)=scale
                  Iinfo(1,varid,ng)=gtype
                END DO
                WRITE (Vname(1,varid),'(a,a)')                          &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(1)))
                WRITE (Vname(2,varid),'(a,", ",a)')                     &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(2)))
                WRITE (Vname(3,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(3,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(3)))
                WRITE (Vname(4,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(vinfo(4)))
                WRITE (Vname(5,varid),'(a)')                            &
     &                TRIM(ADJUSTL(Vinfo(5)))
                CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),  &
     &                             prefix='sea_water_',                 &
     &                             suffix=Vinfo(6))
              END DO
            CASE ('_xdiff')
              varid=varid-1
              DO i=1,MT
                varid=varid+1
                idDtrc(i,iTxdif)=varid
                DO ng=1,Ngrids
                  Fscale(varid,ng)=scale
                  Iinfo(1,varid,ng)=gtype
                END DO
                WRITE (Vname(1,varid),'(a,a)')                          &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(1)))
                WRITE (Vname(2,varid),'(a,", ",a)')                     &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(2)))
                WRITE (Vname(3,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(3,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(3)))
                WRITE (Vname(4,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(vinfo(4)))
                WRITE (Vname(5,varid),'(a)')                            &
     &                TRIM(ADJUSTL(Vinfo(5)))
                CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),  &
     &                             prefix='sea_water_',                 &
     &                             suffix=Vinfo(6))
              END DO
            CASE ('_ydiff')
              varid=varid-1
              DO i=1,MT
                varid=varid+1
                idDtrc(i,iTydif)=varid
                DO ng=1,Ngrids
                  Fscale(varid,ng)=scale
                  Iinfo(1,varid,ng)=gtype
                END DO
                WRITE (Vname(1,varid),'(a,a)')                          &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(1)))
                WRITE (Vname(2,varid),'(a,", ",a)')                     &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(2)))
                WRITE (Vname(3,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(3,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(3)))
                WRITE (Vname(4,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(vinfo(4)))
                WRITE (Vname(5,varid),'(a)')                            &
     &                TRIM(ADJUSTL(Vinfo(5)))
                CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),  &
     &                             prefix='sea_water_',                 &
     &                             suffix=Vinfo(6))
             END DO
            CASE ('_sdiff')
              varid=varid-1
              DO i=1,MT
                varid=varid+1
                idDtrc(i,iTsdif)=varid
                DO ng=1,Ngrids
                  Fscale(varid,ng)=scale
                  Iinfo(1,varid,ng)=gtype
                END DO
                WRITE (Vname(1,varid),'(a,a)')                          &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(1)))
                WRITE (Vname(2,varid),'(a,", ",a)')                     &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(2)))
                WRITE (Vname(3,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(3,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(3)))
                WRITE (Vname(4,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(vinfo(4)))
                WRITE (Vname(5,varid),'(a)')                            &
     &                TRIM(ADJUSTL(Vinfo(5)))
                CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),  &
     &                             prefix='sea_water_',                 &
     &                             suffix=Vinfo(6))
              END DO
            CASE ('_vdiff')
              varid=varid-1
              DO i=1,MT
                varid=varid+1
                idDtrc(i,iTvdif)=varid
                DO ng=1,Ngrids
                  Fscale(varid,ng)=scale
                  Iinfo(1,varid,ng)=gtype
                END DO
                WRITE (Vname(1,varid),'(a,a)')                          &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(1)))
                WRITE (Vname(2,varid),'(a,", ",a)')                     &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(2)))
                WRITE (Vname(3,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(3,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(3)))
                WRITE (Vname(4,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))),                &
     &                TRIM(ADJUSTL(vinfo(4)))
                WRITE (Vname(5,varid),'(a)')                            &
     &                TRIM(ADJUSTL(Vinfo(5)))
                CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),  &
     &                             prefix='sea_water_',                 &
     &                             suffix=Vinfo(6))
              END DO
          END SELECT
!
!  Determine metadata for quadratic tracer averages.
!
          SELECT CASE (Vinfo(1))
            CASE ('tracer2')
              varid=varid-1
              DO i=1,MT
                varid=varid+1
                idTTav(i)=varid
                DO ng=1,Ngrids
                  Fscale(varid,ng)=scale
                  Iinfo(1,varid,ng)=gtype
                END DO
                WRITE (Vname(1,varid),'(a,a)')                          &
     &                TRIM(ADJUSTL(Vname(1,idTvar(i)))), '_2'
                WRITE (Vname(2,varid),'(a,1x,a)')                       &
     &                'squared',                                        &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i))))
                IF (TRIM(ADJUSTL(Vname(3,idTvar(i)))).eq.               &
     &              'nondimensional') THEN
                  WRITE (Vname(3,varid),'(a)')                          &
     &                  TRIM(ADJUSTL(Vname(3,idTvar(i))))
                ELSE
                  WRITE (Vname(3,varid),'(a,a)')                        &
     &                  TRIM(ADJUSTL(Vname(3,idTvar(i)))), '2'
                END IF
                WRITE (Vname(4,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(4)))
                WRITE (Vname(5,varid),'(a)')                            &
     &                TRIM(ADJUSTL(Vinfo(5)))
                CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),  &
     &                             prefix=Vinfo(6),                     &
     &                             suffix='_in_sea_water')
              END DO
            CASE ('Huontracer')
              varid=varid-1
              DO i=1,MT
                varid=varid+1
                iHUTav(i)=varid
                DO ng=1,Ngrids
                  Fscale(varid,ng)=scale
                  Iinfo(1,varid,ng)=gtype
                END DO
                WRITE (Vname(1,varid),'(a,a)')                          &
     &                'Huon_', TRIM(ADJUSTL(Vname(1,idTvar(i))))
                WRITE (Vname(2,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i)))),                &
     &                'u-volume flux'
                IF (TRIM(ADJUSTL(Vname(3,idTvar(i)))).eq.               &
     &              'nondimensional') THEN
                  WRITE (Vname(3,varid),'(a)')                          &
     &                  'meter3 second-1'
                ELSE
                  WRITE (Vname(3,varid),'(a,1x,a)')                     &
     &                  'meter3 second-1',                              &
     &                  TRIM(ADJUSTL(Vname(3,idTvar(i))))
                END IF
                WRITE (Vname(4,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(4)))
                WRITE (Vname(5,varid),'(a)')                            &
     &                TRIM(ADJUSTL(Vinfo(5)))
                CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),  &
     &                             prefix=Vinfo(6))
              END DO
            CASE ('utracer')
              varid=varid-1
              DO i=1,MT
                varid=varid+1
                idUTav(i)=varid
                DO ng=1,Ngrids
                  Fscale(varid,ng)=scale
                  Iinfo(1,varid,ng)=gtype
                END DO
                WRITE (Vname(1,varid),'(a,a)')                          &
     &                'u_', TRIM(ADJUSTL(Vname(1,idTvar(i))))
                WRITE (Vname(2,varid),'(a,1x,a)')                       &
     &                'u-momentum times',                               &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i))))
                IF (TRIM(ADJUSTL(Vname(3,idTvar(i)))).eq.               &
     &              'nondimensional') THEN
                  WRITE (Vname(3,varid),'(a)')                          &
     &                  'meter second-1'
                ELSE
                  WRITE (Vname(3,varid),'(a,1x,a)')                     &
     &                  'meter second-1',                               &
     &                  TRIM(ADJUSTL(Vname(3,idTvar(i))))
                END IF
                WRITE (Vname(4,varid),'(a,1x,a)')                       &
     &                 TRIM(ADJUSTL(Vinfo(4))),                         &
     &                 TRIM(ADJUSTL(Vname(2,idTvar(i))))
                Vname(5,varid)='ocean_time'
                CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),  &
     &                             prefix=Vinfo(6),                     &
     &                             suffix='_in_sea_water')
              END DO
            CASE ('Hvomtracer')
              varid=varid-1
              DO i=1,MT
                varid=varid+1
                iHVTav(i)=varid
                DO ng=1,Ngrids
                  Fscale(varid,ng)=scale
                  Iinfo(1,varid,ng)=gtype
                END DO
                WRITE (Vname(1,varid),'(a,a)')                          &
     &                'Hvom_', TRIM(ADJUSTL(Vname(1,idTvar(i))))
                WRITE (Vname(2,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i)))),                &
     &                'v-volume flux'
                IF (TRIM(ADJUSTL(Vname(3,idTvar(i)))).eq.               &
     &              'nondimensional') THEN
                  WRITE (Vname(3,varid),'(a)')                          &
     &                  'meter3 second-1'
                ELSE
                  WRITE (Vname(3,varid),'(a,1x,a)')                     &
     &                  'meter3 second-1',                              &
     &                  TRIM(ADJUSTL(Vname(3,idTvar(i))))
                END IF
                WRITE (Vname(4,varid),'(a,1x,a)')                       &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i)))),                &
     &                TRIM(ADJUSTL(Vinfo(4)))
                WRITE (Vname(5,varid),'(a)')                            &
     &                TRIM(ADJUSTL(Vinfo(5)))
                CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),  &
     &                             prefix=Vinfo(6))
              END DO
            CASE ('vtracer')
              varid=varid-1
              DO i=1,MT
                varid=varid+1
                idVTav(i)=varid
                DO ng=1,Ngrids
                  Fscale(varid,ng)=scale
                  Iinfo(1,varid,ng)=gtype
                END DO
                WRITE (Vname(1,varid),'(a,a)')                          &
     &                'v_', TRIM(ADJUSTL(Vname(1,idTvar(i))))
                WRITE (Vname(2,varid),'(a,1x,a)')                       &
     &                'v-momentum times',                               &
     &                TRIM(ADJUSTL(Vname(2,idTvar(i))))
                IF (TRIM(ADJUSTL(Vname(3,idTvar(i)))).eq.               &
     &              'nondimensional') THEN
                  WRITE (Vname(3,varid),'(a)')                          &
     &                  'meter second-1'
                ELSE
                  WRITE (Vname(3,varid),'(a,1x,a)')                     &
     &                  'meter second-1',                               &
     &                  TRIM(ADJUSTL(Vname(3,idTvar(i))))
                END IF
                WRITE (Vname(4,varid),'(a,1x,a)')                       &
     &                 TRIM(ADJUSTL(Vinfo(4))),                         &
     &                 TRIM(ADJUSTL(Vname(2,idTvar(i))))
                Vname(5,varid)='ocean_time'
                CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),  &
     &                             prefix=Vinfo(6),                     &
     &                             suffix='_in_sea_water')
              END DO
          END SELECT
        ELSE
          varid=varid-1
        END IF
      END DO METADATA_LOOP
!
!-----------------------------------------------------------------------
!  Set passive tracers surface flux metadata. The variable name is the
!  same as the basic tracer but with the _sflux suffix.
!-----------------------------------------------------------------------
!
      DO i=NAT+1,MT
        varid=varid+1
        IF (varid.gt.MV) THEN
          WRITE (stdout,10) MV, varid
          STOP
        END IF
        idTsur(i)=varid
        DO ng=1,Ngrids
          Fscale(varid,ng)=1.0_r8
          Iinfo(1,varid,ng)=r2dvar
        END DO
        WRITE (Vname(1,varid),'(a,a)')                                  &
     &        TRIM(ADJUSTL(Vname(1,idTvar(i)))), '_sflux'
        WRITE (Vname(2,varid),'(a,a)')                                  &
     &        TRIM(ADJUSTL(Vname(2,idTvar(i)))), ', surface flux'
        WRITE (Vname(3,varid),'(a,1x,a)')                               &
     &        TRIM(ADJUSTL(Vname(3,idTvar(i)))), 'meter second-1'
        WRITE (Vname(4,varid),'(3a)') 'surface ',                       &
     &        TRIM(ADJUSTL(Vname(2,idTvar(i)))), ' flux'
        WRITE (Vname(5,varid),'(a)')                                    &
     &        TRIM(ADJUSTL(Vname(1,idTvar(i))))
        CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),          &
     &                     prefix='sea_surface_',                       &
     &                     suffix='_flux')
      END DO
!
!-----------------------------------------------------------------------
!  Set passive tracers bottom flux metadata. The variable name is the
!  same as the basic tracer but with the _bflux suffix.  Added from Guangyu
!-----------------------------------------------------------------------
!
      DO i=NAT+1,MT
        varid=varid+1
        IF (varid.gt.MV) THEN
          WRITE (stdout,10) MV, varid
          STOP
        END IF
        idTbot(i)=varid
        DO ng=1,Ngrids
          Fscale(varid,ng)=1.0_r8
          Iinfo(1,varid,ng)=r2dvar
        END DO
        WRITE (Vname(1,varid),'(a,a)')                                  &
     &        TRIM(ADJUSTL(Vname(1,idTvar(i)))), '_bflux'
        WRITE (Vname(2,varid),'(a,a)')                                  &
     &        TRIM(ADJUSTL(Vname(2,idTvar(i)))), ', bottom flux'
        WRITE (Vname(3,varid),'(a,1x,a)')                               &
     &        TRIM(ADJUSTL(Vname(3,idTvar(i)))), 'kg m-3 meter second-1'
        WRITE (Vname(4,varid),'(3a)') 'surface ',                       &
     &        TRIM(ADJUSTL(Vname(2,idTvar(i)))), ' flux'
        WRITE (Vname(5,varid),'(a)')                                    &
     &        TRIM(ADJUSTL(Vname(1,idTvar(i))))
        CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),          &
     &                     prefix='seafloor_',                          &
     &                     suffix='_flux')
      END DO
!
!-----------------------------------------------------------------------
!  Set passive model surface tracers metadata. The variable name is the
!  same as the basic tracer but with the _sur suffix.
!-----------------------------------------------------------------------
!
      DO i=NAT+1,MT
        varid=varid+1
        IF (varid.gt.MV) THEN
          WRITE (stdout,10) MV, varid
          STOP
        END IF
        idsurT(i)=varid
        DO ng=1,Ngrids
          Fscale(varid,ng)=1.0_r8
          Iinfo(1,varid,ng)=r2dvar
        END DO
        WRITE (Vname(1,varid),'(a,a)')                                  &
     &        TRIM(ADJUSTL(Vname(1,idTvar(i)))), '_sur'
        WRITE (Vname(2,varid),'(2a)') 'surface ',                       &
     &        TRIM(ADJUSTL(Vname(2,idTvar(i))))
        WRITE (Vname(3,varid),'(a)')                                    &
     &        TRIM(ADJUSTL(Vname(3,idTvar(i))))
        WRITE (Vname(4,varid),'(a)')                                    &
     &        TRIM(Vname(2,varid))
        WRITE (Vname(5,varid),'(a)')                                    &
     &        TRIM(ADJUSTL(Vname(5,idTvar(i))))
        CALL StandardName (Vname(6,varid), Vname(2,idTvar(i)),          &
     &                     prefix='sea_surface_')
      END DO
!
!-----------------------------------------------------------------------
!  Set model state variables metadata indices.
!-----------------------------------------------------------------------
!
      idSvar(isFsur)=idFsur
      idSvar(isUbar)=idUbar
      idSvar(isVbar)=idVbar
      idSvar(isUvel)=idUvel
      idSvar(isVvel)=idVvel
      DO i=1,MT
        idSvar(isTvar(i))=idTvar(i)
      END DO
      idSvar(isWvel)=idWvel
!
!-----------------------------------------------------------------------
!  Set model lateral boundary variables index.
!-----------------------------------------------------------------------
!
      idBvar(isFsur)=idFsur
      idBvar(isUbar)=idUbar
      idBvar(isVbar)=idVbar
      idBvar(isUvel)=idUvel
      idBvar(isVvel)=idVvel
      DO i=1,MT
        idBvar(isTvar(i))=idTvar(i)
      END DO
!
!  Save last variable ID counter used.
!
      last_varid=varid
      Dmem(1)=Dmem(1)+REAL(varid,r8)
!
  10  FORMAT (/,' MOD_NCPARAM - too small dimension ',                  &
     &        'parameter, MV = ',2i5,/,15x,                             &
     &        'change file  mod_ncparam.F  and recompile.')
!
      RETURN
      END SUBROUTINE initialize_ncparam
!
      END MODULE mod_ncparam
