      PROGRAM MyROMS
!
!git $Id$
!svn $Id: roms.h 1210 2024-01-03 22:03:03Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  Regional Ocean Model System (ROMS)                                  !
!  Terrain-following Ocean Model System (TOMS)                         !
!                                                                      !
!  Master program to execute  ROMS/TOMS  drivers in ocean mode only    !
!  without coupling (sequential or concurrent) to  any  atmospheric    !
!  model.                                                              !
!                                                                      !
!  This ocean model solves the free surface, hydrostatic, primitive    !
!  equations  over  variable  topography  using  stretched terrain-    !
!  following coordinates in the vertical and orthogonal curvilinear    !
!  coordinates in the horizontal.                                      !
!                                                                      !
!  Nonlinear Model Developers:                                         !
!                                                                      !
!  Dr. Hernan G. Arango                                                !
!    Institute of Marine and Coastal Sciences                          !
!    Rutgers University, New Brunswick, NJ, USA                        !
!    (arango@marine.rutgers.edu)                                       !
!                                                                      !
!  Dr. Alexander F. Shchepetkin                                        !
!    Institute of Geophysics and Planetary Physics                     !
!    UCLA, Los Angeles, CA, USA                                        !
!    (alex@atmos.ucla.edu)                                             !
!                                                                      !
!  Dr. John C. Warner                                                  !
!    U.S. Geological Survey                                            !
!    Woods Hole, MA, USA                                               !
!    (jcwarner@usgs.gov)                                               !
!                                                                      !
!  Tangent linear and Adjoint Models and Algorithms Developers:        !
!                                                                      !
!    Dr. Hernan G. Arango    (arango@marine.rutgers.edu)               !
!    Dr. Bruce Cornuelle     (bcornuelle@ucsd.edu)                     !
!    Dr. Emanuele Di Lorenzo (edl@eas.gatech.edu)                      !
!    Dr. Arthur J. Miller    (ajmiller@ucsd.edu)                       !
!    Dr. Andrew M. Moore     (ammoore@ucsc.edu)                        !
!    Dr. Brian Powell        (powellb@uscs.edu)                        !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_iounits
      USE mod_scalars
!
      USE mod_arrays,      ONLY : ROMS_deallocate_arrays
!
      USE roms_kernel_mod, ONLY : ROMS_initialize
      USE roms_kernel_mod, ONLY : ROMS_run
      USE roms_kernel_mod, ONLY : ROMS_finalize
!
      implicit none
!
!  Local variable declarations.
!
      logical, save :: first
      integer :: ng, MyError
!
!-----------------------------------------------------------------------
!  Initialize ocean internal and external parameters and state
!  variables for all nested grids, if applicable.
!-----------------------------------------------------------------------
!
      IF (exit_flag.eq.NoError) THEN
        first=.TRUE.
        CALL ROMS_initialize (first)
      END IF
!
!-----------------------------------------------------------------------
!  Time-step ocean model over all nested grids, if applicable, by the
!  specified time interval in seconds.
!-----------------------------------------------------------------------
!
      IF (exit_flag.eq.NoError) THEN
        run_time=0.0_r8
        DO ng=1,Ngrids
          run_time=MAX(run_time, dt(ng)*ntimes(ng))
        END DO
        CALL ROMS_run (run_time)
      END IF
!
!-----------------------------------------------------------------------
!  Terminate ocean model execution: flush and close all IO files.
!-----------------------------------------------------------------------
!
      CALL ROMS_finalize
      CALL ROMS_deallocate_arrays
!
!-----------------------------------------------------------------------
!  If error, issue abort signal. It is expected that the Unix or POSIX
!  environment should handle this as a return errorcode from the ROMS
!  main to abort. It facilitates error handling in scripting.
!-----------------------------------------------------------------------
!
      IF ((exit_flag.ne.NoError).or.(blowup.ne.0)) ERROR STOP ! F-2008
!
      END PROGRAM MyROMS