      MODULE get_grid_mod
!
!git $Id$
!svn $Id: get_grid.F 1210 2024-01-03 22:03:03Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This module reads grid information from input file using either     !
!  the standard NetCDF library or the Parallel-IO (PIO) library.       !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_grid
      USE mod_iounits
      USE mod_mixing
      USE mod_ncparam
      USE mod_netcdf
      USE mod_scalars
!
      USE exchange_2d_mod
      USE nf_fread2d_mod,  ONLY : nf_fread2d
      USE strings_mod,     ONLY : FoundError, find_string
!
      implicit none
!
      PUBLIC  :: get_grid
      PRIVATE :: get_grid_nf90
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE get_grid (ng, tile, model)
!***********************************************************************
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
!
!  Local variable declarations.
!
      integer :: LBi, UBi, LBj, UBj
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/get_grid.F"
!
!-----------------------------------------------------------------------
!  Read in GRID NetCDF file according to IO type.
!-----------------------------------------------------------------------
!
      LBi=BOUNDS(ng)%LBi(tile)
      UBi=BOUNDS(ng)%UBi(tile)
      LBj=BOUNDS(ng)%LBj(tile)
      UBj=BOUNDS(ng)%UBj(tile)
!
      SELECT CASE (GRD(ng)%IOtype)
        CASE (io_nf90)
          CALL get_grid_nf90 (ng, tile, model,                          &
     &                        LBi, UBi, LBj, UBj)
        CASE DEFAULT
          IF (Master) WRITE (stdout,10) GRD(ng)%IOtype
          exit_flag=2
      END SELECT
      IF (FoundError(exit_flag, NoError, 92, MyFile)) RETURN
!
  10  FORMAT (' GET_GRID - Illegal input file type, io_type = ',i0,     &
     &        /,12x,'Check KeyWord ''INP_LIB'' in ''roms.in''.')
!
      RETURN
      END SUBROUTINE get_grid
!
!***********************************************************************
      SUBROUTINE get_grid_nf90 (ng, tile, model,                        &
     &                          LBi, UBi, LBj, UBj)
!***********************************************************************
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile, model
      integer, intent(in) :: LBi, UBi, LBj, UBj
!
!  Local variable declarations.
!
      integer :: cr, gtype, i, status, vindex
      integer :: Vsize(4)
!
      real(dp), parameter :: Fscl = 1.0_dp
      real(r8) :: Fmax, Fmin
!
      character (len=256) :: ncname
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/get_grid.F"//", get_grid_nf90"
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Inquire about the contents of grid NetCDF file:  Inquire about
!  the dimensions and variables.  Check for consistency.
!-----------------------------------------------------------------------
!
      IF (FoundError(exit_flag, NoError, 148, MyFile)) RETURN
      ncname=GRD(ng)%name
!
!  Open grid NetCDF file for reading.
!
      IF (GRD(ng)%ncid.eq.-1) THEN
        CALL netcdf_open (ng, model, ncname, 0, GRD(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 155, MyFile)) THEN
          WRITE (stdout,10) TRIM(ncname)
          RETURN
        END IF
      END IF
!
!  Check grid file dimensions for consitency.
!
      CALL netcdf_check_dim (ng, model, ncname,                         &
     &                       ncid = GRD(ng)%ncid)
      IF (FoundError(exit_flag, NoError, 165, MyFile)) RETURN
!
!  Inquire about the variables.
!
      CALL netcdf_inq_var (ng, model, ncname,                           &
     &                     ncid = GRD(ng)%ncid)
      IF (FoundError(exit_flag, NoError, 171, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Check if required variables are available.
!-----------------------------------------------------------------------
!
      IF (.not.find_string(var_name,n_var,'xl',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'xl', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'el',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'el', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'spherical',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'spherical', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'h',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'h', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'f',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'f', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'pm',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'pm', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (.not.find_string(var_name,n_var,'pn',vindex)) THEN
        IF (Master) WRITE (stdout,20) 'pn', TRIM(ncname)
        exit_flag=2
        RETURN
      END IF
      IF (LuvSponge(ng)) THEN
        IF (.not.find_string(var_name,n_var,'visc_factor',vindex)) THEN
          IF (Master) WRITE (stdout,20) 'visc_factor', TRIM(ncname)
          exit_flag=2
          RETURN
        END IF
      END IF
      IF (ANY(LtracerSponge(:,ng))) THEN
        IF (.not.find_string(var_name,n_var,'diff_factor',vindex)) THEN
          IF (Master) WRITE (stdout,20) 'diff_factor', TRIM(ncname)
          exit_flag=2
          RETURN
        END IF
      END IF
!
!  Read in logical switch for spherical grid configuration.
!
      spherical=.FALSE.
      IF (find_string(var_name,n_var,'spherical',vindex)) THEN
        CALL netcdf_get_lvar (ng, model, ncname, 'spherical',           &
     &                        spherical,                                &
     &                        ncid = GRD(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 337, MyFile)) RETURN
      END IF
!
!-----------------------------------------------------------------------
!  Read in grid variables.
!-----------------------------------------------------------------------
!
!  Set Vsize to zero to deativate interpolation of input data to model
!  grid in "nf_fread2d".
!
      DO i=1,4
        Vsize(i)=0
      END DO
!
!  Scan the variable list and read in needed variables.
!
      IF (Master) WRITE (stdout,'(1x)')
!
      DO i=1,n_var
        SELECT CASE (TRIM(ADJUSTL(var_name(i))))
!
!  Read in basin X-length.
!
          CASE ('xl')
            CALL netcdf_get_fvar (ng, model, ncname,                    &
     &                            'xl', xl(ng),                         &
     &                            ncid = GRD(ng)%ncid)
            IF (FoundError(exit_flag, NoError, 365, MyFile)) EXIT
!
!  Read in basin Y-length.
!
          CASE ('el')
            CALL netcdf_get_fvar (ng, model, ncname,                    &
     &                            'el', el(ng),                         &
     &                            ncid = GRD(ng)%ncid)
            IF (FoundError(exit_flag, NoError, 373, MyFile)) EXIT
!
!  Read in bathymetry.
!
          CASE ('h')
            gtype=r2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % h)
            IF (FoundError(status, nf90_noerr, 393, MyFile)) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              Hmin(ng)=Fmin
              Hmax(ng)=Fmax
              IF (Master) THEN
                WRITE (stdout,30) 'bathymetry at RHO-points: h',        &
     &                            ng, TRIM(ncname), hmin(ng), hmax(ng)
              END IF
            END IF
            IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
              CALL exchange_r2d_tile (ng, tile,                         &
     &                                LBi, UBi, LBj, UBj,               &
     &                                GRID(ng) % h)
            END IF
!
!  Read in horizontal, spatially varying factor to increase/decrease
!  viscosity (nondimensional) in specific areas of the domain.
!
          CASE ('visc_factor')
            IF (LuvSponge(ng)) THEN
              gtype=r2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          MIXING(ng) % visc_factor)
              IF (FoundError(status, nf90_noerr, 753, MyFile)) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'horizontal viscosity sponge '//    &
     &                              'factor: visc_factor',              &
     &                              ng, TRIM(ncname), Fmin, Fmax
                END IF
              END IF
              IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
                CALL exchange_r2d_tile (ng, tile,                       &
     &                                  LBi, UBi, LBj, UBj,             &
     &                                  MIXING(ng) % visc_factor)
              END IF
            END IF
!
!  Read in horizontal, spatially varying factor to increase/decrease
!  diffusivity (nondimensional) in specific areas of the domain.
!
          CASE ('diff_factor')
            IF (ANY(LtracerSponge(:,ng))) THEN
              gtype=r2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          MIXING(ng) % diff_factor)
              IF (FoundError(status, nf90_noerr, 803, MyFile)) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'horizontal diffusivity sponge '//  &
     &                              'factor: diff_factor',              &
     &                              ng, TRIM(ncname), Fmin, Fmax
                END IF
              END IF
              IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
                CALL exchange_r2d_tile (ng, tile,                       &
     &                                  LBi, UBi, LBj, UBj,             &
     &                                  MIXING(ng) % diff_factor)
              END IF
            END IF
!
!  Read in Coriolis parameter.
!
          CASE ('f')
            gtype=r2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % f)
            IF (FoundError(status, nf90_noerr, 851, MyFile)) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'Coriolis parameter at RHO-points: f',&
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
              CALL exchange_r2d_tile (ng, tile,                         &
     &                                LBi, UBi, LBj, UBj,               &
     &                                GRID(ng) % f)
            END IF
!
!  Read in coordinate transfomation metrics (m) associated with the
!  differential distances in XI.
!
          CASE ('pm')
            gtype=r2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % pm)
            IF (FoundError(status, nf90_noerr, 905, MyFile)) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'reciprocal XI-grid spacing: pm',     &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
              CALL exchange_r2d_tile (ng, tile,                         &
     &                                LBi, UBi, LBj, UBj,               &
     &                                GRID(ng) % pm)
            END IF
!
!  Read in coordinate transfomation metrics (n) associated with the
!  differential distances in ETA.
!
          CASE ('pn')
            gtype=r2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % pn)
            IF (FoundError(status, nf90_noerr, 959, MyFile)) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'reciprocal ETA-grid spacing: pn',    &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
              CALL exchange_r2d_tile (ng, tile,                         &
     &                                LBi, UBi, LBj, UBj,               &
     &                                GRID(ng) % pn)
            END IF
!
!  Read in X-coordinates at PSI-points.
!
          CASE ('x_psi')
            gtype=p2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % xp)
            IF (FoundError(status, nf90_noerr, 1122, MyFile)) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'x-location of PSI-points: x_psi',    &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
!
!  Read in Y-coordinates at PSI-points.
!
          CASE ('y_psi')
            gtype=p2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % yp)
            IF (FoundError(status, nf90_noerr, 1161, MyFile)) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'y-location of PSI-points: y-psi',    &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
!
!  Read in X-coordinates at RHO-points.
!
          CASE ('x_rho')
            gtype=r2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % xr)
            IF (FoundError(status, nf90_noerr, 1200, MyFile)) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'x-location of RHO-points: x-rho',    &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            IF (.not.spherical) THEN
              LonMin(ng)=Fmin
              LonMax(ng)=Fmax
            END IF
!
!  Read in Y-coordinates at RHO-points.
!
          CASE ('y_rho')
            gtype=r2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % yr)
            IF (FoundError(status, nf90_noerr, 1255, MyFile)) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'y-location of RHO-points: y_rho',    &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            IF (.not.spherical) THEN
              LatMin(ng)=Fmin
              LatMax(ng)=Fmax
            END IF
!
!  Read in X-coordinates at U-points.
!
          CASE ('x_u')
            gtype=u2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % xu)
            IF (FoundError(status, nf90_noerr, 1310, MyFile)) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'x-location of U-points: x_u',        &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
!
!  Read in Y-coordinates at U-points.
!
          CASE ('y_u')
            gtype=u2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % yu)
            IF (FoundError(status, nf90_noerr, 1361, MyFile)) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'y-location of U-points: y_u',        &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
!
!  Read in X-coordinates at V-points.
!
          CASE ('x_v')
            gtype=v2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % xv)
            IF (FoundError(status, nf90_noerr, 1412, MyFile)) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'x-location of V-points: x_v',        &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
!
!  Read in Y-coordinates at V-points.
!
          CASE ('y_v')
            gtype=v2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % yv)
            IF (FoundError(status, nf90_noerr, 1463, MyFile)) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'y-location of V-points: y_v',        &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
!
!  Read in longitude at PSI-points.
!
          CASE ('lon_psi')
            IF (spherical) THEN
              gtype=p2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % lonp)
              IF (FoundError(status, nf90_noerr, 1515, MyFile)) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'longitude of PSI-points: lon_psi', &
     &                              ng, TRIM(ncname), Fmin, Fmax
                END IF
              END IF
            END IF
!
!  Read in latitude at PSI-points.
!
          CASE ('lat_psi')
            IF (spherical) THEN
              gtype=p2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % latp)
              IF (FoundError(status, nf90_noerr, 1556, MyFile)) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'latitude of PSI-points lat_psi',   &
     &                              ng, TRIM(ncname), Fmin, Fmax
                END IF
              END IF
            END IF
!
!  Read in longitude at RHO-points.
!
          CASE ('lon_rho')
            IF (spherical) THEN
              gtype=r2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, LonMin(ng), LonMax(ng),           &
     &                          GRID(ng) % lonr)
              IF (FoundError(status, nf90_noerr, 1597, MyFile)) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'longitude of RHO-points: lon_rho', &
     &                              ng, TRIM(ncname),                   &
     &                              LonMin(ng), LonMax(ng)
                END IF
              END IF
            END IF
!
!  Read in latitude at RHO-points.
!
          CASE ('lat_rho')
            IF (spherical) THEN
              gtype=r2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, LatMin(ng), LatMax(ng),           &
     &                          GRID(ng) % latr)
              IF (FoundError(status, nf90_noerr, 1649, MyFile)) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'latitude of RHO-points lat_rho',   &
     &                              ng, TRIM(ncname),                   &
     &                              LatMin(ng), LatMax(ng)
                END IF
              END IF
            END IF
!
!  Read in longitude at U-points.
!
          CASE ('lon_u')
            IF (spherical) THEN
              gtype=u2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % lonu)
              IF (FoundError(status, nf90_noerr, 1701, MyFile)) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'longitude of U-points: lon_u',     &
     &                              ng, TRIM(ncname), Fmin, Fmax
                END IF
              END IF
            END IF
!
!  Read in latitude at U-points.
!
          CASE ('lat_u')
            IF (spherical) THEN
              gtype=u2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % latu)
              IF (FoundError(status, nf90_noerr, 1752, MyFile)) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'latitude of U-points: lat_u',      &
     &                              ng, TRIM(ncname), Fmin, Fmax
                END IF
              END IF
            END IF
!
!  Read in longitude at V-points.
!
          CASE ('lon_v')
            IF (spherical) THEN
              gtype=v2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % lonv)
              IF (FoundError(status, nf90_noerr, 1803, MyFile)) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'longitude of V-points: lon_v',     &
     &                              ng, TRIM(ncname), Fmin, Fmax
                END IF
              END IF
            END IF
!
!  Read in latitude at V-points.
!
          CASE ('lat_v')
            IF (spherical) THEN
              gtype=v2dvar
              status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,        &
     &                          var_name(i), var_id(i),                 &
     &                          0, gtype, Vsize,                        &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          Fscl, Fmin, Fmax,                       &
     &                          GRID(ng) % latv)
              IF (FoundError(status, nf90_noerr, 1854, MyFile)) THEN
                exit_flag=2
                ioerror=status
                EXIT
              ELSE
                IF (Master) THEN
                  WRITE (stdout,30) 'latitude of V-points: lat_v',      &
     &                              ng, TRIM(ncname), Fmin, Fmax
                END IF
              END IF
            END IF
!
!  Read in angle (radians) between XI-axis and EAST at RHO-points.
!
          CASE ('angle')
            gtype=r2dvar
            status=nf_fread2d(ng, model, ncname, GRD(ng)%ncid,          &
     &                        var_name(i), var_id(i),                   &
     &                        0, gtype, Vsize,                          &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        Fscl, Fmin, Fmax,                         &
     &                        GRID(ng) % angler)
            IF (FoundError(status, nf90_noerr, 1904, MyFile)) THEN
              exit_flag=2
              ioerror=status
              EXIT
            ELSE
              IF (Master) THEN
                WRITE (stdout,30) 'angle between XI-axis and EAST: '//  &
     &                            'angler',                             &
     &                            ng, TRIM(ncname), Fmin, Fmax
              END IF
            END IF
            IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
              CALL exchange_r2d_tile (ng, tile,                         &
     &                                LBi, UBi, LBj, UBj,               &
     &                                GRID(ng) % angler)
            END IF
        END SELECT
      END DO
      IF (FoundError(exit_flag, NoError, 2085, MyFile)) THEN
        IF (Master) WRITE (stdout,40) TRIM(var_name(i)), TRIM(ncname)
        RETURN
      END IF
!
! Close GRID NetCDF file.
!
      CALL netcdf_close (ng, model, GRD(ng)%ncid, ncname, .FALSE.)
      IF (FoundError(exit_flag, NoError, 2272, MyFile)) RETURN
!
  10  FORMAT (/,' GET_GRID_NF90 - unable to open grid NetCDF file: ',a)
  20  FORMAT (/,' GET_GRID_NF90 - unable to find grid variable: ',a,    &
     &        /,12x,'in grid NetCDF file: ',a)
  30  FORMAT (2x,'GET_GRID_NF90    - ',a,/,22x,                         &
     &        '(Grid = ',i2.2,', File: ',a,')',/,22x,                   &
     &        '(Min = ', 1p,e15.8,0p,' Max = ',1p,e15.8,0p,')')
  40  FORMAT (/,' GET_GRID_NF90 - error while reading variable: ',a,    &
     &        /,12x,'in grid NetCDF file: ',a)
  50  FORMAT (/,2x,'GET_GRID_NF90    - Reading adjoint sensitivity',    &
     &        ' scope arrays from file:',/22x,a,/)
!
      RETURN
      END SUBROUTINE get_grid_nf90
      END MODULE get_grid_mod
