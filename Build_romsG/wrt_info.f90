      MODULE wrt_info_mod
!
!git $Id$
!svn $Id: wrt_info.F 1212 2024-01-26 20:59:21Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This routine defines information variables in requested NetCDF      !
!  file.                                                               !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_grid
      Use mod_iounits
      USE mod_ncparam
      USE mod_netcdf
      USE mod_scalars
      USE mod_sources
!
      USE nf_fwrite2d_mod, ONLY : nf_fwrite2d
      USE strings_mod,     ONLY : FoundError, find_string
!
      implicit none
!
      INTERFACE wrt_info
        MODULE PROCEDURE wrt_info_nf90
      END INTERFACE wrt_info
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE wrt_info_nf90 (ng, model, ncid, ncname)
!***********************************************************************
!                                                                      !
!  This routine writes out information variables into requested        !
!  NetCDF file using the standard NetCDF-3 or NetCDF-4 library.        !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng           Nested grid number (integer)                        !
!     model        Calling model identifier (integer)                  !
!     ncid         NetCDF file ID (integer)                            !
!     ncname       NetCDF filename (string)                            !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     exit_flag    Error flag (integer) stored in MOD_SCALARS          !
!     ioerror      NetCDF return code (integer) stored in MOD_IOUNITS  !
!                                                                      !
!***********************************************************************
!
      USE distribute_mod,  ONLY : mp_bcasti
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, model, ncid
!
      character (len=*), intent(in) :: ncname
!
!  Local variable declarations.
!
      logical :: Cgrid = .TRUE.
!
      integer :: LBi, UBi, LBj, UBj
      integer :: i, j, k, ibry, ilev, itrc, status, varid
      integer, dimension(2) :: ibuffer
      integer :: ifield = 0
!
      real(dp) :: scale
      real(r8), dimension(NT(ng)) :: nudg
      real(r8), dimension(NT(ng),4) :: Tobc
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/wrt_info.F"//", wrt_info_nf90"
!
      SourceFile=MyFile
!
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)
!
!-----------------------------------------------------------------------
!  Write out running parameters.
!-----------------------------------------------------------------------
!
!  Inquire about the variables.
!
      CALL netcdf_inq_var (ng, model, ncname, ncid)
      IF (FoundError(exit_flag, NoError, 133, MyFile)) RETURN
!
!  Time stepping parameters.
!
      CALL netcdf_put_ivar (ng, model, ncname, 'ntimes',                &
     &                      ntimes(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 140, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'ndtfast',               &
     &                      ndtfast(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 145, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'dt',                    &
     &                      dt(ng), (/0/), (/0/),                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 150, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'dtfast',                &
     &                      dtfast(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 155, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'dstart',                &
     &                      dstart, (/0/), (/0/),                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 160, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'nHIS',                  &
     &                      nHIS(ng), (/0/), (/0/),                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 194, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'ndefHIS',               &
     &                      ndefHIS(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 199, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'nRST',                  &
     &                      nRST(ng), (/0/), (/0/),                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 204, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'ntsAVG',                &
     &                      ntsAVG(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 213, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'nAVG',                  &
     &                      nAVG(ng), (/0/), (/0/),                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 218, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'ndefAVG',               &
     &                      ndefAVG(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 223, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'ntsDIA',                &
     &                      ntsDIA(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 305, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'nDIA',                  &
     &                      nDIA(ng), (/0/), (/0/),                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 310, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'ndefDIA',               &
     &                      ndefDIA(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 315, MyFile)) RETURN
!
!  Power-law shape filter parameters for time-averaging of barotropic
!  fields.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'Falpha',                &
     &                      Falpha, (/0/), (/0/),                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 345, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Fbeta',                 &
     &                      Fbeta, (/0/), (/0/),                        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 350, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Fgamma',                &
     &                      Fgamma, (/0/), (/0/),                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 355, MyFile)) RETURN
!
!  Horizontal mixing coefficients.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'nl_tnu2',               &
     &                      nl_tnu2(:,ng), (/1/), (/NT(ng)/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 364, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'nl_visc2',              &
     &                      nl_visc2(ng), (/0/), (/0/),                 &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 416, MyFile)) RETURN
      CALL netcdf_put_lvar (ng, model, ncname, 'LuvSponge',             &
     &                      LuvSponge(ng), (/0/), (/0/),                &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 474, MyFile)) RETURN
      CALL netcdf_put_lvar (ng, model, ncname, 'LtracerSponge',         &
     &                      LtracerSponge(:,ng), (/1/), (/NT(ng)/),     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 481, MyFile)) RETURN
!
!  Background vertical mixing coefficients.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'Akt_bak',               &
     &                      Akt_bak(:,ng), (/1/), (/NT(ng)/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 491, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Akv_bak',               &
     &                      Akv_bak(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 496, MyFile)) RETURN
!
!  Drag coefficients.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'rdrg',                  &
     &                      rdrg(ng), (/0/), (/0/),                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 549, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'rdrg2',                 &
     &                      rdrg2(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 554, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Zob',                   &
     &                      Zob(ng), (/0/), (/0/),                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 560, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Zos',                   &
     &                      Zos(ng), (/0/), (/0/),                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 565, MyFile)) RETURN
!
!  Nudging inverse time scales used in various tasks.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'Znudg',                 &
     &                      Znudg(ng)/sec2day, (/0/), (/0/),            &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 664, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'M2nudg',                &
     &                      M2nudg(ng)/sec2day, (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 669, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'M3nudg',                &
     &                      M3nudg(ng)/sec2day, (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 675, MyFile)) RETURN
      DO itrc=1,NT(ng)
        nudg(itrc)=Tnudg(itrc,ng)/sec2day
      END DO
      CALL netcdf_put_fvar (ng, model, ncname, 'Tnudg',                 &
     &                      nudg, (/1/), (/NT(ng)/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 683, MyFile)) RETURN
!
!  Open boundary nudging, inverse time scales.
!
      IF (NudgingCoeff(ng)) THEN
        CALL netcdf_put_fvar (ng, model, ncname, 'FSobc_in',            &
     &                        FSobc_in(ng,:), (/1/), (/4/),             &
     &                        ncid = ncid)
        IF (FoundError(exit_flag, NoError, 694, MyFile)) RETURN
        CALL netcdf_put_fvar (ng, model, ncname, 'FSobc_out',           &
     &                        FSobc_out(ng,:), (/1/), (/4/),            &
     &                        ncid = ncid)
        IF (FoundError(exit_flag, NoError, 699, MyFile)) RETURN
        CALL netcdf_put_fvar (ng, model, ncname, 'M2obc_in',            &
     &                        M2obc_in(ng,:), (/1/), (/4/),             &
     &                        ncid = ncid)
        IF (FoundError(exit_flag, NoError, 704, MyFile)) RETURN
        CALL netcdf_put_fvar (ng, model, ncname, 'M2obc_out',           &
     &                        M2obc_out(ng,:), (/1/), (/4/),            &
     &                        ncid = ncid)
        IF (FoundError(exit_flag, NoError, 709, MyFile)) RETURN
        DO ibry=1,4
          DO itrc=1,NT(ng)
            Tobc(itrc,ibry)=Tobc_in(itrc,ng,ibry)
          END DO
        END DO
        CALL netcdf_put_fvar (ng, model, ncname, 'Tobc_in',             &
     &                        Tobc, (/1,1/), (/NT(ng),4/),              &
     &                        ncid = ncid)
        IF (FoundError(exit_flag, NoError, 720, MyFile)) RETURN
        DO ibry=1,4
          DO itrc=1,NT(ng)
            Tobc(itrc,ibry)=Tobc_out(itrc,ng,ibry)
          END DO
        END DO
        CALL netcdf_put_fvar (ng, model, ncname, 'Tobc_out',            &
     &                        Tobc, (/1,1/), (/NT(ng),4/),              &
     &                        ncid = ncid)
        IF (FoundError(exit_flag, NoError, 730, MyFile)) RETURN
        CALL netcdf_put_fvar (ng, model, ncname, 'M3obc_in',            &
     &                        M3obc_in(ng,:), (/1/), (/4/),             &
     &                        ncid = ncid)
        IF (FoundError(exit_flag, NoError, 735, MyFile)) RETURN
        CALL netcdf_put_fvar (ng, model, ncname, 'M3obc_out',           &
     &                        M3obc_out(ng,:), (/1/), (/4/),            &
     &                      ncid = ncid)
        IF (FoundError(exit_flag, NoError, 740, MyFile)) RETURN
      END IF
!
!  Equation of State parameters.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'rho0',                  &
     &                      rho0, (/0/), (/0/),                         &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 750, MyFile)) RETURN
!
!  Slipperiness parameters.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'gamma2',                &
     &                      gamma2(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 798, MyFile)) RETURN
!
! Logical switches to activate horizontal momentum transport
! point Sources/Sinks (like river runoff transport) and mass point
! Sources/Sinks (like volume vertical influx).
!
      CALL netcdf_put_lvar (ng, model, ncname, 'LuvSrc',                &
     &                      LuvSrc(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 807, MyFile)) RETURN
      CALL netcdf_put_lvar (ng, model, ncname, 'LwSrc',                 &
     &                      LwSrc(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 812, MyFile)) RETURN
!
!  Logical switches to activate tracer point Sources/Sinks.
!
      CALL netcdf_put_lvar (ng, model, ncname, 'LtracerSrc',            &
     &                      LtracerSrc(:,ng), (/1/), (/NT(ng)/),        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 821, MyFile)) RETURN
!
!  Logical switches to process climatology fields.
!
      CALL netcdf_put_lvar (ng, model, ncname, 'LsshCLM',               &
     &                      LsshCLM(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 829, MyFile)) RETURN
      CALL netcdf_put_lvar (ng, model, ncname, 'Lm2CLM',                &
     &                      Lm2CLM(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 834, MyFile)) RETURN
      CALL netcdf_put_lvar (ng, model, ncname, 'Lm3CLM',                &
     &                      Lm3CLM(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 840, MyFile)) RETURN
      CALL netcdf_put_lvar (ng, model, ncname, 'LtracerCLM',            &
     &                      LtracerCLM(:,ng), (/1/), (/NT(ng)/),        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 845, MyFile)) RETURN
!
!  Logical switches for nudging climatology fields.
!
      CALL netcdf_put_lvar (ng, model, ncname, 'LnudgeM2CLM',           &
     &                      LnudgeM2CLM(ng), (/0/), (/0/),              &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 853, MyFile)) RETURN
      CALL netcdf_put_lvar (ng, model, ncname, 'LnudgeM3CLM',           &
     &                      LnudgeM3CLM(ng), (/0/), (/0/),              &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 859, MyFile)) RETURN
      CALL netcdf_put_lvar (ng, model, ncname, 'LnudgeTCLM',            &
     &                      LnudgeTCLM(:,ng), (/1/), (/NT(ng)/),        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 864, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Write out grid variables.
!-----------------------------------------------------------------------
!
!  Grid type switch. Writing characters in parallel I/O is extremely
!  inefficient.  It is better to write this as an integer switch:
!  0=Cartesian, 1=spherical.
!
      CALL netcdf_put_lvar (ng, model, ncname, 'spherical',             &
     &                      spherical, (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1363, MyFile)) RETURN
!
!  Domain Length.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'xl',                    &
     &                      xl(ng), (/0/), (/0/),                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1370, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'el',                    &
     &                      el(ng), (/0/), (/0/),                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1375, MyFile)) RETURN
!
!  S-coordinate parameters.
!
      CALL netcdf_put_ivar (ng, model, ncname, 'Vtransform',            &
     &                      Vtransform(ng), (/0/), (/0/),               &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1384, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'Vstretching',           &
     &                      Vstretching(ng), (/0/), (/0/),              &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1389, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'theta_s',               &
     &                      theta_s(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1394, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'theta_b',               &
     &                      theta_b(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1399, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Tcline',                &
     &                      Tcline(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1404, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'hc',                    &
     &                      hc(ng), (/0/), (/0/),                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1409, MyFile)) RETURN
!
!  SGRID conventions for staggered data on structured grids. The value
!  is arbitrary but is set to unity so it can be used as logical during
!  post-processing.
!
      CALL netcdf_put_ivar (ng, model, ncname, 'grid',                  &
     &                      (/1/), (/0/), (/0/),                        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1418, MyFile)) RETURN
!
!  S-coordinate non-dimensional independent variables.
!
      CALL netcdf_put_fvar (ng, model, ncname, 's_rho',                 &
     &                      SCALARS(ng)%sc_r(:), (/1/), (/N(ng)/),      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1425, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 's_w',                   &
     &                      SCALARS(ng)%sc_w(0:), (/1/), (/N(ng)+1/),   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1430, MyFile)) RETURN
!
!  S-coordinate non-dimensional stretching curves.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'Cs_r',                  &
     &                      SCALARS(ng)%Cs_r(:), (/1/), (/N(ng)/),      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1437, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Cs_w',                  &
     &                      SCALARS(ng)%Cs_w(0:), (/1/), (/N(ng)+1/),   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1442, MyFile)) RETURN
!
!  User generic parameters.
!
      IF (Nuser.gt.0) THEN
        CALL netcdf_put_fvar (ng, model, ncname, 'user',                &
     &                        user, (/1/), (/Nuser/),                   &
     &                        ncid = ncid)
        IF (FoundError(exit_flag, NoError, 1451, MyFile)) RETURN
      END IF
!
!-----------------------------------------------------------------------
!  Write out grid tiled variables.
!-----------------------------------------------------------------------
!
      GRID_VARS : IF (ncid.ne.FLT(ng)%ncid) THEN
!
!  Bathymetry.
!
        IF (exit_flag.eq.NoError) THEN
          scale=1.0_dp
          IF (ncid.ne.STA(ng)%ncid) THEN
            IF (find_string(var_name, n_var, TRIM(Vname(1,idtopo)),     &
     &                      varid)) THEN
              status=nf_fwrite2d(ng, model, ncid, idtopo,               &
     &                           varid, 0, r2dvar,                      &
     &                           LBi, UBi, LBj, UBj, scale,             &
     &                           GRID(ng) % h,                          &
     &                           SetFillVal = .FALSE.)
              IF (FoundError(status, nf90_noerr, 1497, MyFile)) THEN
                IF (Master) WRITE (stdout,10) TRIM(Vname(1,idtopo)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=status
              END IF
            ELSE
              IF (Master) WRITE (stdout,20) TRIM(Vname(1,idtopo)),      &
     &                                      TRIM(ncname)
              exit_flag=3
              ioerror=nf90_enotvar
            END IF
          END IF
        END IF
!
!  Coriolis parameter.
!
        IF (exit_flag.eq.NoError) THEN
          IF (ncid.ne.STA(ng)%ncid) THEN
            scale=1.0_dp
            IF (find_string(var_name, n_var, TRIM(Vname(1,idfcor)),     &
     &                      varid)) THEN
              status=nf_fwrite2d(ng, model, ncid, idfcor,               &
     &                           varid, 0, r2dvar,                      &
     &                           LBi, UBi, LBj, UBj, scale,             &
     &                           GRID(ng) % f,                          &
     &                           SetFillVal = .FALSE.)
              IF (FoundError(status, nf90_noerr, 1540, MyFile)) THEN
                IF (Master) WRITE (stdout,10) TRIM(Vname(1,idfcor)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=status
              END IF
            ELSE
              IF (Master) WRITE (stdout,20) TRIM(Vname(1,idfcor)),      &
     &                                      TRIM(ncname)
              exit_flag=3
              ioerror=nf90_enotvar
            END IF
          END IF
        END IF
!
!  Curvilinear transformation metrics.
!
        IF (exit_flag.eq.NoError) THEN
          IF (ncid.ne.STA(ng)%ncid) THEN
            scale=1.0_dp
            IF (find_string(var_name, n_var, TRIM(Vname(1,idpmdx)),     &
     &                      varid)) THEN
              status=nf_fwrite2d(ng, model, ncid, idpmdx,               &
     &                           varid, 0, r2dvar,                      &
     &                           LBi, UBi, LBj, UBj, scale,             &
     &                           GRID(ng) % pm,                         &
     &                           SetFillVal = .FALSE.)
              IF (FoundError(status, nf90_noerr, 1570, MyFile)) THEN
                IF (Master) WRITE (stdout,10) TRIM(Vname(1,idpmdx)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=status
              END IF
            ELSE
              IF (Master) WRITE (stdout,20) TRIM(Vname(1,idpmdx)),      &
     &                                      TRIM(ncname)
              exit_flag=3
              ioerror=nf90_enotvar
            END IF
          END IF
        END IF
!
        IF (exit_flag.eq.NoError) THEN
          IF (ncid.ne.STA(ng)%ncid) THEN
            scale=1.0_dp
            IF (find_string(var_name, n_var, TRIM(Vname(1,idpndy)),     &
     &                      varid)) THEN
              status=nf_fwrite2d(ng, model, ncid, idpndy,               &
     &                           varid, 0, r2dvar,                      &
     &                           LBi, UBi, LBj, UBj, scale,             &
     &                           GRID(ng) % pn,                         &
     &                           SetFillVal = .FALSE.)
              IF (FoundError(status, nf90_noerr, 1598, MyFile)) THEN
                IF (Master) WRITE (stdout,10) TRIM(Vname(1,idpndy)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=status
              END IF
            ELSE
              IF (Master) WRITE (stdout,20) TRIM(Vname(1,idpndy)),      &
     &                                      TRIM(ncname)
              exit_flag=3
              ioerror=nf90_enotvar
            END IF
          END IF
        END IF
!
!  Grid coordinates of RHO-points.
!
        IF (spherical) THEN
          IF (exit_flag.eq.NoError) THEN
            scale=1.0_dp
            IF (ncid.ne.STA(ng)%ncid) THEN
              IF (find_string(var_name, n_var, TRIM(Vname(1,idLonR)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idLonR,             &
     &                             varid, 0, r2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % lonr,                     &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         1630, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLonR)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idLonR)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
!
          IF (exit_flag.eq.NoError) THEN
            scale=1.0_dp
            IF (ncid.ne.STA(ng)%ncid) THEN
              IF (find_string(var_name, n_var, TRIM(Vname(1,idLatR)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idLatR,             &
     &                             varid, 0, r2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % latr,                     &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         1672, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLatR)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idLatR)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
        END IF
!
        IF (.not.spherical) THEN
          IF (exit_flag.eq.NoError) THEN
            scale=1.0_dp
            IF (ncid.ne.STA(ng)%ncid) THEN
              IF (find_string(var_name, n_var, TRIM(Vname(1,idXgrR)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idXgrR,             &
     &                             varid, 0, r2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % xr,                       &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         1716, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idXgrR)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idXgrR)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
!
          IF (exit_flag.eq.NoError) THEN
            scale=1.0_dp
            IF (ncid.ne.STA(ng)%ncid) THEN
              IF (find_string(var_name, n_var, TRIM(Vname(1,idYgrR)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idYgrR,             &
     &                             varid, 0, r2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % yr,                       &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         1758, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idYgrR)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idYgrR)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
        END IF
!
!  Grid coordinates of U-points.
!
        IF (spherical) THEN
          IF (exit_flag.eq.NoError) THEN
            IF (ncid.ne.STA(ng)%ncid) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idLonU)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idLonU,             &
     &                             varid, 0, u2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % lonu,                     &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         1804, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLonU)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idLonU)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
!
          IF (exit_flag.eq.NoError) THEN
            IF (ncid.ne.STA(ng)%ncid) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idLatU)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idLatU,             &
     &                             varid, 0, u2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % latu,                     &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         1833, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLatU)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idLatU)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
        END IF
!
        IF (.not.spherical) THEN
          IF (exit_flag.eq.NoError) THEN
            IF (ncid.ne.STA(ng)%ncid) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idXgrU)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idXgrU,             &
     &                             varid, 0, u2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % xu,                       &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         1864, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idXgrU)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idXgrU)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
!
          IF (exit_flag.eq.NoError) THEN
            IF (ncid.ne.STA(ng)%ncid) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idYgrU)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idYgrU,             &
     &                             varid, 0, u2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % yu,                       &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         1893, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idYgrU)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idYgrU)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
        END IF
!
!  Grid coordinates of V-points.
!
        IF (spherical) THEN
          IF (exit_flag.eq.NoError) THEN
            IF (ncid.ne.STA(ng)%ncid) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idLonV)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idLonV,             &
     &                             varid, 0, v2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % lonv,                     &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         1926, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLonV)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLonV)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
!
          IF (exit_flag.eq.NoError) THEN
            IF (ncid.ne.STA(ng)%ncid) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idLatV)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idLatV,             &
     &                             varid, 0, v2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % latv,                     &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         1955, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLatV)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idLatV)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
        END IF
!
        IF (.not.spherical) THEN
          IF (exit_flag.eq.NoError) THEN
            IF (ncid.ne.STA(ng)%ncid) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idXgrV)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idXgrV,             &
     &                             varid, 0, v2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % xv,                       &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         1986, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idXgrV)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,10) TRIM(Vname(1,idXgrV)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
!
          IF (exit_flag.eq.NoError) THEN
            IF (ncid.ne.STA(ng)%ncid) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idYgrV)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idYgrV,             &
     &                             varid, 0, v2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % yv,                       &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         2015, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idYgrV)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idYgrV)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
        END IF
!
!  Grid coordinates of PSI-points.
!
        IF (spherical) THEN
          IF (exit_flag.eq.NoError) THEN
            IF (ncid.ne.STA(ng)%ncid) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idLonP)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idLonP,             &
     &                             varid, 0, p2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % lonp,                     &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         2048, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLonP)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idLonP)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
          IF (exit_flag.eq.NoError) THEN
            IF (ncid.ne.STA(ng)%ncid) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idLatP)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idLatP,             &
     &                             varid, 0, p2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % latp,                     &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         2077, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLatP)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idLatP)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
        END IF
!
        IF (.not.spherical) THEN
          IF (exit_flag.eq.NoError) THEN
            IF (ncid.ne.STA(ng)%ncid) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idXgrP)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idXgrP,             &
     &                             varid, 0, p2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % xp,                       &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         2108, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idXgrP)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idXgrP)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
!
          IF (exit_flag.eq.NoError) THEN
            IF (ncid.ne.STA(ng)%ncid) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idYgrP)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idYgrP,             &
     &                             varid, 0, p2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % yp,                       &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         2137, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idYgrP)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idYgrP)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
        END IF
      END IF GRID_VARS
!
!-----------------------------------------------------------------------
!  Synchronize NetCDF file to disk to allow other processes to access
!  data immediately after it is written.
!-----------------------------------------------------------------------
!
      CALL netcdf_sync (ng, model, ncname, ncid)
      IF (FoundError(exit_flag, NoError, 2487, MyFile)) RETURN
!
!  Broadcast error flags to all processors in the group.
!
      ibuffer(1)=exit_flag
      ibuffer(2)=ioerror
      CALL mp_bcasti (ng, model, ibuffer)
      exit_flag=ibuffer(1)
      ioerror=ibuffer(2)
!
  10  FORMAT (/,' WRT_INFO_NF90 - error while writing variable: ',a,/,  &
     &        17x,'into file: ',a)
  20  FORMAT (/,' WRT_INFO_NF90 - error while inquiring ID for',        &
     &        ' variable: ',a,/,17x,'in file: ',a)
  30  FORMAT (/,' WRT_INFO_NF90 - unable to synchronize to disk file:', &
     &        /,17x,a)
!
      RETURN
      END SUBROUTINE wrt_info_nf90
      END MODULE wrt_info_mod
