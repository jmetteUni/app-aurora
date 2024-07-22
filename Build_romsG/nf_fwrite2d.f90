      MODULE nf_fwrite2d_mod
!
!git $Id$
!svn $Id: nf_fwrite2d.F 1210 2024-01-03 22:03:03Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This module writes out a generic floating point 2D array into       !
!  an output file using either the standard NetCDF library or the      !
!  Parallel-IO (PIO) library.                                          !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng           Nested grid number                                  !
!     model        Calling model identifier                            !
!     ncid         NetCDF file ID                                      !
!     ifield       Field metadata index (integer)                      !
!     ncvarid      NetCDF variable ID                                  !
!     tindex       NetCDF time record index to write                   !
!     gtype        Grid type. If negative, only write water points     !
!     LBi          I-dimension Lower bound                             !
!     UBi          I-dimension Upper bound                             !
!     LBj          J-dimension Lower bound                             !
!     UBj          J-dimension Upper bound                             !
!     Amask        land/Sea mask, if any (real)                        !
!     Ascl         Factor to scale field before writing (real)         !
!     Adat         Field to write out (real)                           !
!     SetFillVal   Logical switch to set fill value in land areas      !
!                    (OPTIONAL)                                        !
!     ExtractField Field extraction flag (integer, OPTIONAL)           !
!                    ExtractField = 0   no extraction                  !
!                    ExtractField = 1   extraction by interpolation    !
!                    ExtractField > 1   extraction by decimation       !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     status       Error flag (integer)                                !
!     MinValue     Minimum value (real, OPTIONAL)                      !
!     MaxValue     Maximum value (real, OPTIONAL)                      !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_ncparam
      USE mod_scalars
!
      USE pack_field_mod,  ONLY : pack_field2d
!
      implicit none
!
      INTERFACE nf_fwrite2d
        MODULE PROCEDURE nf90_fwrite2d
      END INTERFACE nf_fwrite2d
!
      CONTAINS
!
!***********************************************************************
      FUNCTION nf90_fwrite2d (ng, model, ncid, ifield,                  &
     &                        ncvarid, tindex, gtype,                   &
     &                        LBi, UBi, LBj, UBj, Ascl,                 &
     &                        Adat,                                     &
     &                        SetFillVal,                               &
     &                        ExtractField,                             &
     &                        MinValue, MaxValue) RESULT (status)
!***********************************************************************
!
      USE mod_netcdf
!
!  Imported variable declarations.
!
      logical, intent(in), optional :: SetFillVal
!
      integer, intent(in) :: ng, model, ncid, ncvarid, tindex, gtype
      integer, intent(in) :: ifield
      integer, intent(in) :: LBi, UBi, LBj, UBj
!
      integer, intent(in), optional :: ExtractField
!
      real(dp), intent(in) :: Ascl
!
      real(r8), intent(in) :: Adat(LBi:,LBj:)
      real(r8), intent(out), optional :: MinValue
      real(r8), intent(out), optional :: MaxValue
!
!  Local variable declarations.
!
      logical :: LandFill
!
      integer :: Extract_Flag
      integer :: Npts, i, tile
      integer :: status
      integer, dimension(3) :: start, total
!
      real(r8), dimension((Lm(ng)+2)*(Mm(ng)+2)) :: Awrk
!
!-----------------------------------------------------------------------
!  Initialize local variables.
!-----------------------------------------------------------------------
!
      status=nf90_noerr
!
!  Set parallel tile.
!
      tile=-1
!
!  Set switch to replace land areas with fill value, spval.
!
      LandFill=.FALSE.
!
!  If appropriate, set the field extraction flag to the provided grid
!  geometry through interpolation or decimation.
!
      IF (PRESENT(ExtractField)) THEN
        Extract_Flag=ExtractField
      ELSE
        Extract_Flag=0
      END IF
!
!  Initialize local array to avoid denormalized numbers. This
!  facilitates processing and debugging.
!
      Awrk=0.0_r8
!
!-----------------------------------------------------------------------
!  Pack 2D field data into 1D array.
!-----------------------------------------------------------------------
!
      CALL pack_field2d (ng, model, tile,                               &
     &                   gtype, ifield, tindex,                         &
     &                   LandFill, Extract_Flag,                        &
     &                   LBi, UBi, LBj, UBj,                            &
     &                   Ascl, Adat,                                    &
     &                   start, total, Npts, Awrk)
!
!-----------------------------------------------------------------------
!  If applicable, compute output field minimum and maximum values.
!-----------------------------------------------------------------------
!
      IF (PRESENT(MinValue)) THEN
        IF (OutThread) THEN
          MinValue=spval
          MaxValue=-spval
          DO i=1,Npts
            IF (ABS(Awrk(i)).lt.spval) THEN
              MinValue=MIN(MinValue,Awrk(i))
              MaxValue=MAX(MaxValue,Awrk(i))
            END IF
          END DO
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Write output buffer into NetCDF file.
!-----------------------------------------------------------------------
!
      IF (OutThread) THEN
        status=nf90_put_var(ncid, ncvarid, Awrk, start, total)
      END IF
!
      RETURN
      END FUNCTION nf90_fwrite2d
!
      END MODULE nf_fwrite2d_mod
