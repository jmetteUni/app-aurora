      MODULE nf_fwrite4d_mod
!
!git $Id$
!svn $Id: nf_fwrite4d.F 1210 2024-01-03 22:03:03Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This function writes out a generic floating point 4D array into an  !
!  output NetCDF file.                                                 !
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
!     LBk          K-dimension Lower bound                             !
!     UBk          K-dimension Upper bound                             !
!     LBt          Time-dimension Lower bound                          !
!     UBt          Time-dimension Upper bound                          !
!     Amask        land/Sea mask, if any (real)                        !
!     Ascl         Factor to scale field before writing (real)         !
!     Adat         Field to write out (real)                           !
!     SetFillVal   Logical switch to set fill value in land areas      !
!                    (OPTIONAL)                                        !
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
      implicit none
!
      INTERFACE nf_fwrite4d
        MODULE PROCEDURE nf90_fwrite4d
      END INTERFACE nf_fwrite4d
!
      CONTAINS
!
!***********************************************************************
      FUNCTION nf90_fwrite4d (ng, model, ncid, ifield,                  &
     &                        ncvarid, tindex, gtype,                   &
     &                        LBi, UBi, LBj, UBj, LBk, UBk, LBt, UBt,   &
     &                        Ascl,                                     &
     &                        Adat, SetFillVal,                         &
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
      integer, intent(in) :: LBi, UBi, LBj, UBj, LBk, UBk, LBt, UBt
!
      real(dp), intent(in) :: Ascl
!
      real(r8), intent(in) :: Adat(LBi:,LBj:,LBk:,LBt:)
      real(r8), intent(out), optional :: MinValue
      real(r8), intent(out), optional :: MaxValue
!
!  Local variable declarations.
!
      integer :: i, j, k, ic, fourth, Npts, tile
      integer :: Imin, Imax, Jmin, Jmax, Kmin, Kmax, Koff, Loff
      integer :: Ilen, Jlen, Klen, IJlen, MyType
      integer :: status
      integer, dimension(5) :: start, total
!
      real(r8), dimension((Lm(ng)+2)*(Mm(ng)+2)*(UBk-LBk+1)) :: Awrk
!
!-----------------------------------------------------------------------
!  Set starting and ending indices to process.
!-----------------------------------------------------------------------
!
      status=nf90_noerr
!
!  Set first and last grid point according to staggered C-grid
!  classification. Set loops offsets.
!
      MyType=gtype
!
      SELECT CASE (ABS(MyType))
        CASE (p2dvar, p3dvar)
          Imin=IOBOUNDS(ng)%ILB_psi
          Imax=IOBOUNDS(ng)%IUB_psi
          Jmin=IOBOUNDS(ng)%JLB_psi
          Jmax=IOBOUNDS(ng)%JUB_psi
        CASE (r2dvar, r3dvar)
          Imin=IOBOUNDS(ng)%ILB_rho
          Imax=IOBOUNDS(ng)%IUB_rho
          Jmin=IOBOUNDS(ng)%JLB_rho
          Jmax=IOBOUNDS(ng)%JUB_rho
        CASE (u2dvar, u3dvar)
          Imin=IOBOUNDS(ng)%ILB_u
          Imax=IOBOUNDS(ng)%IUB_u
          Jmin=IOBOUNDS(ng)%JLB_u
          Jmax=IOBOUNDS(ng)%JUB_u
        CASE (v2dvar, v3dvar)
          Imin=IOBOUNDS(ng)%ILB_v
          Imax=IOBOUNDS(ng)%IUB_v
          Jmin=IOBOUNDS(ng)%JLB_v
          Jmax=IOBOUNDS(ng)%JUB_v
        CASE DEFAULT
          Imin=IOBOUNDS(ng)%ILB_rho
          Imax=IOBOUNDS(ng)%IUB_rho
          Jmin=IOBOUNDS(ng)%JLB_rho
          Jmax=IOBOUNDS(ng)%JUB_rho
      END SELECT
!
      Ilen=Imax-Imin+1
      Jlen=Jmax-Jmin+1
      Klen=UBk-LBk+1
      IJlen=Ilen*Jlen
!
      IF (LBk.eq.0) THEN
        Koff=0
      ELSE
        Koff=1
      END IF
!
      IF (LBt.eq.0) THEN
        Loff=1
      ELSE
        Loff=0
      END IF
!
!  If appropriate, initialize minimum and maximum processed values.
!
      IF (PRESENT(MinValue)) THEN
        MinValue=spval
        MaxValue=-spval
      END IF
!
!  Initialize local array to avoid denormalized numbers. This
!  facilitates processing and debugging.
!
      Awrk=0.0_r8
!
!-----------------------------------------------------------------------
!  If serial or shared-memory applications and serial output, pack data
!  into a global 1D array in column-major order.
!-----------------------------------------------------------------------
!
!  Process data as 3D slices.
!
      DO fourth=LBt,UBt
        IF (gtype.gt.0) THEN
          ic=0
          Npts=IJlen*Klen
          DO k=LBk,UBk
            DO j=Jmin,Jmax
              DO i=Imin,Imax
                ic=ic+1
                Awrk(ic)=Adat(i,j,k,fourth)*Ascl
              END DO
            END DO
          END DO
        END IF
!
!-----------------------------------------------------------------------
!  If applicable, compute output field minimum and maximum values.
!-----------------------------------------------------------------------
!
        IF (PRESENT(MinValue)) THEN
          IF (OutThread) THEN
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
          IF (gtype.gt.0) THEN
            start(1)=1
            total(1)=Ilen
            start(2)=1
            total(2)=Jlen
            start(3)=1
            total(3)=Klen
            start(4)=fourth+Loff
            total(4)=1
            start(5)=tindex
            total(5)=1
          END IF
          status=nf90_put_var(ncid, ncvarid, Awrk, start, total)
        END IF
      END DO
!
      RETURN
      END FUNCTION nf90_fwrite4d
!
      END MODULE nf_fwrite4d_mod
