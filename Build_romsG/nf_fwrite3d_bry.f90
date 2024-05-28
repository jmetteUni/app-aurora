      MODULE nf_fwrite3d_bry_mod
!
!git $Id$
!svn $Id: nf_fwrite3d_bry.F 1210 2024-01-03 22:03:03Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This function writes out a generic floating point 3D boundary array !
!  into an output file using either the standard NetCDF library or the !
!  Parallel-IO (PIO) library.                                          !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng           Nested grid number (integer)                        !
!     model        Calling model identifier (integer)                  !
!     ncname       NetCDF output file name (string)                    !
!     ncvname      NetCDF variable name (string)                       !
!     ncid         NetCDF file ID (integer)                            !
!     ncvarid      NetCDF variable ID (integer)                        !
!     tindex       NetCDF time record index to write (integer)         !
!     gtype        Grid type (integer)                                 !
!     LBij         IJ-dimension lower bound (integer)                  !
!     UBij         IJ-dimension upper bound (integer)                  !
!     LBk          K-dimension lower bound (integer)                   !
!     UBk          K-dimension upper bound (integer)                   !
!     Nrec         Number of boundary records (integer)                !
!     Ascl         Factor to scale field before writing (real)         !
!     Abry         Boundary field to write out (real)                  !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     status       Error flag (integer).                               !
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
      INTERFACE nf_fwrite3d_bry
        MODULE PROCEDURE nf90_fwrite3d_bry
      END INTERFACE nf_fwrite3d_bry
!
      CONTAINS
!
!***********************************************************************
      FUNCTION nf90_fwrite3d_bry (ng, model, ncname, ncid,              &
     &                            ncvname, ncvarid,                     &
     &                            tindex, gtype,                        &
     &                            LBij, UBij, LBk, UBk, Nrec,           &
     &                            Ascl, Abry,                           &
     &                            MinValue, MaxValue) RESULT(status)
!***********************************************************************
!
      USE mod_netcdf
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, model, ncid, ncvarid, tindex, gtype
      integer, intent(in) :: LBij, UBij, LBk, UBk, Nrec
!
      real(dp), intent(in) :: Ascl
!
      character (len=*), intent(in) :: ncname
      character (len=*), intent(in) :: ncvname
!
      real(r8), intent(in) :: Abry(LBij:,LBk:,:,:)
      real(r8), intent(out), optional :: MinValue
      real(r8), intent(out), optional :: MaxValue
!
!  Local variable declarations.
!
      logical, dimension(4) :: bounded
!
      integer :: bc, i, ib, ic, ir, j, k, kc, rc, tile
      integer :: IorJ, IJKlen, Imin, Imax, Jmin, Jmax, Klen, Npts
      integer :: Istr, Iend, Jstr, Jend
      integer, dimension(5) :: start, total
      integer :: status
!
      real(r8), parameter :: Aspv = 0.0_r8
      real(r8), dimension((UBij-LBij+1)*(UBk-LBk+1)*4*Nrec) :: Awrk
!
!-----------------------------------------------------------------------
!  Set starting and ending indices to process.
!-----------------------------------------------------------------------
!
      tile=-1
      Imin=LBij
      Imax=UBij
      Jmin=LBij
      Jmax=UBij
      IorJ=IOBOUNDS(ng)%IorJ
      Klen=UBk-LBk+1
      IJKlen=IorJ*Klen
      Npts=IJKlen*4*Nrec
!
!  Get tile bounds.
!
      Istr=BOUNDS(ng)%Istr(tile)
      Iend=BOUNDS(ng)%Iend(tile)
      Jstr=BOUNDS(ng)%Jstr(tile)
      Jend=BOUNDS(ng)%Jend(tile)
!
!  Set switch to process boundary data by their associated tiles.
!
      bounded(iwest )=DOMAIN(ng)%Western_Edge (tile)
      bounded(ieast )=DOMAIN(ng)%Eastern_Edge (tile)
      bounded(isouth)=DOMAIN(ng)%Southern_Edge(tile)
      bounded(inorth)=DOMAIN(ng)%Northern_Edge(tile)
!
!  Set NetCDF dimension counters for processing requested field.
!
      start(1)=1
      total(1)=IorJ
      start(2)=1
      total(2)=Klen
      start(3)=1
      total(3)=4
      start(4)=1
      total(4)=Nrec
      start(5)=tindex
      total(5)=1
!
!-----------------------------------------------------------------------
!  Pack and scale output data.
!-----------------------------------------------------------------------
!
      Awrk=Aspv
!
      DO ir=1,Nrec
        rc=(ir-1)*IJKlen*4
        DO ib=1,4
          IF (bounded(ib)) THEN
            bc=(ib-1)*IJKlen+rc
            IF ((ib.eq.iwest).or.(ib.eq.ieast)) THEN
              DO k=LBk,UBk
                kc=(k-LBk)*IorJ+bc
                DO j=Jmin,Jmax
                  ic=1+(j-LBij)+kc
                  Awrk(ic)=Abry(j,k,ib,ir)*Ascl
                END DO
              END DO
            ELSE IF ((ib.eq.isouth).or.(ib.eq.inorth)) THEN
              DO k=LBk,UBk
                kc=(k-LBk)*IorJ+bc
                DO i=Imin,Imax
                  ic=1+(i-LBij)+kc
                  Awrk(ic)=Abry(i,k,ib,ir)*Ascl
                END DO
              END DO
            END IF
          END IF
        END DO
      END DO
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
      status=nf90_noerr
      IF (OutThread) THEN
        status=nf90_put_var(ncid, ncvarid, Awrk, start, total)
      END IF
!
      RETURN
      END FUNCTION nf90_fwrite3d_bry
      END MODULE nf_fwrite3d_bry_mod
