/*
** git $Id$
** svn $Id: upwelling.h 1151 2023-02-09 03:08:53Z arango $
*******************************************************************************
** Copyright (c) 2002-2023 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.md                                                     **
*******************************************************************************
**
** Options for Aurora Test.
**
** Application flag:   AURORA
** Input script:       aurora.in
*/
#define SOLVE3D
#define SPHERICAL
#define CURVGRID

#define UV_ADV
#define UV_COR
#define UV_LDRAG
#define UV_VIS2
#define MIX_GEO_UV
#define MIX_GEO_TS
#define DJ_GRADPS
#define TS_DIF2
#define NONLIN_EOS
#define SALINITY

#define SPLINES_VDIFF
#define SPLINES_VVISC

#define AVERAGES
#define DIAGNOSTICS_TS
#define DIAGNOSTICS_UV

#define ANA_GRID
#define ANA_INITIAL
#define ANA_BTFLUX

#define ANA_SMFLUX
#define ANA_STFLUX
#define ANA_SSFLUX
#define ANA_BSFLUX

# undef GLS_MIXING
# if defined GLS_MIXING
#  define CANUTO_A
#  define N2S2_HORAVG
#  define RI_SPLINES
# endif

#ifdef PERFECT_RESTART
# undef  AVERAGES
# undef  DIAGNOSTICS_BIO
# undef  DIAGNOSTICS_TS
# undef  DIAGNOSTICS_UV
# define OUT_DOUBLE
#endif
