/*
** git $Id$
** svn $Id: upwelling.h 1151 2023-02-09 03:08:53Z arango $
*******************************************************************************
** Copyright (c) 2002-2023 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.md                                                     **
*******************************************************************************
**
** Options for Upwelling Test.
**
** Application flag:   AURORA
** Input script:       aurora.in
*/



#define SOLVE3D
#define SPHERICAL

#define UV_ADV
#define UV_COR
#define UV_LDRAG
#define UV_VIS2
#define MIX_S_UV

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

#undef ANA_GRID
#undef ANA_INITIAL
#undef ANA_BTFLUX

#define ANA_SMFLUX
#define ANA_STFLUX
#define ANA_SSFLUX
#define ANA_BSFLUX

#define RADIATION_2D

#define TIDE_GENERATING_FORCES
#if defined TIDE_GENERATING_FORCES
#define SSH_TIDES
#define UV_TIDES

#define ADD_FSOBC
#define ADD_M2OBC
#undef RAMP_TIDES
#endif


#if defined GLS_MIXING || defined MY25_MIXING
# define KANTHA_CLAYSON
# define N2S2_HORAVG
# define RI_SPLINES
#else
# define ANA_VMIX
#endif

#ifdef PERFECT_RESTART
# undef  AVERAGES
# undef  DIAGNOSTICS_BIO
# undef  DIAGNOSTICS_TS
# undef  DIAGNOSTICS_UV
# define OUT_DOUBLE
#endif
