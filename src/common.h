#ifndef __common_h__
#define __common_h__
/*
	common.c
		includes most of the modules
		this file is part of Cuhre
		last modified 14 Feb 05 th
*/


#include "ChiSquare.h"
#include "Rule.h"
#include "Integrate.h"

/* 01-10-2023: changed bool to int */
static inline int BadDimension(ccount ndim)
{
#if NDIM > 0
  if( ndim > NDIM ) return 1;
#endif
  return ndim < 2;
}

/* 01-10-2023: changed bool to int */
static inline int BadComponent(cint ncomp)
{
#if NCOMP > 0
  if( ncomp > NCOMP ) return 1;
#endif
  return ncomp < 1;
}
#endif
