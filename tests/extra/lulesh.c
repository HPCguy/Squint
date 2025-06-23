/*

                 Copyright (c) 2010.
      Lawrence Livermore National Security, LLC.
Produced at the Lawrence Livermore National Laboratory.
                  LLNL-CODE-461231
                All rights reserved.

This file is part of LULESH, Version 1.0.
Please also read this link -- http://www.opensource.org/licenses/index.php

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the disclaimer below.

   * Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the disclaimer (as noted below)
     in the documentation and/or other materials provided with the
     distribution.

   * Neither the name of the LLNS/LLNL nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL LAWRENCE LIVERMORE NATIONAL SECURITY, LLC,
THE U.S. DEPARTMENT OF ENERGY OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


Additional BSD Notice

1. This notice is required to be provided under our contract with the U.S.
   Department of Energy (DOE). This work was produced at Lawrence Livermore
   National Laboratory under Contract No. DE-AC52-07NA27344 with the DOE.

2. Neither the United States Government nor Lawrence Livermore National
   Security, LLC nor any of their employees, makes any warranty, express
   or implied, or assumes any liability or responsibility for the accuracy,
   completeness, or usefulness of any information, apparatus, product, or
   process disclosed, or represents that its use would not infringe
   privately-owned rights.

3. Also, reference herein to any specific commercial products, process, or
   services by trade name, trademark, manufacturer or otherwise does not
   necessarily itute or imply its endorsement, recommendation, or
   favoring by the United States Government or Lawrence Livermore National
   Security, LLC. The views and opinions of authors expressed herein do not
   necessarily state or reflect those of the United States Government or
   Lawrence Livermore National Security, LLC, and shall not be used for
   advertising or product endorsement purposes.

*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

typedef float Real_t; // floating point representation
typedef int Index_t; // array indexing type

float cbrtf(float x);
float fmaxf(float x, float y);

#define ZERO 0.0f
#define ONE  1.0f
#define HALF 0.5f

#define LULESH_SHOW_PROGRESS 1

enum { VolumeError = -1, QStopError = -2 } ;

/*********************************/
/* Data structure implementation */
/*********************************/

/* might want to add access methods so that memory can be */
/* better managed, as in luleshFT */

typedef struct Domain_s {
   /* Elem-centered */

   Index_t *nodelist;     /* elemToNode connectivity */

   Index_t *lxim;         /* elem connectivity through face */
   Index_t *lxip;
   Index_t *letam;
   Index_t *letap;
   Index_t *lzetam;
   Index_t *lzetap;

   int *elemBC;           /* elem face symm/free-surface flag */

   Real_t *e;             /* energy */

   Real_t *p;             /* pressure */

   Real_t *q;             /* q */
   Real_t *ql;            /* linear term for q */
   Real_t *qq;            /* quadratic term for q */

   Real_t *v;             /* relative volume */

   Real_t *volo;          /* reference volume */
   Real_t *delv;          /* m_vnew - m_v */
   Real_t *vdov;          /* volume derivative over volume */

   Real_t *arealg;        /* elem characteristic length */

   Real_t *ss;            /* "sound speed" */

   Real_t *elemMass;      /* mass */

   /* Elem temporaries */

   Real_t *vnew;          /* new relative volume -- temporary */

   Real_t *delv_xi;       /* velocity gradient -- temporary */
   Real_t *delv_eta;
   Real_t *delv_zeta;

   Real_t *delx_xi;       /* position gradient -- temporary */
   Real_t *delx_eta;
   Real_t *delx_zeta;

   Real_t *dxx;           /* principal strains -- temporary */
   Real_t *dyy;
   Real_t *dzz;

   /* Node-centered */

   Real_t *x;             /* coordinates */
   Real_t *y;
   Real_t *z;

   Real_t *xd;            /* velocities */
   Real_t *yd;
   Real_t *zd;

   Real_t *xdd;           /* accelerations */
   Real_t *ydd;
   Real_t *zdd;

   Real_t *fx;            /* forces */
   Real_t *fy;
   Real_t *fz;

   Real_t *nodalMass;     /* mass */


   /* Boundary nodesets */

   Index_t *symmX;        /* Nodes on X symmetry plane */
   Index_t *symmY;        /* Nodes on Y symmetry plane */
   Index_t *symmZ;        /* Nodes on Z symmetry plane */

   /* Parameters */

   Real_t  dtfixed;           /* fixed time increment */
   Real_t  time;              /* current time */
   Real_t  deltatime;         /* variable time increment */
   Real_t  deltatimemultlb;
   Real_t  deltatimemultub;
   Real_t  stoptime;          /* end time for simulation */

   Real_t  u_cut;             /* velocity tolerance */
   Real_t  hgcoef;            /* hourglass control */
   Real_t  qstop;             /* excessive q indicator */
   Real_t  monoq_max_slope;
   Real_t  monoq_limiter_mult;
   Real_t  e_cut;             /* energy tolerance */
   Real_t  p_cut;             /* pressure tolerance */
   Real_t  ss4o3;
   Real_t  q_cut;             /* q tolerance */
   Real_t  v_cut;             /* relative volume tolerance */
   Real_t  qlc_monoq;         /* linear term coef for q */
   Real_t  qqc_monoq;         /* quadratic term coef for q */
   Real_t  qqc;
   Real_t  eosvmax;
   Real_t  eosvmin;
   Real_t  pmin;              /* pressure floor */
   Real_t  emin;              /* energy floor */
   Real_t  dvovmax;           /* maximum allowable volume change */
   Real_t  refdens;           /* reference density */

   Real_t  dtcourant;         /* courant constraint */
   Real_t  dthydro;           /* volume change constraint */
   Real_t  dtmax;             /* maximum allowable time increment */

   int   cycle;               /* iteration count for simulation */

   Index_t sizeX;
   Index_t sizeY;
   Index_t sizeZ;
   Index_t numElem;

   Index_t numNode;
} Domain;

Real_t *dvdx_;
Real_t *dvdy_;
Real_t *dvdz_;
Real_t *x8n_;
Real_t *y8n_;
Real_t *z8n_;
Real_t *sigxx_;
Real_t *sigyy_;
Real_t *sigzz_;
Real_t *determ_;
Real_t *p_old_;
Real_t *compression_;
Real_t *compHalfStep_;
Real_t *work_;
Real_t *p_new_;
Real_t *e_new_;
Real_t *q_new_;
Real_t *bvc_;
Real_t *pbvc_;
Real_t *pHalfStep_;
Real_t *vnewc_;

Real_t *AllocateReal(int size)
{
   Real_t *retVal;
   posix_memalign((void **)&retVal, 32, size*sizeof(Real_t));
   return retVal;
}

Index_t *AllocateIndex(int size)
{
   Index_t *retVal;
   posix_memalign((void **)&retVal, 32, size*sizeof(Index_t));
   return retVal;
}

int *AllocateInt(int size)
{
   int *retVal;
   posix_memalign((void **)&retVal, 32, size*sizeof(int));
   return retVal;
}

void Release(void **ptr)
{
   if (*ptr != (void *) 0) {
      free(*ptr);
      *ptr = (void *) 0;
   }
}


/* Stuff needed for boundary conditions */
/* 2 BCs on each of 6 hexahedral faces (12 bits) */
#define XI_M        0x003
#define XI_M_SYMM   0x001
#define XI_M_FREE   0x002

#define XI_P        0x00c
#define XI_P_SYMM   0x004
#define XI_P_FREE   0x008

#define ETA_M       0x030
#define ETA_M_SYMM  0x010
#define ETA_M_FREE  0x020

#define ETA_P       0x0c0
#define ETA_P_SYMM  0x040
#define ETA_P_FREE  0x080

#define ZETA_M      0x300
#define ZETA_M_SYMM 0x100
#define ZETA_M_FREE 0x200

#define ZETA_P      0xc00
#define ZETA_P_SYMM 0x400
#define ZETA_P_FREE 0x800


void TimeIncrement(Domain *domain)
{
   Real_t targetdt = domain->stoptime - domain->time;
   Real_t newdt = domain->deltatime;

   if ((domain->dtfixed <= ZERO) && (domain->cycle != 0)) {
      Real_t ratio;
      Real_t olddt = domain->deltatime;

      /* This will require a reduction in parallel */
      newdt = 1.0e+20;
      if (domain->dtcourant < newdt) {
         newdt = domain->dtcourant / 2.0f;
      }
      if (domain->dthydro < newdt) {
         newdt = domain->dthydro * 2.0f / 3.0f;
      }

      ratio = newdt / olddt;
      if (ratio >= ONE) {
         if (ratio < domain->deltatimemultlb) {
            newdt = olddt;
         }
         else if (ratio > domain->deltatimemultub) {
            newdt = olddt*domain->deltatimemultub;
         }
      }

      if (newdt > domain->dtmax) {
         newdt = domain->dtmax;
      }
   }

   /* TRY TO PREVENT VERY SMALL SCALING ON THE NEXT CYCLE */
   if ((targetdt > newdt) &&
       (targetdt < (4.0f * newdt / 3.0f)) ) {
      targetdt = 2.0f * newdt / 3.0f;
   }

   if (targetdt < newdt) {
      newdt = targetdt;
   }

   domain->time += newdt;

   domain->deltatime = newdt;

   ++domain->cycle;
}

void InitStressTermsForElems(Real_t *p, Real_t *q,
                             Real_t *sigxx, Real_t *sigyy, Real_t *sigzz,
                             Index_t numElem)
{
   //
   // pull in the stresses appropriate to the hydro integration
   //

   for (Index_t idx=0; idx<numElem; ++idx) {
      sigxx[idx] = sigyy[idx] = sigzz[idx] =  - p[idx] - q[idx];
   }
}

void CalcElemShapeFunctionDerivatives( Real_t *x, Real_t *y, Real_t *z,
                                       Real_t b[][8], Real_t *volume)
{
   Real_t fjxxi, fjxet, fjxze;
   Real_t fjyxi, fjyet, fjyze;
   Real_t fjzxi, fjzet, fjzze;

   {
      Real_t x0 := x[0];    Real_t x1 := x[1];
      Real_t x2 := x[2];    Real_t x3 := x[3];
      Real_t x4 := x[4];    Real_t x5 := x[5];
      Real_t x6 := x[6];    Real_t x7 := x[7];

      Real_t t1, t2, t3, t4;

      t1 = (x6-x0); t2 = (x5-x3); t3 = (x7-x1); t4 = (x4-x2);

      fjxxi = 0.125f * ( t1 + t2 - t3 - t4 );
      fjxet = 0.125f * ( t1 - t2 + t3 - t4 );
      fjxze = 0.125f * ( t1 + t2 + t3 + t4 );
   }

   {
      Real_t y0 := y[0];    Real_t y1 := y[1];
      Real_t y2 := y[2];    Real_t y3 := y[3];
      Real_t y4 := y[4];    Real_t y5 := y[5];
      Real_t y6 := y[6];    Real_t y7 := y[7];

      Real_t t1, t2, t3, t4;

      t1 = (y6-y0); t2 = (y5-y3); t3 = (y7-y1); t4 = (y4-y2);

      fjyxi = 0.125f * ( t1 + t2 - t3 - t4 );
      fjyet = 0.125f * ( t1 - t2 + t3 - t4 );
      fjyze = 0.125f * ( t1 + t2 + t3 + t4 );
   }

   {
      Real_t z0 := z[0];    Real_t z1 := z[1];
      Real_t z2 := z[2];    Real_t z3 := z[3];
      Real_t z4 := z[4];    Real_t z5 := z[5];
      Real_t z6 := z[6];    Real_t z7 := z[7];

      Real_t t1, t2, t3, t4;

      t1 = (z6-z0); t2 = (z5-z3); t3 = (z7-z1); t4 = (z4-z2);

      fjzxi = 0.125f * ( t1 + t2 - t3 - t4 );
      fjzet = 0.125f * ( t1 - t2 + t3 - t4 );
      fjzze = 0.125f * ( t1 + t2 + t3 + t4 );
   }

   Real_t cjxxi, cjxet, cjxze;
   Real_t cjyxi, cjyet, cjyze;
   Real_t cjzxi, cjzet, cjzze;

   /* compute cofactors */
   cjxxi =    (fjyet * fjzze) - (fjzet * fjyze);
   cjxet =  - (fjyxi * fjzze) + (fjzxi * fjyze);
   cjxze =    (fjyxi * fjzet) - (fjzxi * fjyet);

   cjyxi =  - (fjxet * fjzze) + (fjzet * fjxze);
   cjyet =    (fjxxi * fjzze) - (fjzxi * fjxze);
   cjyze =  - (fjxxi * fjzet) + (fjzxi * fjxet);

   cjzxi =    (fjxet * fjyze) - (fjyet * fjxze);
   cjzet =  - (fjxxi * fjyze) + (fjyxi * fjxze);
   cjzze =    (fjxxi * fjyet) - (fjyxi * fjxet);

   /* calculate partials :
      this need only be done for l = 0,1,2,3   since , by symmetry ,
      (6,7,4,5) = - (0,1,2,3) .
   */
   b[0][6] = - (b[0][0] =   -  cjxxi  -  cjxet  -  cjxze);
   b[0][7] = - (b[0][1] =      cjxxi  -  cjxet  -  cjxze);
   b[0][4] = - (b[0][2] =      cjxxi  +  cjxet  -  cjxze);
   b[0][5] = - (b[0][3] =   -  cjxxi  +  cjxet  -  cjxze);
   // b[0][4] = -b[0][2];
   // b[0][5] = -b[0][3];
   // b[0][6] = -b[0][0];
   // b[0][7] = -b[0][1];

   b[1][6] = - (b[1][0] =   -  cjyxi  -  cjyet  -  cjyze);
   b[1][7] = - (b[1][1] =      cjyxi  -  cjyet  -  cjyze);
   b[1][4] = - (b[1][2] =      cjyxi  +  cjyet  -  cjyze);
   b[1][5] = - (b[1][3] =   -  cjyxi  +  cjyet  -  cjyze);
   // b[1][4] = -b[1][2];
   // b[1][5] = -b[1][3];
   // b[1][6] = -b[1][0];
   // b[1][7] = -b[1][1];

   b[2][6] = - (b[2][0] =   -  cjzxi  -  cjzet  -  cjzze);
   b[2][7] = - (b[2][1] =      cjzxi  -  cjzet  -  cjzze);
   b[2][4] = - (b[2][2] =      cjzxi  +  cjzet  -  cjzze);
   b[2][5] = - (b[2][3] =   -  cjzxi  +  cjzet  -  cjzze);
   // b[2][4] = -b[2][2];
   // b[2][5] = -b[2][3];
   // b[2][6] = -b[2][0];
   // b[2][7] = -b[2][1];

   /* calculate jacobian determinant (volume) */
   *volume = 8.0f * ( fjxet * cjxet + fjyet * cjyet + fjzet * cjzet);
}

inline void SumElemFaceNormal(
                       Real_t *normalX0, Real_t *normalY0, Real_t *normalZ0,
                       Real_t *normalX1, Real_t *normalY1, Real_t *normalZ1,
                       Real_t *normalX2, Real_t *normalY2, Real_t *normalZ2,
                       Real_t *normalX3, Real_t *normalY3, Real_t *normalZ3,
                       Real_t x0,  Real_t y0,  Real_t z0,
                       Real_t x1,  Real_t y1,  Real_t z1,
                       Real_t x2,  Real_t y2,  Real_t z2,
                       Real_t x3,  Real_t y3,  Real_t z3)
{
   Real_t bisectX0 = x3 + x2 - x1 - x0;
   Real_t bisectY0 = y3 + y2 - y1 - y0;
   Real_t bisectZ0 = z3 + z2 - z1 - z0;
   Real_t bisectX1 = x2 + x1 - x3 - x0;
   Real_t bisectY1 = y2 + y1 - y3 - y0;
   Real_t bisectZ1 = z2 + z1 - z3 - z0;
   Real_t areaX = 0.0625f * (bisectY0 * bisectZ1 - bisectZ0 * bisectY1);
   Real_t areaY = 0.0625f * (bisectZ0 * bisectX1 - bisectX0 * bisectZ1);
   Real_t areaZ = 0.0625f * (bisectX0 * bisectY1 - bisectY0 * bisectX1);

   *normalX0 += areaX;
   *normalX1 += areaX;
   *normalX2 += areaX;
   *normalX3 += areaX;

   *normalY0 += areaY;
   *normalY1 += areaY;
   *normalY2 += areaY;
   *normalY3 += areaY;

   *normalZ0 += areaZ;
   *normalZ1 += areaZ;
   *normalZ2 += areaZ;
   *normalZ3 += areaZ;
}

void CalcElemNodeNormals(Real_t *pfx, Real_t *pfy, Real_t *pfz,
                         Real_t *x, Real_t *y, Real_t *z)
{
   Index_t i;
   for (i = 0 ; i < 8 ; ++i ) {
      pfx[i] = ZERO;
      pfy[i] = ZERO;
      pfz[i] = ZERO;
   }
   /* evaluate face one: nodes 0, 1, 2, 3 */
   SumElemFaceNormal(&pfx[0], &pfy[0], &pfz[0],
                  &pfx[1], &pfy[1], &pfz[1],
                  &pfx[2], &pfy[2], &pfz[2],
                  &pfx[3], &pfy[3], &pfz[3],
                  x[0], y[0], z[0], x[1], y[1], z[1],
                  x[2], y[2], z[2], x[3], y[3], z[3]);
   /* evaluate face two: nodes 0, 4, 5, 1 */
   SumElemFaceNormal(&pfx[0], &pfy[0], &pfz[0],
                  &pfx[4], &pfy[4], &pfz[4],
                  &pfx[5], &pfy[5], &pfz[5],
                  &pfx[1], &pfy[1], &pfz[1],
                  x[0], y[0], z[0], x[4], y[4], z[4],
                  x[5], y[5], z[5], x[1], y[1], z[1]);
   /* evaluate face three: nodes 1, 5, 6, 2 */
   SumElemFaceNormal(&pfx[1], &pfy[1], &pfz[1],
                  &pfx[5], &pfy[5], &pfz[5],
                  &pfx[6], &pfy[6], &pfz[6],
                  &pfx[2], &pfy[2], &pfz[2],
                  x[1], y[1], z[1], x[5], y[5], z[5],
                  x[6], y[6], z[6], x[2], y[2], z[2]);
   /* evaluate face four: nodes 2, 6, 7, 3 */
   SumElemFaceNormal(&pfx[2], &pfy[2], &pfz[2],
                  &pfx[6], &pfy[6], &pfz[6],
                  &pfx[7], &pfy[7], &pfz[7],
                  &pfx[3], &pfy[3], &pfz[3],
                  x[2], y[2], z[2], x[6], y[6], z[6],
                  x[7], y[7], z[7], x[3], y[3], z[3]);
   /* evaluate face five: nodes 3, 7, 4, 0 */
   SumElemFaceNormal(&pfx[3], &pfy[3], &pfz[3],
                  &pfx[7], &pfy[7], &pfz[7],
                  &pfx[4], &pfy[4], &pfz[4],
                  &pfx[0], &pfy[0], &pfz[0],
                  x[3], y[3], z[3], x[7], y[7], z[7],
                  x[4], y[4], z[4], x[0], y[0], z[0]);
   /* evaluate face six: nodes 4, 7, 6, 5 */
   SumElemFaceNormal(&pfx[4], &pfy[4], &pfz[4],
                  &pfx[7], &pfy[7], &pfz[7],
                  &pfx[6], &pfy[6], &pfz[6],
                  &pfx[5], &pfy[5], &pfz[5],
                  x[4], y[4], z[4], x[7], y[7], z[7],
                  x[6], y[6], z[6], x[5], y[5], z[5]);
}

void SumElemStressesToNodeForces(Real_t B[][8], Real_t stress_xx,
                                 Real_t stress_yy, Real_t stress_zz,
                                 Real_t *fx, Real_t *fy, Real_t *fz)
{
  Real_t pfx0 := B[0][0];   Real_t pfx1 := B[0][1];
  Real_t pfx2 := B[0][2];   Real_t pfx3 := B[0][3];
  Real_t pfx4 := B[0][4];   Real_t pfx5 := B[0][5];
  Real_t pfx6 := B[0][6];   Real_t pfx7 := B[0][7];

  Real_t pfy0 := B[1][0];   Real_t pfy1 := B[1][1];
  Real_t pfy2 := B[1][2];   Real_t pfy3 := B[1][3];
  Real_t pfy4 := B[1][4];   Real_t pfy5 := B[1][5];
  Real_t pfy6 := B[1][6];   Real_t pfy7 := B[1][7];

  Real_t pfz0 := B[2][0];   Real_t pfz1 := B[2][1];
  Real_t pfz2 := B[2][2];   Real_t pfz3 := B[2][3];
  Real_t pfz4 := B[2][4];   Real_t pfz5 := B[2][5];
  Real_t pfz6 := B[2][6];   Real_t pfz7 := B[2][7];

  fx[0] = -( stress_xx * pfx0 );
  fx[1] = -( stress_xx * pfx1 );
  fx[2] = -( stress_xx * pfx2 );
  fx[3] = -( stress_xx * pfx3 );
  fx[4] = -( stress_xx * pfx4 );
  fx[5] = -( stress_xx * pfx5 );
  fx[6] = -( stress_xx * pfx6 );
  fx[7] = -( stress_xx * pfx7 );

  fy[0] = -( stress_yy * pfy0  );
  fy[1] = -( stress_yy * pfy1  );
  fy[2] = -( stress_yy * pfy2  );
  fy[3] = -( stress_yy * pfy3  );
  fy[4] = -( stress_yy * pfy4  );
  fy[5] = -( stress_yy * pfy5  );
  fy[6] = -( stress_yy * pfy6  );
  fy[7] = -( stress_yy * pfy7  );

  fz[0] = -( stress_zz * pfz0 );
  fz[1] = -( stress_zz * pfz1 );
  fz[2] = -( stress_zz * pfz2 );
  fz[3] = -( stress_zz * pfz3 );
  fz[4] = -( stress_zz * pfz4 );
  fz[5] = -( stress_zz * pfz5 );
  fz[6] = -( stress_zz * pfz6 );
  fz[7] = -( stress_zz * pfz7 );
}

void GatherNodes(Index_t *elemNodes, Real_t *x, Real_t *y, Real_t *z,
                 Real_t x_local[8], Real_t y_local[8], Real_t z_local[8])
{
  for(Index_t lnode=0 ; lnode<8 ; ++lnode )
  {
    Index_t gnode = elemNodes[lnode];
    x_local[lnode] = x[gnode];
    y_local[lnode] = y[gnode];
    z_local[lnode] = z[gnode];
  }
}

void SumForce(Index_t *elemNodes, Real_t *fx, Real_t *fy, Real_t *fz,
              Real_t fx_local[8], Real_t fy_local[8], Real_t fz_local[8])
{
  for(Index_t lnode=0 ; lnode<8 ; ++lnode )
  {
    Index_t gnode = elemNodes[lnode];
    fx[gnode] += fx_local[lnode];
    fy[gnode] += fy_local[lnode];
    fz[gnode] += fz_local[lnode];
  }
}

void IntegrateStressForElems(Index_t *nodelist,
                             Real_t *x,  Real_t *y,  Real_t *z,
                             Real_t *fx, Real_t *fy, Real_t *fz,
                             Real_t *sigxx, Real_t *sigyy, Real_t *sigzz,
                             Real_t *determ, Index_t numElem)
{
  Index_t lnode, gnode, gnode2;
  Index_t *elemNodes;

  // loop over all elements
  for (Index_t k=0; k<numElem; ++k) {
    Real_t  B[3][8];// shape function derivatives

    elemNodes = &nodelist[8*k];

    {
      Real_t  x_local[8];
      Real_t  y_local[8];
      Real_t  z_local[8];

      // get coordinates from global arrays and copy into local arrays.
      GatherNodes(elemNodes, x, y, z, x_local, y_local, z_local);

      /* Volume calculation involves extra work for numerical consistency. */
      CalcElemShapeFunctionDerivatives(x_local, y_local, z_local,
                                       B, &determ[k]);

      CalcElemNodeNormals( B[0] , B[1], B[2],
                           x_local, y_local, z_local );
    }

    {
      Real_t  fx_local[8];
      Real_t  fy_local[8];
      Real_t  fz_local[8];

      SumElemStressesToNodeForces( B, sigxx[k], sigyy[k], sigzz[k],
                                   fx_local, fy_local, fz_local );

      // sum nodal force contributions to global force arrray.
      SumForce(elemNodes, fx, fy, fz, fx_local, fy_local, fz_local);
    }
  }
}

void CollectDomainNodesToElemNodes(Real_t *x, Real_t *y, Real_t *z,
                                   Index_t *elemToNode,
                                   Real_t *elemX, Real_t *elemY, Real_t *elemZ)
{
   Index_t nd0i := elemToNode[0];
   Index_t nd1i := elemToNode[1];
   Index_t nd2i := elemToNode[2];
   Index_t nd3i := elemToNode[3];
   Index_t nd4i := elemToNode[4];
   Index_t nd5i := elemToNode[5];
   Index_t nd6i := elemToNode[6];
   Index_t nd7i := elemToNode[7];

   elemX[0] = x[nd0i];
   elemX[1] = x[nd1i];
   elemX[2] = x[nd2i];
   elemX[3] = x[nd3i];
   elemX[4] = x[nd4i];
   elemX[5] = x[nd5i];
   elemX[6] = x[nd6i];
   elemX[7] = x[nd7i];

   elemY[0] = y[nd0i];
   elemY[1] = y[nd1i];
   elemY[2] = y[nd2i];
   elemY[3] = y[nd3i];
   elemY[4] = y[nd4i];
   elemY[5] = y[nd5i];
   elemY[6] = y[nd6i];
   elemY[7] = y[nd7i];

   elemZ[0] = z[nd0i];
   elemZ[1] = z[nd1i];
   elemZ[2] = z[nd2i];
   elemZ[3] = z[nd3i];
   elemZ[4] = z[nd4i];
   elemZ[5] = z[nd5i];
   elemZ[6] = z[nd6i];
   elemZ[7] = z[nd7i];

}

#define twelfth 0.0833333333f

inline void VoluDer(Real_t x0,  Real_t x1,  Real_t x2,
             Real_t x3,  Real_t x4,  Real_t x5,
             Real_t y0,  Real_t y1,  Real_t y2,
             Real_t y3,  Real_t y4,  Real_t y5,
             Real_t z0,  Real_t z1,  Real_t z2,
             Real_t z3,  Real_t z4,  Real_t z5,
             Real_t *dvdx, Real_t *dvdy, Real_t *dvdz)
{
    // Real_t twelfth = ONE / 12.0f;

   *dvdx =
     ((y1 + y2) * (z0 + z1) - (y0 + y1) * (z1 + z2) +
      (y0 + y4) * (z3 + z4) - (y3 + y4) * (z0 + z4) -
      (y2 + y5) * (z3 + z5) + (y3 + y5) * (z2 + z5)) * twelfth;
   *dvdy =
     (- (x1 + x2) * (z0 + z1) + (x0 + x1) * (z1 + z2) -
      (x0 + x4) * (z3 + z4) + (x3 + x4) * (z0 + z4) +
      (x2 + x5) * (z3 + z5) - (x3 + x5) * (z2 + z5)) * twelfth;

   *dvdz =
     (- (y1 + y2) * (x0 + x1) + (y0 + y1) * (x1 + x2) -
      (y0 + y4) * (x3 + x4) + (y3 + y4) * (x0 + x4) +
      (y2 + y5) * (x3 + x5) - (y3 + y5) * (x2 + x5)) * twelfth;

/*
   *dvdx *= twelfth;
   *dvdy *= twelfth;
   *dvdz *= twelfth;
*/

}

void CalcElemVolumeDerivative(Real_t *dvdx, Real_t *dvdy, Real_t *dvdz,
                              Real_t *x, Real_t *y, Real_t *z)
{
   VoluDer(x[1], x[2], x[3], x[4], x[5], x[7],
           y[1], y[2], y[3], y[4], y[5], y[7],
           z[1], z[2], z[3], z[4], z[5], z[7],
           &dvdx[0], &dvdy[0], &dvdz[0]);
   VoluDer(x[0], x[1], x[2], x[7], x[4], x[6],
           y[0], y[1], y[2], y[7], y[4], y[6],
           z[0], z[1], z[2], z[7], z[4], z[6],
           &dvdx[3], &dvdy[3], &dvdz[3]);
   VoluDer(x[3], x[0], x[1], x[6], x[7], x[5],
           y[3], y[0], y[1], y[6], y[7], y[5],
           z[3], z[0], z[1], z[6], z[7], z[5],
           &dvdx[2], &dvdy[2], &dvdz[2]);
   VoluDer(x[2], x[3], x[0], x[5], x[6], x[4],
           y[2], y[3], y[0], y[5], y[6], y[4],
           z[2], z[3], z[0], z[5], z[6], z[4],
           &dvdx[1], &dvdy[1], &dvdz[1]);
   VoluDer(x[7], x[6], x[5], x[0], x[3], x[1],
           y[7], y[6], y[5], y[0], y[3], y[1],
           z[7], z[6], z[5], z[0], z[3], z[1],
           &dvdx[4], &dvdy[4], &dvdz[4]);
   VoluDer(x[4], x[7], x[6], x[1], x[0], x[2],
           y[4], y[7], y[6], y[1], y[0], y[2],
           z[4], z[7], z[6], z[1], z[0], z[2],
           &dvdx[5], &dvdy[5], &dvdz[5]);
   VoluDer(x[5], x[4], x[7], x[2], x[1], x[3],
           y[5], y[4], y[7], y[2], y[1], y[3],
           z[5], z[4], z[7], z[2], z[1], z[3],
           &dvdx[6], &dvdy[6], &dvdz[6]);
   VoluDer(x[6], x[5], x[4], x[3], x[2], x[0],
           y[6], y[5], y[4], y[3], y[2], y[0],
           z[6], z[5], z[4], z[3], z[2], z[0],
           &dvdx[7], &dvdy[7], &dvdz[7]);
}

void CalcElemFBHourglassForce(Real_t *xd, Real_t *yd, Real_t *zd,
                              Real_t hourgam[][8], Real_t coefficient,
                              Real_t *hgfx, Real_t *hgfy, Real_t *hgfz)
{
   // enum { i00, i01, i02, i03 };

   Index_t i00 := 0;
   Index_t i01 := 1;
   Index_t i02 := 2;
   Index_t i03 := 3;

   Real_t h00 =
      hourgam[i00][0] * xd[0] + hourgam[i00][1] * xd[1] +
      hourgam[i00][2] * xd[2] + hourgam[i00][3] * xd[3] +
      hourgam[i00][4] * xd[4] + hourgam[i00][5] * xd[5] +
      hourgam[i00][6] * xd[6] + hourgam[i00][7] * xd[7];

   Real_t h01 =
      hourgam[i01][0] * xd[0] + hourgam[i01][1] * xd[1] +
      hourgam[i01][2] * xd[2] + hourgam[i01][3] * xd[3] +
      hourgam[i01][4] * xd[4] + hourgam[i01][5] * xd[5] +
      hourgam[i01][6] * xd[6] + hourgam[i01][7] * xd[7];

   Real_t h02 =
      hourgam[i02][0] * xd[0] + hourgam[i02][1] * xd[1] +
      hourgam[i02][2] * xd[2] + hourgam[i02][3] * xd[3] +
      hourgam[i02][4] * xd[4] + hourgam[i02][5] * xd[5] +
      hourgam[i02][6] * xd[6] + hourgam[i02][7] * xd[7];

   Real_t h03 =
      hourgam[i03][0] * xd[0] + hourgam[i03][1] * xd[1] +
      hourgam[i03][2] * xd[2] + hourgam[i03][3] * xd[3] +
      hourgam[i03][4] * xd[4] + hourgam[i03][5] * xd[5] +
      hourgam[i03][6] * xd[6] + hourgam[i03][7] * xd[7];

   hgfx[0] = coefficient *
      (hourgam[i00][0] * h00 + hourgam[i01][0] * h01 +
       hourgam[i02][0] * h02 + hourgam[i03][0] * h03);

   hgfx[1] = coefficient *
      (hourgam[i00][1] * h00 + hourgam[i01][1] * h01 +
       hourgam[i02][1] * h02 + hourgam[i03][1] * h03);

   hgfx[2] = coefficient *
      (hourgam[i00][2] * h00 + hourgam[i01][2] * h01 +
       hourgam[i02][2] * h02 + hourgam[i03][2] * h03);

   hgfx[3] = coefficient *
      (hourgam[i00][3] * h00 + hourgam[i01][3] * h01 +
       hourgam[i02][3] * h02 + hourgam[i03][3] * h03);

   hgfx[4] = coefficient *
      (hourgam[i00][4] * h00 + hourgam[i01][4] * h01 +
       hourgam[i02][4] * h02 + hourgam[i03][4] * h03);

   hgfx[5] = coefficient *
      (hourgam[i00][5] * h00 + hourgam[i01][5] * h01 +
       hourgam[i02][5] * h02 + hourgam[i03][5] * h03);

   hgfx[6] = coefficient *
      (hourgam[i00][6] * h00 + hourgam[i01][6] * h01 +
       hourgam[i02][6] * h02 + hourgam[i03][6] * h03);

   hgfx[7] = coefficient *
      (hourgam[i00][7] * h00 + hourgam[i01][7] * h01 +
       hourgam[i02][7] * h02 + hourgam[i03][7] * h03);

   h00 =
      hourgam[i00][0] * yd[0] + hourgam[i00][1] * yd[1] +
      hourgam[i00][2] * yd[2] + hourgam[i00][3] * yd[3] +
      hourgam[i00][4] * yd[4] + hourgam[i00][5] * yd[5] +
      hourgam[i00][6] * yd[6] + hourgam[i00][7] * yd[7];

   h01 =
      hourgam[i01][0] * yd[0] + hourgam[i01][1] * yd[1] +
      hourgam[i01][2] * yd[2] + hourgam[i01][3] * yd[3] +
      hourgam[i01][4] * yd[4] + hourgam[i01][5] * yd[5] +
      hourgam[i01][6] * yd[6] + hourgam[i01][7] * yd[7];

   h02 =
      hourgam[i02][0] * yd[0] + hourgam[i02][1] * yd[1] +
      hourgam[i02][2] * yd[2] + hourgam[i02][3] * yd[3] +
      hourgam[i02][4] * yd[4] + hourgam[i02][5] * yd[5] +
      hourgam[i02][6] * yd[6] + hourgam[i02][7] * yd[7];

   h03 =
      hourgam[i03][0] * yd[0] + hourgam[i03][1] * yd[1] +
      hourgam[i03][2] * yd[2] + hourgam[i03][3] * yd[3] +
      hourgam[i03][4] * yd[4] + hourgam[i03][5] * yd[5] +
      hourgam[i03][6] * yd[6] + hourgam[i03][7] * yd[7];

   hgfy[0] = coefficient *
      (hourgam[i00][0] * h00 + hourgam[i01][0] * h01 +
       hourgam[i02][0] * h02 + hourgam[i03][0] * h03);

   hgfy[1] = coefficient *
      (hourgam[i00][1] * h00 + hourgam[i01][1] * h01 +
       hourgam[i02][1] * h02 + hourgam[i03][1] * h03);

   hgfy[2] = coefficient *
      (hourgam[i00][2] * h00 + hourgam[i01][2] * h01 +
       hourgam[i02][2] * h02 + hourgam[i03][2] * h03);

   hgfy[3] = coefficient *
      (hourgam[i00][3] * h00 + hourgam[i01][3] * h01 +
       hourgam[i02][3] * h02 + hourgam[i03][3] * h03);

   hgfy[4] = coefficient *
      (hourgam[i00][4] * h00 + hourgam[i01][4] * h01 +
       hourgam[i02][4] * h02 + hourgam[i03][4] * h03);

   hgfy[5] = coefficient *
      (hourgam[i00][5] * h00 + hourgam[i01][5] * h01 +
       hourgam[i02][5] * h02 + hourgam[i03][5] * h03);

   hgfy[6] = coefficient *
      (hourgam[i00][6] * h00 + hourgam[i01][6] * h01 +
       hourgam[i02][6] * h02 + hourgam[i03][6] * h03);

   hgfy[7] = coefficient *
      (hourgam[i00][7] * h00 + hourgam[i01][7] * h01 +
       hourgam[i02][7] * h02 + hourgam[i03][7] * h03);

   h00 =
      hourgam[i00][0] * zd[0] + hourgam[i00][1] * zd[1] +
      hourgam[i00][2] * zd[2] + hourgam[i00][3] * zd[3] +
      hourgam[i00][4] * zd[4] + hourgam[i00][5] * zd[5] +
      hourgam[i00][6] * zd[6] + hourgam[i00][7] * zd[7];

   h01 =
      hourgam[i01][0] * zd[0] + hourgam[i01][1] * zd[1] +
      hourgam[i01][2] * zd[2] + hourgam[i01][3] * zd[3] +
      hourgam[i01][4] * zd[4] + hourgam[i01][5] * zd[5] +
      hourgam[i01][6] * zd[6] + hourgam[i01][7] * zd[7];

   h02 =
      hourgam[i02][0] * zd[0] + hourgam[i02][1] * zd[1] +
      hourgam[i02][2] * zd[2] + hourgam[i02][3] * zd[3] +
      hourgam[i02][4] * zd[4] + hourgam[i02][5] * zd[5] +
      hourgam[i02][6] * zd[6] + hourgam[i02][7] * zd[7];

   h03 =
      hourgam[i03][0] * zd[0] + hourgam[i03][1] * zd[1] +
      hourgam[i03][2] * zd[2] + hourgam[i03][3] * zd[3] +
      hourgam[i03][4] * zd[4] + hourgam[i03][5] * zd[5] +
      hourgam[i03][6] * zd[6] + hourgam[i03][7] * zd[7];

   hgfz[0] = coefficient *
      (hourgam[i00][0] * h00 + hourgam[i01][0] * h01 +
       hourgam[i02][0] * h02 + hourgam[i03][0] * h03);

   hgfz[1] = coefficient *
      (hourgam[i00][1] * h00 + hourgam[i01][1] * h01 +
       hourgam[i02][1] * h02 + hourgam[i03][1] * h03);

   hgfz[2] = coefficient *
      (hourgam[i00][2] * h00 + hourgam[i01][2] * h01 +
       hourgam[i02][2] * h02 + hourgam[i03][2] * h03);

   hgfz[3] = coefficient *
      (hourgam[i00][3] * h00 + hourgam[i01][3] * h01 +
       hourgam[i02][3] * h02 + hourgam[i03][3] * h03);

   hgfz[4] = coefficient *
      (hourgam[i00][4] * h00 + hourgam[i01][4] * h01 +
       hourgam[i02][4] * h02 + hourgam[i03][4] * h03);

   hgfz[5] = coefficient *
      (hourgam[i00][5] * h00 + hourgam[i01][5] * h01 +
       hourgam[i02][5] * h02 + hourgam[i03][5] * h03);

   hgfz[6] = coefficient *
      (hourgam[i00][6] * h00 + hourgam[i01][6] * h01 +
       hourgam[i02][6] * h02 + hourgam[i03][6] * h03);

   hgfz[7] = coefficient *
      (hourgam[i00][7] * h00 + hourgam[i01][7] * h01 +
       hourgam[i02][7] * h02 + hourgam[i03][7] * h03);
}

Real_t gammaa[4][8] =
{
   {  ONE,  ONE, -ONE, -ONE, -ONE, -ONE, ONE,  ONE },
   {  ONE, -ONE, -ONE,  ONE, -ONE,  ONE, ONE, -ONE },
   {  ONE, -ONE,  ONE, -ONE,  ONE, -ONE, ONE, -ONE },
   { -ONE,  ONE, -ONE,  ONE,  ONE, -ONE, ONE, -ONE }
} ;

void FBKernel(Real_t *x8ni, Real_t *y8ni, Real_t *z8ni,
              Real_t *dvdxi, Real_t *dvdyi, Real_t *dvdzi,
              Real_t hourgam[][8], Real_t volinv)
{
   for(Index_t i1=0;i1<4;++i1){
      Real_t *gami = &gammaa[i1][0];
      Real_t *hg = &hourgam[i1][0];

      Real_t hourmodx =
         x8ni[0] * gami[0] + x8ni[1] * gami[1] +
         x8ni[2] * gami[2] + x8ni[3] * gami[3] +
         x8ni[4] * gami[4] + x8ni[5] * gami[5] +
         x8ni[6] * gami[6] + x8ni[7] * gami[7];

      Real_t hourmody =
         y8ni[0] * gami[0] + y8ni[1] * gami[1] +
         y8ni[2] * gami[2] + y8ni[3] * gami[3] +
         y8ni[4] * gami[4] + y8ni[5] * gami[5] +
         y8ni[6] * gami[6] + y8ni[7] * gami[7];

      Real_t hourmodz =
         z8ni[0] * gami[0] + z8ni[1] * gami[1] +
         z8ni[2] * gami[2] + z8ni[3] * gami[3] +
         z8ni[4] * gami[4] + z8ni[5] * gami[5] +
         z8ni[6] * gami[6] + z8ni[7] * gami[7];

      hg[0] = gami[0] -  volinv*(dvdxi[0] * hourmodx +
                                 dvdyi[0] * hourmody +
                                 dvdzi[0] * hourmodz );

      hg[1] = gami[1] -  volinv*(dvdxi[1] * hourmodx +
                                 dvdyi[1] * hourmody +
                                 dvdzi[1] * hourmodz );

      hg[2] = gami[2] -  volinv*(dvdxi[2] * hourmodx +
                                 dvdyi[2] * hourmody +
                                 dvdzi[2] * hourmodz );

      hg[3] = gami[3] -  volinv*(dvdxi[3] * hourmodx +
                                 dvdyi[3] * hourmody +
                                 dvdzi[3] * hourmodz );

      hg[4] = gami[4] -  volinv*(dvdxi[4] * hourmodx +
                                 dvdyi[4] * hourmody +
                                 dvdzi[4] * hourmodz );

      hg[5] = gami[5] -  volinv*(dvdxi[5] * hourmodx +
                                 dvdyi[5] * hourmody +
                                 dvdzi[5] * hourmodz );

      hg[6] = gami[6] -  volinv*(dvdxi[6] * hourmodx +
                                 dvdyi[6] * hourmody +
                                 dvdzi[6] * hourmodz );

      hg[7] = gami[7] -  volinv*(dvdxi[7] * hourmodx +
                                 dvdyi[7] * hourmody +
                                 dvdzi[7] * hourmodz );
   }
}

void CalcFBHourglassForceForElems(Index_t *nodelist,
                                  Real_t *ss, Real_t *elemMass,
                                  Real_t *xd, Real_t *yd, Real_t *zd,
                                  Real_t *fx, Real_t *fy, Real_t *fz,
                                  Real_t *determ,
                                  Real_t *x8n, Real_t *y8n, Real_t *z8n,
                                  Real_t *dvdx, Real_t *dvdy, Real_t *dvdz,
                                  Real_t hourg, Index_t numElem)
{
   /*************************************************
    *
    *     FUNCTION: Calculates the Flanagan-Belytschko anti-hourglass
    *               force.
    *
    *************************************************/

/*************************************************/
/*    compute the hourglass modes */

   for (Index_t i2=0; i2<numElem; ++i2) {
      Index_t i3=8*i2;
      Index_t *elemToNode = &nodelist[i3];
      Real_t hourgam[4][8];

      FBKernel( &x8n[i3],  &y8n[i3],  &z8n[i3],
               &dvdx[i3], &dvdy[i3], &dvdz[i3],
               hourgam, ONE/determ[i2]);

      /* compute forces */
      /* store forces into h arrays (force arrays) */

      Real_t ss1=ss[i2];
      Real_t mass1=elemMass[i2];
      Real_t volume13 = cbrtf(determ[i2]);

      Real_t xd1[8], yd1[8], zd1[8];
      Real_t hgfx[8], hgfy[8], hgfz[8];
      Real_t coefficient = - hourg * 0.01f * ss1 * mass1 / volume13;

      GatherNodes(elemToNode, xd, yd, zd, xd1, yd1, zd1);

      CalcElemFBHourglassForce(xd1, yd1, zd1, hourgam,
                               coefficient, hgfx, hgfy, hgfz);

      SumForce(elemToNode, fx, fy, fz, hgfx, hgfy, hgfz);
   }
}

void CopyBlock(Real_t *dst1, Real_t *dst2, Real_t *dst3,
               Real_t *src1, Real_t *src2, Real_t *src3)
{
  for (Index_t i=0; i<8; ++i) {
    dst1[i] = src1[i];
    dst2[i] = src2[i];
    dst3[i] = src3[i];
  }
}

void CalcHourglassControlForElems(Domain *domain,
                                  Real_t *determ, Real_t hgcoef)
{
   Index_t numElem = domain->numElem;
   Real_t *dvdx = dvdx_;
   Real_t *dvdy = dvdy_;
   Real_t *dvdz = dvdz_;
   Real_t *x8n  = x8n_;
   Real_t *y8n  = y8n_;
   Real_t *z8n  = z8n_;

   /* start loop over elements */
   for (Index_t idx=0; idx<numElem; ++idx) {
      Index_t baseIdx = 8*idx;
      Real_t  x1[8],  y1[8],  z1[8];
      Real_t pfx[8], pfy[8], pfz[8];

      Index_t *elemToNode = &domain->nodelist[baseIdx];
      CollectDomainNodesToElemNodes(domain->x, domain->y, domain->z,
                                    elemToNode, x1, y1, z1);

      CalcElemVolumeDerivative(pfx, pfy, pfz, x1, y1, z1);

      /* load into temporary storage for FB Hour Glass control */
      CopyBlock(&dvdx[baseIdx], &dvdy[baseIdx], &dvdz[baseIdx], pfx, pfy, pfz);
      CopyBlock(&x8n[baseIdx], &y8n[baseIdx], &z8n[baseIdx], x1, y1, z1);

      determ[idx] = domain->volo[idx] * domain->v[idx];

      /* Do a check for negative volumes */
      if ( domain->v[idx] <= ZERO ) {
         exit(VolumeError);
      }
   }

   if ( hgcoef > ZERO ) {
      CalcFBHourglassForceForElems( domain->nodelist,
                                    domain->ss, domain->elemMass,
                                    domain->xd, domain->yd, domain->zd,
                                    domain->fx, domain->fy, domain->fz,
                                    determ, x8n, y8n, z8n, dvdx, dvdy, dvdz,
                                    hgcoef, numElem);
   }

   // Release((void **) &z8n);
   // Release((void **) &y8n);
   // Release((void **) &x8n);
   // Release((void **) &dvdz);
   // Release((void **) &dvdy);
   // Release((void **) &dvdx);

   return ;
}

int VolErr1(Real_t *determ, Index_t numElem)
{
  for (Index_t k=0; k<numElem; ++k) {
    if (determ[k] <= ZERO) return 1;
  }
  return 0;
}

void CalcVolumeForceForElems(Domain *domain)
{
   Index_t numElem = domain->numElem;
   if (numElem != 0) {
      Real_t  hgcoef = domain->hgcoef;
      Real_t *sigxx  = sigxx_;
      Real_t *sigyy  = sigyy_;
      Real_t *sigzz  = sigzz_;
      Real_t *determ = determ_;

      /* Sum contributions to total stress tensor */
      InitStressTermsForElems(domain->p, domain->q,
                              sigxx, sigyy, sigzz, numElem);

      // call elemlib stress integration loop to produce nodal forces from
      // material stresses.
      IntegrateStressForElems( domain->nodelist,
                               domain->x, domain->y, domain->z,
                               domain->fx, domain->fy, domain->fz,
                               sigxx, sigyy, sigzz, determ, numElem);

      // check for negative element volume
      if (VolErr1(determ, numElem))
         exit(VolumeError);

      CalcHourglassControlForElems(domain, determ, hgcoef);

      // Release((void **) &determ);
      // Release((void **) &sigzz);
      // Release((void **) &sigyy);
      // Release((void **) &sigxx);
   }
}

void CalcForceForNodes(Domain *domain)
{
  Index_t numNode = domain->numNode;
  Real_t *fx = domain->fx;
  Real_t *fy = domain->fy;
  Real_t *fz = domain->fz;

  for (Index_t i=0; i<numNode; ++i) {
     fx[i] = ZERO;
     fy[i] = ZERO;
     fz[i] = ZERO;
  }
}

void CalcAccelerationForNodes(Real_t *xdd, Real_t *ydd, Real_t *zdd,
                              Real_t *fx, Real_t *fy, Real_t *fz,
                              Real_t *nodalMass, Index_t numNode)
{
   for (Index_t i=0; i<numNode; ++i) {
      xdd[i] = fx[i] / nodalMass[i];
      ydd[i] = fy[i] / nodalMass[i];
      zdd[i] = fz[i] / nodalMass[i];
   }
}

void ApplyAccelerationBoundaryConditionsForNodes(Real_t *xdd, Real_t *ydd,
                                                 Real_t *zdd, Index_t *symmX,
                                                 Index_t *symmY,
                                                 Index_t *symmZ, Index_t size)
{
  Index_t numNodeBC = (size+1)*(size+1);

  for (Index_t i=0; i<numNodeBC; ++i) {
     xdd[symmX[i]] = ZERO;
     ydd[symmY[i]] = ZERO;
     zdd[symmZ[i]] = ZERO;
  }
}

void CalcVelocityForNodes(Real_t *xd,  Real_t *yd,  Real_t *zd,
                          Real_t *xdd, Real_t *ydd, Real_t *zdd,
                          Real_t dt,  Real_t u_cut, Index_t numNode)
{
   for (Index_t i=0; i<numNode; ++i) {
     Real_t xt, yt, zt;
     Real_t xdtmp, ydtmp, zdtmp;

     xdtmp = xt = xd[i] + xdd[i] * dt;
     if ( fabsf(xt) < u_cut ) xdtmp = ZERO;
     xd[i] = xdtmp;

     ydtmp = yt = yd[i] + ydd[i] * dt;
     if( fabsf(yt) < u_cut ) ydtmp = ZERO;
     yd[i] = ydtmp;

     zdtmp = zt = zd[i] + zdd[i] * dt;
     if( fabsf(zt) < u_cut ) zdtmp = ZERO;
     zd[i] = zdtmp;
   }
}

void CalcPositionForNodes(Real_t *x,  Real_t *y,  Real_t *z,
                          Real_t *xd, Real_t *yd, Real_t *zd,
                          Real_t dt, Index_t numNode)
{
   for (Index_t i=0; i<numNode; ++i) {
     x[i] += xd[i] * dt;
     y[i] += yd[i] * dt;
     z[i] += zd[i] * dt;
   }
}

void LagrangeNodal(Domain *domain)
{
   Real_t delt = domain->deltatime;
  Real_t u_cut = domain->u_cut;

  /* time of boundary condition evaluation is beginning of step for force and
   * acceleration boundary conditions. */
  CalcForceForNodes(domain);

  /* Calcforce calls partial, force, hourq */
  CalcVolumeForceForElems(domain);

  /* Calculate Nodal Forces at domain boundaries */
  /* problem->commSBN->Transfer(CommSBN::forces); */

  CalcAccelerationForNodes(domain->xdd, domain->ydd, domain->zdd,
                           domain->fx, domain->fy, domain->fz,
                           domain->nodalMass, domain->numNode);

  ApplyAccelerationBoundaryConditionsForNodes(domain->xdd, domain->ydd,
                                              domain->zdd, domain->symmX,
                                              domain->symmY, domain->symmZ,
                                              domain->sizeX);

  CalcVelocityForNodes( domain->xd,  domain->yd,  domain->zd,
                        domain->xdd, domain->ydd, domain->zdd,
                        delt, u_cut, domain->numNode);

  CalcPositionForNodes( domain->x,  domain->y,  domain->z,
                        domain->xd, domain->yd, domain->zd,
                        delt, domain->numNode );

  return;
}

inline void TRIPLE_PRODUCT(Real_t x1_, Real_t y1_, Real_t z1_,
                     Real_t x2_, Real_t y2_, Real_t z2_,
                     Real_t x3_, Real_t y3_, Real_t z3_, Real_t *pv_)
{
   *pv_ += (x1_*(y2_*z3_ - z2_*y3_) +
            x2_*(z1_*y3_ - y1_*z3_) +
            x3_*(y1_*z2_ - z1_*y2_));
}


inline void CalcElemVolume2(Real_t x0, Real_t x1, Real_t x2,  Real_t x3,
                     Real_t x4,  Real_t x5, Real_t x6,  Real_t x7,
                     Real_t y0,  Real_t y1, Real_t y2,  Real_t y3,
                     Real_t y4,  Real_t y5, Real_t y6,  Real_t y7,
                     Real_t z0,  Real_t z1, Real_t z2,  Real_t z3,
                     Real_t z4,  Real_t z5, Real_t z6,  Real_t z7, Real_t *fv)
{
  // Real_t twelfth = ONE / 12.0f;

  *fv = ZERO;
  {
     Real_t dx31 = x3 - x1;
     Real_t dy31 = y3 - y1;
     Real_t dz31 = z3 - z1;

     Real_t dx72 = x7 - x2;
     Real_t dy72 = y7 - y2;
     Real_t dz72 = z7 - z2;

     Real_t s1 = dx31 + dx72;
     Real_t s2 = dy31 + dy72;
     Real_t s3 = dz31 + dz72;

     Real_t dx63 = x6 - x3;
     Real_t dy63 = y6 - y3;
     Real_t dz63 = z6 - z3;

     Real_t dx20 = x2 - x0;
     Real_t dy20 = y2 - y0;
     Real_t dz20 = z2 - z0;

     TRIPLE_PRODUCT(s1, dx63, dx20,
                    s2, dy63, dy20,
                    s3, dz63, dz20, fv);
  }
  {
     Real_t dx43 = x4 - x3;
     Real_t dy43 = y4 - y3;
     Real_t dz43 = z4 - z3;

     Real_t dx57 = x5 - x7;
     Real_t dy57 = y5 - y7;
     Real_t dz57 = z5 - z7;

     Real_t s1 = dx43 + dx57;
     Real_t s2 = dy43 + dy57;
     Real_t s3 = dz43 + dz57;

     Real_t dx64 = x6 - x4;
     Real_t dy64 = y6 - y4;
     Real_t dz64 = z6 - z4;

     Real_t dx70 = x7 - x0;
     Real_t dy70 = y7 - y0;
     Real_t dz70 = z7 - z0;

     TRIPLE_PRODUCT(s1, dx64, dx70,
                    s2, dy64, dy70,
                    s3, dz64, dz70, fv);
  }
  {
     Real_t dx14 = x1 - x4;
     Real_t dy14 = y1 - y4;
     Real_t dz14 = z1 - z4;

     Real_t dx25 = x2 - x5;
     Real_t dy25 = y2 - y5;
     Real_t dz25 = z2 - z5;

     Real_t s1 = dx14 + dx25;
     Real_t s2 = dy14 + dy25;
     Real_t s3 = dz14 + dz25;

     Real_t dx61 = x6 - x1;
     Real_t dy61 = y6 - y1;
     Real_t dz61 = z6 - z1;

     Real_t dx50 = x5 - x0;
     Real_t dy50 = y5 - y0;
     Real_t dz50 = z5 - z0;

     TRIPLE_PRODUCT(s1, dx61, dx50,
                    s2, dy61, dy50,
                    s3, dz61, dz50, fv);
   }

  // volume *= twelfth;

  // return volume * twelfth;

  *fv *= twelfth;
}

Real_t CalcElemVolume(Real_t *x, Real_t *y, Real_t *z)
{
   Real_t fv;
   CalcElemVolume2( x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7],
                    y[0], y[1], y[2], y[3], y[4], y[5], y[6], y[7],
                    z[0], z[1], z[2], z[3], z[4], z[5], z[6], z[7], &fv);
   return fv;
}

inline void AreaFace(Real_t x0, Real_t x1, Real_t x2, Real_t x3,
               Real_t y0, Real_t y1, Real_t y2, Real_t y3,
               Real_t z0, Real_t z1, Real_t z2, Real_t z3, Real_t *area)
{
   Real_t dx1 = (x2 - x0);
   Real_t dx2 = (x3 - x1);
   Real_t dy1 = (y2 - y0);
   Real_t dy2 = (y3 - y1);
   Real_t dz1 = (z2 - z0);
   Real_t dz2 = (z3 - z1);
   Real_t fx = dx1 - dx2;
   Real_t fy = dy1 - dy2;
   Real_t fz = dz1 - dz2;
   Real_t gx = dx1 + dx2;
   Real_t gy = dy1 + dy2;
   Real_t gz = dz1 + dz2;
   Real_t term1 = fx * fx;
   term1 += fy * fy;
   term1 += fz * fz;
   Real_t term2 = gx * gx;
   term2 += gy * gy;
   term2 += gz * gz;
   Real_t term3 = fx * gx;
   term3 +=  fy * gy;
   term3 +=  fz * gz;
   Real_t term4 = term1 * term2 - term3 * term3;
   *area = term4;
}

Real_t CalcElemCharacteristicLength(Real_t x[8], Real_t y[8], Real_t z[8],
                                   Real_t volume)
{
   Real_t a;
   Real_t charLength = ZERO;

   AreaFace(x[0],x[1],x[2],x[3],
            y[0],y[1],y[2],y[3],
            z[0],z[1],z[2],z[3], &a);
   if (a > charLength) charLength = a;

   AreaFace(x[4],x[5],x[6],x[7],
            y[4],y[5],y[6],y[7],
            z[4],z[5],z[6],z[7], &a);
   if (a > charLength) charLength = a;

   AreaFace(x[0],x[1],x[5],x[4],
            y[0],y[1],y[5],y[4],
            z[0],z[1],z[5],z[4], &a);
   if (a > charLength) charLength = a;

   AreaFace(x[1],x[2],x[6],x[5],
            y[1],y[2],y[6],y[5],
            z[1],z[2],z[6],z[5], &a);
   if (a > charLength) charLength = a;

   AreaFace(x[2],x[3],x[7],x[6],
            y[2],y[3],y[7],y[6],
            z[2],z[3],z[7],z[6], &a);
   if (a > charLength) charLength = a;

   AreaFace(x[3],x[0],x[4],x[7],
            y[3],y[0],y[4],y[7],
            z[3],z[0],z[4],z[7], &a);
   if (a > charLength) charLength = a;

   charLength = (4.0f * volume) / sqrtf( charLength );

   return charLength;
}

void CalcElemVelocityGradient(Real_t *xvel, Real_t *yvel, Real_t *zvel,
                               Real_t b[][8], Real_t detJ, Real_t *d)
{
   Real_t inv_detJ = ONE / detJ;
   Real_t dyddx, dxddy, dzddx, dxddz, dzddy, dyddz;
   Real_t *pfx = &b[0][0];
   Real_t *pfy = &b[1][0];
   Real_t *pfz = &b[2][0];

   Real_t  dxv0 = (xvel[0]-xvel[6]);
   Real_t  dxv1 = (xvel[1]-xvel[7]);
   Real_t  dxv2 = (xvel[2]-xvel[4]);
   Real_t  dxv3 = (xvel[3]-xvel[5]);
   Real_t  dyv0 = (yvel[0]-yvel[6]);
   Real_t  dyv1 = (yvel[1]-yvel[7]);
   Real_t  dyv2 = (yvel[2]-yvel[4]);
   Real_t  dyv3 = (yvel[3]-yvel[5]);
   Real_t  dzv0 = (zvel[0]-zvel[6]);
   Real_t  dzv1 = (zvel[1]-zvel[7]);
   Real_t  dzv2 = (zvel[2]-zvel[4]);
   Real_t  dzv3 = (zvel[3]-zvel[5]);

  d[0] = inv_detJ * ( pfx[0] * dxv0
                     + pfx[1] * dxv1
                     + pfx[2] * dxv2
                     + pfx[3] * dxv3 );

  d[1] = inv_detJ * ( pfy[0] * dyv0
                     + pfy[1] * dyv1
                     + pfy[2] * dyv2
                     + pfy[3] * dyv3 );

  d[2] = inv_detJ * ( pfz[0] * dzv0
                     + pfz[1] * dzv1
                     + pfz[2] * dzv2
                     + pfz[3] * dzv3 );

  dyddx  = inv_detJ * ( pfx[0] * dyv0
                      + pfx[1] * dyv1
                      + pfx[2] * dyv2
                      + pfx[3] * dyv3 );

  dxddy  = inv_detJ * ( pfy[0] * dxv0
                      + pfy[1] * dxv1
                      + pfy[2] * dxv2
                      + pfy[3] * dxv3 );

  dzddx  = inv_detJ * ( pfx[0] * dzv0
                      + pfx[1] * dzv1
                      + pfx[2] * dzv2
                      + pfx[3] * dzv3 );

  dxddz  = inv_detJ * ( pfz[0] * dxv0
                      + pfz[1] * dxv1
                      + pfz[2] * dxv2
                      + pfz[3] * dxv3 );

  dzddy  = inv_detJ * ( pfy[0] * dzv0
                      + pfy[1] * dzv1
                      + pfy[2] * dzv2
                      + pfy[3] * dzv3 );

  dyddz  = inv_detJ * ( pfz[0] * dyv0
                      + pfz[1] * dyv1
                      + pfz[2] * dyv2
                      + pfz[3] * dyv3 );

  d[5]  = ( dxddy + dyddx ) * HALF;
  d[4]  = ( dxddz + dzddx ) * HALF;
  d[3]  = ( dzddy + dyddz ) * HALF;
}

void UpdatePos(Real_t deltaTime,
               Real_t x_local[8], Real_t y_local[8], Real_t z_local[8],
               Real_t xd_local[8], Real_t yd_local[8], Real_t zd_local[8])
{
  Real_t dt2 = deltaTime * HALF;
  for (Index_t j=0 ; j<8 ; ++j )
  {
    x_local[j] -= dt2 * xd_local[j];
    y_local[j] -= dt2 * yd_local[j];
    z_local[j] -= dt2 * zd_local[j];
  }
}

void CalcKinematicsForElems(Index_t *nodelist,
                            Real_t *x, Real_t *y, Real_t *z,
                            Real_t *xd, Real_t *yd, Real_t *zd,
                            Real_t *dxx, Real_t *dyy, Real_t *dzz,
                            Real_t *v, Real_t *volo,
                            Real_t *vnew, Real_t *delv, Real_t *arealg,
                            Real_t deltaTime, Index_t numElem)
{
  Index_t lnode, gnode, gnode2, j;
  // loop over all elements
  for (Index_t k=0; k<numElem; ++k) {
    Real_t detJ = ZERO;
    Real_t volume;
    Real_t relativeVolume;
    Index_t *elemToNode = &nodelist[8*k];

    Real_t  x_local[8];
    Real_t  y_local[8];
    Real_t  z_local[8];
    Real_t  xd_local[8];
    Real_t  yd_local[8];
    Real_t  zd_local[8];

    /** shape function derivatives */
    Real_t  D[6];
    Real_t  B[3][8];

    // get nodal coordinates from global arrays and copy into local arrays.
    GatherNodes(elemToNode, x, y, z, x_local, y_local, z_local);

    // volume calculations
    volume = CalcElemVolume(x_local, y_local, z_local );
    relativeVolume = volume / volo[k];
    vnew[k] = relativeVolume;
    delv[k] = relativeVolume - v[k];

    // set characteristic length
    arealg[k] = CalcElemCharacteristicLength(x_local, y_local, z_local,
                                             volume);

    // get nodal velocities from global array and copy into local arrays.
    GatherNodes(elemToNode, xd, yd, zd, xd_local, yd_local, zd_local);

    UpdatePos(deltaTime, x_local, y_local, z_local,
              xd_local, yd_local, zd_local);

    CalcElemShapeFunctionDerivatives( x_local, y_local, z_local,
                                      B, &detJ );

    CalcElemVelocityGradient( xd_local, yd_local, zd_local,
                               B, detJ, D );

    // put velocity gradient quantities into their global arrays.
    dxx[k] = D[0];
    dyy[k] = D[1];
    dzz[k] = D[2];
  }
}

int VolErr2(Domain *domain, Index_t numElem)
{
  Real_t *dxx = domain->dxx;
  Real_t *dyy = domain->dyy;
  Real_t *dzz = domain->dzz;
  Real_t *vnew = domain->vnew;
  Real_t *vdovv = domain->vdov;

  for (Index_t k=0; k<numElem; ++k) {
    // calc strain rate and apply as constraint (only done in FB element)
    Real_t vdov = dxx[k] + dyy[k] + dzz[k];
    Real_t vdovthird = vdov * (ONE / 3.0f );

    // make the rate of deformation tensor deviatoric
    vdovv[k] = vdov;
    dxx[k] -= vdovthird;
    dyy[k] -= vdovthird;
    dzz[k] -= vdovthird;

    // See if any volumes are negative, and take appropriate action.
    if (vnew[k] <= ZERO)
    {
      return 1;
    }
  }
  return 0;
}

void CalcLagrangeElements(Domain *domain)
{
   Index_t numElem = domain->numElem;
   if (numElem > 0) {
       Real_t deltatime = domain->deltatime;

      // domain->dxx  = AllocateReal(numElem); /* principal strains */
      // domain->dyy  = AllocateReal(numElem);
      // domain->dzz  = AllocateReal(numElem);

      CalcKinematicsForElems(domain->nodelist,
                             domain->x, domain->y, domain->z,
                             domain->xd, domain->yd, domain->zd,
                             domain->dxx, domain->dyy, domain->dzz,
                             domain->v, domain->volo,
                             domain->vnew, domain->delv, domain->arealg,
                             deltatime, numElem);

      // element loop to do some stuff not included in the elemlib function.
      if (VolErr2(domain, numElem))
         exit(VolumeError);

      // Release((void **) &domain->dzz);
      // Release((void **) &domain->dyy);
      // Release((void **) &domain->dxx);
   }
}

void CalcMonotonicQGradientsForElems(Real_t *x, Real_t *y, Real_t *z,
                                     Real_t *xd, Real_t *yd, Real_t *zd,
                                     Real_t *volo, Real_t *vnew,
                                     Real_t *delv_xi,
                                     Real_t *delv_eta,
                                     Real_t *delv_zeta,
                                     Real_t *delx_xi,
                                     Real_t *delx_eta,
                                     Real_t *delx_zeta,
                                     Index_t *nodelist,
                                     Index_t numElem)
{
   for (Index_t i=0; i<numElem; ++i) {
      Real_t ptiny = 1.0e-36;
      Real_t  ax,  ay,  az;
      Real_t dxv, dyv, dzv;
      Real_t dxi, dyi, dzi;
      Real_t dxj, dyj, dzj;
      Real_t dxk, dyk, dzk;

      Index_t *elemToNode = &nodelist[8*i];
      Index_t n0 := elemToNode[0];
      Index_t n1 := elemToNode[1];
      Index_t n2 := elemToNode[2];
      Index_t n3 := elemToNode[3];
      Index_t n4 := elemToNode[4];
      Index_t n5 := elemToNode[5];
      Index_t n6 := elemToNode[6];
      Index_t n7 := elemToNode[7];

      Real_t vol = volo[i]*vnew[i];
      Real_t norm = ONE / ( vol + ptiny );

      {
         Real_t x0 = x[n0];
         Real_t x1 = x[n1];
         Real_t x2 = x[n2];
         Real_t x3 = x[n3];
         Real_t x4 = x[n4];
         Real_t x5 = x[n5];
         Real_t x6 = x[n6];
         Real_t x7 = x[n7];

         Real_t y0 = y[n0];
         Real_t y1 = y[n1];
         Real_t y2 = y[n2];
         Real_t y3 = y[n3];
         Real_t y4 = y[n4];
         Real_t y5 = y[n5];
         Real_t y6 = y[n6];
         Real_t y7 = y[n7];

         Real_t z0 = z[n0];
         Real_t z1 = z[n1];
         Real_t z2 = z[n2];
         Real_t z3 = z[n3];
         Real_t z4 = z[n4];
         Real_t z5 = z[n5];
         Real_t z6 = z[n6];
         Real_t z7 = z[n7];

         dxj = -0.25f*((x0 + x1 + x5 + x4) - (x3 + x2 + x6 + x7));
         dyj = -0.25f*((y0 + y1 + y5 + y4) - (y3 + y2 + y6 + y7));
         dzj = -0.25f*((z0 + z1 + z5 + z4) - (z3 + z2 + z6 + z7));

         dxi = 0.25f*((x1 + x2 + x6 + x5) - (x0 + x3 + x7 + x4));
         dyi = 0.25f*((y1 + y2 + y6 + y5) - (y0 + y3 + y7 + y4));
         dzi = 0.25f*((z1 + z2 + z6 + z5) - (z0 + z3 + z7 + z4));

         dxk = 0.25f*((x4 + x5 + x6 + x7) - (x0 + x1 + x2 + x3));
         dyk = 0.25f*((y4 + y5 + y6 + y7) - (y0 + y1 + y2 + y3));
         dzk = 0.25f*((z4 + z5 + z6 + z7) - (z0 + z1 + z2 + z3));
      }

      /* find delvk and delxk ( i cross j ) */

      ax = dyi*dzj - dzi*dyj;
      ay = dzi*dxj - dxi*dzj;
      az = dxi*dyj - dyi*dxj;

      // i type and rhs type conflict?
      delx_zeta[i] = ( vol / sqrtf( ax*ax + ay*ay + az*az + ptiny ) );

      Real_t xv0 = xd[n0];
      Real_t xv1 = xd[n1];
      Real_t xv2 = xd[n2];
      Real_t xv3 = xd[n3];
      Real_t xv4 = xd[n4];
      Real_t xv5 = xd[n5];
      Real_t xv6 = xd[n6];
      Real_t xv7 = xd[n7];

      Real_t yv0 = yd[n0];
      Real_t yv1 = yd[n1];
      Real_t yv2 = yd[n2];
      Real_t yv3 = yd[n3];
      Real_t yv4 = yd[n4];
      Real_t yv5 = yd[n5];
      Real_t yv6 = yd[n6];
      Real_t yv7 = yd[n7];

      Real_t zv0 = zd[n0];
      Real_t zv1 = zd[n1];
      Real_t zv2 = zd[n2];
      Real_t zv3 = zd[n3];
      Real_t zv4 = zd[n4];
      Real_t zv5 = zd[n5];
      Real_t zv6 = zd[n6];
      Real_t zv7 = zd[n7];

      dxv = 0.25f*((xv4 + xv5 + xv6 + xv7) - (xv0 + xv1 + xv2 + xv3));
      dyv = 0.25f*((yv4 + yv5 + yv6 + yv7) - (yv0 + yv1 + yv2 + yv3));
      dzv = 0.25f*((zv4 + zv5 + zv6 + zv7) - (zv0 + zv1 + zv2 + zv3));

      delv_zeta[i] = ( ax*dxv + ay*dyv + az*dzv ) * norm;

      /* find delxi and delvi ( j cross k ) */

      ax = dyj*dzk - dzj*dyk;
      ay = dzj*dxk - dxj*dzk;
      az = dxj*dyk - dyj*dxk;

      delx_xi[i] = vol / sqrtf(ax*ax + ay*ay + az*az + ptiny);

      dxv = 0.25f*((xv1 + xv2 + xv6 + xv5) - (xv0 + xv3 + xv7 + xv4));
      dyv = 0.25f*((yv1 + yv2 + yv6 + yv5) - (yv0 + yv3 + yv7 + yv4));
      dzv = 0.25f*((zv1 + zv2 + zv6 + zv5) - (zv0 + zv3 + zv7 + zv4));

      delv_xi[i] = ( ax*dxv + ay*dyv + az*dzv ) * norm;

      /* find delxj and delvj ( k cross i ) */

      ax = dyk*dzi - dzk*dyi;
      ay = dzk*dxi - dxk*dzi;
      az = dxk*dyi - dyk*dxi;

      delx_eta[i] = vol / sqrtf(ax*ax + ay*ay + az*az + ptiny);

      dxv = -0.25f*((xv0 + xv1 + xv5 + xv4) - (xv3 + xv2 + xv6 + xv7));
      dyv = -0.25f*((yv0 + yv1 + yv5 + yv4) - (yv3 + yv2 + yv6 + yv7));
      dzv = -0.25f*((zv0 + zv1 + zv5 + zv4) - (zv3 + zv2 + zv6 + zv7));

      // i is int type and ax is Real_t == mismatch
      delv_eta[i] = ( ax*dxv + ay*dyv + az*dzv ) * norm;
   }

}

void CalcMonotonicQRegionForElems(int *elemBC,
                               Index_t *lxim, Index_t *lxip,
                               Index_t *letam, Index_t *letap,
                               Index_t *lzetam, Index_t *lzetap,
                               Real_t *delv_xi, Real_t *delv_eta,
                               Real_t *delv_zeta, Real_t *delx_xi,
                               Real_t *delx_eta, Real_t *delx_zeta,
                               Real_t *vdov, Real_t *volo, Real_t *vnew,
                               Real_t *elemMass, Real_t *qq, Real_t *ql,
                               Real_t qlc_monoq, Real_t qqc_monoq,
                               Real_t monoq_limiter_mult,
                               Real_t monoq_max_slope,
                               Real_t ptiny, Index_t numElem)
{
   for (Index_t i=0; i<numElem; ++i) {
      Real_t qlin, qquad;
      Real_t phixi, phieta, phizeta;
      int bcMask = elemBC[i];
      Real_t delvm, delvp;

      /*  phixi     */
      Real_t norm = ONE / ( delv_xi[i] + ptiny );

      switch (bcMask & XI_M) {
         case 0:         delvm = delv_xi[lxim[i]]; break;
         case XI_M_SYMM: delvm = delv_xi[i];       break;
         case XI_M_FREE: delvm = ZERO;             break;
         default:        /* ERROR */;              break;
      }
      switch (bcMask & XI_P) {
         case 0:         delvp = delv_xi[lxip[i]]; break;
         case XI_P_SYMM: delvp = delv_xi[i];       break;
         case XI_P_FREE: delvp = ZERO;             break;
         default:        /* ERROR */;              break;
      }

      delvm = delvm * norm;
      delvp = delvp * norm;

      phixi = ( delvm + delvp ) * HALF;

      delvm *= monoq_limiter_mult;
      delvp *= monoq_limiter_mult;

      if ( delvm < phixi ) phixi = delvm;
      if ( delvp < phixi ) phixi = delvp;
      if ( phixi < ZERO) phixi = ZERO;
      if ( phixi > monoq_max_slope) phixi = monoq_max_slope;


      /*  phieta     */
      norm = ONE / ( delv_eta[i] + ptiny );

      switch (bcMask & ETA_M) {
         case 0:          delvm = delv_eta[letam[i]]; break;
         case ETA_M_SYMM: delvm = delv_eta[i];        break;
         case ETA_M_FREE: delvm = ZERO;               break;
         default:         /* ERROR */;                break;
      }
      switch (bcMask & ETA_P) {
         case 0:          delvp = delv_eta[letap[i]]; break;
         case ETA_P_SYMM: delvp = delv_eta[i];        break;
         case ETA_P_FREE: delvp = ZERO;               break;
         default:         /* ERROR */;                break;
      }

      delvm = delvm * norm;
      delvp = delvp * norm;

      phieta = ( delvm + delvp ) * HALF;

      delvm *= monoq_limiter_mult;
      delvp *= monoq_limiter_mult;

      if ( delvm  < phieta ) phieta = delvm;
      if ( delvp  < phieta ) phieta = delvp;
      if ( phieta < ZERO) phieta = ZERO;
      if ( phieta > monoq_max_slope)  phieta = monoq_max_slope;

      /*  phizeta     */
      norm = ONE / ( delv_zeta[i] + ptiny );

      switch (bcMask & ZETA_M) {
         case 0:           delvm = delv_zeta[lzetam[i]]; break;
         case ZETA_M_SYMM: delvm = delv_zeta[i];         break;
         case ZETA_M_FREE: delvm = ZERO;                 break;
         default:          /* ERROR */;                  break;
      }
      switch (bcMask & ZETA_P) {
         case 0:           delvp = delv_zeta[lzetap[i]]; break;
         case ZETA_P_SYMM: delvp = delv_zeta[i];         break;
         case ZETA_P_FREE: delvp = ZERO;                 break;
         default:          /* ERROR */;                  break;
      }

      delvm = delvm * norm;
      delvp = delvp * norm;

      phizeta = ( delvm + delvp ) * HALF;

      delvm *= monoq_limiter_mult;
      delvp *= monoq_limiter_mult;

      if ( delvm   < phizeta ) phizeta = delvm;
      if ( delvp   < phizeta ) phizeta = delvp;
      if ( phizeta < ZERO)     phizeta = ZERO;
      if ( phizeta > monoq_max_slope  ) phizeta = monoq_max_slope;

      /* Remove length scale */

      if ( vdov[i] > ZERO )  {
         qlin  = ZERO;
         qquad = ZERO;
      }
      else {
         Real_t delvxxi   = delv_xi[i]   * delx_xi[i];
         Real_t delvxeta  = delv_eta[i]  * delx_eta[i];
         Real_t delvxzeta = delv_zeta[i] * delx_zeta[i];

         if ( delvxxi   > ZERO ) delvxxi   = ZERO;
         if ( delvxeta  > ZERO ) delvxeta  = ZERO;
         if ( delvxzeta > ZERO ) delvxzeta = ZERO;

         Real_t rho = elemMass[i] / (volo[i] * vnew[i]);

         qlin = -qlc_monoq * rho *
            (  delvxxi   * (ONE - phixi) +
               delvxeta  * (ONE - phieta) +
               delvxzeta * (ONE - phizeta)  );

         qquad = qqc_monoq * rho *
            (  delvxxi*delvxxi     * (ONE - phixi*phixi) +
               delvxeta*delvxeta   * (ONE - phieta*phieta) +
               delvxzeta*delvxzeta * (ONE - phizeta*phizeta)  );
      }

      qq[i] = qquad;
      ql[i] = qlin;
   }
}

void CalcMonotonicQForElems(Domain *domain)
{  
   //
   // calculate the monotonic q for pure regions
   //
   Index_t numElem = domain->numElem;
   if (numElem > 0) {
      //
      // initialize parameters
      // 
       Real_t ptiny = 1.e-36;

      CalcMonotonicQRegionForElems(
                           domain->elemBC,
                           domain->lxim,   domain->lxip,
                           domain->letam,  domain->letap,
                           domain->lzetam, domain->lzetap,
                           domain->delv_xi,domain->delv_eta,domain->delv_zeta,
                           domain->delx_xi,domain->delx_eta,domain->delx_zeta,
                           domain->vdov, domain->volo, domain->vnew,
                           domain->elemMass, domain->qq, domain->ql,
                           domain->qlc_monoq, domain->qqc_monoq,
                           domain->monoq_limiter_mult,
                           domain->monoq_max_slope,
                           ptiny, numElem );
   }
}

Index_t Qerr(Real_t *q, Index_t numElem, Real_t qstop)
{
  Real_t qstopl = qstop; // force register allocation
  Index_t idx = -1;
  for (Index_t i=0; i<numElem; ++i) {
    if ( q[i] > qstopl ) {
      idx = i;
      // break;
    }
  }
  return idx;
}

void CalcQForElems(Domain *domain)
{
   //
   // MONOTONIC Q option
   //

   Index_t numElem = domain->numElem;

   if (numElem != 0) {
      /* allocate domain length arrays */

      // domain->delv_xi = AllocateReal(numElem);   /* velocity gradient */
      // domain->delv_eta = AllocateReal(numElem);
      // domain->delv_zeta = AllocateReal(numElem);

      // domain->delx_xi = AllocateReal(numElem);   /* position gradient */
      // domain->delx_eta = AllocateReal(numElem);
      // domain->delx_zeta = AllocateReal(numElem);

      /* Calculate velocity gradients, applied at the domain level */
      CalcMonotonicQGradientsForElems(domain->x,  domain->y,  domain->z,
                                      domain->xd, domain->yd, domain->zd,
                                      domain->volo, domain->vnew,
                                      domain->delv_xi,
                                      domain->delv_eta,
                                      domain->delv_zeta,
                                      domain->delx_xi,
                                      domain->delx_eta,
                                      domain->delx_zeta,
                                      domain->nodelist,
                                      numElem);

      /* Transfer veloctiy gradients in the first order elements */
      /* problem->commElements->Transfer(CommElements::monoQ); */

      /* This will be applied at the region level */
      CalcMonotonicQForElems(domain);

      /* release domain length arrays */

      // Release((void **) &domain->delx_zeta);
      // Release((void **) &domain->delx_eta);
      // Release((void **) &domain->delx_xi);

      // Release((void **) &domain->delv_zeta);
      // Release((void **) &domain->delv_eta);
      // Release((void **) &domain->delv_xi);

      /* Don't allow excessive artificial viscosity */
      Qerr(domain->q, numElem, domain->qstop);
   }
}

#define c1s 0.666666666f

inline void CalcPressureForElems(Real_t *p_new, Real_t *bvc,
                          Real_t *pbvc, Real_t *e_old,
                          Real_t *compression, Real_t *vnewc,
                          Real_t pmin, Real_t p_cut, Real_t eosvmax,
                          Index_t length)
{
   // Real_t c1s = 2.0f / 3.0f;
   for (Index_t i=0 ; i<length; ++i ) {
      bvc[i] = c1s * (compression[i] + ONE );
      pbvc[i] = c1s;
   }

   for (Index_t i=0; i<length; ++i ) {
      p_new[i] = bvc[i] * e_old[i];

      if (fabsf(p_new[i]) <  p_cut)
         p_new[i] = ZERO;

      if ( vnewc[i] >= eosvmax ) /* impossible condition here? */
         p_new[i] = ZERO;

      if (p_new[i]       <  pmin)
         p_new[i]   = pmin;
   }
}

#define sixth 0.166666666f

void CalcEnergyForElems(Real_t *p_new, Real_t *e_new, Real_t *q_new,
                        Real_t *bvc, Real_t *pbvc,
                        Real_t *p_old, Real_t *e_old, Real_t *q_old,
                        Real_t *compression, Real_t *compHalfStep,
                        Real_t *vnewc, Real_t *work, Real_t *delvc,
                        Real_t pmin, Real_t p_cut, Real_t  e_cut,
                        Real_t q_cut, Real_t emin, Real_t *qq_old,
                        Real_t *ql_old, Real_t rho0, Real_t eosvmax,
                        Real_t *pHalfStep, Index_t length)
{
   // Real_t sixth = ONE / 6.0f;
   // Real_t *pHalfStep = AllocateReal(length);

   for (Index_t i=0; i<length; ++i) {
      e_new[i] = e_old[i] - delvc[i]*(p_old[i] + q_old[i]) * HALF +
                 work[i] * HALF;

      if (e_new[i]  < emin ) {
         e_new[i] = emin;
      }
   }

   CalcPressureForElems(pHalfStep, bvc, pbvc, e_new, compHalfStep, vnewc,
                   pmin, p_cut, eosvmax, length);

   for (Index_t i=0; i<length; ++i) {
      Real_t vhalf = ONE / (ONE + compHalfStep[i]);

      if ( delvc[i] > ZERO ) {
         q_new[i] /* = qq_old[i] = ql_old[i] */ = ZERO;
      }
      else {
         Real_t ssc = ( pbvc[i] * e_new[i]
                 + vhalf * vhalf * bvc[i] * pHalfStep[i] ) / rho0;

         if ( ssc > 0.1111111e-36f ) {
            ssc = sqrtf(ssc);
         } else {
            ssc = 0.3333333e-18f;
         }

         q_new[i] = (ssc*ql_old[i] + qq_old[i]);
      }

      e_new[i] = e_new[i] + delvc[i] * HALF
         * (  3.0f * (p_old[i]     + q_old[i])
              - 4.0f * (pHalfStep[i] + q_new[i]));
   }

   for (Index_t i=0; i<length; ++i) {

      e_new[i] += work[i] * HALF;

      if (fabsf(e_new[i]) < e_cut) {
         e_new[i] = ZERO;
      }
      if (     e_new[i]  < emin ) {
         e_new[i] = emin;
      }
   }

   CalcPressureForElems(p_new, bvc, pbvc, e_new, compression, vnewc,
                   pmin, p_cut, eosvmax, length);

   for (Index_t i=0; i<length; ++i) {
      Real_t q_tilde;

      if (delvc[i] > ZERO) {
         q_tilde = ZERO;
      }
      else {
         Real_t ssc = ( pbvc[i] * e_new[i]
                 + vnewc[i] * vnewc[i] * bvc[i] * p_new[i] ) / rho0;

         if ( ssc > 0.1111111e-36f ) {
            ssc = sqrtf(ssc);
         } else {
            ssc = 0.3333333e-18f;
         }

         q_tilde = (ssc*ql_old[i] + qq_old[i]);
      }

      e_new[i] = e_new[i] - (  (p_old[i]     + q_old[i]) * 7.0f
                               - (pHalfStep[i] + q_new[i]) * 8.0f
                               + (p_new[i] + q_tilde)) * delvc[i]*sixth;

      if (fabsf(e_new[i]) < e_cut) {
         e_new[i] = ZERO;
      }
      if (     e_new[i]  < emin ) {
         e_new[i] = emin;
      }
   }

   CalcPressureForElems(p_new, bvc, pbvc, e_new, compression, vnewc,
                   pmin, p_cut, eosvmax, length);

   for (Index_t i=0 ;i<length; ++i ) {

      if ( delvc[i] <= ZERO ) {
         Real_t ssc = ( pbvc[i] * e_new[i]
                 + vnewc[i] * vnewc[i] * bvc[i] * p_new[i] ) / rho0;

         if ( ssc > 0.1111111e-36f ) {
            ssc = sqrtf(ssc);
         } else {
            ssc = 0.3333333e-18f;
         }

         q_new[i] = (ssc*ql_old[i] + qq_old[i]);

         if (fabsf(q_new[i]) < q_cut) q_new[i] = ZERO;
      }
   }

   // Release((void **) &pHalfStep);

   return ;
}

void CalcSoundSpeedForElems(Index_t length, Real_t *ss,
                            Real_t *vnewc, Real_t rho0, Real_t *enewc,
                            Real_t *pnewc, Real_t *pbvc,
                            Real_t *bvc, Real_t ss4o3)
{
   for (Index_t iz=0; iz<length; ++iz) {
      Real_t ssTmp = (pbvc[iz] * enewc[iz] + vnewc[iz] * vnewc[iz] *
                 bvc[iz] * pnewc[iz]) / rho0;
      if (ssTmp <= 0.1111111e-36f) {
         ssTmp = 0.3333333e-18f;
      }
      else {
         ssTmp = sqrtf( ssTmp );
      }
      ss[iz] = ssTmp;
   }
}

void EvalCopy(Real_t *p_old, Real_t *p, Index_t numElem)
{
  for (Index_t zidx=0; zidx<numElem; ++zidx) {
    p_old[zidx] = p[zidx];
  }
}

void EvalCompression(Real_t *compression, Real_t *compHalfStep,
                     Index_t numElem, Real_t *vnewc, Real_t *delvc)
{
  for (Index_t zidx=0; zidx<numElem; ++zidx) {
    Real_t vchalf;
    compression[zidx] = ONE / vnewc[zidx] - ONE;
    vchalf = vnewc[zidx] - delvc[zidx] * HALF;
    compHalfStep[zidx] = ONE / vchalf - ONE;
  }
}

void EvalEosVmin(Real_t *vnewc, Real_t *compHalfStep, Real_t *compression,
                 Index_t numElem, Real_t eosvmin)
{
  Real_t eosvminl = eosvmin; // force register allocation
  for (Index_t zidx=0; zidx<numElem; ++zidx) {
    if (vnewc[zidx] <= eosvminl) { /* impossible due to calling func? */
      compHalfStep[zidx] = compression[zidx];
    }
  }
}

void EvalEosVmax(Real_t *vnewc, Real_t *p_old,
                 Real_t *compHalfStep, Real_t *compression,
                 Index_t numElem, Real_t eosvmax)
{
  Real_t eosvmaxl = eosvmax; // force register allocation
  for (Index_t zidx=0; zidx<numElem; ++zidx) {
    if (vnewc[zidx] >= eosvmaxl) { /* impossible due to calling func? */
      p_old[zidx]        = ZERO;
      compression[zidx]  = ZERO;
      compHalfStep[zidx] = ZERO;
    }
  }
}

void EvalEosResetWork(Real_t *work, Index_t numElem)
{
  for (Index_t zidx=0; zidx<numElem; ++zidx) {
    work[zidx] = ZERO;
  }
}

void UpdatePE(Real_t *p, Real_t *p_new, Real_t *e, Real_t *e_new,
              Real_t *q, Real_t *q_new, Index_t numElem)
{
  for (Index_t zidx=0; zidx<numElem; ++zidx) {
    p[zidx] = p_new[zidx];
    e[zidx] = e_new[zidx];
    q[zidx] = q_new[zidx];
  }
}

void EvalEOSForElems(Domain *domain, Real_t *vnewc, Index_t numElem)
{
   Real_t  e_cut = domain->e_cut;
   Real_t  p_cut = domain->p_cut;
   Real_t  ss4o3 = domain->ss4o3;
   Real_t  q_cut = domain->q_cut;

   Real_t eosvmax = domain->eosvmax;
   Real_t eosvmin = domain->eosvmin;
   Real_t pmin    = domain->pmin;
   Real_t emin    = domain->emin;
   Real_t rho0    = domain->refdens;

   /* allocate *domain length* arrays.  */
   /* wastes memory, but allows us to get */
   /* around a "temporary workset" issue */
   /* we have not yet addressed. */
   Real_t *delvc = domain->delv;
   Real_t *p_old = p_old_;
   Real_t *compression = compression_;
   Real_t *compHalfStep = compHalfStep_;
   Real_t *work = work_;
   Real_t *p_new = p_new_;
   Real_t *e_new = e_new_;
   Real_t *q_new = q_new_;
   Real_t *bvc = bvc_;
   Real_t *pbvc = pbvc_;
   Real_t *pHalfStep = pHalfStep_;

   /* compress data, minimal set */
   EvalCopy(p_old, domain->p, numElem);

   EvalCompression(compression, compHalfStep, numElem, vnewc, delvc);

   /* Check for v > eosvmax or v < eosvmin */
   if ( eosvmin != ZERO ) {
      EvalEosVmin(vnewc, compHalfStep, compression, numElem, eosvmin);
   }

   if ( eosvmax != ZERO ) {
      EvalEosVmax(vnewc, p_old, compHalfStep, compression, numElem, eosvmax);
   }

   EvalEosResetWork(work, numElem);

   CalcEnergyForElems(p_new, e_new, q_new, bvc, pbvc,
                 p_old, domain->e,  domain->q, compression, compHalfStep,
                 vnewc, work,  delvc, pmin,
                 p_cut, e_cut, q_cut, emin,
                 domain->qq, domain->ql, rho0, eosvmax,
                 pHalfStep, numElem);


   UpdatePE(domain->p, p_new, domain->e, e_new, domain->q, q_new, numElem);

   CalcSoundSpeedForElems(numElem, domain->ss,
             vnewc, rho0, e_new, p_new,
             pbvc, bvc, ss4o3);

   // Release((void **) &pHalfStep);
   // Release((void **) &pbvc);
   // Release((void **) &bvc);
   // Release((void **) &q_new);
   // Release((void **) &e_new);
   // Release((void **) &p_new);
   // Release((void **) &work);
   // Release((void **) &compHalfStep);
   // Release((void **) &compression);
   // Release((void **) &p_old);
}

int VolErr3(Real_t *vnewc, Real_t *vnew, Real_t *v, Index_t numElem,
            Real_t eosvmin, Real_t eosvmax)
{
  for (Index_t zn=0; zn<numElem; ++zn) {
    vnewc[zn] = vnew[zn];
  }

  if (eosvmin != ZERO) {
    for (Index_t zn=0; zn<numElem; ++zn) {
      if (vnewc[zn] < eosvmin)
        vnewc[zn] = eosvmin;
    }
  }

  if (eosvmax != ZERO) {
    for (Index_t zn=0; zn<numElem; ++zn) {
      if (vnewc[zn] > eosvmax)
        vnewc[zn] = eosvmax;
    }
  }

  for (Index_t zn=0; zn<numElem; ++zn) {
    Real_t vc = v[zn];
    if (eosvmin != ZERO) {
      if (vc < eosvmin)
        vc = eosvmin;
    }
    if (eosvmax != ZERO) {
      if (vc > eosvmax)
        vc = eosvmax;
    }
    if (vc <= ZERO) return 1;
  }
  return 0;
}

void ApplyMaterialPropertiesForElems(Domain *domain)
{
  Index_t numElem = domain->numElem;

  if (numElem != 0) {
    /* Expose all of the variables needed for material evaluation */

    /* create a domain length (not material length) temporary */
    /* we are assuming here that the number of dense ranges is */
    /* much greater than the number of sigletons.  We are also */
    /* assuming it is ok to allocate a domain length temporary */
    /* rather than a material length temporary. */

    if (VolErr3(vnewc_, domain->vnew, domain->v, numElem,
                domain->eosvmin, domain->eosvmax))
       exit(VolumeError);

    EvalEOSForElems(domain, vnewc_, numElem);

    // Release((void **) &vnewc);

  }
}

void UpdateVolumesForElems(Real_t *vnew, Real_t *v,
                           Real_t v_cut, Index_t length)
{
   Real_t v_cutl = v_cut; // force register allocation
   if (length != 0) {
      for (Index_t i=0; i<length; ++i) {
         Real_t tmpV = vnew[i];

         if ( fabsf(tmpV - ONE) < v_cutl )
            tmpV = ONE;

         v[i] = tmpV;
      }
   }

   return;
}

void LagrangeElements(Domain *domain, Index_t numElem)
{
  /* new relative volume -- temporary */
  // domain->vnew = AllocateReal(numElem);

  CalcLagrangeElements(domain);

  /* Calculate Q.  (Monotonic q option requires communication) */
  CalcQForElems(domain);

  ApplyMaterialPropertiesForElems(domain);

  UpdateVolumesForElems(domain->vnew, domain->v,
                        domain->v_cut, numElem);

  // Release((void **) &domain->vnew);
}

void CalcCourantConstraintForElems(Index_t length, Real_t *ss,
                                   Real_t *vdov, Real_t *arealg,
                                   Real_t qqc, Real_t *dtcourant)
{
   Real_t dtcourant_tmp = 1.0e+20f;
   Index_t   courant_elem = -1;

   Real_t  qqc2 = 64.0f * qqc * qqc;

   for (Index_t indx=0; indx<length; ++indx) {

      Real_t dtf = ss[indx] * ss[indx];

      if ( vdov[indx] < ZERO ) {

         dtf = dtf
            + qqc2 * arealg[indx] * arealg[indx] * vdov[indx] * vdov[indx];
      }

      dtf = sqrtf( dtf );

      dtf = arealg[indx] / dtf;

      /* determine minimum timestep with its corresponding elem */

      if (vdov[indx] != ZERO) {
         if ( dtf < dtcourant_tmp ) {
            dtcourant_tmp = dtf;
            courant_elem = indx;
         }
      }
   }

   /* Don't try to register a time constraint if none of the elements
    * were active */

   if (courant_elem != -1) {
      *dtcourant = dtcourant_tmp;
   }

   return;
}

void CalcHydroConstraintForElems(Index_t length, Real_t *vdov,
                                 Real_t dvovmax, Real_t *dthydro)
{
   Real_t dvovmaxl = dvovmax;
   Real_t dthydro_tmp = 1.0e+20f;
   Index_t hydro_elem = -1;

   for (Index_t indx=0; indx<length; ++indx) {
      if (vdov[indx] != ZERO) {
         Real_t dtdvov = dvovmaxl / (fabsf(vdov[indx])+1.0e-20f);
         if ( dthydro_tmp > dtdvov ) {
            dthydro_tmp = dtdvov;
            hydro_elem = indx;
         }
      }
   }

   if (hydro_elem != -1) {
      *dthydro = dthydro_tmp;
   }

   return ;
}

void CalcTimeConstraintsForElems(Domain *domain) {
   CalcCourantConstraintForElems(domain->numElem, domain->ss,
                                 domain->vdov, domain->arealg,
                                 domain->qqc, &domain->dtcourant);

   /* check hydro constraint */
   CalcHydroConstraintForElems(domain->numElem, domain->vdov,
                               domain->dvovmax, &domain->dthydro);
}

void LagrangeLeapFrog(Domain *domain)
{
   /* calculate nodal forces, accelerations, velocities, positions, with
    * applied boundary conditions and slide surface considerations */
   LagrangeNodal(domain);

   /* calculate element quantities (i.e. velocity gradient & q), and update
    * material states */
   LagrangeElements(domain, domain->numElem);

   CalcTimeConstraintsForElems(domain);

}

int main(int argc, char *argv[])
{
   Index_t i, j, lnode, plane, row, col;
   Real_t tx, ty, tz;
   Index_t nidx, zidx;
   Domain domain;

   Index_t edgeElems = 20;
   Index_t edgeNodes = edgeElems+1;

   /****************************/
   /*   Initialize Sedov Mesh  */
   /****************************/

   /* construct a uniform box for this processor */

   domain.sizeX = edgeElems;
   domain.sizeY = edgeElems;
   domain.sizeZ = edgeElems;
   domain.numElem = edgeElems*edgeElems*edgeElems;

   domain.numNode = edgeNodes*edgeNodes*edgeNodes;

   Index_t domElems = domain.numElem;
   Index_t domNodes = domain.numNode;

   /*************************/
   /* allocate field memory */
   /*************************/
   
   /*****************/
   /* Elem-centered */
   /*****************/

   /* elemToNode connectivity */
   domain.nodelist = AllocateIndex(8*domElems);

   /* elem connectivity through face */
   domain.lxim = AllocateIndex(domElems);
   domain.lxip = AllocateIndex(domElems);
   domain.letam = AllocateIndex(domElems);
   domain.letap = AllocateIndex(domElems);
   domain.lzetam = AllocateIndex(domElems);
   domain.lzetap = AllocateIndex(domElems);

   /* elem face symm/free-surface flag */
   domain.elemBC = AllocateInt(domElems);

   domain.e = AllocateReal(domElems);   /* energy */
   domain.p = AllocateReal(domElems);   /* pressure */

   domain.q = AllocateReal(domElems);   /* q */
   domain.ql = AllocateReal(domElems);  /* linear term for q */
   domain.qq = AllocateReal(domElems);  /* quadratic term for q */

   domain.v = AllocateReal(domElems);     /* relative volume */
   domain.volo = AllocateReal(domElems);  /* reference volume */
   domain.delv = AllocateReal(domElems);  /* m_vnew - m_v */
   domain.vdov = AllocateReal(domElems);  /* volume deriv over volume */

   /* elem characteristic length */
   domain.arealg = AllocateReal(domElems);

   domain.ss = AllocateReal(domElems);    /* "sound speed" */

   domain.elemMass = AllocateReal(domElems);  /* mass */

   /*****************/
   /* Node-centered */
   /*****************/

   domain.x = AllocateReal(domNodes);  /* coordinates */
   domain.y = AllocateReal(domNodes);
   domain.z = AllocateReal(domNodes);

   domain.xd = AllocateReal(domNodes); /* velocities */
   domain.yd = AllocateReal(domNodes);
   domain.zd = AllocateReal(domNodes);

   domain.xdd = AllocateReal(domNodes); /* accelerations */
   domain.ydd = AllocateReal(domNodes);
   domain.zdd = AllocateReal(domNodes);

   domain.fx = AllocateReal(domNodes);  /* forces */
   domain.fy = AllocateReal(domNodes);
   domain.fz = AllocateReal(domNodes);

   domain.nodalMass = AllocateReal(domNodes);  /* mass */

   /* Boundary nodesets */

   domain.symmX = AllocateIndex(edgeNodes*edgeNodes);
   domain.symmY = AllocateIndex(edgeNodes*edgeNodes);
   domain.symmZ = AllocateIndex(edgeNodes*edgeNodes);

   dvdx_ = AllocateReal(domElems*8);
   dvdy_ = AllocateReal(domElems*8);
   dvdz_ = AllocateReal(domElems*8);

   x8n_  = AllocateReal(domElems*8);
   y8n_  = AllocateReal(domElems*8);
   z8n_  = AllocateReal(domElems*8);

   sigxx_  = /* AllocateReal(domElems) */ dvdx_;
   sigyy_  = /* AllocateReal(domElems) */ sigxx_ + domElems;
   sigzz_  = /* AllocateReal(domElems) */ sigyy_ + domElems;

   determ_ = AllocateReal(domElems);
   vnewc_ = /* AllocateReal(domElems) */ dvdx_;

   p_old_ = /* AllocateReal(domElems) */ vnewc_ + domElems;
   compression_ = /* AllocateReal(domElems) */ p_old_ + domElems;
   compHalfStep_ = /* AllocateReal(domElems) */ compression_ + domElems;
   work_ = /* AllocateReal(domElems) */ compHalfStep_ + domElems;
   p_new_ = /* AllocateReal(domElems) */ work_ + domElems;
   e_new_ = /* AllocateReal(domElems) */ p_new_ + domElems;
   q_new_ = /* AllocateReal(domElems) */ e_new_ + domElems;
   bvc_   = /* AllocateReal(domElems) */ dvdy_;
   pbvc_  = /* AllocateReal(domElems) */ bvc_ + domElems;
   pHalfStep_ = /* AllocateReal(domElems) */ pbvc_ + domElems;

   domain.dxx  = /* AllocateReal(domElems) */ dvdx_; // principal strains
   domain.dyy  = /* AllocateReal(domElems) */ domain.dxx + domElems;
   domain.dzz  = /* AllocateReal(domElems) */ domain.dyy + domElems;

   domain.delv_xi   = /* AllocateReal(domElems) */ dvdx_; // velocity gradient
   domain.delv_eta  = /* AllocateReal(domElems) */ domain.delv_xi + domElems;
   domain.delv_zeta = /* AllocateReal(domElems) */ domain.delv_eta + domElems;

   /* position gradient */
   domain.delx_xi   = /* AllocateReal(domElems) */ domain.delv_zeta + domElems;
   domain.delx_eta  = /* AllocateReal(domElems) */ domain.delx_xi + domElems;
   domain.delx_zeta = /* AllocateReal(domElems) */ domain.delx_eta + domElems;

   domain.vnew = AllocateReal(domElems);

   /* Basic Field Initialization */

   for (i=0; i<domElems; ++i) {
      domain.e[i] = ZERO;
      domain.p[i] = ZERO;
      domain.q[i] = ZERO;
      domain.v[i] = ONE;
   }

   for (i=0; i<domNodes; ++i) {
      domain.xd[i] = ZERO;
      domain.yd[i] = ZERO;
      domain.zd[i] = ZERO;
   }

   for (i=0; i<domNodes; ++i) {
      domain.xdd[i] = ZERO;
      domain.ydd[i] = ZERO;
      domain.zdd[i] = ZERO;
   }

   /* initialize nodal coordinates */

   nidx = 0;
   tz = fmaxf(ZERO, ZERO); // fmaxf call magically shaves 8 sec off runtime
   for (plane=0; plane<edgeNodes; ++plane) {
      ty = ZERO;
      for (row=0; row<edgeNodes; ++row) {
         tx = ZERO;
         for (col=0; col<edgeNodes; ++col) {
            domain.x[nidx] = tx;
            domain.y[nidx] = ty;
            domain.z[nidx] = tz;
            ++nidx;
            // tx += ds; /* may accumulate roundoff... */
            tx = 1.125f*(Real_t)(col+1)/(Real_t)(edgeElems);
         }
         // ty += ds;  /* may accumulate roundoff... */
         ty = 1.125f*(Real_t)(row+1)/(Real_t)(edgeElems);
      }
      // tz += ds;  /* may accumulate roundoff... */
      tz = 1.125f*(Real_t)(plane+1)/(Real_t)(edgeElems);
   }


   /* embed hexehedral elements in nodal point lattice */

   nidx = 0;
   zidx = 0;
   for (plane=0; plane<edgeElems; ++plane) {
      for (row=0; row<edgeElems; ++row) {
         for (col=0; col<edgeElems; ++col) {
            Index_t *localNode = &domain.nodelist[8*zidx];
            localNode[0] = nidx                                       ;
            localNode[1] = nidx                                   + 1 ;
            localNode[2] = nidx                       + edgeNodes + 1 ;
            localNode[3] = nidx                       + edgeNodes     ;
            localNode[4] = nidx + edgeNodes*edgeNodes                 ;
            localNode[5] = nidx + edgeNodes*edgeNodes             + 1 ;
            localNode[6] = nidx + edgeNodes*edgeNodes + edgeNodes + 1 ;
            localNode[7] = nidx + edgeNodes*edgeNodes + edgeNodes     ;
            ++zidx;
            ++nidx;
         }
         ++nidx;
      }
      nidx += edgeNodes;
   }

   /* initialize material parameters */
   domain.dtfixed = -1.0e-7f;
   domain.deltatime = 1.0e-7f;
   domain.deltatimemultlb = 1.1f;
   domain.deltatimemultub = 1.2f;
   domain.stoptime  = 1.0e-2f;
   domain.dtcourant = 1.0e+20f;
   domain.dthydro   = 1.0e+20f;
   domain.dtmax     = 1.0e-2f;
   domain.time    = ZERO;
   domain.cycle   = 0;

   domain.e_cut = 1.0e-7f;
   domain.p_cut = 1.0e-7f;
   domain.q_cut = 1.0e-7f;
   domain.u_cut = 1.0e-7f;
   domain.v_cut = 1.0e-10f;

   domain.hgcoef      = 3.0f;
   domain.ss4o3       = 4.0f/3.0f;

   domain.qstop              =  1.0e+12f;
   domain.monoq_max_slope    =  ONE;
   domain.monoq_limiter_mult =  2.0f;
   domain.qlc_monoq          = HALF;
   domain.qqc_monoq          = 2.0f/3.0f;
   domain.qqc                = 2.0f;

   domain.pmin =  ZERO;
   domain.emin = -1.0e+15f;

   domain.dvovmax =  0.1f;

   domain.eosvmax =  1.0e+9f;
   domain.eosvmin =  1.0e-9f;

   domain.refdens =  ONE;

   /* initialize field data */
   for (i=0; i<domNodes; ++i) {
      domain.nodalMass[i] = ZERO;
   }

   for (i=0; i<domElems; ++i) {
      Real_t  x_local[8];
      Real_t  y_local[8];
      Real_t  z_local[8];
      Index_t *elemToNode = &domain.nodelist[8*i];
      for( lnode=0 ; lnode<8 ; ++lnode )
      {
        Index_t gnode = elemToNode[lnode];
        x_local[lnode] = domain.x[gnode];
        y_local[lnode] = domain.y[gnode];
        z_local[lnode] = domain.z[gnode];
      }

      // volume calculations
      Real_t volume = CalcElemVolume(x_local, y_local, z_local );
      domain.volo[i] = volume;
      domain.elemMass[i] = volume;
      for (j=0; j<8; ++j) {
         Index_t idx = elemToNode[j];
         domain.nodalMass[idx] += volume / 8.0f;
      }
   }

   /* deposit energy */
   domain.e[0] = 3.948746e+7f;

   /* set up symmetry nodesets */
   nidx = 0;
   for (i=0; i<edgeNodes; ++i) {
      Index_t planeInc = i*edgeNodes*edgeNodes;
      Index_t rowInc   = i*edgeNodes;
      for (j=0; j<edgeNodes; ++j) {
         domain.symmX[nidx] = planeInc + j*edgeNodes;
         domain.symmY[nidx] = planeInc + j;
         domain.symmZ[nidx] = rowInc   + j;
         ++nidx;
      }
   }

   /* set up elemement connectivity information */
   domain.lxim[0] = 0;
   for (i=1; i<domElems; ++i) {
      domain.lxim[i]   = i-1;
      domain.lxip[i-1] = i;
   }
   domain.lxip[domElems-1] = domElems-1;

   for (i=0; i<edgeElems; ++i) {
      domain.letam[i] = i; 
      domain.letap[domElems-edgeElems+i] = domElems-edgeElems+i;
   }
   for (i=edgeElems; i<domElems; ++i) {
      domain.letam[i] = i-edgeElems;
      domain.letap[i-edgeElems] = i;
   }

   for (i=0; i<edgeElems*edgeElems; ++i) {
      domain.lzetam[i] = i;
      domain.lzetap[domElems-edgeElems*edgeElems+i] =
         domElems-edgeElems*edgeElems+i;
   }
   for (i=edgeElems*edgeElems; i<domElems; ++i) {
      domain.lzetam[i] = i - edgeElems*edgeElems;
      domain.lzetap[i-edgeElems*edgeElems] = i;
   }

   /* set up boundary condition information */
   for (i=0; i<domElems; ++i) {
      domain.elemBC[i] = 0;  /* clear BCs by default */
   }

   /* faces on "external" boundaries will be */
   /* symmetry plane or free surface BCs */
   for (i=0; i<edgeElems; ++i) {
      Index_t planeInc2 = i*edgeElems*edgeElems;
      Index_t rowInc2   = i*edgeElems;
      for (j=0; j<edgeElems; ++j) {
         domain.elemBC[planeInc2+j*edgeElems] |= XI_M_SYMM;
         domain.elemBC[planeInc2+j*edgeElems+edgeElems-1] |= XI_P_FREE;
         domain.elemBC[planeInc2+j] |= ETA_M_SYMM;
         domain.elemBC[planeInc2+j+edgeElems*edgeElems-edgeElems] |=ETA_P_FREE;
         domain.elemBC[rowInc2+j] |= ZETA_M_SYMM;
         domain.elemBC[rowInc2+j+domElems-edgeElems*edgeElems] |= ZETA_P_FREE;
      }
   }

   /* timestep to solution */
   while(domain.time < domain.stoptime) {
      TimeIncrement(&domain);
      LagrangeLeapFrog(&domain);
      /* problem->commNodes->Transfer(CommNodes::syncposvel); */
#ifdef LULESH_SHOW_PROGRESS
      printf("time = %e, dt=%e\n",
             (Real_t)(domain.time), (Real_t)(domain.deltatime) );
#endif
   }

   Release((void **) &domain.symmZ);
   Release((void **) &domain.symmY);
   Release((void **) &domain.symmX);

   Release((void **) &domain.nodalMass);

   Release((void **) &domain.fz);
   Release((void **) &domain.fy);
   Release((void **) &domain.fx);

   Release((void **) &domain.zdd);
   Release((void **) &domain.ydd);
   Release((void **) &domain.xdd);

   Release((void **) &domain.zd);
   Release((void **) &domain.yd);
   Release((void **) &domain.xd);

   Release((void **) &domain.z);
   Release((void **) &domain.y);
   Release((void **) &domain.x);

   Release((void **) &domain.elemMass);
   Release((void **) &domain.ss);
   Release((void **) &domain.arealg);

   Release((void **) &domain.vdov);
   Release((void **) &domain.delv);
   Release((void **) &domain.volo);
   Release((void **) &domain.v);

   Release((void **) &domain.qq);
   Release((void **) &domain.ql);
   Release((void **) &domain.q);

   Release((void **) &domain.p);
   Release((void **) &domain.e);

   Release((void **) &domain.elemBC);

   Release((void **) &domain.lzetap);
   Release((void **) &domain.lzetam);
   Release((void **) &domain.letap);
   Release((void **) &domain.letam);
   Release((void **) &domain.lxip);
   Release((void **) &domain.lxim);

   Release((void **) &domain.nodelist);

   return 0;
}
