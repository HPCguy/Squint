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

   int *nodelist ;     /* elemToNode connectivity */

   int *lxim ;         /* elem connectivity through face */
   int *lxip ;
   int *letam ;
   int *letap ;
   int *lzetam ;
   int *lzetap ;

   int *elemBC ;         /* elem face symm/free-surface flag */

   float *e ;             /* energy */

   float *p ;             /* pressure */

   float *q ;             /* q */
   float *ql ;            /* linear term for q */
   float *qq ;            /* quadratic term for q */

   float *v ;             /* relative volume */

   float *volo ;          /* reference volume */
   float *delv ;          /* m_vnew - m_v */
   float *vdov ;          /* volume derivative over volume */

   float *arealg ;        /* elem characteristic length */

   float *ss ;            /* "sound speed" */

   float *elemMass ;      /* mass */

   /* Elem temporaries */

   float *vnew ;          /* new relative volume -- temporary */

   float *delv_xi ;       /* velocity gradient -- temporary */
   float *delv_eta ;
   float *delv_zeta ;

   float *delx_xi ;       /* position gradient -- temporary */
   float *delx_eta ;
   float *delx_zeta ;

   float *dxx ;          /* principal strains -- temporary */
   float *dyy ;
   float *dzz ;

   /* Node-centered */

   float *x ;             /* coordinates */
   float *y ;
   float *z ;

   float *xd ;            /* velocities */
   float *yd ;
   float *zd ;

   float *xdd ;           /* accelerations */
   float *ydd ;
   float *zdd ;

   float *fx ;            /* forces */
   float *fy ;
   float *fz ;

   float *nodalMass ;     /* mass */


   /* Boundary nodesets */

   int *symmX ;        /* Nodes on X symmetry plane */
   int *symmY ;        /* Nodes on Y symmetry plane */
   int *symmZ ;        /* Nodes on Z symmetry plane */

   /* Parameters */

   float  dtfixed ;           /* fixed time increment */
   float  time ;              /* current time */
   float  deltatime ;         /* variable time increment */
   float  deltatimemultlb ;
   float  deltatimemultub ;
   float  stoptime ;          /* end time for simulation */

   float  u_cut ;             /* velocity tolerance */
   float  hgcoef ;            /* hourglass control */
   float  qstop ;             /* excessive q indicator */
   float  monoq_max_slope ;
   float  monoq_limiter_mult ;
   float  e_cut ;             /* energy tolerance */
   float  p_cut ;             /* pressure tolerance */
   float  ss4o3 ;
   float  q_cut ;             /* q tolerance */
   float  v_cut ;             /* relative volume tolerance */
   float  qlc_monoq ;         /* linear term coef for q */
   float  qqc_monoq ;         /* quadratic term coef for q */
   float  qqc ;
   float  eosvmax ;
   float  eosvmin ;
   float  pmin ;              /* pressure floor */
   float  emin ;              /* energy floor */
   float  dvovmax ;           /* maximum allowable volume change */
   float  refdens ;           /* reference density */

   float  dtcourant ;         /* courant raint */
   float  dthydro ;           /* volume change raint */
   float  dtmax ;             /* maximum allowable time increment */

   int   cycle ;             /* iteration count for simulation */

   int sizeX ;
   int sizeY ;
   int sizeZ ;
   int numElem ;

   int numNode ;
} Domain;

float *dvdx_;
float *dvdy_;
float *dvdz_;
float *x8n_;
float *y8n_;
float *z8n_;
float *sigxx_;
float *sigyy_;
float *sigzz_;
float *determ_;
float * p_old_;
float * compression_;
float * compHalfStep_;
float * work_;
float * p_new_;
float * e_new_;
float * q_new_;
float * bvc_;
float * pbvc_;
float * pHalfStep_;
float * vnewc_;

float *AllocateFloat(int size)
{
   float *retVal ;
   posix_memalign((void **)&retVal, 32, size*sizeof(float));
   return retVal ;
}

int *AllocateInt(int size)
{
   int *retVal ;
   posix_memalign((void **)&retVal, 32, size*sizeof(int));
   return retVal ;
}

void Release(void **ptr)
{
   if (*ptr != (void *) 0) {
      free(*ptr) ;
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
   float targetdt = domain->stoptime - domain->time ;

   if ((domain->dtfixed <= ZERO) && (domain->cycle != 0)) {
      float ratio ;
      float olddt = domain->deltatime ;

      /* This will require a reduction in parallel */
      float newdt = 1.0e+20 ;
      if (domain->dtcourant < newdt) {
         newdt = domain->dtcourant / 2.0f ;
      }
      if (domain->dthydro < newdt) {
         newdt = domain->dthydro * 2.0f / 3.0f ;
      }

      ratio = newdt / olddt ;
      if (ratio >= ONE) {
         if (ratio < domain->deltatimemultlb) {
            newdt = olddt ;
         }
         else if (ratio > domain->deltatimemultub) {
            newdt = olddt*domain->deltatimemultub ;
         }
      }

      if (newdt > domain->dtmax) {
         newdt = domain->dtmax ;
      }
      domain->deltatime = newdt ;
   }

   /* TRY TO PREVENT VERY SMALL SCALING ON THE NEXT CYCLE */
   if ((targetdt > domain->deltatime) &&
       (targetdt < (4.0f * domain->deltatime / 3.0f)) ) {
      targetdt = 2.0f * domain->deltatime / 3.0f ;
   }

   if (targetdt < domain->deltatime) {
      domain->deltatime = targetdt ;
   }

   domain->time += domain->deltatime ;

   ++domain->cycle ;
}

void InitStressTermsForElems(float *p, float *q,
                             float *sigxx, float *sigyy, float *sigzz,
                             int numElem)
{
   //
   // pull in the stresses appropriate to the hydro integration
   //

   for (int idx=0; idx<numElem; ++idx) {
      sigxx[idx] = sigyy[idx] = sigzz[idx] =  - p[idx] - q[idx] ;
   }
}

void CalcElemShapeFunctionDerivatives( float *x, float *y, float *z,
                                       float b[][8], float *volume)
{
   float fjxxi, fjxet, fjxze;
   float fjyxi, fjyet, fjyze;
   float fjzxi, fjzet, fjzze;

   {
      float x0 := x[0] ;    float x1 := x[1] ;
      float x2 := x[2] ;    float x3 := x[3] ;
      float x4 := x[4] ;    float x5 := x[5] ;
      float x6 := x[6] ;    float x7 := x[7] ;

      float t1, t2, t3, t4;

      t1 = (x6-x0); t2 = (x5-x3); t3 = (x7-x1); t4 = (x4-x2);

      fjxxi = 0.125f * ( t1 + t2 - t3 - t4 );
      fjxet = 0.125f * ( t1 - t2 + t3 - t4 );
      fjxze = 0.125f * ( t1 + t2 + t3 + t4 );
   }

   {
      float y0 := y[0] ;    float y1 := y[1] ;
      float y2 := y[2] ;    float y3 := y[3] ;
      float y4 := y[4] ;    float y5 := y[5] ;
      float y6 := y[6] ;    float y7 := y[7] ;

      float t1, t2, t3, t4;

      t1 = (y6-y0); t2 = (y5-y3); t3 = (y7-y1); t4 = (y4-y2);

      fjyxi = 0.125f * ( t1 + t2 - t3 - t4 );
      fjyet = 0.125f * ( t1 - t2 + t3 - t4 );
      fjyze = 0.125f * ( t1 + t2 + t3 + t4 );
   }

   {
      float z0 := z[0] ;    float z1 := z[1] ;
      float z2 := z[2] ;    float z3 := z[3] ;
      float z4 := z[4] ;    float z5 := z[5] ;
      float z6 := z[6] ;    float z7 := z[7] ;

      float t1, t2, t3, t4;

      t1 = (z6-z0); t2 = (z5-z3); t3 = (z7-z1); t4 = (z4-z2);

      fjzxi = 0.125f * ( t1 + t2 - t3 - t4 );
      fjzet = 0.125f * ( t1 - t2 + t3 - t4 );
      fjzze = 0.125f * ( t1 + t2 + t3 + t4 );
   }

   float cjxxi, cjxet, cjxze;
   float cjyxi, cjyet, cjyze;
   float cjzxi, cjzet, cjzze;

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

inline void SumElemFaceNormal(float *normalX0, float *normalY0, float *normalZ0,
                       float *normalX1, float *normalY1, float *normalZ1,
                       float *normalX2, float *normalY2, float *normalZ2,
                       float *normalX3, float *normalY3, float *normalZ3,
                        float x0,  float y0,  float z0,
                        float x1,  float y1,  float z1,
                        float x2,  float y2,  float z2,
                        float x3,  float y3,  float z3)
{
   float bisectX0 = x3 + x2 - x1 - x0;
   float bisectY0 = y3 + y2 - y1 - y0;
   float bisectZ0 = z3 + z2 - z1 - z0;
   float bisectX1 = x2 + x1 - x3 - x0;
   float bisectY1 = y2 + y1 - y3 - y0;
   float bisectZ1 = z2 + z1 - z3 - z0;
   float areaX = 0.0625f * (bisectY0 * bisectZ1 - bisectZ0 * bisectY1);
   float areaY = 0.0625f * (bisectZ0 * bisectX1 - bisectX0 * bisectZ1);
   float areaZ = 0.0625f * (bisectX0 * bisectY1 - bisectY0 * bisectX1);

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

void CalcElemNodeNormals(float *pfx, float *pfy, float *pfz,
                         float *x, float *y, float *z)
{
   for (int i = 0 ; i < 8 ; ++i) {
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

void SumElemStressesToNodeForces(float B[][8], float stress_xx,
                                 float stress_yy, float stress_zz,
                                 float *fx, float *fy, float *fz)
{
  float pfx0 := B[0][0] ;   float pfx1 := B[0][1] ;
  float pfx2 := B[0][2] ;   float pfx3 := B[0][3] ;
  float pfx4 := B[0][4] ;   float pfx5 := B[0][5] ;
  float pfx6 := B[0][6] ;   float pfx7 := B[0][7] ;

  float pfy0 := B[1][0] ;   float pfy1 := B[1][1] ;
  float pfy2 := B[1][2] ;   float pfy3 := B[1][3] ;
  float pfy4 := B[1][4] ;   float pfy5 := B[1][5] ;
  float pfy6 := B[1][6] ;   float pfy7 := B[1][7] ;

  float pfz0 := B[2][0] ;   float pfz1 := B[2][1] ;
  float pfz2 := B[2][2] ;   float pfz3 := B[2][3] ;
  float pfz4 := B[2][4] ;   float pfz5 := B[2][5] ;
  float pfz6 := B[2][6] ;   float pfz7 := B[2][7] ;

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


void IntegrateStressForElems(int *nodelist,
                             float *x,  float *y,  float *z,
                             float *fx, float *fy, float *fz,
                             float *sigxx, float *sigyy, float *sigzz,
                             float *determ, int numElem)
{
  int lnode, gnode, gnode2;
  int *elemNodes;

  // loop over all elements
  for (int k=0; k<numElem; ++k) {
    float  B[3][8] ;// shape function derivatives

    elemNodes = &nodelist[8*k];

    {
      float  x_local[8] ;
      float  y_local[8] ;
      float  z_local[8] ;

      // get coordinates from global arrays and copy into local arrays.
      for(lnode=0 ; lnode<8 ; ++lnode )
      {
        gnode = elemNodes[lnode];
        x_local[lnode] = x[gnode];
        y_local[lnode] = y[gnode];
        z_local[lnode] = z[gnode];
      }

      /* Volume calculation involves extra work for numerical consistency. */
      CalcElemShapeFunctionDerivatives(x_local, y_local, z_local,
                                       B, &determ[k]);

      CalcElemNodeNormals( B[0] , B[1], B[2],
                           x_local, y_local, z_local );
    }

    {
      float  fx_local[8] ;
      float  fy_local[8] ;
      float  fz_local[8] ;

      SumElemStressesToNodeForces( B, sigxx[k], sigyy[k], sigzz[k],
                                   fx_local, fy_local, fz_local ) ;

      // copy nodal force contributions to global force arrray.
      for(lnode=0 ; lnode<8 ; ++lnode )
      {
        gnode2 = elemNodes[lnode];
        fx[gnode2] += fx_local[lnode];
        fy[gnode2] += fy_local[lnode];
        fz[gnode2] += fz_local[lnode];
      }
    }
  }
}

void CollectDomainNodesToElemNodes(float *x, float *y, float *z,
                                   int *elemToNode,
                                   float *elemX, float *elemY, float *elemZ)
{
   int nd0i := elemToNode[0] ;
   int nd1i := elemToNode[1] ;
   int nd2i := elemToNode[2] ;
   int nd3i := elemToNode[3] ;
   int nd4i := elemToNode[4] ;
   int nd5i := elemToNode[5] ;
   int nd6i := elemToNode[6] ;
   int nd7i := elemToNode[7] ;

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

inline void VoluDer(float x0,  float x1,  float x2,
             float x3,  float x4,  float x5,
             float y0,  float y1,  float y2,
             float y3,  float y4,  float y5,
             float z0,  float z1,  float z2,
             float z3,  float z4,  float z5,
             float *dvdx, float *dvdy, float *dvdz)
{
    // float twelfth = ONE / 12.0f ;

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

void CalcElemVolumeDerivative(float *dvdx, float *dvdy, float *dvdz,
                              float *x, float *y, float *z)
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

void CalcElemFBHourglassForce(float *xd, float *yd, float *zd,
                              float hourgam[][8], float coefficient,
                              float *hgfx, float *hgfy, float *hgfz)
{
/*
    int i00=0;
    int i01=1;
    int i02=2;
    int i03=3;
*/
   enum { i00, i01, i02, i03 };

   float h00 =
      hourgam[i00][0] * xd[0] + hourgam[i00][1] * xd[1] +
      hourgam[i00][2] * xd[2] + hourgam[i00][3] * xd[3] +
      hourgam[i00][4] * xd[4] + hourgam[i00][5] * xd[5] +
      hourgam[i00][6] * xd[6] + hourgam[i00][7] * xd[7];

   float h01 =
      hourgam[i01][0] * xd[0] + hourgam[i01][1] * xd[1] +
      hourgam[i01][2] * xd[2] + hourgam[i01][3] * xd[3] +
      hourgam[i01][4] * xd[4] + hourgam[i01][5] * xd[5] +
      hourgam[i01][6] * xd[6] + hourgam[i01][7] * xd[7];

   float h02 =
      hourgam[i02][0] * xd[0] + hourgam[i02][1] * xd[1] +
      hourgam[i02][2] * xd[2] + hourgam[i02][3] * xd[3] +
      hourgam[i02][4] * xd[4] + hourgam[i02][5] * xd[5] +
      hourgam[i02][6] * xd[6] + hourgam[i02][7] * xd[7];

   float h03 =
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

float gammaa[4][8] =
{
   {  ONE,  ONE, -ONE, -ONE, -ONE, -ONE, ONE,  ONE },
   {  ONE, -ONE, -ONE,  ONE, -ONE,  ONE, ONE, -ONE },
   {  ONE, -ONE,  ONE, -ONE,  ONE, -ONE, ONE, -ONE },
   { -ONE,  ONE, -ONE,  ONE,  ONE, -ONE, ONE, -ONE }
} ;

void FBKernel(float *x8ni, float *y8ni, float *z8ni,
              float *dvdxi, float *dvdyi, float *dvdzi,
              float hourgam[][8], float volinv)
{
   for(int i1=0;i1<4;++i1){
      float *gami = &gammaa[i1][0];
      float *hg = &hourgam[i1][0];

      float hourmodx =
         x8ni[0] * gami[0] + x8ni[1] * gami[1] +
         x8ni[2] * gami[2] + x8ni[3] * gami[3] +
         x8ni[4] * gami[4] + x8ni[5] * gami[5] +
         x8ni[6] * gami[6] + x8ni[7] * gami[7];

      float hourmody =
         y8ni[0] * gami[0] + y8ni[1] * gami[1] +
         y8ni[2] * gami[2] + y8ni[3] * gami[3] +
         y8ni[4] * gami[4] + y8ni[5] * gami[5] +
         y8ni[6] * gami[6] + y8ni[7] * gami[7];

      float hourmodz =
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

void CalcFBHourglassForceForElems(int * nodelist,
                                  float *ss, float *elemMass,
                                  float *xd, float *yd, float *zd,
                                  float *fx, float *fy, float *fz,
                                  float *determ,
                                  float *x8n, float *y8n, float *z8n,
                                  float *dvdx, float *dvdy, float *dvdz,
                                  float hourg, int numElem)
{
   /*************************************************
    *
    *     FUNCTION: Calculates the Flanagan-Belytschko anti-hourglass
    *               force.
    *
    *************************************************/

/*************************************************/
/*    compute the hourglass modes */

   for (int i2=0; i2<numElem; ++i2) {
      float coefficient;
      int *elemToNode = &nodelist[8*i2];
      int i3=8*i2;
      // float volinv = ONE/determ[i2];
      float ss1, mass1, volume13 ;
      // float hourmodx, hourmody, hourmodz;
      int n0si2, n1si2, n2si2, n3si2;
      int n4si2, n5si2, n6si2, n7si2;
      float hourgam[4][8];

      FBKernel( &x8n[i3],  &y8n[i3],  &z8n[i3],
               &dvdx[i3], &dvdy[i3], &dvdz[i3],
               hourgam, ONE/determ[i2]);

      /* compute forces */
      /* store forces into h arrays (force arrays) */

      ss1=ss[i2];
      mass1=elemMass[i2];
      volume13 = cbrtf(determ[i2]);

      n0si2 = elemToNode[0];
      n1si2 = elemToNode[1];
      n2si2 = elemToNode[2];
      n3si2 = elemToNode[3];
      n4si2 = elemToNode[4];
      n5si2 = elemToNode[5];
      n6si2 = elemToNode[6];
      n7si2 = elemToNode[7];

      float xd1[8], yd1[8], zd1[8] ;
      float hgfx[8], hgfy[8], hgfz[8] ;

      xd1[0] = xd[n0si2];
      xd1[1] = xd[n1si2];
      xd1[2] = xd[n2si2];
      xd1[3] = xd[n3si2];
      xd1[4] = xd[n4si2];
      xd1[5] = xd[n5si2];
      xd1[6] = xd[n6si2];
      xd1[7] = xd[n7si2];

      yd1[0] = yd[n0si2];
      yd1[1] = yd[n1si2];
      yd1[2] = yd[n2si2];
      yd1[3] = yd[n3si2];
      yd1[4] = yd[n4si2];
      yd1[5] = yd[n5si2];
      yd1[6] = yd[n6si2];
      yd1[7] = yd[n7si2];

      zd1[0] = zd[n0si2];
      zd1[1] = zd[n1si2];
      zd1[2] = zd[n2si2];
      zd1[3] = zd[n3si2];
      zd1[4] = zd[n4si2];
      zd1[5] = zd[n5si2];
      zd1[6] = zd[n6si2];
      zd1[7] = zd[n7si2];

      coefficient = - hourg * 0.01f * ss1 * mass1 / volume13;

      CalcElemFBHourglassForce(xd1,yd1,zd1,
                      hourgam, coefficient, hgfx, hgfy, hgfz);

      fx[n0si2] += hgfx[0];
      fy[n0si2] += hgfy[0];
      fz[n0si2] += hgfz[0];

      fx[n1si2] += hgfx[1];
      fy[n1si2] += hgfy[1];
      fz[n1si2] += hgfz[1];

      fx[n2si2] += hgfx[2];
      fy[n2si2] += hgfy[2];
      fz[n2si2] += hgfz[2];

      fx[n3si2] += hgfx[3];
      fy[n3si2] += hgfy[3];
      fz[n3si2] += hgfz[3];

      fx[n4si2] += hgfx[4];
      fy[n4si2] += hgfy[4];
      fz[n4si2] += hgfz[4];

      fx[n5si2] += hgfx[5];
      fy[n5si2] += hgfy[5];
      fz[n5si2] += hgfz[5];

      fx[n6si2] += hgfx[6];
      fy[n6si2] += hgfy[6];
      fz[n6si2] += hgfz[6];

      fx[n7si2] += hgfx[7];
      fy[n7si2] += hgfy[7];
      fz[n7si2] += hgfz[7];
   }
}

void CalcHourglassControlForElems(Domain *domain, float *determ, float hgcoef)
{
   int numElem = domain->numElem ;
   float * dvdx = dvdx_;
   float * dvdy = dvdy_;
   float * dvdz = dvdz_;
   float * x8n  = x8n_;
   float * y8n  = y8n_;
   float * z8n  = z8n_;

   /* start loop over elements */
   for (int idx=0; idx<numElem; ++idx) {
      float  x1[8],  y1[8],  z1[8] ;
      float pfx[8], pfy[8], pfz[8] ;

      int * elemToNode = &domain->nodelist[8*idx];
      CollectDomainNodesToElemNodes(domain->x, domain->y, domain->z,
                                    elemToNode, x1, y1, z1);

      CalcElemVolumeDerivative(pfx, pfy, pfz, x1, y1, z1);

      /* load into temporary storage for FB Hour Glass control */
      for(int ii=0;ii<8;++ii){
         int jj=8*idx+ii;

         dvdx[jj] = pfx[ii];
         dvdy[jj] = pfy[ii];
         dvdz[jj] = pfz[ii];
         x8n[jj]  = x1[ii];
         y8n[jj]  = y1[ii];
         z8n[jj]  = z1[ii];
      }

      determ[idx] = domain->volo[idx] * domain->v[idx];

      /* Do a check for negative volumes */
      if ( domain->v[idx] <= ZERO ) {
         exit(VolumeError) ;
      }
   }

   if ( hgcoef > ZERO ) {
      CalcFBHourglassForceForElems( domain->nodelist,
                                    domain->ss, domain->elemMass,
                                    domain->xd, domain->yd, domain->zd,
                                    domain->fx, domain->fy, domain->fz,
                                    determ, x8n, y8n, z8n, dvdx, dvdy, dvdz,
                                    hgcoef, numElem) ;
   }

   // Release((void **) &z8n) ;
   // Release((void **) &y8n) ;
   // Release((void **) &x8n) ;
   // Release((void **) &dvdz) ;
   // Release((void **) &dvdy) ;
   // Release((void **) &dvdx) ;

   return ;
}

void CalcVolumeForceForElems(Domain *domain)
{
   int numElem = domain->numElem ;
   if (numElem != 0) {
      float  hgcoef = domain->hgcoef ;
      float * sigxx  = sigxx_;
      float * sigyy  = sigyy_;
      float * sigzz  = sigzz_;
      float * determ = determ_;

      /* Sum contributions to total stress tensor */
      InitStressTermsForElems(domain->p, domain->q,
                              sigxx, sigyy, sigzz, numElem);

      // call elemlib stress integration loop to produce nodal forces from
      // material stresses.
      IntegrateStressForElems( domain->nodelist,
                               domain->x, domain->y, domain->z,
                               domain->fx, domain->fy, domain->fz,
                               sigxx, sigyy, sigzz, determ, numElem) ;

      // check for negative element volume
      for (int k=0; k<numElem; ++k) {
         if (determ[k] <= ZERO) {
            exit(VolumeError) ;
         }
      }

      CalcHourglassControlForElems(domain, determ, hgcoef) ;

      // Release((void **) &determ) ;
      // Release((void **) &sigzz) ;
      // Release((void **) &sigyy) ;
      // Release((void **) &sigxx) ;
   }
}

void CalcForceForNodes(Domain *domain)
{
  int numNode = domain->numNode ;
  for (int i=0; i<numNode; ++i) {
     domain->fx[i] = ZERO ;
     domain->fy[i] = ZERO ;
     domain->fz[i] = ZERO ;
  }
}

void CalcAccelerationForNodes(float *xdd, float *ydd, float *zdd,
                              float *fx, float *fy, float *fz,
                              float *nodalMass, int numNode)
{
   for (int i=0; i<numNode; ++i) {
      xdd[i] = fx[i] / nodalMass[i];
      ydd[i] = fy[i] / nodalMass[i];
      zdd[i] = fz[i] / nodalMass[i];
   }
}

void ApplyAccelerationBoundaryConditionsForNodes(float *xdd, float *ydd,
                                                 float *zdd, int *symmX,
                                                 int *symmY,
                                                 int *symmZ, int size)
{
  int numNodeBC = (size+1)*(size+1) ;

  for (int i=0; i<numNodeBC; ++i) {
     xdd[symmX[i]] = ZERO ;
     ydd[symmY[i]] = ZERO ;
     zdd[symmZ[i]] = ZERO ;
  }
}

void CalcVelocityForNodes(float *xd,  float *yd,  float *zd,
                          float *xdd, float *ydd, float *zdd,
                          float dt,  float u_cut, int numNode)
{
   for (int i=0; i<numNode; ++i) {
     float xt, yt, zt;
     float xdtmp, ydtmp, zdtmp ;

     xdtmp = xt = xd[i] + xdd[i] * dt ;
     if ( fabsf(xt) < u_cut ) xdtmp = ZERO;
     xd[i] = xdtmp ;

     ydtmp = yt = yd[i] + ydd[i] * dt ;
     if( fabsf(yt) < u_cut ) ydtmp = ZERO;
     yd[i] = ydtmp ;

     zdtmp = zt = zd[i] + zdd[i] * dt ;
     if( fabsf(zt) < u_cut ) zdtmp = ZERO;
     zd[i] = zdtmp ;
   }
}

void CalcPositionForNodes(float *x,  float *y,  float *z,
                          float *xd, float *yd, float *zd,
                          float dt, int numNode)
{
   for (int i=0; i<numNode; ++i) {
     x[i] += xd[i] * dt ;
     y[i] += yd[i] * dt ;
     z[i] += zd[i] * dt ;
   }
}

void LagrangeNodal(Domain *domain)
{
   float delt = domain->deltatime ;
  float u_cut = domain->u_cut ;

  /* time of boundary condition evaluation is beginning of step for force and
   * acceleration boundary conditions. */
  CalcForceForNodes(domain);

  /* Calcforce calls partial, force, hourq */
  CalcVolumeForceForElems(domain) ;

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
                        delt, u_cut, domain->numNode) ;

  CalcPositionForNodes( domain->x,  domain->y,  domain->z,
                        domain->xd, domain->yd, domain->zd,
                        delt, domain->numNode );

  return;
}

inline void TRIPLE_PRODUCT(float x1_, float y1_, float z1_,
                     float x2_, float y2_, float z2_,
                     float x3_, float y3_, float z3_, float *pv_)
{
   *pv_ += (x1_*(y2_*z3_ - z2_*y3_) +
            x2_*(z1_*y3_ - y1_*z3_) +
            x3_*(y1_*z2_ - z1_*y2_));
}


inline void CalcElemVolume2(float x0, float x1, float x2,  float x3,
                     float x4,  float x5, float x6,  float x7,
                     float y0,  float y1, float y2,  float y3,
                     float y4,  float y5, float y6,  float y7,
                     float z0,  float z1, float z2,  float z3,
                     float z4,  float z5, float z6,  float z7, float *fv)
{
  // float twelfth = ONE / 12.0f;

  *fv = ZERO;
  {
     float dx31 = x3 - x1;
     float dy31 = y3 - y1;
     float dz31 = z3 - z1;

     float dx72 = x7 - x2;
     float dy72 = y7 - y2;
     float dz72 = z7 - z2;

     float s1 = dx31 + dx72;
     float s2 = dy31 + dy72;
     float s3 = dz31 + dz72;

     float dx63 = x6 - x3;
     float dy63 = y6 - y3;
     float dz63 = z6 - z3;

     float dx20 = x2 - x0;
     float dy20 = y2 - y0;
     float dz20 = z2 - z0;

     TRIPLE_PRODUCT(s1, dx63, dx20,
                    s2, dy63, dy20,
                    s3, dz63, dz20, fv);
  }
  {
     float dx43 = x4 - x3;
     float dy43 = y4 - y3;
     float dz43 = z4 - z3;

     float dx57 = x5 - x7;
     float dy57 = y5 - y7;
     float dz57 = z5 - z7;

     float s1 = dx43 + dx57;
     float s2 = dy43 + dy57;
     float s3 = dz43 + dz57;

     float dx64 = x6 - x4;
     float dy64 = y6 - y4;
     float dz64 = z6 - z4;

     float dx70 = x7 - x0;
     float dy70 = y7 - y0;
     float dz70 = z7 - z0;

     TRIPLE_PRODUCT(s1, dx64, dx70,
                    s2, dy64, dy70,
                    s3, dz64, dz70, fv);
  }
  {
     float dx14 = x1 - x4;
     float dy14 = y1 - y4;
     float dz14 = z1 - z4;

     float dx25 = x2 - x5;
     float dy25 = y2 - y5;
     float dz25 = z2 - z5;

     float s1 = dx14 + dx25;
     float s2 = dy14 + dy25;
     float s3 = dz14 + dz25;

     float dx61 = x6 - x1;
     float dy61 = y6 - y1;
     float dz61 = z6 - z1;

     float dx50 = x5 - x0;
     float dy50 = y5 - y0;
     float dz50 = z5 - z0;

     TRIPLE_PRODUCT(s1, dx61, dx50,
                    s2, dy61, dy50,
                    s3, dz61, dz50, fv);
   }

  // volume *= twelfth;

  // return volume * twelfth ;

  *fv *= twelfth;
}

float CalcElemVolume(float *x, float *y, float *z)
{
   float fv;
   CalcElemVolume2( x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7],
                    y[0], y[1], y[2], y[3], y[4], y[5], y[6], y[7],
                    z[0], z[1], z[2], z[3], z[4], z[5], z[6], z[7], &fv);
   return fv;
}

inline void AreaFace(float x0, float x1, float x2, float x3,
               float y0, float y1, float y2, float y3,
               float z0, float z1, float z2, float z3, float *area)
{
   float dx1 = (x2 - x0);
   float dx2 = (x3 - x1);
   float dy1 = (y2 - y0);
   float dy2 = (y3 - y1);
   float dz1 = (z2 - z0);
   float dz2 = (z3 - z1);
   float fx = dx1 - dx2;
   float fy = dy1 - dy2;
   float fz = dz1 - dz2;
   float gx = dx1 + dx2;
   float gy = dy1 + dy2;
   float gz = dz1 + dz2;
   float term = (fx * gx + fy * gy + fz * gz);
   *area =
      (fx * fx + fy * fy + fz * fz) *
      (gx * gx + gy * gy + gz * gz) -
      term * term;
}

float CalcElemCharacteristicLength(float x[8], float y[8], float z[8],
                                   float volume)
{
   float a;
   float charLength = ZERO;

   AreaFace(x[0],x[1],x[2],x[3],
            y[0],y[1],y[2],y[3],
            z[0],z[1],z[2],z[3], &a) ;
   charLength = fmaxf(a,charLength) ;

   AreaFace(x[4],x[5],x[6],x[7],
            y[4],y[5],y[6],y[7],
            z[4],z[5],z[6],z[7], &a) ;
   charLength = fmaxf(a,charLength) ;

   AreaFace(x[0],x[1],x[5],x[4],
            y[0],y[1],y[5],y[4],
            z[0],z[1],z[5],z[4], &a) ;
   charLength = fmaxf(a,charLength);

   AreaFace(x[1],x[2],x[6],x[5],
            y[1],y[2],y[6],y[5],
            z[1],z[2],z[6],z[5], &a) ;
   charLength = fmaxf(a,charLength) ;

   AreaFace(x[2],x[3],x[7],x[6],
            y[2],y[3],y[7],y[6],
            z[2],z[3],z[7],z[6], &a) ;
   charLength = fmaxf(a,charLength) ;

   AreaFace(x[3],x[0],x[4],x[7],
            y[3],y[0],y[4],y[7],
            z[3],z[0],z[4],z[7], &a) ;
   charLength = fmaxf(a,charLength) ;

   charLength = (4.0f * volume) / sqrtf( charLength );

   return charLength;
}

void CalcElemVelocityGrandient(float *xvel, float *yvel, float *zvel,
                               float b[][8], float detJ, float *d)
{
   float inv_detJ = ONE / detJ ;
   float dyddx, dxddy, dzddx, dxddz, dzddy, dyddz;
   float*  pfx = &b[0][0];
   float*  pfy = &b[1][0];
   float*  pfz = &b[2][0];

  d[0] = inv_detJ * ( pfx[0] * (xvel[0]-xvel[6])
                     + pfx[1] * (xvel[1]-xvel[7])
                     + pfx[2] * (xvel[2]-xvel[4])
                     + pfx[3] * (xvel[3]-xvel[5]) );

  d[1] = inv_detJ * ( pfy[0] * (yvel[0]-yvel[6])
                     + pfy[1] * (yvel[1]-yvel[7])
                     + pfy[2] * (yvel[2]-yvel[4])
                     + pfy[3] * (yvel[3]-yvel[5]) );

  d[2] = inv_detJ * ( pfz[0] * (zvel[0]-zvel[6])
                     + pfz[1] * (zvel[1]-zvel[7])
                     + pfz[2] * (zvel[2]-zvel[4])
                     + pfz[3] * (zvel[3]-zvel[5]) );

  dyddx  = inv_detJ * ( pfx[0] * (yvel[0]-yvel[6])
                      + pfx[1] * (yvel[1]-yvel[7])
                      + pfx[2] * (yvel[2]-yvel[4])
                      + pfx[3] * (yvel[3]-yvel[5]) );

  dxddy  = inv_detJ * ( pfy[0] * (xvel[0]-xvel[6])
                      + pfy[1] * (xvel[1]-xvel[7])
                      + pfy[2] * (xvel[2]-xvel[4])
                      + pfy[3] * (xvel[3]-xvel[5]) );

  dzddx  = inv_detJ * ( pfx[0] * (zvel[0]-zvel[6])
                      + pfx[1] * (zvel[1]-zvel[7])
                      + pfx[2] * (zvel[2]-zvel[4])
                      + pfx[3] * (zvel[3]-zvel[5]) );

  dxddz  = inv_detJ * ( pfz[0] * (xvel[0]-xvel[6])
                      + pfz[1] * (xvel[1]-xvel[7])
                      + pfz[2] * (xvel[2]-xvel[4])
                      + pfz[3] * (xvel[3]-xvel[5]) );

  dzddy  = inv_detJ * ( pfy[0] * (zvel[0]-zvel[6])
                      + pfy[1] * (zvel[1]-zvel[7])
                      + pfy[2] * (zvel[2]-zvel[4])
                      + pfy[3] * (zvel[3]-zvel[5]) );

  dyddz  = inv_detJ * ( pfz[0] * (yvel[0]-yvel[6])
                      + pfz[1] * (yvel[1]-yvel[7])
                      + pfz[2] * (yvel[2]-yvel[4])
                      + pfz[3] * (yvel[3]-yvel[5]) );
  d[5]  = ( dxddy + dyddx ) * HALF;
  d[4]  = ( dxddz + dzddx ) * HALF;
  d[3]  = ( dzddy + dyddz ) * HALF;
}

void CalcKinematicsForElems(int *nodelist,
                            float *x, float *y, float *z,
                            float *xd, float *yd, float *zd,
                            float *dxx, float *dyy, float *dzz,
                            float *v, float *volo,
                            float *vnew, float *delv, float *arealg,
                            float deltaTime, int numElem)
{
  int lnode, gnode, gnode2, j;
  // loop over all elements
  for (int k=0; k<numElem; ++k) {
    float dt2;
    float detJ = ZERO ;
    float volume ;
    float relativeVolume ;
    int * elemToNode = &nodelist[8*k] ;

    float  x_local[8] ;
    float  y_local[8] ;
    float  z_local[8] ;
    float  xd_local[8] ;
    float  yd_local[8] ;
    float  zd_local[8] ;

    /** shape function derivatives */
    float  D[6] ;
    float  B[3][8] ;

    // get nodal coordinates from global arrays and copy into local arrays.
    for(lnode=0 ; lnode<8 ; ++lnode )
    {
      gnode = elemToNode[lnode];
      x_local[lnode] = x[gnode];
      y_local[lnode] = y[gnode];
      z_local[lnode] = z[gnode];
    }

    // volume calculations
    volume = CalcElemVolume(x_local, y_local, z_local );
    relativeVolume = volume / volo[k] ;
    vnew[k] = relativeVolume ;
    delv[k] = relativeVolume - v[k] ;

    // set characteristic length
    arealg[k] = CalcElemCharacteristicLength(x_local, y_local, z_local,
                                             volume);

    // get nodal velocities from global array and copy into local arrays.
    for(lnode=0 ; lnode<8 ; ++lnode )
    {
      gnode2 = elemToNode[lnode];
      xd_local[lnode] = xd[gnode2];
      yd_local[lnode] = yd[gnode2];
      zd_local[lnode] = zd[gnode2];
    }

    dt2 = deltaTime * HALF;
    for (j=0 ; j<8 ; ++j )
    {
       x_local[j] -= dt2 * xd_local[j];
       y_local[j] -= dt2 * yd_local[j];
       z_local[j] -= dt2 * zd_local[j];
    }

    CalcElemShapeFunctionDerivatives( x_local, y_local, z_local,
                                      B, &detJ );

    CalcElemVelocityGrandient( xd_local, yd_local, zd_local,
                               B, detJ, D );

    // put velocity gradient quantities into their global arrays.
    dxx[k] = D[0];
    dyy[k] = D[1];
    dzz[k] = D[2];
  }
}

void CalcLagrangeElements(Domain *domain)
{
   int numElem = domain->numElem ;
   if (numElem > 0) {
       float deltatime = domain->deltatime ;

      // domain->dxx  = AllocateFloat(numElem) ; /* principal strains */
      // domain->dyy  = AllocateFloat(numElem) ;
      // domain->dzz  = AllocateFloat(numElem) ;

      CalcKinematicsForElems(domain->nodelist,
                             domain->x, domain->y, domain->z,
                             domain->xd, domain->yd, domain->zd,
                             domain->dxx, domain->dyy, domain->dzz,
                             domain->v, domain->volo,
                             domain->vnew, domain->delv, domain->arealg,
                             deltatime, numElem) ;

      // element loop to do some stuff not included in the elemlib function.
      for (int k=0; k<numElem; ++k) {
        // calc strain rate and apply as raint (only done in FB element)
        float vdov = domain->dxx[k] + domain->dyy[k] + domain->dzz[k] ;
        float vdovthird = vdov * (ONE / 3.0f ) ;
        
        // make the rate of deformation tensor deviatoric
        domain->vdov[k] = vdov ;
        domain->dxx[k] -= vdovthird ;
        domain->dyy[k] -= vdovthird ;
        domain->dzz[k] -= vdovthird ;

        // See if any volumes are negative, and take appropriate action.
        if (domain->vnew[k] <= ZERO)
        {
           exit(VolumeError) ;
        }
      }

      // Release((void **) &domain->dzz) ;
      // Release((void **) &domain->dyy) ;
      // Release((void **) &domain->dxx) ;
   }
}

void CalcMonotonicQGradientsForElems(float *x, float *y, float *z,
                                     float *xd, float *yd, float *zd,
                                     float *volo, float *vnew,
                                     float *delv_xi,
                                     float *delv_eta,
                                     float *delv_zeta,
                                     float *delx_xi,
                                     float *delx_eta,
                                     float *delx_zeta,
                                     int *nodelist,
                                     int numElem)
{
   for (int i=0; i<numElem; ++i) {
      float ptiny = 1.0e-36 ;
      float ax, ay, az ;
      float dxv,dyv,dzv ;
      float dxi, dyi, dzi;
      float dxj, dyj, dzj;
      float dxk, dyk, dzk;

      int * elemToNode = &nodelist[8*i];
      int n0 := elemToNode[0] ;
      int n1 := elemToNode[1] ;
      int n2 := elemToNode[2] ;
      int n3 := elemToNode[3] ;
      int n4 := elemToNode[4] ;
      int n5 := elemToNode[5] ;
      int n6 := elemToNode[6] ;
      int n7 := elemToNode[7] ;

      float vol = volo[i]*vnew[i] ;
      float norm = ONE / ( vol + ptiny ) ;

      {
         float x0 = x[n0] ;
         float x1 = x[n1] ;
         float x2 = x[n2] ;
         float x3 = x[n3] ;
         float x4 = x[n4] ;
         float x5 = x[n5] ;
         float x6 = x[n6] ;
         float x7 = x[n7] ;

         float y0 = y[n0] ;
         float y1 = y[n1] ;
         float y2 = y[n2] ;
         float y3 = y[n3] ;
         float y4 = y[n4] ;
         float y5 = y[n5] ;
         float y6 = y[n6] ;
         float y7 = y[n7] ;

         float z0 = z[n0] ;
         float z1 = z[n1] ;
         float z2 = z[n2] ;
         float z3 = z[n3] ;
         float z4 = z[n4] ;
         float z5 = z[n5] ;
         float z6 = z[n6] ;
         float z7 = z[n7] ;

         dxj = -0.25f*((x0 + x1 + x5 + x4) - (x3 + x2 + x6 + x7)) ;
         dyj = -0.25f*((y0 + y1 + y5 + y4) - (y3 + y2 + y6 + y7)) ;
         dzj = -0.25f*((z0 + z1 + z5 + z4) - (z3 + z2 + z6 + z7)) ;

         dxi = 0.25f*((x1 + x2 + x6 + x5) - (x0 + x3 + x7 + x4)) ;
         dyi = 0.25f*((y1 + y2 + y6 + y5) - (y0 + y3 + y7 + y4)) ;
         dzi = 0.25f*((z1 + z2 + z6 + z5) - (z0 + z3 + z7 + z4)) ;

         dxk = 0.25f*((x4 + x5 + x6 + x7) - (x0 + x1 + x2 + x3)) ;
         dyk = 0.25f*((y4 + y5 + y6 + y7) - (y0 + y1 + y2 + y3)) ;
         dzk = 0.25f*((z4 + z5 + z6 + z7) - (z0 + z1 + z2 + z3)) ;
      }

      /* find delvk and delxk ( i cross j ) */

      ax = dyi*dzj - dzi*dyj ;
      ay = dzi*dxj - dxi*dzj ;
      az = dxi*dyj - dyi*dxj ;

      // i type and rhs type conflict?
      delx_zeta[i] = ( vol / sqrtf( ax*ax + ay*ay + az*az + ptiny ) );

      float xv0 = xd[n0] ;
      float xv1 = xd[n1] ;
      float xv2 = xd[n2] ;
      float xv3 = xd[n3] ;
      float xv4 = xd[n4] ;
      float xv5 = xd[n5] ;
      float xv6 = xd[n6] ;
      float xv7 = xd[n7] ;

      float yv0 = yd[n0] ;
      float yv1 = yd[n1] ;
      float yv2 = yd[n2] ;
      float yv3 = yd[n3] ;
      float yv4 = yd[n4] ;
      float yv5 = yd[n5] ;
      float yv6 = yd[n6] ;
      float yv7 = yd[n7] ;

      float zv0 = zd[n0] ;
      float zv1 = zd[n1] ;
      float zv2 = zd[n2] ;
      float zv3 = zd[n3] ;
      float zv4 = zd[n4] ;
      float zv5 = zd[n5] ;
      float zv6 = zd[n6] ;
      float zv7 = zd[n7] ;

      dxv = 0.25f*((xv4 + xv5 + xv6 + xv7) - (xv0 + xv1 + xv2 + xv3)) ;
      dyv = 0.25f*((yv4 + yv5 + yv6 + yv7) - (yv0 + yv1 + yv2 + yv3)) ;
      dzv = 0.25f*((zv4 + zv5 + zv6 + zv7) - (zv0 + zv1 + zv2 + zv3)) ;

      delv_zeta[i] = ( ax*dxv + ay*dyv + az*dzv ) * norm ;

      /* find delxi and delvi ( j cross k ) */

      ax = dyj*dzk - dzj*dyk ;
      ay = dzj*dxk - dxj*dzk ;
      az = dxj*dyk - dyj*dxk ;

      delx_xi[i] = vol / sqrtf(ax*ax + ay*ay + az*az + ptiny) ;

      dxv = 0.25f*((xv1 + xv2 + xv6 + xv5) - (xv0 + xv3 + xv7 + xv4)) ;
      dyv = 0.25f*((yv1 + yv2 + yv6 + yv5) - (yv0 + yv3 + yv7 + yv4)) ;
      dzv = 0.25f*((zv1 + zv2 + zv6 + zv5) - (zv0 + zv3 + zv7 + zv4)) ;

      delv_xi[i] = ( ax*dxv + ay*dyv + az*dzv ) * norm ;

      /* find delxj and delvj ( k cross i ) */

      ax = dyk*dzi - dzk*dyi ;
      ay = dzk*dxi - dxk*dzi ;
      az = dxk*dyi - dyk*dxi ;

      delx_eta[i] = vol / sqrtf(ax*ax + ay*ay + az*az + ptiny) ;

      dxv = -0.25f*((xv0 + xv1 + xv5 + xv4) - (xv3 + xv2 + xv6 + xv7)) ;
      dyv = -0.25f*((yv0 + yv1 + yv5 + yv4) - (yv3 + yv2 + yv6 + yv7)) ;
      dzv = -0.25f*((zv0 + zv1 + zv5 + zv4) - (zv3 + zv2 + zv6 + zv7)) ;

      // i is int type and ax is float == mismatch
      delv_eta[i] = ( ax*dxv + ay*dyv + az*dzv ) * norm ;
   }

}

void CalcMonotonicQRegionForElems(int *elemBC,
                               int *lxim, int *lxip, int *letam, int *letap,
                               int *lzetam, int *lzetap,
                               float *delv_xi,float *delv_eta,float *delv_zeta,
                               float *delx_xi,float *delx_eta,float *delx_zeta,
                               float *vdov, float *volo, float *vnew,
                               float *elemMass, float *qq, float *ql,
                               float qlc_monoq, float qqc_monoq,
                               float monoq_limiter_mult,
                               float monoq_max_slope,
                               float ptiny, int numElem)
{
   for (int i=0; i<numElem; ++i) {
      float qlin, qquad ;
      float phixi, phieta, phizeta ;
      int bcMask = elemBC[i] ;
      float delvm, delvp ;

      /*  phixi     */
      float norm = ONE / ( delv_xi[i] + ptiny ) ;

      switch (bcMask & XI_M) {
         case 0:         delvm = delv_xi[lxim[i]] ; break ;
         case XI_M_SYMM: delvm = delv_xi[i] ;       break ;
         case XI_M_FREE: delvm = ZERO ;             break ;
         default:        /* ERROR */ ;              break ;
      }
      switch (bcMask & XI_P) {
         case 0:         delvp = delv_xi[lxip[i]] ; break ;
         case XI_P_SYMM: delvp = delv_xi[i] ;       break ;
         case XI_P_FREE: delvp = ZERO ;             break ;
         default:        /* ERROR */ ;              break ;
      }

      delvm = delvm * norm ;
      delvp = delvp * norm ;

      phixi = ( delvm + delvp ) * HALF;

      delvm *= monoq_limiter_mult ;
      delvp *= monoq_limiter_mult ;

      if ( delvm < phixi ) phixi = delvm ;
      if ( delvp < phixi ) phixi = delvp ;
      if ( phixi < ZERO) phixi = ZERO ;
      if ( phixi > monoq_max_slope) phixi = monoq_max_slope;


      /*  phieta     */
      norm = ONE / ( delv_eta[i] + ptiny ) ;

      switch (bcMask & ETA_M) {
         case 0:          delvm = delv_eta[letam[i]] ; break ;
         case ETA_M_SYMM: delvm = delv_eta[i] ;        break ;
         case ETA_M_FREE: delvm = ZERO ;               break ;
         default:         /* ERROR */ ;                break ;
      }
      switch (bcMask & ETA_P) {
         case 0:          delvp = delv_eta[letap[i]] ; break ;
         case ETA_P_SYMM: delvp = delv_eta[i] ;        break ;
         case ETA_P_FREE: delvp = ZERO ;               break ;
         default:         /* ERROR */ ;                break ;
      }

      delvm = delvm * norm ;
      delvp = delvp * norm ;

      phieta = ( delvm + delvp ) * HALF;

      delvm *= monoq_limiter_mult ;
      delvp *= monoq_limiter_mult ;

      if ( delvm  < phieta ) phieta = delvm ;
      if ( delvp  < phieta ) phieta = delvp ;
      if ( phieta < ZERO) phieta = ZERO ;
      if ( phieta > monoq_max_slope)  phieta = monoq_max_slope;

      /*  phizeta     */
      norm = ONE / ( delv_zeta[i] + ptiny ) ;

      switch (bcMask & ZETA_M) {
         case 0:           delvm = delv_zeta[lzetam[i]] ; break ;
         case ZETA_M_SYMM: delvm = delv_zeta[i] ;         break ;
         case ZETA_M_FREE: delvm = ZERO ;                 break ;
         default:          /* ERROR */ ;                  break ;
      }
      switch (bcMask & ZETA_P) {
         case 0:           delvp = delv_zeta[lzetap[i]] ; break ;
         case ZETA_P_SYMM: delvp = delv_zeta[i] ;         break ;
         case ZETA_P_FREE: delvp = ZERO ;                 break ;
         default:          /* ERROR */ ;                  break ;
      }

      delvm = delvm * norm ;
      delvp = delvp * norm ;

      phizeta = ( delvm + delvp ) * HALF;

      delvm *= monoq_limiter_mult ;
      delvp *= monoq_limiter_mult ;

      if ( delvm   < phizeta ) phizeta = delvm ;
      if ( delvp   < phizeta ) phizeta = delvp ;
      if ( phizeta < ZERO)     phizeta = ZERO ;
      if ( phizeta > monoq_max_slope  ) phizeta = monoq_max_slope;

      /* Remove length scale */

      if ( vdov[i] > ZERO )  {
         qlin  = ZERO ;
         qquad = ZERO ;
      }
      else {
         float delvxxi   = delv_xi[i]   * delx_xi[i]   ;
         float delvxeta  = delv_eta[i]  * delx_eta[i]  ;
         float delvxzeta = delv_zeta[i] * delx_zeta[i] ;

         if ( delvxxi   > ZERO ) delvxxi   = ZERO ;
         if ( delvxeta  > ZERO ) delvxeta  = ZERO ;
         if ( delvxzeta > ZERO ) delvxzeta = ZERO ;

         float rho = elemMass[i] / (volo[i] * vnew[i]) ;

         qlin = -qlc_monoq * rho *
            (  delvxxi   * (ONE - phixi) +
               delvxeta  * (ONE - phieta) +
               delvxzeta * (ONE - phizeta)  ) ;

         qquad = qqc_monoq * rho *
            (  delvxxi*delvxxi     * (ONE - phixi*phixi) +
               delvxeta*delvxeta   * (ONE - phieta*phieta) +
               delvxzeta*delvxzeta * (ONE - phizeta*phizeta)  ) ;
      }

      qq[i] = qquad ;
      ql[i] = qlin  ;
   }
}

void CalcMonotonicQForElems(Domain *domain)
{  
   //
   // calculate the monotonic q for pure regions
   //
   int numElem = domain->numElem ;
   if (numElem > 0) {
      //
      // initialize parameters
      // 
       float ptiny = 1.e-36 ;

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

void CalcQForElems(Domain *domain)
{
   //
   // MONOTONIC Q option
   //

   int numElem = domain->numElem ;

   if (numElem != 0) {
      /* allocate domain length arrays */

      // domain->delv_xi = AllocateFloat(numElem) ;   /* velocity gradient */
      // domain->delv_eta = AllocateFloat(numElem) ;
      // domain->delv_zeta = AllocateFloat(numElem) ;

      // domain->delx_xi = AllocateFloat(numElem) ;   /* position gradient */
      // domain->delx_eta = AllocateFloat(numElem) ;
      // domain->delx_zeta = AllocateFloat(numElem) ;

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
                                      numElem) ;

      /* Transfer veloctiy gradients in the first order elements */
      /* problem->commElements->Transfer(CommElements::monoQ) ; */

      /* This will be applied at the region level */
      CalcMonotonicQForElems(domain) ;

      /* release domain length arrays */

      // Release((void **) &domain->delx_zeta) ;
      // Release((void **) &domain->delx_eta) ;
      // Release((void **) &domain->delx_xi) ;

      // Release((void **) &domain->delv_zeta) ;
      // Release((void **) &domain->delv_eta) ;
      // Release((void **) &domain->delv_xi) ;

      /* Don't allow excessive artificial viscosity */
      float qstop = domain->qstop ;
      int idx = -1; 
      for (int i=0; i<numElem; ++i) {
         if ( domain->q[i] > qstop ) {
            idx = i ;
            // break ;
         }
      }

      if(idx >= 0) {
         exit(QStopError) ;
      }
   }
}

#define c1s 0.666666666f

inline void CalcPressureForElems(float *p_new, float *bvc,
                          float *pbvc, float *e_old,
                          float *compression, float *vnewc,
                          float pmin, float p_cut, float eosvmax,
                          int length)
{
   // float c1s = 2.0f / 3.0f ;
   for (int i=0 ; i<length; ++i) {
      bvc[i] = c1s * (compression[i] + ONE );
      pbvc[i] = c1s;
   }

   for (int i=0; i<length; ++i) {
      p_new[i] = bvc[i] * e_old[i] ;

      if    (fabsf(p_new[i]) <  p_cut)
         p_new[i] = ZERO ;

      if    ( vnewc[i] >= eosvmax ) /* impossible condition here? */
         p_new[i] = ZERO ;

      if    (p_new[i]       <  pmin)
         p_new[i]   = pmin ;
   }
}

#define sixth 0.166666666f

void CalcEnergyForElems(float *p_new, float *e_new, float *q_new,
                        float *bvc, float *pbvc,
                        float *p_old, float *e_old, float *q_old,
                        float *compression, float *compHalfStep,
                        float *vnewc, float *work, float *delvc, float pmin,
                        float p_cut, float  e_cut, float q_cut, float emin,
                        float *qq_old, float *ql_old,
                        float rho0, float eosvmax, float *pHalfStep,
                        int length)
{
   // float sixth = ONE / 6.0f ;
   // float *pHalfStep = AllocateFloat(length) ;

   for (int i=0; i<length; ++i) {
      e_new[i] = e_old[i] - delvc[i]*(p_old[i] + q_old[i]) * HALF +
                 work[i] * HALF;

      if (e_new[i]  < emin ) {
         e_new[i] = emin ;
      }
   }

   CalcPressureForElems(pHalfStep, bvc, pbvc, e_new, compHalfStep, vnewc,
                   pmin, p_cut, eosvmax, length);

   for (int i=0; i<length; ++i) {
      float vhalf = ONE / (ONE + compHalfStep[i]) ;

      if ( delvc[i] > ZERO ) {
         q_new[i] /* = qq_old[i] = ql_old[i] */ = ZERO ;
      }
      else {
         float ssc = ( pbvc[i] * e_new[i]
                 + vhalf * vhalf * bvc[i] * pHalfStep[i] ) / rho0 ;

         if ( ssc > 0.1111111e-36f ) {
            ssc = sqrtf(ssc) ;
         } else {
            ssc = 0.3333333e-18f ;
         }

         q_new[i] = (ssc*ql_old[i] + qq_old[i]) ;
      }

      e_new[i] = e_new[i] + delvc[i] * HALF
         * (  3.0f * (p_old[i]     + q_old[i])
              - 4.0f * (pHalfStep[i] + q_new[i])) ;
   }

   for (int i=0; i<length; ++i) {

      e_new[i] += work[i] * HALF;

      if (fabsf(e_new[i]) < e_cut) {
         e_new[i] = ZERO ;
      }
      if (     e_new[i]  < emin ) {
         e_new[i] = emin ;
      }
   }

   CalcPressureForElems(p_new, bvc, pbvc, e_new, compression, vnewc,
                   pmin, p_cut, eosvmax, length);

   for (int i=0; i<length; ++i) {
      float q_tilde ;

      if (delvc[i] > ZERO) {
         q_tilde = ZERO ;
      }
      else {
         float ssc = ( pbvc[i] * e_new[i]
                 + vnewc[i] * vnewc[i] * bvc[i] * p_new[i] ) / rho0 ;

         if ( ssc > 0.1111111e-36f ) {
            ssc = sqrtf(ssc) ;
         } else {
            ssc = 0.3333333e-18f ;
         }

         q_tilde = (ssc*ql_old[i] + qq_old[i]) ;
      }

      e_new[i] = e_new[i] - (  (p_old[i]     + q_old[i]) * 7.0f
                               - (pHalfStep[i] + q_new[i]) * 8.0f
                               + (p_new[i] + q_tilde)) * delvc[i]*sixth ;

      if (fabsf(e_new[i]) < e_cut) {
         e_new[i] = ZERO  ;
      }
      if (     e_new[i]  < emin ) {
         e_new[i] = emin ;
      }
   }

   CalcPressureForElems(p_new, bvc, pbvc, e_new, compression, vnewc,
                   pmin, p_cut, eosvmax, length);

   for (int i=0 ;i<length; ++i) {

      if ( delvc[i] <= ZERO ) {
         float ssc = ( pbvc[i] * e_new[i]
                 + vnewc[i] * vnewc[i] * bvc[i] * p_new[i] ) / rho0 ;

         if ( ssc > 0.1111111e-36f ) {
            ssc = sqrtf(ssc) ;
         } else {
            ssc = 0.3333333e-18f ;
         }

         q_new[i] = (ssc*ql_old[i] + qq_old[i]) ;

         if (fabsf(q_new[i]) < q_cut) q_new[i] = ZERO ;
      }
   }

   // Release((void **) &pHalfStep) ;

   return ;
}

void CalcSoundSpeedForElems(int length, float *ss,
                            float *vnewc, float rho0, float *enewc,
                            float *pnewc, float *pbvc,
                            float *bvc, float ss4o3)
{
   for (int iz=0; iz<length; ++iz) {
      float ssTmp = (pbvc[iz] * enewc[iz] + vnewc[iz] * vnewc[iz] *
                 bvc[iz] * pnewc[iz]) / rho0;
      if (ssTmp <= 0.1111111e-36f) {
         ssTmp = 0.3333333e-18f;
      }
      else {
         ssTmp = sqrtf( ssTmp );
      }
      ss[iz] = ssTmp ;
   }
}

void EvalEOSForElems(Domain *domain, float *vnewc, int numElem)
{
   float  e_cut = domain->e_cut ;
   float  p_cut = domain->p_cut ;
   float  ss4o3 = domain->ss4o3 ;
   float  q_cut = domain->q_cut ;

   float eosvmax = domain->eosvmax ;
   float eosvmin = domain->eosvmin ;
   float pmin    = domain->pmin ;
   float emin    = domain->emin ;
   float rho0    = domain->refdens ;

   /* allocate *domain length* arrays.  */
   /* wastes memory, but allows us to get */
   /* around a "temporary workset" issue */
   /* we have not yet addressed. */
   float * delvc = domain->delv ;
   float * p_old = p_old_;
   float * compression = compression_;
   float * compHalfStep = compHalfStep_;
   float * work = work_;
   float * p_new = p_new_;
   float * e_new = e_new_;
   float * q_new = q_new_;
   float * bvc = bvc_;
   float * pbvc = pbvc_;
   float * pHalfStep = pHalfStep_;

   /* compress data, minimal set */
   for (int zidx=0; zidx<numElem; ++zidx) {
      p_old[zidx] = domain->p[zidx] ;
   }

   for (int zidx=0; zidx<numElem; ++zidx) {
      float vchalf ;
      compression[zidx] = ONE / vnewc[zidx] - ONE;
      vchalf = vnewc[zidx] - delvc[zidx] * HALF;
      compHalfStep[zidx] = ONE / vchalf - ONE;
   }

   /* Check for v > eosvmax or v < eosvmin */
   if ( eosvmin != ZERO ) {
      for (int zidx=0; zidx<numElem; ++zidx) {
         if (vnewc[zidx] <= eosvmin) { /* impossible due to calling func? */
            compHalfStep[zidx] = compression[zidx] ;
         }
      }
   }
   if ( eosvmax != ZERO ) {
      for (int zidx=0; zidx<numElem; ++zidx) {
         if (vnewc[zidx] >= eosvmax) { /* impossible due to calling func? */
            p_old[zidx]        = ZERO ;
            compression[zidx]  = ZERO ;
            compHalfStep[zidx] = ZERO ;
         }
      }
   }

   for (int zidx=0; zidx<numElem; ++zidx) {
      work[zidx] = ZERO ; 
   }

   CalcEnergyForElems(p_new, e_new, q_new, bvc, pbvc,
                 p_old, domain->e,  domain->q, compression, compHalfStep,
                 vnewc, work,  delvc, pmin,
                 p_cut, e_cut, q_cut, emin,
                 domain->qq, domain->ql, rho0, eosvmax,
                 pHalfStep, numElem);


   for (int zidx=0; zidx<numElem; ++zidx) {
      domain->p[zidx] = p_new[zidx] ;
      domain->e[zidx] = e_new[zidx] ;
      domain->q[zidx] = q_new[zidx] ;
   }

   CalcSoundSpeedForElems(numElem, domain->ss,
             vnewc, rho0, e_new, p_new,
             pbvc, bvc, ss4o3) ;

   // Release((void **) &pHalfStep) ;
   // Release((void **) &pbvc) ;
   // Release((void **) &bvc) ;
   // Release((void **) &q_new) ;
   // Release((void **) &e_new) ;
   // Release((void **) &p_new) ;
   // Release((void **) &work) ;
   // Release((void **) &compHalfStep) ;
   // Release((void **) &compression) ;
   // Release((void **) &p_old) ;
}

void ApplyMaterialPropertiesForElems(Domain *domain)
{
  int numElem = domain->numElem ;

  if (numElem != 0) {
    /* Expose all of the variables needed for material evaluation */
    float eosvmin = domain->eosvmin ;
    float eosvmax = domain->eosvmax ;

    /* create a domain length (not material length) temporary */
    /* we are assuming here that the number of dense ranges is */
    /* much greater than the number of sigletons.  We are also */
    /* assuming it is ok to allocate a domain length temporary */
    /* rather than a material length temporary. */

    float * vnewc = vnewc_;

    for (int zn=0; zn<numElem; ++zn) {
       vnewc[zn] = domain->vnew[zn] ;
    }

    if (eosvmin != ZERO) {
       for (int zn=0; zn<numElem; ++zn) {
          if (vnewc[zn] < eosvmin)
             vnewc[zn] = eosvmin ;
       }
    }

    if (eosvmax != ZERO) {
       for (int zn=0; zn<numElem; ++zn) {
          if (vnewc[zn] > eosvmax)
             vnewc[zn] = eosvmax ;
       }
    }

    for (int zn=0; zn<numElem; ++zn) {
       float vc = domain->v[zn] ;
       if (eosvmin != ZERO) {
          if (vc < eosvmin)
             vc = eosvmin ;
       }
       if (eosvmax != ZERO) {
          if (vc > eosvmax)
             vc = eosvmax ;
       }
       if (vc <= ZERO) {
          exit(VolumeError) ;
       }
    }

    EvalEOSForElems(domain, vnewc, numElem);

    // Release((void **) &vnewc) ;

  }
}

void UpdateVolumesForElems(float *vnew, float *v,
                           float v_cut, int length)
{
   if (length != 0) {
      for (int i=0; i<length; ++i) {
         float tmpV = vnew[i] ;

         if ( fabsf(tmpV - ONE) < v_cut )
            tmpV = ONE ;

         v[i] = tmpV ;
      }
   }

   return ;
}

void LagrangeElements(Domain *domain, int numElem)
{
  /* new relative volume -- temporary */
  // domain->vnew = AllocateFloat(numElem);

  CalcLagrangeElements(domain) ;

  /* Calculate Q.  (Monotonic q option requires communication) */
  CalcQForElems(domain) ;

  ApplyMaterialPropertiesForElems(domain) ;

  UpdateVolumesForElems(domain->vnew, domain->v,
                        domain->v_cut, numElem) ;

  // Release((void **) &domain->vnew) ;
}

void CalcCourantConstraintForElems(int length, float *ss,
                                   float *vdov, float *arealg,
                                   float qqc, float *dtcourant)
{
   float dtcourant_tmp = 1.0e+20f ;
   int   courant_elem = -1 ;

   float  qqc2 = 64.0f * qqc * qqc ;

   for (int indx=0; indx<length; ++indx) {

      float dtf = ss[indx] * ss[indx] ;

      if ( vdov[indx] < ZERO ) {

         dtf = dtf
            + qqc2 * arealg[indx] * arealg[indx] * vdov[indx] * vdov[indx] ;
      }

      dtf = sqrtf( dtf ) ;

      dtf = arealg[indx] / dtf ;

      /* determine minimum timestep with its corresponding elem */

      if (vdov[indx] != ZERO) {
         if ( dtf < dtcourant_tmp ) {
            dtcourant_tmp = dtf ;
            courant_elem = indx ;
         }
      }
   }

   /* Don't try to register a time raint if none of the elements
    * were active */

   if (courant_elem != -1) {
      *dtcourant = dtcourant_tmp ;
   }

   return ;
}

void CalcHydroConstraintForElems(int length, float *vdov,
                                 float dvovmax, float *dthydro)
{
   float dthydro_tmp = 1.0e+20f ;
   int hydro_elem = -1 ;

   for (int indx=0; indx<length; ++indx) {
      if (vdov[indx] != ZERO) {
         float dtdvov = dvovmax / (fabsf(vdov[indx])+1.0e-20f) ;
         if ( dthydro_tmp > dtdvov ) {
            dthydro_tmp = dtdvov ;
            hydro_elem = indx ;
         }
      }
   }

   if (hydro_elem != -1) {
      *dthydro = dthydro_tmp ;
   }

   return ;
}

void CalcTimeConstraintsForElems(Domain *domain) {
   CalcCourantConstraintForElems(domain->numElem, domain->ss,
                                 domain->vdov, domain->arealg,
                                 domain->qqc, &domain->dtcourant) ;

   /* check hydro raint */
   CalcHydroConstraintForElems(domain->numElem, domain->vdov,
                               domain->dvovmax, &domain->dthydro) ;
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
   int i, j, lnode, plane, row, col;
   float tx, ty, tz ;
   int nidx, zidx ;
   Domain domain ;

   int edgeElems = 20 ;
   int edgeNodes = edgeElems+1 ;

   /****************************/
   /*   Initialize Sedov Mesh  */
   /****************************/

   /* construct a uniform box for this processor */

   domain.sizeX = edgeElems ;
   domain.sizeY = edgeElems ;
   domain.sizeZ = edgeElems ;
   domain.numElem = edgeElems*edgeElems*edgeElems ;

   domain.numNode = edgeNodes*edgeNodes*edgeNodes ;

   int domElems = domain.numElem ;
   int domNodes = domain.numNode ;

   /*************************/
   /* allocate field memory */
   /*************************/
   
   /*****************/
   /* Elem-centered */
   /*****************/

   /* elemToNode connectivity */
   domain.nodelist = AllocateInt(8*domElems) ;

   /* elem connectivity through face */
   domain.lxim = AllocateInt(domElems) ;
   domain.lxip = AllocateInt(domElems)  ;
   domain.letam = AllocateInt(domElems) ;
   domain.letap = AllocateInt(domElems) ;
   domain.lzetam = AllocateInt(domElems) ;
   domain.lzetap = AllocateInt(domElems) ;

   /* elem face symm/free-surface flag */
   domain.elemBC = AllocateInt(domElems) ;

   domain.e = AllocateFloat(domElems) ;   /* energy */
   domain.p = AllocateFloat(domElems) ;   /* pressure */

   domain.q = AllocateFloat(domElems) ;   /* q */
   domain.ql = AllocateFloat(domElems) ;  /* linear term for q */
   domain.qq = AllocateFloat(domElems) ;  /* quadratic term for q */

   domain.v = AllocateFloat(domElems) ;     /* relative volume */
   domain.volo = AllocateFloat(domElems) ;  /* reference volume */
   domain.delv = AllocateFloat(domElems) ;  /* m_vnew - m_v */
   domain.vdov = AllocateFloat(domElems) ;  /* volume deriv over volume */

   /* elem characteristic length */
   domain.arealg = AllocateFloat(domElems) ;

   domain.ss = AllocateFloat(domElems) ;    /* "sound speed" */

   domain.elemMass = AllocateFloat(domElems) ;  /* mass */

   /*****************/
   /* Node-centered */
   /*****************/

   domain.x = AllocateFloat(domNodes) ;  /* coordinates */
   domain.y = AllocateFloat(domNodes)  ;
   domain.z = AllocateFloat(domNodes)  ;

   domain.xd = AllocateFloat(domNodes) ; /* velocities */
   domain.yd = AllocateFloat(domNodes) ;
   domain.zd = AllocateFloat(domNodes) ;

   domain.xdd = AllocateFloat(domNodes)  ; /* accelerations */
   domain.ydd = AllocateFloat(domNodes)  ;
   domain.zdd = AllocateFloat(domNodes)  ;

   domain.fx = AllocateFloat(domNodes) ;  /* forces */
   domain.fy = AllocateFloat(domNodes) ;
   domain.fz = AllocateFloat(domNodes) ;

   domain.nodalMass = AllocateFloat(domNodes) ;  /* mass */

   /* Boundary nodesets */

   domain.symmX = AllocateInt(edgeNodes*edgeNodes) ;
   domain.symmY = AllocateInt(edgeNodes*edgeNodes) ;
   domain.symmZ = AllocateInt(edgeNodes*edgeNodes) ;

   dvdx_ = AllocateFloat(domElems*8) ;
   dvdy_ = AllocateFloat(domElems*8) ;
   dvdz_ = AllocateFloat(domElems*8) ;
   x8n_  = AllocateFloat(domElems*8) ;
   y8n_  = AllocateFloat(domElems*8) ;
   z8n_  = AllocateFloat(domElems*8) ;
   sigxx_  = /* AllocateFloat(domElems) */ dvdx_;
   sigyy_  = /* AllocateFloat(domElems) */ sigxx_ + domElems;
   sigzz_  = /* AllocateFloat(domElems) */ sigyy_ + domElems;
   determ_ = AllocateFloat(domElems) ;
   vnewc_ = /* AllocateFloat(domElems) */ dvdx_ ;
   p_old_ = /* AllocateFloat(domElems) */ vnewc_ + domElems;
   compression_ = /* AllocateFloat(domElems) */ p_old_ + domElems;
   compHalfStep_ = /* AllocateFloat(domElems) */ compression_ + domElems;
   work_ = /* AllocateFloat(domElems) */ compHalfStep_ + domElems;
   p_new_ = /* AllocateFloat(domElems) */ work_ + domElems;
   e_new_ = /* AllocateFloat(domElems) */ p_new_ + domElems;
   q_new_ = /* AllocateFloat(domElems) */ e_new_ + domElems;
   bvc_   = /* AllocateFloat(domElems) */ dvdy_ ;
   pbvc_  = /* AllocateFloat(domElems) */ bvc_ + domElems;
   pHalfStep_ = /* AllocateFloat(domElems) */ pbvc_ + domElems;

   domain.dxx  = /* AllocateFloat(domElems) */ dvdx_ ; /* principal strains */
   domain.dyy  = /* AllocateFloat(domElems) */ domain.dxx + domElems;
   domain.dzz  = /* AllocateFloat(domElems) */ domain.dyy + domElems;
   domain.delv_xi   = /* AllocateFloat(domElems) */ dvdx_ ;   /* velocity gradient */
   domain.delv_eta  = /* AllocateFloat(domElems) */ domain.delv_xi + domElems ;
   domain.delv_zeta = /* AllocateFloat(domElems) */ domain.delv_eta + domElems ;
   domain.delx_xi   = /* AllocateFloat(domElems) */ domain.delv_zeta + domElems ;   /* position gradient */
   domain.delx_eta  = /* AllocateFloat(domElems) */ domain.delx_xi + domElems ;
   domain.delx_zeta = /* AllocateFloat(domElems) */ domain.delx_eta + domElems ;
   domain.vnew = AllocateFloat(domElems) ;

   /* Basic Field Initialization */

   for (i=0; i<domElems; ++i) {
      domain.e[i] = ZERO ;
      domain.p[i] = ZERO ;
      domain.q[i] = ZERO ;
      domain.v[i] = ONE ;
   }

   for (i=0; i<domNodes; ++i) {
      domain.xd[i] = ZERO ;
      domain.yd[i] = ZERO ;
      domain.zd[i] = ZERO ;
   }

   for (i=0; i<domNodes; ++i) {
      domain.xdd[i] = ZERO ;
      domain.ydd[i] = ZERO ;
      domain.zdd[i] = ZERO ;
   }

   /* initialize nodal coordinates */

   nidx = 0 ;
   tz  = ZERO ;
   for (plane=0; plane<edgeNodes; ++plane) {
      ty = ZERO ;
      for (row=0; row<edgeNodes; ++row) {
         tx = ZERO ;
         for (col=0; col<edgeNodes; ++col) {
            domain.x[nidx] = tx ;
            domain.y[nidx] = ty ;
            domain.z[nidx] = tz ;
            ++nidx ;
            // tx += ds ; /* may accumulate roundoff... */
            tx = 1.125f*(float)(col+1)/(float)(edgeElems) ;
         }
         // ty += ds ;  /* may accumulate roundoff... */
         ty = 1.125f*(float)(row+1)/(float)(edgeElems) ;
      }
      // tz += ds ;  /* may accumulate roundoff... */
      tz = 1.125f*(float)(plane+1)/(float)(edgeElems) ;
   }


   /* embed hexehedral elements in nodal point lattice */

   nidx = 0 ;
   zidx = 0 ;
   for (plane=0; plane<edgeElems; ++plane) {
      for (row=0; row<edgeElems; ++row) {
         for (col=0; col<edgeElems; ++col) {
            int * localNode = &domain.nodelist[8*zidx] ;
            localNode[0] = nidx                                       ;
            localNode[1] = nidx                                   + 1 ;
            localNode[2] = nidx                       + edgeNodes + 1 ;
            localNode[3] = nidx                       + edgeNodes     ;
            localNode[4] = nidx + edgeNodes*edgeNodes                 ;
            localNode[5] = nidx + edgeNodes*edgeNodes             + 1 ;
            localNode[6] = nidx + edgeNodes*edgeNodes + edgeNodes + 1 ;
            localNode[7] = nidx + edgeNodes*edgeNodes + edgeNodes     ;
            ++zidx ;
            ++nidx ;
         }
         ++nidx ;
      }
      nidx += edgeNodes ;
   }

   /* initialize material parameters */
   domain.dtfixed = -1.0e-7f ;
   domain.deltatime = 1.0e-7f ;
   domain.deltatimemultlb = 1.1f ;
   domain.deltatimemultub = 1.2f ;
   domain.stoptime  = 1.0e-2f ;
   domain.dtcourant = 1.0e+20f ;
   domain.dthydro   = 1.0e+20f ;
   domain.dtmax     = 1.0e-2f ;
   domain.time    = ZERO ;
   domain.cycle   = 0 ;

   domain.e_cut = 1.0e-7f ;
   domain.p_cut = 1.0e-7f ;
   domain.q_cut = 1.0e-7f ;
   domain.u_cut = 1.0e-7f ;
   domain.v_cut = 1.0e-10f ;

   domain.hgcoef      = 3.0f ;
   domain.ss4o3       = 4.0f/3.0f ;

   domain.qstop              =  1.0e+12f ;
   domain.monoq_max_slope    =  ONE ;
   domain.monoq_limiter_mult =  2.0f ;
   domain.qlc_monoq          = HALF ;
   domain.qqc_monoq          = 2.0f/3.0f ;
   domain.qqc                = 2.0f ;

   domain.pmin =  ZERO ;
   domain.emin = -1.0e+15f ;

   domain.dvovmax =  0.1f ;

   domain.eosvmax =  1.0e+9f ;
   domain.eosvmin =  1.0e-9f ;

   domain.refdens =  ONE ;

   /* initialize field data */
   for (i=0; i<domNodes; ++i) {
      domain.nodalMass[i] = ZERO ;
   }

   for (i=0; i<domElems; ++i) {
      float  x_local[8] ;
      float  y_local[8] ;
      float  z_local[8] ;
      int * elemToNode = &domain.nodelist[8*i] ;
      for( lnode=0 ; lnode<8 ; ++lnode )
      {
        int gnode = elemToNode[lnode];
        x_local[lnode] = domain.x[gnode];
        y_local[lnode] = domain.y[gnode];
        z_local[lnode] = domain.z[gnode];
      }

      // volume calculations
      float volume = CalcElemVolume(x_local, y_local, z_local );
      domain.volo[i] = volume ;
      domain.elemMass[i] = volume ;
      for (j=0; j<8; ++j) {
         int idx = elemToNode[j] ;
         domain.nodalMass[idx] += volume / 8.0f ;
      }
   }

   /* deposit energy */
   domain.e[0] = 3.948746e+7f ;

   /* set up symmetry nodesets */
   nidx = 0 ;
   for (i=0; i<edgeNodes; ++i) {
      int planeInc = i*edgeNodes*edgeNodes ;
      int rowInc   = i*edgeNodes ;
      for (j=0; j<edgeNodes; ++j) {
         domain.symmX[nidx] = planeInc + j*edgeNodes ;
         domain.symmY[nidx] = planeInc + j ;
         domain.symmZ[nidx] = rowInc   + j ;
         ++nidx ;
      }
   }

   /* set up elemement connectivity information */
   domain.lxim[0] = 0 ;
   for (i=1; i<domElems; ++i) {
      domain.lxim[i]   = i-1 ;
      domain.lxip[i-1] = i ;
   }
   domain.lxip[domElems-1] = domElems-1 ;

   for (i=0; i<edgeElems; ++i) {
      domain.letam[i] = i ; 
      domain.letap[domElems-edgeElems+i] = domElems-edgeElems+i ;
   }
   for (i=edgeElems; i<domElems; ++i) {
      domain.letam[i] = i-edgeElems ;
      domain.letap[i-edgeElems] = i ;
   }

   for (i=0; i<edgeElems*edgeElems; ++i) {
      domain.lzetam[i] = i ;
      domain.lzetap[domElems-edgeElems*edgeElems+i] = domElems-edgeElems*edgeElems+i ;
   }
   for (i=edgeElems*edgeElems; i<domElems; ++i) {
      domain.lzetam[i] = i - edgeElems*edgeElems ;
      domain.lzetap[i-edgeElems*edgeElems] = i ;
   }

   /* set up boundary condition information */
   for (i=0; i<domElems; ++i) {
      domain.elemBC[i] = 0 ;  /* clear BCs by default */
   }

   /* faces on "external" boundaries will be */
   /* symmetry plane or free surface BCs */
   for (i=0; i<edgeElems; ++i) {
      int planeInc2 = i*edgeElems*edgeElems ;
      int rowInc2   = i*edgeElems ;
      for (j=0; j<edgeElems; ++j) {
         domain.elemBC[planeInc2+j*edgeElems] |= XI_M_SYMM ;
         domain.elemBC[planeInc2+j*edgeElems+edgeElems-1] |= XI_P_FREE ;
         domain.elemBC[planeInc2+j] |= ETA_M_SYMM ;
         domain.elemBC[planeInc2+j+edgeElems*edgeElems-edgeElems] |= ETA_P_FREE ;
         domain.elemBC[rowInc2+j] |= ZETA_M_SYMM ;
         domain.elemBC[rowInc2+j+domElems-edgeElems*edgeElems] |= ZETA_P_FREE ;
      }
   }

   /* timestep to solution */
   while(domain.time < domain.stoptime) {
      TimeIncrement(&domain) ;
      LagrangeLeapFrog(&domain) ;
      /* problem->commNodes->Transfer(CommNodes::syncposvel) ; */
#ifdef LULESH_SHOW_PROGRESS
      printf("time = %e, dt=%e\n",
             (float)(domain.time), (float)(domain.deltatime) ) ;
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

   return 0 ;
}
