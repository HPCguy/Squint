/**************************************************************************
 * Program:  ShockTube.C
 * Purpose:  1D shock tube, split flux Euler equations
 *
 *         | m  |            |    mv    |
 *     Q = | mv |        F = | mv^2 + P |
 *         | E  |            |  v(E+P)  |
 *
 *     P = (gamma - 1.0)[E - 0.5 mv^2 ]
 *
 *             Cp
 *     gamma = --     m = mass/volume   v = velocity
 *             Cv
 *
 *     All quantities are non-dimensionalized.
 *
 *     @Q   @F    @Q   @F @Q
 *     -- + -- =  -- + -- -- = 0
 *     @t   @x    @t   @Q @x
 *
 *************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define gammaa        1.4142135
#define gammaInverse  0.70710678


/**************************************************************************
 * Subroutine:  CreateShockTubeMesh
 * Purpose   :  Build an empty mesh for the shock tube


     Gaps between elements are faces
                    |
     -------------------------------
     |   |   |             |   |   |

  ### ### ### ###       ### ### ### ###
  ### ### ### ###  ...  ### ### ### ###  <--- 1D Shock tube model
  ### ### ### ###       ### ### ### ###

   |  |                           |  |
   |  -----------------------------  |
  Inflow           |               Outflow
  Element      Tube Elements       Element


 *************************************************************************/

void InitializeShockTubeMesh(int numElem, float *mass, float *momentum,
                                          float *energy, float *pressure)
{
   int i;
   int midTube = numElem / 2;

   float massInitial     = 1.0;
   float pressureInitial = gammaInverse;
   float energyInitial   = pressureInitial/(gammaa-1.0);

   float pressureRatio = 0.4;
   float densityRatio = 0.7; 

   float *m = mass, *e = energy, *p = pressure;
   for (i = 0; i < midTube; ++i) {
      *m++ = massInitial;
      *p++ = pressureInitial;
      *e++ = energyInitial;
   }

   massInitial     = massInitial * densityRatio;
   pressureInitial = pressureInitial * pressureRatio;
   energyInitial   = pressureInitial/(gammaa - 1.0);

   for (i = midTube; i < numElem; ++i) {
      *m++ = massInitial;
      *p++ = pressureInitial;
      *e++ = energyInitial;
   }

   m = momentum;
   for (i = 0; i < numElem; ++i) {
      *m++ = 0.0;
   }
}


/**************************************************************************
 * Subroutine:  ComputeFaceInfo
 * Purpose   :  Compute F quantities at faces.
 *
 *  @F   @F0   @F1   @F2
 *  -- = --- + --- + ---
 *  @x   @x    @x    @x
 *
 *  Calculate F0, F1 and F2 at the face centers.
 *
 *************************************************************************/

void ComputeFaceInfo(int numFace, float *mass, float *momentum, float *energy, 
                                  float *f0, float *f1, float *f2)
{
   float *m = mass, *mo = momentum, *e = energy;
   float *f0_ = f0, *f1_ =f1, *f2_ = f2;
   float f0__, f1__, f2__;
   int contributor;
   float ev;
   float cLocal;

   for (int i = numFace; i != 0; --i)
   {
      /* each face has an upwind and downwind element. */
#ifdef __MC__
      int upWind   := 0;     /* upwind element */
      int downWind := 1; /* downwind element */
#endif
#ifndef __MC__
      int upWind   = 0;     /* upwind element */
      int downWind = 1; /* downwind element */
#endif

      /* calculate face centered quantities */
      float massf =     0.5 * ( m[upWind] +  m[downWind]);
      float momentumf = 0.5 * (mo[upWind] + mo[downWind]);
      float energyf =   0.5 * ( e[upWind] +  e[downWind]);
      float pressuref = (gammaa - 1.0) *
                         (energyf - 0.5*momentumf*momentumf/massf);
      float c = sqrtf(gammaa*pressuref/massf);
      float v = momentumf/massf;

      /* Now that we have the wave speeds, we might want to */
      /* look for the max wave speed here, and update dt */
      /* appropriately right before leaving this function. */
      /* ... */

      /* OK, calculate face quantities */

      contributor = ((v >= 0.0) ? upWind : downWind);
      massf     =  m[contributor];
      momentumf = mo[contributor];
      energyf   =  e[contributor];
      pressuref = energyf - 0.5*momentumf*momentumf/massf;
      ev = v*(gammaa - 1.0);

      f0__ = ev*massf;
      f1__ = ev*momentumf;
      f2__ = ev*(energyf - pressuref);

      contributor = ((v + c >= 0.0) ? upWind : downWind);
      massf     =  m[contributor];
      momentumf = mo[contributor];
      energyf   =  e[contributor];
      pressuref = (gammaa - 1.0)*(energyf - 0.5*momentumf*momentumf/massf);
      ev = 0.5*(v + c);
      cLocal = sqrtf(gammaa*pressuref/massf);

      f0__ += ev*massf;
      f1__ += ev*(momentumf + massf*cLocal);
      f2__ += ev*(energyf + pressuref + momentumf*cLocal);

      contributor = ((v - c >= 0.0) ? upWind : downWind);
      massf     =  m[contributor];
      momentumf = mo[contributor];
      energyf   =  e[contributor];
      pressuref = (gammaa - 1.0)*(energyf - 0.5*momentumf*momentumf/massf); ++m;
      ev = 0.5*(v - c); ++mo;
      cLocal = sqrtf(gammaa*pressuref/massf); ++e;

      f0__ += ev*massf;
      f1__ += ev*(momentumf - massf*cLocal);
      f2__ += ev*(energyf + pressuref - momentumf*cLocal);

      *f0_++ = f0__;
      *f1_++ = f1__;
      *f2_++ = f2__;
   }
}


/**************************************************************************
 * Subroutine:  UpdateElemInfo
 * Purpose   :  Q(elem) = Q(elem) + deltaQ(elem)
 *
 *  deltaQ(elem) = - (F(downWindFace) - F(upWindFace)) * dt / dx ;
 *
 *************************************************************************/

void UpdateElemInfo(int numElem, float *mass, float *momentum,
                                 float *energy, float *pressure,
                                 float *f0, float *f1, float *f2, float dtdx)
{
   float *f0_ = &f0[1], *f1_ = &f1[1], *f2_ = &f2[1];
   float *m = &mass[1], *mo = &momentum[1], *e = &energy[1], *p = pressure;

   for (int i = 1; i < numElem; ++i)
   {
      /* each element inside the tube has an upwind and downwind face */
#ifdef __MC__
      int upWind  := -1;   /* upwind face */
      int downWind := 0;   /* downwind face */
#endif
#ifndef __MC__
      int upWind  = -1;   /* upwind face */
      int downWind = i;   /* downwind face */
#endif

      float m_ = *m, mo_ = *mo, e_ = *e;
      m_  -= gammaInverse*(f0_[downWind] - f0_[upWind])*dtdx;
      mo_ -= gammaInverse*(f1_[downWind] - f1_[upWind])*dtdx;
      e_  -= gammaInverse*(f2_[downWind] - f2_[upWind])*dtdx;
      *++p   = (gammaa - 1.0) * (e_ - 0.5*mo_*(*mo++ = mo_)/(*m++ = m_));
      ++f0_; ++f1_; ++f2_;
      *e++ = e_;
   }
}


/**************************************************************************
 * Subroutine:  DumpField
 * Purpose   :  Create a plot for a single field
 *************************************************************************/

void DumpField(char *tag, int numElem, float *field)
{
   int i;

   printf(tag);
   for (i = 0; i < numElem; ++i) {
      printf("%d.0 %f\n", i, field[i]);
   }
}


/**************************************************************************
 * Subroutine:  DumpPlot
 * Purpose   :  create output that can be viewed with gnuplot: plot "file"
 *************************************************************************/

void DumpPlot(int numElem, float *mass, float *momentum,
                           float *energy, float *pressure)
{
   DumpField("# mass\n", numElem, mass);
   DumpField("\n\n# momentum\n", numElem, momentum);
   DumpField("\n\n# energy\n", numElem, energy);
   DumpField("\n\n# pressure\n", numElem, pressure);
}


/**************************************************************************
 * Subroutine:  main
 * Purpose   :  Simulate a 1D Shock Tube using split flux Euler formulation
 *************************************************************************/

int main(void)
{
   int numElems = 512;          // 2048
   int numFaces = numElems - 1;
   int numTotalCycles = 100;    // 1024
   // int dumpInterval = 20;

   float *mass     = (float *) malloc((numElems+1)*sizeof(float));
   float *momentum = (float *) malloc((numElems+1)*sizeof(float));
   float *energy   = (float *) malloc((numElems+1)*sizeof(float));
   float *pressure = (float *) malloc((numElems+1)*sizeof(float));

   float *f0 = (float *) malloc((numElems-1)*sizeof(float));
   float *f1 = (float *) malloc((numElems-1)*sizeof(float));
   float *f2 = (float *) malloc((numElems-1)*sizeof(float));

   InitializeShockTubeMesh(numElems+1, mass, momentum, energy, pressure);

   float time = 0.0;
   float dx = 1.0 / (float) numElems;
   float dt = 0.4 * dx;
   int currCycle = 0;

   for (currCycle=0; currCycle<numTotalCycles; ++currCycle)
   {
      // if (currCycle % dumpInterval == 0)
      //    DumpPlot(numElems, mass, momentum, energy, pressure);

      ComputeFaceInfo(numFaces, mass, momentum, energy, f0, f1, f2);
      UpdateElemInfo (numElems-1, mass, momentum, energy, pressure,
                      f0, f1, f2, dt/dx);
      time = time + dt;
   }

   DumpPlot(numElems, mass, momentum, energy, pressure);

   free(f2); free(f1); free(f0);
   free(pressure); free(energy); free(momentum); free(mass);

   return 0 ;
}

