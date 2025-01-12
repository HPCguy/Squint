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

float gammaa       = 1.4142135f;
float gammaInverse = 0.70710678f;

struct flux { float f0, f1, f2; };

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

   float massInitial     = 1.0f;
   float pressureInitial = gammaInverse;
   float energyInitial   = pressureInitial/(gammaa-1.0f);

   float pressureRatio = 0.4f;
   float densityRatio = 0.7f;

   for (i = 0; i < midTube; ++i) {
      mass[i]     = massInitial;
      pressure[i] = pressureInitial;
      energy[i]   = energyInitial;
   }

   massInitial     = massInitial * densityRatio;
   pressureInitial = pressureInitial * pressureRatio;
   energyInitial   = pressureInitial/(gammaa - 1.0f);

   for (i = midTube; i < numElem; ++i) {
      mass[i]     = massInitial;
      pressure[i] = pressureInitial;
      energy[i]   = energyInitial;
   }

   for (i = 0; i < numElem; ++i) {
      momentum[i] = 0.0f;
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
                                  struct flux *fl)
{
   int i;
   int contributor;
   float ev;
   float cLocal;

   for (i = 0; i < numFace; ++i)
   {
      /* each face has an upwind and downwind element. */
#ifdef __MC__
      int upWind   := i;     /* upwind element */
      int downWind := i + 1; /* downwind element */
#endif
#ifndef __MC__
      int upWind   = i;     /* upwind element */
      int downWind = i + 1; /* downwind element */
#endif

      /* calculate face centered quantities */
      float massf =     0.5f * (mass[upWind]     + mass[downWind]);
      float momentumf = 0.5f * (momentum[upWind] + momentum[downWind]);
      float energyf =   0.5f * (energy[upWind]   + energy[downWind]);
      float pressuref = (gammaa - 1.0f) *
                        (energyf - 0.5f*momentumf*momentumf/massf);
      float c = sqrtf(gammaa*pressuref/massf);
      float v = momentumf/massf;

      /* Now that we have the wave speeds, we might want to */
      /* look for the max wave speed here, and update dt */
      /* appropriately right before leaving this function. */
      /* ... */

      /* OK, calculate face quantities */

      contributor = ((v >= 0.0f) ? upWind : downWind);
      massf = mass[contributor];
      momentumf = momentum[contributor];
      energyf = energy[contributor];
      pressuref = energyf - 0.5f*momentumf*momentumf/massf;
      ev = v*(gammaa - 1.0f);

      fl[i].f0 = ev*massf;
      fl[i].f1 = ev*momentumf;
      fl[i].f2 = ev*(energyf - pressuref);

      contributor = ((v + c >= 0.0f) ? upWind : downWind);
      massf = mass[contributor];
      momentumf = momentum[contributor];
      energyf = energy[contributor];
      pressuref = (gammaa - 1.0f)*(energyf - 0.5f*momentumf*momentumf/massf);
      ev = 0.5f*(v + c);
      cLocal = sqrtf(gammaa*pressuref/massf);

      fl[i].f0 += ev*massf;
      fl[i].f1 += ev*(momentumf + massf*cLocal);
      fl[i].f2 += ev*(energyf + pressuref + momentumf*cLocal);

      contributor = ((v - c >= 0.0f) ? upWind : downWind);
      massf = mass[contributor];
      momentumf = momentum[contributor];
      energyf = energy[contributor];
      pressuref = (gammaa - 1.0f)*(energyf - 0.5f*momentumf*momentumf/massf);
      ev = 0.5f*(v - c);
      cLocal = sqrtf(gammaa*pressuref/massf);

      fl[i].f0 += ev*massf;
      fl[i].f1 += ev*(momentumf - massf*cLocal);
      fl[i].f2 += ev*(energyf + pressuref - momentumf*cLocal);
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
                                 struct flux *fl, float dtdx)
{
   int i;

   for (i = 1; i < numElem; ++i) 
   {
      /* each element inside the tube has an upwind and downwind face */
#ifdef __MC__
      int upWind := i-1;     /* upwind face */
      int downWind := i;   /* downwind face */
#endif
#ifndef __MC__
      int upWind = i-1;     /* upwind face */
      int downWind = i;   /* downwind face */
#endif

      mass[i]     -= gammaInverse*(fl[downWind].f0 - fl[upWind].f0)*dtdx;
      momentum[i] -= gammaInverse*(fl[downWind].f1 - fl[upWind].f1)*dtdx;
      energy[i]   -= gammaInverse*(fl[downWind].f2 - fl[upWind].f2)*dtdx;
      pressure[i]  = (gammaa - 1.0f) *
                     (energy[i] - 0.5f*momentum[i]*momentum[i]/mass[i]);
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

   struct flux *fl =
      (struct flux *) malloc((numElems-1)*sizeof(struct flux));

   InitializeShockTubeMesh(numElems+1, mass, momentum, energy, pressure);

   float time = 0.0f;
   float dx = 1.0f / (float) numElems;
   float dt = 0.4f * dx;
   int currCycle = 0;

   for (currCycle=0; currCycle<numTotalCycles; ++currCycle)
   {
      // if (currCycle % dumpInterval == 0)
      //    DumpPlot(numElems, mass, momentum, energy, pressure);

      ComputeFaceInfo(numFaces, mass, momentum, energy, fl);
      UpdateElemInfo (numElems-1, mass, momentum, energy, pressure,
                      fl, dt/dx);
      time = time + dt;
   }

   DumpPlot(numElems, mass, momentum, energy, pressure);

   free(fl);
   free(pressure); free(energy); free(momentum); free(mass);

   return 0 ;
}

