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
 *     All quantiites are non-dimensionalized.
 *
 *     @Q   @F    @Q   @F @Q
 *     -- + -- =  -- + -- -- = 0
 *     @t   @x    @t   @Q @x
 *
 *************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

float gammaa;
float gammaInverse;

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

   for (i = 0; i < midTube; ++i) {
      mass[i]     = massInitial;
      pressure[i] = pressureInitial;
      energy[i]   = energyInitial;
   }

   massInitial     = massInitial * densityRatio;
   pressureInitial = pressureInitial * pressureRatio;
   energyInitial   = pressureInitial/(gammaa - 1.0);

   for (i = midTube; i < numElem; ++i) {
      mass[i]     = massInitial;
      pressure[i] = pressureInitial;
      energy[i]   = energyInitial;
   }

   for (i=0; i < numElem; ++i) {
      momentum[i] = 0.0;
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
   int i;

   for (i=0; i<numFace; ++i)
   {
      /* each face has an upwind and downwind element. */
      int upWind   = i;     /* upwind element */
      int downWind = i + 1; /* downwind element */

      /* calculate face centered quantities */
      float massf =     0.5 * (mass[upWind]     + mass[downWind]);
      float momentumf = 0.5 * (momentum[upWind] + momentum[downWind]);
      float energyf =   0.5 * (energy[upWind]   + energy[downWind]);
      float pressuref = (gammaa - 1.0) *
                         (energyf - 0.5*momentumf*momentumf/massf);
      float c = sqrtf(gammaa*pressuref/massf);
      float v = momentumf/massf;
      float ev;
      float cLocal;
      int contributor;

      /* Now that we have the wave speeds, we might want to */
      /* look for the max wave speed here, and update dt */
      /* appropriately right before leaving this function. */
      /* ... */

      /* OK, calculate face quantities */

      f0[i] = f1[i] = f2[i] = 0.0;

      contributor = ((v >= 0.0) ? upWind : downWind);
      massf = mass[contributor];
      momentumf = momentum[contributor];
      energyf = energy[contributor];
      pressuref = energyf - 0.5*momentumf*momentumf/massf;
      ev = v*(gammaa - 1.0);

      f0[i] = f0[i] + ev*massf;
      f1[i] = f1[i] + ev*momentumf;
      f2[i] = f2[i] + ev*(energyf - pressuref);

      contributor = ((v + c >= 0.0) ? upWind : downWind);
      massf = mass[contributor];
      momentumf = momentum[contributor];
      energyf = energy[contributor];
      pressuref = (gammaa - 1.0)*(energyf - 0.5*momentumf*momentumf/massf);
      ev = 0.5*(v + c);
      cLocal = sqrtf(gammaa*pressuref/massf);

      f0[i] = f0[i] + ev*massf;
      f1[i] = f1[i] + ev*(momentumf + massf*cLocal);
      f2[i] = f2[i] + ev*(energyf + pressuref + momentumf*cLocal);

      contributor = ((v - c >= 0.0) ? upWind : downWind);
      massf = mass[contributor];
      momentumf = momentum[contributor];
      energyf = energy[contributor];
      pressuref = (gammaa - 1.0)*(energyf - 0.5*momentumf*momentumf/massf);
      ev = 0.5*(v - c);
      cLocal = sqrtf(gammaa*pressuref/massf);

      f0[i] = f0[i] + ev*massf;
      f1[i] = f1[i] + ev*(momentumf - massf*cLocal);
      f2[i] = f2[i] + ev*(energyf + pressuref - momentumf*cLocal);
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
   int i;

   for (i=1; i<numElem; ++i)
   {
      /* each element inside the tube has an upwind and downwind face */
      int upWind = i-1;     /* upwind face */
      int downWind = i;   /* downwind face */

      mass[i]     = mass[i] - gammaInverse*(f0[downWind] - f0[upWind])*dtdx;
      momentum[i] = momentum[i] - gammaInverse*(f1[downWind] - f1[upWind])*dtdx;
      energy[i]   = energy[i] - gammaInverse*(f2[downWind] - f2[upWind])*dtdx;
      pressure[i]  = (gammaa - 1.0) *
                          (energy[i] - 0.5*momentum[i]*momentum[i]/mass[i]);
   }
}


/**************************************************************************
 * Subroutine:  main
 * Purpose   :  Simulate a 1D Shock Tube using split flux Euler formulation
 *************************************************************************/

int main(void)
{
   int i, ii;
   float val;

   /* initialize gloabals */
   gammaa        = 1.4142135;
   gammaInverse = 0.70710678;

   int numElems = 512;
   int numFaces = numElems - 1;
   int numTotalCycles = 100;
   // int dumpInterval = problem->paramInt("numCyclesPerDump") ;

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
      //    DumpUltra(problem) ;

      ComputeFaceInfo(numFaces, mass, momentum, energy, f0, f1, f2);
      UpdateElemInfo (numElems, mass, momentum, energy, pressure,
                      f0, f1, f2, dt/dx);
      time += dt;
   }

   // DumpUltra(problem) ; /* One last dump */
   printf("\n# mass\n");
   for (i=0; i<numElems; ++i) {
      printf("%d.0 %f\n", i, mass[i]);
   }
   printf("\n# momentum\n");
   for (i=0; i<numElems; ++i) {
      printf("%d.0 %f\n", i, momentum[i]);
   }
   printf("\n# energy\n");
   for (i=0; i<numElems; ++i) {
      printf("%d.0 %f\n", i, energy[i]);
   }
   printf("\n# pressure\n");
   for (i=0; i<numElems; ++i) {
      printf("%d.0 %f\n", i, pressure[i]);
   }

   free(f2); free(f1); free(f0);
   free(pressure); free(energy); free(momentum); free(mass);

   return 0 ;
}

