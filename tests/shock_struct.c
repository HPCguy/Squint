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

float gammaa       = 1.4142135;
float gammaInverse = 0.70710678;

struct flux { float f0, f1, f2; };
struct kin  { float mass, mom, energy; };

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

void InitializeShockTubeMesh(int numElem, struct kin *kv, float *pressure)
{
   int i;
   int midTube = numElem / 2;

   float massInitial     = 1.0;
   float pressureInitial = gammaInverse;
   float energyInitial   = pressureInitial/(gammaa-1.0);

   float pressureRatio = 0.4;
   float densityRatio = 0.7; 

   for (i = 0; i < midTube; ++i) {
      kv[i].mass   = massInitial;
      kv[i].energy = energyInitial;
      pressure[i]  = pressureInitial;
   }

   massInitial     = massInitial * densityRatio;
   pressureInitial = pressureInitial * pressureRatio;
   energyInitial   = pressureInitial/(gammaa - 1.0);

   for (i = midTube; i < numElem; ++i) {
      kv[i].mass   = massInitial;
      kv[i].energy = energyInitial;
      pressure[i]  = pressureInitial;
   }

   for (i = 0; i < numElem; ++i) {
      kv[i].mom = 0.0;
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

void ComputeFaceInfo(int numFace, struct kin *kv, struct flux *fl)
{
   int i;
   int contributor;
   float ev;
   float cLocal;

   for (i = 0; i < numFace; ++i)
   {
      /* each face has an upwind and downwind element. */
      int upWind   = i;     /* upwind element */
      int downWind = i + 1; /* downwind element */

      /* calculate face centered quantities */
      float massf =     0.5 * (kv[upWind].mass + kv[downWind].mass);
      float momentumf = 0.5 * (kv[upWind].mom  + kv[downWind].mom);
      float energyf =   0.5 * (kv[upWind].energy + kv[downWind].energy);
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
      massf = kv[contributor].mass;
      momentumf = kv[contributor].mom;
      energyf = kv[contributor].energy;
      pressuref = energyf - 0.5*momentumf*momentumf/massf;
      ev = v*(gammaa - 1.0);

      fl[i].f0 = ev*massf;
      fl[i].f1 = ev*momentumf;
      fl[i].f2 = ev*(energyf - pressuref);

      contributor = ((v + c >= 0.0) ? upWind : downWind);
      massf = kv[contributor].mass;
      momentumf = kv[contributor].mom;
      energyf = kv[contributor].energy;
      pressuref = (gammaa - 1.0)*(energyf - 0.5*momentumf*momentumf/massf);
      ev = 0.5*(v + c);
      cLocal = sqrtf(gammaa*pressuref/massf);

      fl[i].f0 += ev*massf;
      fl[i].f1 += ev*(momentumf + massf*cLocal);
      fl[i].f2 += ev*(energyf + pressuref + momentumf*cLocal);

      contributor = ((v - c >= 0.0) ? upWind : downWind);
      massf = kv[contributor].mass;
      momentumf = kv[contributor].mom;
      energyf = kv[contributor].energy;
      pressuref = (gammaa - 1.0)*(energyf - 0.5*momentumf*momentumf/massf);
      ev = 0.5*(v - c);
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

void UpdateElemInfo(int numElem, struct kin *kv, float *pressure,
                                 struct flux *fl, float dtdx)
{
   int i;

   for (i = 1; i < numElem; ++i) 
   {
      /* each element inside the tube has an upwind and downwind face */
      int upWind = i-1;     /* upwind face */
      int downWind = i;   /* downwind face */

      kv[i].mass   -= gammaInverse*(fl[downWind].f0 - fl[upWind].f0)*dtdx;
      kv[i].mom    -= gammaInverse*(fl[downWind].f1 - fl[upWind].f1)*dtdx;
      kv[i].energy -= gammaInverse*(fl[downWind].f2 - fl[upWind].f2)*dtdx;
      pressure[i]  = (gammaa - 1.0) *
                     (kv[i].energy - 0.5*kv[i].mom*kv[i].mom/kv[i].mass);
   }
}


/**************************************************************************
 * Subroutine:  DumpField
 * Purpose   :  Create a plot for a single field
 *************************************************************************/

void DumpField(char *tag, int numElem, float *field, int stride)
{
   int i;

   printf(tag);
   for (i = 0; i < numElem; ++i) {
      printf("%d.0 %f\n", i, field[i*stride]);
   }
}


/**************************************************************************
 * Subroutine:  DumpPlot
 * Purpose   :  create output that can be viewed with gnuplot: plot "file"
 *************************************************************************/

void DumpPlot(int numElem, struct kin *kv, float *pressure)
{
   DumpField("# mass\n", numElem, &kv[0].mass, 3);
   DumpField("\n\n# momentum\n", numElem, &kv[0].mom, 3);
   DumpField("\n\n# energy\n", numElem, &kv[0].energy, 3);
   DumpField("\n\n# pressure\n", numElem, pressure, 1);
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

   struct kin *kn =
      (struct kin *) malloc((numElems+1)*sizeof(struct kin));
   float *pressure = (float *) malloc((numElems+1)*sizeof(float));

   struct flux *fl =
      (struct flux *) malloc((numElems-1)*sizeof(struct flux));

   InitializeShockTubeMesh(numElems+1, kn, pressure);

   float time = 0.0;
   float dx = 1.0 / (float) numElems;
   float dt = 0.4 * dx;
   int currCycle = 0;

   for (currCycle=0; currCycle<numTotalCycles; ++currCycle)
   {
      // if (currCycle % dumpInterval == 0)
      //    DumpPlot(numElems, mass, momentum, energy, pressure);

      ComputeFaceInfo(numFaces, kn, fl);
      UpdateElemInfo (numElems-1, kn, pressure,
                      fl, dt/dx);
      time = time + dt;
   }

   DumpPlot(numElems, kn, pressure);

   free(fl); free(pressure); free(kn);

   return 0 ;
}

