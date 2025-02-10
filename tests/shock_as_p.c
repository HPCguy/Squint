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

#define gammaa        1.4142135f
#define gammaInverse  0.70710678f

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

   float *m = mass, *e = energy, *p = pressure;
   for (i = 0; i < midTube; ++i) {
      *m++ = massInitial;
      *p++ = pressureInitial;
      *e++ = energyInitial;
   }

   massInitial     = massInitial * densityRatio;
   pressureInitial = pressureInitial * pressureRatio;
   energyInitial   = pressureInitial/(gammaa - 1.0f);

   for (i = midTube; i < numElem; ++i) {
      *m++ = massInitial;
      *p++ = pressureInitial;
      *e++ = energyInitial;
   }

   m = momentum;
   for (i = 0; i < numElem; ++i) {
      *m++ = 0.0f;
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
   float *m = mass, *mo = momentum, *e = energy;
   struct flux *fl_ = fl;
   float f0_, f1_, f2_;
   int contributor;
   float ev;
   float cLocal;

   for (int i = numFace; i > 0; --i)
   {
      /* each face has an upwind and downwind element. */
#ifdef __MC__
      int upWind   := 0; /* upwind element */
      int downWind := 1; /* downwind element */
#endif
#ifndef __MC__
      int upWind   = 0; /* upwind element */
      int downWind = 1; /* downwind element */
#endif

      /* calculate face centered quantities */
      float massf =     0.5f * ( m[upWind] +  m[downWind]);
      float momentumf = 0.5f * (mo[upWind] + mo[downWind]);
      float energyf =   0.5f * ( e[upWind] +  e[downWind]);
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
      massf     =  m[contributor];
      momentumf = mo[contributor];
      energyf   =  e[contributor];
      pressuref = energyf - 0.5f*momentumf*momentumf/massf;
      ev = v*(gammaa - 1.0f);

      f0_ = ev*massf;
      f1_ = ev*momentumf;
      f2_ = ev*(energyf - pressuref);

      contributor = ((v + c >= 0.0f) ? upWind : downWind);
      massf     =  m[contributor];
      momentumf = mo[contributor];
      energyf   =  e[contributor];
      pressuref = (gammaa - 1.0f)*(energyf - 0.5f*momentumf*momentumf/massf);
      ev = 0.5f*(v + c);
      cLocal = sqrtf(gammaa*pressuref/massf);

      f0_ += ev*massf;
      f1_ += ev*(momentumf + massf*cLocal);
      f2_ += ev*(energyf + pressuref + momentumf*cLocal);

      contributor = ((v - c >= 0.0f) ? upWind : downWind);
      massf     =  m[contributor];
      momentumf = mo[contributor];
      energyf   =  e[contributor];
      pressuref = (gammaa - 1.0f)*(energyf - 0.5f*momentumf*momentumf/massf);
      ++m;
      ev = 0.5f*(v - c); ++mo;
      cLocal = sqrtf(gammaa*pressuref/massf); ++e;

      f0_ += ev*massf;
      fl_->f0 = f0_;
      f1_ += ev*(momentumf - massf*cLocal);
      fl_->f1 = f1_;
      f2_ += ev*(energyf + pressuref - momentumf*cLocal);
      fl_->f2 = f2_;

      ++fl_;
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
   float *m = mass+1, *mo = momentum+1, *e = energy+1, *p = pressure+1;
   struct flux *fl_ = &fl[1];

   for (int i = numElem - 1; i > 0; --i)
   {
      /* each element inside the tube has an upwind and downwind face */
#ifdef __MC__
      int upWind  := -1;   /* upwind face */
      int downWind := 0;   /* downwind face */
#endif
#ifndef __MC__
      int upWind  = -1;   /* upwind face */
      int downWind = 0;   /* downwind face */
#endif

      float m_ = *m, mo_ = *mo, e_ = *e;
      m_  -= gammaInverse*(fl_[downWind].f0 - fl_[upWind].f0)*dtdx;
      mo_ -= gammaInverse*(fl_[downWind].f1 - fl_[upWind].f1)*dtdx;
      e_  -= gammaInverse*(fl_[downWind].f2 - fl_[upWind].f2)*dtdx;
      ++fl_;
      *p++ = (gammaa - 1.0f) *
             ((*e++ = e_) - 0.5f*mo_*(*mo++ = mo_)/(*m++ = m_));
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

