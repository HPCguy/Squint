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
struct con  { float mass, mom, energy; };

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

void InitializeShockTubeMesh(int numElem, struct con *cv, float *pressure)
{
   struct con *cv_ = cv;
   float *p = pressure;
   int i;
   int midTube = numElem / 2;

   float massInitial     = 1.0f;
   float pressureInitial = gammaInverse;
   float energyInitial   = pressureInitial/(gammaa-1.0f);

   float pressureRatio = 0.4f;
   float densityRatio = 0.7f;

   for (i = 0; i < midTube; ++i) {
      cv_->mass   = massInitial;
      cv_->mom    = 0.0f;
      cv_->energy = energyInitial;
      ++cv_;
      *p++        = pressureInitial;
   }

   massInitial     = massInitial * densityRatio;
   pressureInitial = pressureInitial * pressureRatio;
   energyInitial   = pressureInitial/(gammaa - 1.0f);

   for (i = midTube; i < numElem; ++i) {
      cv_->mass   = massInitial;
      cv_->mom    = 0.0f;
      cv_->energy = energyInitial;
      ++cv_;
      *p++        = pressureInitial;
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

void ComputeFaceInfo(int numFace, struct con *cv, struct flux *fl)
{
   struct con *cv_ = cv;
   struct flux *fl_ = fl;
   float f0_, f1_, f2_;
   struct con *contributor;
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
      float massf =     0.5f * (cv_[upWind].mass   + cv_[downWind].mass);
      float momentumf = 0.5f * (cv_[upWind].mom    + cv_[downWind].mom);
      float energyf =   0.5f * (cv_[upWind].energy + cv_[downWind].energy);
      float pressuref = (gammaa - 1.0f) *
                        (energyf - 0.5f*momentumf*momentumf/massf);
      float c = sqrtf(gammaa*pressuref/massf);
      float v = momentumf/massf;

      /* Now that we have the wave speeds, we might want to */
      /* look for the max wave speed here, and update dt */
      /* appropriately right before leaving this function. */
      /* ... */

      /* OK, calculate face quantities */

      contributor = ((v >= 0.0f) ? &cv_[upWind] : &cv_[downWind]);
      massf     = contributor->mass;
      momentumf = contributor->mom;
      energyf   = contributor->energy;
      pressuref = energyf - 0.5f*momentumf*momentumf/massf;
      ev = v*(gammaa - 1.0f);

      f0_ = ev*massf;
      f1_ = ev*momentumf;
      f2_ = ev*(energyf - pressuref);

      contributor = ((v + c >= 0.0f) ? &cv_[upWind] : &cv_[downWind]);
      massf     = contributor->mass;
      momentumf = contributor->mom;
      energyf   = contributor->energy;
      pressuref = (gammaa - 1.0f)*(energyf - 0.5f*momentumf*momentumf/massf);
      ev = 0.5f*(v + c);
      cLocal = sqrtf(gammaa*pressuref/massf);

      f0_ += ev*massf;
      f1_ += ev*(momentumf + massf*cLocal);
      f2_ += ev*(energyf + pressuref + momentumf*cLocal);

      contributor = ((v - c >= 0.0f) ? &cv_[upWind] : &cv_[downWind]);
      massf     = contributor->mass;
      momentumf = contributor->mom;
      energyf   = contributor->energy;
      ++cv_;
      pressuref = (gammaa - 1.0f)*(energyf - 0.5f*momentumf*momentumf/massf);
      ev = 0.5f*(v - c);
      cLocal = sqrtf(gammaa*pressuref/massf);

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

void UpdateElemInfo(int numElem, struct con *cv, float *pressure,
                                 struct flux *fl, float dtdx)
{
   struct con *cv_ = &cv[1];
   struct flux *fl_ = &fl[1];
   float *p = pressure+1;

   for (int i = numElem - 1; i > 0; --i)
   {
      /* each element inside the tube has an upwind and downwind face */
#ifdef __MC__
      int upWind :=  -1;   /* upwind face */
      int downWind := 0;   /* downwind face */
#endif
#ifndef __MC__
      int upWind =  -1;   /* upwind face */
      int downWind = 0;   /* downwind face */
#endif

      float m_ = cv_->mass, mo_ = cv_->mom, e_ = cv_->energy;
      m_  -= gammaInverse*(fl_[downWind].f0 - fl_[upWind].f0)*dtdx;
      mo_ -= gammaInverse*(fl_[downWind].f1 - fl_[upWind].f1)*dtdx;
      e_  -= gammaInverse*(fl_[downWind].f2 - fl_[upWind].f2)*dtdx;
      ++fl_;
      *p++ = (gammaa - 1.0f) *
             ((cv_->energy = e_) - 0.5f*mo_*(cv_->mom = mo_) /
              (cv_->mass = m_));
      ++cv_;
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

void DumpPlot(int numElem, struct con *cv, float *pressure)
{
   DumpField("# mass\n", numElem, &cv[0].mass, 3);
   DumpField("\n\n# momentum\n", numElem, &cv[0].mom, 3);
   DumpField("\n\n# energy\n", numElem, &cv[0].energy, 3);
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

   struct con *cn =
      (struct con *) malloc((numElems+1)*sizeof(struct con));
   float *pressure = (float *) malloc((numElems+1)*sizeof(float));

   struct flux *fl =
      (struct flux *) malloc((numElems-1)*sizeof(struct flux));

   InitializeShockTubeMesh(numElems+1, cn, pressure);

   float time = 0.0f;
   float dx = 1.0f / (float) numElems;
   float dt = 0.4f * dx;
   int currCycle;

   for (currCycle=0; currCycle<numTotalCycles; ++currCycle)
   {
      // if (currCycle % dumpInterval == 0)
      //    DumpPlot(numElems, mass, momentum, energy, pressure);

      ComputeFaceInfo(numFaces, cn, fl);
      UpdateElemInfo (numElems-1, cn, pressure,
                      fl, dt/dx);
      time = time + dt;
   }

   DumpPlot(numElems, cn, pressure);

   free(fl); free(pressure); free(cn);

   return 0 ;
}

