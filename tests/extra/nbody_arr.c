// rename_register1, apply_ptr_cleanup, holes7_8

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

typedef float Real;

#define pi 3.141592653589793f
#define solar_mass (4.0f * pi * pi)
#define days_per_year 365.24f
#define NBODIES 5
#define DT 0.01f


void advance(Real *x, Real *y, Real *z,
             Real *vx, Real *vy, Real *vz, Real *mass,
             int n)
{
    int i, j, k;
    Real x1, y1, z1, dx, dy, dz, R, mag;
    for (k = 0; k < n; ++k)
    {
        for (i = 0; i < NBODIES; ++i)
        {
            x1 = x[i];
            y1 = y[i];
            z1 = z[i];
            for (j = i + 1; j < NBODIES; ++j)
            {
                dx = x1 - x[j];
                R = dx * dx;
                dy = y1 - y[j];
                R += dy * dy;
                dz = z1 - z[j];
                R += dz * dz;
                R = sqrtf(R);
                mag = DT / (R * R * R);
                vx[i] -= dx * mass[j] * mag;
                vy[i] -= dy * mass[j] * mag;
                vz[i] -= dz * mass[j] * mag;
                vx[j] += dx * mass[i] * mag;
                vy[j] += dy * mass[i] * mag;
                vz[j] += dz * mass[i] * mag;
            }
        }

        for (i = 0; i < NBODIES; ++i)
        {
            x[i] += DT * vx[i];
            y[i] += DT * vy[i];
            z[i] += DT * vz[i];
        }
    }
}

Real energy(Real *x, Real *y, Real *z,
            Real *vx, Real *vy, Real *vz, Real *mass)
{
    int i, j;
    Real dx, dy, dz, distance;
    Real e = 0.0f;
    for (i = 0; i < NBODIES; ++i)
    {
        e += 0.5f * mass[i] *
             (vx[i] * vx[i] + vy[i] * vy[i] + vz[i] * vz[i]);
        for (j = i + 1; j < NBODIES; ++j)
        {
            dx = x[i] - x[j];
            dy = y[i] - y[j];
            dz = z[i] - z[j];
            distance = sqrtf(dx * dx + dy * dy + dz * dz);
            e -= (mass[i] * mass[j]) / distance;
        }
    }
    return e;
}

void offset_momentum(Real *vx, Real *vy, Real *vz, Real *mass)
{
    int i;
    Real px = 0.0f, py = 0.0f, pz = 0.0f;
    for (i = 0; i < NBODIES; ++i)
    {
        px += vx[i] * mass[i];
        py += vy[i] * mass[i];
        pz += vz[i] * mass[i];
    }
    vx[0] = -px / solar_mass;
    vy[0] = -py / solar_mass;
    vz[0] = -pz / solar_mass;
}

void init(Real *x, Real *y, Real *z,
          Real *vx, Real *vy, Real *vz, Real *mass)
{
    x[0] = 0.0f;
    y[0] = 0.0f;
    z[0] = 0.0f;
    vx[0] = 0.0f;
    vy[0] = 0.0f;
    vz[0] = 0.0f;
    mass[0] = solar_mass;
    x[1] = 4.84143144246472090e+00f;
    y[1] = -1.16032004402742839e+00f;
    z[1] = -1.03622044471123109e-01f;
    vx[1] = 1.66007664274403694e-03f * days_per_year;
    vy[1] = 7.69901118419740425e-03f * days_per_year;
    vz[1] = -6.90460016972063023e-05f * days_per_year;
    mass[1] = 9.54791938424326609e-04f * solar_mass;
    x[2] = 8.34336671824457987e+00f;
    y[2] = 4.12479856412430479e+00f;
    z[2] = -4.03523417114321381e-01f;
    vx[2] = -2.76742510726862411e-03f * days_per_year;
    vy[2] = 4.99852801234917238e-03f * days_per_year;
    vz[2] = 2.30417297573763929e-05f * days_per_year;
    mass[2] = 2.85885980666130812e-04f * solar_mass;
    x[3] = 1.28943695621391310e+01f;
    y[3] = -1.51111514016986312e+01f;
    z[3] = -2.23307578892655734e-01f;
    vx[3] = 2.96460137564761618e-03f * days_per_year;
    vy[3] = 2.37847173959480950e-03f * days_per_year;
    vz[3] = -2.96589568540237556e-05f * days_per_year;
    mass[3] = 4.36624404335156298e-05f * solar_mass;
    x[4] = 1.53796971148509165e+01f;
    y[4] = -2.59193146099879641e+01f;
    z[4] = 1.79258772950371181e-01f;
    vx[4] = 2.68067772490389322e-03f * days_per_year;
    vy[4] = 1.62824170038242295e-03f * days_per_year;
    vz[4] = -9.51592254519715870e-05f * days_per_year;
    mass[4] = 5.15138902046611451e-05f * solar_mass;
}
int main(int argc, char ** argv)
{
   Real x[NBODIES], y[NBODIES], z[NBODIES];
   Real vx[NBODIES], vy[NBODIES], vz[NBODIES];
   Real mass[NBODIES];

    int n = (argc == 1) ? 10000000 : atoi(argv[1]);
    init(x, y, z, vx, vy, vz, mass);
    offset_momentum(vx, vy, vz, mass);
    printf("%.9f\n",
           energy(x, y, z, vx, vy, vz, mass));
    advance(x, y, z, vx, vy, vz, mass, n);
    printf("%.9f\n",
           energy(x, y, z, vx, vy, vz, mass));
    return 0;
}
