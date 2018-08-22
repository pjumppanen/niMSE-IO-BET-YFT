// ----------------------------------------------------------------------------
// MSE operating model implemented with ADT and ADlib. Implementation of
// minimiser layer.
// ----------------------------------------------------------------------------


#include "niMseomMin.hpp"


// ----------------------------------------------------------------------------

OperatingModelMin::OperatingModelMin(
#include "include/Om_constructor_args.hpp"
)
 : D_OperatingModelBase(
#include "include/Om_constructor_call_args.hpp"
)
{
  #include "include/Om_constructor_locals.hpp"
  #include "include/Om_constructor_scalars_phase_1.hpp"
  #include "include/Om_constructor_arrays_phase_1.hpp"
  #include "include/Om_array_plans_init.hpp"
}

// ----------------------------------------------------------------------------

OperatingModelMin::~OperatingModelMin()
{
  destroy(nd1_par);
  destroy(nbefored1_par);
  destroy(ssnd1_par);
  destroy(cd1_par);
  destroy(last_par);
}

// ----------------------------------------------------------------------------

#ifndef AD

double OperatingModelMin::msyrefs_optimfn(int n, double* par, void* context)
{
  OperatingModelMin* pThis = (OperatingModelMin*)context;

  return pThis->MSYrefs_objective(par[0],
                                  pThis->Context.Report,
                                  pThis->Context.ECurrent,
                                  pThis->Context.qy,
                                  pThis->Context.R0,
                                  pThis->Context.M,
                                  pThis->Context.mat,
                                  pThis->Context.Idist,
                                  pThis->Context.Len_age,
                                  pThis->Context.Wt_age,
                                  pThis->Context.sel,
                                  pThis->Context.mov,
                                  pThis->Context.h,
                                  pThis->Context.Recdist,
                                  pThis->Context.SRrel,
                                  pThis->Context.N,
                                  pThis->Context.NBefore,
                                  pThis->Context.SSN,
                                  pThis->Context.C,
                                  pThis->Context.SSBA,
                                  pThis->Context.ntargets,
                                  pThis->Context.targpop,
                                  pThis->Context.run_years);
}

// ----------------------------------------------------------------------------

void OperatingModelMin::msyrefs_optimgr(int n, double* par, double* gr, void* context)
{
  OperatingModelMin*  pThis             = (OperatingModelMin*)context;
  double              pard1_par         = 1.0;
  double              MSYrefs_objective = 0.0;

  gr[0] = pThis->MSYREFS_OBJECTIVE_DPAR(par[0],
                                        pard1_par,
                                        pThis->Context.Report,
                                        pThis->Context.ECurrent,
                                        pThis->Context.qy,
                                        pThis->Context.R0,
                                        pThis->Context.M,
                                        pThis->Context.mat,
                                        pThis->Context.Idist,
                                        pThis->Context.Len_age,
                                        pThis->Context.Wt_age,
                                        pThis->Context.sel,
                                        pThis->Context.mov,
                                        pThis->Context.h,
                                        pThis->Context.Recdist,
                                        pThis->Context.SRrel,
                                        pThis->Context.N,
                                        pThis->nd1_par,
                                        pThis->Context.NBefore,
                                        pThis->nbefored1_par,
                                        pThis->Context.SSN,
                                        pThis->Context.C,
                                        pThis->cd1_par,
                                        pThis->Context.SSBA,
                                        pThis->Context.ntargets,
                                        pThis->Context.targpop,
                                        pThis->Context.run_years,
                                        MSYrefs_objective);
}

#endif

// ----------------------------------------------------------------------------

void OperatingModelMin::findMSYrefs(const int nReport,
                                    const ARRAY_3D ECurrent/* nfleets, nareas, nsubyears */,
                                    const ARRAY_1D qy/* nfleets */,
                                    const ARRAY_1D R0/* npop */,
                                    const ARRAY_2D M/* nages, npop */,
                                    const ARRAY_2D mat/* nages, npop */,
                                    const ARRAY_3D Idist/* nareas, nages, npop */,
                                    const ARRAY_2D Len_age/* nages, npop */,
                                    const ARRAY_2D Wt_age/* nages, npop */,
                                    const ARRAY_2D sel/* nages, nfleets */,
                                    const ARRAY_5D mov/* nareas, nareas, nsubyears, nages, npop */,
                                    const ARRAY_1D h/* npop */,
                                    const ARRAY_2D Recdist/* nareas, npop */,
                                    const ARRAY_1I SRrel/* npop */,
                                    ARRAY_4D N/* nareas, nsubyears + 1, nages, npop */,
                                    ARRAY_4D NBefore/* nareas, nsubyears + 1, nages, npop */,
                                    ARRAY_4D SSN/* nareas, nsubyears, nages, npop */,
                                    ARRAY_5D C/* nfleets, nareas, nsubyears, nages, npop */,
                                    ARRAY_1D SSBA/* npop */,
                                    const int ntargets,
                                    const ARRAY_1I targpop/* ntargets */,
                                    const int run_years,
                                    double& MinPar,
                                    double& MSY,
                                    double& BMSY,
                                    double& SSBMSY,
                                    double& SSBMSY_B0,
                                    int maxit)
{
#ifndef AD
  bool    bReport   = (nReport != 0);
  char    msg[256]  = {0};
  int     trace     = 0; // Don't print minimsation messages
  double  x         = log(0.001); //log(c(0.0001,10.0)) range
  double  val       = 0.0;
  int     nREPORT   = 100000;
  int     mask      = 1;
  double  factr     = 1.0e+08; // The iteration will stop when
                               //
                               //   (f^k - f^{k+1})/max{|f^k|,|f^{k+1}|,1} <= factr*epsmch
                               //
                               // where epsmch is the machine precision, which is
                               // automatically generated by the code.
  double  pgtol     = 1.0e-5; // The iteration will stop when
                              //
                              //   max{|proj g_i | i = 1, ..., n} <= pgtol
                              //
                              // where pg_i is the ith component of the projected gradient.
  double  lower     = log(1.0e-18);
  double  upper     = log(1000.0);
  int     m         = 10;// m is the maximum number of variable metric corrections allowed in the limited memory matrix. What does that mean?
  int     nbd       = 2; // Bound type 0 if x(i) is unbounded,
                         //            1 if x(i) has only a lower bound,
                         //            2 if x(i) has both lower and upper bounds, and
                         //            3 if x(i) has only an upper bound.

  int     fncount = 0;
  int     grcount = 0;
  int     status  = 0;

  Context.nFnCalls        = 0;
  Context.nGradCalls      = 0;
  Context.Report          = nReport;
  Context.ProjectionYear  = 0;
  Context.TAC             = 0;
  Context.TAE             = 0;
  Context.FbyPar          = 0;
  Context.FbyFixed        = 0;
  Context.ECurrent        = ECurrent;
  Context.qy              = qy;
  Context.R0              = R0;
  Context.M               = M;
  Context.mat             = mat;
  Context.Idist           = Idist;
  Context.Len_age         = Len_age;
  Context.Wt_age          = Wt_age;
  Context.Wt_age_mid      = 0;
  Context.sel             = sel;
  Context.mov             = mov;
  Context.h               = h;
  Context.Recdist         = Recdist;
  Context.Recdevs         = 0;
  Context.RecSpatialDevs  = 0;
  Context.SRrel           = SRrel;
  Context.N               = N;
  Context.NBefore         = NBefore;
  Context.SSN             = SSN;
  Context.C               = C;
  Context.SSBA            = SSBA;
  Context.ntargets        = ntargets;
  Context.targpop         = targpop;
  Context.run_years       = run_years;

  initialiseParameters(M,
                       R0,
                       mat,
                       Idist,
                       Wt_age,
                       h);

  ts_lbfgsb(1,
            m,
            &x,
            &lower,
            &upper,
            &nbd,
            &val,
            OperatingModelMin::msyrefs_optimfn,
            OperatingModelMin::msyrefs_optimgr,
            &status,
            (void*)this,
            factr,
            pgtol,
            &fncount,
            &grcount,
            maxit,
            msg,
            trace,
            nREPORT);

  if (bReport)
  {
    Rprintf("\n%s\n\n", msg);
  }

  MinPar = x; //x is an in-out parameter of ts_lbfgsb()

  MSYrefs(x,
          ECurrent,
          qy,
          R0,
          M,
          mat,
          Idist,
          Len_age,
          Wt_age,
          sel,
          mov,
          h,
          Recdist,
          SRrel,
          N,
          NBefore,
          SSN,
          C,
          SSBA,
          ntargets,
          targpop,
          run_years,
          MSY,
          BMSY,
          SSBMSY,
          SSBMSY_B0);
#endif
}

// ----------------------------------------------------------------------------
#ifndef AD

double OperatingModelMin::projection_optimfn(int n, double* par, void* context)
{
  OperatingModelMin* pThis = (OperatingModelMin*)context;

  pThis->Context.nFnCalls++;

  return pThis->popdyn_projection_objective(par,
                                            pThis->Context.nPar,
                                            pThis->Context.nFixed,
                                            pThis->Context.TAC,
                                            pThis->Context.TAE,
                                            pThis->Context.FbyPar,
                                            pThis->Context.FbyFixed,
                                            pThis->Context.ECurrent,
                                            pThis->Context.qy,
                                            pThis->Context.R0,
                                            pThis->Context.M,
                                            pThis->Context.mat,
                                            pThis->Context.Idist,
                                            pThis->Context.Len_age,
                                            pThis->Context.Wt_age,
                                            pThis->Context.Wt_age_mid,
                                            pThis->Context.sel,
                                            pThis->Context.mov,
                                            pThis->Context.h,
                                            pThis->Context.Recdist,
                                            pThis->Context.Recdevs,
                                            pThis->Context.RecSpatialDevs,
                                            pThis->Context.SRrel,
                                            pThis->Context.N,
                                            pThis->Context.NBefore,
                                            pThis->Context.SSN,
                                            pThis->Context.C,
                                            pThis->Context.SSBA);
}

// ----------------------------------------------------------------------------

void OperatingModelMin::projection_optimgr(int n, double* par, double* gr, void* context)
{
  OperatingModelMin*  pThis                             = (OperatingModelMin*)context;
  double              popdyn_projection_objectiveb2_par = 1.0;
  int                 cn;

  pThis->Context.nGradCalls++;

  for (cn = 0 ; cn < pThis->Context.nPar ; cn++)
  {
    gr[cn] = 0.0;
  }

  pThis->POPDYN_PROJECTION_OBJECTIVE_BPAR(par,
                                          gr,
                                          pThis->Context.nPar,
                                          pThis->Context.nFixed,
                                          pThis->Context.TAC,
                                          pThis->Context.TAE,
                                          pThis->Context.FbyPar,
                                          pThis->Context.FbyFixed,
                                          pThis->Context.ECurrent,
                                          pThis->Context.qy,
                                          pThis->Context.R0,
                                          pThis->Context.M,
                                          pThis->Context.mat,
                                          pThis->Context.Idist,
                                          pThis->Context.Len_age,
                                          pThis->Context.Wt_age,
                                          pThis->Context.Wt_age_mid,
                                          pThis->Context.sel,
                                          pThis->Context.mov,
                                          pThis->Context.h,
                                          pThis->Context.Recdist,
                                          pThis->Context.Recdevs,
                                          pThis->Context.RecSpatialDevs,
                                          pThis->Context.SRrel,
                                          pThis->Context.N,
                                          pThis->nd1_par,
                                          pThis->Context.NBefore,
                                          pThis->nbefored1_par,
                                          pThis->Context.SSN,
                                          pThis->Context.C,
                                          pThis->cd1_par,
                                          pThis->Context.SSBA,
                                          popdyn_projection_objectiveb2_par);
}

// ----------------------------------------------------------------------------

double OperatingModelMin::findUpperLimit(double dEffortCeiling,
                                         int nFleet,
                                         const ARRAY_3D ECurrent/* nfleets, nareas, nsubyears */) const
{
  int     ca;
  int     cm;
  double  dLimit;
  double  dMaxE = 0.0;
  double  dSumE = 0.0;

  // dEffortCeiling corresponds to the maximum anualised E allowed in fitting
  // the foward projection.
  for (ca = 1 ; ca <= nareas ; ca++)
  {
    for (cm = 1 ; cm <= nsubyears ; cm++)
    {
      dSumE += ECurrent[nFleet][ca][cm];
    }
  }

  dLimit = (dSumE != 0.0) ? dEffortCeiling / dSumE : dEffortCeiling;

  return (dLimit);
}

#endif

// ----------------------------------------------------------------------------

void OperatingModelMin::beginProjection(const ARRAY_1D pPar/* nfleets */)
{
  int cf;

  // reset the last_par vector to initial supplied value
  for (cf = 1 ; cf <= nfleets ; cf++)
  {
    last_par[cf] = pPar[cf];
  }
}

// ----------------------------------------------------------------------------

void OperatingModelMin::projection(const int nProjectionYear,
                                   const int nReport,
                                   const double dEffortCeiling,
                                   const double dTAC,
                                   const ARRAY_1D TAEbyF /* nfleets */,
                                   const ARRAY_1D TACEError /* nfleets */,
                                   const ARRAY_3D ECurrent/* nfleets,nareas,nsubyears */,
                                   const ARRAY_3D CMCurrent/* nfleets,nareas,nsubyears */,
                                   const ARRAY_1D qy/* nfleets */,
                                   const ARRAY_1D R0/* npop */,
                                   const ARRAY_2D M/* nages, npop */,
                                   const ARRAY_2D mat/* nages, npop */,
                                   const ARRAY_3D Idist/* nareas, nages, npop */,
                                   const ARRAY_2D Len_age/* nages, npop */,
                                   const ARRAY_2D Wt_age/* nages, npop */,
                                   const ARRAY_2D Wt_age_mid/* nages, npop */,
                                   const ARRAY_2D sel/* nages, nfleets */,
                                   const ARRAY_5D mov/* nareas, nareas, nsubyears, nages, npop */,
                                   const ARRAY_1D h/* npop */,
                                   const ARRAY_2D Recdist/* nareas, npop */,
                                   const ARRAY_2D Recdevs/* SpawnPerYr, npop */,
                                   const ARRAY_2D RecSpatialDevs/* nareas, npop */,
                                   const ARRAY_1I SRrel/* npop */,
                                   ARRAY_4D N/* nareas, nsubyears + 1, nages, npop */,
                                   ARRAY_4D NBefore/* nareas, nsubyears + 1, nages, npop */,
                                   ARRAY_4D SSN/* nareas, nsubyears, nages, npop */,
                                   ARRAY_5D C/* nfleets, nareas, nsubyears, nages, npop */,
                                   ARRAY_1D SSBA/* npop */,
                                   int maxit)
{
#ifndef AD
  bool    bReport = (nReport != 0);
  int     cf;
  int     cr;
  int     cm;
  double  dSum;
  double  dSumAll;

  // Calculate TACbyF from TAC and CMCurrent. Need to check for TAEbyF to see
  // whether to exclude it from TAC
  dSumAll = 0.0;

  for (cf = 1 ; cf <= nfleets ; cf++)
  {
    dSum = 0.0;

    if (TAEbyF[cf] <= 0.0)
    {
      for (cr = 1 ; cr <= nareas ; cr++)
      {
        for (cm = 1 ; cm <= nsubyears ; cm++)
        {
          dSum += isnan(CMCurrent[cf][cr][cm]) ? 0.0 : CMCurrent[cf][cr][cm];
        }
      }
    }

    TACbyF[cf] = dSum;
    dSumAll   += dSum;
  }

  for (cf = 1 ; cf <= nfleets ; cf++)
  {
    if (dSumAll == 0.0)
    {
      TACbyF[cf] = 0.0;
    }
    else
    {
      TACbyF[cf] *= dTAC / dSumAll;
    }
  }

  int                 cn;
  ARRAY_1D            px;
  ARRAY_1D            plower;
  ARRAY_1D            pupper;
  ARRAY_1I            pnbd;
  ARRAY_1D            TAC;
  ARRAY_1D            TAE;
  ARRAY_1I            FbyPar;
  ARRAY_1I            FbyFixed;
  double              dSumTAC  = 0.0;
  bool                bClosure = false;

  if (bReport)
  {
    Rprintf("\nProjection in year = %d\n", Context.ProjectionYear);
  }

  AdtArrayPlan  ArrayPlan(0, nfleets);

  ArrayPlan.create(MemAllocator, px);
  ArrayPlan.create(MemAllocator, plower);
  ArrayPlan.create(MemAllocator, pupper);
  ArrayPlan.create(MemAllocator, pnbd);
  ArrayPlan.create(MemAllocator, TAC);
  ArrayPlan.create(MemAllocator, TAE);
  ArrayPlan.create(MemAllocator, FbyPar);
  ArrayPlan.create(MemAllocator, FbyFixed);

  if ((plower   != 0) &&
      (pupper   != 0) &&
      (pnbd     != 0) &&
      (TAC      != 0) &&
      (TAE      != 0) &&
      (FbyPar   != 0) &&
      (FbyFixed != 0))
  {
    Context.nPar   = 0;
    Context.nFixed = 0;

    for (cn = 1 ; cn <= nfleets ; cn++)
    {
      if (TACbyF[cn] <= 0.0)
      {
        // Uncontrolled fishery
        TAE[Context.nFixed]       = TAEbyF[cn];
        FbyFixed[Context.nFixed]  = cn;

        Context.nFixed++;
      }
      else
      {
        // Controlled fishery
        TAC[Context.nPar]     = TACbyF[cn];
        px[Context.nPar]      = last_par[cn];
        FbyPar[Context.nPar]  = cn;
        plower[Context.nPar]  = log(1.0e-18);
        pupper[Context.nPar]  = log(findUpperLimit(dEffortCeiling, cn, ECurrent));
        pnbd[Context.nPar]    = 2;// Bound type 0 if x(i) is unbounded,
                                  //            1 if x(i) has only a lower bound,
                                  //            2 if x(i) has both lower and upper bounds, and
                                  //            3 if x(i) has only an upper bound.

        dSumTAC += TAC[Context.nPar];
        Context.nPar++;
      }
    }

    if ((Context.nPar > 0) && (dSumTAC > 0.0))
    {
      char    msg[256]  = {0};
      int     trace     = 0; // Don't print minimsation messages
//      int     trace     = 6; // Don't print minimsation messages
      double  val       = 0.0;
      int     nREPORT   = 100000;
      int     mask      = 1;
      double  factr     = 1.0e+8;  // The iteration will stop when
                                   //
                                   //   (f^k - f^{k+1})/max{|f^k|,|f^{k+1}|,1} <= factr*epsmch
                                   //
                                   // where epsmch is the machine precision, which is
                                   // automatically generated by the code.
      double  pgtol     = 1.0e-5;  // The iteration will stop when
                                   //
                                   //   max{|proj g_i | i = 1, ..., n} <= pgtol
                                   //
                                   // where pg_i is the ith component of the projected gradient.
      int     m         = 10;      // m is the maximum number of variable metric corrections allowed in the limited memory matrix. What does that mean?
      int     fncount   = 0;
      int     grcount   = 0;
      int     status    = 0;
      int     nRetry    = 2;

      Context.nFnCalls        = 0;
      Context.nGradCalls      = 0;
      Context.Report          = nReport;
      Context.ProjectionYear  = nProjectionYear;
      Context.TAC             = TAC;
      Context.TAE             = TAE;
      Context.FbyPar          = FbyPar;
      Context.FbyFixed        = FbyFixed;
      Context.ECurrent        = ECurrent;
      Context.qy              = qy;
      Context.R0              = R0;
      Context.M               = M;
      Context.mat             = mat;
      Context.Idist           = Idist;
      Context.Len_age         = Len_age;
      Context.Wt_age          = Wt_age;
      Context.Wt_age_mid      = Wt_age_mid;
      Context.sel             = sel;
      Context.mov             = mov;
      Context.h               = h;
      Context.Recdist         = Recdist;
      Context.Recdevs         = Recdevs;
      Context.RecSpatialDevs  = RecSpatialDevs;
      Context.SRrel           = SRrel;
      Context.N               = N;
      Context.NBefore         = NBefore;
      Context.SSN             = SSN;
      Context.C               = C;
      Context.SSBA            = SSBA;
      Context.ntargets        = 0;
      Context.targpop         = 0;
      Context.run_years       = 0;

      // This retry this is probably masking a bug somewhere in my code
      // leading to low level noise in the derivative evaluations.
      while (nRetry > 0)
      {
        ts_lbfgsb(Context.nPar,
                  m,
                  px,
                  plower,
                  pupper,
                  pnbd,
                  &val,
                  OperatingModelMin::projection_optimfn,
                  OperatingModelMin::projection_optimgr,
                  &status,
                  (void*)this,
                  factr,
                  pgtol,
                  &fncount,
                  &grcount,
                  maxit,
                  msg,
                  trace,
                  nREPORT);

        if (status == 0)
        {
          break;
        }

        nRetry--;
      }

      if (status == 0)
      {
        for (cn = 0 ; cn < Context.nPar ; cn++)
        {
          last_par[FbyPar[cn]] = px[cn];//Save the last par as it should
                                        //be close to the value needed
        }                               //for successive years.
      }
      else
      {
        for (cn = 0 ; cn < Context.nPar ; cn++)
        {
          px[cn] = log(0.001);
        }
      }

      if (bReport)
      {
        Rprintf("\nobjective = %g, function count = %d, gradient count = %d\n", val, Context.nFnCalls, Context.nGradCalls);
      }
    }

    // Apply TACEError by modifying px and TAE
    for (cn = 0 ; cn < Context.nPar ; cn++)
    {
      px[cn] += log(TACEError[FbyPar[cn]]);

      LastEbyF[FbyPar[cn]] = exp(px[cn]);
    }

    for (cn = 0 ; cn < Context.nFixed ; cn++)
    {
      TAE[cn] *= TACEError[FbyFixed[cn]];

      LastEbyF[FbyFixed[cn]] = TAE[cn];
    }

    runProjection(px,
                  Context.nPar,
                  Context.nFixed,
                  TAC,
                  TAE,
                  FbyPar,
                  FbyFixed,
                  ECurrent,
                  qy,
                  R0,
                  M,
                  mat,
                  Idist,
                  Len_age,
                  Wt_age,
                  sel,
                  mov,
                  h,
                  Recdist,
                  Recdevs,
                  RecSpatialDevs,
                  SRrel,
                  N,
                  NBefore,
                  SSN,
                  C,
                  SSBA);

    if (bReport)
    {
      for (cn = 0 ; cn < Context.nPar ; cn++)
      {
        double  dVulnerableBiomass = 0.0;
        double  dCatchBiomass      = 0.0;
        int     cf                 = FbyPar[cn];
        int     cr;
        int     ca;
        int     cp;
        int     cs;

        for (cr = 1 ; cr <= nareas ; cr++)
        {
          for (ca = 1 ; ca <= nages ; ca++)
          {
            for (cp = 1 ; cp <= npop ; cp++)
            {
              for (cs = 1 ; cs <= nsubyears ; cs++)
              {
                dCatchBiomass += C[cf][cr][cs][ca][cp] * Wt_age_mid[ca][cp];
              }

              dVulnerableBiomass += NBefore[cr][1][ca][cp] * sel[ca][cf] * Wt_age_mid[ca][cp];
            }
          }
        }

        Rprintf("Fishery = %d, TAC = %g, Catch Biomass = %g, Vulnerable Biomass = %g, E = %g\n",
                FbyPar[cn],
                TAC[cn],
                dCatchBiomass,
                dVulnerableBiomass,
                exp(px[cn]));
      }
    }

    if (bReport)
    {
      for (cn = 0 ; cn < Context.nFixed ; cn++)
      {
        double  dVulnerableBiomass = 0.0;
        double  dCatchBiomass      = 0.0;
        int     cf                 = FbyFixed[cn];
        int     cr;
        int     ca;
        int     cp;
        int     cs;

        for (cr = 1 ; cr <= nareas ; cr++)
        {
          for (ca = 1 ; ca <= nages ; ca++)
          {
            for (cp = 1 ; cp <= npop ; cp++)
            {
              for (cs = 1 ; cs <= nsubyears ; cs++)
              {
                dCatchBiomass += C[cf][cr][cs][ca][cp] * Wt_age_mid[ca][cp];
              }

              dVulnerableBiomass += NBefore[cr][1][ca][cp] * sel[ca][cf] * Wt_age_mid[ca][cp];
            }
          }
        }

        Rprintf("Fishery = %d, TAE = %g, Catch Biomass = %g, Vulnerable Biomass = %g\n",
                FbyFixed[cn],
                TAE[cn],
                dCatchBiomass,
                dVulnerableBiomass);
      }

      Rprintf("\n");
    }
  }

  ArrayPlan.destroy(MemAllocator, px);
  ArrayPlan.destroy(MemAllocator, plower);
  ArrayPlan.destroy(MemAllocator, pupper);
  ArrayPlan.destroy(MemAllocator, pnbd);
  ArrayPlan.destroy(MemAllocator, TAC);
  ArrayPlan.destroy(MemAllocator, TAE);
  ArrayPlan.destroy(MemAllocator, FbyPar);
  ArrayPlan.destroy(MemAllocator, FbyFixed);
#endif // AD
}
