// ----------------------------------------------------------------------------
// MSE operating model implemented with ADT and ADlib. Declaratation of
// minimiser layer.
// ----------------------------------------------------------------------------

#ifndef __MSEOMMIN_HPP__
#define __MSEOMMIN_HPP__


#include "D_niMseom.hpp"


// ----------------------------------------------------------------------------

class OperatingModelMin : public D_OperatingModelBase
{
protected:
  /* AD_LIBNAME niMseom */
  /* AD_ALIAS Om=D_OperatingModelMin, D_OperatingModelBase */

#include "include/Om_array_plans.hpp"

  /* AUTODEC */
  ARRAY_1D TACbyF/* nfleets */;

public:
  ARRAY_4D nd1_par/* nareas,nsubyears + 1,nages,npop */;
  ARRAY_4D nbefored1_par/* nareas,nsubyears + 1,nages,npop */;
  ARRAY_4D ssnd1_par/* nareas,nsubyears,nages,npop */;
  ARRAY_5D cd1_par/* nfleets,nareas,nsubyears,nages,npop */;
  ARRAY_1D last_par/* nfleets */;

#ifndef AD
private:
  struct OptimContext
  {
    int         nPar;
    int         nFixed;
    int         nFnCalls;
    int         nGradCalls;
    int         Report;
    int         ProjectionYear;
    ARRAY_1D    TAC;
    ARRAY_1D    TAE;
    ARRAY_1I    FbyPar;
    ARRAY_1I    FbyFixed;
    ARRAY_3D    ECurrent/* nfleets, nareas, nsubyears */;
    ARRAY_1D    qy/* nfleets */;
    ARRAY_1D    R0/* npop */;
    ARRAY_2D    M/* nages, npop */;
    ARRAY_2D    mat/* nages, npop */;
    ARRAY_3D    Idist/* nareas, nages, npop */;
    ARRAY_2D    Len_age/* nages, npop */;
    ARRAY_2D    Wt_age/* nages, npop */;
    ARRAY_2D    Wt_age_mid/* nages, npop */;
    ARRAY_2D    sel/* nages, nfleets */;
    ARRAY_5D    mov/* nareas, nareas, nsubyears, nages, npop */;
    ARRAY_1D    h/* npop */;
    ARRAY_2D    Recdist/* nareas, npop */;
    ARRAY_2D    Recdevs/* SpawnPerYr, npop */;
    ARRAY_2D    RecSpatialDevs/* nareas, npop */;
    ARRAY_1I    SRrel/* npop */;
    ARRAY_4D    N/* nareas, nsubyears + 1, nages, npop */;
    ARRAY_4D    NBefore/* nareas, nsubyears + 1, nages, npop */;
    ARRAY_4D    SSN/* nareas, nsubyears, nages, npop */;
    ARRAY_5D    C/* nfleets, nareas, nsubyears, nages, npop */;
    ARRAY_1D    SSBA/* npop */;
    int         ntargets;
    ARRAY_1I    targpop/* ntargets */;
    int         run_years;
  };

  OptimContext  Context;

  static double msyrefs_optimfn(int n, double* par, void* context);
  static void   msyrefs_optimgr(int n, double* par, double* gr, void* context);

  static double projection_optimfn(int n, double* par, void* context);
  static void   projection_optimgr(int n, double* par, double* gr, void* context);

  double        findUpperLimit(double dEffortCeiling,
                               int nFleet,
                               const ARRAY_3D ECurrent/* nfleets, nareas, nsubyears */) const;
#endif

public:
  OperatingModelMin(
#include "include/Om_constructor_args.hpp"
  );

  virtual ~OperatingModelMin();

  void   findMSYrefs(const int nReport,
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
                     int maxit);

  void   beginProjection(const ARRAY_1D pPar/* nfleets */);

  void   projection(const int nProjectionYear,
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
                    int maxit);
};


#endif  //__MSEOMMIN_HPP__
