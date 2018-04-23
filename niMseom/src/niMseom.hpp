// ----------------------------------------------------------------------------
// MSE operating model implemented with ADT and ADlib
// ----------------------------------------------------------------------------


#ifndef __niMseom_HPP__
#define __niMseom_HPP__


#include <adtarrays.hpp>
#include "adtR.hpp"


// ----------------------------------------------------------------------------

class OperatingModelBase : public AdtArrays
{
protected:
  /* AD_LIBNAME niMseom */
  /* AD_ALIAS OmB=D_OperatingModelBase */
  /* AUTOINIT */
  int       npop;
  int       nages;
  int       nsubyears;
  int       nareas;
  int       nfleets;

  ARRAY_1I  Recsubyr/* nsubyears */;

  /* AUTODEC */
  int       SpawnPerYr;

  ARRAY_3D  EforYear/* nfleets, nareas, nsubyears */;
  ARRAY_2D  surv/* nages, npop */;
  ARRAY_1D  B0/* npop */;
  ARRAY_1D  SSB0/* npop */;
  ARRAY_1D  SSBpR/* npop */;
  ARRAY_1D  aR/* npop */;
  ARRAY_1D  bR/* npop */;
  ARRAY_1D  FM/* nfleets */;
  ARRAY_1D  MovN/* nareas */;

  ARRAY_2D  MSY_RecSpatialDevs/* nareas, npop */;

  // Note that MSY_Recdevs is initialised in phase 2 because its dimension
  // depends on the contents of Recsubyr and is calculated during construction
  /* AUTODEC 2 */
  ARRAY_2D  MSY_Recdevs/* SpawnPerYr, npop */;

#include "include/OmB_array_plans.hpp"

protected:
  void    popdyn_init(const ARRAY_1D R0/* npop */,
                      const ARRAY_2D mat/* nages, npop */,
                      const ARRAY_3D Idist/* nareas, nages, npop */,
                      ARRAY_4D N/* nareas, nsubyears + 1, nages, npop */,
                      ARRAY_4D NBefore/* nareas, nsubyears + 1, nages, npop */,
                      ARRAY_4D SSN/* nareas, nsubyears, nages, npop */);

  void    popdyn_year(const ARRAY_1D qy/* nfleets */,
                      const ARRAY_1D R0/* npop */,
                      const ARRAY_2D M/* nages, npop */,
                      const ARRAY_2D mat/* nages, npop */,
                      const ARRAY_2D Len_age/* nages, npop */,
                      const ARRAY_2D Wt_age/* nages, npop */,
                      const ARRAY_2D sel/* nages, nfleets */,
                      const ARRAY_3D Eannual/* nfleets, nareas, nsubyears */,
                      const ARRAY_5D mov/* nareas, nareas, nsubyears, nages, npop */,
                      const ARRAY_1D h/* npop */,
                      const ARRAY_2D Recdist/*nareas, npop */,
                      const ARRAY_2D Recdevs/* SpawnPerYr, npop */,
                      const ARRAY_2D RecSpatialDevs/* nareas, npop */,
                      const ARRAY_1I SRrel/* npop */,
                      ARRAY_4D N/* nareas, nsubyears + 1, nages, npop */,
                      ARRAY_4D NBefore/* nareas, nsubyears + 1, nages, npop */,
                      ARRAY_4D SSN/* nareas, nsubyears, nages, npop */,
                      ARRAY_5D C/* nfleets, nareas, nsubyears, nages, npop */,
                      ARRAY_1D SSBA/* npop */,
                      int bIgnoreLast);

  void    popdyn(double totF,
                 const ARRAY_1D qy/* nfleets */,
                 const ARRAY_3D ECurrent/* nfleets, nareas, nsubyears */,
                 const ARRAY_1D R0/* npop */,
                 const ARRAY_2D M/* nages, npop */,
                 const ARRAY_2D mat/* nages, npop */,
                 const ARRAY_3D Idist/* nareas, nages, npop */,
                 const ARRAY_2D Len_age/* nages, npop */,
                 const ARRAY_2D Wt_age/* nages, npop */,
                 const ARRAY_2D sel/* nages, nfleets */,
                 const ARRAY_5D mov/* nareas, nareas, nsubyears, nages, npop */,
                 const ARRAY_1D h/* npop */,
                 const ARRAY_2D Recdist/*nareas, npop */,
                 const ARRAY_2D Recdevs/* SpawnPerYr, npop */,
                 const ARRAY_2D RecSpatialDevs/* nareas, npop */,
                 const ARRAY_1I SRrel/* npop */,
                 ARRAY_4D N/* nareas, nsubyears + 1, nages, npop */,
                 ARRAY_4D NBefore/* nareas, nsubyears + 1, nages, npop */,
                 ARRAY_4D SSN/* nareas, nsubyears, nages, npop */,
                 ARRAY_5D C/* nfleets, nareas, nsubyears, nages, npop */,
                 ARRAY_1D SSBA/* npop */);

  void    popdyn_projection_par(const ARRAY_1D par/* 0:npar-1 */,
                                const int npar,
                                const int nfixed,
                                const ARRAY_1D TAE/* 0:nfixed-1 */,
                                const ARRAY_1I FbyPar/* 0:npar-1 */,
                                const ARRAY_1I FbyFixed/* 0:nfixed-1 */,
                                const ARRAY_3D ECurrent/* nfleets,nareas,nsubyears */,
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
                                const ARRAY_2D Recdist/*nareas, npop */,
                                const ARRAY_2D Recdevs/* SpawnPerYr, npop */,
                                const ARRAY_2D RecSpatialDevs/* nareas, npop */,
                                const ARRAY_1I SRrel/* npop */,
                                ARRAY_4D N/* nareas, nsubyears + 1, nages, npop */,
                                ARRAY_4D NBefore/* nareas, nsubyears + 1, nages, npop */,
                                ARRAY_4D SSN/* nareas, nsubyears, nages, npop */,
                                ARRAY_5D C/* nfleets, nareas, nsubyears, nages, npop */,
                                ARRAY_1D SSBA/* npop */,
                                int bIgnoreLast);

  void    popdyn_MSY_par(const double par,
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
                         const ARRAY_2D Recdist/*nareas, npop */,
                         const ARRAY_1I SRrel/* npop */,
                         ARRAY_4D N/* nareas, nsubyears + 1, nages, npop */,
                         ARRAY_4D NBefore/* nareas, nsubyears + 1, nages, npop */,
                         ARRAY_4D SSN/* nareas, nsubyears, nages, npop */,
                         ARRAY_5D C/* nfleets, nareas, nsubyears, nages, npop */,
                         ARRAY_1D SSBA/* npop */,
                         const int run_years);

public:
  OperatingModelBase(
#include "include/OmB_constructor_args.hpp"
  );

  virtual ~OperatingModelBase();

  void    initialiseParameters(const ARRAY_2D M/* nages, npop */,
                               const ARRAY_1D R0/* npop */,
                               const ARRAY_2D mat/* nages, npop */,
                               const ARRAY_3D Idist/* nareas, nages, npop */,
                               const ARRAY_2D Wt_age/* nages, npop */,
                               const ARRAY_1D h/* npop */);

  void    nextYear(ARRAY_4D N/* nareas, nsubyears + 1, nages, npop */,
                   ARRAY_4D NBefore/* nareas, nsubyears + 1, nages, npop */);

  double  popdyn_projection_objective(const ARRAY_1D par/* 0:npar-1 */,
                                      const int npar,
                                      const int nfixed,
                                      const ARRAY_1D TAC/* 0:npar-1 */,
                                      const ARRAY_1D TAE/* 0:nfixed-1 */,
                                      const ARRAY_1I FbyPar/* 0:npar-1 */,
                                      const ARRAY_1I FbyFixed/* 0:nfixed-1 */,
                                      const ARRAY_3D ECurrent/* nfleets,nareas,nsubyears */,
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
                                      const ARRAY_2D Recdist/*nareas, npop */,
                                      const ARRAY_2D Recdevs/* SpawnPerYr, npop */,
                                      const ARRAY_2D RecSpatialDevs/* nareas, npop */,
                                      const ARRAY_1I SRrel/* npop */,
                                      ARRAY_4D N/* nareas, nsubyears + 1, nages, npop */,
                                      ARRAY_4D NBefore/* nareas, nsubyears + 1, nages, npop */,
                                      ARRAY_4D SSN/* nareas, nsubyears, nages, npop */,
                                      ARRAY_5D C/* nfleets, nareas, nsubyears, nages, npop */,
                                      ARRAY_1D SSBA/* npop */);

  void    runProjection(const ARRAY_1D par/* 0:npar-1 */,
                        const int npar,
                        const int nfixed,
                        const ARRAY_1D TAC/* 0:npar-1 */,
                        const ARRAY_1D TAE/* 0:nfixed-1 */,
                        const ARRAY_1I FbyPar/* 0:npar-1 */,
                        const ARRAY_1I FbyFixed/* 0:nfixed-1 */,
                        const ARRAY_3D ECurrent/* nfleets,nareas,nsubyears */,
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
                        const ARRAY_2D Recdist/*nareas, npop */,
                        const ARRAY_2D Recdevs/* SpawnPerYr, npop */,
                        const ARRAY_2D RecSpatialDevs/* nareas, npop */,
                        const ARRAY_1I SRrel/* npop */,
                        ARRAY_4D N/* nareas, nsubyears + 1, nages, npop */,
                        ARRAY_4D NBefore/* nareas, nsubyears + 1, nages, npop */,
                        ARRAY_4D SSN/* nareas, nsubyears, nages, npop */,
                        ARRAY_5D C/* nfleets, nareas, nsubyears, nages, npop */,
                        ARRAY_1D SSBA/* npop */);

  // method MSY reference point calculation
  double  MSYrefs_objective(const double par,
                            const int nReport,
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
                            const ARRAY_2D Recdist/*nareas, npop */,
                            const ARRAY_1I SRrel/* npop */,
                            ARRAY_4D N/* nareas, nsubyears + 1, nages, npop */,
                            ARRAY_4D NBefore/* nareas, nsubyears + 1, nages, npop */,
                            ARRAY_4D SSN/* nareas, nsubyears, nages, npop */,
                            ARRAY_5D C/* nfleets, nareas, nsubyears, nages, npop */,
                            ARRAY_1D SSBA/* npop */,
                            const int ntargets,
                            const ARRAY_1I targpop/* ntargets */,
                            const int run_years);

  void    MSYrefs(const double par,
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
                  const ARRAY_2D Recdist/*nareas, npop */,
                  const ARRAY_1I SRrel/* npop */,
                  ARRAY_4D N/* nareas, nsubyears + 1, nages, npop */,
                  ARRAY_4D NBefore/* nareas, nsubyears + 1, nages, npop */,
                  ARRAY_4D SSN/* nareas, nsubyears, nages, npop */,
                  ARRAY_5D C/* nfleets, nareas, nsubyears, nages, npop */,
                  ARRAY_1D SSBA/* npop */,
                  const int ntargets,
                  const ARRAY_1I targpop/* ntargets */,
                  const int run_years,
                  double& MSY,
                  double& BMSY,
                  double& SSBMSY,
                  double& SSBMSY_B0);

  void    runHistoric(double totF,
                      const ARRAY_1D qy/* nfleets */,
                      const ARRAY_3D ECurrent/* nfleets, nareas, nsubyears */,
                      const ARRAY_1D R0/* npop */,
                      const ARRAY_2D M/* nages, npop */,
                      const ARRAY_2D mat/* nages, npop */,
                      const ARRAY_3D Idist/* nareas, nages, npop */,
                      const ARRAY_2D Len_age/* nages, npop */,
                      const ARRAY_2D Wt_age/* nages, npop */,
                      const ARRAY_2D sel/* nages, nfleets */,
                      const ARRAY_5D mov/* nareas, nareas, nsubyears, nages, npop */,
                      const ARRAY_1D h/* npop */,
                      const ARRAY_2D Recdist/*nareas, npop */,
                      const ARRAY_2D Recdevs/* SpawnPerYr, npop */,
                      const ARRAY_2D RecSpatialDevs/* nareas, npop */,
                      const ARRAY_1I SRrel/* npop */,
                      ARRAY_4D N/* nareas, nsubyears + 1, nages, npop */,
                      ARRAY_4D NBefore/* nareas, nsubyears + 1, nages, npop */,
                      ARRAY_4D SSN/* nareas, nsubyears, nages, npop */,
                      ARRAY_5D C/* nfleets, nareas, nsubyears, nages, npop */,
                      ARRAY_1D SSBA/* npop */);
};


#endif  //__niMseom_HPP__
