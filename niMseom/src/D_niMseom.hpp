//  ----------------------------------------------------------------------------
//  ADT generated header defining class D_OperatingModelBase
//  ----------------------------------------------------------------------------


#ifndef __D_OperatingModelBase_HPP
#define __D_OperatingModelBase_HPP


#include "niMseom.hpp"


class D_OperatingModelBase  : public OperatingModelBase
{
protected: 
  
  ARRAY_3D eforyeard1_par/* nfleets,nareas,nsubyears */;
  ARRAY_1D fmd1_par/* nfleets */;
  ARRAY_1D movnd1_par/* nareas */;
  ARRAY_3D eforyearb2_par/* nfleets,nareas,nsubyears */;
  ARRAY_1D fmb2_par/* nfleets */;
  ARRAY_1D movnb2_par/* nareas */;
  ARRAY_1I i4stack_1_2/* dim_stack */;
  int i4stack_1_2i;
  ARRAY_1D r4stack_1_2/* dim_stack */;
  int r4stack_1_2i;
  ARRAY_1I i4stack_2_2/* dim_stack */;
  int i4stack_2_2i;
  ARRAY_1I i4stack_3_2/* dim_stack */;
  int i4stack_3_2i;
  ARRAY_1D r4stack_3_2/* dim_stack */;
  int r4stack_3_2i;
  ARRAY_1I bstack_3_2/* dim_stack */;
  int bstack_3_2i;
  //   Differentiation of operatingmodelbase__popdyn_msy_par in forward (tangent) mode:
  //    variations   of useful results: c
  //    with respect to varying inputs: par
  //   ----------------------------------------------------------------------------
  void POPDYN_MSY_PAR_DPAR(double par, double pard1_par, const ARRAY_3D ECurrent/* nfleets,nareas,nsubyears */, const ARRAY_1D qy/* nfleets */, const ARRAY_1D R0/* npop */, const ARRAY_2D M/* nages,npop */, const ARRAY_2D mat/* nages,npop */, const ARRAY_3D Idist/* nareas,nages,npop */, const ARRAY_2D Len_age/* nages,npop */, const ARRAY_2D Wt_age/* nages,npop */, const ARRAY_2D sel/* nages,nfleets */, const ARRAY_5D mov/* nareas,nareas,nsubyears,nages,npop */, const ARRAY_1D h/* npop */, const ARRAY_2D Recdist/* nareas,npop */, const ARRAY_1I SRrel/* npop */, ARRAY_4D N/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nd1_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D NBefore/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nbefored1_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D SSN/* nareas,nsubyears,nages,npop */, ARRAY_5D C/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_5D cd1_par/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_1D SSBA/* npop */, int run_years);
  //   Differentiation of operatingmodelbase__nextyear in forward (tangent) mode:
  //    variations   of useful results: nbefore n
  //    with respect to varying inputs: nbefore n
  //   ----------------------------------------------------------------------------
  void NEXTYEAR_DPAR(ARRAY_4D N/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nd1_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D NBefore/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nbefored1_par/* nareas,nsubyears + 1,nages,npop */);
  //   Differentiation of operatingmodelbase__popdyn_year in forward (tangent) mode:
  //    variations   of useful results: movn fm nbefore n c
  //    with respect to varying inputs: movn fm nbefore n eannual c
  //   ----------------------------------------------------------------------------
  void POPDYN_YEAR_DPAR(const ARRAY_1D qy/* nfleets */, const ARRAY_1D R0/* npop */, const ARRAY_2D M/* nages,npop */, const ARRAY_2D mat/* nages,npop */, const ARRAY_2D Len_age/* nages,npop */, const ARRAY_2D Wt_age/* nages,npop */, const ARRAY_2D sel/* nages,nfleets */, const ARRAY_3D Eannual/* nfleets,nareas,nsubyears */, const ARRAY_3D eannuald1_par/* nfleets,nareas,nsubyears */, const ARRAY_5D mov/* nareas,nareas,nsubyears,nages,npop */, const ARRAY_1D h/* npop */, const ARRAY_2D Recdist/* nareas,npop */, const ARRAY_2D Recdevs/* SpawnPerYr,npop */, const ARRAY_2D RecSpatialDevs/* nareas,npop */, const ARRAY_1I SRrel/* npop */, ARRAY_4D N/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nd1_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D NBefore/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nbefored1_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D SSN/* nareas,nsubyears,nages,npop */, ARRAY_5D C/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_5D cd1_par/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_1D SSBA/* npop */, int bIgnoreLast);
  //   Differentiation of operatingmodelbase__popdyn_projection_par in reverse (adjoint) mode:
  //    gradient     of useful results: c
  //    with respect to varying inputs: par
  //   ----------------------------------------------------------------------------
  void POPDYN_PROJECTION_PAR_BPAR(const ARRAY_1D par/* 0:npar - 1 */, ARRAY_1D parb2_par/* 0:npar - 1 */, int npar, int nfixed, const ARRAY_1D TAE/* 0:nfixed - 1 */, const ARRAY_1I FbyPar/* 0:npar - 1 */, const ARRAY_1I FbyFixed/* 0:nfixed - 1 */, const ARRAY_3D ECurrent/* nfleets,nareas,nsubyears */, const ARRAY_1D qy/* nfleets */, const ARRAY_1D R0/* npop */, const ARRAY_2D M/* nages,npop */, const ARRAY_2D mat/* nages,npop */, const ARRAY_3D Idist/* nareas,nages,npop */, const ARRAY_2D Len_age/* nages,npop */, const ARRAY_2D Wt_age/* nages,npop */, const ARRAY_2D sel/* nages,nfleets */, const ARRAY_5D mov/* nareas,nareas,nsubyears,nages,npop */, const ARRAY_1D h/* npop */, const ARRAY_2D Recdist/* nareas,npop */, const ARRAY_2D Recdevs/* SpawnPerYr,npop */, const ARRAY_2D RecSpatialDevs/* nareas,npop */, const ARRAY_1I SRrel/* npop */, ARRAY_4D N/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nb2_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D NBefore/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nbeforeb2_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D SSN/* nareas,nsubyears,nages,npop */, ARRAY_5D C/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_5D cb2_par/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_1D SSBA/* npop */, int bIgnoreLast);
  //   Differentiation of operatingmodelbase__popdyn_year in reverse (adjoint) mode:
  //    gradient     of useful results: c
  //    with respect to varying inputs: eannual
  //   ----------------------------------------------------------------------------
  void POPDYN_YEAR_BPAR(const ARRAY_1D qy/* nfleets */, const ARRAY_1D R0/* npop */, const ARRAY_2D M/* nages,npop */, const ARRAY_2D mat/* nages,npop */, const ARRAY_2D Len_age/* nages,npop */, const ARRAY_2D Wt_age/* nages,npop */, const ARRAY_2D sel/* nages,nfleets */, const ARRAY_3D Eannual/* nfleets,nareas,nsubyears */, ARRAY_3D eannualb2_par/* nfleets,nareas,nsubyears */, const ARRAY_5D mov/* nareas,nareas,nsubyears,nages,npop */, const ARRAY_1D h/* npop */, const ARRAY_2D Recdist/* nareas,npop */, const ARRAY_2D Recdevs/* SpawnPerYr,npop */, const ARRAY_2D RecSpatialDevs/* nareas,npop */, const ARRAY_1I SRrel/* npop */, ARRAY_4D N/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nb2_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D NBefore/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nbeforeb2_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D SSN/* nareas,nsubyears,nages,npop */, ARRAY_5D C/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_5D cb2_par/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_1D SSBA/* npop */, int bIgnoreLast);
public: 
  
  #include "include/OmB_decl_lib_interface_methods.hpp"
  D_OperatingModelBase(int arg_npop, int arg_nages, int arg_nsubyears, int arg_nareas, int arg_nfleets, const ARRAY_1I arg_Recsubyr);
  D_OperatingModelBase(const D_OperatingModelBase& rCopy);
  virtual ~D_OperatingModelBase();
  //   Differentiation of operatingmodelbase__msyrefs_objective in forward (tangent) mode:
  //    variations   of useful results: operatingmodelbase__msyrefs_objective
  //    with respect to varying inputs: par
  //    RW status of diff variables: movn:(loc) fm:(loc) eforyear:(loc)
  //                 nbefore:(loc) n:(loc) operatingmodelbase__msyrefs_objective:out
  //                 par:in c:(loc)
  //   ----------------------------------------------------------------------------
  double MSYREFS_OBJECTIVE_DPAR(double par, double pard1_par, int nReport, const ARRAY_3D ECurrent/* nfleets,nareas,nsubyears */, const ARRAY_1D qy/* nfleets */, const ARRAY_1D R0/* npop */, const ARRAY_2D M/* nages,npop */, const ARRAY_2D mat/* nages,npop */, const ARRAY_3D Idist/* nareas,nages,npop */, const ARRAY_2D Len_age/* nages,npop */, const ARRAY_2D Wt_age/* nages,npop */, const ARRAY_2D sel/* nages,nfleets */, const ARRAY_5D mov/* nareas,nareas,nsubyears,nages,npop */, const ARRAY_1D h/* npop */, const ARRAY_2D Recdist/* nareas,npop */, const ARRAY_1I SRrel/* npop */, ARRAY_4D N/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nd1_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D NBefore/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nbefored1_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D SSN/* nareas,nsubyears,nages,npop */, ARRAY_5D C/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_5D cd1_par/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_1D SSBA/* npop */, int ntargets, const ARRAY_1I targpop/* ntargets */, int run_years, double& MSYrefs_objective);
  //   Differentiation of operatingmodelbase__popdyn_projection_objective in reverse (adjoint) mode:
  //    gradient     of useful results: operatingmodelbase__popdyn_projection_objective
  //    with respect to varying inputs: par
  //    RW status of diff variables: movn:(loc) fm:(loc) eforyear:(loc)
  //                 nbefore:(loc) n:(loc) operatingmodelbase__popdyn_projection_objective:in-killed
  //                 par:out c:(loc)
  //   ----------------------------------------------------------------------------
  void POPDYN_PROJECTION_OBJECTIVE_BPAR(const ARRAY_1D par/* 0:npar - 1 */, ARRAY_1D parb2_par/* 0:npar - 1 */, int npar, int nfixed, const ARRAY_1D TAC/* 0:npar - 1 */, const ARRAY_1D TAE/* 0:nfixed - 1 */, const ARRAY_1I FbyPar/* 0:npar - 1 */, const ARRAY_1I FbyFixed/* 0:nfixed - 1 */, const ARRAY_3D ECurrent/* nfleets,nareas,nsubyears */, const ARRAY_1D qy/* nfleets */, const ARRAY_1D R0/* npop */, const ARRAY_2D M/* nages,npop */, const ARRAY_2D mat/* nages,npop */, const ARRAY_3D Idist/* nareas,nages,npop */, const ARRAY_2D Len_age/* nages,npop */, const ARRAY_2D Wt_age/* nages,npop */, const ARRAY_2D Wt_age_mid/* nages,npop */, const ARRAY_2D sel/* nages,nfleets */, const ARRAY_5D mov/* nareas,nareas,nsubyears,nages,npop */, const ARRAY_1D h/* npop */, const ARRAY_2D Recdist/* nareas,npop */, const ARRAY_2D Recdevs/* SpawnPerYr,npop */, const ARRAY_2D RecSpatialDevs/* nareas,npop */, const ARRAY_1I SRrel/* npop */, ARRAY_4D N/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nb2_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D NBefore/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nbeforeb2_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D SSN/* nareas,nsubyears,nages,npop */, ARRAY_5D C/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_5D cb2_par/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_1D SSBA/* npop */, double& popdyn_projection_objectiveb2_par);
  
};


#include "include/OmB_decl_lib_interface_globals.hpp"
#include "include/OmB_decl_lib_interface_constructor.hpp"


#endif //__D_OperatingModelBase_HPP

