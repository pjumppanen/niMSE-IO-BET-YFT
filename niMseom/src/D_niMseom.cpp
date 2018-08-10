//  ----------------------------------------------------------------------------
//  ADT generated file implementing class D_OperatingModelBase
//  ----------------------------------------------------------------------------

#include "niMseom.hpp"



#include "D_niMseom.hpp"


D_OperatingModelBase::D_OperatingModelBase(int arg_npop, int arg_nages, int arg_nsubyears, int arg_nareas, int arg_nfleets, const ARRAY_1I arg_Recsubyr)
 : OperatingModelBase(arg_npop,arg_nages,arg_nsubyears,arg_nareas,arg_nfleets,arg_Recsubyr)
{
  create(eforyeard1_par,EforYear);
  create(fmd1_par,FM);
  create(movnd1_par,MovN);
  create(eforyearb2_par,EforYear);
  create(fmb2_par,FM);
  create(movnb2_par,MovN);
  createStack(i4stack_1_2);
  i4stack_1_2i = 0;
  createStack(r4stack_1_2);
  r4stack_1_2i = 0;
  createStack(i4stack_2_2);
  i4stack_2_2i = 0;
  createStack(i4stack_3_2);
  i4stack_3_2i = 0;
  createStack(r4stack_3_2);
  r4stack_3_2i = 0;
  createStack(bstack_3_2);
  bstack_3_2i = 0;
}



D_OperatingModelBase::D_OperatingModelBase(const D_OperatingModelBase& rCopy)
 : OperatingModelBase(rCopy)
{
  create(eforyeard1_par,EforYear);
  create(fmd1_par,FM);
  create(movnd1_par,MovN);
  create(eforyearb2_par,EforYear);
  create(fmb2_par,FM);
  create(movnb2_par,MovN);
  createStack(i4stack_1_2);
  i4stack_1_2i = 0;
  createStack(r4stack_1_2);
  r4stack_1_2i = 0;
  createStack(i4stack_2_2);
  i4stack_2_2i = 0;
  createStack(i4stack_3_2);
  i4stack_3_2i = 0;
  createStack(r4stack_3_2);
  r4stack_3_2i = 0;
  createStack(bstack_3_2);
  bstack_3_2i = 0;
}



D_OperatingModelBase::~D_OperatingModelBase()
{
  destroy(eforyeard1_par);
  destroy(fmd1_par);
  destroy(movnd1_par);
  destroy(eforyearb2_par);
  destroy(fmb2_par);
  destroy(movnb2_par);
  destroy(i4stack_1_2);
  destroy(r4stack_1_2);
  destroy(i4stack_2_2);
  destroy(i4stack_3_2);
  destroy(r4stack_3_2);
  destroy(bstack_3_2);
}


//   Differentiation of operatingmodelbase__msyrefs_objective in forward (tangent) mode:
//    variations   of useful results: operatingmodelbase__msyrefs_objective
//    with respect to varying inputs: par
//    RW status of diff variables: movn:(loc) fm:(loc) eforyear:(loc)
//                 nbefore:(loc) n:(loc) operatingmodelbase__msyrefs_objective:out
//                 par:in c:(loc)
//   ----------------------------------------------------------------------------

double D_OperatingModelBase::MSYREFS_OBJECTIVE_DPAR(double par, double pard1_par, int nReport, const ARRAY_3D ECurrent/* nfleets,nareas,nsubyears */, const ARRAY_1D qy/* nfleets */, const ARRAY_1D R0/* npop */, const ARRAY_2D M/* nages,npop */, const ARRAY_2D mat/* nages,npop */, const ARRAY_3D Idist/* nareas,nages,npop */, const ARRAY_2D Len_age/* nages,npop */, const ARRAY_2D Wt_age/* nages,npop */, const ARRAY_2D sel/* nages,nfleets */, const ARRAY_5D mov/* nareas,nareas,nsubyears,nages,npop */, const ARRAY_1D h/* npop */, const ARRAY_2D Recdist/* nareas,npop */, const ARRAY_1I SRrel/* npop */, ARRAY_4D N/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nd1_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D NBefore/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nbefored1_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D SSN/* nareas,nsubyears,nages,npop */, ARRAY_5D C/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_5D cd1_par/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_1D SSBA/* npop */, int ntargets, const ARRAY_1I targpop/* ntargets */, int run_years, double& MSYrefs_objective)
{
  int ca;
  int cf;
  int cm;
  int cp;
  int cr;
  double dCatch;
  double dcatchd1_par;
  double dObjective;
  double dobjectived1_par;
  int p;
  double ret_MSYREFS_OBJECTIVE_DPAR;
  POPDYN_MSY_PAR_DPAR(par,pard1_par,ECurrent,qy,R0,M,mat,Idist,Len_age,Wt_age,sel,mov,h,Recdist,SRrel,N,nd1_par,NBefore,nbefored1_par,SSN,C,cd1_par,SSBA,run_years);
  //   return(-log(sum(
  //   array(C[targpop,,nyears,,,],c(length(targpop),nages,nsubyears,nareas,nfleets))*
  //   array(Wt_age[targpop,,nyears],c(length(targpop),nages,nsubyears,nareas,nfleets)))))
  //   Use a small number to stop the function returning +inf for objective
  dCatch = 1.0e-6;
  cp = 1;
  dcatchd1_par = 0.0;
  
  while ((cp <= ntargets))
  {
    p = targpop[cp];
    cr = 1;
    
    while ((cr <= nareas))
    {
      ca = 1;
      
      while ((ca <= nages))
      {
        cf = 1;
        
        while ((cf <= nfleets))
        {
          cm = 1;
          
          while ((cm <= nsubyears))
          {
            dcatchd1_par = dcatchd1_par + Wt_age[ca][p] * cd1_par[cf][cr][cm][ca][p];
            dCatch = dCatch + C[cf][cr][cm][ca][p] * Wt_age[ca][p];
            cm = cm + 1;
          }
          
          cf = cf + 1;
        }
        
        ca = ca + 1;
      }
      
      cr = cr + 1;
    }
    
    cp = cp + 1;
  }
  
  dobjectived1_par = -(dcatchd1_par / dCatch);
  dObjective = -log(dCatch);
  ret_MSYREFS_OBJECTIVE_DPAR = dobjectived1_par;
  MSYrefs_objective = dObjective;
  return (ret_MSYREFS_OBJECTIVE_DPAR);
}


//   Differentiation of operatingmodelbase__popdyn_msy_par in forward (tangent) mode:
//    variations   of useful results: c
//    with respect to varying inputs: par
//   ----------------------------------------------------------------------------

void D_OperatingModelBase::POPDYN_MSY_PAR_DPAR(double par, double pard1_par, const ARRAY_3D ECurrent/* nfleets,nareas,nsubyears */, const ARRAY_1D qy/* nfleets */, const ARRAY_1D R0/* npop */, const ARRAY_2D M/* nages,npop */, const ARRAY_2D mat/* nages,npop */, const ARRAY_3D Idist/* nareas,nages,npop */, const ARRAY_2D Len_age/* nages,npop */, const ARRAY_2D Wt_age/* nages,npop */, const ARRAY_2D sel/* nages,nfleets */, const ARRAY_5D mov/* nareas,nareas,nsubyears,nages,npop */, const ARRAY_1D h/* npop */, const ARRAY_2D Recdist/* nareas,npop */, const ARRAY_1I SRrel/* npop */, ARRAY_4D N/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nd1_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D NBefore/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nbefored1_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D SSN/* nareas,nsubyears,nages,npop */, ARRAY_5D C/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_5D cd1_par/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_1D SSBA/* npop */, int run_years)
{
  int cf;
  int cr;
  int cs;
  int cy;
  int ix_0___;
  int ix_1___;
  int ix_2___;
  int ix_3___;
  int ix_4___;
  double totF;
  double totfd1_par;
  totfd1_par = pard1_par * exp(par);
  totF = exp(par);
  popdyn_init(R0,mat,Idist,N,NBefore,SSN);
  cf = 1;
  
  for (ix_2___ = 1;ix_2___ <= nsubyears; ++ix_2___)
  {
    for (ix_1___ = 1;ix_1___ <= nareas; ++ix_1___)
    {
      for (ix_0___ = 1;ix_0___ <= nfleets; ++ix_0___)
      {
        eforyeard1_par[ix_0___][ix_1___][ix_2___] = 0.0;
      }
    }
  }
  
  while ((cf <= nfleets))
  {
    cr = 1;
    
    while ((cr <= nareas))
    {
      cs = 1;
      
      while ((cs <= nsubyears))
      {
        eforyeard1_par[cf][cr][cs] = ECurrent[cf][cr][cs] * totfd1_par;
        EforYear[cf][cr][cs] = totF * ECurrent[cf][cr][cs];
        cs = cs + 1;
      }
      
      cr = cr + 1;
    }
    
    cf = cf + 1;
  }
  
  cy = 1;
  
  for (ix_0___ = 1;ix_0___ <= nareas; ++ix_0___)
  {
    movnd1_par[ix_0___] = 0.0;
  }
  
  for (ix_0___ = 1;ix_0___ <= nfleets; ++ix_0___)
  {
    fmd1_par[ix_0___] = 0.0;
  }
  
  for (ix_3___ = 1;ix_3___ <= npop; ++ix_3___)
  {
    for (ix_2___ = 1;ix_2___ <= nages; ++ix_2___)
    {
      for (ix_1___ = 1;ix_1___ <= nsubyears + 1; ++ix_1___)
      {
        for (ix_0___ = 1;ix_0___ <= nareas; ++ix_0___)
        {
          nbefored1_par[ix_0___][ix_1___][ix_2___][ix_3___] = 0.0;
        }
      }
    }
  }
  
  for (ix_3___ = 1;ix_3___ <= npop; ++ix_3___)
  {
    for (ix_2___ = 1;ix_2___ <= nages; ++ix_2___)
    {
      for (ix_1___ = 1;ix_1___ <= nsubyears + 1; ++ix_1___)
      {
        for (ix_0___ = 1;ix_0___ <= nareas; ++ix_0___)
        {
          nd1_par[ix_0___][ix_1___][ix_2___][ix_3___] = 0.0;
        }
      }
    }
  }
  
  for (ix_4___ = 1;ix_4___ <= npop; ++ix_4___)
  {
    for (ix_3___ = 1;ix_3___ <= nages; ++ix_3___)
    {
      for (ix_2___ = 1;ix_2___ <= nsubyears; ++ix_2___)
      {
        for (ix_1___ = 1;ix_1___ <= nareas; ++ix_1___)
        {
          for (ix_0___ = 1;ix_0___ <= nfleets; ++ix_0___)
          {
            cd1_par[ix_0___][ix_1___][ix_2___][ix_3___][ix_4___] = 0.0;
          }
        }
      }
    }
  }
  
  while ((cy < run_years))
  {
    POPDYN_YEAR_DPAR(qy,R0,M,mat,Len_age,Wt_age,sel,EforYear,eforyeard1_par,mov,h,Recdist,MSY_Recdevs,MSY_RecSpatialDevs,SRrel,N,nd1_par,NBefore,nbefored1_par,SSN,C,cd1_par,SSBA,0);
    NEXTYEAR_DPAR(N,nd1_par,NBefore,nbefored1_par);
    cy = cy + 1;
  }
  
  POPDYN_YEAR_DPAR(qy,R0,M,mat,Len_age,Wt_age,sel,EforYear,eforyeard1_par,mov,h,Recdist,MSY_Recdevs,MSY_RecSpatialDevs,SRrel,N,nd1_par,NBefore,nbefored1_par,SSN,C,cd1_par,SSBA,0);
}


//   Differentiation of operatingmodelbase__nextyear in forward (tangent) mode:
//    variations   of useful results: nbefore n
//    with respect to varying inputs: nbefore n
//   ----------------------------------------------------------------------------

void D_OperatingModelBase::NEXTYEAR_DPAR(ARRAY_4D N/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nd1_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D NBefore/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nbefored1_par/* nareas,nsubyears + 1,nages,npop */)
{
  int ca;
  int cp;
  int cr;
  cr = 1;
  
  while ((cr <= nareas))
  {
    ca = 1;
    
    while ((ca <= nages))
    {
      cp = 1;
      
      while ((cp <= npop))
      {
        nd1_par[cr][1][ca][cp] = nd1_par[cr][nsubyears + 1][ca][cp];
        N[cr][1][ca][cp] = N[cr][nsubyears + 1][ca][cp];
        nbefored1_par[cr][1][ca][cp] = nbefored1_par[cr][nsubyears + 1][ca][cp];
        NBefore[cr][1][ca][cp] = NBefore[cr][nsubyears + 1][ca][cp];
        cp = cp + 1;
      }
      
      ca = ca + 1;
    }
    
    cr = cr + 1;
  }
}


//   Differentiation of operatingmodelbase__popdyn_year in forward (tangent) mode:
//    variations   of useful results: movn fm nbefore n c
//    with respect to varying inputs: movn fm nbefore n eannual c
//   ----------------------------------------------------------------------------

void D_OperatingModelBase::POPDYN_YEAR_DPAR(const ARRAY_1D qy/* nfleets */, const ARRAY_1D R0/* npop */, const ARRAY_2D M/* nages,npop */, const ARRAY_2D mat/* nages,npop */, const ARRAY_2D Len_age/* nages,npop */, const ARRAY_2D Wt_age/* nages,npop */, const ARRAY_2D sel/* nages,nfleets */, const ARRAY_3D Eannual/* nfleets,nareas,nsubyears */, const ARRAY_3D eannuald1_par/* nfleets,nareas,nsubyears */, const ARRAY_5D mov/* nareas,nareas,nsubyears,nages,npop */, const ARRAY_1D h/* npop */, const ARRAY_2D Recdist/* nareas,npop */, const ARRAY_2D Recdevs/* SpawnPerYr,npop */, const ARRAY_2D RecSpatialDevs/* nareas,npop */, const ARRAY_1I SRrel/* npop */, ARRAY_4D N/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nd1_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D NBefore/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nbefored1_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D SSN/* nareas,nsubyears,nages,npop */, ARRAY_5D C/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_5D cd1_par/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_1D SSBA/* npop */, int bIgnoreLast)
{
  int ca;
  int cf;
  int cp;
  int cr;
  int cr2;
  int cs;
  double dFM;
  double dfmd1_par;
  double dFtot;
  double dftotd1_par;
  double dN;
  double dnd1_par;
  double dPlusGroup;
  double dplusgroupd1_par;
  double dRecruitment;
  double drecruitmentd1_par;
  double dSSB;
  double dSSB_area;
  double dssb_aread1_par;
  double dssbd1_par;
  double dSSN;
  double dssnd1_par;
  double dZ;
  double dzd1_par;
  int nRecdevIdx;
  nRecdevIdx = 1;
  cs = 1;
  
  while ((cs <= nsubyears))
  {
    cp = 1;
    
    while ((cp <= npop))
    //   Update SSB and SSN and do recruitment.
    {
      dSSB = 0.0;
      cr = 1;
      dssbd1_par = 0.0;
      
      while ((cr <= nareas))
      {
        dSSB_area = 0.0;
        ca = 1;
        dssb_aread1_par = 0.0;
        
        while ((ca <= nages))
        {
          dssnd1_par = mat[ca][cp] * nbefored1_par[cr][cs][ca][cp];
          dSSN = NBefore[cr][cs][ca][cp] * mat[ca][cp];
          SSN[cr][cs][ca][cp] = dSSN;
          dssb_aread1_par = dssb_aread1_par + Wt_age[ca][cp] * dssnd1_par;
          dSSB_area = dSSB_area + dSSN * Wt_age[ca][cp];
          ca = ca + 1;
        }
        
        dssbd1_par = dssbd1_par + dssb_aread1_par;
        dSSB = dSSB + dSSB_area;
        cr = cr + 1;
      }
      
      SSBA[cp] = dSSB;
      
      //   Run recruitment
      if ((Recsubyr[cs] != 0))
      {
        if ((SRrel[cp] == 1))
        {
          drecruitmentd1_par = Recdevs[nRecdevIdx][cp] * (0.8 * R0[cp] * h[cp] * dssbd1_par * (0.2 * SSBpR[cp] * R0[cp] * (1.0 - h[cp]) + (h[cp] - 0.2) * dSSB) - 0.8 * R0[cp] * h[cp] * dSSB * (h[cp] - 0.2) * dssbd1_par) / pow((0.2 * SSBpR[cp] * R0[cp] * (1.0 - h[cp]) + (h[cp] - 0.2) * dSSB),2);
          dRecruitment = Recdevs[nRecdevIdx][cp] * (0.8 * R0[cp] * h[cp] * dSSB / (0.2 * SSBpR[cp] * R0[cp] * (1.0 - h[cp]) + (h[cp] - 0.2) * dSSB));
        }
        else
        {
          drecruitmentd1_par = Recdevs[nRecdevIdx][cp] * aR[cp] * (dssbd1_par * exp(-(bR[cp] * dSSB)) - dSSB * bR[cp] * dssbd1_par * exp(-(bR[cp] * dSSB)));
          dRecruitment = Recdevs[nRecdevIdx][cp] * aR[cp] * dSSB * exp(-(bR[cp] * dSSB));
        }
        
        cr = 1;
        
        while ((cr <= nareas))
        {
          nbefored1_par[cr][cs][1][cp] = RecSpatialDevs[cr][cp] * Recdist[cr][cp] * drecruitmentd1_par;
          NBefore[cr][cs][1][cp] = dRecruitment * RecSpatialDevs[cr][cp] * Recdist[cr][cp];
          nd1_par[cr][cs][1][cp] = nbefored1_par[cr][cs][1][cp];
          N[cr][cs][1][cp] = NBefore[cr][cs][1][cp];
          cr = cr + 1;
        }
      }
      
      //  rec<-OM@Recdevs[,pp,RecdevInd]*((0.8*OM@R0[,pp]*OM@h[,pp]*SSBA[,pp,y])/(0.2*SSBpR[,pp]*OM@R0[,pp]*(1-OM@h[,pp])+(OM@h[,pp]-0.2)
      // *SSBA[,pp,y]))
      //   Beverton Holt stock recruitment relationship
      //   Most transparent form of the Ricker uses alpha and beta params
      //   Move Fish
      ca = 1;
      
      while ((ca <= nages))
      {
        cr = 1;
        
        while ((cr <= nareas))
        //   N[,,y,m,]<-domov2(N[,,y,m,],mov[,,m,,])
        {
          dN = 0.0;
          cr2 = 1;
          dnd1_par = 0.0;
          
          while ((cr2 <= nareas))
          {
            dnd1_par = dnd1_par + mov[cr][cr2][cs][ca][cp] * nd1_par[cr2][cs][ca][cp];
            dN = dN + N[cr2][cs][ca][cp] * mov[cr][cr2][cs][ca][cp];
            cr2 = cr2 + 1;
          }
          
          movnd1_par[cr] = dnd1_par;
          MovN[cr] = dN;
          cr = cr + 1;
        }
        
        cr = 1;
        
        while ((cr <= nareas))
        {
          nd1_par[cr][cs][ca][cp] = movnd1_par[cr];
          N[cr][cs][ca][cp] = MovN[cr];
          cr = cr + 1;
        }
        
        ca = ca + 1;
      }
      
      //   Fishing and natural mortality
      ca = 1;
      
      while ((ca <= nages))
      {
        cr = 1;
        
        while ((cr <= nareas))
        {
          dFtot = 0.0;
          cf = 1;
          dftotd1_par = 0.0;
          
          while ((cf <= nfleets))
          //  FM[PAYMRF2] <- totF*ECurrent[MRF2]*sel[FA2]
          {
            dfmd1_par = sel[ca][cf] * qy[cf] * eannuald1_par[cf][cr][cs];
            dFM = Eannual[cf][cr][cs] * sel[ca][cf] * qy[cf];
            fmd1_par[cf] = dfmd1_par;
            FM[cf] = dFM;
            dftotd1_par = dftotd1_par + dfmd1_par;
            dFtot = dFtot + dFM;
            cf = cf + 1;
          }
          
          //  Ftot <- apply(FM[,,y,m,,,drop=F],c(1,2,5),sum)
          //  Z[PAYMR] <- Ftot[PAR]+M[PAY]/nsubyears
          dzd1_par = dftotd1_par;
          dZ = dFtot + M[ca][cp] / nsubyears;
          cf = 1;
          
          while ((cf <= nfleets))
          //  C[PAYMRF2] <- N[PAYMR2]*(1-exp(-Z[PAYMR2]))*(FM[PAYMRF2]/Z[PAYMR2])
          {
            cd1_par[cf][cr][cs][ca][cp] = (nd1_par[cr][cs][ca][cp] * FM[cf] / dZ + N[cr][cs][ca][cp] * (fmd1_par[cf] * dZ - FM[cf] * dzd1_par) / pow(dZ,2)) * (1 - exp(-dZ)) + N[cr][cs][ca][cp] * FM[cf] * dzd1_par * exp(-dZ) / dZ;
            C[cf][cr][cs][ca][cp] = N[cr][cs][ca][cp] * (1 - exp(-dZ)) * (FM[cf] / dZ);
            cf = cf + 1;
          }
          
          //  N[,,y,m,]<-N[,,y,m,]*exp(-Z[,,y,m,])
          nd1_par[cr][cs][ca][cp] = nd1_par[cr][cs][ca][cp] * exp(-dZ) - N[cr][cs][ca][cp] * dzd1_par * exp(-dZ);
          N[cr][cs][ca][cp] = N[cr][cs][ca][cp] * exp(-dZ);
          cr = cr + 1;
        }
        
        ca = ca + 1;
      }
      
      if ((cs != nsubyears) || (bIgnoreLast == 0))
      {
        cr = 1;
        
        while ((cr <= nareas))
        {
          dplusgroupd1_par = nd1_par[cr][cs][nages][cp];
          dPlusGroup = N[cr][cs][nages][cp];
          nbefored1_par[cr][cs + 1][1][cp] = 0.0;
          NBefore[cr][cs + 1][1][cp] = 0.0;
          nd1_par[cr][cs + 1][1][cp] = 0.0;
          N[cr][cs + 1][1][cp] = 0.0;
          ca = nages - 1;
          
          while ((ca >= 1))
          {
            nbefored1_par[cr][cs + 1][ca + 1][cp] = nd1_par[cr][cs][ca][cp];
            NBefore[cr][cs + 1][ca + 1][cp] = N[cr][cs][ca][cp];
            nd1_par[cr][cs + 1][ca + 1][cp] = nbefored1_par[cr][cs + 1][ca + 1][cp];
            N[cr][cs + 1][ca + 1][cp] = NBefore[cr][cs + 1][ca + 1][cp];
            ca = ca - 1;
          }
          
          nbefored1_par[cr][cs + 1][nages][cp] = nbefored1_par[cr][cs + 1][nages][cp] + dplusgroupd1_par;
          NBefore[cr][cs + 1][nages][cp] = NBefore[cr][cs + 1][nages][cp] + dPlusGroup;
          nd1_par[cr][cs + 1][nages][cp] = nd1_par[cr][cs + 1][nages][cp] + dplusgroupd1_par;
          N[cr][cs + 1][nages][cp] = N[cr][cs + 1][nages][cp] + dPlusGroup;
          cr = cr + 1;
        }
      }
      
      //   Age fish.
      //  N[,pp,nages-1,y,mm,]          <- N[,pp,nages-1,y,mm,] + N[,pp,nages,y,mm,]
      //  NBefore[,pp,2:nages,y,mm+1,]  <- N[,pp,1:(nages-1),y,mm,]
      //  NBefore[,pp,1,y,mm+1,]        <- rec*recSpatialDevs[,pp,]
      //  N[,pp,,y,mm+1,]               <- NBefore[,pp,,y,mm+1,]
      cp = cp + 1;
    }
    
    if ((Recsubyr[cs] != 0))
    {
      nRecdevIdx = nRecdevIdx + 1;
    }
    
    cs = cs + 1;
  }
}


//   Differentiation of operatingmodelbase__popdyn_projection_objective in reverse (adjoint) mode:
//    gradient     of useful results: operatingmodelbase__popdyn_projection_objective
//    with respect to varying inputs: par
//    RW status of diff variables: movn:(loc) fm:(loc) eforyear:(loc)
//                 nbefore:(loc) n:(loc) operatingmodelbase__popdyn_projection_objective:in-killed
//                 par:out c:(loc)
//   ----------------------------------------------------------------------------

void D_OperatingModelBase::POPDYN_PROJECTION_OBJECTIVE_BPAR(const ARRAY_1D par/* 0:npar - 1 */, ARRAY_1D parb2_par/* 0:npar - 1 */, int npar, int nfixed, const ARRAY_1D TAC/* 0:npar - 1 */, const ARRAY_1D TAE/* 0:nfixed - 1 */, const ARRAY_1I FbyPar/* 0:npar - 1 */, const ARRAY_1I FbyFixed/* 0:nfixed - 1 */, const ARRAY_3D ECurrent/* nfleets,nareas,nsubyears */, const ARRAY_1D qy/* nfleets */, const ARRAY_1D R0/* npop */, const ARRAY_2D M/* nages,npop */, const ARRAY_2D mat/* nages,npop */, const ARRAY_3D Idist/* nareas,nages,npop */, const ARRAY_2D Len_age/* nages,npop */, const ARRAY_2D Wt_age/* nages,npop */, const ARRAY_2D Wt_age_mid/* nages,npop */, const ARRAY_2D sel/* nages,nfleets */, const ARRAY_5D mov/* nareas,nareas,nsubyears,nages,npop */, const ARRAY_1D h/* npop */, const ARRAY_2D Recdist/* nareas,npop */, const ARRAY_2D Recdevs/* SpawnPerYr,npop */, const ARRAY_2D RecSpatialDevs/* nareas,npop */, const ARRAY_1I SRrel/* npop */, ARRAY_4D N/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nb2_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D NBefore/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nbeforeb2_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D SSN/* nareas,nsubyears,nages,npop */, ARRAY_5D C/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_5D cb2_par/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_1D SSBA/* npop */, double& popdyn_projection_objectiveb2_par)
{
  int ad_count;
  int ad_count0;
  int ad_count1;
  int ad_count2;
  int ad_count3;
  int ca;
  int cf;
  int cp;
  int cr;
  int cs;
  int cx;
  double dCatchBiomass;
  double dcatchbiomassb2_par;
  double dobjectiveb2_par;
  double dTACError;
  double dtacerrorb2_par;
  int i;
  int i0;
  int i1;
  int i2;
  int i3;
  int ix_0___;
  int ix_1___;
  int ix_2___;
  int ix_3___;
  int ix_4___;
  double temp;
  cf = 0;
  dTACError = 0;
  dCatchBiomass = 0;
  popdyn_projection_par(par,npar,nfixed,TAE,FbyPar,FbyFixed,ECurrent,qy,R0,M,mat,Idist,Len_age,Wt_age,sel,mov,h,Recdist,Recdevs,RecSpatialDevs,SRrel,N,NBefore,SSN,C,SSBA,1);
  cx = 0;
  ad_count3 = 0;
  
  while ((cx < npar))
  {
    r4stack_1_2i = r4stack_1_2i + 1;
    
    if (r4stack_1_2i > stackSizeInt(r4stack_1_2))
    {
      growStack(r4stack_1_2,r4stack_1_2i);
    }
    
    r4stack_1_2[r4stack_1_2i] = dCatchBiomass;
    dCatchBiomass = 1.0e-6;
    i4stack_1_2i = i4stack_1_2i + 1;
    
    if (i4stack_1_2i > stackSizeInt(i4stack_1_2))
    {
      growStack(i4stack_1_2,i4stack_1_2i);
    }
    
    i4stack_1_2[i4stack_1_2i] = cf;
    cf = FbyPar[cx];
    cr = 1;
    ad_count2 = 0;
    
    while ((cr <= nareas))
    {
      ca = 1;
      ad_count1 = 0;
      
      while ((ca <= nages))
      {
        cp = 1;
        ad_count0 = 0;
        
        while ((cp <= npop))
        {
          cs = 1;
          ad_count = 0;
          
          while ((cs <= nsubyears))
          {
            dCatchBiomass = dCatchBiomass + C[cf][cr][cs][ca][cp] * Wt_age_mid[ca][cp];
            i4stack_1_2i = i4stack_1_2i + 1;
            
            if (i4stack_1_2i > stackSizeInt(i4stack_1_2))
            {
              growStack(i4stack_1_2,i4stack_1_2i);
            }
            
            i4stack_1_2[i4stack_1_2i] = cs;
            cs = cs + 1;
            ad_count = ad_count + 1;
          }
          
          i4stack_1_2i = i4stack_1_2i + 1;
          
          if (i4stack_1_2i > stackSizeInt(i4stack_1_2))
          {
            growStack(i4stack_1_2,i4stack_1_2i);
          }
          
          i4stack_1_2[i4stack_1_2i] = ad_count;
          i4stack_1_2i = i4stack_1_2i + 1;
          
          if (i4stack_1_2i > stackSizeInt(i4stack_1_2))
          {
            growStack(i4stack_1_2,i4stack_1_2i);
          }
          
          i4stack_1_2[i4stack_1_2i] = cp;
          cp = cp + 1;
          ad_count0 = ad_count0 + 1;
        }
        
        i4stack_1_2i = i4stack_1_2i + 1;
        
        if (i4stack_1_2i > stackSizeInt(i4stack_1_2))
        {
          growStack(i4stack_1_2,i4stack_1_2i);
        }
        
        i4stack_1_2[i4stack_1_2i] = ad_count0;
        i4stack_1_2i = i4stack_1_2i + 1;
        
        if (i4stack_1_2i > stackSizeInt(i4stack_1_2))
        {
          growStack(i4stack_1_2,i4stack_1_2i);
        }
        
        i4stack_1_2[i4stack_1_2i] = ca;
        ca = ca + 1;
        ad_count1 = ad_count1 + 1;
      }
      
      i4stack_1_2i = i4stack_1_2i + 1;
      
      if (i4stack_1_2i > stackSizeInt(i4stack_1_2))
      {
        growStack(i4stack_1_2,i4stack_1_2i);
      }
      
      i4stack_1_2[i4stack_1_2i] = ad_count1;
      i4stack_1_2i = i4stack_1_2i + 1;
      
      if (i4stack_1_2i > stackSizeInt(i4stack_1_2))
      {
        growStack(i4stack_1_2,i4stack_1_2i);
      }
      
      i4stack_1_2[i4stack_1_2i] = cr;
      cr = cr + 1;
      ad_count2 = ad_count2 + 1;
    }
    
    i4stack_1_2i = i4stack_1_2i + 1;
    
    if (i4stack_1_2i > stackSizeInt(i4stack_1_2))
    {
      growStack(i4stack_1_2,i4stack_1_2i);
    }
    
    i4stack_1_2[i4stack_1_2i] = ad_count2;
    r4stack_1_2i = r4stack_1_2i + 1;
    
    if (r4stack_1_2i > stackSizeInt(r4stack_1_2))
    {
      growStack(r4stack_1_2,r4stack_1_2i);
    }
    
    r4stack_1_2[r4stack_1_2i] = dTACError;
    dTACError = log(TAC[cx] / dCatchBiomass);
    i4stack_1_2i = i4stack_1_2i + 1;
    
    if (i4stack_1_2i > stackSizeInt(i4stack_1_2))
    {
      growStack(i4stack_1_2,i4stack_1_2i);
    }
    
    i4stack_1_2[i4stack_1_2i] = cx;
    cx = cx + 1;
    ad_count3 = ad_count3 + 1;
  }
  
  i4stack_1_2i = i4stack_1_2i + 1;
  
  if (i4stack_1_2i > stackSizeInt(i4stack_1_2))
  {
    growStack(i4stack_1_2,i4stack_1_2i);
  }
  
  i4stack_1_2[i4stack_1_2i] = ad_count3;
  dobjectiveb2_par = popdyn_projection_objectiveb2_par;
  
  for (ix_4___ = 1;ix_4___ <= npop; ++ix_4___)
  {
    for (ix_3___ = 1;ix_3___ <= nages; ++ix_3___)
    {
      for (ix_2___ = 1;ix_2___ <= nsubyears; ++ix_2___)
      {
        for (ix_1___ = 1;ix_1___ <= nareas; ++ix_1___)
        {
          for (ix_0___ = 1;ix_0___ <= nfleets; ++ix_0___)
          {
            cb2_par[ix_0___][ix_1___][ix_2___][ix_3___][ix_4___] = 0.0;
          }
        }
      }
    }
  }
  
  ad_count3 = i4stack_1_2[i4stack_1_2i];
  i4stack_1_2i = i4stack_1_2i - 1;
  
  for (i3 = 1;i3 <= ad_count3; ++i3)
  {
    cx = i4stack_1_2[i4stack_1_2i];
    i4stack_1_2i = i4stack_1_2i - 1;
    dtacerrorb2_par = 2 * dTACError * dobjectiveb2_par;
    dTACError = r4stack_1_2[r4stack_1_2i];
    r4stack_1_2i = r4stack_1_2i - 1;
    temp = TAC[cx] / dCatchBiomass;
    dcatchbiomassb2_par = -(dtacerrorb2_par / dCatchBiomass);
    ad_count2 = i4stack_1_2[i4stack_1_2i];
    i4stack_1_2i = i4stack_1_2i - 1;
    
    for (i2 = 1;i2 <= ad_count2; ++i2)
    {
      cr = i4stack_1_2[i4stack_1_2i];
      i4stack_1_2i = i4stack_1_2i - 1;
      ad_count1 = i4stack_1_2[i4stack_1_2i];
      i4stack_1_2i = i4stack_1_2i - 1;
      
      for (i1 = 1;i1 <= ad_count1; ++i1)
      {
        ca = i4stack_1_2[i4stack_1_2i];
        i4stack_1_2i = i4stack_1_2i - 1;
        ad_count0 = i4stack_1_2[i4stack_1_2i];
        i4stack_1_2i = i4stack_1_2i - 1;
        
        for (i0 = 1;i0 <= ad_count0; ++i0)
        {
          cp = i4stack_1_2[i4stack_1_2i];
          i4stack_1_2i = i4stack_1_2i - 1;
          ad_count = i4stack_1_2[i4stack_1_2i];
          i4stack_1_2i = i4stack_1_2i - 1;
          
          for (i = 1;i <= ad_count; ++i)
          {
            cs = i4stack_1_2[i4stack_1_2i];
            i4stack_1_2i = i4stack_1_2i - 1;
            cb2_par[cf][cr][cs][ca][cp] = cb2_par[cf][cr][cs][ca][cp] + Wt_age_mid[ca][cp] * dcatchbiomassb2_par;
          }
        }
      }
    }
    
    cf = i4stack_1_2[i4stack_1_2i];
    i4stack_1_2i = i4stack_1_2i - 1;
    dCatchBiomass = r4stack_1_2[r4stack_1_2i];
    r4stack_1_2i = r4stack_1_2i - 1;
  }
  
  POPDYN_PROJECTION_PAR_BPAR(par,parb2_par,npar,nfixed,TAE,FbyPar,FbyFixed,ECurrent,qy,R0,M,mat,Idist,Len_age,Wt_age,sel,mov,h,Recdist,Recdevs,RecSpatialDevs,SRrel,N,nb2_par,NBefore,nbeforeb2_par,SSN,C,cb2_par,SSBA,1);
}


//   Differentiation of operatingmodelbase__popdyn_projection_par in reverse (adjoint) mode:
//    gradient     of useful results: c
//    with respect to varying inputs: par
//   ----------------------------------------------------------------------------

void D_OperatingModelBase::POPDYN_PROJECTION_PAR_BPAR(const ARRAY_1D par/* 0:npar - 1 */, ARRAY_1D parb2_par/* 0:npar - 1 */, int npar, int nfixed, const ARRAY_1D TAE/* 0:nfixed - 1 */, const ARRAY_1I FbyPar/* 0:npar - 1 */, const ARRAY_1I FbyFixed/* 0:nfixed - 1 */, const ARRAY_3D ECurrent/* nfleets,nareas,nsubyears */, const ARRAY_1D qy/* nfleets */, const ARRAY_1D R0/* npop */, const ARRAY_2D M/* nages,npop */, const ARRAY_2D mat/* nages,npop */, const ARRAY_3D Idist/* nareas,nages,npop */, const ARRAY_2D Len_age/* nages,npop */, const ARRAY_2D Wt_age/* nages,npop */, const ARRAY_2D sel/* nages,nfleets */, const ARRAY_5D mov/* nareas,nareas,nsubyears,nages,npop */, const ARRAY_1D h/* npop */, const ARRAY_2D Recdist/* nareas,npop */, const ARRAY_2D Recdevs/* SpawnPerYr,npop */, const ARRAY_2D RecSpatialDevs/* nareas,npop */, const ARRAY_1I SRrel/* npop */, ARRAY_4D N/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nb2_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D NBefore/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nbeforeb2_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D SSN/* nareas,nsubyears,nages,npop */, ARRAY_5D C/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_5D cb2_par/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_1D SSBA/* npop */, int bIgnoreLast)
{
  int ad_count;
  int ad_count0;
  int ad_count1;
  int ad_count2;
  int ad_count3;
  int cf;
  int cr;
  int cs;
  int cx;
  int i;
  int i0;
  int i1;
  int i2;
  int i3;
  int ix_0___;
  cf = 0;
  cr = 1;
  ad_count3 = 0;
  
  while ((cr <= nareas))
  //   Set effort series for year
  {
    cx = 0;
    ad_count0 = 0;
    
    while ((cx < npar))
    {
      i4stack_2_2i = i4stack_2_2i + 1;
      
      if (i4stack_2_2i > stackSizeInt(i4stack_2_2))
      {
        growStack(i4stack_2_2,i4stack_2_2i);
      }
      
      i4stack_2_2[i4stack_2_2i] = cf;
      cf = FbyPar[cx];
      cs = 1;
      ad_count = 0;
      
      while ((cs <= nsubyears))
      {
        EforYear[cf][cr][cs] = exp(par[cx]) * ECurrent[cf][cr][cs];
        i4stack_2_2i = i4stack_2_2i + 1;
        
        if (i4stack_2_2i > stackSizeInt(i4stack_2_2))
        {
          growStack(i4stack_2_2,i4stack_2_2i);
        }
        
        i4stack_2_2[i4stack_2_2i] = cs;
        cs = cs + 1;
        ad_count = ad_count + 1;
      }
      
      i4stack_2_2i = i4stack_2_2i + 1;
      
      if (i4stack_2_2i > stackSizeInt(i4stack_2_2))
      {
        growStack(i4stack_2_2,i4stack_2_2i);
      }
      
      i4stack_2_2[i4stack_2_2i] = ad_count;
      i4stack_2_2i = i4stack_2_2i + 1;
      
      if (i4stack_2_2i > stackSizeInt(i4stack_2_2))
      {
        growStack(i4stack_2_2,i4stack_2_2i);
      }
      
      i4stack_2_2[i4stack_2_2i] = cx;
      cx = cx + 1;
      ad_count0 = ad_count0 + 1;
    }
    
    i4stack_2_2i = i4stack_2_2i + 1;
    
    if (i4stack_2_2i > stackSizeInt(i4stack_2_2))
    {
      growStack(i4stack_2_2,i4stack_2_2i);
    }
    
    i4stack_2_2[i4stack_2_2i] = ad_count0;
    cx = 0;
    ad_count2 = 0;
    
    while ((cx < nfixed))
    {
      i4stack_2_2i = i4stack_2_2i + 1;
      
      if (i4stack_2_2i > stackSizeInt(i4stack_2_2))
      {
        growStack(i4stack_2_2,i4stack_2_2i);
      }
      
      i4stack_2_2[i4stack_2_2i] = cf;
      cf = FbyFixed[cx];
      cs = 1;
      ad_count1 = 0;
      
      while ((cs <= nsubyears))
      {
        EforYear[cf][cr][cs] = TAE[cx] * ECurrent[cf][cr][cs];
        i4stack_2_2i = i4stack_2_2i + 1;
        
        if (i4stack_2_2i > stackSizeInt(i4stack_2_2))
        {
          growStack(i4stack_2_2,i4stack_2_2i);
        }
        
        i4stack_2_2[i4stack_2_2i] = cs;
        cs = cs + 1;
        ad_count1 = ad_count1 + 1;
      }
      
      i4stack_2_2i = i4stack_2_2i + 1;
      
      if (i4stack_2_2i > stackSizeInt(i4stack_2_2))
      {
        growStack(i4stack_2_2,i4stack_2_2i);
      }
      
      i4stack_2_2[i4stack_2_2i] = ad_count1;
      cx = cx + 1;
      ad_count2 = ad_count2 + 1;
    }
    
    i4stack_2_2i = i4stack_2_2i + 1;
    
    if (i4stack_2_2i > stackSizeInt(i4stack_2_2))
    {
      growStack(i4stack_2_2,i4stack_2_2i);
    }
    
    i4stack_2_2[i4stack_2_2i] = ad_count2;
    i4stack_2_2i = i4stack_2_2i + 1;
    
    if (i4stack_2_2i > stackSizeInt(i4stack_2_2))
    {
      growStack(i4stack_2_2,i4stack_2_2i);
    }
    
    i4stack_2_2[i4stack_2_2i] = cr;
    cr = cr + 1;
    ad_count3 = ad_count3 + 1;
  }
  
  i4stack_2_2i = i4stack_2_2i + 1;
  
  if (i4stack_2_2i > stackSizeInt(i4stack_2_2))
  {
    growStack(i4stack_2_2,i4stack_2_2i);
  }
  
  i4stack_2_2[i4stack_2_2i] = ad_count3;
  //   Initialise N and SSN
  nextYear(N,NBefore);
  POPDYN_YEAR_BPAR(qy,R0,M,mat,Len_age,Wt_age,sel,EforYear,eforyearb2_par,mov,h,Recdist,Recdevs,RecSpatialDevs,SRrel,N,nb2_par,NBefore,nbeforeb2_par,SSN,C,cb2_par,SSBA,bIgnoreLast);
  
  for (ix_0___ = 0;ix_0___ <= npar - 1; ++ix_0___)
  {
    parb2_par[ix_0___] = 0.0;
  }
  
  ad_count3 = i4stack_2_2[i4stack_2_2i];
  i4stack_2_2i = i4stack_2_2i - 1;
  
  for (i3 = 1;i3 <= ad_count3; ++i3)
  {
    cr = i4stack_2_2[i4stack_2_2i];
    i4stack_2_2i = i4stack_2_2i - 1;
    ad_count2 = i4stack_2_2[i4stack_2_2i];
    i4stack_2_2i = i4stack_2_2i - 1;
    
    for (i2 = 1;i2 <= ad_count2; ++i2)
    {
      ad_count1 = i4stack_2_2[i4stack_2_2i];
      i4stack_2_2i = i4stack_2_2i - 1;
      
      for (i1 = 1;i1 <= ad_count1; ++i1)
      {
        cs = i4stack_2_2[i4stack_2_2i];
        i4stack_2_2i = i4stack_2_2i - 1;
        eforyearb2_par[cf][cr][cs] = 0.0;
      }
      
      cf = i4stack_2_2[i4stack_2_2i];
      i4stack_2_2i = i4stack_2_2i - 1;
    }
    
    ad_count0 = i4stack_2_2[i4stack_2_2i];
    i4stack_2_2i = i4stack_2_2i - 1;
    
    for (i0 = 1;i0 <= ad_count0; ++i0)
    {
      cx = i4stack_2_2[i4stack_2_2i];
      i4stack_2_2i = i4stack_2_2i - 1;
      ad_count = i4stack_2_2[i4stack_2_2i];
      i4stack_2_2i = i4stack_2_2i - 1;
      
      for (i = 1;i <= ad_count; ++i)
      {
        cs = i4stack_2_2[i4stack_2_2i];
        i4stack_2_2i = i4stack_2_2i - 1;
        parb2_par[cx] = parb2_par[cx] + ECurrent[cf][cr][cs] * exp(par[cx]) * eforyearb2_par[cf][cr][cs];
        eforyearb2_par[cf][cr][cs] = 0.0;
      }
      
      cf = i4stack_2_2[i4stack_2_2i];
      i4stack_2_2i = i4stack_2_2i - 1;
    }
  }
}


//   Differentiation of operatingmodelbase__popdyn_year in reverse (adjoint) mode:
//    gradient     of useful results: c
//    with respect to varying inputs: eannual
//   ----------------------------------------------------------------------------

void D_OperatingModelBase::POPDYN_YEAR_BPAR(const ARRAY_1D qy/* nfleets */, const ARRAY_1D R0/* npop */, const ARRAY_2D M/* nages,npop */, const ARRAY_2D mat/* nages,npop */, const ARRAY_2D Len_age/* nages,npop */, const ARRAY_2D Wt_age/* nages,npop */, const ARRAY_2D sel/* nages,nfleets */, const ARRAY_3D Eannual/* nfleets,nareas,nsubyears */, ARRAY_3D eannualb2_par/* nfleets,nareas,nsubyears */, const ARRAY_5D mov/* nareas,nareas,nsubyears,nages,npop */, const ARRAY_1D h/* npop */, const ARRAY_2D Recdist/* nareas,npop */, const ARRAY_2D Recdevs/* SpawnPerYr,npop */, const ARRAY_2D RecSpatialDevs/* nareas,npop */, const ARRAY_1I SRrel/* npop */, ARRAY_4D N/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nb2_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D NBefore/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D nbeforeb2_par/* nareas,nsubyears + 1,nages,npop */, ARRAY_4D SSN/* nareas,nsubyears,nages,npop */, ARRAY_5D C/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_5D cb2_par/* nfleets,nareas,nsubyears,nages,npop */, ARRAY_1D SSBA/* npop */, int bIgnoreLast)
{
  int ad_count;
  int ad_count0;
  int ad_count1;
  int ad_count10;
  int ad_count11;
  int ad_count12;
  int ad_count13;
  int ad_count2;
  int ad_count3;
  int ad_count4;
  int ad_count5;
  int ad_count6;
  int ad_count7;
  int ad_count8;
  int ad_count9;
  int branch;
  int ca;
  int cf;
  int cp;
  int cr;
  int cr2;
  int cs;
  double dFM;
  double dfmb2_par;
  double dFtot;
  double dftotb2_par;
  double dN;
  double dnb2_par;
  double dPlusGroup;
  double dplusgroupb2_par;
  double dRecruitment;
  double drecruitmentb2_par;
  double dSSB;
  double dSSB_area;
  double dssb_areab2_par;
  double dssbb2_par;
  double dSSN;
  double dssnb2_par;
  double dZ;
  double dzb2_par;
  int i;
  int i0;
  int i1;
  int i10;
  int i11;
  int i12;
  int i13;
  int i2;
  int i3;
  int i4;
  int i5;
  int i6;
  int i7;
  int i8;
  int i9;
  int ix_0___;
  int ix_1___;
  int ix_2___;
  int ix_3___;
  int nRecdevIdx;
  double temp;
  double temp0;
  double temp1;
  double tempb2_par;
  double tempb2_par0;
  double tempb2_par1;
  double tempb2_par2;
  dSSB = 0;
  dZ = 0;
  branch = 0;
  nRecdevIdx = 1;
  cs = 1;
  ad_count13 = 0;
  
  while ((cs <= nsubyears))
  {
    cp = 1;
    ad_count12 = 0;
    
    while ((cp <= npop))
    {
      r4stack_3_2i = r4stack_3_2i + 1;
      
      if (r4stack_3_2i > stackSizeInt(r4stack_3_2))
      {
        growStack(r4stack_3_2,r4stack_3_2i);
      }
      
      r4stack_3_2[r4stack_3_2i] = dSSB;
      dSSB = 0.0;
      cr = 1;
      ad_count0 = 0;
      
      while ((cr <= nareas))
      {
        dSSB_area = 0.0;
        ca = 1;
        ad_count = 0;
        
        while ((ca <= nages))
        {
          dSSN = NBefore[cr][cs][ca][cp] * mat[ca][cp];
          dSSB_area = dSSB_area + dSSN * Wt_age[ca][cp];
          i4stack_3_2i = i4stack_3_2i + 1;
          
          if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
          {
            growStack(i4stack_3_2,i4stack_3_2i);
          }
          
          i4stack_3_2[i4stack_3_2i] = ca;
          ca = ca + 1;
          ad_count = ad_count + 1;
        }
        
        i4stack_3_2i = i4stack_3_2i + 1;
        
        if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
        {
          growStack(i4stack_3_2,i4stack_3_2i);
        }
        
        i4stack_3_2[i4stack_3_2i] = ad_count;
        dSSB = dSSB + dSSB_area;
        i4stack_3_2i = i4stack_3_2i + 1;
        
        if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
        {
          growStack(i4stack_3_2,i4stack_3_2i);
        }
        
        i4stack_3_2[i4stack_3_2i] = cr;
        cr = cr + 1;
        ad_count0 = ad_count0 + 1;
      }
      
      i4stack_3_2i = i4stack_3_2i + 1;
      
      if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
      {
        growStack(i4stack_3_2,i4stack_3_2i);
      }
      
      i4stack_3_2[i4stack_3_2i] = ad_count0;
      
      //   Run recruitment
      if ((Recsubyr[cs] != 0))
      {
        if ((SRrel[cp] == 1))
        {
          dRecruitment = Recdevs[nRecdevIdx][cp] * (0.8 * R0[cp] * h[cp] * dSSB / (0.2 * SSBpR[cp] * R0[cp] * (1.0 - h[cp]) + (h[cp] - 0.2) * dSSB));
          bstack_3_2i = bstack_3_2i + 1;
          
          if (bstack_3_2i > stackSizeInt(bstack_3_2))
          {
            growStack(bstack_3_2,bstack_3_2i);
          }
          
          bstack_3_2[bstack_3_2i] = 0;
        }
        else
        {
          dRecruitment = Recdevs[nRecdevIdx][cp] * aR[cp] * dSSB * exp(-(bR[cp] * dSSB));
          bstack_3_2i = bstack_3_2i + 1;
          
          if (bstack_3_2i > stackSizeInt(bstack_3_2))
          {
            growStack(bstack_3_2,bstack_3_2i);
          }
          
          bstack_3_2[bstack_3_2i] = 1;
        }
        
        cr = 1;
        ad_count1 = 0;
        
        while ((cr <= nareas))
        {
          NBefore[cr][cs][1][cp] = dRecruitment * RecSpatialDevs[cr][cp] * Recdist[cr][cp];
          r4stack_3_2i = r4stack_3_2i + 1;
          
          if (r4stack_3_2i > stackSizeInt(r4stack_3_2))
          {
            growStack(r4stack_3_2,r4stack_3_2i);
          }
          
          r4stack_3_2[r4stack_3_2i] = N[cr][cs][1][cp];
          N[cr][cs][1][cp] = NBefore[cr][cs][1][cp];
          i4stack_3_2i = i4stack_3_2i + 1;
          
          if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
          {
            growStack(i4stack_3_2,i4stack_3_2i);
          }
          
          i4stack_3_2[i4stack_3_2i] = cr;
          cr = cr + 1;
          ad_count1 = ad_count1 + 1;
        }
        
        i4stack_3_2i = i4stack_3_2i + 1;
        
        if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
        {
          growStack(i4stack_3_2,i4stack_3_2i);
        }
        
        i4stack_3_2[i4stack_3_2i] = ad_count1;
        bstack_3_2i = bstack_3_2i + 1;
        
        if (bstack_3_2i > stackSizeInt(bstack_3_2))
        {
          growStack(bstack_3_2,bstack_3_2i);
        }
        
        bstack_3_2[bstack_3_2i] = 0;
      }
      else
      {
        bstack_3_2i = bstack_3_2i + 1;
        
        if (bstack_3_2i > stackSizeInt(bstack_3_2))
        {
          growStack(bstack_3_2,bstack_3_2i);
        }
        
        bstack_3_2[bstack_3_2i] = 1;
      }
      
      //  rec<-OM@Recdevs[,pp,RecdevInd]*((0.8*OM@R0[,pp]*OM@h[,pp]*SSBA[,pp,y])/(0.2*SSBpR[,pp]*OM@R0[,pp]*(1-OM@h[,pp])+(OM@h[,pp]-0.2)
      // *SSBA[,pp,y]))
      //   Beverton Holt stock recruitment relationship
      //   Most transparent form of the Ricker uses alpha and beta params
      //   Move Fish
      ca = 1;
      ad_count5 = 0;
      
      while ((ca <= nages))
      {
        cr = 1;
        ad_count3 = 0;
        
        while ((cr <= nareas))
        //   N[,,y,m,]<-domov2(N[,,y,m,],mov[,,m,,])
        {
          dN = 0.0;
          cr2 = 1;
          ad_count2 = 0;
          
          while ((cr2 <= nareas))
          {
            dN = dN + N[cr2][cs][ca][cp] * mov[cr][cr2][cs][ca][cp];
            i4stack_3_2i = i4stack_3_2i + 1;
            
            if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
            {
              growStack(i4stack_3_2,i4stack_3_2i);
            }
            
            i4stack_3_2[i4stack_3_2i] = cr2;
            cr2 = cr2 + 1;
            ad_count2 = ad_count2 + 1;
          }
          
          i4stack_3_2i = i4stack_3_2i + 1;
          
          if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
          {
            growStack(i4stack_3_2,i4stack_3_2i);
          }
          
          i4stack_3_2[i4stack_3_2i] = ad_count2;
          MovN[cr] = dN;
          i4stack_3_2i = i4stack_3_2i + 1;
          
          if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
          {
            growStack(i4stack_3_2,i4stack_3_2i);
          }
          
          i4stack_3_2[i4stack_3_2i] = cr;
          cr = cr + 1;
          ad_count3 = ad_count3 + 1;
        }
        
        i4stack_3_2i = i4stack_3_2i + 1;
        
        if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
        {
          growStack(i4stack_3_2,i4stack_3_2i);
        }
        
        i4stack_3_2[i4stack_3_2i] = ad_count3;
        cr = 1;
        ad_count4 = 0;
        
        while ((cr <= nareas))
        {
          r4stack_3_2i = r4stack_3_2i + 1;
          
          if (r4stack_3_2i > stackSizeInt(r4stack_3_2))
          {
            growStack(r4stack_3_2,r4stack_3_2i);
          }
          
          r4stack_3_2[r4stack_3_2i] = N[cr][cs][ca][cp];
          N[cr][cs][ca][cp] = MovN[cr];
          i4stack_3_2i = i4stack_3_2i + 1;
          
          if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
          {
            growStack(i4stack_3_2,i4stack_3_2i);
          }
          
          i4stack_3_2[i4stack_3_2i] = cr;
          cr = cr + 1;
          ad_count4 = ad_count4 + 1;
        }
        
        i4stack_3_2i = i4stack_3_2i + 1;
        
        if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
        {
          growStack(i4stack_3_2,i4stack_3_2i);
        }
        
        i4stack_3_2[i4stack_3_2i] = ad_count4;
        i4stack_3_2i = i4stack_3_2i + 1;
        
        if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
        {
          growStack(i4stack_3_2,i4stack_3_2i);
        }
        
        i4stack_3_2[i4stack_3_2i] = ca;
        ca = ca + 1;
        ad_count5 = ad_count5 + 1;
      }
      
      i4stack_3_2i = i4stack_3_2i + 1;
      
      if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
      {
        growStack(i4stack_3_2,i4stack_3_2i);
      }
      
      i4stack_3_2[i4stack_3_2i] = ad_count5;
      //   Fishing and natural mortality
      ca = 1;
      ad_count9 = 0;
      
      while ((ca <= nages))
      {
        cr = 1;
        ad_count8 = 0;
        
        while ((cr <= nareas))
        {
          dFtot = 0.0;
          cf = 1;
          ad_count6 = 0;
          
          while ((cf <= nfleets))
          //  FM[PAYMRF2] <- totF*ECurrent[MRF2]*sel[FA2]
          {
            dFM = Eannual[cf][cr][cs] * sel[ca][cf] * qy[cf];
            r4stack_3_2i = r4stack_3_2i + 1;
            
            if (r4stack_3_2i > stackSizeInt(r4stack_3_2))
            {
              growStack(r4stack_3_2,r4stack_3_2i);
            }
            
            r4stack_3_2[r4stack_3_2i] = FM[cf];
            FM[cf] = dFM;
            dFtot = dFtot + dFM;
            i4stack_3_2i = i4stack_3_2i + 1;
            
            if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
            {
              growStack(i4stack_3_2,i4stack_3_2i);
            }
            
            i4stack_3_2[i4stack_3_2i] = cf;
            cf = cf + 1;
            ad_count6 = ad_count6 + 1;
          }
          
          i4stack_3_2i = i4stack_3_2i + 1;
          
          if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
          {
            growStack(i4stack_3_2,i4stack_3_2i);
          }
          
          i4stack_3_2[i4stack_3_2i] = ad_count6;
          r4stack_3_2i = r4stack_3_2i + 1;
          
          if (r4stack_3_2i > stackSizeInt(r4stack_3_2))
          {
            growStack(r4stack_3_2,r4stack_3_2i);
          }
          
          r4stack_3_2[r4stack_3_2i] = dZ;
          dZ = dFtot + M[ca][cp] / nsubyears;
          cf = 1;
          ad_count7 = 0;
          
          while ((cf <= nfleets))
          {
            i4stack_3_2i = i4stack_3_2i + 1;
            
            if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
            {
              growStack(i4stack_3_2,i4stack_3_2i);
            }
            
            i4stack_3_2[i4stack_3_2i] = cf;
            cf = cf + 1;
            ad_count7 = ad_count7 + 1;
          }
          
          i4stack_3_2i = i4stack_3_2i + 1;
          
          if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
          {
            growStack(i4stack_3_2,i4stack_3_2i);
          }
          
          i4stack_3_2[i4stack_3_2i] = ad_count7;
          r4stack_3_2i = r4stack_3_2i + 1;
          
          if (r4stack_3_2i > stackSizeInt(r4stack_3_2))
          {
            growStack(r4stack_3_2,r4stack_3_2i);
          }
          
          r4stack_3_2[r4stack_3_2i] = N[cr][cs][ca][cp];
          N[cr][cs][ca][cp] = N[cr][cs][ca][cp] * exp(-dZ);
          i4stack_3_2i = i4stack_3_2i + 1;
          
          if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
          {
            growStack(i4stack_3_2,i4stack_3_2i);
          }
          
          i4stack_3_2[i4stack_3_2i] = cr;
          cr = cr + 1;
          ad_count8 = ad_count8 + 1;
        }
        
        i4stack_3_2i = i4stack_3_2i + 1;
        
        if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
        {
          growStack(i4stack_3_2,i4stack_3_2i);
        }
        
        i4stack_3_2[i4stack_3_2i] = ad_count8;
        i4stack_3_2i = i4stack_3_2i + 1;
        
        if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
        {
          growStack(i4stack_3_2,i4stack_3_2i);
        }
        
        i4stack_3_2[i4stack_3_2i] = ca;
        ca = ca + 1;
        ad_count9 = ad_count9 + 1;
      }
      
      i4stack_3_2i = i4stack_3_2i + 1;
      
      if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
      {
        growStack(i4stack_3_2,i4stack_3_2i);
      }
      
      i4stack_3_2[i4stack_3_2i] = ad_count9;
      
      if ((cs != nsubyears) || (bIgnoreLast == 0))
      {
        cr = 1;
        ad_count11 = 0;
        
        while ((cr <= nareas))
        {
          dPlusGroup = N[cr][cs][nages][cp];
          NBefore[cr][cs + 1][1][cp] = 0.0;
          r4stack_3_2i = r4stack_3_2i + 1;
          
          if (r4stack_3_2i > stackSizeInt(r4stack_3_2))
          {
            growStack(r4stack_3_2,r4stack_3_2i);
          }
          
          r4stack_3_2[r4stack_3_2i] = N[cr][cs + 1][1][cp];
          N[cr][cs + 1][1][cp] = 0.0;
          ca = nages - 1;
          ad_count10 = 0;
          
          while ((ca >= 1))
          {
            NBefore[cr][cs + 1][ca + 1][cp] = N[cr][cs][ca][cp];
            r4stack_3_2i = r4stack_3_2i + 1;
            
            if (r4stack_3_2i > stackSizeInt(r4stack_3_2))
            {
              growStack(r4stack_3_2,r4stack_3_2i);
            }
            
            r4stack_3_2[r4stack_3_2i] = N[cr][cs + 1][ca + 1][cp];
            N[cr][cs + 1][ca + 1][cp] = NBefore[cr][cs + 1][ca + 1][cp];
            i4stack_3_2i = i4stack_3_2i + 1;
            
            if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
            {
              growStack(i4stack_3_2,i4stack_3_2i);
            }
            
            i4stack_3_2[i4stack_3_2i] = ca;
            ca = ca - 1;
            ad_count10 = ad_count10 + 1;
          }
          
          i4stack_3_2i = i4stack_3_2i + 1;
          
          if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
          {
            growStack(i4stack_3_2,i4stack_3_2i);
          }
          
          i4stack_3_2[i4stack_3_2i] = ad_count10;
          NBefore[cr][cs + 1][nages][cp] = NBefore[cr][cs + 1][nages][cp] + dPlusGroup;
          r4stack_3_2i = r4stack_3_2i + 1;
          
          if (r4stack_3_2i > stackSizeInt(r4stack_3_2))
          {
            growStack(r4stack_3_2,r4stack_3_2i);
          }
          
          r4stack_3_2[r4stack_3_2i] = N[cr][cs + 1][nages][cp];
          N[cr][cs + 1][nages][cp] = N[cr][cs + 1][nages][cp] + dPlusGroup;
          i4stack_3_2i = i4stack_3_2i + 1;
          
          if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
          {
            growStack(i4stack_3_2,i4stack_3_2i);
          }
          
          i4stack_3_2[i4stack_3_2i] = cr;
          cr = cr + 1;
          ad_count11 = ad_count11 + 1;
        }
        
        i4stack_3_2i = i4stack_3_2i + 1;
        
        if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
        {
          growStack(i4stack_3_2,i4stack_3_2i);
        }
        
        i4stack_3_2[i4stack_3_2i] = ad_count11;
        bstack_3_2i = bstack_3_2i + 1;
        
        if (bstack_3_2i > stackSizeInt(bstack_3_2))
        {
          growStack(bstack_3_2,bstack_3_2i);
        }
        
        bstack_3_2[bstack_3_2i] = 0;
      }
      else
      {
        bstack_3_2i = bstack_3_2i + 1;
        
        if (bstack_3_2i > stackSizeInt(bstack_3_2))
        {
          growStack(bstack_3_2,bstack_3_2i);
        }
        
        bstack_3_2[bstack_3_2i] = 1;
      }
      
      i4stack_3_2i = i4stack_3_2i + 1;
      
      if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
      {
        growStack(i4stack_3_2,i4stack_3_2i);
      }
      
      i4stack_3_2[i4stack_3_2i] = cp;
      cp = cp + 1;
      ad_count12 = ad_count12 + 1;
    }
    
    i4stack_3_2i = i4stack_3_2i + 1;
    
    if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
    {
      growStack(i4stack_3_2,i4stack_3_2i);
    }
    
    i4stack_3_2[i4stack_3_2i] = ad_count12;
    
    if ((Recsubyr[cs] != 0))
    {
      i4stack_3_2i = i4stack_3_2i + 1;
      
      if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
      {
        growStack(i4stack_3_2,i4stack_3_2i);
      }
      
      i4stack_3_2[i4stack_3_2i] = nRecdevIdx;
      nRecdevIdx = nRecdevIdx + 1;
      bstack_3_2i = bstack_3_2i + 1;
      
      if (bstack_3_2i > stackSizeInt(bstack_3_2))
      {
        growStack(bstack_3_2,bstack_3_2i);
      }
      
      bstack_3_2[bstack_3_2i] = 0;
    }
    else
    {
      bstack_3_2i = bstack_3_2i + 1;
      
      if (bstack_3_2i > stackSizeInt(bstack_3_2))
      {
        growStack(bstack_3_2,bstack_3_2i);
      }
      
      bstack_3_2[bstack_3_2i] = 1;
    }
    
    i4stack_3_2i = i4stack_3_2i + 1;
    
    if (i4stack_3_2i > stackSizeInt(i4stack_3_2))
    {
      growStack(i4stack_3_2,i4stack_3_2i);
    }
    
    i4stack_3_2[i4stack_3_2i] = cs;
    cs = cs + 1;
    ad_count13 = ad_count13 + 1;
  }
  
  for (ix_0___ = 1;ix_0___ <= nareas; ++ix_0___)
  {
    movnb2_par[ix_0___] = 0.0;
  }
  
  for (ix_0___ = 1;ix_0___ <= nfleets; ++ix_0___)
  {
    fmb2_par[ix_0___] = 0.0;
  }
  
  for (ix_3___ = 1;ix_3___ <= npop; ++ix_3___)
  {
    for (ix_2___ = 1;ix_2___ <= nages; ++ix_2___)
    {
      for (ix_1___ = 1;ix_1___ <= nsubyears + 1; ++ix_1___)
      {
        for (ix_0___ = 1;ix_0___ <= nareas; ++ix_0___)
        {
          nbeforeb2_par[ix_0___][ix_1___][ix_2___][ix_3___] = 0.0;
        }
      }
    }
  }
  
  for (ix_3___ = 1;ix_3___ <= npop; ++ix_3___)
  {
    for (ix_2___ = 1;ix_2___ <= nages; ++ix_2___)
    {
      for (ix_1___ = 1;ix_1___ <= nsubyears + 1; ++ix_1___)
      {
        for (ix_0___ = 1;ix_0___ <= nareas; ++ix_0___)
        {
          nb2_par[ix_0___][ix_1___][ix_2___][ix_3___] = 0.0;
        }
      }
    }
  }
  
  for (ix_2___ = 1;ix_2___ <= nsubyears; ++ix_2___)
  {
    for (ix_1___ = 1;ix_1___ <= nareas; ++ix_1___)
    {
      for (ix_0___ = 1;ix_0___ <= nfleets; ++ix_0___)
      {
        eannualb2_par[ix_0___][ix_1___][ix_2___] = 0.0;
      }
    }
  }
  
  for (i13 = 1;i13 <= ad_count13; ++i13)
  {
    cs = i4stack_3_2[i4stack_3_2i];
    i4stack_3_2i = i4stack_3_2i - 1;
    branch = bstack_3_2[bstack_3_2i];
    bstack_3_2i = bstack_3_2i - 1;
    nRecdevIdx = i4stack_3_2[i4stack_3_2i];
    i4stack_3_2i = i4stack_3_2i - 1;
    ad_count12 = i4stack_3_2[i4stack_3_2i];
    i4stack_3_2i = i4stack_3_2i - 1;
    
    for (i12 = 1;i12 <= ad_count12; ++i12)
    {
      cp = i4stack_3_2[i4stack_3_2i];
      i4stack_3_2i = i4stack_3_2i - 1;
      branch = bstack_3_2[bstack_3_2i];
      bstack_3_2i = bstack_3_2i - 1;
      
      if ((branch == 0))
      {
        ad_count11 = i4stack_3_2[i4stack_3_2i];
        i4stack_3_2i = i4stack_3_2i - 1;
        
        for (i11 = 1;i11 <= ad_count11; ++i11)
        {
          cr = i4stack_3_2[i4stack_3_2i];
          i4stack_3_2i = i4stack_3_2i - 1;
          N[cr][cs + 1][nages][cp] = r4stack_3_2[r4stack_3_2i];
          r4stack_3_2i = r4stack_3_2i - 1;
          dplusgroupb2_par = nbeforeb2_par[cr][cs + 1][nages][cp] + nb2_par[cr][cs + 1][nages][cp];
          ad_count10 = i4stack_3_2[i4stack_3_2i];
          i4stack_3_2i = i4stack_3_2i - 1;
          
          for (i10 = 1;i10 <= ad_count10; ++i10)
          {
            ca = i4stack_3_2[i4stack_3_2i];
            i4stack_3_2i = i4stack_3_2i - 1;
            N[cr][cs + 1][ca + 1][cp] = r4stack_3_2[r4stack_3_2i];
            r4stack_3_2i = r4stack_3_2i - 1;
            nbeforeb2_par[cr][cs + 1][ca + 1][cp] = nbeforeb2_par[cr][cs + 1][ca + 1][cp] + nb2_par[cr][cs + 1][ca + 1][cp];
            nb2_par[cr][cs + 1][ca + 1][cp] = 0.0;
            nb2_par[cr][cs][ca][cp] = nb2_par[cr][cs][ca][cp] + nbeforeb2_par[cr][cs + 1][ca + 1][cp];
            nbeforeb2_par[cr][cs + 1][ca + 1][cp] = 0.0;
          }
          
          N[cr][cs + 1][1][cp] = r4stack_3_2[r4stack_3_2i];
          r4stack_3_2i = r4stack_3_2i - 1;
          nb2_par[cr][cs + 1][1][cp] = 0.0;
          nbeforeb2_par[cr][cs + 1][1][cp] = 0.0;
          nb2_par[cr][cs][nages][cp] = nb2_par[cr][cs][nages][cp] + dplusgroupb2_par;
        }
      }
      
      ad_count9 = i4stack_3_2[i4stack_3_2i];
      i4stack_3_2i = i4stack_3_2i - 1;
      
      for (i9 = 1;i9 <= ad_count9; ++i9)
      {
        ca = i4stack_3_2[i4stack_3_2i];
        i4stack_3_2i = i4stack_3_2i - 1;
        ad_count8 = i4stack_3_2[i4stack_3_2i];
        i4stack_3_2i = i4stack_3_2i - 1;
        
        for (i8 = 1;i8 <= ad_count8; ++i8)
        {
          cr = i4stack_3_2[i4stack_3_2i];
          i4stack_3_2i = i4stack_3_2i - 1;
          N[cr][cs][ca][cp] = r4stack_3_2[r4stack_3_2i];
          r4stack_3_2i = r4stack_3_2i - 1;
          dzb2_par = -(exp(-dZ) * N[cr][cs][ca][cp] * nb2_par[cr][cs][ca][cp]);
          nb2_par[cr][cs][ca][cp] = exp(-dZ) * nb2_par[cr][cs][ca][cp];
          ad_count7 = i4stack_3_2[i4stack_3_2i];
          i4stack_3_2i = i4stack_3_2i - 1;
          
          for (i7 = 1;i7 <= ad_count7; ++i7)
          {
            cf = i4stack_3_2[i4stack_3_2i];
            i4stack_3_2i = i4stack_3_2i - 1;
            tempb2_par1 = (1 - exp(-dZ)) * cb2_par[cf][cr][cs][ca][cp];
            temp0 = FM[cf] / dZ;
            temp1 = N[cr][cs][ca][cp];
            tempb2_par2 = temp1 * tempb2_par1 / dZ;
            nb2_par[cr][cs][ca][cp] = nb2_par[cr][cs][ca][cp] + temp0 * tempb2_par1;
            fmb2_par[cf] = fmb2_par[cf] + tempb2_par2;
            dzb2_par = dzb2_par + exp(-dZ) * temp1 * temp0 * cb2_par[cf][cr][cs][ca][cp] - temp0 * tempb2_par2;
            cb2_par[cf][cr][cs][ca][cp] = 0.0;
          }
          
          dZ = r4stack_3_2[r4stack_3_2i];
          r4stack_3_2i = r4stack_3_2i - 1;
          dftotb2_par = dzb2_par;
          ad_count6 = i4stack_3_2[i4stack_3_2i];
          i4stack_3_2i = i4stack_3_2i - 1;
          
          for (i6 = 1;i6 <= ad_count6; ++i6)
          {
            cf = i4stack_3_2[i4stack_3_2i];
            i4stack_3_2i = i4stack_3_2i - 1;
            dfmb2_par = fmb2_par[cf] + dftotb2_par;
            FM[cf] = r4stack_3_2[r4stack_3_2i];
            r4stack_3_2i = r4stack_3_2i - 1;
            fmb2_par[cf] = 0.0;
            eannualb2_par[cf][cr][cs] = eannualb2_par[cf][cr][cs] + sel[ca][cf] * qy[cf] * dfmb2_par;
          }
        }
      }
      
      ad_count5 = i4stack_3_2[i4stack_3_2i];
      i4stack_3_2i = i4stack_3_2i - 1;
      
      for (i5 = 1;i5 <= ad_count5; ++i5)
      {
        ca = i4stack_3_2[i4stack_3_2i];
        i4stack_3_2i = i4stack_3_2i - 1;
        ad_count4 = i4stack_3_2[i4stack_3_2i];
        i4stack_3_2i = i4stack_3_2i - 1;
        
        for (i4 = 1;i4 <= ad_count4; ++i4)
        {
          cr = i4stack_3_2[i4stack_3_2i];
          i4stack_3_2i = i4stack_3_2i - 1;
          N[cr][cs][ca][cp] = r4stack_3_2[r4stack_3_2i];
          r4stack_3_2i = r4stack_3_2i - 1;
          movnb2_par[cr] = movnb2_par[cr] + nb2_par[cr][cs][ca][cp];
          nb2_par[cr][cs][ca][cp] = 0.0;
        }
        
        ad_count3 = i4stack_3_2[i4stack_3_2i];
        i4stack_3_2i = i4stack_3_2i - 1;
        
        for (i3 = 1;i3 <= ad_count3; ++i3)
        {
          cr = i4stack_3_2[i4stack_3_2i];
          i4stack_3_2i = i4stack_3_2i - 1;
          dnb2_par = movnb2_par[cr];
          movnb2_par[cr] = 0.0;
          ad_count2 = i4stack_3_2[i4stack_3_2i];
          i4stack_3_2i = i4stack_3_2i - 1;
          
          for (i2 = 1;i2 <= ad_count2; ++i2)
          {
            cr2 = i4stack_3_2[i4stack_3_2i];
            i4stack_3_2i = i4stack_3_2i - 1;
            nb2_par[cr2][cs][ca][cp] = nb2_par[cr2][cs][ca][cp] + mov[cr][cr2][cs][ca][cp] * dnb2_par;
          }
        }
      }
      
      branch = bstack_3_2[bstack_3_2i];
      bstack_3_2i = bstack_3_2i - 1;
      
      if ((branch == 0))
      {
        drecruitmentb2_par = 0.0;
        ad_count1 = i4stack_3_2[i4stack_3_2i];
        i4stack_3_2i = i4stack_3_2i - 1;
        
        for (i1 = 1;i1 <= ad_count1; ++i1)
        {
          cr = i4stack_3_2[i4stack_3_2i];
          i4stack_3_2i = i4stack_3_2i - 1;
          N[cr][cs][1][cp] = r4stack_3_2[r4stack_3_2i];
          r4stack_3_2i = r4stack_3_2i - 1;
          nbeforeb2_par[cr][cs][1][cp] = nbeforeb2_par[cr][cs][1][cp] + nb2_par[cr][cs][1][cp];
          nb2_par[cr][cs][1][cp] = 0.0;
          drecruitmentb2_par = drecruitmentb2_par + Recdist[cr][cp] * RecSpatialDevs[cr][cp] * nbeforeb2_par[cr][cs][1][cp];
          nbeforeb2_par[cr][cs][1][cp] = 0.0;
        }
        
        branch = bstack_3_2[bstack_3_2i];
        bstack_3_2i = bstack_3_2i - 1;
        
        if ((branch == 0))
        {
          temp = 0.2 * SSBpR[cp] * R0[cp] * (-h[cp] + 1.0) + (h[cp] - 0.2) * dSSB;
          tempb2_par = Recdevs[nRecdevIdx][cp] * h[cp] * R0[cp] * 0.8 * drecruitmentb2_par / temp;
          dssbb2_par = (1.0 - dSSB * (h[cp] - 0.2) / temp) * tempb2_par;
        }
        else
        {
          tempb2_par0 = Recdevs[nRecdevIdx][cp] * aR[cp] * drecruitmentb2_par;
          dssbb2_par = (exp(-(bR[cp] * dSSB)) - exp(-(bR[cp] * dSSB)) * dSSB * bR[cp]) * tempb2_par0;
        }
      }
      else
      {
        dssbb2_par = 0.0;
      }
      
      ad_count0 = i4stack_3_2[i4stack_3_2i];
      i4stack_3_2i = i4stack_3_2i - 1;
      
      for (i0 = 1;i0 <= ad_count0; ++i0)
      {
        cr = i4stack_3_2[i4stack_3_2i];
        i4stack_3_2i = i4stack_3_2i - 1;
        dssb_areab2_par = dssbb2_par;
        ad_count = i4stack_3_2[i4stack_3_2i];
        i4stack_3_2i = i4stack_3_2i - 1;
        
        for (i = 1;i <= ad_count; ++i)
        {
          ca = i4stack_3_2[i4stack_3_2i];
          i4stack_3_2i = i4stack_3_2i - 1;
          dssnb2_par = Wt_age[ca][cp] * dssb_areab2_par;
          nbeforeb2_par[cr][cs][ca][cp] = nbeforeb2_par[cr][cs][ca][cp] + mat[ca][cp] * dssnb2_par;
        }
      }
      
      dSSB = r4stack_3_2[r4stack_3_2i];
      r4stack_3_2i = r4stack_3_2i - 1;
    }
  }
}




#include "include/OmB_impl_lib_interface_methods.hpp"
#include "include/OmB_impl_lib_interface_globals.hpp"
#include "include/OmB_impl_lib_interface_constructor.hpp"


