// ----------------------------------------------------------------------------
// MSE operating model implemented with ADT and ADlib
// ----------------------------------------------------------------------------


#include "niMseom.hpp"
#include <R.h>


// ----------------------------------------------------------------------------

void OperatingModelBase::popdyn_init(const ARRAY_1D R0/* npop */,
                                     const ARRAY_2D mat/* nages, npop */,
                                     const ARRAY_3D Idist/* nareas, nages, npop */,
                                     ARRAY_4D N/* nareas, nsubyears + 1, nages, npop */,
                                     ARRAY_4D NBefore/* nareas, nsubyears + 1, nages, npop */,
                                     ARRAY_4D SSN/* nareas, nsubyears, nages, npop */)
{
  int    ca;
  int    cp;
  int    cr;
  int    mm;
  double dN;
  double dNBefore;
  double dSSN;

  mm = 1;

  // N[PAYMR]<-surv[PA]*R0[P]*Idist[PAR]                         # Calculate virgin Numbers
  // SSN[PAYMR] <- N[PAYMR] * mat[PAY]
  // SSB[PAYMR]<-SSN[PAYMR]*Wt_age[PAY]                          # Calculate spawning stock biomass
  for (cp = 1 ; cp <= npop ; cp++)
  {
    for (ca = 1 ; ca <= nages ; ca++)
    {
      for (cr = 1 ; cr <= nareas ; cr++)
      {
        dN        = surv[ca][cp] * R0[cp] * Idist[cr][ca][cp] * 0.3;
        dNBefore  = dN;
        dSSN      = dN * mat[ca][cp];

        N[cr][mm][ca][cp]        = dN;
        NBefore[cr][mm][ca][cp]  = dNBefore;
        SSN[cr][mm][ca][cp]      = dSSN;
      }
    }
  }
}

// ----------------------------------------------------------------------------

void OperatingModelBase::popdyn_year(const ARRAY_1D qy/* nfleets */,
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
                                     int bIgnoreLast)
{
  int    ca;
  int    cp;
  int    cf;
  int    cr;
  int    cs;
  int    cr2;
  double dSSN;
  double dSSB;
  double dSSB_area;
  double dFtot;
  double dFM;
  double dZ;
  double dRecruitment;
  double dN;
  double dPlusGroup;
  int    nRecdevIdx;

  nRecdevIdx  = 1;

  for (cs = 1 ; cs <= nsubyears ; cs++)
  {
    for (cp = 1 ; cp <= npop ; cp++)
    {
      // Update SSB and SSN and do recruitment.
      dSSB = 0.0;

      for (cr = 1 ; cr <= nareas ; cr++)
      {
        dSSB_area = 0.0;

        for (ca = 1 ; ca <= nages ; ca++)
        {
          dSSN                = NBefore[cr][cs][ca][cp] * mat[ca][cp];
          SSN[cr][cs][ca][cp] = dSSN;
          dSSB_area          += dSSN * Wt_age[ca][cp];
        }

        dSSB += dSSB_area;
      }

      SSBA[cp] = dSSB;

      // Run recruitment
      if (Recsubyr[cs] != 0)
      {
        //rec<-OM@Recdevs[,pp,RecdevInd]*((0.8*OM@R0[,pp]*OM@h[,pp]*SSBA[,pp,y])/(0.2*SSBpR[,pp]*OM@R0[,pp]*(1-OM@h[,pp])+(OM@h[,pp]-0.2)*SSBA[,pp,y]))
        if (SRrel[cp] == 1)
        {
          // Beverton Holt stock recruitment relationship
          dRecruitment = Recdevs[nRecdevIdx][cp] * ((0.8 * R0[cp] * h[cp] * dSSB) / (0.2 * SSBpR[cp] * R0[cp] * (1.0 - h[cp]) + (h[cp] - 0.2) * dSSB));
        }
        else
        {
          // Most transparent form of the Ricker uses alpha and beta params
          dRecruitment = Recdevs[nRecdevIdx][cp] * aR[cp] * dSSB * exp(-bR[cp] * dSSB);
        }

        for (cr = 1 ; cr <= nareas ; cr++)
        {
          NBefore[cr][cs][1][cp] = dRecruitment * RecSpatialDevs[cr][cp] * Recdist[cr][cp];
          N[cr][cs][1][cp]       = NBefore[cr][cs][1][cp];
        }
      }

      // Move Fish
      for (ca = 1 ; ca <= nages ; ca++)
      {
        for (cr = 1 ; cr <= nareas ; cr++)
        {
          // N[,,y,m,]<-domov2(N[,,y,m,],mov[,,m,,])
          dN = 0.0;

          for (cr2 = 1 ; cr2 <= nareas ; cr2++)
          {
            dN += N[cr2][cs][ca][cp] * mov[cr][cr2][cs][ca][cp];
          }

          MovN[cr] = dN;
        }

        for (cr = 1 ; cr <= nareas ; cr++)
        {
          N[cr][cs][ca][cp] = MovN[cr];
        }
      }

      // Fishing and natural mortality
      for (ca = 1 ; ca <= nages ; ca++)
      {
        for (cr = 1 ; cr <= nareas ; cr++)
        {
          dFtot = 0.0;

          for (cf = 1 ; cf <= nfleets ; cf++)
          {
            //FM[PAYMRF2] <- totF*ECurrent[MRF2]*sel[FA2]
            dFM = Eannual[cf][cr][cs] * sel[ca][cf] * qy[cf];

            FM[cf] = dFM;
            dFtot += dFM;
          }

          //Ftot <- apply(FM[,,y,m,,,drop=F],c(1,2,5),sum)
          //Z[PAYMR] <- Ftot[PAR]+M[PAY]/nsubyears
          dZ = dFtot + (M[ca][cp] / nsubyears);

          for (cf = 1 ; cf <= nfleets ; cf++)
          {
            //C[PAYMRF2] <- N[PAYMR2]*(1-exp(-Z[PAYMR2]))*(FM[PAYMRF2]/Z[PAYMR2])
            C[cf][cr][cs][ca][cp] = N[cr][cs][ca][cp] * (1 - exp(-dZ)) * (FM[cf] / dZ);
          }

          //N[,,y,m,]<-N[,,y,m,]*exp(-Z[,,y,m,])
          N[cr][cs][ca][cp] *= exp(-dZ);
        }
      }

      if ((cs != nsubyears) || (bIgnoreLast == 0))
      {
        // Age fish.
        //N[,pp,nages-1,y,mm,]          <- N[,pp,nages-1,y,mm,] + N[,pp,nages,y,mm,]
        //NBefore[,pp,2:nages,y,mm+1,]  <- N[,pp,1:(nages-1),y,mm,]
        //NBefore[,pp,1,y,mm+1,]        <- rec*recSpatialDevs[,pp,]
        //N[,pp,,y,mm+1,]               <- NBefore[,pp,,y,mm+1,]
        for (cr = 1 ; cr <= nareas ; cr++)
        {
          dPlusGroup                 = N[cr][cs][nages][cp];
          NBefore[cr][cs + 1][1][cp] = 0.0;
          N[cr][cs + 1][1][cp]       = 0.0;

          for (ca = nages - 1 ; ca >= 1 ; ca--)
          {
            NBefore[cr][cs + 1][ca + 1][cp] = N[cr][cs][ca][cp];
            N[cr][cs + 1][ca + 1][cp]       = NBefore[cr][cs + 1][ca + 1][cp];
          }

          NBefore[cr][cs + 1][nages][cp] += dPlusGroup;
          N[cr][cs + 1][nages][cp]       += dPlusGroup;
        }
      }
    }

    if (Recsubyr[cs] != 0)
    {
      nRecdevIdx++;
    }
  } // end nsubyears
}

// ----------------------------------------------------------------------------

void OperatingModelBase::popdyn(double totF,
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
                                ARRAY_1D SSBA/* npop */)
{
  int cf;
  int cr;
  int cs;

  for (cf = 1 ; cf <= nfleets ; cf++)
  {
    for (cr = 1 ; cr <= nareas ; cr++)
    {
      for (cs = 1 ; cs <= nsubyears ; cs++)
      {
        EforYear[cf][cr][cs] = totF * ECurrent[cf][cr][cs];
      }
    }
  }

  popdyn_year(qy,
              R0,
              M,
              mat,
              Len_age,
              Wt_age,
              sel,
              EforYear,
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
              SSBA,
              0);
}

// ----------------------------------------------------------------------------

void OperatingModelBase::popdyn_projection_par(const ARRAY_1D par/* 0:npar-1 */,
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
                                               int bIgnoreLast)
{
  int     cr;
  int     cf;
  int     cs;
  int     cx;

  for (cr = 1 ; cr <= nareas ; cr++)
  {
    // Set effort series for year
    for (cx = 0 ; cx < npar ; cx++)
    {
      cf = FbyPar[cx];

      for (cs = 1 ; cs <= nsubyears ; cs++)
      {
        EforYear[cf][cr][cs] = exp(par[cx]) * ECurrent[cf][cr][cs];
      }
    }

    for (cx = 0 ; cx < nfixed ; cx++)
    {
      cf = FbyFixed[cx];

      for (cs = 1 ; cs <= nsubyears ; cs++)
      {
        EforYear[cf][cr][cs] = TAE[cx] * ECurrent[cf][cr][cs];
      }
    }
  }

  // Initialise N and SSN
  nextYear(N,
           NBefore);

  popdyn_year(qy,
              R0,
              M,
              mat,
              Len_age,
              Wt_age,
              sel,
              EforYear,
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
              SSBA,
              bIgnoreLast);
}

// ----------------------------------------------------------------------------

void OperatingModelBase::popdyn_MSY_par(const double par,
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
                                        const int run_years)
{
  int     cy;
  int     cf;
  int     cr;
  int     cs;
  double  totF;

  totF  = exp(par);

  popdyn_init(R0,
              mat,
              Idist,
              N,
              NBefore,
              SSN);

  for (cf = 1 ; cf <= nfleets ; cf++)
  {
    for (cr = 1 ; cr <= nareas ; cr++)
    {
      for (cs = 1 ; cs <= nsubyears ; cs++)
      {
        EforYear[cf][cr][cs] = totF * ECurrent[cf][cr][cs];
      }
    }
  }

  for (cy = 1 ; cy < run_years ; cy++)
  {
    popdyn_year(qy,
                R0,
                M,
                mat,
                Len_age,
                Wt_age,
                sel,
                EforYear,
                mov,
                h,
                Recdist,
                MSY_Recdevs,
                MSY_RecSpatialDevs,
                SRrel,
                N,
                NBefore,
                SSN,
                C,
                SSBA,
                0);

    nextYear(N,
             NBefore);
  }

  popdyn_year(qy,
              R0,
              M,
              mat,
              Len_age,
              Wt_age,
              sel,
              EforYear,
              mov,
              h,
              Recdist,
              MSY_Recdevs,
              MSY_RecSpatialDevs,
              SRrel,
              N,
              NBefore,
              SSN,
              C,
              SSBA,
              0);
}

// ----------------------------------------------------------------------------

OperatingModelBase::OperatingModelBase(
#include "include/OmB_constructor_args.hpp"
)
 : AdtArrays()
{
  int cn;

  #include "include/OmB_constructor_locals.hpp"
  #include "include/OmB_constructor_scalars_phase_1.hpp"
  #include "include/OmB_constructor_arrays_phase_1.hpp"

  // Initialise SpawnPerYr based on number of spawning events in Recsubyr
  SpawnPerYr = 0;

  for (cn = 1 ; cn <= nsubyears ; cn++)
  {
    if (Recsubyr[cn] != 0)
    {
      SpawnPerYr++;
    }
  }

  #include "include/OmB_constructor_scalars_phase_2.hpp"
  #include "include/OmB_constructor_arrays_phase_2.hpp"
  #include "include/OmB_array_plans_init.hpp"

  set_all(MSY_Recdevs, 1.0);
  set_all(MSY_RecSpatialDevs, 1.0);
}

// ----------------------------------------------------------------------------

OperatingModelBase::~OperatingModelBase()
{

}

// ----------------------------------------------------------------------------

void OperatingModelBase::initialiseParameters(const ARRAY_2D M/* nages, npop */,
                                              const ARRAY_1D R0/* npop */,
                                              const ARRAY_2D mat/* nages, npop */,
                                              const ARRAY_3D Idist/* nareas, nages, npop */,
                                              const ARRAY_2D Wt_age/* nages, npop */,
                                              const ARRAY_1D h/* npop */)
{
  int    ca;
  int    cp;
  int    cr;
  double dN;
  double dB;
  double dSSB;

  // Work out survivorship
  // surv=t(exp(-cumsum(c(0,M[,1:(nages-1),1])/nsubyears)))
  for (cp = 1 ; cp <= npop ; cp++)
  {
    surv[1][cp] = 0.0;

    for (ca = 2 ; ca <= nages ; ca++)
    {
      surv[ca][cp] = surv[ca - 1][cp] + M[ca - 1][cp];
    }
  }

  for (cp = 1 ; cp <= npop ; cp++)
  {
    for (ca = 1 ; ca < nages ; ca++)
    {
      surv[ca][cp] = exp(-surv[ca][cp] / nsubyears);
    }

    // infinite sum for plus group
    // surv[,nages] <- surv[,nages-1]*exp(-Madvanced[,nages]/nsubyears)/(1-exp(-Madvanced[,nages]/nsubyears))
    surv[nages][cp] = surv[nages - 1][cp] * exp(-M[nages][cp] / nsubyears) / (1.0 - exp(-M[nages][cp] / nsubyears));
  }

  // Work out SSB0, B0, SSBpR, aR, bR
  for (cp = 1 ; cp <= npop ; cp++)
  {
    SSB0[cp] = 0.0;
    B0[cp]   = 0.0;

    for (ca = 1 ; ca <= nages ; ca++)
    {
      for (cr = 1 ; cr <= nareas ; cr++)
      {
        dN    = surv[ca][cp] * R0[cp] * Idist[cr][ca][cp];
        dB    = dN * Wt_age[ca][cp];
        dSSB  = dB * mat[ca][cp];

        B0[cp]   += dB;
        SSB0[cp] += dSSB;
      }
    }

    SSBpR[cp] = SSB0[cp] / R0[cp];
    bR[cp]    = log(5.0 * h[cp]) / (0.8 * SSB0[cp]);
    aR[cp]    = exp(bR[cp] * SSB0[cp]) / SSBpR[cp];
  }
}

// ----------------------------------------------------------------------------

void OperatingModelBase::nextYear(ARRAY_4D N/* nareas, nsubyears + 1, nages, npop */,
                                  ARRAY_4D NBefore/* nareas, nsubyears + 1, nages, npop */)
{
  int cr;
  int ca;
  int cp;

  for (cr = 1 ; cr <= nareas ; cr++)
  {
    for (ca = 1 ; ca <= nages ; ca++)
    {
      for (cp = 1 ; cp <= npop ; cp++)
      {
        N[cr][1][ca][cp]       = N[cr][nsubyears + 1][ca][cp];
        NBefore[cr][1][ca][cp] = NBefore[cr][nsubyears + 1][ca][cp];
      }
    }
  }
}

// ----------------------------------------------------------------------------

double OperatingModelBase::popdyn_projection_objective(const ARRAY_1D par/* 0:npar-1 */,
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
                                                       ARRAY_1D SSBA/* npop */)
{
  int     cs;
  int     cf;
  int     cr;
  int     ca;
  int     cp;
  int     cx;
  double  dTACError;
  double  dObjective;
  double  dBiomass;
  double  dCatchBiomass;

  popdyn_projection_par(par,
                        npar,
                        nfixed,
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
                        SSBA,
                        1);

  dObjective = 0.0;

  for (cx = 0 ; cx < npar ; cx++)
  {
    dBiomass      = 0.0;
    dCatchBiomass = 1.0e-6;
    cf            = FbyPar[cx];

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
        }
      }
    }

    dTACError     = log(TAC[cx] / dCatchBiomass);
    dObjective   += (dTACError * dTACError);
  }

  return (dObjective);
}

// ----------------------------------------------------------------------------

void OperatingModelBase::runProjection(const ARRAY_1D par/* 0:npar-1 */,
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
                                       ARRAY_1D SSBA/* npop */)
{
  popdyn_projection_par(par,
                        npar,
                        nfixed,
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
                        SSBA,
                        0);
}

// ----------------------------------------------------------------------------

double OperatingModelBase::MSYrefs_objective(const double par,
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
                                             const int run_years)
{
  int     p;
  int     cr;
  int     cf;
  int     cm;
  int     ca;
  int     cp;
  double  dCatch;
  double  dObjective;

  popdyn_MSY_par(par,
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
                 run_years);

  // return(-log(sum(
  // array(C[targpop,,nyears,,,],c(length(targpop),nages,nsubyears,nareas,nfleets))*
  // array(Wt_age[targpop,,nyears],c(length(targpop),nages,nsubyears,nareas,nfleets)))))
  // Use a small number to stop the function returning +inf for objective
  dCatch = 1.0e-6;

  for (cp = 1 ; cp <= ntargets ; cp++)
  {
    p = targpop[cp];

    for (cr = 1 ; cr <= nareas ; cr++)
    {
      for (ca = 1 ; ca <= nages ; ca++)
      {
        for (cf = 1 ; cf <= nfleets ; cf++)
        {
          for (cm = 1 ; cm <= nsubyears ; cm++)
          {
            dCatch += C[cf][cr][cm][ca][p] * Wt_age[ca][p];
          }
        }
      }
    }
  }

  dObjective = -log(dCatch);

  #ifndef AD
  if (nReport != 0)
  {
    Rprintf("par=%g, C=%g, objective=%g\n", par, dCatch, dObjective);
  }
  #endif

  return (dObjective);
}

// ----------------------------------------------------------------------------

void OperatingModelBase::MSYrefs(const double par,
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
                                 double& SSBMSY_B0)
{
  int     p;
  int     cr;
  int     cf;
  int     cm;
  int     ca;
  int     cp;
  double  dMSY;
  double  dBMSY;
  double  dSSBMSY;
  double  dSSB0;
  double  dBiomass;

  popdyn_MSY_par(par,
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
                 run_years);

  // MSY<-sum(array(C[targpop,,nyears,,,],c(length(targpop),nages,nsubyears,nareas,nfleets))*
  //          array(Wt_age[targpop,,nyears],c(length(targpop),nages,nsubyears,nareas,nfleets)))
  // BMSY<-sum(
  //   array((SSN[targpop,,nyears,1,]+NSN[targpop,,nyears,1,]),c(length(targpop),nages,nareas))*
  //   array(Wt_age[targpop,,nyears],c(length(targpop),nages,nareas)))
  //
  // sof<-apply(E[,MSYyear]*sel*qs*4,2,sum)
  // FMSYa<-max(sof)
  // sof<-sof/max(sof)
  // VBMSY<-sum(array(rep(sof,each=length(targpop)),c(length(targpop),nages,nareas))*
  //   array((SSN[targpop,,nyears,1,]+NSN[targpop,,nyears,1,]),c(length(targpop),nages,nareas))*
  //     array(Wt_age[targpop,,nyears],c(length(targpop),nages,nareas)))
  //
  // SSBMSY<-sum(array(SSN[targpop,,nyears,1,],c(length(targpop),nages,nareas))*array(Wt_age[targpop,,nyears],c(length(targpop),nages,nareas)))
  // UMSY<-MSY/VBMSY
  // SSBMSY_B0<-SSBMSY/sum(SSB0[targpop])
  // return(c(MSY,BMSY,VBMSY,SSBMSY,UMSY,FMSYa,SSBMSY_B0))
  dMSY        = 0.0;
  dBMSY       = 0.0;
  dSSBMSY     = 0.0;
  dSSB0       = 0.0;

  for (cp = 1 ; cp <= ntargets ; cp++)
  {
    p = targpop[cp];

    for (cr = 1 ; cr <= nareas ; cr++)
    {
      for (ca = 1 ; ca <= nages ; ca++)
      {
        for (cf = 1 ; cf <= nfleets ; cf++)
        {
          for (cm = 1 ; cm <= nsubyears ; cm++)
          {
            dMSY += C[cf][cr][cm][ca][p] * Wt_age[ca][p];
          }
        }

        dBiomass = N[cr][1][ca][p] * Wt_age[ca][p];

        dBMSY   += dBiomass;
        dSSBMSY += SSN[cr][1][ca][p] * Wt_age[ca][p];
      }
    }

    dSSB0 += SSB0[p];
  }

  MSY       = dMSY;
  BMSY      = dBMSY;
  SSBMSY    = dSSBMSY;
  SSBMSY_B0 = dSSBMSY / dSSB0;
}

// ----------------------------------------------------------------------------

void OperatingModelBase::runHistoric(double totF,
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
                                     ARRAY_1D SSBA/* npop */)
{
  #ifndef AD

  popdyn(totF,
         qy,
         ECurrent,
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

  #endif
}
