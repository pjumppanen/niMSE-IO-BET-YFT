#include <debugbreak.h>
#include <TMB.hpp>


// Iterative solution of p as a function of K,r and MSY
// Could exit conditionally based on diff but as this 
// is code dependent on parameters and conditionals are
// properly handled in AD I've left it as a fixed number
// of iterations. 
//
// This comes from the fact that,
//
// r = (MSY / K) * (1 + p)^(1 + (1 / p))
//
// and if setting alpha = 1 / p it becomes,
//
// r = (MSY / K) * (1 + (1 / alpha))^(1 + alpha)
//
// Now putting the exponent component on one side and taking 
// the log we find,
//
// log(r * K / MSY) = (1 + alpha) * log(1 + (1 / alpha))
//
// Let,
//
// V = log(r * K / MSY)
//
// We can solve for alpha by saying that,
//
// Vestimate = (1 + alpha_estimate) * log(1 + (1 / alpha_estimate))
//
// and recursing alpha_estimate with,
//
// Alpha_next_estimate = alpha_estimate * (Vestimate / V)
//
// This recurrence relationship appears to be stable and converges
// to the true solution. Hence the code shown below.
//
// Left this for historical purpose of know how to estimate
// p for given other parameters even though I am not using it cos
// it doesn't seem to help in fitting.
template<class Type>
Type find_p(Type K, Type r, Type MSY)
{
  Type Vest;
  Type V     = log((r * K) / MSY);
  Type alpha = 1.0;

  for (int cx = 0 ; cx < 80 ; cx++)
  {
    Vest  = (1.0 + alpha) * (log((1.0 + alpha) / alpha));
    alpha = alpha * Vest / V;
  }

  return (1.0 / alpha);
}

//-----------------------------------------------------------------------------
// This is a simplified model that assumes catch is reported largely without 
// error, but uses a saturation mechanism to ensure projected B never goes
// negative, and supplies a Catch sigma to penalize the downgrading of Catch
// to maintain positive B. 
//-----------------------------------------------------------------------------
// objective function
// objective_function<double>::operator()
//-----------------------------------------------------------------------------
template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data
  DATA_VECTOR(I_t);
  DATA_VECTOR(C_t);

  // Priors
  DATA_SCALAR(prior_p_mean);
  DATA_SCALAR(prior_p_sigma);
  DATA_SCALAR(prior_logMSY_mean);
  DATA_SCALAR(prior_logMSY_sigma);
  DATA_SCALAR(prior_logK_mean);
  DATA_SCALAR(prior_logK_sigma);
  DATA_SCALAR(prior_logq_mean);
  DATA_SCALAR(prior_logq_sigma);
  DATA_SCALAR(prior_logSigmaI_mean);
  DATA_SCALAR(prior_logSigmaI_sigma);
  DATA_SCALAR(sigmaC);
  DATA_SCALAR(sigmaEonI);
 
  // Parameters
  PARAMETER(log_KonMSY);
  PARAMETER(log_MSY);
  PARAMETER(log_p_n);
  PARAMETER(log_n);
  PARAMETER(log_sigmaI);
  PARAMETER(log_sigmaRonI);
  PARAMETER_VECTOR(Eps_t);

  // Derived quantities
  int t;
  int n_years = I_t.size();
  
  Type nll        = 0.0;
  Type KonMSY     = (1.0 + exp(log_KonMSY)); // Note that K should be > MSY always!
  Type MSY        = exp(log_MSY);
  Type K          = MSY * KonMSY;
  Type p          = (100.0 + exp(log_p_n)) / (1.0 + 100.0 * exp(log_p_n)); // Limits p to a range from 0.01 to 100.0
  Type B0         = K * 1.0 / (1.0 + exp(log_n)); // Note that B0 should be <= K always!
  Type r          = pow((1.0 + p), 1.0 + (1.0 / p)) * MSY / (p * K);
  Type BMSYonMSY  = (p + 1.0) / r;
  Type BMSY       = MSY * BMSYonMSY;
  Type sigmaI     = exp(log_sigmaI);
  Type sigmaRonI  = exp(log_sigmaRonI);
  Type sigmaR     = sigmaRonI * sigmaI;
  Type sigmaE     = sigmaEonI * sigmaI;
  Type meanC      = 0;
  Type minSigma   = 1.0e-6; // To prevent the pdf from becoming INF if optimiser tries zero sigma
  Type sumLIdivB  = 0.0;
  int  sumCount   = 0;
  Type q;
  Type Bsurvive;
  Type Bdie;
  
  vector<Type> B_t(n_years + 1);
  vector<Type> E_t(n_years);
  vector<Type> Cpred_t(n_years);
  vector<Type> Ipred_t(n_years);
  
  B_t(0) = B0;

  for (int t = 1 ; t < n_years + 1 ; t++)
  {
    B_t(t - 1) *= exp(Eps_t(t - 1));

    if (!CppAD::isnan(I_t(t - 1)))
    {
      sumLIdivB += log(I_t(t - 1) / B_t(t - 1));
      sumCount++;
    }

    Bsurvive = B_t(t - 1) + r * B_t(t - 1) - r * B_t(t - 1) * pow((B_t(t - 1) / K), p);
    Bdie     = 0.0;

    if (CppAD::isnan(C_t(t - 1)))
    {
      // Setting Cpred_t and E_t to nan
      Cpred_t(t - 1) = C_t(t - 1);
      E_t(t - 1)     = C_t(t - 1);
    }
    else
    {
      Type dlim;

      // This saturation mechanism isused to ensure that 
      // B can never go negative.
      Bdie     = C_t(t - 1);
      dlim     = 2.0 / (1.0 + exp(pow(Bdie / Bsurvive, 5.0))); // saturation mechanism that forces B to always be positive
      Bdie    *= dlim;

      Cpred_t(t - 1) = Bdie;
      E_t(t - 1)     = Cpred_t(t - 1) / B_t(t - 1);
      meanC         += C_t(t - 1);
    }

    B_t(t) = (Bsurvive - Bdie);
  }

  // Fix q based on exact fitting B~I relationship
  q = exp(sumLIdivB / sumCount);

  // Find average C value to use in absence of catch data for likelihood 
  meanC /= sumCount;

  // Penalty on priors
  if (!CppAD::isnan(prior_p_mean))
  {
    nll -= dnorm(p, prior_p_mean, prior_p_sigma, true);
  }

  if (!CppAD::isnan(prior_logMSY_mean))
  {
    nll -= dnorm(log_MSY, prior_logMSY_mean, prior_logMSY_sigma, true);
  }

  if (!CppAD::isnan(prior_logK_mean))
  {
    nll -= dnorm(log(K), prior_logK_mean, prior_logK_sigma, true);
  }

  if (!CppAD::isnan(prior_logq_mean))
  {
    nll -= dnorm(log(q), prior_logq_mean, prior_logq_sigma, true);
  }

  if (!CppAD::isnan(prior_logSigmaI_mean))
  {
    nll -= dnorm(log_sigmaI, prior_logSigmaI_mean, prior_logSigmaI_sigma, true);
  }

  for (int t = 0 ; t < n_years ; t++)
  {
    bool bHasCatch = !CppAD::isnan(C_t(t));
    bool bHasCPUE  = !CppAD::isnan(I_t(t));

    // Determine I prediction based on predicted B and q
    Ipred_t(t) = B_t(t) * q;

    // Penality for productivity process noise
    nll -= dnorm(Eps_t(t), Type(0.0), sigmaR + minSigma, true);
    
    // Penalty for effort series fit
    if (bHasCatch && bHasCPUE)
    {
      nll -= dnorm(log(E_t(t)), log(q * C_t(t) / I_t(t)), sigmaE + minSigma, true);
    }
    else
    {
      nll -= dnorm(log(E_t(t)), log(E_t(t)), sigmaE + minSigma, true);
    }

    // Penalty on catch series
    if (bHasCatch)
    {
      nll -= dnorm(log(C_t(t)), log(Cpred_t(t)), sigmaC + minSigma, true);
    }
    else
    {
      nll -= dnorm(log(meanC), log(meanC), sigmaC + minSigma, true);
    }

    // Penalty on CPUE series
    if (bHasCPUE)
    {
      nll -= dnorm(log(I_t(t)), log(Ipred_t(t)), sigmaI + minSigma, true);
    }
    else
    {
      nll -= dnorm(log(Ipred_t(t)), log(Ipred_t(t)), sigmaI + minSigma, true);
    }
#ifdef DEBUG
    if (CppAD::isnan(nll))
    {
      debug_break();
    }
#endif
  }

  // report log likelihood
  ADREPORT(nll);

  // Report predictions
  REPORT(B_t);
  REPORT(Ipred_t);
  REPORT(Cpred_t);
  REPORT(Eps_t);
  REPORT(E_t);

  // Report parameters
  REPORT(MSY);
  REPORT(BMSY);
  REPORT(B0);
  REPORT(K);
  REPORT(r);
  REPORT(p);
  REPORT(q);
  REPORT(sigmaI);
  REPORT(sigmaR);
  REPORT(sigmaE);

  REPORT(log_KonMSY);
  REPORT(log_MSY);
  REPORT(log_p_n);
  REPORT(log_n);
  REPORT(log_sigmaI);
  REPORT(log_sigmaRonI);

  return (nll);
}
