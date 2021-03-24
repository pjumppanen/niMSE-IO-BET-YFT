// Pella Tomlinson production model
// derived from 

// Space time
#include <TMB.hpp>

// square
template<class Type>
Type square(Type x){
  return pow(x,2.0); 
}

// sqrt
template<class Type>
Type sqrt(Type x){
  return pow(x,0.5); 
}



// objective function
template<class Type>
Type objective_function<Type>::operator() ()
{
  // Data
  DATA_VECTOR( I_t ); // CPUE               
  DATA_VECTOR( c_t );                
  //DATA_VECTOR( ysd_t );
  //DATA_VECTOR( penalties_z );
  DATA_VECTOR( priorMode );
  DATA_VECTOR( priorLogCV );
  DATA_VECTOR( log_kBounds );
  DATA_VECTOR( newTACSwitch );
  DATA_SCALAR( BoB0Targ );
  DATA_SCALAR( Depletion_Y );
  DATA_INTEGER(nProjYears);
  
  // Parameters
  PARAMETER( log_MSY );
  PARAMETER( log_k );
  //PARAMETER( log_q );
  PARAMETER( shape );
  PARAMETER( log_sigmaP );
  PARAMETER( log_sigmaI );
  //PARAMETER( log_sigmac );
  PARAMETER_VECTOR( log_B_t );  //Biomass with the productivity deviation
  PARAMETER( log_newTAC );

  //PARAMETER_VECTOR( logit_exploit_t );
  
  // Derived quantities
  int n_years = I_t.size();

  //Type r = exp(log_r);
  //  MSY <- r*K/((p+1)^(1/p))
  
  Type k = exp(log_k); //
  Type newTAC = exp(log_newTAC); // TAC for projections
  //Bound test - should not be hard-coded
  //Type log_klb = log_MSY + log(Type(5));  //k lower bound - probably always irrelevant
  //Type log_kub = log_MSY + log(Type(50)); //k upper bound - dubiously high, but unlikely to make a difference to MP if hit 
  //Type log_kTmp = exp(0.1*log_k)/(1+exp(0.1*log_k))*(log_kub-log_klb)+log_klb;
  //Type k = exp(log_kTmp);
  //k Bounded with logit - also not as robust as hoped, unless maybe k too tightly bound
  //Type ilogit_log_kTmp = exp(0.1*log_k)/(1+exp(0.1*log_k))*(log_kBounds(1)-log_kBounds(0))+log_kBounds(0);
  //Type k = exp(ilogit_log_kTmp);

  Type r = exp(log_MSY)*pow((shape+1),(1/shape))/k;

  //Type q = exp(log_q);
  vector<Type> Bpred_t( n_years ); //Biomass without the productivity deviation
  vector<Type> recDev( n_years ); //Biomass without the productivity deviation
  //vector<Type> cpred_t( n_years );
  //vector<Type> sigmaI_t( n_years );
  //vector<Type> exploit_t = invlogit( logit_exploit_t );
  vector<Type> B_t = exp( log_B_t );
  vector<Type> Depletion_t = B_t / k;
  //cpred_t.setZero();
  
  //int nProj = 10; // change to dynamic input Arg   
  int nProj = nProjYears; // dynamic input Arg   
  vector<Type> BProj_t( nProj ); //projection biomass  
    
  
  // Objective function
  vector<Type> nll_comp(6);
  nll_comp.setZero();

  recDev(0) = 0.;
  // Reconstruct time series
  //let N go regative and have negative productivity
  //cpred_t(0) = x_t(0);
  //for( int t=1; t<n_years      +2       ; t++){ //insert array bound err
  Bpred_t(0) = k;
  B_t(0) = k;  
  
  for( int t=1; t<n_years; t++){
    //cpred_t(t-1) = c_t(t-1);
    //xpred_t(t) = (x_t(t-1)-cpred_t(t-1)) * (1.0 + r*(1-(x_t(t-1)-cpred_t(t-1))/k));

    //xpred_t(t) = (x_t(t-1)-c_t(t-1)) * (1.0 + r*(1-(x_t(t-1)-c_t(t-1))/k));

    // not sure that the abs() is useful, but does not seem to cause a problem 
    //xpred_t[t]   = x_t[t-1]  + ((shape+1)/shape)*r*x_t[t-1]*(1-pow((x_t[t-1]/k),shape)) - c_t(t-1);
    Bpred_t[t]   = B_t[t-1]  + ((shape+1)/shape)*r*B_t[t-1]*(1-pow(sqrt(square((B_t[t-1]/k))),shape)) - c_t(t-1);
    recDev(t) = log(Bpred_t(t)/B_t(t));
    // PT_v1
    //jnll_comp(0) -= dnorm( x_t(t), xpred_t(t), xpred_t(t)*exp(log_sigmap), true );
    nll_comp(0) -= dnorm( log(B_t(t)), log(Bpred_t(t)), exp(log_sigmaP), true );

    //std::cerr<<"t"; //this works in Rstudio
  }

  // deterministic projections aim for B(t=nProj)/B0 Target   
  // not necessarily possible which could cause problems
  //Type BoB0Targ = .34;   //now an argument = tuning parameter

  //original projection starting point based on best point estimate
  //BProj_t[0] = B_t[n_years-1]; 

  //new projection starting point based on 
  //current depletion with user-defined uncertainty argument from R     
  BProj_t[0] = B_t[n_years-1]*Depletion_Y/Depletion_t[n_years-1]; 
  
  
  
  for( int tProj=1; tProj<nProj; tProj++){
    BProj_t[tProj]  = BProj_t[tProj-1]  + ((shape+1)/shape)*r*BProj_t[tProj-1]*(1-pow(sqrt(square((BProj_t[tProj-1]/k))),shape)) - newTAC;
    //std::cerr << "\n BProj_t[tProj]/k " << tProj << "   "<< BProj_t[tProj]/k << std::endl; 
    //std::cerr << "\n BoB0Targ      " << BoB0Targ;
  }
  //penalty to hit Target 
  nll_comp(5) += newTACSwitch(0)*10000*square(BProj_t(nProj-1)/k - BoB0Targ);
  //penalty for bounds 
  Type boundWtTAC  = 1.;
  vector<Type> log_TACBounds(2); //projection TAC bounds  
  log_TACBounds(0) = log(10);
  log_TACBounds(1) = log(1000000);
  Type mTAC        = (2*boundWtTAC)   /  (log_TACBounds(1) - log_TACBounds(0));
  Type bTAC        = boundWtTAC - mTAC*log_TACBounds(1);
  Type boundPenTAC = pow(mTAC*log_newTAC + bTAC, 4);  
  nll_comp(5)  += newTACSwitch(1)*boundPenTAC;


  //std::cerr<<"BProj_t(nProj-1)/k "<<BProj_t(nProj-1)/k<<std::endl; 
  //std::cerr<<"BoB0Targ           "<<BoB0Targ<<std::endl; 
  //std::cerr<<"newTAC inside:     "<<newTAC<<std::endl; 
  //std::cerr<<"nll_comp():     "<<nll_comp<<std::endl; 
  
  //calculate analytical q
  //q <- exp(1/sum(!is.na(I_hist))*sum(log(I_hist[!is.na(I_hist)] / BI[!is.na(I_hist)])))
  //Type q = exp(1/sum(y_t)*sum(log(y_t / x_t)));
  //    q        <- exp(mean(LI_hist - LB))
  
  // seems not all TMB functions have vector overloads...
  Type tmpSum=0.;
  Type tmpSumI=0.;
  Type tmpSumB=0.;
  Type nI=0; 
  for( int t=0; t<n_years; t++){
    if(!CppAD::isnan(log(I_t(t)))) nI += 1;
    if(!CppAD::isnan(log(I_t(t)))) tmpSum  += log(I_t(t) / B_t(t));
    if(!CppAD::isnan(log(I_t(t)))) tmpSumI += I_t(t);
    if(!CppAD::isnan(log(I_t(t)))) tmpSumB += B_t(t);
  }  
  Type q     = exp((1/nI) * tmpSum);           //correct version
  //Type q = tmpSumI/tmpSumB; //naive version that works
  

  //  CPUE likelihood
  for( int t=0; t<n_years; t++){
    // PT_v1
    //sigmaI_t(t) = sqrt( exp(2.0*log_sigmaI) + square(ysd_t(t)) );
    //jnll_comp(1) -= dnorm( y_t(t), q*x_t(t), q*x_t(t)*sigmaI_t(t), true );   

    if(!CppAD::isnan(log(I_t(t)))) nll_comp(1) -= dnorm( log(I_t(t)), log(q*B_t(t)), exp(log_sigmaI), true );   

  }                                                                                 
  
  // Penalty on catches
  //for( int t=1; t<n_years; t++){
  //  jnll_comp(2) -= dnorm( c_t(t-1), cpred_t(t-1), cpred_t(t-1)*exp(log_sigmac), true );
  //}
  // conditional for missing values - input standard R NAs
  //for(int y=0;y<ny;y++)
    //Only estimate likelihood when data !is.na
    //if(!CppAD::isnan(catches(a,y))){ nll_c -= dnorm(log(catches(a,y)),log(Chat(a,y)),Csd,true);}
  //}


  // penalties for parameter priors
  //nll_comp(2) log(priorMode(0)), priorLogCV(0) = initial depletion;
  nll_comp(2) -= dnorm( log_MSY, log(priorMode(1)), priorLogCV(1), true );
  //nll_comp(2) -= dnorm( log(k), log(priorMode(2)), priorLogCV(2), true );
  nll_comp(2) -= dnorm( shape, priorMode(3), priorLogCV(3), true );
  nll_comp(2) -= dnorm( log_sigmaI, log(priorMode(4)), priorLogCV(4), true );
  nll_comp(2) -= dnorm( log_sigmaP, log(priorMode(5)), priorLogCV(5), true );

  // Penalty on starting biomass
  nll_comp(3) -= dnorm( log(Depletion_t(0)), log(priorMode(0)), priorLogCV(0), true );

  //add bound on k - should only be relevant on ~linear decline below any sensible level
  //this conditional penalty does not work...
      // seems to affect nll as expected,  
      // but fails to influence minimization - 
      // presumably its the gradients that fail with AD par/var conditionals
  //Type boundSigma = 10.;
  //Type kMax = priorMode(1)*25.;
  //Type kMin = priorMode(1)*5.;
  //  if(k > kMax) nll_comp(4) -= dnorm( log(k), log(kMax), boundSigma, true);
  //  if(k < kMin) nll_comp(4) -= dnorm( log(k), log(kMin), boundSigma, true);
  // instead use inverse logit transform lb + (ub-lb)(invlogit x) with a reasonable re-scaling of x
  // alternatively - try nlminb bounds - not very successful
  //Type ilogit_log_kTmp = exp(0.1*log_k)/(1+exp(0.1*log_k))*(log_kBounds(1)-log_kBounds(0))+log_kBounds(0);
  //Type k = exp(ilogit_log_kTmp);
  //xl = CppAD::CondExpGt(lli,llj,Type(1),Type(0));
  //nll_comp(4) -= CppAD::CondExpGt(log_k, log_kBounds(0), square(logk-log_kBounds(0)), Type(0));
  //nll_comp(4) -= CppAD::CondExpGt(log_k, log_kBounds(1), square(log_k-log_kBounds(1)), Type(0));

  //this works, but gradient often fails right on k bound 
  //Type boundPen = dnorm( log(k), log_kBounds(1), boundSigma, true);
  //nll_comp(4) -= CppAD::CondExpGt(log_k, log_kBounds(1), boundPen, Type(0));
  //std::cerr<<"log(k) "<<log(k)<<"     log_k: "<<log_k<<std::endl; 
  //std::cerr<<"log_kBounds(0): "<<log_kBounds(0)<<"    log_kBounds(1): "<<log_kBounds(1)<<std::endl; 
  //std::cerr<<"boundPen "<<boundPen<<"    nll_comp(4): "<<nll_comp(4)<<std::endl; 

  //try a flattened bottom U-shaped penalty function (platykurtic maybe)
  //value of boundWt likelihood units on bounds, rapidly increases    
  Type boundWt  = 1.;
  Type m        = (2*boundWt)/(log_kBounds(1) - log_kBounds(0));
  Type b        = boundWt - m*log_kBounds(1);
  Type boundPen = pow(m*log_k + b, 8);  
  nll_comp(4)  += boundPen;
  //std::cerr<<"log(k) "<<log(k)<<"     log_k: "<<log_k<<std::endl; 
  //std::cerr<<"log_kBounds(0): "<<log_kBounds(0)<<"    log_kBounds(1): "<<log_kBounds(1)<<std::endl; 
  //std::cerr<<"boundPen "<<boundPen<<"    nll_comp(4): "<<nll_comp(4)<<std::endl; 
  
  
  // Total likelihood
  Type nll = nll_comp.sum();

  //if(k > kMax) std::cerr<<"k "<<k<<" kMax: "<<kMax<<" nll: "<<nll<<std::endl<<"        nll_comp "<<nll_comp<<std::endl; 
  
  // Reporting
  REPORT( log_B_t );
  REPORT( n_years );
  //REPORT( cpred_t );
  REPORT( B_t );
  REPORT( Bpred_t ); //not really useful - should convert to dev 
  REPORT( nll_comp );
  REPORT( recDev );
  REPORT( log_MSY );
  REPORT( k );
  REPORT( r );
  REPORT( shape );
  REPORT( q );
  //REPORT( exploit_t );
  REPORT( Depletion_t );
  REPORT( log_sigmaI );
  REPORT( log_sigmaP );
  REPORT( newTAC );
  REPORT(BProj_t); 
  
  ADREPORT( log_B_t );
  ADREPORT( B_t );
  ADREPORT( Depletion_t );
  ADREPORT( recDev );
  
  //std::cerr<<"MSY: " << exp(log_MSY) << "   r: " << r <<"   k "<<k<<std::endl; 

  return nll;
}
