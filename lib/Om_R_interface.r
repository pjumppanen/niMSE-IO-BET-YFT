require(Oarray)

Om.destroy <- function(Context)
{
  return (.External('_Om.destroy', Context))
}

Om.create <- function(npop, nages, nsubyears, nareas, nfleets, Recsubyr)
{
  return (.External('_Om.create', as.integer(npop), as.integer(nages), as.integer(nsubyears), as.integer(nareas), as.integer(nfleets), as.integer(Recsubyr)))
}

Om.beginProjection <- function(Context, pPar)
{
  return (.External('_Om.beginProjection', Context, pPar))
}

Om.nt.beginProjection <- function(Context, pPar)
{
  return (.External('_Om.nt.beginProjection', Context, pPar))
}

Om.get.cd1_par <- function(Context, ...)
{
  return (.External('_Om.get.cd1_par', Context, list(...)))
}

Om.set.cd1_par <- function(Context, Arg, ...)
{
  return (.External('_Om.set.cd1_par', Context, as.double(Arg), list(...)))
}

Om.get.nt.cd1_par <- function(Context, ...)
{
  return (.External('_Om.get.nt.cd1_par', Context, list(...)))
}

Om.set.nt.cd1_par <- function(Context, Arg, ...)
{
  return (.External('_Om.set.nt.cd1_par', Context, as.double(Arg), list(...)))
}

Om.findMSYrefs <- function(Context, nReport, ECurrent, qy, R0, M, mat, Idist, Len_age, Wt_age, sel, mov, h, Recdist, SRrel, N, NBefore, SSN, C, SSBA, ntargets, targpop, run_years, MinPar, MSY, BMSY, SSBMSY, SSBMSY_B0, maxit)
{
  return (.External('_Om.findMSYrefs', Context, nReport, ECurrent, qy, R0, M, mat, Idist, Len_age, Wt_age, sel, mov, h, Recdist, SRrel, N, NBefore, SSN, C, SSBA, ntargets, targpop, run_years, MinPar, MSY, BMSY, SSBMSY, SSBMSY_B0, maxit))
}

Om.nt.findMSYrefs <- function(Context, nReport, ECurrent, qy, R0, M, mat, Idist, Len_age, Wt_age, sel, mov, h, Recdist, SRrel, N, NBefore, SSN, C, SSBA, ntargets, targpop, run_years, MinPar, MSY, BMSY, SSBMSY, SSBMSY_B0, maxit)
{
  return (.External('_Om.nt.findMSYrefs', Context, nReport, ECurrent, qy, R0, M, mat, Idist, Len_age, Wt_age, sel, mov, h, Recdist, SRrel, N, NBefore, SSN, C, SSBA, ntargets, targpop, run_years, MinPar, MSY, BMSY, SSBMSY, SSBMSY_B0, maxit))
}

Om.get.last_par <- function(Context, ...)
{
  return (.External('_Om.get.last_par', Context, list(...)))
}

Om.set.last_par <- function(Context, Arg, ...)
{
  return (.External('_Om.set.last_par', Context, as.double(Arg), list(...)))
}

Om.get.nt.last_par <- function(Context, ...)
{
  return (.External('_Om.get.nt.last_par', Context, list(...)))
}

Om.set.nt.last_par <- function(Context, Arg, ...)
{
  return (.External('_Om.set.nt.last_par', Context, as.double(Arg), list(...)))
}

Om.get.LastEbyF <- function(Context, ...)
{
  return (.External('_Om.get.LastEbyF', Context, list(...)))
}

Om.set.LastEbyF <- function(Context, Arg, ...)
{
  return (.External('_Om.set.LastEbyF', Context, as.double(Arg), list(...)))
}

Om.get.nt.LastEbyF <- function(Context, ...)
{
  return (.External('_Om.get.nt.LastEbyF', Context, list(...)))
}

Om.set.nt.LastEbyF <- function(Context, Arg, ...)
{
  return (.External('_Om.set.nt.LastEbyF', Context, as.double(Arg), list(...)))
}

Om.get.nbefored1_par <- function(Context, ...)
{
  return (.External('_Om.get.nbefored1_par', Context, list(...)))
}

Om.set.nbefored1_par <- function(Context, Arg, ...)
{
  return (.External('_Om.set.nbefored1_par', Context, as.double(Arg), list(...)))
}

Om.get.nt.nbefored1_par <- function(Context, ...)
{
  return (.External('_Om.get.nt.nbefored1_par', Context, list(...)))
}

Om.set.nt.nbefored1_par <- function(Context, Arg, ...)
{
  return (.External('_Om.set.nt.nbefored1_par', Context, as.double(Arg), list(...)))
}

Om.get.nd1_par <- function(Context, ...)
{
  return (.External('_Om.get.nd1_par', Context, list(...)))
}

Om.set.nd1_par <- function(Context, Arg, ...)
{
  return (.External('_Om.set.nd1_par', Context, as.double(Arg), list(...)))
}

Om.get.nt.nd1_par <- function(Context, ...)
{
  return (.External('_Om.get.nt.nd1_par', Context, list(...)))
}

Om.set.nt.nd1_par <- function(Context, Arg, ...)
{
  return (.External('_Om.set.nt.nd1_par', Context, as.double(Arg), list(...)))
}

Om.projection <- function(Context, nProjectionYear, nReport, dEffortCeiling, dTAC, TAEbyF, TACEError, ECurrent, CMCurrent, qy, R0, M, mat, Idist, Len_age, Wt_age, Wt_age_mid, sel, mov, h, Recdist, Recdevs, RecSpatialDevs, SRrel, N, NBefore, SSN, C, SSBA, maxit)
{
  return (.External('_Om.projection', Context, nProjectionYear, nReport, dEffortCeiling, dTAC, TAEbyF, TACEError, ECurrent, CMCurrent, qy, R0, M, mat, Idist, Len_age, Wt_age, Wt_age_mid, sel, mov, h, Recdist, Recdevs, RecSpatialDevs, SRrel, N, NBefore, SSN, C, SSBA, maxit))
}

Om.nt.projection <- function(Context, nProjectionYear, nReport, dEffortCeiling, dTAC, TAEbyF, TACEError, ECurrent, CMCurrent, qy, R0, M, mat, Idist, Len_age, Wt_age, Wt_age_mid, sel, mov, h, Recdist, Recdevs, RecSpatialDevs, SRrel, N, NBefore, SSN, C, SSBA, maxit)
{
  return (.External('_Om.nt.projection', Context, nProjectionYear, nReport, dEffortCeiling, dTAC, TAEbyF, TACEError, ECurrent, CMCurrent, qy, R0, M, mat, Idist, Len_age, Wt_age, Wt_age_mid, sel, mov, h, Recdist, Recdevs, RecSpatialDevs, SRrel, N, NBefore, SSN, C, SSBA, maxit))
}

Om.get.ssnd1_par <- function(Context, ...)
{
  return (.External('_Om.get.ssnd1_par', Context, list(...)))
}

Om.set.ssnd1_par <- function(Context, Arg, ...)
{
  return (.External('_Om.set.ssnd1_par', Context, as.double(Arg), list(...)))
}

Om.get.nt.ssnd1_par <- function(Context, ...)
{
  return (.External('_Om.get.nt.ssnd1_par', Context, list(...)))
}

Om.set.nt.ssnd1_par <- function(Context, Arg, ...)
{
  return (.External('_Om.set.nt.ssnd1_par', Context, as.double(Arg), list(...)))
}

Om.get.TACbyF <- function(Context, ...)
{
  return (.External('_Om.get.TACbyF', Context, list(...)))
}

Om.set.TACbyF <- function(Context, Arg, ...)
{
  return (.External('_Om.set.TACbyF', Context, as.double(Arg), list(...)))
}

Om.get.nt.TACbyF <- function(Context, ...)
{
  return (.External('_Om.get.nt.TACbyF', Context, list(...)))
}

Om.set.nt.TACbyF <- function(Context, Arg, ...)
{
  return (.External('_Om.set.nt.TACbyF', Context, as.double(Arg), list(...)))
}

