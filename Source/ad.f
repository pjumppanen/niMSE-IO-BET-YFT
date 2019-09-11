MODULE COMMON

 INTEGER, PARAMETER :: dim_stack

 INTEGER Y
 REAL C_hist(Y), I_hist(Y), weight(Y)

END


SUBROUTINE PT(params, result)

  USE COMMON

  REAL, INTENT (IN) :: params(4)
  REAL, INTENT (OUT) :: result

  REAL B(Y), q, r, K, p, lim, n, MSY, L, Bsurvive, Bdie, dlim, devs(Y)
  INTEGER yi

  K     = EXP(params(1))
  MSY   = EXP(params(2))
  p     = EXP(params(3))
  n     = EXP(params(4))
  L     = MSY * ((p + 1.0) ** (1.0 + (1.0 / p)))
  r     = L / K
  B(1)  = n * K

  DO yi=2,Y
    Bsurvive = B(yi-1) + (r / p) * B(yi-1)
    Bdie     = (r / p) * B(yi-1) * ((B(yi-1) / K) ** p) + C_hist(yi-1)
    dlim     = 2.0 / (1 + exp((Bdie / Bsurvive) ** 5))
    B(yi)    = Bsurvive - (dlim * Bdie)
  END DO

  devs = log(I_hist) - log(B)
  devs = devs - (sum(devs) / Y)

  result = sum((weight * devs) ** 2)
END

