MODULE COMMON

  INTEGER, PARAMETER :: dim_stack

  INTEGER Y
  REAL C_hist(Y), I_hist(Y)

END


REAL FUNCTION PT(params)

  USE COMMON

  REAL, INTENT (IN) :: params(3), C_hist(Y), I_hist(Y), Y

  REAL B(Y), q, r, K, p
  INTEGER yi

  r = EXP(params(1))
  K = EXP(params(2))
  p = EXP(params(3))

  B(1) = K

  DO yi=2,Y
    ! Note that we square B/K to force a positive number so that then
    ! raising to the power of p/2 exists. When B > 0 it is the same as
    ! (B/K)**p but still exists when B is negative. This is just an
    ! aid for fitting purposes.
    B(yi) = B(yi-1) + (r * B(yi-1) / p) * (1.0 - ((B(yi-1) / K)**2)**(p/2)) - C_hist(yi-1)
  END DO

  q  = sum(I_hist) / sum(B)
  PT = sum((q * B - I_hist)**2)

  RETURN

END

