
#Bit of a play with paritally confouneded factorial experimaental design
library('FrF2')     # 2 level designs only - output seems more useful
library('planor')   # can do mixed 2 and 3 level designs, but not sure how to interpret confounding


#####################################################################################
# FrF2

# 7 factor full design
design.info(FrF2(nruns=128, nfactors=7))
FrF2(128,7)

design.info(FrF2(nfactors=7, resolution=3))$aliased  # res 3 - main aliased with 2 way interactions
design.info(FrF2(nfactors=7, resolution=4))$aliased  # res 4 - no main effects aliased with 2 way, but 2 ways aliased
design.info(FrF2(nfactors=7, resolution=5))$aliased  # res 5 - "no aliasing among main effects and 2fis"


FrF2(nfactors=9, resolution=4)   #32
design.info(FrF2(nfactors=9, resolution=4))$aliased

FrF2(nfactors=11, resolution=4)  #32
design.info(FrF2(nfactors=11, resolution=4))$aliased



FrF2(nfactors=9, resolution=5)  #128
design.info(FrF2(nfactors=9, resolution=5))$aliased

FrF2(nfactors=11, resolution=5)  #128
design.info(FrF2(nfactors=11, resolution=5))$aliased

FrF2(nfactors=15, resolution=5)  #256
design.info(FrF2(nfactors=15, resolution=5))$aliased


# 7 factor 1/2 fractional
design.info(FrF2(nruns=64, nfactors=7))
FrF2(64,7)
design.info(FrF2(64,7))$aliased
# [1] "no aliasing among main effects and 2fis"


# 7 factor 1/4 fractional
design.info(FrF2(nruns=32, nfactors=7))
FrF2(32,7)
design.info(FrF2(32,7))$aliased
#$fi2
#[1] "AB=CF" "AC=BF" "AF=BC"


# 7 factor 1/8 fractional  - Interpret this to be the limit for estimating main effects
design.info(FrF2(nruns=16, nfactors=7))
FrF2(16,7)
design.info(FrF2(16,7))$aliased
#$fi2
#[1] "AB=CE=DF" "AC=BE=DG" "AD=BF=CG" "AE=BC=FG" "AF=BD=EG" "AG=CD=EF" "BG=CF=DE"


# 7 factor 1/16 fractional
design.info(FrF2(nruns=8, nfactors=7))
FrF2(8,7)
design.info(FrF2(8,7))$aliased
#$main
#[1] "A=BD=CE=FG" "B=AD=CF=EG" "C=AE=BF=DG" "D=AB=CG=EF" "E=AC=BG=DF"
#[6] "F=AG=BC=DE" "G=AF=BE=CD"



#####################################################################################
# planor
# note from planor help: The nunits argument is compulsory except if the base argument is used. When both arguments
# are missing, the program stops and it gives the size that would be required by a full factorial design.
# When only nunits is missing, the number of units is given by the product of the numbers of levels
# of the base factors.
# The base formula must be an additive formula involving a subset of factors, called the basic factors.
# Using the base argument ensures that the design solutions will include the full factorial design for
# the basic factors. This option can speed up the search because it restricts the domain to be explored
# by the search algorithm.


# two levels (2^n) ouput seems easy enough to interpret
# 7 factors with 2 levels, 2 way interactions
mixKey <- planor.designkey(factors=c( LETTERS[1:7]),
  nlevels=rep(2,7),
  #yields 2^7 run design with 4th or higher order interactions
  model    = ~(A+B+C+D+E+F+G)^4, # main model formula. contains all the non-negligible interaction terms
  #estimate = ~(A+B+C+D+E)^2, # optional formula specifying the factorial terms to estimate.
                                     # If missing, it is considered that all factorial terms in "model" have to be estimated.
  nunits= 2^7 / (1), #full factorial
  #base=~A+B+D, # an optional additive formula to specify the basic factors. see note.
  max.sol=1)

mixPlan <- planor.design(key=mixKey)
mixPlan@design
alias(mixPlan)  #only seems to report "UNALIASED TREATMENT EFFECTS"
summary(mixPlan)
# Regular matrix: no confounding



mixKey <- planor.designkey(factors=c( LETTERS[1:7]),

  # 64 run (of 128 full) solution seems to be required for main effects + 2 way interactions
  nlevels=rep(2,7),
  model    = ~(A+B+C+D+E+F+G)^2,
  nunits= 2^7 / (2),

  max.sol=1)
mixPlan <- planor.design(key=mixKey)
mixPlan@design
alias(mixPlan)
summary(mixPlan)
# WEIGHT PROFILES
# Treatment effects confounded with the mean: 5^1
# Treatment effects confounded with block effects: none
# Treatment pseudo-effects confounded with the mean: 5^1
# Treatment pseudo-effects confounded with block effects: none



mixKey <- planor.designkey(factors=c( LETTERS[1:7]),
  # 16 run (of 128 full) solution seems okay for main effects?
  nlevels=rep(2,7),
  model    = ~(A+B+C+D+E+F+G),
  nunits= 2^7 / (8),

  max.sol=1)

mixPlan <- planor.design(key=mixKey)
mixPlan@design
alias(mixPlan)
summary(mixPlan)
# WEIGHT PROFILES
# Treatment effects confounded with the mean: 3^7 4^7 7^1
# Treatment effects confounded with block effects: none
# Treatment pseudo-effects confounded with the mean: 3^7 4^7 7^1
# Treatment pseudo-effects confounded with block effects: none









#mixed levels

# Nine treatment factors at 3, 3, 2, 2, 2, 2, 2, 2, 2 levels
# Full factorial = 1152 runs
mixKey <- planor.designkey(factors=c( LETTERS[1:9]),
  nlevels=c(3,3,2,2,2,2,2,2,2),
  model    = ~(A+B+C+D+E+F+G+H+I), # main model formula.
  nunits= 3^2 * 2^7 / (1),
  #base=~A+B+D, # an optional additive formula to specify the basic factors. see note.
  max.sol=1)

mixPlan <- planor.design(key=mixKey)
mixPlan@design
alias(mixPlan)
summary(mixPlan)
# produces 2 key matrices (with UNALIASED TREATMENT EFFECTS for the 2 level and 3 level factors respectively)



# Nine treatment factors at 3, 3, 2, 2, 2, 2, 2, 2, 2 levels
# Full factorial = 1152 runs
mixKey <- planor.designkey(factors=c( LETTERS[1:9]),
  nlevels=c(3,3,2,2,2,2,2,2,2),
  model    = ~(A+B+C+D+E+F+G+H+I)^2, # summary function fails with interaction terms if full rns specified
  nunits= 3^2 * 2^7 / (2),  # no solution for x/4 (or x/6) design

  max.sol=1)

mixPlan <- planor.design(key=mixKey)
mixPlan@design
alias(mixPlan)
summary(mixPlan)
# WEIGHT PROFILES
# Treatment effects confounded with the mean: 5^1
# Treatment effects confounded with block effects: none
# Treatment pseudo-effects confounded with the mean: 5^1
# Treatment pseudo-effects confounded with block effects: none





# Nine treatment factors at 3, 3, 2, 2, 2, 2, 2, 2, 2 levels
# Full factorial = 1152 runs
#main effects only  ?  Can seemingly get away with 72 runs ?
mixKey <- planor.designkey(factors=c( LETTERS[1:9]),
  nlevels=c(3,3,2,2,2,2,2,2,2),
  model    = ~(A+B+C+D+E+F+G+H+I), # main effects only

  nunits= 3^2 * 2^7 / (16),
  #base=~A+B+D, # an optional additive formula to specify the basic factors. see note.
  max.sol=1)
alias(mixKey)
alias(mixKey[1])

mixPlan <- planor.design(key=mixKey)
mixPlan@design
summary(mixPlan)
# WEIGHT PROFILES
# Treatment effects confounded with the mean: 3^7 4^7 7^1
# Treatment effects confounded with block effects: none
# Treatment pseudo-effects confounded with the mean: 3^7 4^7 7^1
# Treatment pseudo-effects confounded with block effects: none
alias(mixPlan)
alias(mixPlan[1])



# 11 treatment factors at 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2 levels
# Full factorial = 6912 runs
#main effects only  ?  Can seemingly get away with 144 runs ?
mixKey <- planor.designkey(factors=c( LETTERS[1:11]),
  nlevels=c(3,3,3,2,2,2,2,2,2,2,2),
  model    = ~(A+B+C+D+E+F+G+H+I+J+K), # main effects only

  nunits= 3^3 * 2^8 / (48),
  #base=~A+B+D, # an optional additive formula to specify the basic factors. see note.
  max.sol=1)

mixPlan <- planor.design(key=mixKey)
mixPlan@design
alias(mixPlan)
summary(mixPlan)
# WEIGHT PROFILES
# Treatment effects confounded with the mean: 3^7 4^7 7^1
# Treatment effects confounded with block effects: none
# Treatment pseudo-effects confounded with the mean: 3^7 4^7 7^1
# Treatment pseudo-effects confounded with block effects: none






FrF2(nfactors=5, resolution=5)
design.info(FrF2(nfactors=5, resolution=5))$aliased

#confirm with GLM

# effect relative to low state = no effect (0)
A <- 1
B <- 2
C <- 3
AB <- 0.1
AC <- 0.2
BC <- 0.3
ABC <- 0.01

Y <- c( 0,
        1,
        2,
        3 + AB,
        3,
        4 + AC,
        5 + BC,
        6 + AB+AC+BC+ABC)

x <- FrF2(nfactors=3, resolution=5, randomize=F)

X <- cbind (x$A,x$B,x$C)

m <- lm(Y ~ as.factor(X[,1]) + as.factor(X[,2]) + as.factor(X[,3]))
anova(m)
m$coeff

m <- lm(Y ~ as.factor(X[,1])*as.factor(X[,2]) + as.factor(X[,1])*as.factor(X[,3]) + as.factor(X[,2])*as.factor(X[,3]))
anova(m)
m$coeff

m <- lm(Y ~ as.factor(X[,1])*as.factor(X[,2])*as.factor(X[,3]))
anova(m)
m$coeff





#example
mydesign <- regular.design(factors=c("block", LETTERS[1:4]),
 nlevels=rep(3,5), model=~block+(A+B+C+D)^2, estimate=~A+B+C+D,
 nunits=3^3, randomize=~block/UNITS)
print(mydesign)
# DUMMY ANALYSIS
# Here we omit two-factor interactions from the model, so they are
# confounded with the residuals (but not with ABCD main effects)
set.seed(123)
mydesigndata <- mydesign@design
mydesigndata$Y <- runif(27)
mydesign.aov <- aov(Y ~ block + A + B + C + D, data=mydesigndata)
summary(mydesign.aov)
# 2. STEP-BY-STEP GENERATION, USING planor.designkey
F0 <- planor.factors(factors=c( "block", LETTERS[1:4]), nlevels=rep(3,5),
block=~block)
M0 <- planor.model(model=~block+(A+B+C+D)^2, estimate=~A+B+C+D)
K0 <- planor.designkey(factors=F0, model=M0, nunits=3^3, max.sol=2)
summary(K0)
mydesign.S4 <- planor.design(key=K0, select=2)

K0 <- planor.designkey(factors=c(LETTERS[1:4], "block"), nlevels=rep(3,5),
model=~block+(A+B+C+D)^2, estimate=~A+B+C+D,
nunits=3^3, base=~A+B+C, max.sol=2)
### alias on an object of class keymatrix
alias(K0[[1]][[1]])
### alias on an object of class designkey
alias(K0[1])
### alias on an object of class listofkeyrings
alias(K0)


K0 <- planor.designkey(factors=c(LETTERS[1:5]), nlevels=rep(2,5),
model= ~(A+B+C+D + E)^2, estimate=~A+B+C+D+E,
nunits=2^4, base=~A+B+C+D, max.sol=1)
### alias on an object of class keymatrix
alias(K0[[1]][[1]])
### alias on an object of class designkey
alias(K0[1])
### alias on an object of class listofkeyrings
alias(K0)


# Nine treatment factors at 3, 3, 2, 2, 2, 2, 2, 2, 2 levels
# Full factorial = 1152 runs
#main effects only  ?  Can seemingly get away with 72 runs ?
mixKey <- planor.designkey(factors=c( LETTERS[1:9]),
  nlevels=c(3,3, 2,2,2,2,2,2,2),
  model    = ~(A+B+C+D+E+F+G+H+I)^2, estimate=~ A+B+C+D+E+F+G+H+I,  # main effects only

  nunits= 3^2 * 2^7 / (2),
  #base=~A+B+D, # an optional additive formula to specify the basic factors. see note.
  max.sol=1)
alias(mixKey)
alias(mixKey[[1]][[1]])



