#
# makeGridY20.2.f.R - 
# create YFT grid Y20.2 = 13 dimensional grid requested by WPTT 2019 with explicit ENV link
# environmental-link uses seasonal average, but not inteneded to be part of the grid
# included as a dimension, but only to be tested once and retained 100% if it makes a difference and MSE code can be modified
# The fractional factorial calculation is not automated, those bits are hard-coded, check whether design elements are unique (replicates may occur)
# require(planor)
# require(MASS)

makeGridY20.2.f <- function (path='H:\\C-offline\\MSE-IO-BET-YFT\\OMconditioning\\YFT\\', gridName= 'gridY20.2', makeGrid=T, makeBatFiles=T, splitMasterBatch=35, batFile="projBatNoHessJitterLoop")
{
  # Start fractional factorial design stuff  ######################################
  # If single level design elements are retained here, they must be appended after factorial design
  #sp.val      <- c("R4", "R2")        # spatial structure (2 or 4 areas)
  h.val      <- c( "h70","h80","h90") # Beverton-Holt SR steepness
  M.val      <- c("M06", "M08","M10") # mortality                                                      xxx change from 2019

  growth.val <- c("gr1","gr2")        # Growth curve from assessment, or ad hoc alternate              xxx change from 2019
  mix.val    <- c("x4","x8")          # tag mixing period (qtrs); irrelevant if tags not used
  t.val      <- c("t10", "t01", "t0001") # tag weight - dropped t01 to greatly simplify design         xxx change from 2019
  q.val      <- c("q0","q1")             # CPUE q trend % per y
  i.val      <- c("iH","iC")             # CPUE standardization method: cluster vs HBF
  iCV.val    <- c("i1","i3")             # sigma CPUE = 0.3 or 0.1
  iRWt.val   <- c("iR1", "iR2")          # CPUE area weighitng factor (iR1 preferred by Hoyle)
  ess.val    <- c("CL75","ess5")         # size comp weighting; 1 iteration of re-weighting or all fisheries at ess=5
  sel.val    <- c("SL","SD")             # LL selectivity logistic or dome-shaped
  sr.val  <- c("sr6")                 # stock-recruit sigmaR
  ev.val  <- c("EV") #c("EV","nEV")
    # define grid options: put in descending order of the number of options
  # Cannot seem to get design < 432 with 3^3, can get 144 with 3^2
  #sp.val  = c( "R4MvEst"),        # Spatial&Population structure - assessment default single option
  #sp.val  = c( "R3I1"),           # single CPUE aggregated over seasons
  #h.val   <- c( "h70","h80","h90") # SR steepness
#  h.val   <- c( "h70","h90") # SR steepness
#  M.val   = c( "M10", "M06") # mortality
#  t.val   = c("t10", "t0001")      # tag weight - dropped t01 to greatly simplify design
#  q.val   = c("q0","q1")           # CPUE q trend % per y
#  i.val   = c("iH","iC")           # CPUE standardization method: cluster vs HBF
#  iCV.val = c("i1","i3")           # sigma CPUE = 0.3 or 0.1
#  iRWt.val= c("iR1","iR2")       # CPUE area weighitng factor (iR1 preferred by Hoyle)
#  growth.val= c("gr1","gr2")         # Growth curve from assessment, or ad hoc alternate
#  ess.val = c("CLRW","ess5")      #size comp weighting; 1 iteration of re-weighting or all fisheries at ess=5
#  sel.val = c("SL","Sspl")           #LL selectivity logistic or dome-shaped

  #single level grid factors not in file name (i.e. because they were multi-level at one point, and are still options in the template files)
  #iRWt.val= c("iR1")       # CPUE area weighitng factor (iR1 preferred by Hoyle)
  #growth.val= c("gr1")         # Growth curve from assessment, or ad hoc alternate
  singleDimOptions = c(
    #h.val,
    #ess.val,   #size comp weighting; 1 iteration of re-weighting or all fisheries at ess=5
    sr.val,     # sigmaR
    ev.val
  )

  optList=list(
    #sp.val,
    h.val,
    M.val,
    t.val,
    q.val,
    i.val,
    iCV.val,
    iRWt.val,
    growth.val,
    ess.val,
      sel.val,
      mix.val)

  optListNames <- c(
    #'sp.val',
    'h.val',
    'M.val',
    't.val',
    'q.val',
    'i.val',
    'iCV.val',
    'iRWt.val',
    'growth.val',
    'ess.val',
    'sel.val',
    'mix.val')


  names(optList) <- optListNames

  nlevels <- NULL
  for(i in 1:length(optList)){
    nlevels <- c(nlevels, length(optList[[i]]))
  }

  mixKey <- planor.designkey(factors=optListNames,
    nlevels=nlevels,

    #main effects, no spatial option
    model    = ~ ( h.val + M.val + t.val + q.val + i.val + iCV.val + iRWt.val + growth.val + ess.val + sel.val + mix.val),

    # main effects with 2 spatial optiokns    
    #model    = ~( sp.val + h.val + M.val + t.val + q.val + i.val + iCV.val + iRWt.val + growth.val + ess.val + sel.val + mix.val),
    # main effects + 2 way interactions with 2 spatial optiokns    
    #model    = ~( sp.val + h.val + M.val + t.val + q.val + i.val + iCV.val + iRWt.val + growth.val + ess.val + sel.val + mix.val)^2,
    #estimate=  ~ h.val + M.val + t.val + q.val + i.val + iCV.val + iRWt.val + growth.val + ess.val, # main effects only
    #estimate=  ~ h.val + M.val + t.val + q.val + i.val + iCV.val + iRWt.val + growth.val + ess.val, # main effects only
    #model    = ~(  h.val + M.val + t.val + q.val + i.val + iCV.val +                        ess.val  )^2,
    #model    = ~(  h.val + M.val + t.val + q.val + i.val + iCV.val +                        ess.val  )^4,
    #model    = ~(  h.val + M.val + t.val + q.val + i.val + iCV.val +                        ess.val + h.val*M.val ),
    #estimate=  ~ (h.val + M.val + t.val + q.val + i.val + iCV.val +                          ess.val)^2  , # main effects only
    #estimate=  ~ (h.val + M.val + t.val + q.val + i.val + iCV.val +                          ess.val)  , # main effects only
    #nunits= prod(nlevels)/(16), # (set denominator to 1 for full grid)
    #nunits= prod(nlevels)/(8), # (set denominator to 1 for full grid)
    #base=~A+B+D, # an optional additive formula to specify the basic factors. see note.

    nunits= prod(nlevels)/(48), # (set denominator to 1 for full grid)
    max.sol=1)
  
  alias(mixKey)
  mixPlan <- planor.design(key=mixKey, selection=c(1,1))

  D <- mixPlan@design
  # End fractional design  ##########################################################


  #make the grid list
  gridList <- NULL
  for(i in 1:nrow(D)){
      j <- 1
      modName <- optList[[colnames(D)[j]]][D[i,colnames(D)[j]]]
      for(j in 2:ncol(D)){
        #fudge for fixed values removed from this grid, but put back into name in same order as grid B19.1
        #if(j==7) modName <- modName %&% "_" %&% iRWt.val[1] %&% "_" %&% growth.val[1]

        modName <- modName %&% "_" %&% optList[[colnames(D)[j]]][D[i,colnames(D)[j]]]
      }
      gridList <- c(gridList, modName)
  }
  
  length(gridList)
  length(unique(gridList))
  print("pause to inspect the fractional factorial design")

  browser()


      #define master Batch File
      masterBatchFName <- gridName
      if(makeGrid){
         #read the baseline CPUE and area-weighting files
         #iMatBase <- read.table(path  %&% gridName %&% "\\gridTemplate\\CPUEbet.dat", header=T, stringsAsFactors=F)
         #iRBase   <- read.table(path  %&% gridName %&% "\\gridTemplate\\CPUEbetAreaWt.dat", header=T, stringsAsFactors=F)
         iMatBase <- read.table(path  %&% gridName %&% "\\gridTemplate\\yftOMcpue.dat", header=T, stringsAsFactors=F)

         ctltemplate <- scan(path %&% gridName %&% "\\gridTemplate" %&% "\\templateYFT.ctl", what="", sep='\n')
         dattemplate <- scan(path %&% gridName %&% "\\gridTemplate" %&% "\\templateYFT.dat", what="", sep='\n')
      }
      if(makeBatFiles) mf <- file(  path %&% gridName %&% "\\" %&% masterBatchFName %&% ".bat", open="w" )

      if(makeBatFiles & splitMasterBatch){
        for (iBat in 1:splitMasterBatch){
          if(iBat<10) {
            fName <- "bat0" %&% iBat %&% ".bat"
          } else {
            fName <- "bat" %&% iBat %&% ".bat"
          }

        assign(fName, file(  path %&% gridName %&% "\\" %&% fName, open="w" ))

#if(makeBatFiles) mf02 <- file(  path %&% gridName %&% "\\" %&% masterBatchFName %&% ".02.bat", open="w" )
        }
      }



      for(iMod in 1:length(gridList)){
          modName <- gridList[iMod]
          modOptions <- unlist(strsplit(modName, split="_"))

          if(makeGrid){ #make and populate the and dirs, otherwise just make the batch files or return the list


            #create directories and copy requisite files
            #mkdir(c(path %&% '\\' %&% gridName))  #mvbutils versions
            modDir <- gridName %&% '\\' %&% modName
            dir.create(path %&% modDir)

            copyString <- " copy "  %&% path %&% gridName %&% "\\gridTemplate " %&% path %&% gridName %&% '\\' %&% modName
            system(paste(Sys.getenv("COMSPEC"),"/c ", copyString))


            #create CONTROL.SS from template by removing comments on appropriate switches
            ctlFile <- file(path %&% gridName %&% '\\' %&% modName %&% "\\YFT.ctl", open="w")
            cat('# ' %&% modName, file=ctlFile, sep='\n')
            #write ('# ' %&% gridName, file=path %&% "\\" %&% gridName %&% "\\YFT.ctl", append=F)

            for(i in 1:length(ctltemplate)){
              str <- ctltemplate[i]

              for(j in c(modOptions, singleDimOptions)){
                str <- sub('# xxx ' %&%  j,      '', str)
              }

              #write (str, file=path %&% "\\" %&% gridName %&% "\\YFT.ctl", append=T)
              cat(str, file=ctlFile, sep='\n')

            }
            close(ctlFile)

            #create the cpue observations, combining ~4 specification options

            iCV <- 0.2
            if("i1" %in% modOptions) iCV <- 0.1 # cluster analysis in tropical regions (cluster in termperate)
            if("i3" %in% modOptions) iCV <- 0.3 # cluster analysis in tropical regions (cluster in termperate)

            #area weighting method proposed for YFT (and BET implicitly) in WPTT 2018
            Iseries <- "pr_7994_m8" # preferred area weighting
            if("iR2" %in% modOptions) Iseries <- "pr_7594_m8" # alternate area weighting
            if("iR3" %in% modOptions) Iseries <- "pr_8000_m8" # alternate area weighting

            if("q1" %in% modOptions) Iseries <- Iseries %&% "_q1" # quarterly catchability trend ~1% per year

            #cpueBlock <- NULL
            #for(iS in unique(iMatBase$index)){ #survey loop
                 #select the appropriate series the files
                 # select targeting
                 #if(i.val == "iH") target <- "Htrop"   # HBF analysis in tropical regions (cluster in temperate)
                 #if(i.val == "iC") target <- "Ctrop"   # cluster analysis in tropical regions (cluster in termperate)

                 target <- "Htrop" # HBF analysis in tropical regions (cluster in temperate)
                 if("iC" %in% modOptions) target <- "Ctrop" # cluster analysis in tropical regions (cluster in termperate)

                 iMat <- iMatBase[(iMatBase$target == target),]

                 # start in 1972
                 iMat <- iMat[iMat$ssYr>100,]

                 #set the CV
                 iMat$cv <- iCV
                 iMat$year <- iMat$ssYr
                 iMat$season <- 1

                 #series mapping to SS fisheries
                 iMat$AssessmentAreaName[iMat$AssessmentAreaName == "1b"] <- 26
                 iMat$AssessmentAreaName[iMat$AssessmentAreaName == "2"]  <- 27
                 iMat$AssessmentAreaName[iMat$AssessmentAreaName == "3"]  <- 28
                 iMat$AssessmentAreaName[iMat$AssessmentAreaName == "4"]  <- 29

                 #year  season  index  cpue  cv
                 #209  1  16  0.731728481  0.2
                 cpueBlock <- cbind(as.integer(iMat$year), as.integer(iMat$season), as.integer(iMat$AssessmentAreaName) ,as.numeric(iMat[,colnames(iMat)==Iseries]), as.numeric(iMat$cv))
                 colnames(cpueBlock) <- c("year","season","index","cpue","cv")
                 #Sblock <- Sblock[!is.na(Sblock[,'cpue']),]
                 #cpueBlock <- rbind(cpueBlock, Sblock)
            #} #survey loop

            datFile <- file(path %&% gridName %&% '\\' %&% modName %&% "\\YFT.dat", open="w")
            cat('# ' %&% modName, file=datFile, sep='\n')
            #write ('# ' %&% gridName, file=path %&% "\\" %&% gridName %&% "\\YFT.dat", append=F)

            for(i in 1:length(dattemplate)){
               str <- dattemplate[i]

               for(j in c(modOptions, singleDimOptions)){
                 str <- sub('# xxx ' %&%  j,      '', str)
               }
               #  str <- sub('# xxx ' %&%  sp.val[sp], '', str)
               #  str <- sub('# xxx ' %&%  h.val[h],   '', str)
               #  str <- sub('# xxx ' %&%  M.val[M],   '', str)
               #  str <- sub('# xxx ' %&%  t.val[t], '', str)
               #  str <- sub('# xxx ' %&%  mix.val[imix], '', str)
               #  str <- sub('# xxx ' %&%  ess.val[iess], '', str)


               if (grepl("# xxx num CPUE obs",str, ignore.case=T)) str <- nrow(cpueBlock)
               if (grepl("# xxx add CPUE series here",str, ignore.case=T)){
                 cat("# ", file=datFile, sep=' ')
                 write.matrix(cpueBlock, file = datFile, sep = " ")
               }
              cat(str, file=datFile, sep='\n')
              #write (str, file=path %&% "\\" %&% gridName %&% "\\YFT.dat", append=T)
            } #dat Template

            close(datFile)
          } #if makeGrid

#}} #sub test

            #create DATA.SS from template
            ### loop over oldtxt and replace switches...
#            template <- scan(path %&% "\\gridTemplate" %&% "\\templateYFT.dat", what="", sep='\n')
#            write ('# ' %&% gridName, file=path %&% "\\" %&% gridName %&% "\\YFT.dat", append=F)
#
#            for(i in 1:length(template)){
#              str <- template[i]
#
#              str <- sub('# xxx ' %&%  sp.val[sp], '', str)
#              str <- sub('# xxx ' %&%  h.val[h], '', str)
#              str <- sub('# xxx ' %&%  M.val[M], '', str)
#              str <- sub('# xxx ' %&%  t.val[t], '', str)
#
#              write (str, file=path %&% "\\" %&% gridName %&% "\\YFT.dat", append=T)
#            }


            if(makeBatFiles){
              projCall <- "\ncall " %&% batFile
              #write the master Batch file call
              cat("\ncd " %&% modName, file=mf, sep='\n' )
              cat(projCall, file=mf, sep='\n' )
              cat("cd ..", file=mf, sep='\n' )

              #make arbitrary number of Batch files for splitting grid
              if(splitMasterBatch){
                 fNum <- floor(iMod/(length(gridList)+0.01)*splitMasterBatch)+1
                 if(fNum<10) {
                   fName <- "bat0" %&% fNum  %&% ".bat"
                 } else {
                   fName <- "bat" %&% fNum  %&% ".bat"
                 }
                cat("\ncd " %&% modName %&% projCall %&% "\ncd .. \n", file=get(fName), sep='\n' )
                #if(iMod <= 0.05*length(gridList)) cat("\ncd " %&% modName %&% projCall %&% "\ncd .. \n", file=mf01, sep='\n' )
                #if(iMod  > 0.05*length(gridList) & iMod <= 0.10*length(gridList)) cat("\ncd " %&% modName %&% projCall %&% "\ncd .. \n", file=mf02, sep='\n' )
              }
            } #if makeBatFiles
       } # end iMod loop
#        }
#        }
#        }
#        }
#        }
#        }
#        }
#        }

      if(makeBatFiles){
        close(mf)
        if(splitMasterBatch){
          for (iBat in 1:splitMasterBatch){
            if(iBat<10) {
              fName <- "bat0" %&% iBat %&% ".bat"
            } else {
              fName <- "bat" %&% iBat %&% ".bat"
            }
            close(get(fName))
          }
        }
      }
      #closeAllConnections()
      #graphics.off()

    return(gridList)
}

deleteMe <- makeGridY20.2.f(path=path, makeBatFiles=F, makeGrid=F)
print(deleteMe)
print(length(deleteMe))
print(length(unique(deleteMe)))
rm(deleteMe)
