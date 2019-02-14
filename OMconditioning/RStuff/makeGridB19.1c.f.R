#
# makeGridB19.1c.R replaces gridB19.1 with more control option for creating bat files etc
# create BET grid B19.1c = full 288 model grid from the reduced B19.1
# The fractional factorial calculation is not automated, those bits are hard-coded
# require(planor)
# require(MASS)

makeGridB19.1c.f <- function (path='H:\\C-offline\\MSE-IO-BET-YFT\\OMconditioning\\BET\\', gridName= 'gridB19.1c', makeGrid=T, makeBatFiles=T, splitMasterBatch=35, batFile="projBatNoHessJitterLoop")
{
  # Start fractional factorial design stuff  ######################################
  # If single level design elements are retained, they must be appended after factorial design

  # define grid options: put in descending order of the number of options
  # Cannot seem to get design < 432 with 3^3, can get 144 with 3^2
  #sp.val  = c( "R4MvEst"),        # Spatial&Population structure - assessment default single option
  #sp.val  = c( "R3I1"),           # single CPUE aggregated over seasons
  #h.val   <- c( "h70","h80","h90") # SR steepness
  #M.val   = c( "M10", "M08","M06") # mortality
  h.val   <- c( "h70","h90") # SR steepness
  M.val   = c( "M10", "M06") # mortality
  #t.val   = c("t10", "t01", "t0001")      # tag weight
  t.val   = c("t10", "t0001")      # tag weight - dropped t01 to greatly simplify design
  q.val   = c("q0","q1")           # CPUE q trend % per y
  i.val   = c("iH","iC")           # CPUE standardization method: cluster vs HBF
  iCV.val = c("i1","i3")           # sigma CPUE = 0.3 or 0.1
  iRWt.val= c("iR1","iR2")       # CPUE area weighitng factor (iR1 preferred by Hoyle)
  growth.val= c("gr1","gr2")         # Growth curve from assessment, or ad hoc alternate
  ess.val = c("CLRW","ess10")      #size comp weighting; 1 iteration of re-weighting or all fisheries at ess=5
  #sel.val = c("LL","LD")           #LL selectivity logistic or dome-shaped

  #single level grid factors not in file name (i.e. because they were multi-level at one point, and are still options in the template files)
  mix.val = c("x4")                # tag mixing period (qtrs)
  #iRWt.val= c("iR1")       # CPUE area weighitng factor (iR1 preferred by Hoyle)
  #growth.val= c("gr1")         # Growth curve from assessment, or ad hoc alternate
  singleDimOptions = c(mix.val)

  optList=list(
    h.val,
    M.val,
    t.val,
    q.val,
    i.val,
    iCV.val,
    iRWt.val,
    growth.val,
    ess.val)
    # sel.val)
    #  mix.val)

  optListNames <- c(
    'h.val',
    'M.val',
    't.val',
    'q.val',
    'i.val',
    'iCV.val',
    'iRWt.val',
    'growth.val',
    'ess.val')
    #'sel.val')
    #  'mix.val')


  names(optList) <- optListNames

  nlevels <- NULL
  for(i in 1:length(optList)){
    nlevels <- c(nlevels, length(optList[[i]]))
  }

  mixKey <- planor.designkey(factors=optListNames,
    nlevels=nlevels,
    model    = ~(  h.val + M.val + t.val + q.val + i.val + iCV.val + iRWt.val + growth.val + ess.val  )^2,
    #estimate=  ~ h.val + M.val + t.val + q.val + i.val + iCV.val + iRWt.val + growth.val + ess.val, # main effects only
    nunits= prod(nlevels)/(4), # (set denominator to 1 for full grid)
    #estimate=  ~ h.val + M.val + t.val + q.val + i.val + iCV.val + iRWt.val + growth.val + ess.val, # main effects only
#model    = ~(  h.val + M.val + t.val + q.val + i.val + iCV.val +                        ess.val  )^2,
    #model    = ~(  h.val + M.val + t.val + q.val + i.val + iCV.val +                        ess.val  )^4,
    #model    = ~(  h.val + M.val + t.val + q.val + i.val + iCV.val +                        ess.val + h.val*M.val ),
#estimate=  ~ (h.val + M.val + t.val + q.val + i.val + iCV.val +                          ess.val)^2  , # main effects only
    #estimate=  ~ (h.val + M.val + t.val + q.val + i.val + iCV.val +                          ess.val)  , # main effects only
    #nunits= prod(nlevels)/(16), # (set denominator to 1 for full grid)
    #nunits= prod(nlevels)/(8), # (set denominator to 1 for full grid)
    #base=~A+B+D, # an optional additive formula to specify the basic factors. see note.
    max.sol=1)

  alias(mixKey)
  mixPlan <- planor.design(key=mixKey)

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
  print("pause to inspect the fractional factorial design")
browser()


      #define master Batch File
      masterBatchFName <- gridName
      if(makeGrid){
         #read the baseline CPUE and area-weighting files
         iMatBase <- read.table(path  %&% gridName %&% "\\gridTemplate\\CPUEbet.dat", header=T, stringsAsFactors=F)
         iRBase   <- read.table(path  %&% gridName %&% "\\gridTemplate\\CPUEbetAreaWt.dat", header=T, stringsAsFactors=F)
         ctltemplate <- scan(path %&% gridName %&% "\\gridTemplate" %&% "\\templateBET.ctl", what="", sep='\n')
         dattemplate <- scan(path %&% gridName %&% "\\gridTemplate" %&% "\\templateBET.dat", what="", sep='\n')
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
            ctlFile <- file(path %&% gridName %&% '\\' %&% modName %&% "\\BET.ctl", open="w")
            cat('# ' %&% modName, file=ctlFile, sep='\n')
            #write ('# ' %&% gridName, file=path %&% "\\" %&% gridName %&% "\\BET.ctl", append=F)

            for(i in 1:length(ctltemplate)){
              str <- ctltemplate[i]

              for(j in c(modOptions, singleDimOptions)){
                str <- sub('# xxx ' %&%  j,      '', str)
              }

              #write (str, file=path %&% "\\" %&% gridName %&% "\\BET.ctl", append=T)
              cat(str, file=ctlFile, sep='\n')

            }
            close(ctlFile)

            #create the cpue observations, combining ~4 specification options
            qTrend <- 1.0                             # no catchability trend
            if("q1" %in% modOptions) qTrend <- 1.0025 # quarterly catchability trend ~1% per year

            stdMethodTrop <- "Htrop" # HBF analysis in tropical regions (cluster in temperate)
            if("iC" %in% modOptions) stdMethodTrop <- "Ctrop" # cluster analysis in tropical regions (cluster in termperate)

            iCV <- 0.2
            if("iCV1" %in% modOptions) iCV <- 0.1 # cluster analysis in tropical regions (cluster in termperate)
            if("iCV3" %in% modOptions) iCV <- 0.3 # cluster analysis in tropical regions (cluster in termperate)

            aWtMethod <- "iR1" #improved area weighting method proposed for YFT (and BET implicitly) in WPTT 2018
            if("iR2" %in% modOptions) aWtMethod <- "iR2" # alternate area weighting
            if("iR3" %in% modOptions) aWtMethod <- "iR3" # alternate area weighting

            cpueBlock <- NULL
            for(iS in unique(iMatBase$index)){ #survey loop
                 #select the standardization and area weighting options from the files
                 iMat <- iMatBase[(iMatBase$index == iS),]
                 iMat <- iMat[(iMat$stdMethod == 'Ctemp' | iMat$stdMethod == stdMethodTrop),]

                 lastSScpueYr <- max(as.numeric(iMat$year))

                 iR   <- iRBase[(iRBase$rWtOption == aWtMethod & iRBase$index == iS),]

                 #scale CPUE by the area weighting
                 iMat$cpue <- as.numeric(iMat$cpue)*as.numeric(iR$weight)

                 #calculate the CPUE trend given the q option
                 iMat$cpue <- as.numeric(iMat$cpue)*(qTrend^(lastSScpueYr - as.numeric(iMat$year)))

                 #set the CV
                 iMat$cv <- iCV

                 #year  season  index  cpue  cv
                 #209  1  16  0.731728481  0.2
                 Sblock <- cbind(as.numeric(iMat$year), as.numeric(iMat$season), as.numeric(substring(iMat$index,2)) ,as.numeric(iMat$cpue), as.numeric(iMat$cv))
                 colnames(Sblock) <- c("year","season","index","cpue","cv")
                 Sblock <- Sblock[!is.na(Sblock[,'cpue']),]

                 cpueBlock <- rbind(cpueBlock, Sblock)
            } #survey loop

            datFile <- file(path %&% gridName %&% '\\' %&% modName %&% "\\BET.dat", open="w")
            cat('# ' %&% modName, file=datFile, sep='\n')
            #write ('# ' %&% gridName, file=path %&% "\\" %&% gridName %&% "\\BET.dat", append=F)

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
              #write (str, file=path %&% "\\" %&% gridName %&% "\\BET.dat", append=T)
            } #dat Template

            close(datFile)
          } #if makeGrid

#}} #sub test

            #create DATA.SS from template
            ### loop over oldtxt and replace switches...
#            template <- scan(path %&% "\\gridTemplate" %&% "\\templateBET.dat", what="", sep='\n')
#            write ('# ' %&% gridName, file=path %&% "\\" %&% gridName %&% "\\BET.dat", append=F)
#
#            for(i in 1:length(template)){
#              str <- template[i]
#
#              str <- sub('# xxx ' %&%  sp.val[sp], '', str)
#              str <- sub('# xxx ' %&%  h.val[h], '', str)
#              str <- sub('# xxx ' %&%  M.val[M], '', str)
#              str <- sub('# xxx ' %&%  t.val[t], '', str)
#
#              write (str, file=path %&% "\\" %&% gridName %&% "\\BET.dat", append=T)
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

makeGridB19.1c.f(path=path, makeBatFiles=F, makeGrid=F)