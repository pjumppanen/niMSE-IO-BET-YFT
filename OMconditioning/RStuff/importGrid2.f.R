#function to import a list of SS3 models
#if convergedDir != "" the function will import all of the converged models and export the one with the lowest OBJFn value
# some key results from all models that converged are retained for a quick and dirty evaluation of sensitivity to initial conditions

importGrid.f <- function(
    gridList=gridY2List,
    gridDir=gridDir,
    convergedNum=0,
    covar=F, exeFile="SS3.24Z",
    stdOnly=F, #only load the OMs with exsiting std file (does not distinguish whether run not done, Hessian failed or Hessian deliberately not calculated)
    keepEverything=F, #keepEverything added to save re-importing everytime, uses too much memory to do a big list
    doLoad=T, #just check what converged, don't load it
    SPB_Yr="SPB_276",
    ncolSS=213) #this is the BET summary year, not YFT
{
  loadList <- NULL # if we do not waste time loading the std failures, this is a list of the models actually loaded
  failList <- NULL # models that did not converge

  tmp <- 0
  conStats <- matrix(NA,nrow=0,ncol=6) # statistics for evaluating convergence sensitivity
  colnames(conStats) <- c("modID","iCon", "objFn", "MSY", "SSBYoSSBMSY","maxGrad")

  for(i in 1:length(gridList)){
    rm(tmp)
    bestOBJfn <- 9.99e+256
    if(convergedNum) {
      convergedDir <- "converged" %&% convergedNum }
    else {
      convergedDir <- ""
    }
    stdExists <- file.exists(gridDir  %&% "\\" %&%  gridList[i] %&% "\\" %&% convergedDir %&% "\\" %&% exeFile %&% ".std") # "ss3_opt.std")
    if(convergedDir != "") converged <- file.exists(gridDir  %&% "\\" %&%  gridList[i] %&% "\\" %&% convergedDir) # "ss3_opt.std")

    loadFile <- T
    if(stdOnly & !stdExists){
      loadFile <- F
      print(c("File " %&% i %&% " skipped; loadFile = ",loadFile))
    }
    if(convergedDir != "" && converged==F){loadFile<- F}
    if(!loadFile){failList <- c(failList, gridList[i])}
    if(loadFile){
      loadList <- c(loadList, gridList[i])
      if(doLoad & !exists(gridList[i])){   #don't reload if it already exists in memory
        for(iCon in 1:convergedNum){
          tmp <- SS_output2(dir=gridDir  %&% "\\" %&%  gridList[i]  %&% "\\" %&%  "converged" %&% iCon %&% "\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=ncolSS, covar=covar,forecast=F)
          M <- tmp$endgrowth$M

          #output some summary statistics for convergence sensitivity analysis
          MSY <- tmp$derived_quants[tmp$derived_quants$LABEL == 'TotYield_MSY',]$Value
          SSBY <- tmp$derived_quants[tmp$derived_quants$LABEL == SPB_Yr,]$Value
          SSBMSY <- tmp$derived_quants[tmp$derived_quants$LABEL == 'SSB_MSY',]$Value
          SSBYoSSBMSY <- SSBY/SSBMSY
          LLH <- tmp$likelihoods_used$values[1]
          maxGrad <- tmp$maximum_gradient_component

          conStats <- rbind(conStats,c(gridList[i], iCon, LLH, MSY, SSBYoSSBMSY, maxGrad))

          #only retain the full model results from the lowest OBJfn converged result
          if(LLH < bestOBJfn) {
             bestOBJfn <- LLH
             # copy the relevant files to the parent directory
             copyStr <- "copy " %&% gridDir  %&%  gridList[i]  %&% "\\" %&%  "converged" %&% iCon %&% "\\*.* " %&% gridDir  %&%  gridList[i]
             shell(copyStr)

            if(keepEverything){
              tmp$M <- M
              tmp$stdExists <- tmp$stdExists
            } else {
              tmp <- list(tmp$derived_quants, tmp$recruit, tmp$maximum_gradient_component,tmp$likelihoods_used,tmp$likelihoods_raw_by_fleet,tmp$cpue, tmp$Length_comp_Eff_N_tuning_check, tmp$timeseries[,1:8], tmp$parameters[,1:9],M,
                 tmp$equil_yield, stdExists )
              names(tmp) <- c('derived_quants', 'recruit', 'maximum_gradient_component','likelihoods_used',"likelihoods_raw_by_fleet",'cpue', 'Length_comp_Eff_N_tuning_check', 'timeseries', 'parameters','M',
                'equil_yield', 'stdExists')
            }
          }
        } #iCon
        assign(gridList[i],tmp, envir = .GlobalEnv)
      }
    }
  }
  print(loadList)
  out <- list(loadList,failList,conStats)
  names(out) <- c("loadList","failList","conStats")
  return(out)
}
