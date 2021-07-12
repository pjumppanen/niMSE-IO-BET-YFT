refYFT2018 <- SS_output(dir = "H:\\OMconditioning-from-C-offline\\YFT\\AssessmentFiles2018\\refYFT2018", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=T, forecast=F)
SS_plots(refYFT2018, uncertainty=T)




yftCrash1 <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.3.2way\\h80_M06_t10_q0_iH_i1_iR1_gr2_CL75_SD_x4\\converged0\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(yftCrash1, uncertainty=F)

yftHiRec1 <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.3.2way\\h70_M10_t10_q1_iC_i1_iR1_gr1_CL75_SD_x4\\converged0\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(yftHiRec1, uncertainty=F)

yftHiRec1.1 <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.3.2way\\h70_M10_t10_q1_iC_i1_iR1_gr1_CL75_SD_x4\\converged1\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(yftHiRec1.1, uncertainty=F)

yftHiRec1.2 <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.3.2way\\h70_M10_t10_q1_iC_i1_iR1_gr1_CL75_SD_x4\\converged2\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(yftHiRec1.2, uncertainty=F)

yftHiRec1.3 <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.3.2way\\h70_M10_t10_q1_iC_i1_iR1_gr1_CL75_SD_x4\\converged3\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(yftHiRec1.3, uncertainty=F)

#replace dev_vector with regular vector
nonRecDev <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.3.2way\\h70_M10_t10_q1_iC_i1_iR1_gr1_CL75_SD_x4\\SigmaRlateLoSimpleDev\\converged1\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(nonRecDev, uncertainty=F)

nonRecDev2 <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.3.2way\\h70_M10_t10_q1_iC_i1_iR1_gr1_CL75_SD_x4\\SigmaRlateLoSimpleDev\\converged2\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(nonRecDev2, uncertainty=F)


nonRecDev3 <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.3.2way\\z-h80_M10_t10_q0_iH_i1_iR1_gr2_CL75_SD_x4-nonRecDev\\converged1\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(nonRecDev3, uncertainty=F)


jd4 <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.3.2way\\h70_M10_t10_q1_iC_i1_iR1_gr1_CL75_SD_x4\\SigmaRlateAdvanced4\\converged1\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=T, forecast=F)
SS_plots(jd4, uncertainty=T)

jd8 <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.3.2way\\h70_M10_t10_q1_iC_i1_iR1_gr1_CL75_SD_x4\\SigmaRlateAdvanced8\\convergedAlmost\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=T, forecast=F)
SS_plots(jd8, uncertainty=T)

jd12 <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.3.2way\\h70_M10_t10_q1_iC_i1_iR1_gr1_CL75_SD_x4\\SigmaRlateAdvanced12\\converged2\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=T, forecast=F)
SS_plots(jd12, uncertainty=T)


library(r4ss)



#add two more years of known catch, but no other data
C2019 <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.3.2way\\h80_M06_t10_q0_iH_i1_iR1_gr2_CL75_SD_x4\\C2019\\convergedAlmost\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(C2019, uncertainty=F)

C2025 <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.3.2way\\h80_M06_t10_q0_iH_i1_iR1_gr2_CL75_SD_x4\\C2025\\converged1\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(C2025, uncertainty=F)

yftHiRec2 <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.3.2way\\h80_M08_t10_q1_iH_i1_iR2_gr1_CL75_SD_x4\\converged0\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(yftHiRec2, uncertainty=F)


recFail2 <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.3.2way\\h80_M10_t10_q0_iH_i1_iR1_gr2_CL75_SD_x4\\converged1\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(recFail2, uncertainty=F)

recFail3 <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.3.2way\\h70_M08_t0001_q1_iC_i1_iR2_gr2_CL75_SD_x8\\converged1\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(recFail3, uncertainty=F)

recTest <- SS_output(dir = OMRootDir %&% "OMconditioning\\YFT\\gridY19.3.2way\\z-h80_M10_t10_q0_iH_i1_iR1_gr2_CL75_SD_x4-RecDevTest\\converged1\\", repfile = "Report.sso", compfile = "CompReport.sso", ncol=213, covar=F, forecast=F)
SS_plots(recTest, uncertainty=F)



plot(yftCrash1$timeseries$Yr, yftCrash1$timeseries$"F:_10", col=1, type='b')
lines(yftCrash1$timeseries$Yr, yftCrash1$timeseries$"F:_11", col=2)
lines(yftCrash1$timeseries$Yr, yftCrash1$timeseries$"F:_13", col=3)
lines(yftCrash1$timeseries$Yr, yftCrash1$timeseries$"F:_3", col=4)
lines(yftCrash1$timeseries$Yr, yftCrash1$timeseries$"F:_7", col=5)


# Region 1 (NW main=southern western equatorial) =fishery 7, survey 26
catch <- yftCrash1$timeseries[yftCrash1$timeseries$'obs_cat:_7'>0, c("Yr",'obs_cat:_7',"F:_7")]
cpue  <- yftCrash1$cpue[yftCrash1$cpue$Fleet==26,c("Yr",'Obs')]
ftest <- merge(catch,cpue, by='Yr')
ftest$stdEff <- ftest$`obs_cat:_7`/ftest$Obs
ftest$stdEff <- ftest$stdEff/mean(ftest$stdEff)
ftest$modF <- ftest$"F:_7"/mean(ftest$"F:_7")

par(mfrow=c(3,1))
plot(ftest$Yr, ftest$modF, type='l', main="NW region - SS F and std Effort")
lines(ftest$Yr, ftest$stdEff, col=2)

plot(ftest$modF, ftest$stdEff, main="NW region - std Effort vs: SS F  ")
lines(ftest$modF,ftest$modF)
plot(ftest$Yr, ftest$modF/ftest$stdEff, ylim=c(0,3), main="NW LL F/(std effort proxy) ")
abline(h=mean(ftest$modF/ftest$stdEff),lty=2)
lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(48)/184),col=1,lwd=2)
lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(31)/184),col=2)
lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(19)/184),col=3)
lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(7)/184),col=4)
lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(4)/184),col=5)
legend(150,.3,col=c(1,1:5), legend=c("mean","48 qtrs","31","19","7","4"), lty=c(2,rep(1,5)), lwd=c(1,2,rep(1,5)), horiz=T)

#Just cpue
plot(cpue$Yr, cpue$Obs)
lines(loess.smooth(cpue$Yr, cpue$Obs, span=(48)/184),col=1,lwd=2)
lines(loess.smooth(cpue$Yr, cpue$Obs, span=(31)/184),col=2)
lines(loess.smooth(cpue$Yr, cpue$Obs, span=(19)/184),col=3)
lines(loess.smooth(cpue$Yr, cpue$Obs, span=(7)/184),col=4)
lines(loess.smooth(cpue$Yr, cpue$Obs, span=(4)/184),col=5)

plot(cpue$Yr, log(cpue$Obs))
abline(lm(log(cpue$Obs)~cpue$Yr), col="red")
lines(loess.smooth(cpue$Yr, log(cpue$Obs), span=(48)/184),col=1,lwd=2)
lines(loess.smooth(cpue$Yr, log(cpue$Obs), span=(31)/184),col=2)
lines(loess.smooth(cpue$Yr, log(cpue$Obs), span=(19)/184),col=3)
lines(loess.smooth(cpue$Yr, log(cpue$Obs), span=(7)/184),col=4)
lines(loess.smooth(cpue$Yr, log(cpue$Obs), span=(4)/184),col=5)


#detrended ln(cpue) residuals from linear reg
t <- lm(log(cpue$Obs)~cpue$Yr)
plot(cpue$Yr, t$residuals, main="NW LL detrended log(cpue)")
lines(loess.smooth(cpue$Yr, t$residuals, span=(48)/184),col=1,lwd=2)
lines(loess.smooth(cpue$Yr, t$residuals, span=(31)/184),col=2)
lines(loess.smooth(cpue$Yr, t$residuals, span=(19)/184),col=3)
lines(loess.smooth(cpue$Yr, t$residuals, span=(7)/184),col=4)
lines(loess.smooth(cpue$Yr, t$residuals, span=(4)/184),col=5)

#Just F
plot(ftest$Yr, log(ftest$"F:_7"), main="NW LL SS-estimated log(F)")
lines(loess.smooth(ftest$Yr, log(ftest$"F:_7"), span=(31)/184),col=2)
lines(loess.smooth(ftest$Yr, log(ftest$"F:_7"), span=(19)/184),col=3)
lines(loess.smooth(ftest$Yr, log(ftest$"F:_7"), span=(7)/184),col=4)

#detrended ln(cpue) residuals from linear reg and F
plot(loess.smooth(cpue$Yr, t$residuals, span=(19)/184)$y, loess.smooth(ftest$Yr, log(ftest$"F:_7"), span=(19)/184)$y)
plot(loess.smooth(ftest$Yr, log(ftest$"F:_7"), span=(19)/184),col=3)



# just vulnerable N
vn <- yftCrash1$timeseries[yftCrash1$timeseries$Area==1,]
vn <- vn[vn$Era=="TIME",]
vn <- vn[vn$Yr>100,]
plot(vn$Yr, log(vn$"SpawnBio"), main="NW spawning biomass")

lines(loess.smooth(vn$Yr, log(vn$"SpawnBio"), span=(31)/184),col=2)
lines(loess.smooth(vn$Yr, log(vn$"SpawnBio"), span=(19)/184),col=3)
lines(loess.smooth(vn$Yr, log(vn$"SpawnBio"), span=(7)/184),col=4)

#recruitment
rec <- yftCrash1$recruit[yftCrash1$recruit$era=="Main",]
plot(rec$year,rec$dev, main="aggregate recruitment", main="aggregate recruitment devs")
lines(loess.smooth(rec$year,rec$dev, span=(31)/184),col=2)
lines(loess.smooth(rec$year,rec$dev, span=(19)/184),col=3)
lines(loess.smooth(rec$year,rec$dev, span=(7)/184),col=4)

rec2 <- yftCrash1$natage[yftCrash1$natage$Area==1,1:12]
rec2 <- rec2[rec2$Era=="TIME",] 
rec2 <- rec2[rec2$"Beg/Mid"=="B",] 
rec2 <- rec2[rec2$Yr>100,]
plot(rec2$Yr,log(rec2$"0"), main="NW log(rec)")
lines(loess.smooth(rec2$Yr,log(rec2$"0"), span=(31)/184),col=2)
lines(loess.smooth(rec2$Yr,log(rec2$"0"), span=(19)/184),col=3)
lines(loess.smooth(rec2$Yr,log(rec2$"0"), span=(7)/184),col=4)




# Region 2 (SW) = fishery 10, survey 2(27)
catch <- yftCrash1$timeseries[yftCrash1$timeseries$'obs_cat:_10'>0, c("Yr",'obs_cat:_10',"F:_10")]
cpue  <- yftCrash1$cpue[yftCrash1$cpue$Fleet==27,c("Yr",'Obs')]
ftest <- merge(catch,cpue, by='Yr')
ftest$stdEff <- ftest$`obs_cat:_10`/ftest$Obs
ftest$stdEff <- ftest$stdEff/mean(ftest$stdEff)
ftest$modF <- ftest$"F:_10"/mean(ftest$"F:_10")

par(mfrow=c(3,1))
plot(ftest$Yr, ftest$modF, type='l', main="SW region")
lines(ftest$Yr, ftest$stdEff, col=2)

plot(ftest$modF, ftest$stdEff)
lines(ftest$modF,ftest$modF)
plot(ftest$Yr, ftest$modF/ftest$stdEff, ylim=c(0,3), main="SW LL F/(std effort proxy) ")
abline(h=mean(ftest$modF/ftest$stdEff),lty=2)
lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(48)/184),col=1,lwd=2)
lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(31)/184),col=2)
lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(19)/184),col=3)
lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(7)/184),col=4)
#lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(4)/184),col=5)
legend(150,.3,col=c(1,1:5), legend=c("mean","48 qtrs","31","19","7","4"), lty=c(2,rep(1,5)), lwd=c(1,2,rep(1,5)), horiz=T)

# Region 3 (SE) = fishery 11, survey 3(28)
catch <- yftCrash1$timeseries[yftCrash1$timeseries$'obs_cat:_11'>0, c("Yr",'obs_cat:_11',"F:_11")]
cpue  <- yftCrash1$cpue[yftCrash1$cpue$Fleet==28,c("Yr",'Obs')]
ftest <- merge(catch,cpue, by='Yr')
ftest$stdEff <- ftest$`obs_cat:_11`/ftest$Obs
ftest$stdEff <- ftest$stdEff/mean(ftest$stdEff)
ftest$modF <- ftest$"F:_11"/mean(ftest$"F:_11")

par(mfrow=c(3,1))
plot(ftest$Yr, ftest$modF, type='l', main="SE region")
lines(ftest$Yr, ftest$stdEff, col=2)

plot(ftest$modF, ftest$stdEff)
lines(ftest$modF,ftest$modF)
plot(ftest$Yr, ftest$modF/ftest$stdEff, ylim=c(0,3))
abline(h=mean(ftest$modF/ftest$stdEff),lty=2)
lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(48)/184),col=1,lwd=2)
lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(31)/184),col=2)
lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(19)/184),col=3)
#lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(7)/184),col=4)
legend(150,.3,col=c(1,1:5), legend=c("mean","48 qtrs","31","19","7","4"), lty=c(2,rep(1,5)), lwd=c(1,2,rep(1,5)), horiz=T)

# Region 4 (NE) = fishery 13, survey 4(29)
catch <- yftCrash1$timeseries[yftCrash1$timeseries$'obs_cat:_13'>0, c("Yr",'obs_cat:_13',"F:_13")]
cpue  <- yftCrash1$cpue[yftCrash1$cpue$Fleet==29,c("Yr",'Obs')]
ftest <- merge(catch,cpue, by='Yr')
ftest$stdEff <- ftest$`obs_cat:_13`/ftest$Obs
ftest$stdEff <- ftest$stdEff/mean(ftest$stdEff)
ftest$modF <- ftest$"F:_13"/mean(ftest$"F:_13")

par(mfrow=c(3,1))
plot(ftest$Yr, ftest$modF, type='l', main="NE region")
lines(ftest$Yr, ftest$stdEff, col=2)

plot(ftest$modF, ftest$stdEff)
lines(ftest$modF,ftest$modF)
plot(ftest$Yr, ftest$modF/ftest$stdEff, ylim=c(0,3), main="NE LL F/(std effort proxy) ")
abline(h=mean(ftest$modF/ftest$stdEff),lty=2)
lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(48)/184),col=1,lwd=2)
lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(31)/184),col=2)
lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(19)/184),col=3)
lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(7)/184),col=4)
lines(loess.smooth(ftest$Yr, ftest$modF/ftest$stdEff, span=(4)/184),col=5)
legend(150,.3,col=c(1,1:5), legend=c("mean","48 qtrs","31","19","7","4"), lty=c(2,rep(1,5)), lwd=c(1,2,rep(1,5)), horiz=T)



#YFT(2019) models with rec spikes
5 "h80_M10_t10_q0_iH_i1_iR1_gr2_CL75_SD_x4"
8 "h90_M10_t10_q0_iH_i1_iR1_gr2_CL75_SD_x4"
11 "h90_M10_t10_q0_iH_i1_iR1_gr2_CL75_SD_x8"
55 "h90_M10_t10_q0_iC_i1_iR2_gr2_CL75_SD_x8"
90 "h70_M06_t10_q1_iH_i1_iR2_gr2_CL75_SL_x4"
96 "h90_M10_t10_q1_iH_i1_iR2_gr2_CL75_SL_x4"
122 "h70_M10_t10_q1_iC_i1_iR1_gr1_CL75_SD_x4"
123 "h80_M08_t10_q1_iC_i1_iR1_gr1_CL75_SD_x4"
124 "h90_M08_t10_q1_iC_i1_iR1_gr1_CL75_SD_x8"
133 "h90_M10_t10_q1_iC_i1_iR1_gr2_CL75_SL_x4"
137 "h90_M10_t10_q1_iC_i1_iR1_gr2_CL75_SL_x8"
204 "h70_M10_t0001_q0_iC_i1_iR1_gr1_CL75_SD_x4"
206 "h80_M10_t0001_q0_iC_i1_iR1_gr1_CL75_SD_x4"
247 "h70_M10_t0001_q1_iH_i1_iR1_gr2_CL75_SD_x4"
248 "h90_M08_t0001_q1_iH_i1_iR1_gr2_CL75_SD_x4"
249 "h70_M08_t0001_q1_iH_i1_iR1_gr2_CL75_SD_x8"
251 "h90_M10_t0001_q1_iH_i1_iR1_gr2_CL75_SD_x8"
271 "h70_M10_t0001_q1_iC_i1_iR2_gr2_CL75_SD_x4"
272 "h80_M08_t0001_q1_iC_i1_iR2_gr2_CL75_SD_x4"
273 "h90_M08_t0001_q1_iC_i1_iR2_gr2_CL75_SD_x4"
274 "h70_M08_t0001_q1_iC_i1_iR2_gr2_CL75_SD_x8"








