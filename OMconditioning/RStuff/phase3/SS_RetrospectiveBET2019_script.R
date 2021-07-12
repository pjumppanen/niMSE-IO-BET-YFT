#------------------------------------------------------------------
# SS Retrospective Analysis (R Script):
#
# Configured for SESSF Stock Assessment 2015 
# By Athol Whitten, 2015, athol.whitten@mezo.com.au
# modified by Dale Kolody (2019) for IOTC BET 

#------------------------------------------------------------------

# Get latest (master) version of R4SS from Github:
# devtools::install_github("r4ss/r4ss")

# Open r4ss package:
library(r4ss)
"%&%" <- function(x,y){return(paste(x,y,sep=""))}


# Specify working folder (directory where each folder contains a set of model files, base-case or otherwise):
folder <- "E:\\KOL018\\MSE-IO-BET-YFT\\OMconditioning\\BET\\" 


# Enter the name of the model folder that retrospective analysis will be performed on:
#model <- "zretro-h80_M06_t10_q0_iH_i1_iR1_gr2_CL75_SD_x4" #probably bad example - this is a recruitment spike case
model <- "refB2019" # quick crash example case


# List of relative years over which to run retrospective tests (should be 0 to -Y, e.g. 0:-5, or seq(0, -10, -2)):
years <- c(0, -8, -16, -24, -32) # seems to be complication with tags if one goes back too far

# Number of recent years/cohorts of recruitment estimates to plot in relation to main model (for 'squid' plot):
n_cohorts <- 10

# Include hessian calculation in model runs?
#hessian <- TRUE
hessian <- FALSE

# Call to system or shell to run SS3? (Choice depends on how you’re running R. Default is "system"):
call_type <- "system"

# Print retrospective plots to a folder and/or show on screen?
show_plots <- TRUE
print_plots <- FALSE

# For spawning depletion plots, specify biomass target and biomass minimum threshold:
biomass_target    <- 1.0
biomass_threshold <- 0.5


#-------------------------------------------------------------------
# End user controls, start main script:
#-------------------------------------------------------------------

# Run retrospective model tests:
model <- "refB2019-H0"
#SS_doRetro(masterdir=folder, oldsubdir=model, newsubdir=paste0(model,"Retro"), subdirstart="Retro", years=years, extras=ifelse(hessian==TRUE, "-nox", "-nox -nohess"), CallType=call_type)
retro_mods <- SSgetoutput(dirvec=paste0(folder, '\\', model, 'Retro\\Retro', years), getcovar=hessian, ncols=250)

model <- "refB2019-H0.2"
#SS_doRetro(masterdir=folder, oldsubdir=model, newsubdir=paste0(model,"Retro"), subdirstart="Retro", years=years, extras=ifelse(hessian==TRUE, "-nox", "-nox -nohess"), CallType=call_type)
retro_mods <- SSgetoutput(dirvec=paste0(folder, '\\', model, 'Retro\\Retro', years), getcovar=hessian, ncols=250)

model <- "refB2019-H-0.2"
#SS_doRetro(masterdir=folder, oldsubdir=model, newsubdir=paste0(model,"Retro"), subdirstart="Retro", years=years, extras=ifelse(hessian==TRUE, "-nox", "-nox -nohess"), CallType=call_type)
retro_mods <- SSgetoutput(dirvec=paste0(folder, '\\', model, 'Retro\\Retro', years), getcovar=hessian, ncols=250)

model <- "refB2019-H-0.1"
#SS_doRetro(masterdir=folder, oldsubdir=model, newsubdir=paste0(model,"Retro"), subdirstart="Retro", years=years, extras=ifelse(hessian==TRUE, "-nox", "-nox -nohess"), CallType=call_type)
retro_mods <- SSgetoutput(dirvec=paste0(folder, '\\', model, 'Retro\\Retro', years), getcovar=hessian, ncols=250)


model <- "refB2019-H0.1"
#SS_doRetro(masterdir=folder, oldsubdir=model, newsubdir=paste0(model,"Retro"), subdirstart="Retro", years=years, extras=ifelse(hessian==TRUE, "-nox", "-nox -nohess"), CallType=call_type)
retro_mods <- SSgetoutput(dirvec=paste0(folder, '\\', model, 'Retro\\Retro', years), getcovar=hessian, ncols=250)

model <- "refB2019-H-0.5"
#SS_doRetro(masterdir=folder, oldsubdir=model, newsubdir=paste0(model,"Retro"), subdirstart="Retro", years=years, extras=ifelse(hessian==TRUE, "-nox", "-nox -nohess"), CallType=call_type)
retro_mods <- SSgetoutput(dirvec=paste0(folder, '\\', model, 'Retro\\Retro', years), getcovar=hessian, ncols=250)


model <- "refB2019-H0.5"
#SS_doRetro(masterdir=folder, oldsubdir=model, newsubdir=paste0(model,"Retro"), subdirstart="Retro", years=years, extras=ifelse(hessian==TRUE, "-nox", "-nox -nohess"), CallType=call_type)
retro_mods <- SSgetoutput(dirvec=paste0(folder, '\\', model, 'Retro\\Retro', years), getcovar=hessian, ncols=250)



# Commented out because the retrsopectives were repeated manually to fix the last year of recruitment
# "C:\Data\SS3\2019\TigerFlathead2019\RetrospectivesMan\Retro-5"
# change file control_modified.ss
# having run it autmatically before to set up teh structure

# Collect output information for each of the retrospective models:
#retro_mods <- SSgetoutput(dirvec=paste0(folder, '/', model, 'Retrospectives/Retro', years), getcovar=hessian)
#retro_mods <- SSgetoutput(dirvec=paste0(folder, '/RetrospectivesMan/Retro', years), getcovar=hessian)


# Summarise information collected above:
retro_summary <- SSsummarize(retro_mods)

# Create a list of end years, one for for each model:
end_years <- retro_mods[[1]]$endyr + years

# Create a list of cohorts, relative to last year with recruitment estimation:
#end_rec_year <- retro_mods[[1]]$recruit$Yr[max(which(retro_mods[[1]]$recruit$era=="Main"))]
end_rec_year <- retro_mods[[1]]$recruit$year[max(which(retro_mods[[1]]$recruit$era=="Main"))]
retro_cohorts <- (end_rec_year - n_cohorts):end_rec_year

# Create a folder to print compare plots:
#plot_dir <- file.path(folder, "Retrospectives/Plots") 
#plot_dir <- file.path(folder, "Retrospectives/Plots")
#dir.create(plot_dir)

# Create list of retrospective model names:
retro_names <- paste0(model, "_", end_years)

# Print compare plots as PNG files, and to screen:
for (iii in c(1,3,7)){
  SSplotComparisons(retro_summary, 
	endyrvec=end_years, 
	btarg=biomass_target, 
	minbthresh=biomass_threshold, 
	subplots=iii, #1:20 #seeral errors, at least some related to not doing Hessian
	spacepoints=5, 
	staggerpoints=1, 
	plot=show_plots, 
	print=print_plots, 
	new=TRUE, 
	legendlabels=retro_names, 
	plotdir=folder %&% model #plot_dir
)
}
# Print retrospective recruitment plot (a 'Squid Plot') as a PNG file:
png(file.path(plot_dir, "/Retro_Recruit_Devs.png"), width=7, height=7, units="in", res=300)

SSplotRetroRecruits(retro_summary,
	endyrvec=end_years,
	cohorts=retro_cohorts,
	relative=TRUE,
	labelyears=TRUE,
	legend=FALSE,
)

dev.off()

#------------------------------------------------------------------
# EOF.

#Hyperdepletion plot
x <- c(0:100)/100
plot(x,x, type='l', xlab="Numbers", ylab="CPUE = QN^(1+H)")
lines(x,x^(1+0.5), col=2)
lines(x,x^(1+0.25), col=3)
lines(x,x^(1-0.25), col=4)
lines(x,x^(1-0.5), col=5)

#lines(x,x^exp(-0.2), col=2)
#lines(x,x^exp(-0.1), col=3)
#lines(x,x^exp(0.1), col=4)
#lines(x,x^exp(0.2), col=5)



