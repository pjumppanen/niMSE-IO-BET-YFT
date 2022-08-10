library(readxl)
library(tidyverse)

Data =  rbind (cbind(subset(read.csv(make.filename(file = "Joint_regB2_R1_dellog_vessid_79nd_yq.csv",path=Paste(PATH_DATA,'CPUE\\trop_cl0_hb1_hk1_TW2005_discard2\\')),header=T,as.is=T), !is.na(pr),select=c('yq','pr','yr','qtr')),ModelAreaName='1'),
			   cbind(subset(read.csv(make.filename(file = "Joint_regB2_R2_dellog_vessid_79nd_yq.csv",path=Paste(PATH_DATA,'CPUE\\trop_cl0_hb1_hk1_TW2005_discard2\\')),header=T,as.is=T), !is.na(pr),select=c('yq','pr','yr','qtr')),ModelAreaName='2'),
			   cbind(subset(read.csv(make.filename(file = "Joint_regB2_R3_dellog_vessid_79nd_yq.csv",path=Paste(PATH_DATA,'CPUE\\temp_cl1_hb0_hk1_TW2005_discard2\\')),header=T,as.is=T), !is.na(pr),select=c('yq','pr','yr','qtr')),ModelAreaName='3'))

			   


betwts = list('2016'= c(1, 0.868406491487984,0.827594885758832),
              '7994 m8' = c(0.799+0.626,1.000,0.373+0.486),
 			  '8000 m8' = c(0.687+0.531,1.000,0.489+0.601))
				  
			


data = Data
data$pr_2016 = data$pr
data$pr_7994_m8 = data$pr
data$pr_8000_m8 = data$pr





index = data$ModelAreaName=='1'
work = data[index,]
work$pr_2016 = work$pr_2016/Mean(work[work$yr>=1981 & work$yr<= 2000,'pr_2016']) * betwts[['2016']][1]
work$pr_7994_m8 = work$pr_7994_m8/Mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * betwts[['7994 m8']][1]
work$pr_8000_m8 = work$pr_8000_m8/Mean(work[work$yr>=1980 & work$yr<= 2000,'pr_8000_m8']) * betwts[['8000 m8']][1]
data[index,]=work
index = data$ModelAreaName=='2'
work = data[index,]
work$pr_2016 = work$pr_2016/Mean(work[work$yr>=1981 & work$yr<= 2000,'pr_2016']) * betwts[['2016']][2]
work$pr_7994_m8 = work$pr_7994_m8/Mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * betwts[['7994 m8']][2]
work$pr_8000_m8 = work$pr_8000_m8/Mean(work[work$yr>=1980 & work$yr<= 2000,'pr_8000_m8']) * betwts[['8000 m8']][2]
data[index,]=work
index = data$ModelAreaName=='3'
work = data[index,]
work$pr_2016 = work$pr_2016/Mean(work[work$yr>=1981 & work$yr<= 2000,'pr_2016']) * betwts[['2016']][3]
work$pr_7994_m8 = work$pr_7994_m8/Mean(work[work$yr>=1979 & work$yr<= 1994,'pr_7994_m8']) * betwts[['7994 m8']][3]
work$pr_8000_m8 = work$pr_8000_m8/Mean(work[work$yr>=1980 & work$yr<= 2000,'pr_8000_m8']) * betwts[['8000 m8']][3]

data[index,]=work


	
##################  
# output for SS3
##################  

work = subset(data,ModelAreaName %in% c('1','2','3'))
work$fleet = ifelse(work$ModelAreaName =='1',13,
			 ifelse(work$ModelAreaName =='2',14,
			 ifelse(work$ModelAreaName =='3' & work$qtr==0.125,15, 
			 ifelse(work$ModelAreaName =='3' & work$qtr==0.375,16, 
			 ifelse(work$ModelAreaName =='3' & work$qtr==0.625,27, 
			 ifelse(work$ModelAreaName =='3' & work$qtr==0.875,18,0))))))
work$qtr = (work$qtr - 1/8) * 4 + 1
work$qtr =  yearqtr2qtr(work$yr,work$qtr,1952,101)
work$season = 1
work$cv = 0.3
			 
temp = work[work$yr>=1979,c('qtr','season','fleet','pr_2016','cv')]
temp = work[work$yr>=1979,c('qtr','season','fleet','pr_7994_m8','cv')]
temp = work[work$yr>=1979,c('qtr','season','fleet','pr_8000_m8','cv')]


	


