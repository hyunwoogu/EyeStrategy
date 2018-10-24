library(tidyverse)
library(rhdf5)
library(MASS)
library(survival)
library(survminer)

## Load the data
h5ls("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/etdb_v1.0.hdf5")
Data = h5read("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/etdb_v1.0.hdf5", "/Face Discrim.")

## Preprocessing
DataFra = data.frame(Data$SUBJECTINDEX, Data$trial, Data$filenumber, Data$isref, Data$start, Data$end, Data$x, Data$y)
DataFra = DataFra %>% mutate(Duration = Data.end - Data.start)
DataFra$Censor = (DataFra$Duration < 1500)

## mutate a variable 'counts'
Counts = c(1)
j = 1
for (i in 2:dim(DataFra)[1])
{
  if (all(DataFra[i-1, 1:2] == DataFra[i, 1:2])) {j = j+ 1
  }else {j = 1}
  Counts = c(Counts, j)
}

DataFra['fixation'] = Counts
DataFraROI = DataFra[DataFra$fixation %in% c(1,2,3,5,7), ]


##
names(DataFra)
ROI = c("Data.start", "Data.end", "Data.x", 
        "Data.y", "fixation")
Region1 = c(520, 390)
Region2 = c(720, 540)
winSize = 150

DataFra = DataFra[, ROI] %>% 
  filter(fixation == 1) %>%
  mutate(Duration = Data.end - Data.start,
         Hemifield = ifelse(Data.x <= 800, "L", "R"),
         UpperLower = ifelse(Data.y <= 500, "U", "L"),
         Region = ifelse(Data.x >= Region1[1] & Data.x <= Region1[1] + winSize &
                        Data.y >= Region1[2] & Data.y >= Region1[2] + winSize, "Eye",
                        ifelse(Data.x >= Region2[1] & Data.x <= Region2[1] + winSize &
                                 Data.y >= Region2[2] & Data.y >= Region2[2] + winSize, "Nose", "Else")),
         Censor = Data.end < 1450)


## Survival analysis
Data_surv = Surv(time = DataFra$Duration, 
                 event = DataFra$Censor)

Fit = survfit(Data_surv ~ Region, data = DataFra)
summary(Fit)
ggsurvplot(Fit, data = DataFra, pval = TRUE)


plot(Fit, fun="cloglog")


## 

## 

Sam = c(3,4,5,7,8,9,10)
Cen = c(1,0,1,1,0,0,1)
Suvv = Surv(time = Sam, event=Cen)
SuvvFit = survfit(Suvv ~ 1, Suvv) 
summary(SuvvFit)
SuvvFit$time

cumsum(SuvvFit$n.event/SuvvFit$n.risk)

(1-1/7 ) * (4/5)
