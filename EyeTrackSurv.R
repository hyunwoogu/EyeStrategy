library(tidyverse)
library(rhdf5)
library(MASS)
library(survival)
library(survminer)

## Load the data
h5ls("../Dropbox/2018Autumn/GradThesis/etdb_v1.0.hdf5")
Data = h5read("../Dropbox/2018Autumn/GradThesis/etdb_v1.0.hdf5", "/Face Discrim.")

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

## Survival analysis
Data_surv = Surv(time = DataFraROI$Duration, event = DataFraROI$Censor)

Fit = survfit(Data_surv ~ fixation, data = DataFraROI)
summary(Fit)
ggsurvplot(Fit, data = DataFraROI, pval = TRUE)

