library(tidyverse)
library(rhdf5)
library(MASS)
library(survival)
library(survminer)

## Load the data
h5ls("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/etdb_v1.0.hdf5")
Data = h5read("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/etdb_v1.0.hdf5", "/Face Discrim.")

## Preprocessing
DataFra = data.frame(SUBJECTINDEX=Data$SUBJECTINDEX, 
                     trial = Data$trial, 
                     filenumber = Data$filenumber,
                     start = Data$start, 
                     end = Data$end, 
                     x = Data$x, 
                     y = Data$y)
DataFra = DataFra %>% mutate(Duration = end - start)
DataFra = data.frame(DataFra, count=ave(rep(1,length(DataFra$trial)),
                                        DataFra$SUBJECTINDEX,
                                        DataFra$trial,
                                        FUN = cumsum))

DataFra[DataFra$count == 1 & DataFra$end > 1400, ]

##
DataFraFirst = DataFra[DataFra$count == 1,]
DataFraFirst = DataFraFirst %>% mutate(Cen = (end > 1450))

##
DataFra[DataFra$count == 1 & DataFra$end > 1450, 'Duration'] %>% density %>% plot
DataFra[count == 1, 'start'] %>% mean(na.rm=T)


##
names(DataFraFirst)
ROI = c("Data.start", "Data.end", "Data.x", 
        "Data.y", "fixation")
Region1 = c(550, 410)
Region2 = c(550, 570)
XwinSize = 500
YwinSize = 150

DataFraFirst = DataFraFirst %>% 
  mutate(Region = ifelse(DataFraFirst$x >= Region1[1] & DataFraFirst$x <= Region1[1] + XwinSize &
                         DataFraFirst$y >= Region1[2] & DataFraFirst$y <= Region1[2] + YwinSize, "Eye",
                         ifelse(DataFraFirst$x >= Region2[1] & DataFraFirst$x <= Region2[1] + XwinSize &
                                DataFraFirst$y >= Region2[2] & DataFraFirst$y <= Region2[2] + YwinSize, "Mouth", "Else")))

plot(density(DataFraFirst$y))

DataFraFirst$Region %>% table

data(kidney)
kidney #str, dim check
survobj.kidney = Surv(time = kidney$time, event = kidney$delta) ;
survobj.kidney;

DataFraFirst$Region %>% table

DataFraFirst_ROI = DataFraFirst %>%
  dplyr::filter(Region != "Else")

res = NULL

for (i in seq(-1,1,by=.05))
{
  A = survdiff(Data_surv ~ DataFraFirst_ROI$Region, rho=i)
  res = c(res, 1 - pchisq(A$chisq, length(A$n) - 1))
}

plot(seq(-1,1,by=.05), res)

DataFraFirst_ROI = DataFraFirst %>%
  dplyr::filter(Region != "Else")

## Survival analysis

DataFraFirst_ROI = DataFraFirst %>%
  dplyr::filter(Region != "Else")


Data_surv = Surv(time = DataFraFirst_ROI$Duration, 
                 event = DataFraFirst_ROI$UnCen)

Fit = survfit(Data_surv ~ Region, data = DataFraFirst_ROI)
summary(Fit)
ggsurvplot(Fit, data = DataFraFirst_ROI, pval = TRUE)


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
