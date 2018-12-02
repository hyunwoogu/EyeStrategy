library(tidyverse)
library(rhdf5)
library(MASS)
library(survival)
library(survminer)
library(coxme)
library(KMsurv)
library(muhaz)

# Preprocessing

#++++++++++++++++++++++++++++
# Load the data
h5ls("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/etdb_v1.0.hdf5")
Data = h5read("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/etdb_v1.0.hdf5", "/Face Discrim.")

# Preprocessing
DataFra = data.frame(SUBJECTINDEX=Data$SUBJECTINDEX, 
                     trial = Data$trial, 
                     filenumber = Data$filenumber,
                     start = Data$start, 
                     end = Data$end, 
                     x = Data$x, 
                     y = Data$y)

DataFra$SUBJECTINDEX = match(DataFra$SUBJECTINDEX, unique(DataFra$SUBJECTINDEX))

DataFra = DataFra %>% mutate(Duration = end - start)
DataFra = data.frame(DataFra, count=ave(rep(1,length(DataFra$trial)),
                                        DataFra$SUBJECTINDEX,
                                        DataFra$trial,
                                        FUN = cumsum))

DataFra[DataFra$count == 1 & DataFra$end > 1400, ]

DataFraSecond = DataFra[DataFra$count == 2,]

rownames( DataFraSecond) [!rownames( DataFraSecond) %in% 
                            as.character(as.numeric(rownames( DataFraFirst)) + 1) ]


ggplot(DataFraFirst, aes(x=start, y=Duration, fill=as.factor(SUBJECTINDEX))) + geom_point()


## First Fixations
DataFra[DataFra$count == 1 & DataFra$end > 1450, 'Duration'] %>% density %>% plot
DataFraFirst = DataFra[DataFra$count == 1,]
DataFraFirst = DataFraFirst %>% mutate(UnCen = (end < 1450))


## Comparison of ROIs : Symmetric
Region1 = c(575, 420)
Region2 = c(865, 420)
Region3 = c(720, 590)
Region4 = c(720, 740)
XwinSize = 160
YwinSize = 160

DataFraFirst = DataFraFirst %>% 
  mutate(Region = ifelse(DataFraFirst$x >= Region1[1] & DataFraFirst$x <= Region1[1] + XwinSize &
                         DataFraFirst$y >= Region1[2] & DataFraFirst$y <= Region1[2] + YwinSize, "EyeL",
                         ifelse(DataFraFirst$x >= Region2[1] & DataFraFirst$x <= Region2[1] + XwinSize &
                                DataFraFirst$y >= Region2[2] & DataFraFirst$y <= Region2[2] + YwinSize, "EyeR", 
                                ifelse(DataFraFirst$x >= Region3[1] & DataFraFirst$x <= Region3[1] + XwinSize &
                                       DataFraFirst$y >= Region3[2] & DataFraFirst$y <= Region3[2] + YwinSize, "Nose",
                                       ifelse(DataFraFirst$x >= Region4[1] & DataFraFirst$x <= Region4[1] + XwinSize &
                                                DataFraFirst$y >= Region4[2] & DataFraFirst$y <= Region4[2] + YwinSize, "Else", "Else")))))


table(DataFraFirst$SUBJECTINDEX, DataFraFirst$Region)
DataFraFirst$Region %>% table