library(tidyverse)
library(rhdf5)
library(MASS)

## Load the data
h5ls("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/etdb_v1.0.hdf5")
Data = h5read("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/etdb_v1.0.hdf5", "/Face Discrim.")

## Preprocessing
DataFra = data.frame(Data$SUBJECTINDEX, Data$trial, Data$filenumber, Data$isref, Data$start, Data$end, Data$x, Data$y)
DataFra = DataFra %>% mutate(Duration = Data.end - Data.start)

## mutate a variable 'counts' (WARNING : running time)
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




## Bayesian updating : any ideas?
DataFra$Data.x[DataFra$fixation == 1] %>% density %>% plot
DataFra$Data.y[DataFra$fixation == 1] %>% density %>% plot

DataFra$Data.x[DataFra$fixation == 4] %>% density %>% plot
DataFra$Data.y[DataFra$fixation == 2] %>% density %>% plot



