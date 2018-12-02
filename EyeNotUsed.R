# Another Data
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Load the data
h5ls("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/etdb_v1.0.hdf5")
Data = h5read("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/etdb_v1.0.hdf5", "/Face Discrim.")

## Another Dataset Analysis (Do Not Run : Not applicable)
AnaDataFra %>% filter(SUBJECTINDEX %in% unique(DataFra$SUBJECTINDEX))

AnaData = h5read("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/etdb_v1.0.hdf5", "/Face Learning")
AnaDataFra = data.frame(SUBJECTINDEX=AnaData$SUBJECTINDEX[1,], 
                        trial = AnaData$trial[1,], 
                        filenumber = AnaData$filenumber[1,], 
                        start = AnaData$start[1,], 
                        end = AnaData$end[1,], 
                        x = AnaData$x[1,], 
                        y = AnaData$y[1,],
                        oddball = AnaData$oddball[1,],
                        ucs = AnaData$ucs[1,])

A = AnaDataFra %>% filter(SUBJECTINDEX %in% unique(DataFra$SUBJECTINDEX))  %>%
  group_by(SUBJECTINDEX) %>% summarise(meanDuration = mean(Duration)) %>% pull(meanDuration)

B = DataFra %>% group_by(SUBJECTINDEX) %>% summarise(meanDuration = mean(Duration)) %>% pull(meanDuration)

plot(A, B) # Different Participants! -> thus not applicable! :(



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Mixed Effect
coxme(Part_surv ~ 
        age + sex + transplant + (1 | ID), data = DataFraFirst)



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Next Fixations
DataFra = DataFra %>% 
  mutate(Region = ifelse(DataFra$x >= Region1[1] & DataFra$x <= Region1[1] + XwinSize &
                           DataFra$y >= Region1[2] & DataFra$y <= Region1[2] + YwinSize, "EyeL",
                         ifelse(DataFra$x >= Region2[1] & DataFra$x <= Region2[1] + XwinSize &
                                  DataFra$y >= Region2[2] & DataFra$y <= Region2[2] + YwinSize, "EyeR", 
                                ifelse(DataFra$x >= Region3[1] & DataFra$x <= Region3[1] + XwinSize &
                                         DataFra$y >= Region3[2] & DataFra$y <= Region3[2] + YwinSize, "Nose",
                                       ifelse(DataFra$x >= Region4[1] & DataFra$x <= Region4[1] + XwinSize &
                                                DataFra$y >= Region4[2] & DataFra$y <= Region4[2] + YwinSize, "Else", "Else")))))

Data1 = DataFra[DataFra$count==1, ]
Data2 = DataFra[DataFra$count==2, ]

test = Data1 %>% left_join(Data2, by=c("SUBJECTINDEX" = "SUBJECTINDEX", 
                                       "trial" = "trial"))

DataFraFirst = DataFraFirst %>% 
  mutate(constant = apply(test[,c("Region", "Region2")], 1, function(x) {x[1]==x[2]}))

data.m = melt(test[,c("SUBJECTINDEX", "Region", "Region2")], id.vars='SUBJECTINDEX')
data.m %>% group_by(SUBJECTINDEX, variable, value) %>% summarise(cnt = n())  %>% 
  ggplot(aes(x=value, y=cnt)) + geom_bar(aes(fill=variable), position="dodge", stat="identity")

ggplot(test, aes(x=Region))
geom_bar(aes(fill = variable), position = "dodge", stat="identity")




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Frailty Model
m3 <- coxph(Surv(time1, time2, mortality) ~ age + sex + transplant + frailty(ID, 
                                                                             distribution = "gaussian", sparse = FALSE, method = "reml"), data = dat)





#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Unclassified

## Survival of first fixations of patients
Part_surv = Surv(time = DataFraFirst$Duration, 
                 event = DataFraFirst$UnCen)

Fit = survfit(Part_surv ~ SUBJECTINDEX, data = DataFraFirst)
ggsurvplot(Fit, data = DataFraFirst, pval = TRUE)


unique(DataFraFirst$SUBJECTINDEX)[match(unique(DataFraFirst$SUBJECTINDEX), 8) >= .5]


## Summary statistics

DataFraFirst %>% group_by(SUBJECTINDEX) %>% dplyr::summarise(Censored = sum(UnCen==FALSE))
Surv(time=DataFraFirst_i$Duration, event=DataFraFirst_i$UnCen)





## Load the data
h5ls("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/etdb_v1.0.hdf5")
Data = h5read("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/etdb_v1.0.hdf5", "/Face Discrim.")
AnaData = h5read("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/etdb_v1.0.hdf5", "/Face Learning")

DatDat = data.frame(SUBJECTINDEX=AnaData$SUBJECTINDEX, 
                    trial = AnaData$trial, 
                    filenumber = AnaData$filenumber,
                    start = AnaData$start, 
                    end = AnaData$end, 
                    x = AnaData$x, 
                    y = AnaData$y)

DatDat %>% group_by(SUBJECTINDEX, filenumber) %>%
  summarise(meanX = mean(x))

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
Fit1 = survfit(Data_surv ~ filenumber, data = DataFraFirst_ROI)
Fit2 = survfit(Data_surv ~ filenumber, data = DataFraFirst_ROI)



##
Part_surv = Surv(time = DataFraFirst_ROI$Duration, 
                 event = DataFraFirst_ROI$UnCen)

Fit1 = survfit(Data_surv ~ filenumber, data = DataFraFirst_ROI)
Fit2 = survfit(Data_surv ~ filenumber, data = DataFraFirst_ROI)

ggsurvplot(Fit1, data = DataFraFirst_ROI, pval = TRUE)
ggsurvplot(Fit2, data = DataFraFirst_ROI, pval = TRUE)

DataFraFirst_ROI %>% names


DataFraFirst_ROI



## First Fixations
DataFraFirst = DataFra[DataFra$count == 1,]
DataFraFirst = DataFraFirst %>% mutate(UnCen = (end < 1450))

DataFra[DataFra$count == 1 & DataFra$end > 1450, 'Duration'] %>% density %>% plot
DataFra[count == 1, 'start'] %>% mean(na.rm=T)


## Comparison of ROIs
CountPic = data.frame(DataFraFirst, 
                      faceCNT=ave(rep(1,length(DataFraFirst$SUBJECTINDEX)),
                                  DataFraFirst$SUBJECTINDEX,
                                  DataFraFirst$filenumber,
                                  FUN = cumsum))
CountPic = CountPic %>% 
  mutate(First = (faceCNT==1))

DataFraFirst = CountPic[CountPic$faceCNT==1,]

Region1 = c(560, 420)
Region2 = c(850, 420)
Region3 = c(720, 590)
Region4 = c(720, 800)
XwinSize = 150
YwinSize = 150

DataFraFirst = DataFraFirst %>% 
  mutate(Region = ifelse(DataFraFirst$x >= Region1[1] & DataFraFirst$x <= Region1[1] + XwinSize &
                           DataFraFirst$y >= Region1[2] & DataFraFirst$y <= Region1[2] + YwinSize, "EyeL",
                         ifelse(DataFraFirst$x >= Region2[1] & DataFraFirst$x <= Region2[1] + XwinSize &
                                  DataFraFirst$y >= Region2[2] & DataFraFirst$y <= Region2[2] + YwinSize, "EyeR", 
                                ifelse(DataFraFirst$x >= Region3[1] & DataFraFirst$x <= Region3[1] + XwinSize &
                                         DataFraFirst$y >= Region3[2] & DataFraFirst$y <= Region3[2] + YwinSize, "Nose",
                                       ifelse(DataFraFirst$x >= Region4[1] & DataFraFirst$x <= Region4[1] + XwinSize &
                                                DataFraFirst$y >= Region4[2] & DataFraFirst$y <= Region4[2] + YwinSize, "Mouth", "Else")))))

DataFraFirst$Region %>% table

DataFraFirst = DataFraFirst %>% 
  mutate(Region = ifelse(DataFraFirst$x >= Region1[1] & DataFraFirst$x <= Region1[1] + XwinSize &
                           DataFraFirst$y >= Region1[2] & DataFraFirst$y <= Region1[2] + YwinSize, "Eye",
                         ifelse(DataFraFirst$x >= Region2[1] & DataFraFirst$x <= Region2[1] + XwinSize &
                                  DataFraFirst$y >= Region2[2] & DataFraFirst$y <= Region2[2] + YwinSize, "Nose", 
                                ifelse(DataFraFirst$x >= Region3[1] & DataFraFirst$x <= Region3[1] + XwinSize &
                                         DataFraFirst$y >= Region3[2] & DataFraFirst$y <= Region3[2] + YwinSize, "Mouth", "Else"))))

DataFraFirst$Region %>% table



BaseSurv = Surv(DataFraFirst$Duration,
                event = DataFraFirst$UnCen)

BaseFit = survfit(BaseSurv ~ Region, data=DataFraFirst)
ggsurvplot(BaseFit, data=DataFraFirst, pval=T)

DataFraFirst %>% group_by(SUBJECTINDEX, filenumber) %>% summarise(cnt=n())



pval = NULL

for (i in seq(-1,1,by=.05))
{
  A = survdiff(Data_surv ~ DataFraFirst_ROI$Region, rho=i)
  pval = c(pval, 1 - pchisq(A$chisq, length(A$n) - 1))
}

plot(seq(-1,1,by=.05), pval)

DataFraFirst_ROI = DataFraFirst %>%
  dplyr::filter(Region != "Else")



## Survival analysis
DataFraFirst_ROI %>% 
  group_by(SUBJECTINDEX, filenumber) %>%
  summarise(CNT = n(),
            MeanRes = mean(Duration),
            UnCens = mean(UnCen)) %>%
  filter(UnCens != 1) %>% print(n=52)


CountPic = data.frame(DataFraFirst_ROI, 
                      faceCNT=ave(rep(1,length(DataFraFirst_ROI$SUBJECTINDEX)),
                                  DataFraFirst_ROI$SUBJECTINDEX,
                                  DataFraFirst_ROI$filenumber,
                                  FUN = cumsum))
CountPic = CountPic %>% 
  mutate(First = (faceCNT==1))

FirstPic = FirstPic[FirstPic$faceCNT==1,]
table(FirstPic$SUBJECTINDEX)
FirstPic$UnCen %>% length

sum(FirstPic$UnCen==0)

FP_surv = Surv(time = FirstPic$Duration, 
               event = FirstPic$UnCen)

FP_Fit = survfit(FP_surv ~ filenumber, data = FirstPic)
summary(Fit)
ggsurvplot(FP_Fit, data = FirstPic, pval = TRUE)

CNT_surv = Surv(time = CountPic$Duration,
                event = CountPic$UnCen)

CNT_Fit = survfit(CNT_surv ~ First, data=CountPic)
ggsurvplot(CNT_Fit, data = CountPic, pval=T)

## 
Sam = c(3,4,5,7,8,9,10)
Cen = c(1,0,1,1,0,0,1)
Suvv = Surv(time = Sam, event=Cen)
SuvvFit = survfit(Suvv ~ 1, Suvv) 
summary(SuvvFit)
SuvvFit$time

cumsum(SuvvFit$n.event/SuvvFit$n.risk)



##

