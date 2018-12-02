library(tidyverse)
library(rhdf5)
library(MASS)
library(survival)
library(survminer)
library(coxme)
library(KMsurv)
library(muhaz)

# Exploratory Data Analysis

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


## Comparison of ROIs
Region1 = c(560, 420)
Region2 = c(850, 420)
Region3 = c(720, 590)
Region4 = c(720, 740)
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
                                                DataFraFirst$y >= Region4[2] & DataFraFirst$y <= Region4[2] + YwinSize, "Else", "Else")))))


##
refPoint = c(800, 600)
DataFraFirst$DistFromCent = sqrt((DataFraFirst$x - refPoint[1])^2 + (DataFraFirst$y - refPoint[2])^2)

ggplot(DataFraFirst, aes(DistFromCent, Duration)) +
  geom_point() +
  xlim(0, 1000) +
  theme_classic()

facet_grid(SUBJECTINDEX ~ .) +


Dist_surv = Surv(time = DataFraFirst$Duration, 
                 event = DataFraFirst$UnCen)

Fit = survfit(Dist_surv ~ DistFromCent, data = DataFraFirst)
ggsurvplot(Fit, data = DataFraFirst, pval = TRUE)


distAFT = with(DataFraFirst, Dist_surv)
RegAFTdistWei = survreg(distAFT ~ 1 + DistFromCent, 
                    dist="weibull", data = DataFraFirst)
summary(RegAFTdistWei)

Part_surv = Surv(time = DataFraFirst$Duration, 
                 event = DataFraFirst$UnCen)

Fit = survfit(Part_surv ~ Region, data = DataFraFirst)
ggsurvplot(Fit, data = DataFraFirst, pval = TRUE)



## 
DataFraFirst$Region %>% table
Part_surv = Surv(time = DataFraFirst$Duration, 
                 event = DataFraFirst$UnCen)

Fit = survfit(Part_surv ~ Region, data = DataFraFirst)
ggsurvplot(Fit, data = DataFraFirst, pval = TRUE)


Data_i = DataFraFirst[DataFraFirst$SUBJECTINDEX==(i+5),]
Data_i$Region %>% table
Part_surv_i = Surv(time = Data_i$Duration, 
                   event = Data_i$UnCen)

Fit_i = survfit(Part_surv_i ~ Region, data = Data_i)
ggsurvplot(Fit_i, data = Data_i, pval = TRUE)

i = i + 1

DataFraFirst$SUBJECTINDEX %>% table



##

DataFraFirst = data.frame(DataFraFirst, 
                      faceCNT=ave(rep(1,length(DataFraFirst$SUBJECTINDEX)),
                                  DataFraFirst$SUBJECTINDEX,
                                  DataFraFirst$filenumber,
                                  FUN = cumsum))


###

ggplot(DataFraFirst, aes(x=faceCNT, y=Duration, color=factor(SUBJECTINDEX))) +
  geom_point() +
  theme_classic()

DataFraFirst %>% 

DataFraFirst %>% filter(SUBJECTINDEX==6) %>% dplyr::select(filenumber) %>% table

DataFraFirst %>% 

DataFraFirst %>% filter(SUBJECTINDEX==6) %>%
  dplyr::select(filenumber)

fct_lump(DataFraFirst$filenumber) %>% levels; fct_lump(gss_cat$relig, n=5) %>% levels;

###


###
DataFraFirst



## X not perp C but X perp C when conditioning on participants

##

kernFitwhole =
  with(DataFraFirst, muhaz(times=Duration, delta=UnCen, kern="epanechnikov", bw.grid=100))

plot(kernFitwhole, xlim=c(-10,1500), ylim=c(-0.001, 0.01), main='hazard')



plot( with(DataFraFirst, muhaz(times=Duration, delta=UnCen, kern="epanechnikov", bw.grid=100)),
      xlim=c(-10,1500), ylim=c(-0.001, 0.04), main='hazard')

for (i in 2:29)
{
  Data_i = DataFraFirst[DataFraFirst$SUBJECTINDEX==(i+5),]
  lines(with(Data_i, muhaz(times=Duration, delta=UnCen, kern="epanechnikov", bw.grid=100)), 
        col=i)
}



dataAFT = with(DataFraFirst, Surv(time=Duration, event=UnCen, type="right"))
RegAFTwei = survreg(dataAFT ~ 1 + Region + Duration, 
                    dist="loglogistic", data = DataFraFirst)
summary(RegAFTwei)


my.fit = survfit(dataAFT~1);
my.fit.summ = summary(my.fit)
obs.time = my.fit.summ$time ; # t_i
n.risk = my.fit.summ$n.risk ; # Y_i
n.event = my.fit.summ$n.event ; # d_i
KM.surv = my.fit.summ$surv ;    #\hat{S}(t_i)
incr = n.event / n.risk ; # d_i / y_i
NA.cumhzd = NULL ; # initialize
for (i in 1:length(obs.time)) NA.cumhzd[i] = sum(incr[1:i]) ;

log.time=log(obs.time); H=NA.cumhzd;
plot(log.time, log(exp(H)-1), type="l", lty=1, main="Is log-logistic?", col="Blue");
abline(coef(lm(log(exp(H)-1)~log.time)), col="red")


plot(log.time,log(H),type="l",lty=1,main="Is Weibull?", col ="Blue" ) ;
abline(coef(lm(log(H)~log.time)), col="red")


##
DataFraFirst %>% group_by(SUBJECTINDEX, Region) %>%
  summarise(cnt = n(),
            medDur = median(Duration),
            meanDur = mean(Duration),
            sdDur = sd(Duration),
            numCen = sum(UnCen==0)
            ) %>% print(n=29)


## Two-sample testing

test_kidney = survdiff(survobj_kidney ~ as.factor(kidney$type), rho=0)
test_kidney = survdiff(survobj_kidney ~ kidney$type, rho=0) ;
test_kidney = survdiff(survobj_kidney ~ type, rho=0, data=kidney) ;


##
Part_surv = Surv(time = DataFraFirst[DataFraFirst$SUBJECTINDEX==6,]$Duration, 
                 event = DataFraFirst[DataFraFirst$SUBJECTINDEX==6,]$UnCen)

Fit = survfit(Part_surv ~ Region, data = DataFraFirst[DataFraFirst$SUBJECTINDEX==6,])
ggsurvplot(Fit, data = DataFraFirst[DataFraFirst$SUBJECTINDEX==6,], pval = TRUE)

splots = list()

for (i in 1:29)
{
  Data_i = DataFraFirst[DataFraFirst$SUBJECTINDEX==(i+5),]
  Part_surv = Surv(time = Data_i$Duration, 
                   event = Data_i$UnCen)
  
  Fit = survfit(Part_surv ~ Region, data = Data_i)
  splots[[i]] = ggsurvplot(Fit, data = Data_i, 
                           pval = TRUE)
}

arrange_ggsurvplots(splots, print = TRUE,
                    ncol = 6, nrow = 5)


summary(Fit)

DataFraFirst %>% group_by(SUBJECTINDEX) %>%
  ggsurvplot(Fit, pval = TRUE) + facet_wrap(~SUBJECTINDEX, scales = 'free_x')
  
boxplot(NUMS ~ GRP, data = ddf, lwd = 2, ylab = 'NUMS')
stripchart(NUMS ~ GRP, vertical = TRUE, data = ddf, 
           method = "jitter", add = TRUE, pch = 20, col = 'blue') 


meanDur = DataFraFirst %>% group_by(SUBJECTINDEX) %>%
  summarise(meanDur = mean(Duration)) %>% 
  arrange(meanDur) %>% pull(SUBJECTINDEX)

DataFraFirst = DataFraFirst %>%
  mutate(mDur = ifelse(SUBJECTINDEX %in% meanDur[1:14], "short", "long"))

DataFraFirst = DataFraFirst %>%
  mutate(Ord = which(SUBJECTINDEX == meanDur))

DataFraFirst$Ord = sapply(DataFraFirst$SUBJECTINDEX, function(x){which(x == meanDur)})

DataFraFirst$Ord %>% table
which(6 == meanDur)


qplot(Region, Duration, col = Ord,
      data=DataFraFirst, geom=c("boxplot")) +
  geom_point(alpha = 0.2) + 
  geom_jitter(shape=16, position=position_jitter(0.2)) + 
  theme_classic()

p + geom_jitter(shape=16, position=position_jitter(0.2))




pm_select %>% gather() %>% ggplot(aes(value))+
  geom_histogram(bins=15) + facet_wrap(~key, scales = 'free_x') +
  ggtitle("Histogram of continuous variables")

##
Region_surv = Surv(time = DataFraFirst$Duration, 
                   event = DataFraFirst$UnCen)
(Fit = survfit(Region_surv ~ SUBJECTINDEX + Region,
               data = DataFraFirst))

##
Region_surv = Surv(time = DataFraFirst$Duration, 
                   event = DataFraFirst$UnCen)
coxme(Region_surv ~ Region + (1 | SUBJECTINDEX), 
      data = DataFraFirst)


##
CountPic = data.frame(DataFraFirst, 
                      faceCNT=ave(rep(1,length(DataFraFirst$SUBJECTINDEX)),
                                  DataFraFirst$SUBJECTINDEX,
                                  DataFraFirst$filenumber,
                                  FUN = cumsum))

CountPic = CountPic[CountPic$faceCNT==1,]

Count_surv = Surv(time = CountPic$Duration, 
                  event = CountPic$UnCen)
coxme(Count_surv ~ Region + (1 | SUBJECTINDEX), 
      data = CountPic)


##
DataFraFirst
DataEyeL = with(DataFraFirst, DataFraFirst[Region=='EyeL',])
DataEyeR = with(DataFraFirst, DataFraFirst[Region=='EyeR',])
DataNose = with(DataFraFirst, DataFraFirst[Region=='Nose',])
DataMouth = with(DataFraFirst, DataFraFirst[Region=='Mouth',])

lambda = sum(bmt1$d3)/sum(bmt1$t2)
print(lambda)
curve( exp(-lambda*x), from=0, to=max(bmt1$t2)+1, ylab='S(t)', xlab='time', main='survival function via a parametric approach')

DataEyeL_kernel = with(DataNose, 
                        muhaz(times=Duration, 
                              delta=UnCen, 
                              kern="epanechnikov", bw.grid=100))
plot( DataEyeL_kernel,  xlim=c(-10,1500), main='hazard')

##



##
DataFraFirst = DataFraFirst[DataFraFirst$Region %in% c("EyeL", "EyeR", "Nose", "Mouth"),]
DataAFT = Surv(time=DataFraFirst$Duration, 
               event=DataFraFirst$UnCen, type="right")

RegAFT_Wei = survreg(DataAFT ~ 1 + factor(Region) + Duration, dist="weibull", 
                     data = DataFraFirst)
summary(RegAFT_Wei)

plot(log.time,log(H),type="l",lty=1,main="Is Weibull?", col ="Blue" ) ;
abline(coef(lm(log(H)~log.time)), col="red")


RegAFT_LL = survreg(DataAFT ~ 1 + factor(Region) + Duration, dist="loglogistic", 
                    data = DataFraFirst)
summary(RegAFT_LL)


MyFit = survfit(DataAFT ~ 1)
MyFitSum = summary(MyFit)
obs.time = MyFitSum$time ; # t_i
n.risk = MyFitSum$n.risk ; # Y_i
n.event = MyFitSum$n.event ; # d_i
KM.surv = MyFitSum$surv ;    #\hat{S}(t_i)
incr = n.event / n.risk ; # d_i / y_i

NA.cumhzd = NULL ; # initialize
for (i in 1:length(obs.time)) NA.cumhzd[i] = sum(incr[1:i]) ;
log.time=log(obs.time); H=NA.cumhzd;


plot(log.time,log(H),type="l",lty=1,main="Is Weibull?", col ="Blue" ) ;
abline(coef(lm(log(H)~log.time)), col="red")

plot(log.time, log(exp(H)-1), type="l", lty=1, main="Is log-logistic?", col="Blue");
abline(coef(lm(log(exp(H)-1)~log.time)), col="red")


plot(density(DataFraFirst$start))

(DataFraFirst$start > 1463)


hzd_est_EyeL <- DataEyeL_kernel$haz.est
CumHazEyeL <- cumsum(hzd_est_EyeL)*11.67

plot(DataEyeL_kernel$est.grid, hzd_est_EyeL, 
     main='hazard estimates via kernel', type='l')


##
BaseSurv = Surv(DataFraFirst$Duration,
                event = DataFraFirst$UnCen)

BaseFit = survfit(BaseSurv ~ Region, data=DataFraFirst)
ggsurvplot(BaseFit, data=DataFraFirst, pval=T)

DataFraFirst %>% group_by(SUBJECTINDEX, filenumber) %>% summarise(cnt=n())

setwd("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/")
write_csv(DataFraFirst, "../Dropbox/2018Autumn/GradThesis/EyeTracking_data/Data.csv")


##




pval = NULL

for (i in seq(-1,1,by=.05))
{
  A = survdiff(Data_surv ~ DataFraFirst_ROI$Region, rho=i)
  pval = c(pval, 1 - pchisq(A$chisq, length(A$n) - 1))
}

plot(seq(-1,1,by=.05), pval)

DataFraFirst_ROI = DataFraFirst %>%
  dplyr::filter(Region != "Else")




## show model results
Fit1 = survfit(Data_surv ~ filenumber, data = DataFraFirst_ROI)
Fit2 = survfit(Data_surv ~ filenumber, data = DataFraFirst_ROI)

ggsurvplot(Fit1, data = DataFraFirst_ROI, pval = TRUE)
ggsurvplot(Fit2, data = DataFraFirst_ROI, pval = TRUE)

DataFraFirst_ROI %>% names


DataFraFirst_ROI


## Survival analysis
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
