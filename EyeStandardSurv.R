library(tidyverse)
library(rhdf5)
library(MASS)
library(survival)
library(survminer)
library(coxme)
library(KMsurv)
library(muhaz)
library(plyr)



unique(DataFraFirst$SUBJECTINDEX)[match(unique(DataFraFirst$SUBJECTINDEX), 8) >= .5]

## Summary statistics
DataFraFirst %>% group_by(SUBJECTINDEX) %>% dplyr::summarise(Censored = sum(UnCen==FALSE))


Surv(time=DataFraFirst_i$Duration, event=DataFraFirst_i$UnCen)






match(DataFraFirst$SUBJECTINDEX, unique(DataFraFirst$SUBJECTINDEX))


DataFraFirst$SUBJECTINDEX %>% table

ggsurvplot(fit=my_fit_i, data=DataFraFirst_i)
DataFraFirst_i %>% group_by(SUBJECTINDEX) %>% summarise(NumCen = sum(UnCen!=1))


survobj.aft = Surv(time=DataFraFirst_i$Duration, 
                   event=DataFraFirst_i$UnCen)

i = 8
i = i + 1
DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i)
Fit_ph = coxph(survobj.aft ~  as.factor(DataFraFirst_i$Region) + DataFraFirst_i$start)
summary(Fit_ph)

i = i + 1
DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i)
survobj.aft = Surv(time=DataFraFirst_i$Duration, 
                   event=DataFraFirst_i$UnCen)
regobj.aft = survreg(survobj.aft ~ 1 + as.factor(DataFraFirst_i$Region) + 
                       DataFraFirst_i$start, dist="weibull") ;
summary(regobj.aft)


survobj.aft = Surv(time=DataFraFirst$Duration, 
                   event=DataFraFirst$UnCen)

regobj.aft = survreg(survobj.aft ~ 1 + as.factor(DataFraFirst$Region) + 
                     DataFraFirst$start + as.factor(DataFraFirst$SUBJECTINDEX),
                     data=DataFraFirst,
                     dist="weibull")

Fit_ph = coxph(survobj.aft ~  as.factor(DataFraFirst$Region) + 
                 DataFraFirst$start + as.factor(DataFraFirst$SUBJECTINDEX))


summary(regobj.aft)
summary(Fit_ph)




# Censoring : just the table?

#++++++++++++++++++++++++
# Survival plot by participants
survPlotMaker = function(i)
{
  DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i)
  my_surv_i = Surv(time=DataFraFirst_i$Duration,
                   event=DataFraFirst_i$UnCen)
  my_fit_i  = survfit(formula = my_surv_i ~ 1, data=my_surv_i)
  my_fit_summ_i = summary(my_fit_i)
  obs_time = my_fit_summ_i$time  # t_i
  n_risk   = my_fit_summ_i$n.risk # Y_i
  n_event = my_fit_summ_i$n.event # d_i
  KM_surv = my_fit_summ_i$surv   #\hat{S}(t_i)
  
  incr = n_event / n_risk  # d_i / y_i
  NA_cumhzd = NULL  # initialize
  for (j in 1:length(obs_time)) NA_cumhzd[j] = sum(incr[1:j])
  NA_surv = exp(-NA_cumhzd)
  
  incr_GW = n_event / n_risk / (n_risk - n_event)
  var_GW = NULL
  for (i in 1:length(obs_time)) var_GW[i] = sum(incr_GW[1:i])
  incr_NA = n_event / n_risk^2
  var_NA = NULL
  for (i in 1:length(obs_time)) var_NA[i] = sum(incr_NA[1:i])
  
  gamma = qnorm(0.975)
  std_NA = sqrt(var_NA)
  up_NA = exp(- NA_cumhzd + gamma*std_NA)
  lo_NA = exp(- NA_cumhzd - gamma*std_NA)
  
  stderr_GW = KM_surv * sqrt(var_GW)
  up_GW = KM_surv + gamma*stderr_GW
  lo_GW = KM_surv - gamma*stderr_GW
  
  res = data.frame(Subject = sprintf("Subject%02d", indx),
                    obsTimes = obs_time,
                    Nrisk = n_risk,
                    Nevent = n_event,
                    KMsv = KM_surv,
                    NAsv = NA_surv,
                    upNA = up_NA,
                    loNA = lo_NA,
                    upGW = up_GW,
                    loGW = lo_GW)
  
  return(res)
}

SurvData = NULL

indx = 0
for (i in unique(DataFraFirst$SUBJECTINDEX))
{
  indx = indx + 1
  SurvData = rbind(SurvData, survPlotMaker(i))
}

ggplot(data=SurvData, aes(x=obsTimes)) + 
  geom_step(aes(y=KMsv), linetype=1,color='red',alpha=0.5) + 
  geom_ribbon(aes(ymin=loGW, ymax=upGW), alpha=0.2, fill='red') +
  geom_step(aes(y=NAsv), linetype=1,color='blue',alpha=0.5) +
  geom_ribbon(aes(ymin=loNA, ymax=upNA), alpha=0.2, fill='blue') +
  facet_wrap(.~Subject) + 
  ylab('Prob?') + xlab('time') +  
  theme_light()



#++++++++++++++++++++++++
# Hypothesis test by participants : Different from the reference == Else

DataFraFirst_i
DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i)
my_surv_i = Surv(time=DataFraFirst_i$Duration,
                 event=DataFraFirst_i$UnCen)

testSurv_i = survdiff(my_surv_i ~ DataFraFirst_i$Region, rho=0) ;
testSurv_i$chisq

1 - pchisq(testSurv_i$chisq, length(testSurv_i$n) - 1)


indx = 0
p.s = NULL
chisq.s = NULL

for (i in unique(DataFraFirst$SUBJECTINDEX))
{
  indx = indx + 1
  DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i)
  my_surv_i = Surv(time=DataFraFirst_i$Duration,
                   event=DataFraFirst_i$UnCen)
  
  testSurv_i = survdiff(my_surv_i ~ DataFraFirst_i$Region, rho=0) ;
  p.s = c(p.s, 1 - pchisq(testSurv_i$chisq, length(testSurv_i$n) - 1))
  chisq.s = c(chisq.s, testSurv_i$chisq)
}

## different scale needed...
test = data.frame(Subject = 1:29,
                  p.values = p.s,
                  Wald.chisq = chisq.s)

ggplot(test, aes(x = Subject)) +
  geom_point(aes(y = Wald.chisq, colour = "chi-squared")) +
  geom_point(aes(y = p.values, colour = "p-value")) +
  scale_y_continuous(sec.axis = sec_axis(~.*.00001, name = "p-value")) +
  scale_colour_manual(values = c("blue", "red")) +
  labs(y = "chi-squared",
       x = "Subject",
       colour = "Parameter") +
  theme(legend.position = c(0.8, 0.9)) + 
  theme_light()



#++++++++++++++++++++++++
# Hazard plot by participants

DataFraFirst_i
KF_i = muhaz(times=DataFraFirst_i$Duration, 
             delta=DataFraFirst_i$UnCen, kern="epanechnikov", bw.grid=100)
plot(KF_i)

lines(KF_i$est.grid, KF_i$haz.est)
kernel_fit_bmt1 <- with(bmt1, muhaz(times=t2, delta=d3, kern="epanechnikov", bw.grid=100))
plot( with(bmt1, muhaz(times=t2, delta=d3, kern="epanechnikov", bw.grid=100)) , xlim=c(-10,1000), ylim=c(-0.001, 0.005), main='hazard')

hzd_est_bmt1 <- kernel_fit_bmt1$haz.est
cumulative_hzd_est_bmt1 <- cumsum(hzd_est_bmt1)*11.67

plot( kernel_fit_bmt1$est.grid, hzd_est_bmt1 , xlim=c(-10,1000), ylim=c(-0.001, 0.005), main='hazard estimates via kernel', type='l')

plot( with(bmt1, muhaz(times=t2, delta=d3, kern="epanechnikov", bw.grid=100)) , xlim=c(-10,650), ylim=c(-0.001, 0.005), main='hazard')
lines( with(bmt2, muhaz(times=t2, delta=d3, kern="epanechnikov", bw.grid=100)), col="blue" )
lines( with(bmt3, muhaz(times=t2, delta=d3, kern="epanechnikov", bw.grid=100)), col="red" )

DataFraFirst$Region %>% unique

DataFraFirst_i_Nose = DataFraFirst_i %>% filter(Region == "Nose")
DataFraFirst_i_EyeL = DataFraFirst_i %>% filter(Region == "EyeL")
DataFraFirst_i_EyeR = DataFraFirst_i %>% filter(Region == "EyeR")



hazardPlotMaker = function(i)
{
  DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i)
  haz_i = with(DataFraFirst_i,
               muhaz(time=Duration, delta=UnCen, kern="epanechnikov", bw.grid=100))

  res = data.frame(Subject = sprintf("Subject%02d", indx),
                   times = haz_i$est.grid,
                   hazrd = haz_i$haz.est)
  return(res)
}


hazrdData = NULL

indx = 0
for (i in unique(DataFraFirst$SUBJECTINDEX))
{
  indx = indx + 1
  hazrdData = rbind(hazrdData, hazardPlotMaker(i))
}

ggplot(data=hazrdData, aes(x=times)) + 
  geom_line(aes(y=hazrd), linetype=1,color='black',alpha=0.5) + 
  facet_wrap(.~Subject) + 
  ylab('hazard') + xlab('time') +  
  theme_light()



# +++++++ Different Hazard +++++++++ 
hazardPlotMaker = function(i)
{
  DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i)
  i_Else_haz = with(DataFraFirst_i %>% filter(Region == "Else"),
                    muhaz(time=Duration, delta=UnCen, kern="epanechnikov", bw.grid=100))
  i_Nose_haz = with(DataFraFirst_i %>% filter(Region == "Nose"),
                    muhaz(time=Duration, delta=UnCen, kern="epanechnikov", bw.grid=100))
  i_EyeL_haz = with(DataFraFirst_i %>% filter(Region == "EyeL"),
                    muhaz(time=Duration, delta=UnCen, kern="epanechnikov", bw.grid=100))
  i_EyeR_haz = with(DataFraFirst_i %>% filter(Region == "EyeR"),
                    muhaz(time=Duration, delta=UnCen, kern="epanechnikov", bw.grid=100))
  
  res = data.frame(Subject = sprintf("Subject%02d", indx),
                   times = obs_time,
                   Nrisk = n_risk,
                   Nevent = n_event,
                   KMsv = KM_surv,
                   NAsv = NA_surv,
                   upNA = up_NA,
                   loNA = lo_NA,
                   upGW = up_GW,
                   loGW = lo_GW)
  
  return(res)
}





#++++++++++++++++++++++++++++++++
# Participant-based : Is log-logistic? Is Weibull? : Hypothesis-driven !!



# 
data_frame(obs_time)

##
aggregate()



##
plot(c(0, max(obs_time)+10), c(0,1), type="n", xlab="Time",
     ylab="Survival probability",
     main="Comparing two estimators for survival function") ;
points(obs_time, KM_surv, type="s", col="red", lty=1) ; 
points(obs_time, NA_surv, type="s", col="blue", lty=1) ; 
legend("bottomright", col=c("red", "blue"), lty=c(1, 1), 
       c("K-M estimator", "exp(-{N-A esetimator})")) ;





# Comparison Among Event 
i = 6
DataFraFirst_i = DataFraFirst[DataFraFirst$SUBJECTINDEX == i, ]


#

## 

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

AnaDataFra = AnaDataFra %>% mutate(Duration = end - start)
AnaDataFra = data.frame(AnaDataFra, count=ave(rep(1,length(AnaDataFra$trial)),
                                              AnaDataFra$SUBJECTINDEX,
                                              AnaDataFra$trial,
                                              FUN = cumsum))

AnaDataFraFirst = AnaDataFra[AnaDataFra$count == 1,]
AnaDataFraFirst = AnaDataFraFirst %>% mutate(UnCen = (end < 1450))






# Concerns about censoring

## Positive correlation between # of censoring & mean Duration
DataFraFirst %>% group_by(SUBJECTINDEX) %>% 
  summarise(NumCensored = sum(UnCen==0),
            meanDur = mean(Duration)) %>% ggplot(aes(x=NumCensored, y=meanDur, 
                                                     col=as.factor(SUBJECTINDEX))) +
  geom_point()




## Whole-Data Analysis
my_surv = Surv(time=DataFraFirst$Duration,
               event=DataFraFirst$UnCen)
my_fit = survfit(formula = my_surv ~ 1, data=my_surv) ;

my_surv = Surv(time=data$t2, event=data$d3) ;
my_fit = survfit(formula = my_surv ~ 1, data=my_surv) ;

my_fit_summ = summary(my_fit) ;
my_fit_summ
my_fit_summ$time     # t_i's : the distinct uncensored timepoints
my_fit_summ$surv     # the Kaplan-Meier estimate at each t_i's
my_fit_summ$n.risk   # Y_i's
my_fit_summ$n.event  # d_i's
my_fit_summ$std.err  # standard error of the K-M estimate at t_i
my_fit_summ$lower    # lower bound of pointwise CI bound


obs_time = my_fit_summ$time ; # t_i
n_risk = my_fit_summ$n.risk ; # Y_i
n_event = my_fit_summ$n.event ; # d_i
KM_surv = my_fit_summ$surv ;    #\hat{S}(t_i)

incr = n_event / n_risk ; # d_i / y_i
NA_cumhzd = NULL ; # initialize
for (i in 1:length(obs_time)) NA_cumhzd[i] = sum(incr[1:i]) ;
NA_surv = exp(-NA_cumhzd) ;

windows(width=6, height=6) ;
plot(c(0, max(obs_time)+10), c(0,1), type="n", xlab="Time",
     ylab="Survival probability",
     main="Comparing two estimators for survival function") ;
points(obs_time, KM_surv, type="s", col="red", lty=1) ; 
points(obs_time, NA_surv, type="s", col="blue", lty=1) ; 
legend("topright", col=c("red", "blue"), lty=c(1, 1), 
       c("K-M estimator", "exp(-{N-A esetimator})")) ;


incr_GW = n_event / n_risk / (n_risk - n_event)
var_GW = NULL
for (i in 1:length(obs_time)) var_GW[i] = sum(incr_GW[1:i]) ;
incr_NA = n_event / n_risk^2 ;
var_NA = NULL ;
for (i in 1:length(obs_time)) var_NA[i] = sum(incr_NA[1:i]) ;

sqrt(var_GW * KM_surv^2)
my_fit_summ$std.err

gamma = qnorm(0.975) ;
std_NA = sqrt(var_NA) ;
up_NA = exp(- NA_cumhzd + gamma*std_NA) ;
lo_NA = exp(- NA_cumhzd - gamma*std_NA) ;

stderr_GW = KM_surv * sqrt(var_GW) ;
up_GW = KM_surv + gamma*stderr_GW ;
lo_GW = KM_surv - gamma*stderr_GW ;

windows(width=6, height=6) ;
plot(c(0, max(obs_time)+10), c(0,1), type="n", xlab="Time",
     ylab="Survival probability",
     main="Comparing two estimators for survival function") ;
points(obs_time, KM_surv, type="s", col="red", lty=1) ; 
points(obs_time, up_GW, type="s", col="red", lty=2) ; 
points(obs_time, lo_GW, type="s", col="red", lty=2) ; 
points(obs_time, NA_surv, type="s", col="blue", lty=1) ; 
points(obs_time, up_NA, type="s", col="blue", lty=2) ; 
points(obs_time, lo_NA, type="s", col="blue", lty=2) ; 
legend("topright", col=c("red", "blue"), lty=c(1, 1), 
       c("K-M estimator", "exp(-{N-A esetimator})")) ; 



##
par(mfrow=c(4,2))

for (i in (DataFraFirst$SUBJECTINDEX %>% unique))
{
  DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i)
  
  my_surv_i = Surv(time=DataFraFirst_i$Duration,
                   event=DataFraFirst_i$UnCen)
  my_fit_i = survfit(formula = my_surv_i ~ 1, data=my_surv_i) ;
  
  my_fit_summ_i = summary(my_fit_i) ;
  obs_time = my_fit_summ_i$time ; # t_i
  n_risk = my_fit_summ_i$n.risk ; # Y_i
  n_event = my_fit_summ_i$n.event ; # d_i
  KM_surv = my_fit_summ_i$surv ;    #\hat{S}(t_i)
  
  incr = n_event / n_risk ; # d_i / y_i
  NA_cumhzd = NULL ; # initialize
  for (j in 1:length(obs_time)) NA_cumhzd[j] = sum(incr[1:j]) ;
  NA_surv = exp(-NA_cumhzd) ;
  
  plot(obs_time, KM_surv, type="s",col=i, lty=1) ;
}

table(DataFraFirst$UnCen)

## Participant-wise Analysis (More efficient way...)
plot(c(0, max(obs_time)+10), c(0,1), type="n", xlab="Time",
     ylab="Survival probability",
     main="Comparing two estimators for survival function") ;



for (i in (DataFraFirst$SUBJECTINDEX %>% unique))
{
  DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i)
  
  my_surv_i = Surv(time=DataFraFirst_i$Duration,
                   event=DataFraFirst_i$UnCen)
  my_fit_i = survfit(formula = my_surv_i ~ 1, data=my_surv_i) ;
  
  my_fit_summ_i = summary(my_fit_i) ;
  obs_time = my_fit_summ_i$time ; # t_i
  n_risk = my_fit_summ_i$n.risk ; # Y_i
  n_event = my_fit_summ_i$n.event ; # d_i
  KM_surv = my_fit_summ_i$surv ;    #\hat{S}(t_i)
  
  incr = n_event / n_risk ; # d_i / y_i
  NA_cumhzd = NULL ; # initialize
  for (j in 1:length(obs_time)) NA_cumhzd[j] = sum(incr[1:j]) ;
  NA_surv = exp(-NA_cumhzd) ;
  
  points(obs_time, KM_surv, type="s", 
         col=i,
         #col=(CenAtLeastOnce %>% filter(SUBJECTINDEX==i) )$CenAtLeastOnce + 1,
         lty=1) ; 
}


### Participant Censored at least Once
CenAtLeastOnce = DataFraFirst %>%
  group_by(SUBJECTINDEX) %>%
  summarise(CenAtLeastOnce = (sum(UnCen)<400))

print(CenAtLeastOnce, n=29)


###





incr_GW = n_event / n_risk / (n_risk - n_event)
var_GW = NULL
for (i in 1:length(obs_time)) var_GW[i] = sum(incr_GW[1:i]) ;
incr_NA = n_event / n_risk^2 ;
var_NA = NULL ;
for (i in 1:length(obs_time)) var_NA[i] = sum(incr_NA[1:i]) ;

sqrt(var_GW * KM_surv^2)
my_fit_summ$std.err

gamma = qnorm(0.975) ;
std_NA = sqrt(var_NA) ;
up_NA = exp(- NA_cumhzd + gamma*std_NA) ;
lo_NA = exp(- NA_cumhzd - gamma*std_NA) ;

stderr_GW = KM_surv * sqrt(var_GW) ;
up_GW = KM_surv + gamma*stderr_GW ;
lo_GW = KM_surv - gamma*stderr_GW ;

windows(width=6, height=6) ;
plot(c(0, max(obs_time)+10), c(0,1), type="n", xlab="Time",
     ylab="Survival probability",
     main="Comparing two estimators for survival function") ;
points(obs_time, KM_surv, type="s", col="red", lty=1) ; 
points(obs_time, up_GW, type="s", col="red", lty=2) ; 
points(obs_time, lo_GW, type="s", col="red", lty=2) ; 
points(obs_time, NA_surv, type="s", col="blue", lty=1) ; 
points(obs_time, up_NA, type="s", col="blue", lty=2) ; 
points(obs_time, lo_NA, type="s", col="blue", lty=2) ; 
legend("topright", col=c("red", "blue"), lty=c(1, 1), 
       c("K-M estimator", "exp(-{N-A esetimator})")) ; 


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Facet ggsurvplot() output by
# a combination of factors
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Fit (complexe) survival curves
#++++++++++++++++++++++++++++++++++++
require("survival")
fit3 <- survfit( Surv(time, status) ~ sex + rx + adhere,
                 data = colon )

# Visualize: plot survival curves by sex and facet by rx and adhere
#++++++++++++++++++++++++++++++++++++
ggsurv <- ggsurvplot(fit3, fun = "cumhaz", conf.int = TRUE)
ggsurv$plot +theme_bw() + facet_grid(rx ~ adhere)

fit
ggsurvplot(my_fit, my_surv, facet.by = SUBJECTINDEX, 
           palette = "jco", pval = TRUE)


require("survival")
library("survminer")
fit2 <- survfit( my_surv ~ Region + SUBJECTINDEX, data = DataFraFirst )
ggsurv <- ggsurvplot(fit2, conf.int = TRUE)

ggsurv$plot + theme_bw() + facet_wrap(~ SUBJECTINDEX)

surv_summary(fit2)

ggsurvplot()

# Comparing two estimates 

# Fit (complexe) survival curves
#++++++++++++++++++++++++++++++++++++

require("survival")
fit3 <- survfit( Surv(time, status) ~ sex + rx + adhere,
                 data = colon )

# Visualize: plot survival curves by sex and facet by rx and adhere
#++++++++++++++++++++++++++++++++++++
ggsurv <- ggsurvplot(fit3, fun = "cumhaz", conf.int = TRUE)
ggsurv$plot +theme_bw() + facet_grid(rx ~ adhere)

require("survival")
fit <- survfit( Surv(time, status) ~ sex + rx + adhere,
                data = colon )
devtools::install_github("kassambara/survminer")

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/survminer", build_vignettes = FALSE)


# Visualize
#++++++++++++++++++++++++++++++++++++
require("survminer")

ggsurv <- ggsurvplot(fit3, fun = "cumhaz", conf.int = TRUE,
                     risk.table = TRUE, risk.table.col="strata",
                     ggtheme = theme_bw())
# Faceting survival curves
curv_facet <- ggsurv$plot + facet_grid(rx ~ adhere)
curv_facet


# Log-Rank test 

## [1] Two-sample testing

DataFraFirst = DataFraFirst %>% mutate(Left = DataFraFirst$x < 800,
                                       Top  = DataFraFirst$y < 600)



LRsurvObj = Surv(time = DataFraFirst$Duration,
                 event = DataFraFirst$UnCen)

A = survdiff(LRsurvObj ~ as.factor(DataFraFirst$Left), rho=0)
survdiff(LRsurvObj ~ as.factor(DataFraFirst$Top), rho=0)

p.val = 1 - pchisq(sdf$chisq, length(sdf$n) - 1) 







for (i in (DataFraFirst$SUBJECTINDEX %>% unique))
{
  DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i)
  
  LRsurvObj = Surv(time = DataFraFirst_i$Duration,
                   event = DataFraFirst_i$UnCen)
  
  a = survdiff(LRsurvObj ~ as.factor(DataFraFirst_i$Left), rho=0)
  b = survdiff(LRsurvObj ~ as.factor(DataFraFirst_i$Top), rho=0)
}






# Parametric Approach



# Two-sample testing



test_kidney = survdiff(survobj_kidney ~ as.factor(kidney$type), rho=0)
test_kidney = survdiff(survobj_kidney ~ kidney$type, rho=0) ;
test_kidney = survdiff(survobj_kidney ~ type, rho=0, data=kidney) ;

# 


## 




survobj.aft = Surv(time=larynx$time, event=larynx$delta, type="right") ; head(survobj.aft)
regobj.aft = survreg(survobj.aft ~ 1 + factor(larynx$stage) + larynx$age, dist="weibull") ;
summary(regobj.aft)


fit.phfix = coxph(survobj.aft ~  factor(larynx$stage) + larynx$age); #기본값으로 실행
summary(fit.phfix)

fit.phfix$coefficients
fit.phfix$var        # I^(-1), the inverse of information matrix
fit.phfix$loglik        

install.packages("parfm")
library(parfm)

par.fit=parfm(Surv(DataFraFirst$Duration, 1*DataFraFirst$UnCen)
              ~ 1 + factor(DataFraFirst$Region), 
              data=DataFraFirst, dist="weibull")
par.fit

#baseline hazard
time=DataFraFirst$Duration
base.hazard = par.fit[1]*par.fit[2]*time^(par.fit[1]*-1)
plot(time, base.hazard, type="p")



data("kidney")
head(kidney)


SurvData = with(DataFraFirst,
                Surv(time = DataFraFirst$Duration,
                     event= DataFraFirst$UnCen))

fit.bre = coxph(SurvData ~  Region, 
                data=DataFraFirst, method="breslow")

(fit.bre.summ = summary(fit.bre))

DataFraFirst$Region %>% table

fit.exact = coxph(SurvData ~  Region, 
                  data=DataFraFirst, method="exact")

fit.efron = coxph(SurvData ~  Region, 
                  data=DataFraFirst, method="efron")

res.cox <- coxph(Surv(time, status) ~ age + sex + wt.loss, data =  lung)
res.cox
test.ph <- cox.zph(fit.efron)
test.ph

survobj.kid = with( kidney, Surv(time=time, event=delta, type="right")); head(survobj.kid)

fit.bre = coxph(survobj.kid ~  type, init=0.5, data=kidney , method="breslow");
fit.bre.summ = summary(fit.bre)
fit.exact = coxph(survobj.kid ~  type, init=0.5, data=kidney , method="exact");
fit.exact.summ = summary(fit.exact)
fit.efron = coxph(survobj.kid ~  factor(type), init=0.5, data=kidney , method="efron"); #기본값으로 실행


fit.efron.summ = summary(fit.efron)

fit.bre = coxph(survobj.kid ~  type, init=0, data=kidney , method="breslow");
fit.bre.summ = summary(fit.bre)
names(fit.bre.summ)
fit.bre.summ$loglik
fit.bre.summ$coefficients
fit.bre.summ$sctest
fit.bre.summ$waldtest
fit.bre.summ$logtest



DataFraFirst_i

DataFraFirst_i$

surv_fit = survfit(my_surv_i ~ Region, data=DataFraFirst_i) 

summary(surv_fit)
surv_fit$strata

names(fit_kidney)
fit_kidney$surv
summ_kidney = summary(fit_kidney) ;
names(summ_kidney)
summ_kidney$table


test = summary(surv_fit)



# Cox PH model
strataPlotMaker = function(i)
{
  DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i)
  my_surv_i = Surv(time=DataFraFirst_i$Duration,
                   event=DataFraFirst_i$UnCen)
  my_fit_i  = survfit(formula = my_surv_i ~ DataFraFirst_i$Region, data=my_surv_i)
  my_fit_summ_i = summary(my_fit_i)
  
  obs_time = my_fit_summ_i$time  # t_i
  n_risk   = my_fit_summ_i$n.risk # Y_i
  n_event = my_fit_summ_i$n.event # d_i
  KM_surv = my_fit_summ_i$surv   #\hat{S}(t_i)
  
  # incr = n_event / n_risk  # d_i / y_i
  # NA_cumhzd = NULL  # initialize
  # for (j in 1:length(obs_time)) NA_cumhzd[j] = sum(incr[1:j])
  # NA_surv = exp(-NA_cumhzd)
  
  # incr_GW = n_event / n_risk / (n_risk - n_event)
  # var_GW = NULL
  # for (i in 1:length(obs_time)) var_GW[i] = sum(incr_GW[1:i])
  # incr_NA = n_event / n_risk^2
  # var_NA = NULL
  # for (i in 1:length(obs_time)) var_NA[i] = sum(incr_NA[1:i])
  
  # gamma = qnorm(0.975)

  # stderr_GW = KM_surv * sqrt(var_GW)
  # up_GW = KM_surv + gamma*stderr_GW
  # lo_GW = KM_surv - gamma*stderr_GW
  
  res = data.frame(Subject = sprintf("Subject%02d", indx),
                   obsTimes = obs_time,
                   Nrisk = n_risk,
                   Nevent = n_event,
                   KMsv = KM_surv,
                   Group = rep(c("Else", "EyeL", "EyeR", "Nose"), 
                               my_fit_summ_i$strata)) #,
                   #upGW = up_GW,
                   # loGW = lo_GW)
  
  return(res)
}

strataData = NULL

indx = 0
for (i in unique(DataFraFirst$SUBJECTINDEX))
{
  indx = indx + 1
  SurvData = rbind(SurvData, survPlotMaker(i))
}

ggplot(data=SurvData, aes(x=obsTimes)) + 
  geom_step(aes(y=KMsv), linetype=1,color='red',alpha=0.5) + 
  geom_ribbon(aes(ymin=loGW, ymax=upGW), alpha=0.2, fill='red') +
  geom_step(aes(y=NAsv), linetype=1,color='blue',alpha=0.5) +
  geom_ribbon(aes(ymin=loNA, ymax=upNA), alpha=0.2, fill='blue') +
  facet_wrap(.~Subject) + 
  ylab('Prob?') + xlab('time') +  
  theme_light()
