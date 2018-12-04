library(tidyverse)
library(rhdf5)
library(MASS)
library(survival)
library(survminer)
library(coxme)
library(KMsurv)
library(muhaz)
library(plyr)
library(parfm)

#+++++++++++++++++++++++
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#+++++++++++++++++++++++
DataFraFirst_explore = DataFraFirst
DataFraFirst_explore$SUBJECTINDEX = sprintf("Subject%02d", DataFraFirst_explore$SUBJECTINDEX)
DataFraFirst_explore$SUBJECTINDEX[DataFraFirst$UnCen == 0] = "Censored"
  

ggplot(DataFraFirst_explore, aes(x=Region, y=Duration, color=SUBJECTINDEX)) +
  scale_color_manual(values=c("gray", gg_color_hue(29))) +
  geom_boxplot(color="black")+
  geom_jitter(position=position_jitter(0.2)) +
  theme_light()

ggplot(DataFraFirst_explore, aes(x=start, y=Duration, color=SUBJECTINDEX)) +
  scale_color_manual(values=c("gray", gg_color_hue(29))) +
  geom_point() +
  theme_light()



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
  geom_ribbon(aes(ymin=loGW, ymax=upGW), alpha=0.3, fill='red') +
  geom_step(aes(y=NAsv), linetype=1,color='blue',alpha=0.5) +
  geom_ribbon(aes(ymin=loNA, ymax=upNA), alpha=0.3, fill='blue') +
  facet_wrap(.~Subject) + 
  ylab('Probability') + xlab('time') +  
  theme_light()

ggplot(data=SurvData, aes(x=obsTimes, color=Subject)) +
  geom_step(aes(y=KMsv), linetype=1, alpha=0.5) + 
  geom_ribbon(aes(ymin=loGW, ymax=upGW), alpha=0.3) +
  geom_step(aes(y=NAsv), linetype=1, alpha=0.5) +
  geom_ribbon(aes(ymin=loNA, ymax=upNA), alpha=0.3) +
  ylab('Probability') + xlab('time') +  
  theme_light()

ggplot(data=SurvData, aes(x=obsTimes, color=Subject)) +
  geom_step(aes(y=KMsv), linetype=1, alpha=0.5) + 
  # geom_ribbon(aes(ymin=loGW, ymax=upGW), alpha=0.3) +
  geom_step(aes(y=NAsv), linetype=1, alpha=0.5) +
  # geom_ribbon(aes(ymin=loNA, ymax=upNA), alpha=0.3) +
  ylab('Probability') + xlab('time') +
  ggtitle("Product-Limit Estimates of Survival Function by Participants") +
  theme_light() +
  theme(plot.title=element_text(hjust=.5, size=15))



##






## The Lowest, The Highest


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Censoring's effect 

DataFraFirst %>% group_by(SUBJECTINDEX) %>% summarise(numCen = sum(UnCen==0)) %>%
  ggplot(aes(x=SUBJECTINDEX, y=numCen)) + geom_bar(stat="identity")

i = 29
i = 24

DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i)
my_surv_i = Surv(time=DataFraFirst_i$Duration,
                 event=DataFraFirst_i$UnCen)
my_fit_i  = survfit(formula = my_surv_i ~ 1, data=my_surv_i)
my_fit_summ_i = summary(my_fit_i)
obs_time = my_fit_summ_i$time  # t_i
n_risk   = my_fit_summ_i$n.risk # Y_i
n_event = my_fit_summ_i$n.event # d_i
KM_surv = my_fit_summ_i$surv

incr = n_event / n_risk  # d_i / y_i
NA_cumhzd = NULL  # initialize
for (j in 1:length(obs_time)) NA_cumhzd[j] = sum(incr[1:j])
NA_surv = exp(-NA_cumhzd)


DurECDF = ecdf(DataFraFirst_i$Duration)
ecdfRes = 1 - DurECDF(obs_time)

obs_time[180:length(obs_time)]

ggplot(NULL, aes(x=obs_time)) + 
  geom_step(aes(y=NA_surv), linetype=1, color='red',alpha=0.5) + 
  geom_step(aes(y=ecdfRes), linetype=1, color='blue',alpha=0.5) +
  ylab('Probability') + xlab('time') +  
  theme_light()


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# No dependency over the pictures
i = 3
DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i)
CountPic = data.frame(DataFraFirst_i, 
                      faceCNT=ave(rep(1,length(DataFraFirst_i$SUBJECTINDEX)),
                                  DataFraFirst_i$SUBJECTINDEX,
                                  DataFraFirst_i$filenumber,
                                  FUN = cumsum))

ggplot(CountPic, aes(x=faceCNT, y=Duration, fill=as.factor(UnCen))) +
  scale_fill_manual(values=cols, aesthetics=c("black", "red")) +
  geom_point() + theme_light()

lm(Duration ~ faceCNT, CountPic ) %>% summary



## Positive correlation between # of censoring & mean Duration
DataFraFirst %>% group_by(SUBJECTINDEX) %>% 
  summarise(NumCensored = sum(UnCen==0),
            meanDur = mean(Duration)) %>% ggplot(aes(x=NumCensored, y=meanDur, 
                                                     col=as.factor(SUBJECTINDEX))) +
  geom_point()



# +++++++ 
# Hazard (NA) plot by participants



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# is_dist plot by participants
is_distiPlotMaker = function(i)
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
                   NAsv_cumh = NA_cumhzd,
                   Interccc=coef(lm(log(NA_cumhzd)~log(obs_time)))[1],
                   Slopeee=coef(lm(log(NA_cumhzd)~log(obs_time)))[2],
                   Linter=coef(lm(log(exp(NA_cumhzd)-1)~log(obs_time)))[1],
                   Lsploe=coef(lm(log(exp(NA_cumhzd)-1)~log(obs_time)))[2])
  
  return(res)
}


is_distData = NULL

indx = 0
for (i in unique(DataFraFirst$SUBJECTINDEX))
{
  indx = indx + 1
  is_distData = rbind(is_distData, is_distiPlotMaker(i))
}


## is Weibull?
ggplot(data=is_distData, aes(x=log(obsTimes))) + 
#  geom_line(aes(y=log(NAsv_cumh)), linetype=1,color='blue',alpha=0.5) + 
  geom_point(aes(y=log(NAsv_cumh)), color='blue',alpha=0.5) + 
  geom_abline(aes(intercept=Interccc, slope=Slopeee), color='red') +
  facet_wrap(.~Subject) + 
  ylab('Log(H)') + xlab('Log(time)') +  
  theme_light()




## is Log-Logistic?
ggplot(data=is_distData, aes(x=log(obsTimes))) + 
  geom_line(aes(y=log(exp(NAsv_cumh)-1)), linetype=1,color='blue',alpha=0.5) + 
  geom_abline(aes(intercept=Linter, slope=Lsploe), color='red') +
  facet_wrap(.~Subject) + 
  ylab('log(exp(H)-1)') + xlab('Log(time)') +  
  theme_light()


geom_abline(mapping = NULL, data = NULL, ..., slope, intercept,
            na.rm = FALSE, show.legend = NA)


#++++++++++++++++++++++++
# Hazard (Kernel) plot by participants

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



# +++++++ Different Hazard +++++++++ for a specific Participant

i = 3 ##
i = 4
i = 8 
i = 11 ##
i = 12
i = 13 ###

i = 1

i = i + 1
DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i)

i_Else_haz = with(DataFraFirst_i %>% filter(Region == "Else"),
                  muhaz(time=Duration, delta=UnCen, kern="epanechnikov", bw.grid=100))
i_Nose_haz = with(DataFraFirst_i %>% filter(Region == "Nose"),
                  muhaz(time=Duration, delta=UnCen, kern="epanechnikov", bw.grid=100))
i_EyeL_haz = with(DataFraFirst_i %>% filter(Region == "EyeL"),
                  muhaz(time=Duration, delta=UnCen, kern="epanechnikov", bw.grid=100))
i_EyeR_haz = with(DataFraFirst_i %>% filter(Region == "EyeR"),
                  muhaz(time=Duration, delta=UnCen, kern="epanechnikov", bw.grid=100))


res = data.frame(Subject = sprintf("Subject%02d", i),
                 Else_times = i_Else_haz$est.grid,
                 Else_hazrd = i_Else_haz$haz.est,
                 Nose_times = i_Nose_haz$est.grid,
                 Nose_hazrd = i_Nose_haz$haz.est,
                 EyeL_times = i_EyeL_haz$est.grid,
                 EyeL_hazrd = i_EyeL_haz$haz.est,
                 EyeR_times = i_EyeR_haz$est.grid,
                 EyeR_hazrd = i_EyeR_haz$haz.est)


ggplot(data=res) + 
  geom_line(aes(x=Else_times, y=Else_hazrd), linetype=1,color='red',alpha=0.5) + 
  geom_line(aes(x=Nose_times, y=Nose_hazrd), linetype=1,color='purple',alpha=0.5) + 
  geom_line(aes(x=EyeL_times, y=EyeL_hazrd), linetype=1,color='green',alpha=0.5) + 
  geom_line(aes(x=EyeR_times, y=EyeR_hazrd), linetype=1,color='blue',alpha=0.5) + 
  facet_wrap(.~Subject) + 
  ylab('hazard') + xlab('time') +  
  theme_light()



#++++++++++++++++++++++++++++++++
# Participant-based : Is log-logistic? Is Weibull? : Hypothesis-driven !!

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



# Fit (complex) survival curves
#++++++++++++++++++++++++++++++++++++

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
## strata Plots
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
  
  res = data.frame(Subject = rep(sprintf("Subject%02d", indx), length(obs_time)),
                   obsTimes = obs_time,
                   Nrisk = n_risk,
                   Nevent = n_event,
                   KMsv = KM_surv,
                   Group = rep(c("Else", "EyeL", "EyeR", "Nose"), 
                               diff(c(0, which(diff(obs_time) < 0), length(obs_time))))) #,
                   #upGW = up_GW,
                   # loGW = lo_GW)
  
  return(res)
}



strataData = NULL

indx = 0
for (i in unique(DataFraFirst$SUBJECTINDEX))
{
  indx = indx + 1
  strataData = rbind(strataData, strataPlotMaker(i))
}

ggplot(data=strataData, aes(x=obsTimes)) + 
  geom_step(aes(y=KMsv, col=Group), linetype=1,alpha=0.9) + 
  facet_wrap(.~Subject) + 
  ylab('Prob?') + xlab('time') +  
  theme_light()




## isWeibull

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
  
  res = data.frame(Subject = rep(sprintf("Subject%02d", indx), length(obs_time)),
                   obsTimes = obs_time,
                   Nrisk = n_risk,
                   Nevent = n_event,
                   KMsv = KM_surv,
                   Group = rep(c("Else", "EyeL", "EyeR", "Nose"), 
                               diff(c(0, which(diff(obs_time) < 0), length(obs_time))))) #,
  #upGW = up_GW,
  # loGW = lo_GW)
  
  return(res)
}



##
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
  
  res = data.frame(Subject = rep(sprintf("Subject%02d", indx), length(obs_time)),
                   obsTimes = obs_time,
                   Nrisk = n_risk,
                   Nevent = n_event,
                   KMsv = KM_surv,
                   Group = rep(c("Else", "EyeL", "EyeR", "Nose"), 
                               diff(c(0, which(diff(obs_time) < 0), length(obs_time))))) #,
  #upGW = up_GW,
  # loGW = lo_GW)
  
  return(res)
}






plot(log.time,log(H),type="l",lty=1,main="Is Weibull?", col ="Blue" ) ;
abline(coef(lm(log(H)~log.time)), col="red")