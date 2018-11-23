library(tidyverse)
library(rhdf5)
library(MASS)
library(survival)
library(survminer)
library(coxme)
library(KMsurv)
library(muhaz)

# Comparison Among Event 
i = 6
DataFraFirst_i = DataFraFirst[DataFraFirst$SUBJECTINDEX == i, ]


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



# Comparing two estimates 


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




# Cox PH model
