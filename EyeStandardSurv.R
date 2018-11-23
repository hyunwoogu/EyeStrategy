library(tidyverse)
library(rhdf5)
library(MASS)
library(survival)
library(survminer)
library(coxme)
library(KMsurv)
library(muhaz)


# Concerns about censoring

## [1] Positive correlation between # of censoring & mean Duration
DataFraFirst %>% group_by(SUBJECTINDEX) %>% 
  summarise(NumCensored = sum(UnCen==0),
            meanDur = mean(Duration)) %>% ggplot(aes(x=NumCensored, y=meanDur, 
                                                     col=as.factor(SUBJECTINDEX))) +
  geom_point()




# Comparing two estimates 



# Log-Rank test 

## [1] Two-sample testing

DataFraFirst = DataFraFirst %>% mutate(Left = DataFraFirst$x < 800,
                                       Top  = DataFraFirst$x < 600)

LRsurvObj = Surv(time = DataFraFirst$Duration,
                 event = DataFraFirst$UnCen)

survdiff(LRsurvObj ~ as.factor(DataFraFirst$Left), rho=0)
survdiff(LRsurvObj ~ as.factor(DataFraFirst$Top), rho=0)

survobj_kidney = Surv(time = kidney$time, event = kidney$delta) ;
survobj_kidney;

test_kidney = survdiff(survobj_kidney ~ as.factor(kidney$type), rho=0)
test_kidney = survdiff(survobj_kidney ~ kidney$type, rho=0) ;
test_kidney = survdiff(survobj_kidney ~ type, rho=0, data=kidney) ;




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

  fit.bre = coxph(SurvData ~  as.factor(Reg), init=0.5, data=kidney , method="breslow");


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
