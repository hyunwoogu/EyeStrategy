library(survMisc)
library(tidyverse)
library(rhdf5)
library(MASS)
library(survival)
library(coin)

# Whole-sample Analysis
survObj = Surv(time = DataFraFirst$Duration, 
               event = DataFraFirst$UnCen)
survFit = survfit(survObj ~ DataFraFirst$Region)


## Summary Statistics 
sumStat = summary(survFit)
sumStat$table


### LRT plot
LRTplot = data.frame(rho0 = NULL,
                     rho1 = NULL)

for (i in 1:29)
{
  DataFraFirst_i = DataFraFirst %>% dplyr::filter(SUBJECTINDEX==i)
  survObj_i = Surv(time = DataFraFirst_i$Duration, 
                   event = DataFraFirst_i$UnCen)
  testSurvDiff_i_0 = survdiff(survObj_i ~ DataFraFirst_i$Region, rho=0)
  testSurvDiff_i_1 = survdiff(survObj_i ~ DataFraFirst_i$Region, rho=1)
  
  LRTplot = rbind(LRTplot, c(testSurvDiff_i_0$chisq,
           testSurvDiff_i_1$chisq))
}

names(LRTplot) = c("rho0", "rho1")


### comp : attribute not accessible
for (i in 1:29)
{
  DataFraFirst_i = DataFraFirst %>% dplyr::filter(SUBJECTINDEX==i)
  survObj_i = Surv(time = DataFraFirst_i$Duration, 
                   event = DataFraFirst_i$UnCen)
  survFit_i = survfit(survObj_i ~ DataFraFirst_i$Region)
  comp(ten(survFit_i),  p=c(0, 1, 0), q=c(0, 0, 1))
}


## AFT 
regobj.aft1 = survreg(survObj ~ 1 + as.factor(DataFraFirst$Region), dist="weibull")
regobj.aft2 = survreg(survObj ~ 1 + as.factor(DataFraFirst$Region) +
                        DataFraFirst$start, dist="weibull")
regobj.aft3 = survreg(survObj ~ 1 + as.factor(DataFraFirst$Region) +
                        DataFraFirst$start + as.factor(DataFraFirst$SUBJECTINDEX), dist="weibull")
regobj.aft32 = survreg(survObj ~ 1 + as.factor(DataFraFirst$Region) +
                        as.factor(DataFraFirst$SUBJECTINDEX), dist="weibull")


summary(regobj.aft1)
test$wald
summary(regobj.aft2)
summary(regobj.aft3)
summary(regobj.aft32)

extractAIC(regobj.aft1)
extractAIC(regobj.aft2)
extractAIC(regobj.aft3)
extractAIC(regobj.aft32)


regobj.aft4 = survreg(survObj ~ 1 + as.factor(DataFraFirst$Region), dist="loglogistic")
regobj.aft5 = survreg(survObj ~ 1 + as.factor(DataFraFirst$Region) +
                        DataFraFirst$start, dist="loglogistic")
regobj.aft6 = survreg(survObj ~ 1 + as.factor(DataFraFirst$Region) +
                        DataFraFirst$start + as.factor(DataFraFirst$SUBJECTINDEX), dist="loglogistic")
regobj.aft62 = survreg(survObj ~ 1 + as.factor(DataFraFirst$Region) +
                        as.factor(DataFraFirst$SUBJECTINDEX), dist="loglogistic")



summary(regobj.aft4)
summary(regobj.aft5)
summary(regobj.aft6)
summary(regobj.aft62)

extractAIC(regobj.aft4)
extractAIC(regobj.aft5)
extractAIC(regobj.aft6)
extractAIC(regobj.aft62)

summary(coxph(Surv(futime, fustat) ~ age + strata(rx), data=ovarian))
fit.phfix3_Strata = coxph(survObj ~ 1 + + as.factor(DataFraFirst$Region) +
                            DataFraFirst$start )
a = summary(fit.phfix3_Strata)
extractAIC(fit.phfix3_Strata)
a 

b = basehaz(fit.phfix3_Strata)
b$hazard
ggplot(b, aes(hazard, time)) + geom_point()
a$coefficientsZ
?strata

?basehaz
## Cox PH
fit.phfix1 = coxph(survObj ~ 1 + as.factor(DataFraFirst$Region))
fit.phfix2 = coxph(survObj ~ 1 + as.factor(DataFraFirst$Region) +
                        DataFraFirst$start)
fit.phfix3 = coxph(survObj ~ 1 + as.factor(DataFraFirst$Region) +
                        DataFraFirst$start + as.factor(DataFraFirst$SUBJECTINDEX))
fit.phfix32 = coxph(survObj ~ 1 + as.factor(DataFraFirst$Region) +
                     as.factor(DataFraFirst$SUBJECTINDEX))


(fit.phfix1.sum = summary(fit.phfix1))
(fit.phfix2.sum = summary(fit.phfix2))
(fit.phfix3.sum = summary(fit.phfix3))
(fit.phfix32.sum = summary(fit.phfix32))

extractAIC(fit.phfix1)
extractAIC(fit.phfix2)
extractAIC(fit.phfix3)


#fit.phfix32 = coxph(survObj ~ 1 + as.factor(DataFraFirst$Region) +
#                      DataFraFirst$start + as.factor(DataFraFirst$SUBJECTINDEX), method="breslow");
#fit.phfix33 = coxph(survObj ~ 1 + as.factor(DataFraFirst$Region) +
#                      DataFraFirst$start + as.factor(DataFraFirst$SUBJECTINDEX), method="exact");

extractAIC(fit.phfix3)
extractAIC(fit.phfix32)
extractAIC(fit.phfix33)


C = matrix(c(1, -1, 0, 0,
             0, -1, 1, 0), nrow=2, byrow=TRUE)

numer = C %*% a$coefficients
denom = C %*% a$ %*% t(C)
WALD = t(numer) %*% solve(denom) %*% numer

1 - pchisq(q=WALD, df=2)


C = matrix(c(1, -1, rep(0, 30),
             0, -1, 1, rep(0, 29)), nrow=2, byrow=TRUE)

numer = C %*% fit.phfix3$coefficients
denom = C %*% fit.phfix3$var %*% t(C)
WALD = t(numer) %*% solve(denom) %*% numer

1 - pchisq(q=WALD, df=2)

ggplot(DataFraFirst, aes(x=Region, y=Duration)) + geom_boxplot()

DataFraFirst %>% group_by(Region) %>% summarise(Me = mean(Duration),
                                                Med= median(Duration),
                                                SD = sd(Duration))


fit.phfix3.sum$loglik
fit.phfix3.sum$coefficients
fit.phfix3.sum$sctest
fit.phfix3.sum$waldtest
fit.phfix3.sum$logtest





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Participant-wise Analysis 


DataFraFirst = DataFraFirst %>% mutate(Left = (x < 800),
                                       Top  = (y < 600))

DataFraFirst_i = DataFraFirst %>% dplyr::filter(SUBJECTINDEX==i)

survObj = Surv(time = DataFraFirst$Duration, 
               event = DataFraFirst$UnCen)

test_res = survdiff(survObj ~ as.factor(DataFraFirst$Top * 1), rho=0)
length(test_res$n)

test_res = survdiff(survObj ~ as.factor(DataFraFirst$Top * 1), rho=0)


ph_res = survdiff(survObj ~ as.factor(DataFraFirst$Top * 1), rho=0)



ggplot(DataFraFirst, aes(x = as.factor(SUBJECTINDEX), 
                         y = Duration, fill=as.factor(Top))) + geom_boxplot()

DataFraFirst$SUBJECTINDEX %>% table

df2<-melt(df1,id.var=c("ID","Tool","Name"))

p <- ggplot(df2, aes(variable, value,fill=Tool))
p + geom_boxplot() + labs(title = "CMP")



## Is_proportional


DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==6)
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
plot(obs_time, KM_surv, type='s')


dd = survreg(Surv(Duration, UnCen) ~1, data=DataFraFirst_i, dist="weibull")

shape = 1/dd$scale
scale = exp(dd$coef)

support = seq(0, 1500, by=.1)
ddd = 1- pweibull(support, shape=shape, scale=scale)
lines(support, ddd, col='red')


plot(density(DataFraFirst$Duration))




DataFraSecond = DataFra[DataFra$count == 2, ]






# Participant-wise 

## Summary Statistics 



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



## Cox


survobj.aft = Surv(time=DataFraFirst$Duration, 
                   event=DataFraFirst$UnCen)


Fit_ph = coxph(survobj.aft ~  as.factor(DataFraFirst$Region) + DataFraFirst$start +
                 DataFraFirst$constant)
Fit_ph = coxph(survobj.aft ~  as.factor(DataFraFirst$Region) 
               + DataFraFirst$start + DataFraFirst$constant +
                 factor(DataFraFirst$SUBJECTINDEX, levels = sample(1:29)))


summary(Fit_ph)

DataFraFirst$constant %>% table

ggplot(DataFraFirst, aes(x=constant, y=Duration, fill=Region)) + geom_boxplot()



## AFT
regobj.aft1 = survreg(survobj.aft ~ 1 + as.factor(DataFraFirst$Region), dist="weibull")

regobj.aft2 = survreg(survobj.aft ~ 1 + as.factor(DataFraFirst$Region) + 
                        DataFraFirst$start, dist="weibull") ;

regobj.aft3 = survreg(survobj.aft ~ 1 + as.factor(DataFraFirst$Region) + 
                        DataFraFirst$start + DataFraFirst$constant, dist="weibull") ;

regobj.aft4 = survreg(survobj.aft ~ 1 + as.factor(DataFraFirst$Region) + 
                        DataFraFirst$start + DataFraFirst$constant +
                        as.factor(DataFraFirst$SUBJECTINDEX), dist="weibull") ;

extractAIC(regobj.aft1)
extractAIC(regobj.aft2)
extractAIC(regobj.aft3)
extractAIC(regobj.aft4)

summary(regobj.aft1)




fit.phfix = coxph(survobj.aft ~  factor(larynx$stage) + larynx$age, method="breslow");

#fit.phfix = coxph(survobj.aft ~  factor(larynx$stage) + larynx$age, method="exact");
#fit.phfix = coxph(survobj.aft ~  factor(larynx$stage) + larynx$age, method="efron"); #기본값으로 실행

fit.phfix = coxph(survobj.aft ~  as.factor(DataFraFirst$Region) 
                  + DataFraFirst$start + factor(DataFraFirst$SUBJECTINDEX, levels = sample(1:29)))

summary(fit.phfix)

C = matrix(c(1, -1, 0, 0,
             0, -1, 1, 0), nrow=2, byrow=TRUE)

numer = C %*% fit.phfix$coefficients
denom = C %*% fit.phfix$var %*% t(C)
WALD = t(numer) %*% solve(denom) %*% numer




survobj.aft = Surv(time=DataFraFirst_i$Duration, 
                   event=DataFraFirst_i$UnCen)

i = 1
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


