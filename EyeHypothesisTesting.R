library(tidyverse)
library(rhdf5)
library(MASS)
library(survival)
library(coin)
library(survMisc)

# Whole-sample Analysis
survObj = Surv(time = DataFraFirst$Duration, 
               event = DataFraFirst$UnCen)
survFit = survfit(survObj ~ DataFraFirst$Region)

## Log-rank tests
comp(ten(survFit),  p=c(0, 1, 1, 0.5, 0.5), q=c(1, 0, 1, 0.5, 2))


## AFT 
regobj.aft1 = survreg(survObj ~ 1 + as.factor(DataFraFirst$Region), dist="weibull")
regobj.aft2 = survreg(survObj ~ 1 + as.factor(DataFraFirst$Region) +
                        DataFraFirst$start, dist="weibull")
regobj.aft3 = survreg(survObj ~ 1 + as.factor(DataFraFirst$Region) +
                        DataFraFirst$start + as.factor(DataFraFirst$SUBJECTINDEX), dist="weibull")

summary(regobj.aft1)
summary(regobj.aft2)
summary(regobj.aft3)

extractAIC(regobj.aft1)
extractAIC(regobj.aft2)
extractAIC(regobj.aft3)


regobj.aft4 = survreg(survObj ~ 1 + as.factor(DataFraFirst$Region), dist="loglogistic")
regobj.aft5 = survreg(survObj ~ 1 + as.factor(DataFraFirst$Region) +
                        DataFraFirst$start, dist="loglogistic")
regobj.aft6 = survreg(survObj ~ 1 + as.factor(DataFraFirst$Region) +
                        DataFraFirst$start + as.factor(DataFraFirst$SUBJECTINDEX), dist="loglogistic")

summary(regobj.aft4)
summary(regobj.aft5)
summary(regobj.aft6)

extractAIC(regobj.aft4)
extractAIC(regobj.aft5)
extractAIC(regobj.aft6)



## Cox PH
fit.phfix1 = coxph(survObj ~ 1 + as.factor(DataFraFirst$Region))
fit.phfix2 = coxph(survObj ~ 1 + as.factor(DataFraFirst$Region) +
                        DataFraFirst$start)
fit.phfix3 = coxph(survObj ~ 1 + as.factor(DataFraFirst$Region) +
                        DataFraFirst$start + as.factor(DataFraFirst$SUBJECTINDEX))

summary(fit.phfix1)
summary(fit.phfix2)
summary(fit.phfix3)

extractAIC(fit.phfix1)
extractAIC(fit.phfix2)
extractAIC(fit.phfix3)


fit.phfix32 = coxph(survObj ~ 1 + as.factor(DataFraFirst$Region) +
                      DataFraFirst$start + as.factor(DataFraFirst$SUBJECTINDEX), method="breslow");
fit.phfix33 = coxph(survObj ~ 1 + as.factor(DataFraFirst$Region) +
                      DataFraFirst$start + as.factor(DataFraFirst$SUBJECTINDEX), method="exact");

extractAIC(fit.phfix3)
extractAIC(fit.phfix32)
extractAIC(fit.phfix33)




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



