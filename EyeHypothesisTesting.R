library(tidyverse)
library(rhdf5)
library(MASS)
library(survival)


## Log-rank test of Left, Top


DataFraFirst = DataFraFirst %>% mutate(Left = (x < 800),
                                       Top  = (y < 600))

DataFraFirst_i = DataFraFirst %>% dplyr::filter(SUBJECTINDEX==i)

survObj = Surv(time = DataFraFirst$Duration, 
               event = DataFraFirst$UnCen)

test_res = survdiff(survObj ~ as.factor(DataFraFirst$Top * 1), rho=0)
length(test_res$n)



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




