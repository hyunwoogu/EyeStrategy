library(tidyverse)
library(rhdf5)
library(MASS)
library(survival)

dd = survreg(Surv(Duration, UnCen) ~1, data=DataFraFirst, dist="weibull")
dd$y

shape = 1/dd$scale
scale = exp(dd$coef)



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


mean(DataFraFirst$Duration)
scale * gamma(1 + 1/shape)


weibMLEfinder = function(x_de, y_de, xWin, yWin)
{
  Dat = DataFraFirst %>% filter(x >= x_de & x < x_de + xWin,
                                y >= y_de & y < y_de + yWin) %>% 
    dplyr::select(Duration, UnCen)
  dd = survreg(Surv(Duration, UnCen) ~1, data=DataFraFirst, dist="weibull")
  dd$y
  
  shape = 1/dd$scale
  scale = exp(dd$coef)
  
  scale * gamma(1 + 1/shape)
}

sum_t = sum(DataFirst_i.loc[(DataFirst_i.x >= x) & (DataFirst_i.x < x + Xwin) & \
                            (DataFirst_i.y >= y) & (DataFirst_i.y < y + Ywin) , 'Duration'])

sum_delta = sum(DataFirst_i.loc[(DataFirst_i.x >= x) & (DataFirst_i.x < x + Xwin) & \
                                (DataFirst_i.y >= y) & (DataFirst_i.y < y + Ywin) ,'UnCen'])


numSliceX = 50
numSliceY = 50

XwinSize = 1600/numSliceX
YwinSize = 1200/numSliceY

SegsX = np.arange(0, 1600-XwinSize, XwinSize)
SegsY = np.arange(0, 1200-YwinSize, YwinSize)

res = []
for j in SegsY:
  for i in SegsX:
  res.append(expMLEfinder(i,j, XwinSize, YwinSize))

res = np.reshape(res, [numSliceY-1, numSliceX-1]) 


