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
  
  NA_pm = gamma*exp(- NA_cumhzd)*std_NA
  
  stderr_GW = KM_surv * sqrt(var_GW)
  up_GW = KM_surv + gamma*stderr_GW
  lo_GW = KM_surv - gamma*stderr_GW
  
  GW_pm = gamma*stderr_GW
  
  res = data.frame(Subject = sprintf("Subject%02d", indx),
                    obsTimes = obs_time,
                    Nrisk = n_risk,
                    Nevent = n_event,
                    KMsv = KM_surv,
                    GWpm = GW_pm,
                    NAsv = NA_surv,
                    NApm = NA_pm,
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
 
SurvData %>% dplyr::group_by(Subject) %>% dplyr::summarise(Until = max(obsTimes)) %>%
  arrange(Until) %>% print(n=29)

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


SurvData

survEstimates = function(obs, tTime)
{
  inx = findInterval(obs, c(100, 200, 300, 400, 500), left.open = T) %>% diff
  res = obs[as.logical(c(inx,0))]
  return(res[tTime])
}

obs = SurvData %>% filter(Subject=="Subject03") %>% pull(obsTimes)

inx %>% length
obs %>% length
as.logical(inx)

majorIndex = SurvData %>% dplyr::group_by(Subject) %>% 
  dplyr::summarise(first = survEstimates(obsTimes, 1),
                   second = survEstimates(obsTimes, 2),
                   third = survEstimates(obsTimes, 3),
                   fourth = survEstimates(obsTimes, 4),
                   fifth = survEstimates(obsTimes, 5))

SurvData %>% names


newSurvData2 = data.frame(NULL)
for (i in 1:29)
{
  MidRes = NULL
  for (j in 1:5)
  {
    DataData = SurvData %>% dplyr::filter(Subject==sprintf("Subject%02d", i)) 
    
    if(is.na(majorIndex[i,j+1])) {
      MidRes = c(MidRes, NA)
    } else {
      theTimeIndex = match(majorIndex[i,j+1], DataData$obsTimes)
      MidRes = c(MidRes, paste0(round(DataData$NAsv[theTimeIndex], 2), "±", 
                                round(DataData$NApm[theTimeIndex], 2)))
    }
  }
  
  ResRes = data.frame(Subject = sprintf("Subject%02d", i),
                      p100ms = MidRes[1],
                      p200ms = MidRes[2],
                      p300ms = MidRes[3],
                      p400ms = MidRes[4],
                      p500ms = MidRes[5])
  MidRes = NULL
  newSurvData2 = rbind(newSurvData2, ResRes)
}



newSurvData = data.frame(NULL)
for (i in 1:29)
{
  MidRes = NULL
  for (j in 1:5)
  {
    DataData = SurvData %>% dplyr::filter(Subject==sprintf("Subject%02d", i)) 
    
    if(is.na(majorIndex[i,j+1])) {
      MidRes = c(MidRes, NA)
    } else {
      theTimeIndex = match(majorIndex[i,j+1], DataData$obsTimes)
      MidRes = c(MidRes, paste0(round(DataData$KMsv[theTimeIndex], 2), "±", 
                                round(DataData$GWpm[theTimeIndex], 2)))
    }
  }
  
  ResRes = data.frame(Subject = sprintf("Subject%02d", i),
                      p100ms = MidRes[1],
                      p200ms = MidRes[2],
                      p300ms = MidRes[3],
                      p400ms = MidRes[4],
                      p500ms = MidRes[5])
  MidRes = NULL
  newSurvData = rbind(newSurvData, ResRes)
}


SurvDataRes = bind_rows(newSurvData, setNames(newSurvData2, names(newSurvData))) %>% arrange(Subject)

warnings()

setwd("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/")
write.csv(newSurvData2, "SurvSummary2.csv")



#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
# Meds 

MedData = NULL
for (p in 1:29)
{
  DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==p)
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
  
  res = data.frame(obsTimes = obs_time,
                   Nrisk = n_risk,
                   Nevent = n_event,
                   KMsv = KM_surv,
                   NAsv = NA_surv,
                   upNA = up_NA,
                   loNA = lo_NA,
                   upGW = up_GW,
                   loGW = lo_GW)
  
  indxx = findInterval(my_fit_summ_i$table["median"], res$obsTimes)
  res2 = data.frame(Subject = sprintf("Subject%02d", p),
                    Med = my_fit_summ_i$table["median"],
                    NAup = res$upNA[indxx],
                    NAlo = res$loNA[indxx],
                    GWup = res$upGW[indxx],
                    GWlo = res$loGW[indxx])
  
  MedData = rbind(MedData, res2)
}


MedData %>% arrange(desc(Med)) %>% 
  ggplot(aes(y=Med, x=Subject, color=Subject)) + 
  geom_point() + 
  # geom_errorbar(aes(ymin = NAlo, ymax = NAup, fill="red"), width = 0.2) +
  # geom_errorbar(aes(ymin = GWlo, ymax = GWup, fill="blue"), width = 0.2) +
  coord_flip() +
  theme_light() +
  xlab('Median Survival Time') +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  ggtitle("Median Survival Time of Subjects") +
  theme(plot.title=element_text(hjust=.5, size=15))

DataFraFirst %>% dplyr::group_by(as.factor(SUBJECTINDEX)) %>% dplyr::summarise(Min=min(Duration)) %>% 
  pull(Min) %>% density %>% plot




MedData2 = NULL
for (i in 1:29)
{
  DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i)
  my_surv_i = Surv(time=DataFraFirst_i$Duration,
                   event=DataFraFirst_i$UnCen)
  my_fit_i  = survfit(formula = my_surv_i ~ 1, data=my_surv_i)

  res = data.frame(Subject = sprintf("Subject%02d", i),
                   FirstQuant = quantile(my_fit_i, c(.25, .5, .75))$quantile[1],
                   Med = quantile(my_fit_i, c(.25, .5, .75))$quantile[2],
                   ThirdQuant = quantile(my_fit_i, c(.25, .5, .75))$quantile[3])
  
  MedData2 = rbind(MedData2, res)
}




MedData2 %>% arrange(desc(Med)) %>% 
  ggplot(aes(y=Med, x=Subject, color=Subject)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = FirstQuant, ymax = ThirdQuant, color=Subject), width=.2, alpha=.7) +
  # geom_errorbar(aes(ymin = GWlo, ymax = GWup, fill="blue"), width = 0.2) +
  coord_flip() +
  theme_light() +
  ylab('Survival Time(ms)') +
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  ggtitle("Quartiles of Survival Time by Subjects") +
  theme(plot.title=element_text(hjust=.5, size=15))


# Parametric Survival Curves
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
## Exp param
ExpParams = data.frame(NULL)
for (i in 1:29)
{
  DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i) 
  
  numer = sum(DataFraFirst_i$Duration)
  denom = sum(DataFraFirst_i$UnCen)
  
  lambda = (denom/numer)
  SE = lambda / sqrt(sum(denom))
  res = data.frame(Subject=sprintf("Subject%02d", i),
                   Lambda = lambda,
                   LambdaSE = SE)
  ExpParams = rbind(ExpParams, res)
}



## Weibull param
weibParam = data.frame(NULL)
for (i in 1:29)
{
  DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i) 
  
  foo = survreg(Surv(Duration, UnCen) ~1, 
                data=DataFraFirst_i, dist="weibull")
  
  shape = 1/foo$scale
  scale = exp( - foo$coef * shape)
  real_scale = exp(foo$coef)
  
  SE = c(exp(-foo$scale), exp(foo$coef)) * sqrt(rev(diag(foo$var))) / sqrt(400) ## need torecompute 
  
  res = data.frame(Subject = sprintf("Subject%02d", i),
                   alpha = shape,
                   lambda = scale,
                   alpha_SE = SE[1],
                   lambda_SE = SE[2],
                   realScale = real_scale)
  
  weibParam = rbind(weibParam, res)
}



gDot = function(lam, al)
{
  element11 = -exp(lam * (-exp(-al)) - al)
  element12 = 0
  element21 = lam * exp(lam * (-exp(-al)) - al)
  element22 = -exp(-al)
  
  return(matrix(c(element11,element12,element21,element22),
                byrow=T, nrow=2))
}



test = gDot(foo$coef, log(foo$scale))
test2 = test %*% foo$var %*% t(test)


g2Dot = function(x, y)
{
  ele1 = gamma(1 + 1/y) * (1/y) * x^(1/y - 1)
  ele2 = (x^(1/y) * gamma(1 + 1/y) * (log(x) + digamma(1+1/y)))/(y^2)
  return(matrix(c(ele1, ele2), nrow=2))
}

test3 = g2Dot(2.193462e-06, 2.395816)
t(test3) %*% test2 %*% test3


SurvData$ExpLambda = rep(ExpParams$Lambda, table(SurvData$Subject))
SurvData$weibShape = rep(weibParam$alpha, table(SurvData$Subject))
SurvData$weibScale = rep(weibParam$realScale, table(SurvData$Subject))



Infor = function(Alpha, Beta)
{
  element11 = 1/(Alpha^2) * (1+trigamma(2)+(digamma(2))^2)
  element12 = (-1) * digamma(2)/Beta
  element22 = Alpha^2/(Beta^2)
  
  return(matrix(c(element11, element12, element12, element22), 
                nrow=2, byrow=T))
}

DeltaMethod = function(Alpha, Beta)
{
  element21 = Beta^(-Alpha) * log(Beta)
  element22 = -Alpha*(Beta^(-Alpha-1))
  
  return(matrix(c(1,0,element21,element22), nrow=2, byrow=T))
}

weibParamRes = data.frame(NULL)

for(i in 1:29)
{
  alpha = weibParam[i, "alpha"]
  beta = weibParam[i, "realScale"]
  lambda = weibParam[i, "lambda"]
  Res = sqrt(diag(t(DeltaMethod(alpha, beta)) %*%
                    solve(Infor(alpha, beta)) %*% 
                    DeltaMethod(alpha, beta)))
  res = data.frame(alphaHat = alpha,
                   alphaSD = Res[1],
                   lambdaHat = lambda,
                   lambdaSD = Res[2])
 
  weibParamRes = rbind(weibParamRes, res) 
}

ParamRes = cbind(formatC(ExpParams[,2], format = "e", digits = 2),
                 formatC(ExpParams[,3], format = "e", digits = 2),
                 formatC(weibParamRes[,1], format = "e", digits = 2),
                 formatC(weibParamRes[,2], format = "e", digits = 2),
                 formatC(weibParamRes[,3], format = "e", digits = 2),
                 formatC(weibParamRes[,4], format = "e", digits = 2))

write.csv(ParamRes, "ParamRes.csv")




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

i = 14
SurvData_i = SurvData[SurvData$Subject==sprintf("Subject%02d", i),]
theta = seq(1, 1500, by=1)
ExpTheta = pexp(q= theta, rate=SurvData_i$ExpLambda)
WeibTheta = pweibull(q=theta, shape=SurvData_i$weibShape, scale=SurvData_i$weibScale)
YesItsData = data.frame(Theta=theta,
                        expTheta=ExpTheta,
                        weibTheta=WeibTheta)
ExpLambda = SurvData_i$ExpLambda[1]
WeibShape = SurvData_i$weibShape[1]
weibScale = SurvData_i$weibScale[1]

ggplot(SurvData_i, aes(x=obsTimes)) +
  geom_step(aes(y=KMsv, colour="K-M estimator"), linetype=1, alpha=0.5, show.legend=TRUE) + 
  stat_function(data=SurvData_i, fun=function(x){1-pexp(q= x, rate=ExpLambda)}, aes(colour="Exponential"), xlim=c(0,1500)) +
  stat_function(data=SurvData_i, fun=function(x){1-pweibull(q= x, shape=WeibShape, scale=weibScale)}, aes(colour="Weibull"), xlim=c(0,1500)) +
  #geom_line(YesItsData, aes(x=Theta, y=expTheta, colour="Exponential"), linetype=1, alpha=0.5, show.legend=TRUE) +
  #geom_line(YesItsData, aes(x=Theta, y=weibTheta, colour="Weibull"), linetype=1, alpha=0.5, show.legend=TRUE) +
  scale_colour_manual(name = "Survival Estimates",
                      values = c("K-M estimator"="red", "Exponential"="blue", "Weibull"="green")) +
  facet_wrap(.~Subject, ncol = 1) + 
  theme_light() + xlim(0, 1500) +
  ggtitle("Parametric Survival Curve Estimates") + 
  ylab('Probability') + xlab('time(ms)') +
  theme(plot.title=element_text(hjust=.5, size=15))



#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
## Two Participants

SurvData_i = SurvData[SurvData$Subject=="Subject04" | SurvData$Subject=="Subject14", ]


ggplot(data=SurvData_i, aes(x=obsTimes)) +
  geom_step(aes(y=KMsv, colour="K-M estimator"), linetype=1, alpha=0.5, show.legend=TRUE) + 
  geom_ribbon(aes(ymin=loGW, ymax=upGW, fill="Greenwood"), alpha=0.3, show.legend=TRUE) +
  geom_step(aes(y=NAsv, colour="exp(-(NelsonAalen))"), linetype=1, alpha=0.5, show.legend=TRUE) +
  geom_ribbon(aes(ymin=loNA, ymax=upNA, fill="Nelson-Aalen"), alpha=0.3, show.legend=TRUE) +
  scale_colour_manual(name = "Survival Estimates",
                      values = c("K-M estimator"="red", "exp(-(NelsonAalen))"="blue")) +
  facet_wrap(.~Subject, ncol = 1) + 
  scale_fill_manual(name = "Standard Error",
                    values =c("Greenwood" = "red", "Nelson-Aalen" = "blue")) +
  theme_light() + xlim(0, 1500) +
  ggtitle("Survival Curve Estimates") + 
  ylab('Probability') + xlab('time(ms)') +
  theme(plot.title=element_text(hjust=.5, size=15))



#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
## Log-rank tests
LRtestRes = data.frame(NULL)

for (i in 1:29)
{
  DataFraFirst_i = DataFraFirst %>% dplyr::filter(SUBJECTINDEX==i)
  
  survObj_i = Surv(time = DataFraFirst_i$Duration, 
                   event = DataFraFirst_i$UnCen)
  survFit_i = survfit(survObj_i ~ DataFraFirst_i$Region)
  
  assign(sprintf("lr%02d", i), ten(survFit_i, call = NULL))  
  comp(get(sprintf("lr%02d", i)),  p=c(0, 1, 1, 0.5, 0.5), q=c(1, 0, 1, 0.5, 2))
  
  chis = attr(get(sprintf("lr%02d", i)), "lrt")
  res = (chis[c(1,6,7), ] %>% unlist)[c(4:6)] %>% as.numeric
  
  LRtestRes = rbind(LRtestRes, res)
}

names(LRtestRes) = c("LogRank", "FHp0q1", "FHp1q0")
LRtestRes = rownames_to_column(LRtestRes, var="Participant")
LRtestRes$Participant = sprintf("Subject%02d", as.numeric(LRtestRes$Participant))

LRtestResM = melt(LRtestRes, measure.vars = c("LogRank", "FHp1q0", "FHp0q1"),
                  variable.name="Weight")
LRtestResM$reject = (LRtestResM$value > qchisq(.975, df=3) | LRtestResM$value < c(qchisq(.025, df=3)))

ggplot(LRtestResM, aes(x=Participant, y=value, fill=Weight, alpha=reject) ) +
  geom_bar(stat="identity", position="dodge") + 
  # scale_fill_manual(values=c("#e1e1e1", "#aeaeae", "#ff5252")) + 
  scale_alpha_manual(values = c(0.9, .3), guide = FALSE) +
  geom_hline(yintercept = c(qchisq(.025, df=3), qchisq(.975, df=3)), 
             linetype = "dashed", color='red') +
  #  annotate("text", x = 27, y= qchisq(.975, df=3), 
  #           label = paste0("Cutoff")) +
  ggtitle("Log Rank Test Statistic by Weight and Paritipant") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title=element_text(hjust=.5, size=15))


## The Lowest, The Highest


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Censoring's effect 

DataFraFirst %>% group_by(SUBJECTINDEX) %>% summarise(numCen = sum(UnCen==0)) %>%
  ggplot(aes(x=SUBJECTINDEX, y=numCen)) + geom_bar(stat="identity")

i = 29
i = 24
i = 1

i = i + 1
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


#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
AFTcoeff1 = data.frame(NULL)
for (i in 1:29)
{
  DataFraFirst_i = DataFraFirst %>% dplyr::filter(SUBJECTINDEX==i)
  survObj_i = Surv(time = DataFraFirst_i$Duration, 
                   event = DataFraFirst_i$UnCen)
  
  regobj.aft1_i = survreg(survObj_i ~ 1 + as.factor(DataFraFirst_i$Region), 
                          dist="weibull")
  c0 = rep(sprintf("Subject%02d", i), 5)
  c1 = c("Intercept", "EyeL", "EyeR", "Nose", "Log(Scale)")
  c2 = c(regobj.aft1_i$coefficients, log(regobj.aft1_i$scale))
  c3 = (diag(regobj.aft1_i$var) %>% sqrt)
  AFTcoeff1 = rbind(AFTcoeff1, cbind(c0, c1, c2, c3))
}


names(AFTcoeff1) = c("Subject", "Variable", "Estimate", "SE")
AFTcoeff1$Estimate = as.numeric(as.numeric(levels(AFTcoeff1$Estimate))[AFTcoeff1$Estimate])
AFTcoeff1$SE = as.numeric(as.numeric(levels(AFTcoeff1$SE))[AFTcoeff1$SE])
AFTcoeff1$Variable = factor(AFTcoeff1$Variable, levels=c("EyeL", "EyeR", "Nose", "Intercept", "Log(Scale)"))

ggplot(AFTcoeff1, aes(x=Subject, y=Estimate, color=Variable)) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate-SE, ymax = Estimate+SE), width = 0.2) +
  facet_wrap(.~Variable, ncol=1, scales = "free_y") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



AFTcoeff2 = data.frame(NULL)
for (i in 1:29)
{
  DataFraFirst_i = DataFraFirst %>% dplyr::filter(SUBJECTINDEX==i)
  survObj_i = Surv(time = DataFraFirst_i$Duration, 
                   event = DataFraFirst_i$UnCen)
  
  regobj.aft2_i = survreg(survObj_i ~ 1 + as.factor(DataFraFirst_i$Region) +
                            DataFraFirst_i$start, dist="weibull")
  
  c0 = rep(sprintf("Subject%02d", i), 6)
  c1 = c("Intercept", "EyeL", "EyeR", "Nose", "TTFF", "Log(Scale)")
  c2 = c(regobj.aft2_i$coefficients, log(regobj.aft2_i$scale))
  c3 = (diag(regobj.aft2_i$var) %>% sqrt)
  AFTcoeff2 = rbind(AFTcoeff2, cbind(c0, c1, c2, c3))
}

names(AFTcoeff2) = c("Subject", "Variable", "Estimate", "SE")
AFTcoeff2$Estimate = as.numeric(as.numeric(levels(AFTcoeff2$Estimate))[AFTcoeff2$Estimate])
AFTcoeff2$SE = as.numeric(as.numeric(levels(AFTcoeff2$SE))[AFTcoeff2$SE])
AFTcoeff2$Variable = factor(AFTcoeff2$Variable, 
                            levels=c("EyeL", "EyeR", "Nose", "TTFF", "Intercept", "Log(Scale)"))

cols = c(gg_color_hue(5)[1:3], "orange", gg_color_hue(5)[4:5])
ggplot(AFTcoeff2, aes(x=Subject, y=Estimate, color=Variable)) +
  geom_point() +
  scale_color_manual(values=cols) + 
  geom_errorbar(aes(ymin = Estimate-SE, ymax = Estimate+SE), width = 0.2) +
  facet_wrap(.~Variable, ncol=1, scales = "free_y") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# No dependency over the pictures
i = 4
i = i + 1
DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i)
CountPic = data.frame(DataFraFirst_i, 
                      faceCNT=ave(rep(1,length(DataFraFirst_i$SUBJECTINDEX)),
                                  DataFraFirst_i$SUBJECTINDEX,
                                  DataFraFirst_i$filenumber,
                                  FUN = cumsum))
CountPic$SUBJECTINDEX = sprintf("Subject%02d", i)

ggplot(CountPic, aes(x=faceCNT, y=Duration, colour=as.factor(UnCen))) +
  scale_colour_manual(name="Delta", values=c("black", "red")) +
  geom_point() + xlab("# of Exposure") + facet_grid(.~SUBJECTINDEX) +
  theme_light() 



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
  geom_line(aes(y=log(NAsv_cumh)), linetype=1,color='blue',alpha=0.5) + 
#  geom_point(aes(y=log(NAsv_cumh)), color='blue',alpha=0.5) + 
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

ggplot(hazrdData, aes(x=times, y=hazrd, color=Subject)) +
  geom_line(linetype=1, alpha=0.5) +
  ylab('hazard') + xlab('time') +  
  theme_light()

hazrdData_i = hazrdData[hazrdData$Subject=="Subject04" | hazrdData$Subject=="Subject14",]

ggplot(data=hazrdData_i, aes(x=times)) + 
  geom_line(aes(y=hazrd), linetype=1,color='black',alpha=0.5) + 
  facet_wrap(.~Subject, ncol=1) + 
  ylab('hazard') + xlab('time') +  xlim(0, 1500) +
  theme_light() + ggtitle("Kernel Estimates of Hazard") +
  theme(plot.title=element_text(hjust=.5, size=15))



# +++++++ Different Hazard +++++++++ for a specific Participant

i = 3 ##
i = 4
i = 9
i = 11 ##
i = 12
i = 13 ###
i = 14

i = 19

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
#  geom_line(aes(x=Nose_times, y=Nose_hazrd), linetype=1,color='purple',alpha=0.5) + 
  geom_line(aes(x=EyeL_times, y=EyeL_hazrd), linetype=1,color='green',alpha=0.5) + 
  geom_line(aes(x=EyeR_times, y=EyeR_hazrd), linetype=1,color='blue',alpha=0.5) + 
  facet_wrap(.~Subject) + 
  ylab('hazard') + xlab('time') +  
  theme_light()


res_Res = data.frame(NULL)
for (i in c(4, 11))
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
  
  res = data.frame(Subject = sprintf("Subject%02d", i),
                   Else_times = i_Else_haz$est.grid,
                   Else_hazrd = i_Else_haz$haz.est,
                   Nose_times = i_Nose_haz$est.grid,
                   Nose_hazrd = i_Nose_haz$haz.est,
                   EyeL_times = i_EyeL_haz$est.grid,
                   EyeL_hazrd = i_EyeL_haz$haz.est,
                   EyeR_times = i_EyeR_haz$est.grid,
                   EyeR_hazrd = i_EyeR_haz$haz.est)
  
  res_Res = rbind(res_Res, res)
}



ggplot(data=res_Res) + 
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

i = 8
strataData_i = strataData %>% dplyr::filter(Subject==sprintf("Subject%02d", i))
ggplot(data=strataData_i, aes(x=obsTimes)) + 
  geom_step(aes(y=KMsv, col=Group), linetype=1,alpha=0.9) + 
  facet_wrap(.~Subject, ncol=1) + 
  ylab("Survival Probability") + xlab('time') +  
  theme_light() +
  theme(strip.background =element_rect(fill="dark gray"))+
  theme(strip.text = element_text(colour = 'white', size = 12))
  # theme(strip.text.x = element_text(, angle = 90))

exp(-.002)




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