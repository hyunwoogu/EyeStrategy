# Likelihood Inference 
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+





# Exponential & Weibull Survival Curve
i = 4

expSA = function(i)
{
  DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i) 
  
  numer = sum(DataFraFirst_i$Duration)
  denom = sum(DataFraFirst_i$UnCen)
  
  return(numer/denom)
}

res = NULL
for (i in 1:29)
{
  res = c(res, expSA(i))
}


write.csv(res, "ExpHeat.csv")


weibSA = function(i)
{
  DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i) 
  
  foo = survreg(Surv(Duration, UnCen) ~1, 
                data=DataFraFirst_i, dist="weibull")
  
  shape = 1/foo$scale
  scale = exp(foo$coef)
  
  return(scale * gamma(1 + 1/shape))
}


res2 = NULL
for (i in 1:29)
{
  res2 = c(res2, expSA(i))
}

resData = data.frame(Subject=1:29,
                     ExponLambda = res,
                     WeibEV = res2)

resDataM = melt(resData, measure.vars=c("ExponLambda", "WeibEV"),
                variable.name="Estimates")


ggplot(resDataM) + geom_bar(aes(Subject, value, fill=Estimates),
                        stat='identity', position='dodge')






## Exponential MLE - Method1

i = 4
DataFraFirst_i = DataFraFirst %>% filter(SUBJECTINDEX==i)

expMLEfinder = function(x_de, y_de, xWin, yWin)
{
  Dat = DataFraFirst_i %>% filter(x >= x_de & x < x_de + xWin,
                                y >= y_de & y < y_de + yWin) %>% 
    dplyr::select(Duration, UnCen)

  numer = sum(Dat$Duration)
  denom = sum(Dat$UnCen)
  
  if (denom == 0) return(0)
  return(numer/denom)
}

numSliceX = 50
numSliceY = 50

XwinSize = 1600/numSliceX
YwinSize = 1200/numSliceY

SegsX = seq(0, 1600-XwinSize, by=XwinSize)
SegsY = seq(0, 1200-YwinSize, by=YwinSize)

res = NULL
for (j in SegsY)
{
  for (i in SegsX)
  {
    res = c(res, expMLEfinder(i,j, XwinSize, YwinSize))
  }
}

res1 = matrix(res, nrow=numSliceY, ncol=numSliceX, byrow=T)
res2= apply(res1,2,rev)


mat.melted = melt(res2)
ggplot(mat.melted, aes(x = Var2, y = Var1, fill = value)) + geom_tile()

# setwd("../Dropbox/2018Autumn/GradThesis/EyeTracking_data")
write.csv(res1, "ExpHeat.csv")




## Weibull MLE
weibMLEfinder = function(x_de, y_de, xWin, yWin)
{
  Dat = DataFraFirst_i %>% filter(x >= x_de & x < x_de + xWin,
                                y >= y_de & y < y_de + yWin) %>% 
    dplyr::select(Duration, UnCen)
  foo = survreg(Surv(Duration, UnCen) ~1, data=Dat, dist="weibull")
  
  shape = 1/foo$scale
  scale = exp(foo$coef)
  
  return(scale * gamma(1 + 1/shape))
}

weibMLEfinder



numSliceX = 50
numSliceY = 50

XwinSize = 1600/numSliceX
YwinSize = 1200/numSliceY

SegsX = seq(0, 1600-XwinSize, by=XwinSize)
SegsY = seq(0, 1200-YwinSize, by=YwinSize)

res = NULL
for (j in SegsY)
{
  for (i in SegsX)
  {
    res = c(res, weibMLEfinder(i,j, XwinSize, YwinSize))
  }
}

warnings()

res1 = matrix(res, nrow=numSliceY, ncol=numSliceX, byrow=T)
res2= apply(res1,2,rev)

mat.melted = melt(res2)
ggplot(mat.melted, aes(x = Var2, y = Var1, fill = value)) + geom_tile()

write.csv(res1, "WeibHeat.csv")





setwd("../Dropbox/2018Autumn/GradThesis/EyeTracking_data")
