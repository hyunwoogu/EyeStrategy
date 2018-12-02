# Map of Importance

## Exponential MLE - Method1
expMLEfinder = function(x_de, y_de, xWin, yWin)
{
  Dat = DataFraFirst %>% filter(x >= x_de & x < x_de + xWin,
                                y >= y_de & y < y_de + yWin) %>% 
    dplyr::select(Duration, UnCen)
  foo = survreg(Surv(Duration, UnCen) ~1, data=Dat, dist="exponential")
  
  return(exp(1/(foo$coef)))
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

setwd("../Dropbox/2018Autumn/GradThesis/EyeTracking_data")
write.csv(res1, "WeibHeat.csv")




## Weibull MLE
weibMLEfinder = function(x_de, y_de, xWin, yWin)
{
  Dat = DataFraFirst %>% filter(x >= x_de & x < x_de + xWin,
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

res1 = matrix(res, nrow=numSliceY, ncol=numSliceX, byrow=T)
res2= apply(res1,2,rev)

mat.melted = melt(res2)
ggplot(mat.melted, aes(x = Var2, y = Var1, fill = value)) + geom_tile()

setwd("../Dropbox/2018Autumn/GradThesis/EyeTracking_data")
write.csv(res1, "WeibHeat.csv")