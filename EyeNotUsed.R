# Another Data
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Load the data
h5ls("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/etdb_v1.0.hdf5")
Data = h5read("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/etdb_v1.0.hdf5", "/Face Discrim.")

## Another Dataset Analysis (Do Not Run : Not applicable)
AnaDataFra %>% filter(SUBJECTINDEX %in% unique(DataFra$SUBJECTINDEX))

AnaData = h5read("../Dropbox/2018Autumn/GradThesis/EyeTracking_data/etdb_v1.0.hdf5", "/Face Learning")
AnaDataFra = data.frame(SUBJECTINDEX=AnaData$SUBJECTINDEX[1,], 
                        trial = AnaData$trial[1,], 
                        filenumber = AnaData$filenumber[1,], 
                        start = AnaData$start[1,], 
                        end = AnaData$end[1,], 
                        x = AnaData$x[1,], 
                        y = AnaData$y[1,],
                        oddball = AnaData$oddball[1,],
                        ucs = AnaData$ucs[1,])

A = AnaDataFra %>% filter(SUBJECTINDEX %in% unique(DataFra$SUBJECTINDEX))  %>%
  group_by(SUBJECTINDEX) %>% summarise(meanDuration = mean(Duration)) %>% pull(meanDuration)

B = DataFra %>% group_by(SUBJECTINDEX) %>% summarise(meanDuration = mean(Duration)) %>% pull(meanDuration)

plot(A, B) # Different Participants! -> thus not applicable! :(



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Mixed Effect
coxme(Part_surv ~ 
        age + sex + transplant + (1 | ID), data = DataFraFirst)



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Next Fixations
DataFra = DataFra %>% 
  mutate(Region = ifelse(DataFra$x >= Region1[1] & DataFra$x <= Region1[1] + XwinSize &
                           DataFra$y >= Region1[2] & DataFra$y <= Region1[2] + YwinSize, "EyeL",
                         ifelse(DataFra$x >= Region2[1] & DataFra$x <= Region2[1] + XwinSize &
                                  DataFra$y >= Region2[2] & DataFra$y <= Region2[2] + YwinSize, "EyeR", 
                                ifelse(DataFra$x >= Region3[1] & DataFra$x <= Region3[1] + XwinSize &
                                         DataFra$y >= Region3[2] & DataFra$y <= Region3[2] + YwinSize, "Nose",
                                       ifelse(DataFra$x >= Region4[1] & DataFra$x <= Region4[1] + XwinSize &
                                                DataFra$y >= Region4[2] & DataFra$y <= Region4[2] + YwinSize, "Else", "Else")))))

Data1 = DataFra[DataFra$count==1, ]
Data2 = DataFra[DataFra$count==2, ]

test = Data1 %>% left_join(Data2, by=c("SUBJECTINDEX" = "SUBJECTINDEX", 
                                       "trial" = "trial"))

DataFraFirst = DataFraFirst %>% 
  mutate(constant = apply(test[,c("Region", "Region2")], 1, function(x) {x[1]==x[2]}))

data.m = melt(test[,c("SUBJECTINDEX", "Region", "Region2")], id.vars='SUBJECTINDEX')
data.m %>% group_by(SUBJECTINDEX, variable, value) %>% summarise(cnt = n())  %>% 
  ggplot(aes(x=value, y=cnt)) + geom_bar(aes(fill=variable), position="dodge", stat="identity")

ggplot(test, aes(x=Region))
geom_bar(aes(fill = variable), position = "dodge", stat="identity")




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Frailty Model
m3 <- coxph(Surv(time1, time2, mortality) ~ age + sex + transplant + frailty(ID, 
                                                                             distribution = "gaussian", sparse = FALSE, method = "reml"), data = dat)





#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Unclassified

## Survival of first fixations of patients
Part_surv = Surv(time = DataFraFirst$Duration, 
                 event = DataFraFirst$UnCen)

Fit = survfit(Part_surv ~ SUBJECTINDEX, data = DataFraFirst)
ggsurvplot(Fit, data = DataFraFirst, pval = TRUE)


unique(DataFraFirst$SUBJECTINDEX)[match(unique(DataFraFirst$SUBJECTINDEX), 8) >= .5]


## Summary statistics

DataFraFirst %>% group_by(SUBJECTINDEX) %>% dplyr::summarise(Censored = sum(UnCen==FALSE))
Surv(time=DataFraFirst_i$Duration, event=DataFraFirst_i$UnCen)
