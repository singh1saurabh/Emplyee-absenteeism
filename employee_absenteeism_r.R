rm(list = ls())
getwd()
library(readxl)
getwd()
EA=Absenteeism_at_work_Project
x = c("ggplot2","ggplot", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','rpart','Metrics')
install.packages(x)
lapply(x, require, character.only = TRUE)
       dim(EA)
       head(EA)       
length(unique(EA$`Absenteeism time in hours`))    

#renaming the variables
names(EA)[2]= "Reason.for.absence"
names(EA)[3]= "Month.of.absence"
names(EA)[4]= "Day.of.the.week"
names(EA)[6]= "Transportation.expense"
names(EA)[7]= "Distance.from.residence.to.work"
names(EA)[8]= "Service.time" 
names(EA)[10]= "Workload.average.perday"
names(EA)[11]= "Hit.target"
names(EA)[12]= "Disciplinary.failure"
names(EA)[15]= "Social.drinker"
names(EA)[16]= "Social.smoker" 
names(EA)[20]= "Body.mass.index"
names(EA)[21]= "Absenteeism.time.in.hours"
EA
# Checking missing values
sum(is.na(EA))
missing_val = data.frame(apply(EA,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
row.names(missing_val) = NULL
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(EA)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
missing_val = missing_val[,c(2,1)]


sum(is.na(EA))
#actual 31
# mean 23.2
#median 25 
#EA[2,20]
#EA[2,20] = NA


# Freezing mean and median for missing value analysis

EA$Reason.for.absence[is.na(EA$Reason.for.absence)] = median(EA$Reason.for.absence,na.rm = T)
EA$Month.of.absence[is.na(EA$Month.of.absence)] = median(EA$Month.of.absence,na.rm = T)
EA$Transportation.expense[is.na(EA$Transportation.expense)] = median(EA$Transportation.expense,na.rm = T)
EA$Distance.from.residence.to.work[is.na(EA$Distance.from.residence.to.work)] = median(EA$Distance.from.residence.to.work,na.rm = T)
EA$Service.time[is.na(EA$Service.time)] = median(EA$Service.time,na.rm = T)
EA$Age[is.na(EA$Age)] = median(EA$Age,na.rm = T)
EA$Workload.average.perday[is.na(EA$Workload.average.perday)] = median(EA$Workload.average.perday,na.rm = T)
EA$Hit.target[is.na(EA$Hit.target)] = median(EA$Hit.target,na.rm = T)
EA$Disciplinary.failure[is.na(EA$Disciplinary.failure)] = median(EA$Disciplinary.failure,na.rm = T)
EA$Education[is.na(EA$Education)] = median(EA$Education,na.rm = T)
EA$Son[is.na(EA$Son)] = median(EA$Son,na.rm = T)
EA$Social.drinker[is.na(EA$Social.drinker)] = median(EA$Social.drinker,na.rm = T)
EA$Social.smoker[is.na(EA$Social.smoker)] = median(EA$Social.smoker,na.rm = T)
EA$Pet[is.na(EA$Pet)] = median(EA$Pet,na.rm = T)
EA$Weight[is.na(EA$Weight)] = median(EA$Weight,na.rm = T)
EA$Height[is.na(EA$Height)] = median(EA$Height,na.rm = T)
EA$Body.mass.index[is.na(EA$Body.mass.index)] = mean(EA$Body.mass.index,na.rm = T)
EA$Absenteeism.time.in.hours[is.na(EA$Absenteeism.time.in.hours)] = median(EA$Absenteeism.time.in.hours,na.rm = T)


sum(is.na(EA))

#changing numeric to categorical

EA$ID = as.factor(EA$ID)
EA$Reason.for.absence = as.factor(EA$Reason.for.absence)
EA$Month.of.absence = as.factor(EA$Month.of.absence)
EA$Day.of.the.week = as.factor(EA$Day.of.the.week)
EA$Seasons= as.factor(EA$Seasons)
EA$Disciplinary.failure = as.factor(EA$Disciplinary.failure)
EA$Education = as.factor(EA$Education)
EA$Social.drinker= as.factor(EA$Social.drinker)
EA$Social.smoker = as.factor(EA$Social.smoker)

# outlier analysis 

numeric_index = sapply(EA,is.numeric)
numeric_data = EA[,numeric_index]
numeric_data= as.data.frame(numeric_data)
cnames = colnames(numeric_data)

# boxplot for numeric data
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Absenteeism.time.in.hours"), data = subset(EA))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot of absenteeism for",cnames[i])))
}

#  Plotting plots together
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,ncol=2)
gridExtra::grid.arrange(gn7,gn8,ncol=2)
gridExtra::grid.arrange(gn9,gn10,ncol=2)
gridExtra::grid.arrange(gn11,gn12,ncol=2)

val = EA$Transportation.expense[EA$Transportation.expense %in% boxplot.stats(EA$Transportation.expense)$out]
EA$Transportation.expense[(EA$Transportation.expense %in% val)] = NA
EA$Transportation.expense[is.na(EA$Transportation.expense)] = median(EA$Transportation.expense, na.rm = T)
summary(EA$Transportation.expense)

val_1 =EA$Distance.from.residence.to.work[EA$Distance.from.residence.to.work %in% boxplot.stats(EA$Distance.from.residence.to.work)$out]
EA$Distance.from.residence.to.work[(EA$Distance.from.residence.to.work %in% val1)] = NA
EA$Distance.from.residence.to.work[is.na(EA$Distance.from.residence.to.work)] = median(EA$Distance.from.residence.to.work, na.rm = T)
summary(EA$Distance.from.residence.to.work)

val_2 = EA$Service.time[EA$Service.time %in% boxplot.stats(EA$Service.time)$out]
EA$Service.time[(EA$Service.time %in% val2)] = NA
EA$Service.time[is.na(EA$Service.time)] = median(EA$Service.time, na.rm = T)
summary(EA$Service.time)

val_3 = EA$Age[absent$Age %in% boxplot.stats(EA$Age)$out]
EA$Age[(EA$Age %in% val3)] = NA
EA$Age[is.na(EA$Age)] = median(EA$Age, na.rm = T)
summary(EA$Age)

val_4 = EA$Workload.average.perday[EA$Workload.average.perday %in% boxplot.stats(EA$Workload.average.perday)$out]
EA$Workload.average.perday[(EA$Workload.average.perday %in% val4)] = NA
EA$Workload.average.perday[is.na(EA$Workload.average.perday)] = median(EA$Workload.average.perday, na.rm = T)
summary(EA$Workload.average.perday)

val_5 = EA$Hit.target[EA$Hit.target %in% boxplot.stats(absent$Hit.target)$out]
EA$Hit.target[(EA$Hit.target %in% val5)] = NA
EA$Hit.target[is.na(EA$Hit.target)] = median(EA$Hit.target, na.rm = T)
summary(EA$Hit.target)

val_6 = EA$Weight[EA$Weight %in% boxplot.stats(EA$Weight)$out]
EA$Weight[(EA$Weight %in% val6)] = NA
EA$Weight[is.na(EA$Weight)] = median(EA$Weight, na.rm = T)
summary(EA$Weight)

val_7 = EA$Height[EA$Height %in% boxplot.stats(EA$Height)$out]
EA$Height[(EA$Height %in% val7)] = NA
EA$Height[is.na(EA$Height)] = median(EA$Height, na.rm = T)
summary(EA$Height)

val8 = EA$Body.mass.index[EA$Body.mass.index %in% boxplot.stats(EA$Body.mass.index)$out]
EA$Body.mass.index[(EA$Body.mass.index %in% val8)] = NA
EA$Body.mass.index[is.na(EA$Body.mass.index)] = median(EA$Body.mass.index, na.rm = T)
summary(EA$Body.mass.index)

val_9 = EA$Pet[EA$Pet %in% boxplot.stats(EA$Pet)$out]
EA$Pet[(EA$Pet %in% val9)] = NA
EA$Pet[is.na(EA$Pet)] = median(EA$Pet, na.rm = T)
summary(EA$Pet)

val_10 = EA$Son[EA$Son %in% boxplot.stats(EA$Son)$out]
EA$Son[(EA$Son %in% val10)] = NA
EA$Son[is.na(EA$Son)] = median(EA$Son, na.rm = T)
summary(EA$Son)

# correlation for continuous variables
corrgram(EA[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

# anova for categorical data
str(EA)
result = aov(formula=Absenteeism.time.in.hours~ID, data = EA)
summary(result)
result1 = aov(formula=Absenteeism.time.in.hours~Reason.for.absence, data = EA)
summary(result1)
result2 = aov(formula=Absenteeism.time.in.hours~Month.of.absence, data = EA)
summary(result2)
result3 = aov(formula=Absenteeism.time.in.hours~Day.of.the.week, data = EA)
summary(result3)
result4 = aov(formula=Absenteeism.time.in.hours~Seasons, data = EA)
summary(result4)
result5 = aov(formula=Absenteeism.time.in.hours~Disciplinary.failure, data = EA)
summary(result5)
result6 = aov(formula=Absenteeism.time.in.hours~Education, data = EA)
summary(result6)
result7 = aov(formula=Absenteeism.time.in.hours~Social.smoker, data = EA)
summary(result7)
result8 = aov(formula=Absenteeism.time.in.hours~Social.drinker, data = EA)
summary(result8)


# Feature selection(important variables)
EA_1 = subset(EA,select=-c(ID,Seasons,Disciplinary.failure,Pet,Age,Education,Son,Social.smoker,Body.mass.index,Height,Hit.target,Social.drinker))
str(EA_1)


#checking data distributions
qqnorm(EA_1$Transportation.expense)
hist(EA_1$Transportation.expense)
qqnorm(EA_1$Distance.from.residence.to.work)
hist(EA_1$Distance.from.residence.to.work)
qqnorm(EA_1$Service.time)
hist(EA_1$Service.time)
qqnorm(EA_1$Workload.average.perday)
hist(EA_1$Workload.average.perday)
qqnorm(EA_1$Weight)
hist(EA_1$Weight)

# Feature Scaling

cnames1 = c("Transportation.expense","Distance.from.residence.to.work","Service.time","Weight",
            "Workload.average.perday","Absenteeism.time.in.hours")
for (i in cnames1) 
{
  print(i)
  EA_1[,i] = (EA_1[,i]-min(EA_1[,i]))/(max(EA_1[,i]-min(EA_1[,i])))
} 
range(EA_1$Transportation.expense)
range(EA_1$Distance.from.residence.to.work)
range(EA_1$Service.time)
range(EA_1$Workload.average.perday)
range(EA_1$Weight)
EA_1
str(EA_1)


# histogram after normalisation

qqnorm(EA_1$Transportation.expense)
hist(EA_1$Transportation.expense)
qqnorm(EA_1$Distance.from.residence.to.work)
hist(EA_1$Distance.from.residence.to.work)
qqnorm(EA_1$Service.time)
hist(EA_1$Service.time)
qqnorm(EA_1$Workload.average.perday)
hist(EA_1$Workload.average.perday)
qqnorm(EA_1$Weight)
hist(EA_1$Weight)

# MACHINE LEARNING ALGORITHMS

EA_1 = as.data.frame(EA_1)
train_ind = sample(1:nrow(EA_1),0.8*nrow(EA_1))                                                                                      
train = EA_1[train_ind,]
test = EA_1[-train_ind,]

# Decision trees

set.seed(1234)
fit = rpart(Absenteeism.time.in.hours~.,data = train, method = 'anova')
summary(fit)
prediction_dt = predict(fit,test[,-9])
actual = test[,9]
predicted1 = data.frame(prediction_dt)


# Random forest

set.seed(1234)
rf_mod = randomForest(Absenteeism.time.in.hours~.,train,importance = TRUE,ntree = 100)
rf_pred = predict(rf_mod,test[,-9])
actual = test[,9]
predicted1 = data.frame(rf_pred)
rmse(preds = rf_pred, actuals = actual, weights = 1, na.rm = FALSE)

#for ntree = 200

set.seed(1234)
rf_mod1 = randomForest(Absenteeism.time.in.hours~.,train,importance = TRUE,ntree = 200)
rf_pred1 = predict(rf_mod1,test[,-9])
actual = test[,9]
predicted2 = data.frame(rf_pred1)
rmse(preds = rf_pred1, actuals = actual, weights = 1, na.rm = FALSE) 
# error rate =11.093 #accuracy 88.907

# for ntree = 300

set.seed(1234)
rf_mod2 = randomForest(Absenteeism.time.in.hours~.,train,importance = TRUE,ntree = 300)
rf_pred2 = predict(rf_mod2,test[,-9])
actual = test[,9]
predicted3 = data.frame(rf_pred2)
rmse(preds = rf_pred2, actuals = actual, weights = 1, na.rm = FALSE)
# error rate = 11.147  #accuracy 88.853

# for ntree = 500

set.seed(1234)
rf_mod3 = randomForest(Absenteeism.time.in.hours~.,train,importance = TRUE,ntree = 500)
rf_pred3 = predict(rf_mod3,test[,-9])
actual = test[,9]
predicted4 = data.frame(rf_pred3)
rmse(preds = rf_pred3, actuals = actual, weights = 1, na.rm = FALSE)
# error rate = 11.172 #accuracy = 88.828

#LINEAR REGRESSION
#vif
library(car)
mymodel = lm(Absenteeism.time.in.hours~.,data= EA_1)
vif(mymodel)
# LR Model

set.seed(1234)
absent = as.data.frame(absent)
droplevels(absent_final$Reason.for.absence)
train_in1 = sample(1:nrow(absent),0.8*nrow(absent_final))
train1 = absent_final[train_in1,]
test1 = absent_final[-train_in1,]
lr_model = lm(Absenteeism.time.in.hours~.,data = train1)
summary(lr_model)
predictions_lr = predict(lr_model,test1[,1:8])
predicted_lr = data.frame(predictions_lr)
actual1 = test1[,9]
rmse(preds = predictions_lr, actuals = actual, weights = 1, na.rm = FALSE)
# error rate = 11.052  #accuracy 88.948

# creating subset(part 2 for loss calculation)

loss = subset(EA,select = c(Month.of.absence,Service.time,Absenteeism.time.in.hours,Workload.average.perday))  

# Workloss/month = (absent time * workload)/service time    mathematical formula
loss["month.loss"]=with(loss,((loss[,"Workload.average.perday"]*loss[,"Absenteeism.time.in.hours"])/loss[,"Service.time"]))
for (i in 9) {
  emp = loss[which(loss["Month.of.absence"]==i),]
  print(sum(emp$month.loss))
}

print(emp$month.loss) 


