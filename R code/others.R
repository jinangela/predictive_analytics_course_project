library("e1071", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
# library("moments", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")

########Months Since Last Order########
summary(train$recmon)
skewness(train$recmon) # > 0, right skewed
boxplot(train$recmon, main = "Months Since Last Order")
hist(train$recmon, main = "Months Since Last Order")
qqnorm(y = train$recmon, main = "Months Since Last Order")
qqline(train$recmon)
# log transformation -- left skewed
skewness(log(train$recmon+1)) # < 0, left skewed
boxplot(log(train$recmon+1), main = "LOG(Months Since Last Order)")
hist(log(train$recmon+1), main = "LOG(Months Since Last Order)")
# square root transformation
skewness(sqrt(train$recmon)) # OK
boxplot(sqrt(train$recmon), main = "SQRT(Months Since Last Order)")
hist(sqrt(train$recmon), main = "SQRT(Months Since Last Order)")
qqnorm(sqrt(train$recmon), main = "SQRT(Months Since Last Order)")
qqline(sqrt(train$recmon))

########Time on File########
summary(train$tof)
skewness(train$tof) # > 0, right skewed
boxplot(train$tof, main = "Time on File")
hist(train$tof, main = "Time on File")
qqnorm(y = train$tof, main = "Time on File")
qqline(train$tof)
# log transformation -- OK
skewness(log(train$tof)) # OK
boxplot(log(train$tof), main = "LOG(Time on File)")
hist(log(train$tof), main = "LOG(Time on File)") # -- outliers at the left
# square root tranformation -- right skewed
skewness(sqrt(train$tof)) # > 0, right skewed
boxplot(sqrt(train$tof), main = "SQRT(Time on File)")
hist(sqrt(train$tof), main = "SQRT(Time on File)")
# cube root transformation
skewness(train$tof^(1/3)) # > 0, right skewed
boxplot(train$tof^(1/3), main = "CUBE ROOT(Time on File)")
hist(train$tof^(1/3), main = "CUBE ROOT(Time on File)")
# 1/4 root transformation 
skewness(sqrt(sqrt(train$tof))) # > 0, right skewed
boxplot(sqrt(sqrt(train$tof)), main = "SQRT SQRT(Time on File)")
hist(sqrt(sqrt(train$tof)), main = "SQRT SQRT(Time on File)")

########TARGAMNT########
summary(train$targamnt)
skewness(train$targamnt) # > 0, right skewed
boxplot(train$targamnt)
hist(train$targamnt)