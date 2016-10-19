## Read in the clean data
train = read.csv("train_clean.csv", header = T)
test = read.csv("test_clean.csv", header = T)

##Multiple Regression Model

#Only look at positive sales
train2 = train_clean[train_clean$targamnt>0,]
train3 = subset(train_clean, ordcls1 > 0 | ordcls2 > 0 | ordcls3 > 0 | ordcls4 > 0 | ordcls5 > 0 | ordcls6 > 0 | ordcls7 > 0)
trial = glm(targamnt_ind ~ ordcls1+ordcls2+ordcls3+ordcls4+PR_ind, family = binomial, data = train3)
summary(trial)

#1#
train2$PR2 <- train2$totsale/train2$tof
model1 = lm(targamnt_log ~ recmon 
            # + salcls1_log + salcls2_log + salcls3_log + salcls4_log + salcls5_log + salcls6_log + salcls7_log
            + ordcls1 + ordcls2 + ordcls3 + ordcls4 + ordcls5 + ordcls6 + ordcls7
            # + prefer1 + prefer2 + prefer3 + prefer4 + prefer5 + prefer6 + prefer7
            # + breadth
            + AOA_log + AOA1_log + AOA2_log + AOA3_log + AOA4_log + AOA5_log + AOA6_log + AOA7_log
            + ord185 + ord285 + ord385 + ord485 + tof + totord + totsale + PR2,
            data = train2)
summary(model1)

model1.stepwise = step(object = model1, direction = "both")
summary(model1.stepwise) #Adjusted R = 0.1906

#test for multicollinearity
vif(model1.stepwise) #ok

#test for outliers and influential observations
r = rstudent(model1.stepwise)
outliers = r[abs(r)>3]
length(outliers) #11 outliers; ok relative to the size of the data
cook = cooks.distance(model1.stepwise)
influential = cook[cook>10]
length(influential) #0 influential observations

#2#
model2 = lm(targamnt_log ~ recmon 
            + salcls1_log + salcls2_log + salcls4_log + salcls5_log + salcls6_log + salcls7_log #salcsl3_log is removed to avoid multicollinearity
            # + ordcls1 + ordcls2 + ordcls3 + ordcls4 + ordcls5 + ordcls6 + ordcls7
            # + prefer1 + prefer2 + prefer3 + prefer4 + prefer5 + prefer6 + prefer7
            + breadth
            # + AOA_log + AOA1_log + AOA2_log + AOA3_log + AOA4_log + AOA5_log + AOA6_log + AOA7_log
            + ord185 + ord285 + ord385 + ord485 + tof + totord + totsale + PR,
            data = train2)
summary(model2)

model2.stepwise = step(object = model2, direction = "both")
summary(model2.stepwise) #Adjusted R = 0.1848

#test for multicollinearity
vif(model2.stepwise) #First round not ok because vif of salcls3_log > 10.  Remove salcsl3_log and refit the model.  Adjusted R = 0.1603

#test for outliers and influential observations
r = rstudent(model2.stepwise)
outliers = r[abs(r)>3]
length(outliers) #10 outliers; ok relative to the size of the data
cook = cooks.distance(model2.stepwise)
influential = cook[cook>10]
length(influential) #0 influential observations

#3#
model3 = lm(targamnt_log ~ recmon 
            + salcls1_log + salcls2_log + salcls4_log + salcls5_log + salcls6_log + salcls7_log
            # + ordcls1 + ordcls2 + ordcls3 + ordcls4 + ordcls5 + ordcls6 + ordcls7
            + prefer1 + prefer2 + prefer3 + prefer4 + prefer5 + prefer6 + prefer7
            # + breadth
            # + AOA_log + AOA1_log + AOA2_log + AOA3_log + AOA4_log + AOA5_log + AOA6_log + AOA7_log
            + ord185 + ord285 + ord385 + ord485 + tof + totord + totsale + PR,
            data = train2)
summary(model3)

model3.stepwise = step(object = model3, direction = "both")
summary(model3.stepwise) #Adjusted R = 0.1607

#test for multicollinearity
vif(model3.stepwise) #ok

#test for outliers and influential observations
r = rstudent(model3.stepwise)
outliers = r[abs(r)>3]
length(outliers) #10 outliers; ok relative to the size of the data
cook = cooks.distance(model3.stepwise)
influential = cook[cook>10]
length(influential) #0 influential observations

#4# model1.stepwise plus interaction terms
model4 = lm(targamnt_log ~ ordcls1 + ordcls3 + ordcls7
            + AOA_log + AOA2_log + AOA3_log + AOA5_log + AOA6_log + AOA7_log
            + ord185 + ord285 + ord485 + tof + totord + totsale + PR
            + AOA_log:tof + AOA2_log:tof + AOA3_log:tof + AOA5_log:tof + AOA6_log:tof + AOA7_log:tof,
            data = train2)
summary(model4)

model4.stepwise = step(object = model4, direction = "both")
summary(model4.stepwise) #Adjusted R = 0.1928

#test for multicollinearity
vif(model4.stepwise) #vif of tof, AOA_log:tof >>10

#5# model1.stepwise plus interaction terms
model5 = lm(targamnt_log ~ ordcls1 + ordcls3 + ordcls7
            + AOA_log + AOA2_log + AOA3_log + AOA5_log + AOA6_log + AOA7_log
            + ord185 + ord285 + ord485 + tof + totord + totsale + PR
            + AOA_log:PR + AOA2_log:PR + AOA3_log:PR + AOA5_log:PR + AOA6_log:PR + AOA7_log:PR,
            data = train2)
summary(model5)

model5.stepwise = step(object = model5, direction = "both")
summary(model5.stepwise) #Adjusted R = 0.1974

#test for multicollinearity
vif(model5.stepwise) #ok

#test for outliers and influential observations
r = rstudent(model5.stepwise)
outliers = r[abs(r)>3]
length(outliers) #14 outliers; ok relative to the size of the data
cook = cooks.distance(model5.stepwise)
influential = cook[cook>10]
length(influential) #0 influential observations

##Model Diagnostics
plot(model1.stepwise, which = 1) #ok
plot(model1.stepwise, which = 2) #ok

##testing
logistic_model = glm(targamnt_ind ~ ord185 + ord285 + ord385 + ord485 + PR_ind, data = train_clean, family = binomial)
test_clean$probs = predict(logistic_model, newdata = test_clean, type = "response")
test_clean$pred_targamnt_ind = ifelse(test_clean$probs > 0.2, 1, 0)

test2 = test_clean[test_clean$pred_targamnt_ind == 1,]
test2$PR2 = test2$totsale/test2$tof
test2$pred_targamnt = predict(model1.stepwise, newdata = test2)

test3 = merge(test_clean[,c("custno", "targamnt", "targamnt_log", "probs", "pred_targamnt_ind")], 
              test2[,c("custno", "pred_targamnt")], by = "custno", all.x = TRUE)
test3[is.na(test3)] = 0
error = sum((exp(test3$pred_targamnt) - test3$targamnt)^2)/nrow(test3)

test3$expected_sale = test3$probs * test3$pred_targamnt
test3_ord = test3[order(test3$expected_sale, decreasing = T),]
top_customer = test3_ord[1:1000,]
sum(top_customer$targamnt)

a = top_customer[top_customer$targamnt==0,]
b = top_customer[top_customer$targamnt>0,]

test_ordered = test[order(test$targamnt, decreasing = T),]
true_top_customer = test_ordered[1:1000,]
sum(true_top_customer$targamnt)
sum(top_customer$targamnt)/sum(true_top_customer$targamnt)
length(which(top_customer$custno %in% true_top_customer$custno))
