## Read in the data:
train = read.csv("train-1.csv", header = T) # 52,844 transactions
test = read.csv("test.csv", header = T)

## Data Cleaning
# remove negative values
train = subset(train, targamnt>=0 & totsale>=0 & salcls1>=0 & salcls2>=0 & 
                 salcls3>=0 & salcls4>=0 & salcls5>=0 & 
                 salcls6>=0 & salcls7>=0)
test = subset(test, targamnt>=0 & totsale>=0 & salcls1>=0 & salcls2>=0 & 
                salcls3>=0 & salcls4>=0 & salcls5>=0 & 
                salcls6>=0 & salcls7>=0)
# subset = function(dataset){
#   dataset = dataset[dataset$totsale>=0 & dataset$salcls1>=0 & dataset$salcls2>=0 & 
#                      dataset$salcls3>=0 & dataset$salcls4>=0 & dataset$salcls5>=0 & 
#                       dataset$salcls6>=0 & dataset$salcls7>=0,]
#   return(dataset)
# }
# 
# train2 = subset(train)
# test2 = subset(test)

# feature creation
feature = function(dataset){
  #whether buy indicator
  dataset$targamnt_ind = ifelse(dataset$targamnt > 0, 1, 0)
  
  #Average Order Amount
  dataset$AOA = dataset$totsale/dataset$totord
  dataset$AOA1 = dataset$salcls1/dataset$ordcls1
  dataset$AOA2 = dataset$salcls2/dataset$ordcls2
  dataset$AOA3 = dataset$salcls3/dataset$ordcls3
  dataset$AOA4 = dataset$salcls4/dataset$ordcls4
  dataset$AOA5 = dataset$salcls5/dataset$ordcls5
  dataset$AOA6 = dataset$salcls6/dataset$ordcls6
  dataset$AOA7 = dataset$salcls7/dataset$ordcls7
  
  #Purchase Rate
  dataset$PR = dataset$totord/dataset$tof
  dataset$PR_ind = ifelse(dataset$PR > 0.1, 1, 0)
  
  #Breadth
  dataset$breadth <- rowSums(dataset[,4:10])
#   index <- vector()
#   for(i in 1:nrow(dataset))
#   {
#     index[i] = 0
#     if(dataset$ordcls1[i] > 0)
#     {index[i] <- index[i] + 1}
#     if(dataset$ordcls2[i] > 0)
#     {index[i] <- index[i] + 1}
#     if(dataset$ordcls3[i] > 0)
#     {index[i] <- index[i] + 1}
#     if(dataset$ordcls4[i] > 0)
#     {index[i] <- index[i] + 1}
#     if(dataset$ordcls5[i] > 0)
#     {index[i] <- index[i] + 1}
#     if(dataset$ordcls6[i] > 0)
#     {index[i] <- index[i] + 1}
#     if(dataset$ordcls7[i] >  0)
#     {index[i] <- index[i] + 1}
#   }
#   dataset$breadth <- index
  
  #Preferred Product
  dataset$prefer1 <- ifelse(rowSums(dataset[,4:10]) > 0, ifelse(dataset$ordcls1 == apply(dataset[,4:10],1,max), 1, 0), 0)
  dataset$preder2 <- ifelse(rowSums(dataset[,4:10]) > 0, ifelse(dataset$ordcls2 == apply(dataset[,4:10],1,max), 1, 0), 0)
  dataset$preder3 <- ifelse(rowSums(dataset[,4:10]) > 0, ifelse(dataset$ordcls3 == apply(dataset[,4:10],1,max), 1, 0), 0)
  dataset$preder4 <- ifelse(rowSums(dataset[,4:10]) > 0, ifelse(dataset$ordcls4 == apply(dataset[,4:10],1,max), 1, 0), 0)
  dataset$prefer5 <- ifelse(rowSums(dataset[,4:10]) > 0, ifelse(dataset$ordcls5 == apply(dataset[,4:10],1,max), 1, 0), 0)
  dataset$prefer6 <- ifelse(rowSums(dataset[,4:10]) > 0, ifelse(dataset$ordcls6 == apply(dataset[,4:10],1,max), 1, 0), 0)
  dataset$prefer7 <- ifelse(rowSums(dataset[,4:10]) > 0, ifelse(dataset$ordcls7 == apply(dataset[,4:10],1,max), 1, 0), 0)

#   prefer1 <- vector()
#   prefer2 <- vector()
#   prefer3 <- vector()
#   prefer4 <- vector()
#   prefer5 <- vector()
#   prefer6 <- vector()
#   prefer7 <- vector()
#   
#   for(i in 1:nrow(dataset))
#   {
#     prefer1[i] = prefer2[i] = prefer3[i] = prefer4[i] = prefer5[i] = prefer6[i] = prefer7[i] = 0
#     
#     if(dataset$ordcls1[i] != 0 || dataset$ordcls2[i] != 0 || dataset$ordcls3[i] !=0 ||
#        dataset$ordcls4[i] != 0 || dataset$ordcls5[i] !=0 || dataset$ordcls6[i] != 0 ||
#        dataset$ordcls7[i] != 0)
#     {
#       if(dataset$ordcls1[i] >= max(dataset$ordcls2[i], dataset$ordcls3[i], dataset$ordcls4[i], dataset$ordcls5[i], dataset$ordcls6[i], dataset$ordcls7[i]))
#       {prefer1[i] = 1}
#       if(dataset$ordcls2[i] >= max(dataset$ordcls1[i], dataset$ordcls3[i], dataset$ordcls4[i], dataset$ordcls5[i], dataset$ordcls6[i], dataset$ordcls7[i]))
#       {prefer2[i] = 1}
#       if(dataset$ordcls3[i] >= max(dataset$ordcls1[i], dataset$ordcls2[i], dataset$ordcls4[i], dataset$ordcls5[i], dataset$ordcls6[i], dataset$ordcls7[i]))
#       {prefer3[i] = 1}
#       if(dataset$ordcls4[i] >= max(dataset$ordcls1[i], dataset$ordcls2[i], dataset$ordcls3[i], dataset$ordcls5[i], dataset$ordcls6[i], dataset$ordcls7[i]))
#       {prefer4[i] = 1}
#       if(dataset$ordcls5[i] >= max(dataset$ordcls1[i], dataset$ordcls2[i], dataset$ordcls3[i], dataset$ordcls4[i], dataset$ordcls6[i], dataset$ordcls7[i]))
#       {prefer5[i] = 1}
#       if(dataset$ordcls6[i] >= max(dataset$ordcls1[i], dataset$ordcls2[i], dataset$ordcls3[i], dataset$ordcls4[i], dataset$ordcls5[i], dataset$ordcls7[i]))
#       {prefer6[i] = 1}
#       if(dataset$ordcls7[i] >= max(dataset$ordcls1[i], dataset$ordcls2[i], dataset$ordcls3[i], dataset$ordcls4[i], dataset$ordcls5[i], dataset$ordcls6[i]))
#       {prefer7[i] = 1}}
#   }
#   
#   dataset$prefer1 <- prefer1
#   dataset$prefer2 <- prefer2
#   dataset$prefer3 <- prefer3
#   dataset$prefer4 <- prefer4
#   dataset$prefer5 <- prefer5
#   dataset$prefer6 <- prefer6
#   dataset$prefer7 <- prefer7
  
  ## Log transformation
  dataset$targamnt_log = log(dataset$targamnt + 1)
  dataset$totsale_log = log(dataset$totsale + 1)
  dataset$AOA_log = log(dataset$AOA + 1)
  dataset$AOA1_log = log(dataset$AOA1 + 1)
  dataset$AOA2_log = log(dataset$AOA2 + 1)
  dataset$AOA3_log = log(dataset$AOA3 + 1)
  dataset$AOA4_log = log(dataset$AOA4 + 1)
  dataset$AOA5_log = log(dataset$AOA5 + 1)
  dataset$AOA6_log = log(dataset$AOA6 + 1)
  dataset$AOA7_log = log(dataset$AOA7 + 1)
  
  dataset$salcls1_log = log(dataset$salcls1 + 1)
  dataset$salcls2_log = log(dataset$salcls2 + 1)
  dataset$salcls3_log = log(dataset$salcls3 + 1)
  dataset$salcls4_log = log(dataset$salcls4 + 1)
  dataset$salcls5_log = log(dataset$salcls5 + 1)
  dataset$salcls6_log = log(dataset$salcls6 + 1)
  dataset$salcls7_log = log(dataset$salcls7 + 1)
  
  ##Replace NA with 0
  dataset[is.na(dataset)] = 0
  
  return(dataset)
}

train_clean = feature(train)
test_clean = feature(test)

write.csv(train_clean, "train_clean.csv")
write.csv(test_clean, "test_clean.csv")
