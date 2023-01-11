library('readxl')
library(jtools)
library(MASS)
library(psych)
library(car)
library(reshape2)
library("BBmisc")
library(vcd)
library(caret)

glm.control(epsilon = 1e-8, maxit = 1e10, trace = FALSE)
#dataset of products
data_product <- read_excel("/Users/tyrusyuen/Library/CloudStorage/OneDrive-TheChineseUniversityofHongKong/2022 H2/UoT Datathon/UoT Datathon 2022 Shared Drive/Tyrus_working files/discount by product ID.xlsx",skip=2)

final_Manufacuturer = c(1046, 693, 857, 1071,1208)

get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}
calc_nrmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))/(max(actual)-min(actual))
}


df1=data.frame()

for (i in 1:length(final_Manufacuturer)) {
  
  #for (i in 1:3) { 
  sum_QUANTITY_manufacturer=data_product$sum_QUANTITY[data_product$MANUFACTURER==final_Manufacuturer[i]]
  sum_COUPON_MATCH_DISC_manufacturer=-1*data_product$sum_COUPON_MATCH_DISC[data_product$MANUFACTURER==final_Manufacuturer[i]]
  sum_RETAIL_DISC_manufacturer=-1*data_product$sum_RETAIL_DISC[data_product$MANUFACTURER==final_Manufacuturer[i]]
  count_STORE_ID_manufacturer=data_product$count_STORE_ID[data_product$MANUFACTURER==final_Manufacuturer[i]]
  data_final=data.frame(sum_QUANTITY_manufacturer,sum_COUPON_MATCH_DISC_manufacturer,sum_RETAIL_DISC_manufacturer,count_STORE_ID_manufacturer)
  
  std_sum_QUANTITY_manufacturer=(sum_QUANTITY_manufacturer-mean(sum_QUANTITY_manufacturer))/sd(sum_QUANTITY_manufacturer)
  std_sum_COUPON_MATCH_DISC_manufacturer=(sum_COUPON_MATCH_DISC_manufacturer-mean(sum_COUPON_MATCH_DISC_manufacturer))/sd(sum_COUPON_MATCH_DISC_manufacturer)
  std_sum_RETAIL_DISC_manufacturer=(sum_RETAIL_DISC_manufacturer-mean(sum_RETAIL_DISC_manufacturer))/sd(sum_RETAIL_DISC_manufacturer)
  std_count_STORE_ID_manufacturer=(count_STORE_ID_manufacturer-mean(count_STORE_ID_manufacturer))/sd(count_STORE_ID_manufacturer)
  data_final=data.frame(sum_QUANTITY_manufacturer,std_sum_COUPON_MATCH_DISC_manufacturer,std_sum_RETAIL_DISC_manufacturer,std_count_STORE_ID_manufacturer)
  
  print(c(final_Manufacuturer[i],dim(data_final)))
  
  #Specify the parameters for cv <caret>
  train_control <- trainControl(method = "cv", number=10) 
  
  #Fit the model with glm.nb <MASS>
  m1 <- (try(glm.nb(sum_QUANTITY_manufacturer ~ sum_COUPON_MATCH_DISC_manufacturer 
                    +sum_RETAIL_DISC_manufacturer   #Default log-link
                    +count_STORE_ID_manufacturer, data = data_final)))
  #Train the model with cv and output the best model <caret>
  model <- train(sum_QUANTITY_manufacturer ~., data = data_final,  
                 method = "glm.nb", 
                 trControl = train_control)
  summary(m1)
  print(model$finalModel)
  
  set.seed(1155077960)
  default_idx = createDataPartition(data_final$sum_QUANTITY_manufacturer, p = 0.75, list = FALSE)
  default_trn = data_final[default_idx, ]
  default_tst = data_final[-default_idx, ]
  
  print(get_best_result(model))
  
  print(head(predict(model, newdata = default_tst)))
  print(calc_nrmse(actual = default_tst$sum_QUANTITY_manufacturer,
           predicted = predict(model, newdata = default_tst)))
  print(summary(model$finalModel))
}

#1. re-run the best model for each manufacturer
#2. output the results for the 4 manufacturers