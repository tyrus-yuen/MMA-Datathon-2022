#To deploy linear regression model, t-distribution & generalized linear model to the dataset of manufacturer promotion budget
#The results will reflect the effectiveness of the promotion budget by the manufacturer to determine which manufacturer has the incentive of investing the digital adverstisment of the supermarket

obtain the best tune parameters for each manufacturer


library('readxl')
library(jtools)
library(MASS)
library(psych)
library(car)
library(reshape2)
library("BBmisc")

glm.control(epsilon = 1e-8, maxit = 1e10, trace = FALSE)
#raw dataset of transactions
data_transactions<-read.csv("/Users/tyrusyuen/Library/CloudStorage/OneDrive-TheChineseUniversityofHongKong/2022 H2/UoT Datathon/UoT Datathon 2022 Shared Drive/transactions.csv")

#dataset of products
data_product <- read_excel("/Users/tyrusyuen/Library/CloudStorage/OneDrive-TheChineseUniversityofHongKong/2022 H2/UoT Datathon/UoT Datathon 2022 Shared Drive/Tyrus_working files/discount by product ID.xlsx",skip=2)

beera <- function(expr){
  tryCatch(expr,
           error = function(e){
             message("An error occurred:\n", e)
           },
           warning = function(w){
             message("A warning occured:\n", w)
           },
           finally = {
             message("Finally done!")
           })
}

df1=data.frame()
df2=data.frame()
train_control <- trainControl(method = "cv", number=10)
for (i in 1:length(unique(data_product[["MANUFACTURER"]]))) {
  
#for (i in 1:3) { 
  sum_QUANTITY_manufacturer=data_product$sum_QUANTITY[data_product$MANUFACTURER==unique(data_product[["MANUFACTURER"]])[i]]
  sum_COUPON_MATCH_DISC_manufacturer=-1*data_product$sum_COUPON_MATCH_DISC[data_product$MANUFACTURER==unique(data_product[["MANUFACTURER"]])[i]]
  sum_RETAIL_DISC_manufacturer=-1*data_product$sum_RETAIL_DISC[data_product$MANUFACTURER==unique(data_product[["MANUFACTURER"]])[i]]
  count_STORE_ID_manufacturer=data_product$count_STORE_ID[data_product$MANUFACTURER==unique(data_product[["MANUFACTURER"]])[i]]
  data1=data.frame(sum_QUANTITY_manufacturer,sum_COUPON_MATCH_DISC_manufacturer,sum_RETAIL_DISC_manufacturer,count_STORE_ID_manufacturer)
  print(unique(data_product[["MANUFACTURER"]])[i])
  
  std_sum_QUANTITY_manufacturer=(sum_QUANTITY_manufacturer-mean(sum_QUANTITY_manufacturer))/sd(sum_QUANTITY_manufacturer)
  std_sum_COUPON_MATCH_DISC_manufacturer=(sum_COUPON_MATCH_DISC_manufacturer-mean(sum_COUPON_MATCH_DISC_manufacturer))/sd(sum_COUPON_MATCH_DISC_manufacturer)
  std_sum_RETAIL_DISC_manufacturer=(sum_RETAIL_DISC_manufacturer-mean(sum_RETAIL_DISC_manufacturer))/sd(sum_RETAIL_DISC_manufacturer)
  std_count_STORE_ID_manufacturer=(count_STORE_ID_manufacturer-mean(count_STORE_ID_manufacturer))/sd(count_STORE_ID_manufacturer)
  data2=data.frame(sum_QUANTITY_manufacturer,std_sum_COUPON_MATCH_DISC_manufacturer,std_sum_RETAIL_DISC_manufacturer,std_count_STORE_ID_manufacturer)
  
  m1_T <- (try(train(sum_QUANTITY_manufacturer ~., data = data1,
                   method = "glm.nb", 
                   trControl = train_control)))
  m1 <- try(m1_T$finalModel)
  n1 <- (try(glm.nb(sum_QUANTITY_manufacturer ~ sum_RETAIL_DISC_manufacturer+count_STORE_ID_manufacturer, data = data1,link=m1_T$bestTune[,1])))
  o1 <- (try(lm(sum_QUANTITY_manufacturer ~ sum_COUPON_MATCH_DISC_manufacturer+sum_RETAIL_DISC_manufacturer+count_STORE_ID_manufacturer, data = data1)))
  p1 <- (try(glm(sum_QUANTITY_manufacturer ~ sum_COUPON_MATCH_DISC_manufacturer+sum_RETAIL_DISC_manufacturer+count_STORE_ID_manufacturer, family = "poisson", data = data1)))
  
  m2_T <- (try(train(sum_QUANTITY_manufacturer ~., data = data2,
                   method = "glm.nb", 
                   trControl = train_control)))
  m2 <- try(m2_T$finalModel)
  n2 <- (try(glm.nb(sum_QUANTITY_manufacturer ~ std_sum_RETAIL_DISC_manufacturer+std_count_STORE_ID_manufacturer, data = data2,link=m2_T$bestTune[,1])))
  o2 <- (try(lm(sum_QUANTITY_manufacturer ~ std_sum_COUPON_MATCH_DISC_manufacturer+std_sum_RETAIL_DISC_manufacturer+std_count_STORE_ID_manufacturer, data = data2)))
  p2 <- (try(glm(sum_QUANTITY_manufacturer ~ std_sum_COUPON_MATCH_DISC_manufacturer+std_sum_RETAIL_DISC_manufacturer+std_count_STORE_ID_manufacturer, family = "poisson", data = data2)))
  
  
  des_stat1<-psych::describe(sum_QUANTITY_manufacturer)
  des_stat2<-psych::describe(sum_QUANTITY_manufacturer)
  
  est1<-(try(as.data.frame(summary(m1)$coefficient)))
  est2 <- (try(as.numeric(as.matrix(est1)))) # flatten the data.frame
  names(est2) <- (try(as.vector(outer(rownames(est1), names(est1), paste, sep='.'))))
  t_test1<-(try(t(t.test(sum_QUANTITY_manufacturer)$conf.int[1:2])))
  
  est3<-(try(as.data.frame(summary(m2)$coefficient)))
  est4 <- (try(as.numeric(as.matrix(est3)))) # flatten the data.frame
  names(est4) <- (try(as.vector(outer(rownames(est3), names(est3), paste, sep='.'))))
  t_test2<-(try(t(t.test(sum_QUANTITY_manufacturer)$conf.int[1:2])))
  
  n1_L<-try(anova(m1, n1)[8][2,]) #Likelihood test to test general effect
  o1_M<-try(t(as.data.frame(vif(o1)))) #Multilinearity, by running VIF test on linear regression model
  assump1<-try(pchisq(2 * (logLik(m1) - logLik(p1)), df = 1, lower.tail = FALSE)[1])
  
  link1=try(m1_T$bestTune[,1])
  
  n2_L<-try(anova(m2, n2)[8][2,]) #Likelihood test to test general effect
  o2_M<-try(t(as.data.frame(vif(o2)))) #Multilinearity, by running VIF test on linear regression model
  assump2<-try(pchisq(2 * (logLik(m2) - logLik(p2)), df = 1, lower.tail = FALSE)[1])
  
  link2=try(m2_T$bestTune[,1])
  
  if (is.error(m1)) { 
    est2<-(as.data.frame(rep("NA", 16)))
    n1_L<-"NA"
    assump1<-"NA"
    link1<-"NA"
  } else if (length(m1$coefficients[is.na(m1$coefficients)])>0) { 
    est2<-(as.data.frame(rep("NA", 16)))
  } 
  
  if (is.error(n1)) {
    n1_L<-"NA"
  }
  if  (is.error(o1_M)) {
    o1_M<-t(as.data.frame(rep("NA", 3)))
  }
  if (is.error(p1)) {
    assump1<-"NA"
  }
  if (is.error(t_test1)) {
    t_test1<-t(as.data.frame(rep("NA", 2)))
  }
  
  if (is.error(m2)) { 
    est4<-(as.data.frame(rep("NA", 16)))
    n2_L<-"NA"
    assump2<-"NA"
    link2<-"NA"
  } else if (length(m2$coefficients[is.na(m2$coefficients)])>0) { 
    est4<-(as.data.frame(rep("NA", 16)))
  } 
  
  if (is.error(n2)) {
    n2_L<-"NA"
  }
  if  (is.error(o2_M)) {
    o2_M<-t(as.data.frame(rep("NA", 3)))
  }
  if (is.error(p2)) {
    assump2<-"NA"
  }
  if (is.error(t_test2)) {
    t_test2<-t(as.data.frame(rep("NA", 2)))
  }
  
  row_temp1=cbind(unique(data_product[["MANUFACTURER"]])[i],des_stat1$n,des_stat1$mean,des_stat1$sd,t_test1,t(est2), n1_L,  o1_M,assump1,link1)
  row_temp2=cbind(unique(data_product[["MANUFACTURER"]])[i],des_stat2$n,des_stat2$mean,des_stat2$sd,t_test2,t(est4), n2_L,  o2_M,assump2,link2)
  
  if (ncol(row_temp1)!=28){
    row_temp1=cbind(unique(data_product[["MANUFACTURER"]])[i],des_stat1$n,des_stat1$mean,des_stat1$sd,t_test1,t(as.data.frame(rep("NA", 18))),o1_M,"NA",link1)
  }
  
  if (ncol(row_temp2)!=28){
    row_temp2=cbind(unique(data_product[["MANUFACTURER"]])[i],des_stat2$n,des_stat2$mean,des_stat2$sd,t_test2,t(as.data.frame(rep("NA", 18))),o2_M,"NA",link2)
  }
  
  colnames(row_temp1) <- colnames(df1)
  df1=rbind(df1,as.data.frame(row_temp1))
  
  colnames(row_temp2) <- colnames(df2)
  df2=rbind(df2,as.data.frame(row_temp2))
}

est2_names<-c("(Intercept).Estimate", "sum_COUPON_MATCH_DISC_manufacturer.Estimate" , "sum_RETAIL_DISC_manufacturer.Estimate", "count_STORE_ID_manufacturer.Estimate" , "(Intercept).Std. Error","sum_COUPON_MATCH_DISC_manufacturer.Std. Error", "sum_RETAIL_DISC_manufacturer.Std. Error","count_STORE_ID_manufacturer.Std. Error" , "(Intercept).z value", "sum_COUPON_MATCH_DISC_manufacturer.z value", "sum_RETAIL_DISC_manufacturer.z value","count_STORE_ID_manufacturer.z value", "(Intercept).Pr(>|z|)" ,"sum_COUPON_MATCH_DISC_manufacturer.Pr(>|z|)","sum_RETAIL_DISC_manufacturer.Pr(>|z|)", "count_STORE_ID_manufacturer.Pr(>|z|)")
colnames(df1) <- c('Manufacturer','Product no. (Sample size)','Sample mean','Sample S.D.','t_test.2.5%','t_test.97.5%',est2_names,'Pr(chi)','VIF_COUPON_MATCH_DISC','VIF_RETAIL_DISC','VIF_STORE_ID','Pr(Chi).model_check',"Best link function")
write.csv(df1,"/Users/tyrusyuen/Library/CloudStorage/OneDrive-TheChineseUniversityofHongKong/2022 H2/UoT Datathon/GLM (NB best) Results (Product ID basis).csv", row.names = FALSE)

est4_names<-c("(Intercept).Estimate", "sum_COUPON_MATCH_DISC_manufacturer.Estimate" , "sum_RETAIL_DISC_manufacturer.Estimate", "count_STORE_ID_manufacturer.Estimate" , "(Intercept).Std. Error","sum_COUPON_MATCH_DISC_manufacturer.Std. Error", "sum_RETAIL_DISC_manufacturer.Std. Error","count_STORE_ID_manufacturer.Std. Error" , "(Intercept).z value", "sum_COUPON_MATCH_DISC_manufacturer.z value", "sum_RETAIL_DISC_manufacturer.z value","count_STORE_ID_manufacturer.z value", "(Intercept).Pr(>|z|)" ,"sum_COUPON_MATCH_DISC_manufacturer.Pr(>|z|)","sum_RETAIL_DISC_manufacturer.Pr(>|z|)", "count_STORE_ID_manufacturer.Pr(>|z|)")
colnames(df2) <- c('Manufacturer','Product no. (Sample size)','Sample mean','Sample S.D.','t_test.2.5%','t_test.97.5%',est4_names,'Pr(chi)','VIF_COUPON_MATCH_DISC','VIF_RETAIL_DISC','VIF_STORE_ID','Pr(Chi).model_check',"Best link function")
write.csv(df2,"/Users/tyrusyuen/Library/CloudStorage/OneDrive-TheChineseUniversityofHongKong/2022 H2/UoT Datathon/GLM (NB best) Results (std Product ID basis).csv", row.names = FALSE)

