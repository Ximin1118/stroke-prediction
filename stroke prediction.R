library(ggplot2)
library(dplyr)
library(Hmisc)
library(corrplot)
load("brfss2013.RData")
ggplot(aes(x=physhlth, fill=sex), data = brfss2013[!is.na(brfss2013$sex), ]) +
  geom_histogram(bins=30, position = position_dodge()) + ggtitle('Number of Days Physical Health not Good in the Past 30 Days')

ggplot(aes(x=menthlth, fill=sex), data=brfss2013[!is.na(brfss2013$sex), ]) +
  geom_histogram(bins=30, position = position_dodge()) + ggtitle('Number of Days Mental Health not Good in the Past 30 Days')

ggplot(aes(x=poorhlth, fill=sex), data=brfss2013[!is.na(brfss2013$sex), ]) +
  geom_histogram(bins=30, position = position_dodge()) + ggtitle('Number of Days with Poor Physical Or Mental Health in the Past 30 Days')

summary(brfss2013$sex)
##  Male  Female   NA's 
##201313 290455      7 

by_month <- brfss2013 %>% filter(iyear=='2013') %>% group_by(imonth, genhlth) %>% summarise(n=n())
ggplot(aes(x=imonth, y=n, fill = genhlth), data = by_month[!is.na(by_month$genhlth), ]) + geom_bar(stat = 'identity', position = position_dodge()) + ggtitle('Health Perception By Month')

by_month1 <- brfss2013 %>% filter(iyear=='2013') %>% group_by(imonth) %>% summarise(n=n())
ggplot(aes(x=imonth, y=n), data=by_month1) + geom_bar(stat = 'identity') + ggtitle('Number of Respondents by Month')


plot(brfss2013$income2, brfss2013$hlthpln1, xlab = 'Income Level', ylab = 'Health Care Coverage', main ='Income Level versus Health Care Coverage')


vars <- names(brfss2013) %in% c('smoke100', 'avedrnk2', 'bphigh4', 'toldhi2', 'weight2')
selected_brfss <- brfss2013[vars]
selected_brfss$toldhi2 <- ifelse(selected_brfss$toldhi2=="Yes", 1, 0)
selected_brfss$smoke100 <- ifelse(selected_brfss$smoke100=="Yes", 1, 0)
selected_brfss$weight2 <- as.numeric(selected_brfss$weight2)
selected_brfss$bphigh4 <- as.factor(ifelse(selected_brfss$bphigh4=="Yes", "Yes", (ifelse(selected_brfss$bphigh4=="Yes, but female told only during pregnancy", "Yes", (ifelse(selected_brfss$bphigh4=="Told borderline or pre-hypertensive", "Yes", "No"))))))
selected_brfss$bphigh4 <- ifelse(selected_brfss$bphigh4=="Yes", 1, 0)
selected_brfss <- na.delete(selected_brfss)
corr.matrix <- cor(selected_brfss)
corrplot(corr.matrix, main="\n\nCorrelation Plot of Smoke, Alcohol, Blood pressure, Cholesterol, and Weight", method="number")


vars1 <- names(brfss2013) %in% c('smoke100', 'avedrnk2', 'bphigh4', 'toldhi2', 'weight2', 'cvdstrk3')
stroke <- brfss2013[vars1]
stroke$bphigh4 <- as.factor(ifelse(stroke$bphigh4=="Yes", "Yes", (ifelse(stroke$bphigh4=="Yes, but female told only during pregnancy", "Yes", (ifelse(stroke$bphigh4=="Told borderline or pre-hypertensive", "Yes", "No"))))))
stroke$weight2<-as.numeric(stroke$weight2)

stroke$bphigh4 <- replace(stroke$bphigh4, which(is.na(stroke$bphigh4)), "No")
stroke$toldhi2 <- replace(stroke$toldhi2, which(is.na(stroke$toldhi2)), "No")
stroke$cvdstrk3 <- replace(stroke$cvdstrk3, which(is.na(stroke$cvdstrk3)), "No")
stroke$smoke100 <- replace(stroke$smoke100, which(is.na(stroke$smoke100)), 'No')

mean(stroke$avedrnk2,na.rm = T)
##[1] 2.209905

stroke$avedrnk2 <- replace(stroke$avedrnk2, which(is.na(stroke$avedrnk2)), 2)

head(stroke)
summary(stroke)
##   bphigh4 toldhi2 cvdstrk3 weight2 smoke100 avedrnk2
##1     Yes     Yes       No     154      Yes        2
##2      No      No       No      30       No        2
##3      No      No       No      63      Yes        4
##4      No     Yes       No      31       No        2
##5     Yes      No       No     169      Yes        2
##6     Yes     Yes       No     128       No        2

##  bphigh4      toldhi2      cvdstrk3        weight2       smoke100    
## No :284107   Yes:183501   Yes: 20391   Min.   :  1.00   Yes:215201  
## Yes:207668   No :308274   No :471384   1st Qu.: 43.00   No :276574  
##                                        Median : 73.00               
##                                        Mean   : 80.22               
##                                        3rd Qu.:103.00               
##                                        Max.   :570.00               
##    avedrnk2     
## Min.   : 1.000  
## 1st Qu.: 2.000  
## Median : 2.000  
## Mean   : 2.099  
## 3rd Qu.: 2.000  
## Max.   :76.000  


train <- stroke[1:390000,]
test <- stroke[390001:491775,]
model <- glm(cvdstrk3 ~.,family=binomial(link = 'logit'),data=train)
summary(model)
##Call:
##glm(formula = cvdstrk3 ~ ., family = binomial(link = "logit"), 
##    data = train)

##Deviance Residuals: 
##    Min       1Q   Median       3Q      Max  
##-0.5057  -0.3672  -0.2109  -0.1630   3.2363  

##Coefficients:
##              Estimate Std. Error  z value Pr(>|z|)    
##(Intercept) -3.2690106  0.0268240 -121.869  < 2e-16 ***
##bphigh4Yes   1.3051850  0.0193447   67.470  < 2e-16 ***
##toldhi2No   -0.5678048  0.0171500  -33.108  < 2e-16 ***
##weight2     -0.0009628  0.0001487   -6.476 9.41e-11 ***
##smoke100No  -0.3990598  0.0163896  -24.348  < 2e-16 ***
##avedrnk2    -0.0274511  0.0065099   -4.217 2.48e-05 ***
##---
##Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

##(Dispersion parameter for binomial family taken to be 1)

##    Null deviance: 136364  on 389999  degrees of freedom
##Residual deviance: 126648  on 389994  degrees of freedom
##AIC: 126660

##Number of Fisher Scoring iterations: 6


anova(model, test="Chisq")
##Analysis of Deviance Table

##Model: binomial, link: logit

##Response: cvdstrk3

##Terms added sequentially (first to last)


##         Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
##NULL                    389999     136364              
##bphigh4   1   7848.6    389998     128516 < 2.2e-16 ***
##toldhi2   1   1230.1    389997     127285 < 2.2e-16 ***
##weight2   1     33.2    389996     127252 8.453e-09 ***
##smoke100  1    584.5    389995     126668 < 2.2e-16 ***
##avedrnk2  1     19.9    389994     126648 7.958e-06 ***
##---
##Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$cvdstrk3)
print(paste('Accuracy',1-misClasificError))
##[1] "Accuracy 0.961296978629329


library(ROCR)
p <- predict(model, newdata=test, type="response")
pr <- prediction(p, test$cvdstrk3)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
##[1] 0.7226642
