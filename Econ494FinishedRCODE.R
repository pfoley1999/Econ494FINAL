df <- Business.Analytics.Data...Sheet1
df$isDoc <- as.integer(df$Ownership.Model == "Doc")
View(df)
summary(df)

cor(df[2],df[3])
cor(df[2],df[4])
cor(df[2],df[6])
cor(df[2],df[7])
cor(df[2],df[9])

cov(df[2],df[3])
cov(df[2],df[4])
cov(df[2],df[6])
cov(df[2],df[7])
cov(df[2],df[9])

#Plot1
library(ggplot2)
plot_b <- ggplot(df, aes(Total.Income)) +
   geom_boxplot() +
   facet_wrap(~isDoc, dir = "v")
   options(scipen = 5)
theme_update(plot.title = element_text(hjust = 0.5))
plot_b + ggtitle("Income by Ownership") +
   xlab("Total Income($)")

#Plot2
plot_a <- ggplot(df, aes(Total.Income)) +
   geom_histogram(aes(y=..density..), position = "identity", binwidth = 15000) +
   geom_density()
plot_a
theme_update(plot.title = element_text(hjust = 0.5))
plot_a + ggtitle("Histogram of Total Income") +
   xlab("Total Income($)") + ylab("Density")

#Plot3
install.packages('sm')
library(sm)
options(scipen=5)
sm.density.compare(df$Total.Income, df$Ownership.Model,main=" ", xlab=" ", ylab=" ") 
legend("topright", legend=c("Owned by Doctor", "Owned by Investor", "Owned by Company"), 
       col=c("red", "green","blue"), lty=1:2, cex=0.8,
       box.lty=2, box.lwd=2, box.col="black")
title(main = "Total Income Density", 
      xlab = "Total Income", ylab = "Density")

#Plot4
plot_c <- ggplot(df, aes(Chiropractic.Visits, Total.Income)) +
   geom_point(aes(col = factor(Ownership.Model))) +
   scale_color_discrete("Ownership Model")
plot_c
theme_update(plot.title = element_text(hjust = 0.5))
plot_c + ggtitle("Total Income vs. Chiropractic Visits") +
   xlab("Chiropractic Visits") + ylab("Total Income($)")

#Plot5
plot(df$Chiropractic.Visits~df$Patient.Referrals, main=" ", xlab=" ", ylab=" ", xlim=c(0,80))
abline(lm(df$Chiropractic.Visits~df$Patient.Referrals), col="red")
title(main = "Chiropractic Visits vs. Patient Referrals", 
      xlab = "Patient Referrals", ylab = "Chiropractic Visits")    


#--------------------------------------------------------------------------------------------
#Part 2 Tom Hollerbach's R code
#--------------------------------------------------------------------------------------------

library(plyr) #for ddply()
library(tseries) #for the J-B test

MODEL1<-lm(Total.Income ~ Chiropractic.Visits, df)
summary(MODEL1)

hist(MODEL1$residuals)
jarque.bera.test(MODEL1$residuals)

p<-.7 
obs_count<-dim(df)[1]
training_size <- floor(p * obs_count)
set.seed(1234)
train_ind <- sample(obs_count, size = training_size)
Training <- df[train_ind, ]
Testing <- df[-train_ind, ]

M1 <- lm(Total.Income ~ Chiropractic.Visits, Training)
summary(M1)

PRED_1_IN <- predict(M1, Training) 
View(PRED_1_IN)
View(M1$fitted.values) 
PRED_1_OUT <- predict(M1, Testing)
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$Total.Income)^2)/length(PRED_1_IN))  
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$Total.Income)^2)/length(PRED_1_OUT)) 
RMSE_1_IN
RMSE_1_OUT

M2 <- lm(Total.Income ~ Chiropractic.Visits + New.Patient.Visits + Year.Opened, Training)
summary(M2)

hist(M2$residuals)
jarque.bera.test(M2$residuals)

PRED_2_IN <- predict(M2, Training)
View(PRED_2_IN)
View(M2$fitted.values) 
PRED_2_OUT <- predict(M2, Testing) 
RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$Total.Income)^2)/length(PRED_2_IN))  
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$Total.Income)^2)/length(PRED_2_OUT)) 
RMSE_2_IN
RMSE_2_OUT

#-----------------------------------------------------------------------------------------
#Patrick Foley's R Code
#-----------------------------------------------------------------------------------------
library(plyr)
library(tseries)


MODEL3<-lm(Total.Income ~ Population..City., df)
summary(MODEL3)

plot(Total.Income ~ Population..City., df, xlim=c(0,1000000), ylim=c(0,1000000)) 
plot(MODEL3$residuals)
jarque.bera.test(MODEL3$residuals)

p<-.7 
obs_count<-dim(df)[1]
training_size <- floor(p * obs_count)
set.seed(1234)
train_ind <- sample(obs_count, size = training_size)
Training <- df[train_ind, ]
Testing <- df[-train_ind, ]

M3 <- lm(Total.Income ~ Population..City., Training)
summary(M3)

PRED_1_IN <- predict(M3, Training) 
View(PRED_1_IN)
View(M3$fitted.values) 

PRED_1_OUT <- predict(M3, Testing)
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$Total.Income)^2)/length(PRED_1_IN)) 
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$Total.Income)^2)/length(PRED_1_OUT))
RMSE_1_IN
RMSE_1_OUT

library(plyr)
library(tseries) 

M4 <- lm(Total.Income ~ Population..City. + Chiropractic.Visits + Ownership.Model, Training)
summary(M4)

PRED_2_IN <- predict(M4, Training)
View(PRED_2_IN)
View(M4$fitted.values) 

plot(M4$residuals)
jarque.bera.test(M4$residuals)

#----------------------------------------------------------------------------------------------
#Ty Helfrich's RCode
#----------------------------------------------------------------------------------------------

library(plyr) #for ddply()
library(tseries) #for the J-B test

MODEL5<-lm(Total.Income ~ Patient.Referrals, df)
summary(MODEL5)

line(MODEL5$residuals)
jarque.bera.test(MODEL5$residuals)

p<-.7 
obs_count<-dim(df)[1]
training_size <- floor(p * obs_count)
set.seed(1234)
train_ind <- sample(obs_count, size = training_size)
Training <- df[train_ind, ]
Testing <- df[-train_ind, ]

M5 <- lm(Total.Income ~ Patient.Referrals, Training)
summary(M5)

PRED_5_IN <- predict(M5, Training) #generate predictions on the (in-sample) training data
View(PRED_5_IN)
View(M5$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_5_OUT <- predict(M5, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_5_IN<-sqrt(sum((PRED_5_IN-Training$Total.Income)^2)/length(PRED_5_IN))  #computes in-sample error
RMSE_5_OUT<-sqrt(sum((PRED_5_OUT-Testing$Total.Income)^2)/length(PRED_5_OUT)) #computes out-of-sample 

RMSE_5_IN
RMSE_5_OUT


M6 <- lm(Total.Income ~ Patient.Referrals + New.Patient.Visits + City.Population, Training)
summary(M6)

PRED_6_IN <- predict(M6, Training) #generate predictions on the (in-sample) training data
View(PRED_6_IN)
View(M6$fitted.values) #these are the same as the fitted values

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING
PRED_6_OUT <- predict(M6, Testing) #generate predictions on the (out-of-sample) testing data

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_6_IN<-sqrt(sum((PRED_6_IN-Training$Total.Income)^2)/length(PRED_6_IN))  #computes in-sample error
RMSE_6_OUT<-sqrt(sum((PRED_6_OUT-Testing$Total.Income)^2)/length(PRED_6_OUT)) #computes out-of-sample 

RMSE_6_IN
RMSE_6_OUT

line(M6$residuals)
jarque.bera.test(M6$residuals)
