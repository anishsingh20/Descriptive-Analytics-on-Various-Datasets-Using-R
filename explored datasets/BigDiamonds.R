#Loading the big diamonds dataset
setwd('C:/Users/hp/Documents/RStudio/DataSets/')

install.packages('RCurl')

diamondsBigSample <- load("BigDiamonds.rda")

#filtering data sets-considering diamonds omly price under 10,000
library(dplyr)

#new data frame with only diamonds of price less than 10000
diam_new<-filter(diamondsbig, price<=10000,cert == 'GIA')

#Barplot of clarity - x -axes has always discrete/categorical var
#fill arg to fill by cut of diamonds
ggplot(aes(x = clarity),data= na.omit(diam_new)) + 
  geom_bar(aes(fill=cut))  + 
  scale_fill_brewer(guide=guide_legend('Cut of Diamonds',reverse =T))



ggplot(aes(x = cert),data= na.omit(diamondsbig)) + 
  geom_bar(aes(fill=color))  + 
  scale_fill_brewer(guide=guide_legend('Cut of Diamonds',reverse =T)) +
  scale_y_sqrt()




  

#checking the prop of diamonds by clarity and cut
prop.table(table(diamondsbig$clarity,diamondsbig$cut))


#modelling-lm()

m1<-lm(I(log(price)) ~ I(carat^(1/3)),data = diam_new)
summary(m1)
#adding more predictor var to our new models
m2<-update(m1 ,~ . + cut)
m3<-update(m2 , ~ . + carat)
m4<-update(m3, ~ . + clarity)
m5<-update(m4,~ . +  color)
#we can use R-sq to see the predicting power and the AIC and deviance values to check the 
#goodness of fit and quality of the model
#Also the Residula std error==0.067 which means the sqrt(RSS/df) is mimimum of this

#R^2=0.9685 for m5 which means that 96% of variation in diamonds price is due to all the
# predictor var included in the mode 5(m5) 
#hence our main aim of doing multiple reg is to find out the variation in the price due to
# the more predictor var or regressors.

#hence we conclude that carat,color,clarity,cut all collectively have more 
#effect on the variation in price of diamonds.


suppressMessages(library(lattice))
suppressMessages(library(MASS))
suppressMessages(library(memisc))
models <- mtable(m1, m2, m3, m4, m5)


#making predictions

#test_set to test accuracy of the model against random values
test_set<-data.frame(carat=1.00 , cut='V.Good', color='I',clarity='VS1')

model_estimate= predict(m5 , newdata = test_set , interval = 'prediction', level = 0.95)
exp(model_estimate)



dat = data.frame(m4$model, exp(m4$residuals) )

with(dat, sd(m4.residuals)) 

with(subset(dat, carat > .9 & carat < 1.1), sd(m4.residuals)) 

dat$resid <- as.numeric(dat$m4.residuals)
ggplot(aes(y = resid, x = round(carat, 2)), data = dat) + 
  geom_line(stat = "summary", fun.y = sd) 



?diamonds

par(mfrow=c(2,2))

plot(m5)


#aggregation function to perform summarization of data and transforming it,moving to high levels
aggregate( price~cut,diam_new,mean)
#similar to by() and summarize() for group wise aggregation