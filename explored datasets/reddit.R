#Reddit conducted Survey asking people 
#some personal question to them about gender,employement,children,reletionship status etc
reddit<-read.csv('DataSets/reddit.csv')
str(reddit)
##income.range column of the data set is an example of Ordered factor
#while others are mostly No /YES , 0/1 ie which are unordered factors or Binary Factors

library(ggplot2)

qplot(data = reddit , x = age.range)
##creating ordered factor
ord_age<-ordered(reddit$age.range , levels= c('Under 18','18-24', '25-34','35-44', '45-54 ','55-64', "65 or Above "))
#Ordered plot for ages vs count
qplot(data = reddit , x = ord_age)

#Clean Data
reddit<-na.omit(reddit) ##na.omit to omit NA values

levels(reddit$income.range)
#Ordering the income.range in ascending order
reddit$income.range<-ordered(reddit$income.range , levels = c("Under $20,000" ,"$20,000 - $29,999","$30,000 - $39,999", "$40,000 - $49,999" , "$50,000 - $69,999" ,"$70,000 - $99,999", "$100,000 - $149,999","$150,000 or more"))
#Ordered Historgram plot
qplot(data=reddit, x = reddit$income.range)

cor.test(unclass(reddit$children) , unclass(reddit$military.service))

#Reports for Proportions who participated in the survey
prop.table(table(reddit$military.service))

prop.table(table(reddit$marital.status))


prop.table(table(reddit$gender))


prop.table(table(reddit$employment.status))

#ifelse used to subset data 
reddit$gender<-ifelse(reddit$gender==0,'Male','Female')

reddit$gender<-as.factor(reddit$gender)

levels(reddit$gender)
##Ordering the Factor first-Male then Female(default=Alphabatical order) 
ordered(reddit$gender,levels=c('Male','Female'))

#Imp to explicitly give data and x args
qplot(data = reddit,x = gender)






