#danone yogurt dataset

yo<-read.csv('yogurt.csv')
str(yo)

library(ggplot2)

#histogram of price of yogurt
ggplot(aes(x = price), data = na.omit(yo)) +
  geom_histogram(binwidth=.1)
table(yo$price)
summary(yo$price)

#adding new variables  =total count of yogurts for each observation
#method1 transform(df,new_var=values)
yo<-transform(yo,all.purchases = strawberry + blueberry + pina.colada + plain + mixed.berry)

library(dplyr)#for transforming data

library(tidyr) #for reshaping data

#Method2 - using  mutate()
yo<-yo%>% mutate(count_purchases=strawberry + blueberry + pina.colada + plain + mixed.berry)

summary(yo$count_purchases)
qplot(x = count_purchases, data = na.omit(yo) , binwidth=1 , fill=I('yellow'),color=I('black')) +
  scale_x_continuous(limits=c(0,8),breaks=seq(0,8,1))
#Most households have purchased maximum of 1 or 2 yogurts only.
#75 Percentile is also 2 yogurts -  ie 75% households have buyed less than 2 yogurts types.



#Scatterplot of price vs time - A time Series Graph

ggplot(aes( x = time ,  y = price),data= na.omit(yo)) + 
  geom_jitter(alpha = 1/4 ,shape=20, color='red')



#DATe-28/1/17
#using count to count the no under plyr
library(plyr)
with(filter(yo,count_purchases==1),count(count_purchases))

#adding a new factor variable , grouping count_purchases using cut() -continious to discrete
#now count_purchases has been converted to categorical var with classes or groups
#using breaks to specify where to cut or split

#Using CUT() we can convert any continious variable to categorical variable with different 
# groups/levels/class
yo$count_group<-cut(yo$count_purchases , breaks = c(0 , 2 , 3 , 6, 9 , 11 , 21))

summary(yo$count_group)

#A barchart for categorical data on x and y has count--
ggplot(aes( x = count_group), data = na.omit(yo)) + 
  geom_bar(fill='pink',color="black")

#converting id to factor var
yo$id<-factor(yo$id)


#sampling of random rows from a table or column of df using sample
set.seed(4230)

#taking 16 random samples of  ids 
sample.ids<-sample(levels(yo$id ), 16)

#plotting and faceting(16) graphs the id with 3 way interaction b/w price,time,count_purchases
#IMP plot
#condition for all id in sample.ids
#size used as same as color aes for splitting the plot on the basis of 3rd variable
ggplot(aes(x=time , y = price ) ,data = filter(yo,id %in% sample.ids)) + 
  facet_wrap( ~ id) + 
  #joining all data points with a line
  geom_line() + 
  #splitting by count of purchases for households using size  aes
  geom_point(aes(size = count_purchases) , shape=2)
#such plots are useful in determining the consumer behavior








  