#reading Data set - Corruption Index for years 2008 and 2009
library(XLConnect)
corrupt<-readWorksheetFromFile("CPI.xlsx",sheet=1) #Reading EXCEL files
corrupt<-na.omit(corrupt)
str(corrupt)
#Total 180 countries in the data
names(corrupt)<-c('Country', 'CPI1' , 'CPI2')

#The lesser CPI the more corrupt the country
#The scores of countries for respective years are given out of 10 , lesser -more corrupt
#Score of 10 -Very Clean ,  0 -Highly Corrupt

library(ggplot2)

#Historgrm for year CPI  of 2008
p1=qplot(data  = corrupt , x =CPI1 , color=I('black') ,fill=I('blue')) +
  scale_x_continuous(limits=c(0,5), breaks=seq(0,5,0.5))

summary(corrupt$CPI1)
summary(corrupt$CPI2)

#Hsit for year 2009

ggplot(data= na.omit(corrupt) , aes(x =CPI2  )) +
  geom_histogram(color=I('black'), fill = I('yellow') , binwidth = .01) +
  scale_x_continuous(limits=c(9,9.5) , breaks=seq(9,9.5,0.1))


#The mean CPI in 2009 increases by some small number but the Median CPI for both years is same

#Conc-2 By seeing the Quartiles we observe that the corruption increases by a slight factor in 2009 

#conc-3 Though median values remain the same - but avg CPI  score has increased in 2009

sum(corrupt$CPI2 > 9 ) #Result  2009= 5 ie only 5 countries with CPI > 9 in 2009

sum(corrupt$CPI1 > 9 ) #2008- 4 countries only




