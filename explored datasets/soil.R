#DATE-23/1/17

library(alr3)
library(ggplot2)
#loading data set - Mitchell
data(Mitchell)
str(Mitchell)

ggplot(aes( x = Month%%12 , y = Temp ) , data = Mitchell) + 
  geom_point() + 
  scale_x_continuous(breaks = seq(0,204,12) )
#breaking up the x ases into inc of 12 months to correspond to a year.
#No correlation at all

cor(Mitchell$Month ,Mitchell$Temp)

#The plot shows a sinosudal behavior
#i.e which shows that in winters the soil temp dec and in summers it increases so the mechanism is quiet
# understanding that when and why at what time of the year the soil temp changes