#Reading the CSV data frame
bday<-read.csv('birthdaysExample.csv')

#converting to Date formate
bday1<-as.Date(bday$dates ,format=  '%m/%d/%Y')


#Changing the Date Format 
bday2<-format(bday1,"%a %b %d")
bday$Ndate<-bday1


library(ggplot2)

qplot(data=bday , x =Ndate , color= I('black'), fill = I('green')) +
scale_x_date(breaks = waiver(), labels=waiver(), minor_breaks = waiver())