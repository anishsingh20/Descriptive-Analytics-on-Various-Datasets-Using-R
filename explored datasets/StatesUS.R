#States Data set -Udacity
data1<-read.csv('C:/Users/hp/Documents/RStudio/DataSets/stateData.csv')
setwd('C:/Users/hp/Documents/RStudio')
data1$X[data1$population > 20000] #State having population > 20000
min(data1$population)
regn1<-data1[data1$state.region==1,] ##states which belongs to region 1
regn1##exploring Region 1

plot(regn1$population , regn1$income,xlab = 'Population' , ylab = 'Income', main='Plot of Population vs Income rates')


regn1$Educated<-NA #Adding new column 

regn1$Educated[regn1$illiteracy <  1]<-'Educated'; #If illiteracy < 1% = educated
regn1$Educated[regn1$illiteracy >= 1]<-'Uneducated';  # If illiteracy >  1%  = UnEducated
prop.table(table(regn1$Educated))##proportions of Illiteracy
cor.test(regn1$illiteracy , regn1$income) 
## We can say that in Region 1 of USA as income increases illiteracy also increas in the States

cor.test(regn1$population, regn1$income)