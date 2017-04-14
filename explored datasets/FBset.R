#Reading the dataSet
fb<-read.delim('pseudo_facebook.tsv')
str(fb)

#How many males and females are there
prop.table(table(fb$gender))

library(ggplot2)
##historgram plot for Continious variables which measures the area of distribution
#qplot(data=fb , x = dob_day) +
 # scale_x_continuous(breaks =seq(1,31) )

#Converting to Factor and adding Labels
fb$dob_month<-factor(fb$dob_month , labels=c('January','February', 'March', 'April ', 'may', 'June','July','August','September','October','november','December'))
ordered(fb$dob_month , levels=c('January','February', 'March', 'April ', 'may', 'June','July','August','September','October','november','December'))


##historgram plot of the Dates of Birthdays of the Audience
ggplot(aes(x = dob_day), data = fb) + 
  geom_histogram(binwidth = 1) + 
  scale_x_continuous(breaks = 1:31) +
#facet_wrap(~formula) used for faceting a categorical var to include individual interactions for each month and day
  facet_wrap(~dob_month , ncol =3 )
#Used for splitting the data over the car used in formula

#histogram of DOB months - MAX Birthdays in January
qplot(data = fb , x = dob_month)

#Facet Histogram for interaction b/w birthdates months and days of months-31 different plots for each day of month
qplot(data = fb , x = dob_month) +
  facet_wrap(~dob_day)

#including binwidth and break points in the histogram to have a closer look 
qplot(data= fb , x  =  friend_count , binwidth=25) +
    scale_x_continuous(limits =c(0,1000) , breaks = seq(0,1000 ,50)) 
  #distributed in intervals of 50 on x-axis(friend count)
  

#facetting the historgram with inclusion of Gender vertically to check who has more friends-Male or Female
#ommitting NA values
qplot(data= na.omit(fb) , x  =  friend_count , binwidth=25) +
  scale_x_continuous(limits =c(0,1000) , breaks = seq(0,1000 ,50))  +
  facet_wrap(~gender)

by(fb$friend_count , fb$gender , summary)
#Hence females on a average have more friends , although there are more mens in sample than females

#Median represents the center(middle value) of our distribution and is more reliable here because mean value 
#gets pulled towards right(higher mean value) due to long tailed data

#Histogram for analysing the no of days or tenure of a user on FB in Years
qplot(data = fb , x = tenure/365 , binwidth=.10,color = I('black'), fill = I('#A79420')) + 
  scale_x_continuous(limits=c(0,2), breaks=seq(0,2,0.5))



#Histogram for age of FB users
qplot(data = subset(fb,!is.na(fb$age))  , x = age , color=I('black'), fill = I('blue'), xlim=c(0,100),xlab='Ages of FB users', ylab='Count of Users' ,main='Hist of Ages Vs Freq')


#Digging deep into ages Max count of ages from 10 - 30
qplot(data=subset(fb, !is.na(fb$age)& !is.na(fb$gender)), x = age,binwidth=1, color=I('red'), fill = I('blue'),xlab='Ages of FB users', ylab='Count of Users' ,main='Hist of Ages Vs Freq') +
  scale_x_continuous(limits=c(0,20), breaks=seq(0,20,4)) +
  scale_y_sqrt() +
  facet_wrap(~gender)

#Summary of no of Male and females and their respective ages 
by(fb$age, fb$gender ,summary)


#Multiple plots in one page
#1 
p1=qplot(data=fb, x = friend_count , color=I('black'), fill= I('green')) +
  scale_x_continuous(limits=c(0,1000) , breaks= seq(0,1000 , 100))
p2=p1 + scale_x_log10()

p3= p1 + scale_x_sqrt()

grid.arrange(p1,p2,p3,ncol=1)

##Method 2 to transform the data into logNormal(Normally Distributed) form or log Form
qplot(data = fb , x = log10(friend_count) ) #x -axes shows log values





#Frequecny Polygon-like Histograms
##geom arg and color arg
p4=qplot(data = na.omit(fb) , x = www_likes ,y = ..count../ sum(..count..), geom='freqpoly'  , color=gender) +
  scale_x_continuous(limits=c(0,10) , breaks = seq(0,10,1))
#For 0  to  10 likes only and breaks of 1 on x-axes

 

#2 y = ..density.. (% within each group  )
qplot(data = na.omit(fb) , x = www_likes ,y = (..count../ sum(..count..))*100, geom='freqpoly'  , color=gender) +
  scale_x_continuous(limits=c(0,100) , breaks = seq(0,100,10)) +
  xlab('WWW-Likes of Users') +
  ylab('% of Users')


ggplot(data= na.omit(fb) , aes(x = www_likes)) +
         geom_freqpoly(aes(color=gender)) +
  scale_x_continuous(limits=c(100,2000), breaks= seq(100,2000 , 500))
  xlab('WWW-Likes of Users') +
  ylab('% of Users')
  
#Comparison of Gender vs no of total www-likes for each group  
by(fb$www_likes, fb$gender,sum)
#splitting www_likes over gender and finding total no of likes for each group
 
#boxplots-which contains the continious data on the y-axis and x-axis contains Categorical var
#(opposite of Histograms) for comparing medians and quartiles of categorical data

qplot(data=na.omit(fb), x = gender ,y = friend_count , geom = 'boxplot') +
  
  coord_cartesian(ylim=c(0,1000))
#coord_cartesian() does not removes the data points now it matches the summary ie medians and quantiles


#Checking who initiated more friendships
by(fb$friendships_initiated,fb$gender, summary)

qplot(data = subset(fb,!is.na(gender)) , x=gender , y = friendships_initiated , geom = 'boxplot') +
        coord_cartesian(ylim = c(0,500)) +
  xlab('Gender') +
  ylab('Friend Requests initiated by Genders')


#Introducing new variable or transforming binary data to categorical values 0,1 or Yes/No
fb$mobile_chkin<-NA
fb$mobile_chkin<-ifelse(fb$mobile_likes > 0  , 'Checked in using Mobile' , 'Not used Mobile')
fb$mobile_chkin<-as.factor(fb$mobile_chkin)

summary(fb$mobile_chkin)


prop.table(table(fb$mobile_chkin))*100
#Checked in using Mobile 64.59097 %






#2-variables comparison and analysis

#Scatter plots -basic 2 var plotting - hist ,barplots for 1 variables(on x-axis) vs counts

#adding acsthetic mapping by using color , size, shape 
# argument which splits the distribution into the 2 categories-eg used same in freqpoly plot 
qplot(age,friend_count , data=na.omit(fb), color = gender , geom='point') + 
  xlim(13,30)

#Jitter plot for more accuracy of ages - alpha parameter for less overplotting
# 1/20 means 1 point is now eqv to 20 people
# jittered to reduce overplotting.
qplot(age,friend_count , data=na.omit(fb), geom='point',alpha=1/20) + 
  xlim(13,90) +
  coord_trans(y = 'sqrt')
  
#Obsr 1  - Bulk of young users have friend count below 1000 or close to 1000 
# Also each dark circle takes 20 people or points to become one circle.


#geom = count to count the no of observations at each point.
qplot(age,friend_count , data=na.omit(fb), geom='count') + 
  xlim(13,20) +
  scale_y_continuous(limits = c(0,2000),breaks=seq(0,2000 , 200))



#TRANSFORMING THE COORDINATES coord_trans()

qplot(age,friend_count , data=na.omit(fb), geom='point',alpha=1/20,position = position_jitter(h = 0)) + 
  xlim(13,90) +
  coord_trans(y = 'sqrt') +
  geom_smooth(method = "lm")



#Exploring friend_initiated vs age of which age people created more friend_requests

p1=ggplot(data = na.omit(fb) ,aes(x = age , y = friendships_initiated)) +
  geom_point(aes(color=gender))
by(fb$friendships_initiated , fb$gender , sum)
#On a whole young females have initiated more friend requets than males

#jitter and alpha to reduce overplotting and closely check out density of points in a area
p2 = p1 + geom_jitter(alpha = 1/20, position = position_jitter( h = 0)) + xlim(13,30) + coord_trans(y = 'sqrt') 
#transforaming the coordinates to have a closer look where is the max distribution & where not

# adding a new layer which changes the y axes
p3 = p2 + ylim(0,2000) 
#Obsr 1-maximum young users have initiated friend reauests in a age group of 13 to 25
#Also the bulk of young users have initiated friend_req aroung 500 to 1000 and some above 1000 too


#Analysing Relationship between friend_count and friend_req generated by those people
ggplot(aes(x= friend_count, y = friendships_initiated), data =na.omit(fb)) + 
  geom_jitter(alpha=1/10 , position = position_jitter(h=0),aes(color=gender) )  + 
  geom_smooth(method='lm') + 
  coord_trans(y='sqrt')
#Both these variables are directly related to each other and 
#have a strong positive corelation(r near to 1)
#ALpha TO set the transparency to check out the density of points in an area

library(dplyr) #date-22/1/17
#For transforming tabular data
#METHOD1
#grouping the data frame by age
grp_age<-group_by(fb , age)
#sumarising the data frame grp_age(grouped by age) and adding new columns to it , mean ,median,count of frnd_count
fb_age<-summarise(grp_age , frnd_mean=mean(friend_count) , frnd_median=median(friend_count) ,sum=sum(friend_count),  n=n())
#arranging in ascending order
fb_age<-arrange(fb_age , age)
head(fb_age)



#METHOD 2
#Chaining in R using %>%
##Picks out the first data argument in all functions
#new data frame grouped by age and containeing new summary columns based on friend_count
fb_age_fc<- fb %>% group_by(age) %>% 
                summarise(frnd_mean=mean(friend_count) , frnd_median=median(friend_count) ,sum=sum(friend_count),  count=n()) 
            %>%
                arrange(age)
                
head(fb_age_fc)

#plotting new data frame
p1=ggplot(aes(x = age ,  y = frnd_mean)  ,data = filter(fb_age_fc,age < 71)) + 
  geom_line(arrow = arrow())

p2= p1 + scale_x_continuous(limits = c(13,30), breaks = seq(13,30,5))
p2




#checking which  age group has maximum count
ggplot(aes(x = age , y = count) , data = na.omit(fb_age_fc))  + 
  geom_point()
#age = 18 has maximum count ,  people with age 18 have maximum counts of 5196 also have maximum sum of friend_counts
 



#Overlaying summaries on the plots using geom_line( stat = 'summary') 
#We can use geom_line and overlaying summaries of variables to have 
#a closer look at the distributions , quantiles, mean ,medians ,percentiles etc.


p3=ggplot(data = na.omit(fb) ,aes(x = age , y = friend_count)) +
  geom_point(alpha = 1/20 ,position = position_jitter(h = 0), color='blue') +
  geom_line(stat = 'summary', fun.y = mean) +
#MEdian line 50% , middle value
  geom_line(stat = 'summary', fun.y = median , color = 'red' , linetype = 2 ) + 
#90 th quantile-90% users have less frind_count then 90% quantile value or 10 % have more than this value
  geom_line(stat = 'summary', fun.y = quantile ,fun.args=list(probs=.9) ,color = 'black' , linetype = 3 ) +
  coord_trans(y = 'sqrt')
  
p4= p3 + coord_cartesian(xlim=c(13,30), ylim = c(0,1000)) 
  






#CORELLATION COEFFIECIENT-TO FIND THE MEASEURE OF STRENGTH OF LINEAR RELATIONSHIP B/W
#2 VARIABLES OF A DATA SET- r = 1(completely linearly related) , r = -1(anti-corellated)
# r > 0 (positive relation)  , r < 1 ( inversely related) , r = 0 (no relation)


cor.test(fb$age , fb $ friend_count)
#Both are negetively corellated r =-0.0274
# i.e which means there is no actual relationship b/w both.
#Rule of thumb- r = |0.3| min is regarded meaningful. 

#corelation for only young  members
with(filter(fb , age<=30 ), cor.test(age, friend_count ))


#DATE-23/1/17
with(na.omit(fb),cor.test(likes_received , www_likes_received,method = 'pearson'))
# r = 0.9479837  , which represents a strong +ve linear relationship i.e if
# likes increases then the www_likes for a user will also increase linearly.

#plotting 
#jitter and alpha to avoid overplotting and to closely check the 
#density of distributions in which area is high and low


p1=ggplot(aes(x =  www_likes_received , y = likes_received ) , data = na.omit(fb)) + 
  geom_jitter(position=position_jitter( h = 0), alpha= 1/5 ,color= 'blue') +
#adding xlims from 0 to 95 percentile of www_likes to have a closer look  
  xlim(0 ,quantile(fb$www_likes_received , c(0.95))) + 
#adding ylims from 0 to 95 percentile of likes to have a closer look 
  ylim(0,quantile(fb$likes_received , c(0.95))) +
  geom_smooth(method = 'lm' , color = 'red')
#including a regresssion line to examine real relationship b/w variables


p2= p1 + coord_trans(y = 'sqrt')
#line plot
p3=ggplot(aes(x =  www_likes_received , y = likes_received ) , data = na.omit(fb)) +
  geom_line(arrow = arrow()) + coord_trans(y = 'sqrt')


summary(fb$www_likes_received)
summary(fb$likes_received)

quantile(na.omit(fb$www_likes_received) , c(0.90))
#90 % of users have less than 98 www_likes count, only 10 % more than 98

quantile(na.omit(fb$likes_received) , c(0.90))
# 90 % of users have less than 259 likes ,only 10% more than 259

#obsv1- mostly bulk of users have 0 to few  100  likes and www_likes

#corelation for 95 percentile of www_likes and likes_received
with(subset(fb , www_likes_received > 227 & likes_received  > 561),cor.test(likes_received , www_likes_received,method = 'pearson'))





#DATE-24/1/17
#Understanding Noise
#Creating a new variable 'age_with_months'
fb$age_with_month<- fb$age + (12-fb$dob_month)/12


#creating and now grouping by age_with_month using dplyr package and summarise function
# for mean friend_count and median for smaller binwidths(smaller the binwidth better accuracy)

fb.fc_by_age_month<-fb  %>%   group_by(age_with_month) %>%     
  
  summarise(mean_friend_count = mean(friend_count) , median_friend_count = median(as.numeric(friend_count)) , total_sum = sum(friend_count),quantile_95 = quantile(friend_count ,c(.95))  ,n = n())  %>%     
    
  arrange(age_with_month) 

head(fb.fc_by_age_month)
  

#plotting the mean_friend_count vs the ages by months 
p1=ggplot(aes(x = age_with_month  ,   y = mean_friend_count ), data = filter(fb.fc_by_age_month , age_with_month < 71 ))+   
    
  geom_line(stat='summary' , fun.y = quantile , fun.args=list(probs=0.9) , color= 'red')
  geom_smooth()
#plot has more noise because this time we compare ages_in_months vs friend_count i.e lesser binwidth

#plot from first grouping by ages
p2=ggplot(aes(x = age ,  y = frnd_mean)  ,data = filter(fb_age_fc,age < 71)) + 
  geom_line(arrow = arrow()) +
  geom_smooth()

#locally weighted scatterplot smoothing(LOWSS) - geom_smooth()-smooth the statistics of conditional mean

p3=ggplot(aes(x = round(age/5) * 5 ,  y = friend_count)  ,data = filter(fb,age < 71)) + 
  geom_line(stat='summary', fun.y = mean)  + 
  geom_smooth() 
#addling a linear model line-this shows that we can actually predict price from volume or
#vice versa
#plotting the mean friend_count line

#NOTE-The best-fit curve is often assumed to be that which 
# minimizes the sum of squared residuals(RSS) - OLS regression.
#Residuals are the diff b/w the actual observed value of Response var of the data 
# and the values of Respone Variables that the regression model predicted.
#Generally residuals should be Normally distributed-ASsumption 1 of regression
# ie near about 0 - ie no errors in model

library(gridExtra)
grid.arrange(p1,p2,p3 ,ncol=1)





#DATE-27/1/17
#MULTIVARIATE ANALYSIS

#grouping by 2 variables  age and gender_group

fb.fc_by_age_gender<- fb %>% 
  #Removing NA values using filter()
  filter(!is.na(gender)) %>%   
  group_by(age,gender) %>% 
  summarise(mean_friend_count = mean(friend_count), median_friend_count = median(friend_count), total_count=sum(friend_count), n = n())   %>%
#Need to use ungroup() , because summarise will remove one layer grouping when we run it.
    ungroup() %>%
  arrange(age) #arranging in ascending order of age


#Plot of age vs Median_friend_count and also overlying gender(Binary var) 
#in line and scatter plots
ggplot(aes(x = age, y = median_friend_count) ,data = fb.fc_by_age_gender ) + 
  geom_line(aes(color=gender)) +
  coord_trans( y = 'sqrt') +
  geom_smooth() + 
  scale_x_continuous(limits= c(13,60),breaks= seq(13,60,5))
#transforming the x-axes for ages 13 to 30 with breaks = 2
#mostly young females have more median_friend_count than males

ggplot(aes(x = age, y = median_friend_count) ,data = fb.fc_by_age_gender)  + 
  geom_line(stat='summary' , aes(color=gender),fun.y=median)  
  
  
#TIDYR and DPLYR are data cleaning and transforming packages - pre processing 
#Reshaping Data using tidyr - spread , gather ,unite and saperate
library(tidyr)
#using tidyr - to spread - long to wide format
#Using piping
new_fb_fc_age_gender<-fb.fc_by_age_gender %>%
  #selecting only 3 variables(columns) , using select()
  select( age , median_friend_count , gender) %>%
  #spreading long to wide format- adding new columns-decreasing the observations(tuples)
  spread(gender,median_friend_count)


#Method2 - using dcast for long to wide
library(reshape2)
new_fb_fc_age_gender.wide<-fb.fc_by_age_gender %>%
      dcast(age  ~  gender, value.var= 'median_friend_count') %>%
      
  #adding a new column of female to male ratios
  mutate(ratio=female/male)

#Plotting  male_median_friend_count vs female_median_fc
ggplot(aes(x = female, y = male   ), data=new_fb_fc_age_gender.wide) + 
  geom_point() +
  geom_smooth()  +
  ylim(0,100) + 
  xlim(0,100)  + 
  xlab('Female Median Frient Count')  + 
  ylab('Male Median Friend Count')
#hence we can say that the median_friend_count(male) is somewhat +ve linearly related with 
#female_median_friend_count
with(new_fb_fc_age_gender.wide,cor.test(female,male))
# r= 0.7172891



#DATE-27/1/17
#Plotting age vs ratio of male median fc and female median fc 
ggplot(aes(x = age , y = ratio), data = new_fb_fc_age_gender.wide) + 
  geom_line()  + 
  geom_hline(yintercept = 1 , alpha = 0.3, linetype= 2 )
#ratio of 1 means that the ratio of female to male is same
#geom_hline to add a y-intercept of 1

#OBSV-1 for young users mostly female median FC is more than 2.5 times of male median FC




#THIRD CONTINIOUS(QUANTITATIVE) VARIABLE
#taking 2014 as refrence year
#year_joined  = 2014 - tenure_in_days / 365
#using mutate to add new variable

#eg of data discretization ( covnverting continious values to discrete values)

fb<-fb  %>%  mutate(year_joined=floor(2014 - tenure/365 ))

#Hence year_joined is a dicrete variable(does not have much values-finite small distribution)
summary(fb$year_joined)
#To have count of users in each year
table(fb$year_joined)
#looks like max users joined FB in year 2013 - 43588 users

#using cut() to convert continious var to discrete of factor variable , grouping data in bins

#grouping data
fb$year_joined.bucket<-cut(fb$year_joined,breaks = c(2004 , 2009 , 2011 , 2012 , 2014))
#breaking at 2004 , 2009 , 2011 , 2012 , 2014 using break in cut
#so the groups are 2004-2009 , 2009 -2011 , 2011 - 2012 , 2012 -2014 - 4 levels of factor
#Note that a parenthesis means exclude the year and a
# bracket means include the year.
summary(fb$year_joined.bucket)
table(fb$year_joined.bucket)


#plotting age vs friend_count and splitting by 3rd categorical var -year_joined_bucket as line
#representing the median_friend_count in each joining year group
#adding 3rd var as aes color in ggplot or saperately in geom_line() aes wrapper
#adding the median friend_count lines for each year of joining FB group

p1=ggplot(aes(x = age , y = friend_count,color= year_joined.bucket) ,data = na.omit(fb)) + 
  geom_line( stat = 'summary' , fun.y = mean)  + 
  geom_line(stat='summary' , fun.y = mean , linetype = 2,color='black') #mean for friend_count

#obsr-1
#We can easily observe that users with longer tenure have more number of friend_counts than those
#with shorted tenure , also young users are the ones with longer tenure and more friend_counts
p2=p1 + scale_x_continuous(limits=c(13, 30), breaks=seq(13,30,4))


#friend_rate using friend_count since the first day they joined FB
by(fb$friend_count , fb$year_joined.bucket,summary)
#Boxplot
ggplot(aes(x = year_joined.bucket, y = friend_count) , data = na.omit(fb)) + 
  geom_boxplot() + 
  coord_cartesian(ylim=c(0,1000))
#highest friend-count for users b/w tenure 2004-2009 -been longest on FB 


#finding the friending rate per day since the user joined FB = frined_count/tenure=friending_rate
#subsetting the data for users having tenure of atleast 1 day
fb %>% filter(tenure >= 1) %>% with(summary(friend_count/tenure))
#outlier max = 417 and 75quantile = 0.5 
#95 Percentile for friending rate
with(filter(fb , tenure >= 1),quantile(fb$friend_count/fb$tenure,c(.95),na.rm=T))



#Exploring friendships_initiated per day (tenure) vs tenure and splitting by 
#mean(friendships_initiated/tenure) for year_joined_bucket(categorical var)

# adjusting binwidths of x-axis to reduce noisy data and smoothen the plot 
# using geom_smooth
#averaging the tenure by 7 days to smoothen the plot and decrease the variances and Bias
ggplot(aes(x = 7 * round(tenure/7), y = friendships_initiated/tenure),data= filter(na.omit(fb),tenure>=1)) + 
  geom_smooth(aes(color=year_joined.bucket)) + 
#smoothing the plot
  scale_x_log10()

#Hence we make an observation that there is a decline in friendships_initiated as tenure
# on FB increases , only those initiated more frnd_reqs which were new to FB(lesser tenure)
#This trend completely holds truth as people who have been longer on FB have generated less
# number of friend_requests than those who recetly joined FB i.e with lesser tenure.
#THis thing also applies to real life.





#DATE-28/1/17
#ScatterPlots Matrices -for creating multiple scatterplots for all varaiables of a data set
# along with boxplots for categorical variables and corelations coef(r)
install.packages('GGally') 
library(GGally)

set.seed(1836)
#subsetting the fb data - removing some columns undesirable variables
#including only 14 var -removing userids and NA values
fb.subset<-subset(na.omit(fb[  ,  c(2:15)  ]))
names(fb.subset)
#generating 1000 random sample observations from fb.subset df for 
# creating scatterplot matrix

ggpairs(fb.subset[sample.int(nrow(fb.subset), 1000) ,    ])   
ggsave("FBdataScatterplotMatrixNew.pdf",plot=last_plot(), path='C:/Users/hp/Documents/RStudio')

#for categorical variables vs continious var , it creates a histogram faceted with
# a categorical var like we did earlier for variables splitted over categorical var in histogrmas


# Many interesting variables are derived from two or more others.
# For example, we might wonder how much of a person's network on
# a service like Facebook the user actively initiated. Two users
# with the same degree (or number of friends) might be very
# different if one initiated most of those connections on the
# service, while the other initiated very few. So it could be
# useful to consider this proportion of existing friendships that
# the user initiated. This might be a good predictor of how active
# a user is compared with their peers, or other traits, such as
# personality (i.e., is this person an extrovert?).
library(dplyr)
#New variables added - transforming data

mutate( fb, prop_initiated = friendships_initiated / friend_count )

#transforming data by adding new attrs
#Method 2 using ifelse for people with friend_count of 0 
fb$prop_initiated<-ifelse(fb$friend_count > 0, (fb$friendships_initiated / fb$friend_count ) , 0 )



#plotting median proportions of friendship initiated vs tenure and coloring the graph with year 
#joined bucket(categorical var with 4 bins of years)


ggplot(aes(x = tenure, y = prop_initiated), data = na.omit(fb)) + 
  #y has median of prop_initiated for each bin of year.joined bucket
  geom_line(stat='summary',aes(color=year_joined.bucket), fun.y = median) + 
  #smoothining the line plot
  geom_smooth()
#hence we observe that our intuition was right people who recently joined FB initiated more 
#friendships i.e their median prop_firndships was greater than other with more time on FB

#this was also observed earlier when we plotted year.joined vs friendships_initiated
#i.e users with more time on FB initiated lesser frnd reqs 

by(fb$prop_initiated, fb$year_joined.bucket,summary)
#Hence the group which initiated the most proption of 
#friend req was belonging to the bucket (2012,2014] i.e 
#after 2012 users initiated max frnd_req



