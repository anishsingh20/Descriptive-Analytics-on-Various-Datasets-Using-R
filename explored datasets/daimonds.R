#Loading data
library(ggplot2)
data('diamonds')

p1=qplot(data= na.omit(diamonds), x = price , color=I('black'), fill = I('yellow'),binwidth=15) +
  facet_wrap(~cut ,ncol =2)
p2=p1 + scale_x_continuous(limits=c(0,800), breaks = seq(0,800,50))
p2
#most common price of the daimond is around 700 and  the mode of the daimonds ie most frequenrly ouccuring value is 605(counts)
ggsave('priceHistogram.png')

summary(diamonds$price)



#Different historgrams of price by diff breaks and limits

p3=p1 + scale_x_continuous(limits=c(0,5000), breaks = seq(0,5000,1000))

p4= p1 + scale_x_continuous(limits=c(0,1000), breaks = seq(0,1000,100))

p5 = p1 + scale_x_continuous(limits=c(0,10000), breaks = seq(0,10000,1000))


#Finding SUmmary statistics of prices of diamonds splitted by cut
by(diamonds$price , diamonds$cut ,summary,digits = max(getOption('digits')))
#Lsat argument gives us the exact values


#Histogram of price per caret facetted by cut - maximum for Ideal cut
qplot(data= na.omit(diamonds), x =  price/carat , color = I('black'), fill = I('yellow'))  +
  facet_wrap(~cut) +
  scale_x_continuous(limits=c(0,10000), breaks = seq(0,10000 , 1000))


#Finding total counts of daimonds splitted by each cut - MAx for Ideal daimonds(most daimonds belong to this category)  
by(diamonds$price/diamonds$carat , diamonds$cut,sum)



#Box plots for analysing categorical var and their distribution-Medians
p6=qplot(data=subset(diamonds,!is.na(cut)) , x = cut , y = price , geom = 'boxplot' )
p7= p6 +scale_y_continuous(limits=c(0,5000)) # wrong values-deleted data points and gives wrong values
p8= p6 + coord_cartesian(ylim =c(0,8000)) ##to avoid deletion of data points - accurate
p8#highest median price for fair cut diamonds(50% of Fair cut diamonds cost more than median price)



#Boxplot For clartity of diamonds vs price 
#Summary statistics
by(diamonds$price, diamonds$clarity ,summary,digits = max(getOption('digits')))



qplot(data =diamonds , x = clarity , y = price , geom = 'boxplot') +
  coord_cartesian(ylim=c(0,8000)) 
#Clarity level SI2 has highest median price and mean price-ie on an average these are most costly




#Boxplot for color of diamonds vs Price-which color diamond has highest price
by(diamonds$price, diamonds$color , summary ,digits = max(getOption('digits'))) 
qplot(data =diamonds , x = color , y = price , geom = 'boxplot') +
  coord_cartesian(ylim=c(0,8000)) 
#highest mean and median price for J-coloured diamonds ie(worst)-on an average J-col diamonds 
#most costly
#Finding IQR - Q3-Q1
IQR(subset(diamonds,diamonds$color=='J')$price)

#On a whole it can be concluded that on an average most costly diamonds are those having worst colour-J
#having clarity SI2 (not the best) and Premium  cut diamonds.


by(diamonds$price  / diamonds$carat , diamonds$color ,summary )
#Price per carat box plot vs Color
qplot(data =diamonds , x = color , y =  price /carat , geom  = 'boxplot') + 
  coord_cartesian(ylim = c(0,8000)) +
  scale_y_continuous(breaks=seq(0,8000,500))
#obsvr - 



#FREQUENCY POLYGON

qplot(data = na.omit(diamonds) , x = carat , geom= 'freqpoly' , binwidth=.1,color=cut) +
  coord_cartesian(ylim=c(2000,10000))
#NOTE- THe lesser the binwidth the more accurate count(freq) the freq plot graph gives
#count> 2000 only for 2 weights 0.3 and 1.01 which belongs to Ideal and very good Cuts

#tidyr package 
#uniting multiple columns in one single column
librayr(tidyr)
diam1<-unite(diamonds,'l-b-h' , x, y ,z , sep ='-' )

#Saperating single column to multiple columns
diam2<-separate(diam1, 'l-b-h' , c('Length in  mm' , 'Breadth in mm ' , 'Depth in  mm'), sep='-')




#DATE-26/1/17
#analysing 2 Variables-

#scatterplot of price vs length of diamonds in mm
p1=ggplot(aes(x = price, y = x ), data = na.omit(diamonds)) + 
  geom_point(alpha=1/10,color='red')
#reducing overplotting
#OBSVR-
#One can observer some what exponential relationship b/w price and x , also has some outliers with
# x =0


#Correlations - strength of linear relationship(Quantifying the relation)
#using correlations we can only make out which variables in a data set have 
#strong linear relations or associations and using the value of r next step we can 
#make do statictical modelling(predictive analytics) and predict values 
#using the related variables.


cor.test(diamonds$x , diamonds$price)
cor.test(diamonds$y , diamonds$price)
cor.test(diamonds$z , diamonds$price)
#hence there exits a linear relationship b/w price and  x, y, z due to high r value

#NOTE-A single outlying observation can have a substantial effect on 
#the correlation coefficient.

cor(diamonds$price, diamonds$depth)

#high alpha value to add more transparency and observe more closely the bulk values of depth and price
#1 dark point = 100 diamonds
ggplot(aes(x = price, y = depth), data = na.omit(diamonds)) + 
  geom_jitter(alpha = 1/100)  + 
  scale_y_continuous(limits=c(50,70), breaks =seq(50,70,5))
  scale_x_continuous(limits=c(0,5000),breaks=seq(0,5000,500))
# a weird observaion , there is no diamond for cost=1500
#there is no linear relationship b/w prcie and depth of diamonds ie r = 0 
#most diamonds are in a range of depth 60 to 64 mm
  
#PLOT-3 Price vs Carat
#both have a very strong positive relation(r = .95) which suggests as carat(wt) of diamond 
#increases the price also linearly increases.
  
ggplot(aes(x = carat, y = price), data = na.omit(diamonds)) + 
  geom_point()
  
  
#plot-4 -Price vs Volume of diamonds(l*b*h)

#creating new variable Volume(l*b*h)
diamonds<-mutate(diamonds , volume = x*y*z)

p3=ggplot(aes(x = price , y =volume ), data = filter(diamonds , volume!=0 & volume<=800)) + 
  geom_point(alpha = 1/100) + 
  geom_smooth()

#having a closer look o to 95% percentile(ie95% diamonds have less than this value) 
# of volume i.e
p4= p3  + scale_y_continuous(limits=c(0,quantile(diamonds$volume , c(0.95))))  +geom_smooth()

p5=p3 + scale_y_continuous(limits=c(0,quantile(diamonds$volume , c(0.25))))

#df of diamonds excluding volume= 0 and volume > = 800
diam_vol<-subset(diamonds , volume!=0 & volume<=800)
with(filter(diamonds , volume!=0 & volume<=800) ,cor(volume,price))
#[1]r = 0.9023845
#Again we conclude tht there is a strong linear relationship b/w price and volume of the
#diamond , hence we can use it in precitive modelling(regression) -prediction price using 
# random volumes





# Creating new Dataframe-grouping by clarity
#and summarising on the basis of price of each category of clarity

#1-clarity
diamondsByClarity<- diamonds %>% group_by(clarity) %>%
        summarise(mean_price = mean(price) , median_price = median(price), min_price=min(price), max_price=max(price),sum_price=sum(price),n = n()) 

        
#2 color and price statistics for each categrory of color of diamonds
diamondsByColor<- diamonds %>% group_by(color) %>%
  summarise(mean_price = mean(price), median_price=median(price) , min_price=min(price),max_price=max(price) , sum_price=sum(price), count=n())
  

#plotting barplots-x-axis has categorical data , y has count(frequency) of each category 
#Different from  histograms-have x-axes as continious data , y = count

#1Color vsmean_price-use geom_col() if using y axes as continious var not count
  p1=ggplot(aes(x=color,y=mean_price), data=na.omit(diamondsByColor))  +
    geom_col()
  #Highest for color J(worst)
  
#2Clarity Vs mean_price  
  p2=ggplot(aes(x=clarity,y=mean_price), data=na.omit(diamondsByClarity))  +
    geom_col()
 #highest mean_price for clarity=SI2(worse)
  
  #AGAINST OUR INTUITION

library(gridExtra)
grid.arrange(p1,p2,ncol=1)






#DATE-29/1/17

#1)MULTIVARIATE ANALYSIS - analysis b/w multiple 3 or more variables using color aes 
#2)using geom_line() to add lines or color lines to the existing scaterplot of 2 variables etc.
#3)using Scatterplot Matrices to analyse all the variables of the data set using ggpairs()
#4)Facetting multiple groups and their summmaries into the plots using facet_wrap().
#5)Grouping multiple variables ,transforming data , summarising data under dplyr package
#6) Then splitting using those summaries statistics the plots b/w 2 var and including
#3rd variable using geom_line/point(aes(color/size/shape))
#7)Long data to wide data using spread()/dcast() and vice versa using gather()/melt()  
#8)Converting continious var to factor var(categorical) having diff groups/classes using cut()

#freqploy for price and count splitting by cut and faceted by color of diamonds
ggplot(aes(x=price,color=cut) ,data = na.omit(diamonds)) +
  geom_freqpoly() +
  facet_wrap(~color)
#OBSVR
#looking at the graph we can see that highest count is for 'ideal' diamonds 
#in all coloured diamonds
#J(worst) colored diamonds gave least count(least in number)
#mostly all ideal cutted diamonds are cheap
#fair cut diamonds have least count amongst all coloured diamonds
  
#generating scatterplot and coloring points by cut

  ggplot(aes(x = table , y = price,color=cut) ,data = na.omit(diamonds)) + 
    geom_point() + 
    scale_color_brewer(type = 'qual') + 
    scale_x_continuous(limits=c(50,65),breaks=seq(50,65,2))
  
  #having acloser look at the plot we observe
#1)Typical table range for Ideal cut diamonds is from 53 to 57 
  #2) Table range for Premium cut diamonds is from 58 to 62 
  #3) Also maximum distribution in the sample is for ideal and premium cut diamonds
  
  
#adding new varible volume of diamond
#Method1- using tansform  
diamonds<-transform(diamonds,volume = x*y*z)  

#Method2- using mutate()
diamonds<- diamonds %>% mutate(volume = x*y*z)

#IMP-taking  99 percentile of volume as xlims 
ggplot(aes(x = volume, y = price), data  = na.omit(diamonds)) + 
  geom_point(aes(color = clarity)) + 
  scale_color_brewer(type='div') +
#taking  99 percentile of diamonds volume(i.e 99 % diamonds have volume 
#less than this value) - or omitting top 1% of diamonds volumes
  xlim(0,quantile(diamonds$volume,c(0.99))) +
  scale_y_log10()  + 
  #transforming y coordinates
    coord_trans(y = 'sqrt')  
 
#diamonds of clarity level= IF(best),VVS2,VVS1 typically have lesser 
#volumes than those of belonging to SI1, SI2 ,L1 ,VS2
  #Highest volumes for least clear diamonds L1
  
by(diamonds$volume, diamonds$clarity,summary)  


#DATE-30/1/17


diamonds<-mutate(diamonds, ratio_ppc = price/carat)
#scatterplot of price/caret ratio of diamonds vs cut colored by color faceted by  clarity
ggplot(aes(x=cut , y = ratio_ppc), data = diamonds)  +
  geom_point(aes(color=color)) + 
  scale_color_brewer(type = 'div') +
  facet_wrap(~ clarity) +
  coord_trans(y = 'sqrt')


#BOXPLOT
ggplot(aes(x = clarity, y = ratio_ppc),data = na.omit(diamonds)) + 
  geom_boxplot() + 
  coord_cartesian()
    


#-DATE-1/2/17
# The scatterplot is a powerful tool to help you understand
# the relationship between two continuous variables.
# We can quickly see if the relationship is linear or not.

ggplot(aes(x = carat ,  y = price,color = cut), data =na.omit(diamonds)) +
  geom_point(alpha= 1/4) + 
  geom_smooth(method = 'glm',color='red')+
  scale_color_brewer(type = 'div')  +
  xlim(0 ,quantile(diamonds$carat,c(.99))) + 
  ylim(0 ,quantile(diamonds$price,c(.99)))
#limiting the x and  y axes to omit top 1% diamonds -i.e 99 percentile 
  
#for Scatterplot  matrices - and r
library(GGally)#scatterplot matrices
library(memisc)#to summarise the regression
library(scales)#for writing things
library(MASS)
library(car)
library(ggplot2)


#creating a Random diamonds sample with 10000 rows
#sample(df , size)
diamonds_samp<- diamonds[sample(nrow(diamonds),10000) , ]
set.seed(121221)

ggpairs(diamonds_samp) #for making scatterplot matrix


#Normalising Data  - logNormal to improve the Modelling-Normal Distributions
#for single continious var-hist
p1=ggplot(aes(x = price), data = na.omit(diamonds)) + 
  geom_histogram(binwidth = 100,color=I('black'), fill = I('yellow')) + 
  ggtitle('Not Normalized')
  #this one looks long-tailed and a lot  skewed

#binwidth is basically width or interval/size  of 1 bin-bucket (group)
#lesser binwidth more accurate counts-ie smaller groups or smaller size of bucket

#lognormal distribution- scale_x_log10
p2=ggplot(aes(x = price), data = na.omit(diamonds)) + 
  geom_histogram(binwidth = .01,color=I('black'), fill = I('red')) + 
  scale_x_log10() + 
  ggtitle('Lognormal Distribution for Normalizing data')
  #this plot shows somewhat bell-shaped curve - giving us a closer look at Normalized data.  


library(gridExtra)
grid.arrange(p1,p2 , ncol=2)



#cuberoot transformation of axes
cube_root<- function() trans_new('cuberoot', 
                                 transform = function(x) x^(1/3) , 
                                 inverse = function(x) x^3)


#transforming the axes - for Normalizing results

ggplot(aes(x = carat ,  y = price,color = cut), data =na.omit(diamonds)) +
  geom_point() + 
  #x-axes transformed to cuberoot-volume
  scale_x_continuous(trans = cube_root(), limits = c(0.2,3) , breaks = c(0.2,0.5,1,2,3)) + 
#transforming y i.e price lognormally-due to wide range of value-spread  
  scale_y_continuous(limits = c(350,15000 ) , trans=log10_trans() , breaks=c(250,1000,5000,10000,15000)) +
  scale_color_brewer(type = 'div')  +
  geom_smooth(method = 'glm',color='red')
#now after transforming the axes-we can notice that the plot now is nice and linear,is nicely 
#scaled



#solving overplotting issue-when same point takes multiple values-which can result in
#wrong judgements and hide the imp distributions and density of distributions at certain areas
head(sort(table(diamonds$carat),decreasing = T))
#checking out the count-same carat value has lots of diamonds in that wt group

head(sort(table(diamonds$price),decreasing = T))


#to avoid over-plotting we add transparency in plot using alpha, use jitter plots and size
#to check out densities at different areas


#1)--Coloring the plot by clarity-3 var analysis
ggplot(aes(x = carat ,  y = price,color=clarity), data =na.omit(diamonds)) +
  geom_point(alpha=1/2 , size = 2 , position = 'jitter') + 
  #x-axes transformed to cuberoot-volume
  scale_x_continuous(trans = cube_root(), limits = c(0.2,3) , breaks = c(0.2,0.5,1,2,3)) + 
  #transforming y i.e price lognormally-due to wide range of value-spread  
  scale_y_continuous(limits = c(350,15000 ) , trans=log10_trans() , breaks=c(250,1000,5000,10000,15000)) +
  scale_color_brewer(type = 'div',guide = guide_legend(title = 'Clarity', reverse=T))  


#OBSV-yes clarity does explains the variance in price of the diamonds , because if we see,
#for constant weight of diamonds the diamonds with higher clarity level have greater price than 
#other lower clarity level which are  cheaper.



#2--Coloring the plot by cut-most important factor , the cut actually expalains the shine
# in the diamonds -also color and clarity does not affect the diamond quality to shine(sparkle) , as 
# much as cut does.
#but most of the diamonds are IDEAL cut diamonds

ggplot(aes(x = carat, y = price,color=cut),data = na.omit(diamonds)) + 
  geom_point(alpha = 1/5 , position = 'jitter',size =1) + 
  scale_color_brewer(type='div',direction = -1, guide = guide_legend(title ='Cut of diamonds' , reverse = T)) + 
  #theme_dark() +
#direction = -1 given for revercing the color assigned to  cuts
  scale_x_continuous(trans = cube_root() ,limits = c(0.2,3) , breaks = c(0.2,0.5,1,2,3)) + 
  scale_y_continuous(trans = log10_trans(),limits = c(350,15000 ),breaks=c(250,1000,5000,10000,15000)) + 
  ggtitle('Carat vs Price splitted by cut of diamonds')




#3) By color


ggplot(aes(x = carat, y = price,color=color),data = na.omit(diamonds)) + 
  geom_point(alpha = 1/5 , position = 'jitter',size =1) + 
  scale_color_brewer(type='div',direction = -1, guide = guide_legend(title ='Color of diamonds' , reverse = F)) + 
  #theme_dark() +
  #direction = -1 given for revercing the color assigned to  cuts
  scale_x_continuous(trans = cube_root() ,limits = c(0.2,3) , breaks = c(0.2,0.5,1,2,3)) + 
  scale_y_continuous(limits = c(350,15000 ),breaks=c(250,1000,5000,10000,15000)) + 
  ggtitle('Carat vs Price splitted by color of diamonds')+ 
  #tansforming the y-coordinates
  coord_trans( y = 'log10')



#DATE-3/2/17
#Building LINEAR MODELS-REGRESSION TECHNIQUES for PREDICTING VALUES

#model1
#I()-for transforming the variables before running/using it in regression for extimating coef  
#log for Normalizing the price var due to long tailed and values spread over wide ranges
model1<-lm(I(log(price)) ~ I(carat^(1/3)) , data = na.omit(diamonds))
summary(model1)
#R-squared:  0.9236 for goodness of fit and predicting power
#it expalains that 92% of variance in price is explained by var in carat
#Also it explainds that 92% of cause of price of diamond is due to the carat of diamonds

#updating the model
#adding more predictor variables
m2<-update(model1,  ~ . + carat)
m3<-update(m2 , ~  .  + cut + color + clarity)

#plotting the residuals
#hence we can see that the residuals are Normally distributed-i.e mean diff b/w actual price
# and the price that the model predicted is nearby 0
hist(m3$residuals)

plot(model1)

mtable(model1 , m2 ,m3,sdigits = 3)

