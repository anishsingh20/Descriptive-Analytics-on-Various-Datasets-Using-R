mean(mtcars$mpg)
library(ggplot2)
plot(data= mtcars ,  x=  mpg , y = wt  , type = 'l')
cars1<-subset(mtcars , complete.cases(mpg), select = c(mpg,hp,wt))
cars1
plot(cars1$hp , cars1$mpg)
cor.test(cars1$hp , cars1$mpg, alternative = "less" , conf.level = .80)
abline(v = mean(cars1$hp))
library(ggplot2)
ggplot(cars1, aes(x=hp, y=mpg)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)
cdplot(genres ~ imdb_score,data=movie2) ## conditional densitie plot of Categorical response var vs continious predictor var