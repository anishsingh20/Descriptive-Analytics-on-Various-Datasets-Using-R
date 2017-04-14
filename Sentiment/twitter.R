#SENTIMENTAL ANALYSIS- ANALYSIS OF PEOPLE's SENTIMENTS -HOW SAD,HAPPY,ANGRY etc a person 
# is due to something, mining the reviews of a product etc etc,a person's mood , opinions on something

#loading the package
library(twitteR)

#doing authentications and loading tokens from twitter
api_key <- "hvaamlxy730zlRDXqLnkYa2N5"

api_secret <- "Wi5zYV7txtro3lhm4M1k3xxgGMV86NwADjKlgWbQgod7IBRSfI"

access_token <- "	830571150036103168-R1Ly6IGNcbb9WQ3cWQSxxcWn01e6DXX"

access_token_secret <- "gYTOmsRO6GGNJV5NQWc0SZ1xrEty3wIaOOV5I6OdZxobM"

#twitter oauth
setup_twitter_oauth(api_key,api_secret)


#getting 100 tweets mentioning '@jetairways'
#it returns a list of tweets
airline<-searchTwitter('@jetairways', n =100)

length(airline)
#extracting the tweets 
#getting the username who made the tweet
airline[[100]]$getScreenName() # tweet of twitter of class 'status'


airline.text<-lapply(airline , function(t) t$getText() )

airline.text[2]
 







searchTwitter("iphone")