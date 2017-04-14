#FB analytics using R

require('Rfacebook')

#connect our R session with our test app and 
#authenticate it to our Facebook Profile for Data Mining

fb_oauth <- fbOAuth(app_id="1847751152164460", app_secret="1d209dbff68e1f0f1e5904182edd9849",extended_permissions = TRUE)

#saving fb_auth object for later times
save(fb_oauth, file="fb_oauth")

load("fb_oauth")

#another mannual token-vaidity- 2 hrs
token<-'EAACEdEose0cBACI8UQVaNSCTdys5uPc1Rzew4d2ujqQfSBsCePxeop7PoWqZCnJroP7X5sM6jVgv6mjUJ6vSfNXiHFKuR4WsPZBwfn30iJN0tsRh4xPBaeeOgUkjxh4V6weQ7E4kGg607Eu9YCuFO7ybxsetLwIJ7DHNm81k5lkVhZAjF6ZCvu760rbdXGIZD'

#getting my profile 
me<-getUsers("me",token,private_info = T)



#getting list of pages i have liked
my_likes <- getLikes(user="me", token=fb_oauth)


#only 5 of my friends use FB app service
my_friends <- getFriends(token, simplify = TRUE)


my_friends_info <- getUsers(my_friends$id, token, private_info = TRUE)


table(my_friends_info$gender  )


mat <- getNetwork(token, format = "adj.matrix")


ggplot(aes(x = locale),data = my_friends_info) + 
  geom_bar() + 
  coord_flip()
