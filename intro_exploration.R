#intro script and exploratory analysis

#gets the data from GITHUB
library(readr)
x <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/nfl-elo-game/master/data/nfl_games.csv")

library(dplyr)

#Don't use this code, it only gets the home games played
x_new <- dplyr::filter(x, season > 2001) #filters out anything before 2002, when Texans joined the league
den <- dplyr::filter(x_new, team1 == "DEN")


#visualize league histories but only for home games
library(ggplot2);library(ggthemes)
ggplot(den) + geom_point(aes(x = 1:nrow(den),y = elo1), color = "orange", size =) + 
  geom_line(aes(x = 1:nrow(den),y = elo1), color = "blue3") + 
  xlab("Game") + ylab("Elo Rating") + theme_economist_white()


#lets see if we can get the data into the right form.
#want TEAM, GAME, ELO

#mutate some new columns
x_1 <- x_new[,c("date","season","team1","elo1","playoff")]
x_2 <- x_new[,c("date","season","team2","elo2","playoff")]
names(x_1) <- names(x_2) <- c("date","season","team","elo","playoff")
x_full <- rbind(x_1,x_2)

#remove playoff games
x_np <- arrange(dplyr::filter(x_full, playoff == 0), date)
x_np$teamyear <- interaction(x_np$season,x_np$team)

#Plot of Denver Elos
plot(x_np$date[x_np$team == "DEN"],x_np$elo[x_np$team == "DEN"], type = "p", xlab = "Year", ylab = "ELO", pch = 20, col = "orange2")
title("Denver Broncos Elos since 2002")

library(mgcv)
den.dat <- x_np[x_np$team == "DEN",]
gam1 <- gam(elo ~ s(as.numeric(date), k = 16), data = den.dat) #tensor product
gam2 <- gam(elo ~ s(as.numeric(date), k = 16, bs = "cs"), data = den.dat) #cubicshrinkage
gam3 <- gam(elo ~ s(as.numeric(date), k = 16, bs = "ds"), data = den.dat)
gam4 <- gam(elo ~ s(as.numeric(date), k = 16, bs = "cr"), data = den.dat)

gamm1 <- gamm(elo ~ s(as.numeric(date), k = 16, bs = "cs"), random = , data = den.dat)

lines(den.dat$date, predict(gam1), col = "blue3", lwd = 2)
lines(den.dat$date, predict(gam2), col = "red", lwd = 2, lty = 3)
lines(den.dat$date, predict(gam3), col = "pink", lwd = 2, lty = 4)
lines(den.dat$date, predict(gam4), col = "purple", lwd = 2, lty = 2)

legend('bottomright', legend = c("TP","CS","DS","CR"), fill = c("blue3","red","pink","purple"))

title("Long-Term Trend:ELO RATINGS FOR DENVER BRONCOS")


#some commentary: it looks like these are, in a certain sense, marginal models for the average
#elo of the team during a given year. 
x_DOD <- filter(x_np, team %in%c("DEN","OAK","DAL"))
ggplot(x_DOD) + geom_line(aes(x = as.numeric(date), y = elo)) + facet_grid(facets = ~season)


#try and produce a matplot



##Aggregate by team and get mean elos
mean_elos <- aggregate(x_np$elo, by = list(factor(x_np$team)),mean)

sd_elos <- aggregate(x_np$elo, by = list(factor(x_np$team)),sd)

library(xtable)
elo.df <- cbind(mean_elos,sd_elos[,2])
xtable(elo.df)
pander(elo.df)


#Try to create a "long" dataset
#Structure - Observations of every game for each year for each team as rows
x_sort <- arrange(x_np, teamyear) #sort so I can add a game indicator by team/game
x_sort$GAME <- rep(c(1:16),32*15) #adds a game indicator for each variable

write.csv(x_sort,"elo.long.csv")
#that game variable will become a column
library(tidyr)
x_wide <- x_sort[,c(2,3,4,7)]%>% spread(key = GAME, value = elo) #check it out I put in a pipe operator!
#we have a wide dataset, can i fit gam to each season


#some quick plots of GAMs for the presentation
ggplot(filter(x_sort, teamyear %in% c('2012.DEN','2007.NE','2008.DET','2016.NE'))) + 
  geom_line(aes(x = GAME, y = elo, group = teamyear,col = teamyear),lwd = 2) +
  theme_bw() + xlab("Game") + ylab("Elo Rating") + ggtitle("Three Teams' Elo Ratings") + 
  scale_color_manual(values=c("orange", "lightblue", "red","blue4"))








game <- 1:16

x <- gam(unlist(x_wide[1,-c(1,2)])~s(game, k=4), bs = 'cr')


#let's try a couple of different functions for this: 


gam_Wide <- function(vector, gam.type = 'cr', max.df = 16){ 

gam_Wide <- function(vector, gam.type = 'cs', max.df = 16){
  #create game variable
  game <- 1:16
  #takes a wide dataset (remove name and year first)
  x <- gam(unlist(vector) ~ s(game, k = max.df))
  
return(x)}


gam_list <- apply(x_wide[,-c(1,2)],1,gam_Wide) #returns a list of the stuff you need
names(gam_list) <- interaction(x_wide$season,x_wide$team)

x_wide$teamyear <- interaction(x_wide$season,x_wide$team)

#some quick plots of GAMs for the presentation
ggplot(filter(x_sort, teamyear %in% c('2012.DEN','2007.NE','2008.DET','2016.NE'))) + 
  geom_line(aes(x = GAME, y = elo, group = teamyear,col = teamyear),lwd = 2) +
  theme_bw() + xlab("Game") + ylab("Elo Rating") + ggtitle("Three Teams' Elo Ratings") + 
  scale_color_manual(values=c("orange", "lightblue", "red","blue4"))





x_wide$teamyear <- interaction(x_wide$season,x_wide$team)

#let's make a data frame of predicted gams
pred_mat <- matrix(0, nrow = length(gam_list), ncol = 16)
for(j in 1:length(gam_list)){
pred_mat[j,] <- predict(gam_list[[j]])  
}
rownames(pred_mat) <- names(gam_list)




#create dissimilarity matrix via R-package (although below might be interesting as well)
#uses L2 norm (Euclidean distances) but I'd be curious about Mahalonobis distances as well
library(fda.usc)
gam_distances <- metric.lp(pred_mat)

#let's go ahead and try some hierarchical clustering
library(mclust)


#is there a way to do medoid-based clustering?
library(cluster)
pam1 <- pam(as.dist(gam_distances), metric = "euclidean", k = 4)

ggplot(filter(x_sort, teamyear %in% pam1$medoids)) + 
  geom_line(aes(x = GAME, y = elo, group = teamyear,col = teamyear),lwd = 2) +
  theme_bw() + xlab("Game") + ylab("Elo Rating") + ggtitle("Three Teams' Elo Ratings") + 
  scale_color_manual(values=c("orange", "lightblue", "red","blue4"))




#hierarchical clustering 
CF <- hclust(as.dist(gam_distances), method = 'ward.D2')
plot(CF)

tree_cut <- cutree(CF, k = 4)
table(tree_cut)

#let's take a look at representatives from each cluster 

which(tree_cut == 1)[1]
which(tree_cut == 2)[1]
which(tree_cut == 3)[1]
which(tree_cut == 4)[1]

#make a quick data frame
which(rownames(pred_mat)== '2002.CAR')
which(rownames(pred_mat)== '2002.ATL')
which(rownames(pred_mat)== '2002.ARI')
which(rownames(pred_mat)== '2002.GB')

#make a data.frame
car02 <- pred_mat[5,]
atl02 <- pred_mat[2,]
ari02 <- pred_mat[1,]
gb02 <- pred_mat[12,]
df.reps <- data.frame(cbind(car02,atl02,ari02,gb02,game))


ggplot(df.reps) + geom_line(aes(game,car02),col = "red",size =2) + geom_line(aes(game,atl02),col = "orange",size = 2) + geom_line(aes(game,ari02),col = "green",size =2) + 
  geom_line(aes(game,gb02),col = "blue2", size = 2) + ggtitle("Representative Teams") + 
  theme_economist_white() + ylab("Elo Rating") + scale_x_continuous("Game",breaks = 1:16)

ggplot(filter(x_))





#OK, now we have gams for each team in each year. Can we estimate dissimilarities?
#it would be nice to get the square root of the integrated distance: 
#ie. sqrt[(int1..16(fit.1 -fit.2))^2]

preds <- predict(gam_list[[1]])
plot(1:16, preds, ylim = c(0, max(preds)), type = 'l')
x <- runif(10000,0,16)
y <- runif(10000,0,1500)
points(x,y)

#do it via monte carlo integration
MC_integrate <- function(gam.fit){
  runif()
}

#wait, there might be an easier way to do this than how I'm trying to do it
metric.lp(predict(gam_list[[1]]), predict(gam_list[[2]]))


##FOR KICKS, TRY FITTING MIXED MODELS
#want data in the long form so go with x_np
library(lme4)
lmer1 <- lmer(elo ~ 1 + (1|season), data = x_np)
#note that this really isn't the question of interest

#produce plots of gamms
library(mgcv)
season.list <- list("season")
gamm1 <- gamm(elo ~ team, random = list(season = ~1), data = x_np)
plot(fitted(gamm1$lme),resid(gamm1$lme),col = factor(x_np$team)) 
#fits the fitted values based solely on team

#model-based clustring based on each gam fit
library(mclust)
gam_funk <- function(vec){
  elo <- as.numeric(vec[-c(1,2)]) #removes the year and team name
  time <- seq(1,16,by =1)
  predicted <- predict(gam(elo ~ s(time,bs = 'cs')))
return(list(pred = predicted, gam = gam(elo ~ s(time,bs = 'cs'))))}

pred.gam <- apply(x_wide,1,gam_funk)



