#intro script and exploratory analysis

#gets the data from GITHUB
library(readr)
x <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/nfl-elo-game/master/data/nfl_games.csv")

library(dplyr)

x_new <- dplyr::filter(x, season > 2001) #filters out anything before 2002, when Texans joined the league
den <- dplyr::filter(x, team1 == "DEN" | team2 == "DEN") # 


#visualize league histories
library(ggplot2)
ggplot(den) + geom_point(aes(x = 1:nrow(den),y = elo1), color = "orange") + geom_line(aes(x = 1:nrow(den),y = elo1), color = "blue3") 


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

title("ELO RATINGS FOR DENVER BRONCOS")


#some commentary: it looks like these are, in a certain sense, marginal models for the average
#elo of the team during a given year. 
x_np <- filter(x_np, team %in%c("DEN","OAK","DAL"))
ggplot(x_np) + geom_line(aes(x = as.numeric(date), y = elo)) + facet_grid(facets = ~season)













