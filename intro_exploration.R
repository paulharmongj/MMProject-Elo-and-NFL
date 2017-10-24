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


#Plot of Denver Elos
plot(x_np$date[x_np$team == "DEN"],x_np$elo[x_np$team == "DEN"], type = "p", xlab = "Year", ylab = "ELO", pch = 20, col = "orange2")
library(mgcv)
den.dat <- x_np[x_np$team == "DEN",]
gam1 <- gam(elo ~ s(as.numeric(date)), data = den.dat)
lines(den.dat$date, predict(gam1), color = "blue3", lwd = 2)
title("ELO RATINGS FOR DENVER BRONCOS")


















