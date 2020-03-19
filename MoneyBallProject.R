batting <- read.csv('Batting.csv')
str(batting)
head(batting$AB)
head(batting$X2B)
#formula
batting$BA <- batting$H / batting$AB
tail(batting$BA,5)
# On Base Percentage
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)
# Creating X1B (Singles)
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR
# Creating Slugging Average (SLG)
batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB
str(batting)
sal <- read.csv('Salaries.csv')
summary(batting)
batting <- subset(batting,yearID >= 1985)
summary(batting)
combo <- merge(batting,sal,by=c('playerID','yearID'))
summary(combo)
#lost players
lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01') )
lost_players
lost_players <- subset(lost_players,yearID == 2001)
#Reduce the lost_players data frame to the following columns: playerID,H,X2B,X3B,HR,OBP,SLG,BA,AB
lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]
head(lost_players)
#replacement players constraints
#The total combined salary of the three players can not exceed 15 million dollars.
#Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
#Their mean OBP had to equal to or greater than the mean OBP of the lost players

#First only grab available players from year 2001
library(dplyr)
avail.players <- filter(combo,yearID==2001)
#plot to see where I should cut-off for salary in respect to OBP
library(ggplot2)
ggplot(avail.players,aes(x=OBP,y=salary)) + geom_point()
#choose 8000000 as a cut off point
avail.players <- filter(avail.players,salary<8000000,OBP>0)
#The total AB of the lost players is 1469. This is about 1500, probably cut off my avail.players at 1500/3= 500 AB.
avail.players <- filter(avail.players,AB >= 500)
#sort by OBP
possible <- head(arrange(avail.players,desc(OBP)),10)
possible <- possible[,c('playerID','OBP','AB','salary')]
possible
#Can't choose giambja again, but the other ones look good (2-4)
possible[2:4,]