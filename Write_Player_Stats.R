#After reading in the day's matches, this code is intended to extract individual stats for each player participating in club matches
setwd("C:/Users/przyb/Dropbox/Backgammon Club")
options(stringsAsFactors = FALSE)
library(pracma)

All_Matches <- read.csv("All_Matches.csv", header = TRUE)
participants <- unique(c(All_Matches$Winner, All_Matches$Loser))
participants <- sort(participants)
N <- length(unique(participants))
#Create Individual Stats
Player_Stats <- data.frame(matrix(data = NA, nrow = N, ncol = 20))
colnames(Player_Stats) <- c("Points", "1st Pl", "2nd Pl", "3rd Pl", "4th Pl","App", "Wins", "Losses", "Pct", "FIBS", "Game_Wins", "Game_Loss",  "Game_Pct", "Pts_Earned", "Pts_Allowed","Experience", "Save_Chances", "Save_Pct", "Comeback_Chances", "Comeback_Pct")
rownames(Player_Stats) <- participants

#Run the FIBS_tally script
source("Tally_FIBS.R")

#Player_Stats$Player <- participants
for (player in participants){
player_data <- All_Matches[All_Matches$Winner == player | All_Matches$Loser == player,]
player_wins <- player_data[player_data$Winner == player,]
player_losses <- player_data[player_data$Loser == player,]
Player_Stats[player, "App"] <- length(unique(player_data$Date))
FP_wins <- player_wins[player_wins$Round == "Final",]
Player_Stats[player, "1st Pl"] <- length(unique(FP_wins$Date))
RunnerUps <- player_losses[player_losses$Round == "Final",]
Player_Stats[player, "2nd Pl"] <- length(unique(RunnerUps$Date))
TP_wins <- player_wins[player_wins$Round == "3rd Pl",]
Player_Stats[player, "3rd Pl"] <- length(unique(TP_wins$Date))
FourthP <- player_wins[player_losses$Round == "3rd Pl",]
Player_Stats[player, "4th Pl"] <- length(unique(FourthP$Date))
Player_Stats[player, "Wins"] <- sum(player_data$Winner== player)
Player_Stats[player, "Losses"] <- sum(player_data$Loser == player)

Player_Stats[player, "Pct"] <- sum(player_data$Winner== player)/length(player_data$Winner)
Player_Stats[player, "Game_Wins"] <- sum(player_wins$Winner_Games)+sum(player_losses$Loser_Games)
Player_Stats[player, "Game_Loss"] <- sum(player_wins$Loser_Games)+sum(player_losses$Winner_Games)
Player_Stats[player, "Game_Pct"] <- Player_Stats[player, "Game_Wins"]/sum(player_data$Total_Games)
Player_Stats[player, "Pts_Earned"] <- sum(player_wins$Winner_Score)+sum(player_losses$Loser_Score)
Player_Stats[player, "Pts_Allowed"] <- sum(player_wins$Loser_Score)+sum(player_losses$Winner_Score)
Player_Stats[player, "Experience"] <- sum(player_data$Length)
Player_Stats[player, "Save_Chances"] <- sum(player_data$Crawford == player)
if (is.na(Player_Stats[player, "Save_Chances"])){
  next
}
if (Player_Stats[player,"Save_Chances"]==0){
  Player_Stats[player, "Save_Pct"]<-0
}else {
Player_Stats[player, "Save_Pct"] <- sum(player_wins$Crawford== player)/sum(player_data$Crawford != "none")}
Player_Stats[player,"Comeback_Chances"] <- sum(player_data$Crawford != player & player_data$Crawford != "none")
if (is.na(Player_Stats[player, "Comeback_Chances"])){
  next
}
if (Player_Stats[player,"Comeback_Chances"] == 0){
  Player_Stats[player, "Comeback_Pct"] <- 0
  }else {
    Player_Stats[player, "Comeback_Pct"] <- sum(player_wins$Crawford != player & player_wins$Crawford != "none")/Player_Stats[player,"Comeback_Chances"]
  }}

#Update annual points based on finishes:
Finish_Mat <- as.matrix(Player_Stats[,2:6])
Player_Stats$Points <- Finish_Mat %*%c(7,5,4,2,1)

write.table(Player_Stats, file = "Player_Stats.csv", row.names = TRUE, col.names = TRUE, sep = ",")
