options(stringsAsFactors = FALSE)

#This script computes FIBS for the Player_Stats
N <- length(rownames(All_Matches))
FIBS_tally <- data.frame(matrix(data = 0, nrow = N+1, ncol = length(row.names(Player_Stats))))
colnames(FIBS_tally) <- rownames(Player_Stats)
Exp_tally <- data.frame(matrix(data = 0, nrow =N+1, ncol = length(row.names(Player_Stats))))
colnames(Exp_tally)<- rownames(Player_Stats)
FIBS_tally[1,] <- 1500
for (n in 1:N){
  Winner <- All_Matches$Winner[n]
  Loser <- All_Matches$Loser[n]
  WRtg <- FIBS_tally[n,Winner]
  LRtg <- FIBS_tally[n,Loser]
  Diff <- abs(WRtg-LRtg)
  Len <- All_Matches$Length[n]  
  WExp <- Exp_tally[n,Winner] + Len
  LExp <- Exp_tally[n,Loser] + Len
  WExF <- max(1,(5-(WExp+Len)/100))
  LExF <- max(1,(5-(LExp+Len)/100))
  U <- 1/(10^(Diff*sqrt(Len)/2000)+1)  
  F <- 1-U  
  if (WRtg >=LRtg ){
    WRtg <- WRtg + 4*WExF*sqrt(Len)*U
    LRtg <- LRtg - 4*LExF*sqrt(Len)*U
  }else {
    WRtg<- WRtg + 4*WExF*sqrt(Len)*F
    LRtg <- LRtg - 4*LExF*sqrt(Len)*F
  }
  NewFIBS <- FIBS_tally[n,]
  NewFIBS[,Winner] <- WRtg  
  NewFIBS[,Loser]<- LRtg  
  FIBS_tally[n+1,] <- NewFIBS  
  Exp_tally[n+1, Winner] <- WExp
  Exp_tally[n+1, Loser] <- LExp
}
Player_Stats$FIBS <- t(FIBS_tally[N+1,])
