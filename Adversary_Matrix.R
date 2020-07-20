#Once the data base of All_Matches is ready, we may compute a matrix that displays the series record between each pair of players

All_Matches <- read.csv("All_Matches.csv", header = TRUE)
participants <- unique(c(All_Matches$Winner, All_Matches$Loser))
participants <- sort(participants)
initials <- function(full.name) {
  # Returns initials of a full name
  # Input will contain only letters (uppercase and/or lowercase) plus 
  # single spaces between words. Folks like Joseph Gordon-Levitt, 
  # Conan O’Brien, and David J. Malan won’t be using your program. (If only!)
  if (nchar(full.name) == 0) {
    stop ("Valid name please")
  }
  isspace  <- integer(0)
  fn.split <- unlist(strsplit(full.name, fixed = TRUE, split = ""))
  isspace  <- which(fn.split == " ")
  init     <- toupper(fn.split[c(1, (isspace+1))])
  paste(init, collapse = "")
}
part_initials <- lapply(participants, initials)
N <- length(participants)
Adv_Record <- as.data.frame(matrix(data = NA, nrow = N, ncol = N), row.names = participants)
colnames(Adv_Record) <- part_initials
Adv_Perc <- as.data.frame(matrix(data = NA, nrow = N, ncol = N), row.names = participants)
colnames(Adv_Perc) <- part_initials
for (n in 1:N){
  player <- participants[n]
  player_data <- All_Matches[All_Matches$Winner == player | All_Matches$Loser == player,]
  Adv_ind <- 1:N
  Adv_ind <- Adv_ind[-n]
  for (m in Adv_ind){
    Adv <- participants[m]
    matchups <- player_data[player_data$Winner == Adv | player_data$Loser == Adv,]
    if (nrow(matchups) == 0){
      next
    }
    Wins <- sum(matchups$Winner == player)
    Losses <- sum(matchups$Loser == player)
    Adv_Record[n,m] <- paste(Wins,"--",Losses)
    Adv_Perc[n,m] <- Wins/(Wins+Losses)
}}

write.table(Adv_Record, file = "Adversary_Records.csv", row.names = TRUE, col.names = TRUE, sep = ",")
write.table(Adv_Perc, file = "Adversary_Percentages.csv", row.names = TRUE, col.names = TRUE, sep = ",")

