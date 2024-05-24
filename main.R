# import dataset
#rm(list=ls())
df <- read.csv('DatasetR_eng_fin.csv', header=TRUE, sep=';', 
               fileEncoding="UTF-8")
anyNA(df) # check missing values

# nick: Meaningful variables can be (based on the 'research' question):
# outcomeWL: crucial to understand which factors contribute to a win
# FieldAdvantage: could have a significant impact on the team's performance (historically the team playing home has
# a 'support advantege' given by the presence of local funs)
# BallPoss, Shots, ShotsOnT, PassAtt, PassSucc, TacklesW, TacklesAtt, AirDuelW, DribW (higher is better)
# YellowC, RedC, FoulsC, FoulsT (lower is better)

# nick: check if variable need standardization/normalization
str(df)       # Class of the dataframe


# Trasformare FieldAdvantage in variabile binaria
df$FieldAdvantage <- ifelse(df$FieldAdvantage == "H", 1, 
                            ifelse(df$FieldAdvantage == "A", 0, NA))


summary(df)

df$BallPoss <- as.numeric(gsub("%", "", gsub(",", ".", df$BallPoss))) / 100
df$ShotsPrec <- as.numeric(gsub("%", "", gsub(",", ".", df$ShotsPrec))) / 100
df$PassPrec <- as.numeric(gsub("%", "", gsub(",", ".", df$PassPrec))) / 100
df$TacklesWRatio <- as.numeric(gsub("%", "", gsub(",", ".", df$TacklesWRatio))) / 100
df$AirDuelW <- as.numeric(gsub("%", "", gsub(",", ".", df$AirDuelW))) / 100
df$DribWRatio <- as.numeric(gsub("%", "", gsub(",", ".", df$DribWRatio))) / 100
df$Knowledge <- as.numeric(gsub(",","", df$Knowledge))
summary(df)

df1 <- df[, c("FieldAdvantage", "PointsWon", "GoalsMade", "GoalsTaken", "BallPoss", "Shots", "ShotsOnT",
              "ShotsPrec", "PassAtt", "PassSucc", "PassPrec", "RosterQuality", "Knowledge", "YellowC", "RedC", 
              "FoulsC", "FoulsT", "TacklesWRatio", "AirDuelW", "DribW", "DribAtt", "DribWRatio")]

summary(df1)





###############################################################
## We can try to observe which variable is normally distributed
###############################################################
par(mfrow=c(2, 2))
qqnorm(df1$FieldAdvantage)
qqline(df1$FieldAdvantage, col = "red")

qqnorm(df1$PointsWon)
qqline(df1$PointsWon, col = "red")

qqnorm(df1$GoalsMade)
qqline(df1$GoalsMade, col = "red")

qqnorm(df1$GoalsTaken)
qqline(df1$GoalsTaken, col = "red")
par(mfrow=c(1, 1))


par(mfrow=c(2, 2))
qqnorm(df1$BallPoss)
qqline(df1$BallPoss, col = "red")

qqnorm(df1$Shots)
qqline(df1$Shots, col = "red")

qqnorm(df1$ShotsOnT)
qqline(df1$ShotsOnT, col = "red")

qqnorm(df1$ShotsPrec)
qqline(df1$ShotsPrec, col = "red")
par(mfrow=c(1, 1))


par(mfrow=c(2, 2))
qqnorm(df1$PassAtt)
qqline(df1$PassAtt, col = "red")

qqnorm(df1$PassSucc)
qqline(df1$PassSucc, col = "red")

qqnorm(df1$PassPrec)
qqline(df1$PassPrec, col = "red")

qqnorm(df1$RosterQuality)
qqline(df1$RosterQuality, col = "red")
par(mfrow=c(1, 1))


par(mfrow=c(2, 2))
qqnorm(df1$Knowledge)
qqline(df1$Knowledge, col = "red")

qqnorm(df1$YellowC)
qqline(df1$YellowC, col = "red")

qqnorm(df1$RedC)
qqline(df1$RedC, col = "red")

qqnorm(df1$FoulsC)
qqline(df1$FoulsC, col = "red")
par(mfrow=c(1, 1))


par(mfrow=c(2, 2))
qqnorm(df1$FoulsT)
qqline(df1$FoulsT, col = "red")

qqnorm(df1$TacklesWRatio)
qqline(df1$TacklesWRatio, col = "red")

qqnorm(df1$AirDuelW)
qqline(df1$AirDuelW, col = "red")

qqnorm(df1$DribW)
qqline(df1$DribW, col = "red")
par(mfrow=c(1, 1))


par(mfrow=c(2, 1))
qqnorm(df1$DribAtt)
qqline(df1$DribAtt, col = "red")

qqnorm(df1$DribWRatio)
qqline(df1$DribWRatio, col = "red")
par(mfrow=c(1, 1))



wins <- df1[df1$PointsWon == 3, ]
defeats <- df1[df1$PointsWon == 0, ]
draws <- df1[df1$PointsWon == 1, ]

#############################################
############# aggregate hists ###############
#############################################

barplot(table(df1$FieldAdvantage), col = "skyblue", main = "FieldAdvantage Bar Plot")
abline(v = mean(df1$FieldAdvantage), col = "red", lwd = 2)  
barplot(table(df1$PointsWon), col = "skyblue", main = "PointsWon Bar Plot")
abline(v = mean(df1$PointsWon), col = "red", lwd = 2)  
barplot(table(df1$GoalsMade), col = "skyblue", main = "GoalsMade Bar Plot")
abline(v = mean(df1$GoalsMade), col = "red", lwd = 2)  
barplot(table(df1$GoalsTaken), col = "skyblue", main = "GoalsTaken Bar Plot")
abline(v = mean(df1$GoalsTaken), col = "red", lwd = 2)
barplot(table(df1$YellowC), col = "skyblue", main = "YellowC Bar Plot")
abline(v = mean(df1$YellowC), col = "red", lwd = 2)


par(mfrow = c(2, 2))
hist(df1$BallPoss, col = "skyblue", border = "black", main = "BallPoss Histogram")
abline(v = mean(df1$BallPoss), col = "red", lwd = 2)
hist(df1$Shots, col = "skyblue", border = "black", main = "Shots Histogram")
abline(v = mean(df1$Shots), col = "red", lwd = 2)
hist(df1$ShotsOnT, col = "skyblue", border = "black", main = "ShotsOnT Histogram")
abline(v = mean(df1$ShotsOnT), col = "red", lwd = 2)
hist(df1$ShotsPrec, col = "skyblue", border = "black", main = "ShotsPrec Histogram")
abline(v = mean(df1$ShotsPrec), col = "red", lwd = 2)
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
hist(df1$PassAtt, col = "skyblue", border = "black", main = "PassAtt Histogram")
abline(v = mean(df1$PassAtt), col = "red", lwd = 2)
hist(df1$PassSucc, col = "skyblue", border = "black", main = "PassSucc Histogram")
abline(v = mean(df1$PassSucc), col = "red", lwd = 2)
hist(df1$PassPrec, col = "skyblue", border = "black", main = "PassPrec Histogram")
abline(v = mean(df1$PassPrec), col = "red", lwd = 2)
hist(df1$RosterQuality, col = "skyblue", border = "black", main = "RosterQuality Histogram")
abline(v = mean(df1$RosterQuality), col = "red", lwd = 2)
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
hist(df1$Knowledge, col = "skyblue", border = "black", main = "Knowledge Histogram") # not numeric
abline(v = mean(df$Knowledge), col = "red", lwd = 2) 
hist(df1$FoulsC, col = "skyblue", border = "black", main = "FoulsC Histogram")
abline(v = mean(df1$FoulsC), col = "red", lwd = 2)
hist(df1$FoulsT, col = "skyblue", border = "black", main = "FoulsT Histogram")
abline(v = mean(df1$FoulsT), col = "red", lwd = 2)
hist(df1$TacklesWRatio, col = "skyblue", border = "black", main = "TacklesWRatio Histogram")
abline(v = mean(df1$TacklesWRatio), col = "red", lwd = 2)
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
hist(df1$AirDuelW, col = "skyblue", border = "black", main = "AirDuelW Histogram")
abline(v = mean(df1$AirDuelW), col = "red", lwd = 2)
hist(df1$DribW, col = "skyblue", border = "black", main = "DribW Histogram")
abline(v = mean(df1$DribW), col = "red", lwd = 2)
hist(df1$DribAtt, col = "skyblue", border = "black", main = "DribAtt Histogram")
abline(v = mean(df1$DribAtt), col = "red", lwd = 2)
hist(df1$DribWRatio, col = "skyblue", border = "black", main = "DribWRatio Histogram")
abline(v = mean(df1$DribWRatio), col = "red", lwd = 2)
par(mfrow = c(1, 1))

#############################################
############# match winners hists ###############
#############################################

barplot(table(wins$FieldAdvantage), col = "skyblue", main = "FieldAdvantage Bar Plot")
abline(v = mean(wins$FieldAdvantage), col = "red", lwd = 2)  
barplot(table(wins$PointsWon), col = "skyblue", main = "PointsWon Bar Plot")
abline(v = mean(wins$PointsWon), col = "red", lwd = 2)  
barplot(table(wins$GoalsMade), col = "skyblue", main = "GoalsMade Bar Plot")
abline(v = mean(wins$GoalsMade), col = "red", lwd = 2)  
barplot(table(wins$GoalsTaken), col = "skyblue", main = "GoalsTaken Bar Plot")
abline(v = mean(wins$GoalsTaken), col = "red", lwd = 2)
barplot(table(wins$YellowC), col = "skyblue", main = "YellowC Bar Plot")
abline(v = mean(wins$YellowC), col = "red", lwd = 2)


par(mfrow = c(2, 2))
hist(wins$BallPoss, col = "skyblue", border = "black", main = "BallPoss Histogram")
abline(v = mean(wins$BallPoss), col = "red", lwd = 2)
hist(wins$Shots, col = "skyblue", border = "black", main = "Shots Histogram")
abline(v = mean(wins$Shots), col = "red", lwd = 2)
hist(wins$ShotsOnT, col = "skyblue", border = "black", main = "ShotsOnT Histogram")
abline(v = mean(wins$ShotsOnT), col = "red", lwd = 2)
hist(wins$ShotsPrec, col = "skyblue", border = "black", main = "ShotsPrec Histogram")
abline(v = mean(wins$ShotsPrec), col = "red", lwd = 2)
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
hist(wins$PassAtt, col = "skyblue", border = "black", main = "PassAtt Histogram")
abline(v = mean(wins$PassAtt), col = "red", lwd = 2)
hist(wins$PassSucc, col = "skyblue", border = "black", main = "PassSucc Histogram")
abline(v = mean(wins$PassSucc), col = "red", lwd = 2)
hist(wins$PassPrec, col = "skyblue", border = "black", main = "PassPrec Histogram")
abline(v = mean(wins$PassPrec), col = "red", lwd = 2)
hist(wins$RosterQuality, col = "skyblue", border = "black", main = "RosterQuality Histogram")
abline(v = mean(wins$RosterQuality), col = "red", lwd = 2)
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
hist(wins$Knowledge, col = "skyblue", border = "black", main = "Knowledge Histogram") # not numeric
abline(v = mean(wins$Knowledge), col = "red", lwd = 2) 
hist(wins$FoulsC, col = "skyblue", border = "black", main = "FoulsC Histogram")
abline(v = mean(wins$FoulsC), col = "red", lwd = 2)
hist(wins$FoulsT, col = "skyblue", border = "black", main = "FoulsT Histogram")
abline(v = mean(wins$FoulsT), col = "red", lwd = 2)
hist(wins$TacklesWRatio, col = "skyblue", border = "black", main = "TacklesWRatio Histogram")
abline(v = mean(wins$TacklesWRatio), col = "red", lwd = 2)
par(mfrow = c(1, 1))

par(mfrow = c(2, 2))
hist(wins$AirDuelW, col = "skyblue", border = "black", main = "AirDuelW Histogram")
abline(v = mean(wins$AirDuelW), col = "red", lwd = 2)
hist(wins$DribW, col = "skyblue", border = "black", main = "DribW Histogram")
abline(v = mean(wins$DribW), col = "red", lwd = 2)
hist(wins$DribAtt, col = "skyblue", border = "black", main = "DribAtt Histogram")
abline(v = mean(wins$DribAtt), col = "red", lwd = 2)
hist(wins$DribWRatio, col = "skyblue", border = "black", main = "DribWRatio Histogram")
abline(v = mean(wins$DribWRatio), col = "red", lwd = 2)
par(mfrow = c(1, 1))


#############################################
############# densities data ###############
############# winners vs others (defeats+draws) ###############
#############################################

win_1 <- df1[df1$PointsWon==3, "FieldAdvantage"]
win_0 <- df1[df1$PointsWon!=3, "FieldAdvantage"]

plot(density(win_0), col = "blue", main = "Density Plot of FieldAdvantage")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)


win_1 <- df1[df1$PointsWon==3, "GoalsMade"]
win_0 <- df1[df1$PointsWon!=3, "GoalsMade"]

plot(density(win_0), col = "blue", main = "Density Plot of GoalsMade")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)


win_1 <- df1[df1$PointsWon==3, "GoalsTaken"]
win_0 <- df1[df1$PointsWon!=3, "GoalsTaken"]

plot(density(win_0), col = "blue", main = "Density Plot of GoalsTaken")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)


win_1 <- df1[df1$PointsWon==3, "BallPoss"]
win_0 <- df1[df1$PointsWon!=3, "BallPoss"]

plot(density(win_0), col = "blue", main = "Density Plot of BallPoss")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)


win_1 <- df1[df1$PointsWon==3, "Shots"]
win_0 <- df1[df1$PointsWon!=3, "Shots"]

plot(density(win_0), col = "blue", main = "Density Plot of Shots")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)

win_1 <- df1[df1$PointsWon==3, "ShotsOnT"]
win_0 <- df1[df1$PointsWon!=3, "ShotsOnT"]

plot(density(win_0), col = "blue", main = "Density Plot of ShotsOnT")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)

win_1 <- df1[df1$PointsWon==3, "ShotsPrec"]
win_0 <- df1[df1$PointsWon!=3, "ShotsPrec"]

plot(density(win_0), col = "blue", main = "Density Plot of ShotsPrec")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)

win_1 <- df1[df1$PointsWon==3, "PassAtt"]
win_0 <- df1[df1$PointsWon!=3, "PassAtt"]

plot(density(win_0), col = "blue", main = "Density Plot of PassAtt")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)

win_1 <- df1[df1$PointsWon==3, "PassSucc"]
win_0 <- df1[df1$PointsWon!=3, "PassSucc"]

plot(density(win_0), col = "blue", main = "Density Plot of PassSucc")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)


win_1 <- df1[df1$PointsWon==3, "PassPrec"]
win_0 <- df1[df1$PointsWon!=3, "PassPrec"]

plot(density(win_0), col = "blue", main = "Density Plot of PassPrec")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)


win_1 <- df1[df1$PointsWon==3, "RosterQuality"]
win_0 <- df1[df1$PointsWon!=3, "RosterQuality"]

plot(density(win_0), col = "blue", main = "Density Plot of RosterQuality")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)

win_1 <- df1[df1$PointsWon==3, "Knowledge"]
win_0 <- df1[df1$PointsWon!=3, "Knowledge"]

plot(density(win_0), col = "blue", main = "Density Plot of Knowledge")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)

win_1 <- df1[df1$PointsWon==3, "YellowC"]
win_0 <- df1[df1$PointsWon!=3, "YellowC"]

plot(density(win_0), col = "blue", main = "Density Plot of YellowC")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)

win_1 <- df1[df1$PointsWon==3, "RedC"]
win_0 <- df1[df1$PointsWon!=3, "RedC"]

plot(density(win_0), col = "blue", main = "Density Plot of RedC")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)

win_1 <- df1[df1$PointsWon==3, "FoulsC"]
win_0 <- df1[df1$PointsWon!=3, "FoulsC"]

plot(density(win_0), col = "blue", main = "Density Plot of FoulsC")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)

win_1 <- df1[df1$PointsWon==3, "FoulsT"]
win_0 <- df1[df1$PointsWon!=3, "FoulsT"]

plot(density(win_0), col = "blue", main = "Density Plot of FoulsT")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)


win_1 <- df1[df1$PointsWon==3, "AirDuelW"]
win_0 <- df1[df1$PointsWon!=3, "AirDuelW"]

plot(density(win_0), col = "blue", main = "Density Plot of AirDuelW")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)


win_1 <- df1[df1$PointsWon==3, "DribW"]
win_0 <- df1[df1$PointsWon!=3, "DribW"]

plot(density(win_0), col = "blue", main = "Density Plot of DribW")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)


win_1 <- df1[df1$PointsWon==3, "DribAtt"]
win_0 <- df1[df1$PointsWon!=3, "DribAtt"]

plot(density(win_0), col = "blue", main = "Density Plot of DribAtt")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)



win_1 <- df1[df1$PointsWon==3, "DribWRatio"]
win_0 <- df1[df1$PointsWon!=3, "DribWRatio"]

plot(density(win_0), col = "blue", main = "Density Plot of DribWRatio")
lines(density(win_1), col = "red")
legend("topright", legend = c("Not win", "Win"), col = c("blue", "red"), lty = 1)


# Boxplots
win_box <- df1[df1$PointsWon==3, ]
not_win_box <- df1[df1$PointsWon!=3, ]


par(mfrow=c(1, 2))  
boxplot(win_box$FieldAdvantage, main = "FieldAdvantage Boxplot")
boxplot(not_win_box$FieldAdvantage, main = "FieldAdvantage Boxplot")
par(mfrow=c(1, 1))  

par(mfrow=c(1, 2))  
boxplot(win_box$PointsWon, main = "PointsWon Boxplot")
boxplot(not_win_box$PointsWon, main = "PointsWon Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))  
boxplot(win_box$GoalsMade, main = "GoalsMade Boxplot")
boxplot(not_win_box$GoalsMade, main = "GoalsMade Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$GoalsTaken, main = "GoalsTaken Boxplot")
boxplot(not_win_box$GoalsTaken, main = "GoalsTaken Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$BallPoss, main = "BallPoss Boxplot")
boxplot(not_win_box$BallPoss, main = "BallPoss Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$Shots, main = "Shots Boxplot")
boxplot(not_win_box$Shots, main = "Shots Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$ShotsOnT, main = "ShotsOnT Boxplot")
boxplot(not_win_box$ShotsOnT, main = "ShotsOnT Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$ShotsPrec, main = "ShotsPrec Boxplot")
boxplot(not_win_box$ShotsPrec, main = "ShotsPrec Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$PassAtt, main = "PassAtt Boxplot")
boxplot(not_win_box$PassAtt, main = "PassAtt Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$PassSucc, main = "PassSucc Boxplot")
boxplot(not_win_box$PassSucc, main = "PassSucc Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$PassPrec, main = "PassPrec Boxplot")
boxplot(not_win_box$PassPrec, main = "PassPrec Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$RosterQuality, main = "RosterQuality Boxplot")
boxplot(not_win_box$RosterQuality, main = "RosterQuality Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$Knowledge, main = "Knowledge Boxplot")
boxplot(not_win_box$Knowledge, main = "Knowledge Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$YellowC, main = "YellowC Boxplot")
boxplot(not_win_box$YellowC, main = "YellowC Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$YellowC, main = "YellowC Boxplot")
boxplot(not_win_box$YellowC, main = "YellowC Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$RedC, main = "RedC Boxplot")
boxplot(not_win_box$RedC, main = "RedC Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$FoulsC, main = "FoulsC Boxplot")
boxplot(not_win_box$FoulsC, main = "FoulsC Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$FoulsT, main = "FoulsT Boxplot")
boxplot(not_win_box$FoulsT, main = "FoulsT Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$TacklesWRatio, main = "TacklesWRatio Boxplot")
boxplot(not_win_box$TacklesWRatio, main = "TacklesWRatio Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$AirDuelW, main = "AirDuelW Boxplot")
boxplot(not_win_box$AirDuelW, main = "AirDuelW Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$DribW, main = "DribW Boxplot")
boxplot(not_win_box$DribW, main = "DribW Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$DribAtt, main = "DribAtt Boxplot")
boxplot(not_win_box$DribAtt, main = "DribAtt Boxplot")
par(mfrow=c(1, 1))

par(mfrow=c(1, 2))
boxplot(win_box$DribWRatio, main = "DribWRatio Boxplot")
boxplot(not_win_box$DribWRatio, main = "DribWRatio Boxplot")
par(mfrow=c(1, 1))

############## Boxplot of 'winners' group vs 'not winners' (draw+defeats) ############

# wins vs draws vs defeats
boxplot(wins$GoalsMade, draws$GoalsMade, defeats$GoalsMade, names = c("Wins", "Draws", "Defeats"),
        xlab = "Groups", ylab = "Goals Made"
        )
grid()

# wins vs others
boxplot(wins$GoalsMade, draws$GoalsMade+defeats$GoalsMade, names = c("Wins", "Draws+Defeats"),
        xlab = "Groups", ylab = "Goals Made"
)
grid()





# Correlation Analysis
correlation_matrix <- cor(df1)
correlation_matrix

# Step 3: Define a Custom Red Color Palette
red_palette <- colorRampPalette(c("white", "red"))(64)


heatmap(correlation_matrix, Rowv = NA, Colv = NA, col = heat.colors(24), scale = "none", margins = c(5, 10))

library(corrplot)

selected_corr <- cor(df1)

# Collinearity matrix
corrplot(selected_corr,
         method = "number",
         diag = FALSE,
         tl.cex = 0.8,
         number.cex = 0.65,
         tl.col = "black")

###### Visual check of variable correlation
plot(df1$BallPoss, df1$PassAtt) # strong positive correlation
plot(df1$GoalsMade, df1$ShotsPrec) # don't know how to interpret it (maybe due to a integer value vs percentage)
########## scatterplot to understand it better:
plot(df1$GoalsMade, df1$ShotsPrec,
     main = "Scatterplot GoalsMade vs ShotsPrec",
     xlab = "Goals Made",
     ylab = "Shots Precision",
     pch = 19, col = "darkorange", cex = 0.7)
grid()

plot(df1$BallPoss, df1$Shots) # slightly positive
plot(df1$BallPoss, df1$PassSucc)# strong positive correlation

plot(df1$Shots, df1$PassPrec) # positive? ----> scattplot
plot(df1$Shots, df1$PassPrec,
     main = "Scatterplot Shots vs PassPrec",
     xlab = "Number of shots",
     ylab = "Passage precision",
     pch = 19, col = "darkorange", cex = 0.7)
grid()

plot(df1$ShotsOnT, df1$GoalsMade) #?
plot(df1$GoalsMade, df1$ShotsOnT,
     main = "GoalsMade Shots vs ShotsOnT",
     xlab = "Goals made",
     ylab = "Shots on target",
     pch = 19, col = "darkorange", cex = 0.7)
grid() ### With 10 shots on target i can expect from 2 to 3 goals

plot(df1$FoulsC, df1$PassAtt) #?
plot(df1$PassAtt, df1$FoulsC,
     main = "Passages Attempted vs Fouls Committed",
     xlab = "Attempted passeges",
     ylab = "Fouls committed",
     pch = 19, col = "darkorange", cex = 0.7)
grid()

plot(df1$FoulsC, df1$BallPoss,
     main = "FoulsC vs BallPoss",
     xlab = "FoulsC",
     ylab = "BallPoss",
     pch = 19, col = "darkorange", cex = 0.7)
grid()





##### regression diagnostic ######
mod.out1 <- lm(df1$ShotsPrec~df1$PassPrec)
par(mfrow=c(2,2))
plot(mod.out1)
par(mfrow=c(1,1))


# count the number of outliers
# Define a function to count outliers using the IQR method
count_outliers <- function(x) {
  Q <- quantile(x, probs = c(0.25, 0.75))
  IQR <- Q[2] - Q[1]
  lower_bound <- Q[1] - 1.5 * IQR
  upper_bound <- Q[2] + 1.5 * IQR
  outliers <- x < lower_bound | x > upper_bound
  return(sum(outliers))
}

# Apply the function to each column of your dataframe
outlier_counts <- sapply(df, function(col) {
  if(is.numeric(col)) {
    count_outliers(col)
  } else {
    NA  # For non-numeric columns
  }
})
# Display the counts of outliers for each variable
print(outlier_counts)



head(df)



# Single linear regressions
cor(df$BallPoss, df$Shots)

mod.out <- lm(BallPoss ~ Shots, data=df)
summary(mod.out)
plot(df$Shots, df$BallPoss, pch=20, xlab="Shots", ylab="Ball Possession")
abline(mod.out, col="red", lwd=2)

plot(mod.out$residuals, main="Residuals Plot", ylab="Residuals", xlab="Fitted Values")
abline(h=0, col="red")


mod.out <- lm(ShotsPrec ~ GoalsMade, data=df)
summary(mod.out)
plot(df$GoalsMade, df$ShotsPrec, pch=20, xlab="GoalsMade", ylab="ShotsPrec")
abline(mod.out, col="red", lwd=2)

plot(mod.out$residuals, main="Residuals Plot", ylab="Residuals", xlab="Fitted Values")
abline(h=0, col="red")

mod.out <- lm(GoalsMade ~ OutcomeWL, data=df)
summary(mod.out)
plot(df$OutcomeWL, df$GoalsMade, pch=20, xlab="OutcomeWL", ylab="GoalsMade")
abline(mod.out, col="red", lwd=2)

plot(mod.out$residuals, main="Residuals Plot", ylab="Residuals", xlab="Fitted Values")
abline(h=0, col="red")

# Multiple linear regressions

# Reminder: Pr(>|t|)=p-value denotes the quality of hypothesis
# High meaningful p-value < 0.001 "***"
# High meaningful 0.001 < p-value < 0.01 "**"
# Slightly meaningful 0.01 < p-value < 0.05 "*"
# Marginally meaningful 0.05 < p-value < 0.1 "."
# not meaningful p-value >= 0.1 " "

# Reminder 2: R^2 denotes the percentage of variability of Y (response)

# Multiple regression 1
# OutcomeWL = b_0 + b_1 * BallPoss + b_2 * ShotPrec + b_3 * PassPrec


mod.multi <- lm(OutcomeWL ~ BallPoss + ShotsPrec + PassPrec, data=df)
summary(mod.multi)

par(mfrow=c(2, 2))  # imposta l'area di plotting per mostrare 4 grafici
plot(mod.multi)


# Multiple regression 1
# OutcomeWL = b_0 + b_1 * BallPoss + b_2 * ShotPrec + b_3 * PassPrec



























#----------------------------------------------------------------------------------------------------------------------------------
# create a new column that identifies matches from the "winner - 1" and "control - 0" groups
df$Group <- ifelse(grepl("Ca", df$ID), "1",
                    ifelse(grepl("Co", df$ID), "0", NA))
df$Group <- as.integer(df$Group)

# let's see what data types we have in the columns:
str(df)

# we notice that percentages variables are stored as characters. we would like them to be
# represented as numerical [0,1] 
df$BallPoss <- as.numeric(sub(",", ".", sub("%", "", df$BallPoss))) / 100
df$ShotsPrec <- as.numeric(sub(",", ".", sub("%", "", df$ShotsPrec))) / 100
df$PassPrec <- as.numeric(sub(",", ".", sub("%", "", df$PassPrec))) / 100
df$TacklesWRatio <- as.numeric(sub(",", ".", sub("%", "", df$TacklesWRatio))) / 100
df$AirDuelW <- as.numeric(sub(",", ".", sub("%", "", df$AirDuelW))) / 100
df$DribWRatio <- as.numeric(sub(",", ".", sub("%", "", df$DribWRatio))) / 100

# the Knowledge column is a chr as well, let's handle it
df$Knowledge <- as.numeric(sub(",", ".", df$Knowledge))

# columns ID and match are not useful for our model, let's remove them
df$ID <- NULL
df$Match <- NULL
# we notice that the "result" column is redundant wrt goals made and taken, we remove it
df$Result <- NULL

# Let's do some label encoding. We want in FieldAdvantage column, to show home matches as 1 and
# Away matches as 0
df$FieldAdvantage <- ifelse(df$FieldAdvantage == "H", 1, 
                            ifelse(df$FieldAdvantage == "A", 0, NA))
# we want losses as -1, wins as 1 and draws as 0 in OutcomeWL column. This column can be redundant
# wrt to Points won. We will think about it later
df$OutcomeWL <- ifelse(df$OutcomeWL == "W", 1, 
                            ifelse(df$OutcomeWL == "D", 0, 
                                   ifelse(df$OutcomeWL == "L", -1, NA)))

# Double check every column is now numerical 
str(df)

summary(df)

# check if the target variable is balanced
prop.table(table(df$OutcomeWL))

df$OutcomeWL[df$OutcomeWL == -1] <- 0


# Let's see which features are correlated with each other:
S <- round(cov(df), 4)
P <- round(cor(df), 4)

# We use a heatmap to explore the correlations
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = P, col = col, symm = TRUE)

df_winners <- df[df$Group == 1, ]
df_winners$Group <- NULL
df_control <- df[df$Group == 0, ]
df_control$Group <- NULL

P_win <- round(cor(df_winners), 4)
P_cont <- round(cor(df_control), 4)

heatmap(x = P_win, col = col, symm = TRUE, main = 'P_win')
heatmap(x = P_cont, col = col, symm = TRUE, main = 'P_cont')



save(df, file = '/Users/nico/Desktop/football_data_/processed_data.RData')

