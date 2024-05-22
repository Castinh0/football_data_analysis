# import dataset

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
# Example: Preprocessing percentage variables
df$BallPoss <- as.numeric(gsub("%", "", gsub(",", ".", df$BallPoss))) / 100
df$ShotsPrec <- as.numeric(gsub("%", "", gsub(",", ".", df$ShotsPrec))) / 100
df$PassPrec <- as.numeric(gsub("%", "", gsub(",", ".", df$PassPrec))) / 100
df$TacklesWRatio <- as.numeric(gsub("%", "", gsub(",", ".", df$TacklesWRatio))) / 100
df$AirDuelW <- as.numeric(gsub("%", "", gsub(",", ".", df$AirDuelW))) / 100
df$DribWRatio <- as.numeric(gsub("%", "", gsub(",", ".", df$DribWRatio))) / 100

# Encoding FieldAdvantage
df$FieldAdvantage <- ifelse(df$FieldAdvantage == "H", 1, 
                            ifelse(df$FieldAdvantage == "A", 0, NA))

# Encoding OutcomeWL
df$OutcomeWL <- ifelse(df$OutcomeWL == "W", 3, 
                       ifelse(df$OutcomeWL == "D", 1, 
                              ifelse(df$OutcomeWL == "L", 0, NA)))

str(df)


par(mfrow=c(2, 2))  # Arrange plots in a 2x2 grid

barplot(table(df$OutcomeWL), col = "skyblue", main = "OutcomeWL Bar Plot")
barplot(table(df$FieldAdvantage), col = "skyblue", main = "FieldAdvantage Bar Plot")
barplot(table(df$BallPoss), col = "skyblue", main = "BallPoss Bar Plot")
barplot(table(df$Shots), col = "skyblue", main = "Shots Bar Plot")
abline(v = mean(df$Shots), col = "red", lwd = 2)  # Add mean line
par(mfrow=c(1, 1))

par(mfrow=c(2, 2)) 
hist(df$OutcomeWL, col = "skyblue", border = "black", main = "OutcomeWL Histogram")
hist(df$FieldAdvantage, col = "skyblue", border = "black", main = "FieldAdvantage Histogram")
abline(v = mean(df$OutcomeWL), col = "red", lwd = 2)  # Add mean line
hist(df$BallPoss, col = "skyblue", border = "black", main = "BallPoss Histogram")
abline(v = mean(df$BallPoss), col = "red", lwd = 2)  # Add mean line
hist(df$Shots, col = "skyblue", border = "black", main = "Shots Histogram")
abline(v = mean(df$Shots), col = "red", lwd = 2)  # Add mean line
par(mfrow=c(1, 1))

par(mfrow=c(2, 2))
hist(df$ShotsOnT, col = "skyblue", border = "black", main = "ShotsOnT Histogram")
abline(v = mean(df$ShotsOnT), col = "red", lwd = 2)  # Add mean line
hist(df$ShotsPrec, col = "skyblue", border = "black", main = "ShotsPrec Histogram")
abline(v = mean(df$ShotsPrec), col = "red", lwd = 2)  # Add mean line
hist(df$PassAtt, col = "skyblue", border = "black", main = "PassAtt Histogram")
abline(v = mean(df$PassAtt), col = "red", lwd = 2)  # Add mean line
hist(df$PassSucc, col = "skyblue", border = "black", main = "PassSucc Histogram")
abline(v = mean(df$PassSucc), col = "red", lwd = 2)  # Add mean line
par(mfrow=c(1, 1))

par(mfrow=c(2, 2))
hist(df$PassPrec, col = "skyblue", border = "black", main = "PassPrec Histogram")
abline(v = mean(df$PassPrec), col = "red", lwd = 2)  # Add mean line
hist(df$TacklesW, col = "skyblue", border = "black", main = "TacklesW Histogram")
abline(v = mean(df$TacklesW), col = "red", lwd = 2)  # Add mean line
hist(df$TacklesAtt, col = "skyblue", border = "black", main = "TacklesAtt Histogram")
abline(v = mean(df$TacklesAtt), col = "red", lwd = 2)  # Add mean line
hist(df$AirDuelW, col = "skyblue", border = "black", main = "AirDuelW Histogram")
abline(v = mean(df$AirDuelW), col = "red", lwd = 2)  # Add mean line
par(mfrow=c(1, 1))
# Reset graphics parameters
par(mfrow=c(1, 1))  # Reset to default single plot layout


summary_df <- data.frame(
  Variable = c("OutcomeWL", "FieldAdvantage", "BallPoss", "Shots", "ShotsOnT",
               "PassAtt", "PassSucc", "TacklesW", "TacklesAtt", "AirDuelW", "DribW"),
  Mean = sapply(df[, c("OutcomeWL", "FieldAdvantage", "BallPoss", "Shots", "ShotsOnT",
                       "PassAtt", "PassSucc", "TacklesW", "TacklesAtt", "AirDuelW", "DribW")], mean),
  Median = sapply(df[, c("OutcomeWL", "FieldAdvantage", "BallPoss", "Shots", "ShotsOnT",
                         "PassAtt", "PassSucc", "TacklesW", "TacklesAtt", "AirDuelW", "DribW")], median),
  SD = sapply(df[, c("OutcomeWL", "FieldAdvantage", "BallPoss", "Shots", "ShotsOnT",
                     "PassAtt", "PassSucc", "TacklesW", "TacklesAtt", "AirDuelW", "DribW")], sd),
  Min = sapply(df[, c("OutcomeWL", "FieldAdvantage", "BallPoss", "Shots", "ShotsOnT",
                      "PassAtt", "PassSucc", "TacklesW", "TacklesAtt", "AirDuelW", "DribW")], min),
  Max = sapply(df[, c("OutcomeWL", "FieldAdvantage", "BallPoss", "Shots", "ShotsOnT",
                      "PassAtt", "PassSucc", "TacklesW", "TacklesAtt", "AirDuelW", "DribW")], max)
)
summary_df


# Boxplots
par(mfrow=c(2, 2))  # Arrange plots in a 4x3 grid
boxplot(df$OutcomeWL, main = "OutcomeWL Boxplot")
boxplot(df$FieldAdvantage, main = "OutcomeWL Boxplot")
boxplot(df$BallPoss, main = "OutcomeWL Boxplot")
boxplot(df$Shots, main = "OutcomeWL Boxplot")
par(mfrow=c(1, 1))  

par(mfrow=c(2, 2))  # Arrange plots in a 4x3 grid
boxplot(df$ShotsOnT, main = "OutcomeWL Boxplot")
boxplot(df$PassAtt, main = "OutcomeWL Boxplot")
boxplot(df$PassSucc, main = "OutcomeWL Boxplot")
boxplot(df$TacklesW, main = "OutcomeWL Boxplot")
par(mfrow=c(1, 1))  

par(mfrow=c(2,2))
boxplot(df$TacklesAtt, main = "OutcomeWL Boxplot")
boxplot(df$AirDuelW, main = "OutcomeWL Boxplot")
boxplot(df$DribW, main = "OutcomeWL Boxplot")
par(mfrow=c(1, 1))  


# Correlation Analysis
correlation_matrix <- cor(df[, c("OutcomeWL", "FieldAdvantage", "BallPoss", "Shots", "ShotsOnT",
                                 "PassAtt", "PassSucc", "TacklesW", "TacklesAtt", "AirDuelW", "DribW")])
correlation_matrix

heatmap(correlation_matrix, 
        main = "Correlation Matrix",
        xlab = "Variables",
        ylab = "Variables",
        col = colorRampPalette(c("blue", "white", "red"))(100))

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

# from the hists, PassAtt and PassSucc look high varying on scale (standardization),
# furthermore shots on target seems to has not relevant influence on the winning so i want try to normalize it.

df$PassAtt_Standardized <- scale(df$PassAtt)
df$PassSucc_Standardized <- scale(df$PassSucc)

# Standardize and normalize ShotsOnT
df$ShotsOnT_Standardized <- scale(df$ShotsOnT)
df$ShotsOnT_Normalized <- (df$ShotsOnT_Standardized - min(df$ShotsOnT_Standardized)) / (max(df$ShotsOnT_Standardized) - min(df$ShotsOnT_Standardized))

colnames(df)

df_standardized <- df

# Replace the original columns with their standardized versions
df_standardized$PassAtt <- df$PassAtt_Standardized
df_standardized$PassSucc <- df$PassSucc_Standardized
df_standardized$ShotsOnT <- df$ShotsOnT_Normalized


head(df)

# let's now see the heat map with the standardized variables

correlation_matrix_std <- cor(df_standardized[, c("OutcomeWL", "FieldAdvantage", "BallPoss", "Shots", "ShotsOnT_Standardized", "ShotsOnT_Normalized",
                                 "PassAtt_Standardized", "PassSucc_Standardized", "TacklesW", "TacklesAtt", "AirDuelW", "DribW")])
correlation_matrix_std

heatmap(correlation_matrix_std, 
        main = "Correlation Matrix",
        xlab = "Variables",
        ylab = "Variables",
        col = colorRampPalette(c("blue", "white", "red"))(100))


# Step 1: Extract prefix and number
df$ID_Type <- substr(df$ID, 1, 4)
df$ID_Number <- as.integer(substr(df$ID, 5, nchar(df$ID)))

# Step 2: Calculate total points for each ID type
total_points <- tapply(df$PointsWon, df$ID_Type, sum)

# Step 3: Merge total points back into the original dataset
df$Total_Points <- total_points[df$ID_Type]


numeric_cols <- sapply(df, is.numeric)
numeric_df <- df[, numeric_cols]

cor_matrix <- cor(numeric_df)

# Step 3: Define a Custom Red Color Palette
red_palette <- colorRampPalette(c("white", "red"))(4)


heatmap(cor_matrix, Rowv = NA, Colv = NA, col = red_palette, scale = "none", margins = c(5, 10))

# Considerations:
# total points have a strong correlation with: RoosterQuality (4 out of 4)
# Total_Points is also correlated with: ShotsOnT, PassSucc, PassAtt, PassPrec, DribAtt, DribWin, BallPoss (3 out of 4)
# It is obviously medium-high correlated (3 out of 4) with PointsWon, OutcomeWL but it is irrelevant
# interesting to note that FieldAdvantage, Takles and Fouls (made and against) have small correlation 

head(df)

count_wins <- function(outcome_wl) {
  return(sum(outcome_wl == 3))
}

# Extract the ID type from the ID column
df$ID_type <- substr(df$ID, 1, 4)

counts <- tapply(df$OutcomeWL, df$ID_type, count_wins)

# Add the counts as a new variable to the dataframe
df$wins <- counts[df$ID_type]

tail(df)


numeric_cols <- sapply(df, is.numeric)
numeric_df <- df[, numeric_cols]

cor_matrix <- cor(numeric_df)

# Step 3: Define a Custom Red Color Palette
red_palette <- colorRampPalette(c("white", "red"))(4)


heatmap(cor_matrix, Rowv = NA, Colv = NA, col = red_palette, scale = "none", margins = c(5, 10))


df$win <- ifelse(df$OutcomeWL == 3, 1, 0)

head(df)

# Split the data based on the win variable
win_0 <- df[df$win == 0, "ShotsOnT_Normalized"]
win_1 <- df[df$win == 1, "ShotsOnT_Normalized"]

# Create a density plot for ShotsOnT
plot(density(win_0), col = "blue", main = "Density Plot of ShotsOnT by Win")
lines(density(win_1), col = "red")
legend("topright", legend = c("win=0", "win=1"), col = c("blue", "red"), lty = 1)

# Split the data based on the win variable
win_0 <- df[df$win == 0, "BallPoss"]
win_1 <- df[df$win == 1, "BallPoss"]

# Create a density plot for BallPoss
plot(density(win_0), col = "blue", main = "Density Plot of ShotsOnT by Win")
lines(density(win_1), col = "red")
legend("topright", legend = c("win=0", "win=1"), col = c("blue", "red"), lty = 1)

# Split the data based on the win variable
win_0 <- df[df$win == 0, "RosterQuality"]
win_1 <- df[df$win == 1, "RosterQuality"]

# Create a density plot for RosterQuality
plot(density(win_0), col = "blue", main = "Density Plot of ShotsOnT by Win")
lines(density(win_1), col = "red")
legend("topright", legend = c("win=0", "win=1"), col = c("blue", "red"), lty = 1)

# Normalization of RoasterQuality
df$RosterQuality_std <- scale(df$RosterQuality)
df$RosterQuality_norm <- (df$RosterQuality_std - min(df$RosterQuality_std)) / (max(df$RosterQuality_std) - min(df$RosterQuality_std))

win_0 <- df[df$win == 0, "RosterQuality_norm"]
win_1 <- df[df$win == 1, "RosterQuality_norm"]

# Create a density plot for RosterQuality_norm
plot(density(win_0), col = "blue", main = "Density Plot of ShotsOnT by Win")
lines(density(win_1), col = "red")
legend("topright", legend = c("win=0", "win=1"), col = c("blue", "red"), lty = 1)


numeric_cols <- sapply(df, is.numeric)
numeric_df <- df[, numeric_cols]

cor_matrix <- cor(numeric_df)

# Step 3: Define a Custom Red Color Palette
red_palette <- colorRampPalette(c("white", "red"))(4)


heatmap(cor_matrix, Rowv = NA, Colv = NA, col = red_palette, scale = "none", margins = c(5, 10))



# Collinearity matrix
library(corrplot)

selected_vars <- c("FieldAdvantage", "OutcomeWL", "GoalsMade", "GoalsTaken", "BallPoss", "Shots", "ShotsPrec",
                   "PassPrec", "Knowledge", "TacklesWRatio", "DribWRatio", "RosterQuality_std",
                   "Total_Points", "wins", "win")

numeric_cols <- selected_vars[sapply(selected_vars, function(var) is.numeric(df[[var]]))]
selected_corr <- cor(df[, numeric_cols])

corrplot(selected_corr,
         method = "number",
         diag = FALSE,
         tl.cex = 0.8,
         number.cex = 0.6,
         tl.col = "black")


str(df)

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

