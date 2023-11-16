library(dplyr)
library(ggplot2)
library(ggrepel)
library(caret)
library(FNN)
library(purrr)

"We'll first read in Trackman data from 2021 to 2023 data to create a df 'grouped_data',
which finds wOBAcon values based on launch angle and exit speed across all the data.
If you already have the 2021_2023_wobacon.csv file you can skip ahead to line 57"

# 2021 Data
data21 <- read.csv("Trackman_2021_All.csv")
data21 <- data21[data21$Level == 'D1',]
data21$ExitSpeed_int <- round(data21$ExitSpeed)
data21$Angle_int <- round(data21$Angle)
data21$Direction_int <- round(data21$Direction)
data21$PitchCall <- toupper(data21$PitchCall)

# 2022 Data
data22 <- read.csv("Trackman_2022_All.csv")
data22 <- data22[data22$Level == 'D1',]
data22$ExitSpeed_int <- round(data22$ExitSpeed)
data22$Angle_int <- round(data22$Angle)
data22$Direction_int <- round(data22$Direction)
data22$PitchCall <- toupper(data22$PitchCall)

# 2023 Data
data23 <- read.csv("Trackman_2023_All.csv")
data23 <- data23[data23$Level == 'D1',]
data23$ExitSpeed_int <- round(data23$ExitSpeed)
data23$Angle_int <- round(data23$Angle)
data23$Direction_int <- round(data23$Direction)
data23$PitchCall <- toupper(data23$PitchCall)

# Combined Data
combined = rbind(data21, data22, data23)

# Lets first visualize league wOBAcon vs Launch Angle and Exit Speed
grouped_data = combined %>% 
  filter(PitchCall == "INPLAY", ExitSpeed_int != 'NA', Angle_int != 'NA') %>%
  group_by(ExitSpeed_int, Angle_int) %>%
  summarise(num_occurrences = n(),
            single = length(which(PlayResult == "Single")),
            double = length(which(PlayResult == "Double")),
            triple = length(which(PlayResult == "Triple")),
            hr = length(which(PlayResult == "HomeRun"))
            
  ) %>%
  summarise(ExitSpeed_int, Angle_int, single, double, triple, hr, num_occurrences, 
            wOBAcon = (w1B*single + w2B*double + w3B*triple + wHR*hr) / num_occurrences)

#write.csv(grouped_data, "C:\\Users\\phill\\OneDrive\\Desktop\\ExpStatistics\\2021_2023_wobacon.csv")

#####################################################################################################

"If you have the 2021_2023_wobacon.csv file, you can start running the code from here"

grouped_data = read.csv("2021_2023_wobacon.csv")

# New Trackman data we will predict on. This should be a large sample size of data (e.g. full season)
# To obtain useful results
new_data <- read.csv("new_data.csv")
new_data <- new_data[new_data$Level == 'D1',]
new_data$ExitSpeed_int <- round(new_data$ExitSpeed)
new_data$Angle_int <- round(new_data$Angle)
new_data$Direction_int <- round(new_data$Direction)
new_data$PitchCall <- toupper(new_data$PitchCall)

"The first thing we'll do is calculate linear weights. This code is currently a work in progress
so I have elected to hard code these weights for 2021-2023 data"
# RAA relative to outs 2021-2023
wuBB = 0.592186463
wHBP = 0.799806269
w1B = 0.917605278
w2B = 1.181858374
w3B = 1.476140438
wHR = 1.740373848

# Calculate Linear weights by finding the wOBA scale (league OBP / league wOBA with RAA rel to outs)
woba_scale = 1.095

# Linear Weights
wuBB <- wuBB * woba_scale
wHBP <- wHBP * woba_scale
w1B <- w1B * woba_scale
w2B <- w2B * woba_scale
w3B <- w3B * woba_scale
wHR <- wHR * woba_scale

# The 'xwOBA' function calculates xwOBA for a specific hitter by averaging the xwOBA of all his batted balls (xwOBAcon)
# Then factoring in strikeouts, walks, HBP, etc. weighted by their linear weights to get the true xwOBA

xwOBA <- function(batter_name, df, df_with_xwobacon) {
  bb_events = df_with_xwobacon %>%
    filter(Batter == batter_name)
  KorBBorHBP = df %>%
    filter(Batter == batter_name)

  k_total = length(which(KorBBorHBP$KorBB == 'Strikeout'))
  bb_total = length(which(KorBBorHBP$KorBB == 'Walk'))
  hbp_total = length(which(KorBBorHBP$PitchCall == 'HITBYPITCH'))
  total_pas = nrow(bb_events) + k_total + bb_total + hbp_total
  if (total_pas >= 75) {
    xwOBA_val = round((mean(bb_events$xwOBAcon) * nrow(bb_events) + wuBB * bb_total + wHBP * hbp_total) / total_pas, digits = 3)

    return(xwOBA_val) } else {
      return('DNQ')

    }
}

# Similarly, the 'wOBA' function calculates wOBA for a specific hitter by using the traditional formula for wOBA
# and the linear weights above

wOBA <- function(batter_name, df) {
  woba_data <- df %>%
    filter(Batter == batter_name)

  bip = length(which(woba_data$PitchCall == 'INPLAY'))

  sac_bunts = 0
  for (i in 1:nrow(woba_data)) {
    if (woba_data$PlayResult[i] == 'Sacrifice' && woba_data$TaggedHitType[i] == 'Bunt') {
      sac_bunts = sac_bunts + 1
    }
  }
  
  k_total = length(which(woba_data$KorBB == 'Strikeout'))
  bb_total = length(which(woba_data$KorBB == 'Walk'))
  hbp_total = length(which(woba_data$PitchCall == 'HITBYPITCH'))
  total_pas = bip + k_total + bb_total + hbp_total
  
  sing = length(which(woba_data$PlayResult == "Single"))
  doub = length(which(woba_data$PlayResult == "Double"))
  trip = length(which(woba_data$PlayResult == "Triple"))
  hrun = length(which(woba_data$PlayResult == "HomeRun"))

  if(total_pas >= 75) {
    wOBA_val = round((wuBB * bb_total + wHBP * hbp_total + w1B * sing + w2B * doub +
                      w3B * trip + wHR * hrun) / (total_pas - sac_bunts), digits = 3)

    return(wOBA_val) } else {
      return('DNQ')

    }
}

# Plot wOBAcon vs Launch Angle & Exit Speed
woba_plot <- ggplot(grouped_data, aes(x = ExitSpeed_int, y = Angle_int, color = wOBAcon)) + geom_point()
woba_plot + scale_color_gradient2(midpoint = max(grouped_data$wOBAcon)/2, low="blue", mid = "white", high="red") + labs(x = 'Exit Speed', y = 'Launch Angle', color = 'wOBAcon', title = 'wOBAcon vs Launch Angle & Exit Speed (2021-2023)') + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=c(0, 25, 50, 75, 100, 125)) + scale_y_continuous(breaks=c(-75, -50, -25, 0, 25, 50, 75))

# Now perform KNN-Regression to generate a model for predicting xwOBAcon.
# To do so, we first split grouped_data into a training set to build our model on
# and a testing set to test its accuracy.
set.seed(12)
indexes = createDataPartition(grouped_data$wOBAcon, p = .75, list = F)
train = grouped_data[indexes, ]
test = grouped_data[-indexes, ]

train_x = train[, 1:2]
preProc_train = preProcess(train_x, method =c("center", "scale"))
train_x_scaled = predict(preProc_train, train_x)
train_y = train[,8]

test_x = test[, 1:2]
test_x_scaled = predict(preProc_train, test_x)
test_y = test[,8]

best_k <- NULL
best_rmse <- 999999
rmse_values <- c()

# This for loop iterates through different values of K from 1 to 100 and finds the K which 
# Minimizes the prediction error (RMSE)
for (k in 1:100) {
  knnmodel = knnreg(train_x_scaled, as.numeric(unlist(train_y)), k = k)
  
  pred_y = predict(knnmodel, data.frame(test_x_scaled))
  rmse = sqrt(mean((as.numeric(unlist(test_y)) - pred_y)^2))
  rmse_values = append(rmse_values, rmse)
  
  if (rmse < best_rmse) {
    best_k = k
    best_rmse = rmse
  }
}

plot(1:100, rmse_values, type = 'l')

# Using the best K, we build our knn regression model
knnmodel = knnreg(train_x_scaled, as.numeric(unlist(train_y)), k = best_k)

# Now we can predict xwOBAcon for a specific exit speed and launch angle
point = predict(preProc_train, data.frame(ExitSpeed_int = c(108), Angle_int = c(14)))
pred_spec = predict(knnmodel, point)

# Let's also predict for all values in test_x
pred_y = predict(knnmodel, data.frame(test_x_scaled))
rmse = sqrt(mean((as.numeric(unlist(test_y)) - pred_y)^2))

# Plot xwOBAcon (pred_y) vs Launch Angle & Exit Speed
xwoba_plot <- ggplot(data = test_x, aes(x = ExitSpeed_int, y = Angle_int, color = pred_y)) + geom_point()
xwoba_plot + scale_color_gradient2(midpoint = max(pred_y)/2, low="blue", mid = "white", high="red") + labs(x = 'Exit Speed', y = 'Launch Angle', color = 'xwOBAcon', title = 'xwOBAcon vs Launch Angle & Exit Speed (2021-2023)') + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=c(0, 25, 50, 75, 100, 125)) + scale_y_continuous(breaks=c(-75, -50, -25, 0, 25, 50, 75))

# Finally, plot all values for the xwOBAcon model
all_data = grouped_data[,1:2]
all_data_scaled = predict(preProc_train, all_data)

pred_y = predict(knnmodel, data.frame(all_data_scaled))
rmse = sqrt(mean((as.numeric(unlist(grouped_data[,8])) - pred_y)^2))

xwoba_plot <- ggplot(data = all_data, aes(x = ExitSpeed_int, y = Angle_int, color = pred_y)) + geom_point()
xwoba_plot + scale_color_gradient2(midpoint = max(pred_y)/2, low="blue", mid = "white", high="red") + labs(x = 'Exit Speed', y = 'Launch Angle', color = 'xwOBAcon', title = 'xwOBAcon vs Launch Angle & Exit Speed (2021-2023)') + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=c(0, 25, 50, 75, 100, 125)) + scale_y_continuous(breaks=c(-75, -50, -25, 0, 25, 50, 75))

# Now that we have a model to build xwOBAcon on, we'll introduce a new column, xwOBAcon, to our "new_data" df
# that assigns the xwOBAcon of a particular batted ball profile To each ball in play in the "mutated_data" df

mutated_data = new_data %>%
  filter(PitchCall == "INPLAY", ExitSpeed_int != 'NA', Angle_int != 'NA') %>%
  mutate(
    xwOBAcon = predict(knnmodel, predict(preProc_train, data.frame(ExitSpeed_int, Angle_int)))
  )
#write.csv(mutated_data, "new_xwobacon.csv")

'*****************************************************************************'

"You may enter the name of a hitter below in the format 'Lastname, Firstname' 
and run the xwOBA and wOBA functions to calculate his xwOBA and wOBA on the season"

batter_name = 'Bost, Austin'

cat("Batter: ", batter_name, "/ wOBA: ", wOBA(batter_name, new_data),
    "/ xwOBA: ", xwOBA(batter_name, new_data, mutated_data))


batters_all = unique(mutated_data$Batter)

# Plot xwOBA vs wOBA
xwOBA_all <- map(batters_all, ~ xwOBA(.x, new_data, mutated_data))
xwOBA_all <- as.numeric(unlist(xwOBA_all))
wOBA_all <- map(batters_all, ~wOBA(.x, new_data))
wOBA_all <- as.numeric(unlist(wOBA_all))

wOBA_xwOBA = data.frame(cbind(batters_all, wOBA_all, xwOBA_all))
wOBA_xwOBA <- na.omit(wOBA_xwOBA)
wOBA_xwOBA$dist_to_line <- abs((-1)*as.numeric(wOBA_xwOBA$wOBA_all) + as.numeric(wOBA_xwOBA$xwOBA_all)) / sqrt(2)
wOBA_xwOBA <- wOBA_xwOBA[order(wOBA_xwOBA$dist_to_line, decreasing = TRUE), ]
rownames(wOBA_xwOBA) <- 1:nrow(wOBA_xwOBA)

wOBA_xwOBA_under <- wOBA_xwOBA[wOBA_xwOBA$wOBA_all<=wOBA_xwOBA$xwOBA_all,][1:5,]
wOBA_xwOBA_over <- wOBA_xwOBA[wOBA_xwOBA$wOBA_all>wOBA_xwOBA$xwOBA_all,][1:5,]
wOBA_xwOBA_outliers <- rbind(wOBA_xwOBA_under, wOBA_xwOBA_over)
rownames(wOBA_xwOBA_outliers) <- 1:nrow(wOBA_xwOBA_outliers)

# The points highlighted in blue are the top 5 overperformers/underperformers
xwoba_woba_plot <- ggplot(data = wOBA_xwOBA, aes(x = as.numeric(wOBA_all), y = as.numeric(xwOBA_all), 
                                         color = ifelse(wOBA_xwOBA$batters_all %in% wOBA_xwOBA_outliers$batters_all, 'red', 'blue'))) + geom_point()
xwoba_woba_plot  + labs(x = 'wOBA', y = 'xwOBA', color = '', title = 'xwOBA vs wOBA (2023)') + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(color = "none") + geom_abline(slope = 1, intercept = 0, color = 'blue') +
  geom_label_repel(aes(label = ifelse(wOBA_xwOBA$batters_all %in% wOBA_xwOBA_outliers$batters_all, as.character(wOBA_xwOBA$batters_all), '')),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   segment.size  = 0.2,
                   min.segment.length = 1)
