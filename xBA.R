# You'll need to install these libraries to RStudio using install.packages(" "), entering the name of each
# Library in the quotations
library(dplyr)
library(ggplot2)
library(ggrepel)
library(caret)
library(FNN)
library(purrr)

"Regardless of whether running the model with or without spray angle, run the 2 code blocks
and two functions below first"

# This is the data we will build the model on (e.g. 2022 Trackman Data)
data <- read.csv("Trackman_model.csv")
data$ExitSpeed_int <- round(data$ExitSpeed)
data$Angle_int <- round(data$Angle)
data$Direction_int <- round(data$Direction)

# This is the data we will predict on (e.g. 2023 Trackman Data)
new_data <- read.csv("Trackman_predict.csv")
new_data$ExitSpeed_int <- round(new_data$ExitSpeed)
new_data$Angle_int <- round(new_data$Angle)
new_data$Direction_int <- round(new_data$Direction)

# The 'xBA' function calculates xBA for a specific hitter by averaging the xBA of all his batted balls (xBACON)
# Then factoring in strikeouts to get the true xBA

xBA <- function(batter_name, df, df_with_ba) {
  bb_events = df_with_ba %>%
    filter(Batter == batter_name)
  strikeouts = df %>%
    filter(Batter == batter_name)
  
  k_total = length(which(strikeouts$KorBB == 'Strikeout'))
  total_abs = nrow(bb_events) + k_total
  if (total_abs >= 75) {
    xBA_val = round(mean(bb_events$xBA) / (1 + k_total/total_abs), digits = 3)
    
    return(xBA_val) } else {
      return('DNQ')
      
    }
}

# Similarly, the 'BA' function calculates BA for a specific hitter by totaling all of his hits and dividing this
# by his total number of at bats (BIP + K - SH)

BA <- function(batter_name, df) {
  ba_data <- df %>%
    filter(Batter == batter_name)
  
  bip = length(which(ba_data$PitchCall == 'InPlay'))
  
  sac_bunts = 0
  for (i in 1:nrow(ba_data)) {
    if (ba_data$PlayResult[i] == 'Sacrifice' && ba_data$TaggedHitType[i] == 'Bunt') {
      sac_bunts = sac_bunts + 1
    }
  }
  
  hits = length(which(ba_data$PlayResult == "Single" | ba_data$PlayResult == "Double" | ba_data$PlayResult == "Triple" | ba_data$PlayResult == "HomeRun"))
  k_total = length(which(ba_data$KorBB == 'Strikeout'))
  
  if(bip + k_total >= 75) {
    BA_val = round(hits / (bip - sac_bunts + k_total), digits = 3)
    
    return(BA_val) } else {
      return('DNQ')
      
    }
}

###################################################################

"xBA using Launch Angle and Exit Speed"

# Lets first visualize league BA vs Launch Angle and Exit Speed
grouped_data = data %>% 
  filter(PitchCall == "InPlay", ExitSpeed_int != 'NA', Angle_int != 'NA') %>%
  group_by(ExitSpeed_int, Angle_int) %>%
  summarise(num_occurrences = n(),
            hits = length(which(PlayResult == "Single" | PlayResult == "Double" | PlayResult == "Triple" | PlayResult == "HomeRun")),
  ) %>%
  summarise(ExitSpeed_int, Angle_int, hits, num_occurrences, BA = hits / num_occurrences)

View(grouped_data)

# Plot BA vs Launch Angle & Exit Speed
ba_plot <- ggplot(grouped_data, aes(x = ExitSpeed_int, y = Angle_int, color = BA)) + geom_point()
ba_plot + scale_color_gradient(low="blue", high="red") + labs(x = 'Exit Speed', y = 'Launch Angle', title = 'BA vs Launch Angle & Exit Speed (2022)') + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=c(0, 25, 50, 75, 100, 125)) + scale_y_continuous(breaks=c(-75, -50, -25, 0, 25, 50, 75))

# Now perform KNN-Regression to generate a model for predicting xBA.
# To do so, we first split grouped_data into a training set to build our model on
# and a testing set to test its accuracy.
set.seed(12)
indexes = createDataPartition(grouped_data$BA, p = .75, list = F)
train = grouped_data[indexes, ]
test = grouped_data[-indexes, ]

train_x = train[, 1:2]
preProc_train = preProcess(train_x, method =c("center", "scale"))
train_x_scaled = predict(preProc_train, train_x)
train_y = train[,5]

test_x = test[, 1:2]
test_x_scaled = predict(preProc_train, test_x)
test_y = test[,5]

best_k <- NULL
best_rmse <- 999999
rmse_values <- c()

# This for loop iterates through different values of K from 1 to 100 and finds the K which 
# Minimizes the prediction error (RMSE)
# You can adjust the range of K if its clear the optimal K isn't in this range
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

# Now we can predict xBA for a specific exit speed and launch angle
point = predict(preProc_train, data.frame(ExitSpeed_int = c(108), Angle_int = c(14)))
pred_spec = predict(knnmodel, point)

# Let's also predict for all values in test_x
pred_y = predict(knnmodel, data.frame(test_x_scaled))
rmse = sqrt(mean((as.numeric(unlist(test_y)) - pred_y)^2))

# Plot xBA (pred_y) vs Launch Angle & Exit Speed
ba_plot <- ggplot(data = test_x, aes(x = ExitSpeed_int, y = Angle_int, color = pred_y)) + geom_point()
ba_plot + scale_color_gradient(low="blue", high="red") + labs(x = 'Exit Speed', y = 'Launch Angle', color = 'xBA', title = 'xBA vs Launch Angle & Exit Speed (2022)') + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=c(0, 25, 50, 75, 100, 125)) + scale_y_continuous(breaks=c(-75, -50, -25, 0, 25, 50, 75))

# Each point on the plot comes from the test data and represents the xBA for a batted ball 
# with that particular exit speed and launch angle

##################################################################

# Now that we have a model to build xBA on, we'll update the introduce a new column, xBA, to new_data
# that assigns the xBA of a particular batted ball profile To each ball in play in the "mutated_data" df

mutated_data = new_data %>%
  filter(PitchCall == "InPlay", ExitSpeed_int != 'NA', Angle_int != 'NA') %>%
  mutate(
    xBA = predict(knnmodel, predict(preProc_train, data.frame(ExitSpeed_int, Angle_int)))
  )

# Gets the name of each unique batter in the dataset
batters_all = unique(mutated_data$Batter)

# Plot xBA vs BA
xBA_all <- map(batters_all, ~ xBA(.x, new_data, mutated_data))
xBA_all <- as.numeric(unlist(xBA_all))
BA_all <- map(batters_all, ~BA(.x, new_data))
BA_all <- as.numeric(unlist(BA_all))

BA_xBA = data.frame(cbind(batters_all, BA_all, xBA_all))
BA_xBA <- na.omit(BA_xBA)
BA_xBA$dist_to_line <- abs((-1)*as.numeric(BA_xBA$BA_all) + as.numeric(BA_xBA$xBA_all)) / sqrt(2)
BA_xBA <- BA_xBA[order(BA_xBA$dist_to_line, decreasing = TRUE), ]
rownames(BA_xBA) <- 1:nrow(BA_xBA)

BA_xBA_under <- BA_xBA[BA_xBA$BA_all<=BA_xBA$xBA_all,][1:5,]
BA_xBA_over <- BA_xBA[BA_xBA$BA_all>BA_xBA$xBA_all,][1:5,]
BA_xBA_outliers <- rbind(BA_xBA_under, BA_xBA_over)
rownames(BA_xBA_outliers) <- 1:nrow(BA_xBA_outliers)

# The points highlighted in blue are the top 5 overperformers/underperformers
xba_ba_plot <- ggplot(data = BA_xBA, aes(x = as.numeric(BA_all), y = as.numeric(xBA_all), 
                      color = ifelse(BA_xBA$batters_all %in% BA_xBA_outliers$batters_all, 'red', 'blue'))) + geom_point()
xba_ba_plot  + labs(x = 'BA', y = 'xBA', color = '', title = 'xBA vs BA (2023)') + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(color = "none") + geom_abline(slope = 1, intercept = 0, color = 'blue') +
  geom_label_repel(aes(label = ifelse(BA_xBA$batters_all %in% BA_xBA_outliers$batters_all, as.character(BA_xBA$batters_all), '')),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   segment.size  = 0.2,
                   min.segment.length = 1)

'*****************************************************************************'

"You may enter the name of a hitter below in the format 'Lastname, Firstname' 
and run the xBA and BA functions to calculate his xBA and BA on the season"

batter_name = 'Bost, Austin'

cat("Batter: ", batter_name, "/ BA: ", BA(batter_name, new_data),
    "/ xBA: ", xBA(batter_name, new_data, mutated_data))


################################################################
################################################################
################################################################

"xBA using Launch Angle, Spray Angle, and Exit Speed"

## Now lets do the same thing but include Spray Angle ##
grouped_data = data %>% 
  filter(PitchCall == "InPlay", ExitSpeed_int != 'NA', Angle_int != 'NA', Direction_int != 'NA') %>%
  group_by(ExitSpeed_int, Angle_int, Direction_int) %>%
  summarise(num_occurrences = n(),
            hits = length(which(PlayResult == "Single" | PlayResult == "Double" | PlayResult == "Triple" | PlayResult == "HomeRun")),
  ) %>%
  summarise(ExitSpeed_int, Angle_int, Direction_int, hits, num_occurrences, BA = hits / num_occurrences)


#View(grouped_data)

#ba_plot <- ggplot(grouped_data, aes(x = ExitSpeed_int, y = Angle_int, color = BA)) + geom_point()
#ba_plot + scale_color_gradient(low="blue", high="red") + labs(x = 'Exit Speed', y = 'Launch Angle', title = 'BA vs Launch Angle & Exit Speed (2022)') + 
#  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=c(0, 25, 50, 75, 100, 125)) + scale_y_continuous(breaks=c(-75, -50, -25, 0, 25, 50, 75))

###############################################################

# Now perform KNN-Regression generate a model for predicting xBA
set.seed(12)
indexes = createDataPartition(grouped_data$BA, p = .75, list = F)
train = grouped_data[indexes, ]
test = grouped_data[-indexes, ]

train_x = train[, 1:3]
preProc_train = preProcess(train_x, method =c("center", "scale"))
train_x_scaled = predict(preProc_train, train_x)
train_y = train[,6]

test_x = test[, 1:3]
test_x_scaled = predict(preProc_train, test_x)
test_y = test[,6]

best_k <- NULL
best_rmse <- 999999
rmse_values <- c()

# This for loop iterates through different values of K from 1 to 100 and finds the K which 
# Minimizes the prediction error (RMSE)
# You can adjust the range of K if its clear the optimal K isn't in this range
for (k in 50:80) {
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

# Now we can predict xBA for a specific exit speed and launch angle
point = predict(preProc_train, data.frame(ExitSpeed_int = c(92), Angle_int = c(20), Direction_int = c(-20)))
pred_spec = predict(knnmodel, point)

# Let's also predict for all values in test_x
pred_y = predict(knnmodel, data.frame(test_x_scaled))
rmse = sqrt(mean((as.numeric(unlist(test_y)) - pred_y)^2))

# Plot xBA (pred_y) vs Launch Angle & Exit Speed
#ba_plot <- ggplot(data = test_x, aes(x = ExitSpeed_int, y = Angle_int, color = pred_y)) + geom_point()
#ba_plot + scale_color_gradient(low="blue", high="red") + labs(x = 'Exit Speed', y = 'Launch Angle', color = 'xBA', title = 'xBA vs Launch Angle & Exit Speed (2022)') + 
#  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=c(0, 25, 50, 75, 100, 125)) + scale_y_continuous(breaks=c(-75, -50, -25, 0, 25, 50, 75))

mutated_data = new_data %>%
  filter(PitchCall == "InPlay", ExitSpeed_int != 'NA', Angle_int != 'NA', Direction_int != 'NA') %>%
  mutate(
         xBA = predict(knnmodel, predict(preProc_train, data.frame(ExitSpeed_int, Angle_int, Direction_int)))
  )

# Gets the name of each unique batter in the dataset
batters_all = unique(mutated_data$Batter)

# Plot xBA vs BA
xBA_all <- map(batters_all, ~ xBA(.x, new_data, mutated_data))
xBA_all <- as.numeric(unlist(xBA_all))
BA_all <- map(batters_all, ~BA(.x, new_data))
BA_all <- as.numeric(unlist(BA_all))

BA_xBA = data.frame(cbind(batters_all, BA_all, xBA_all))
BA_xBA <- na.omit(BA_xBA)
BA_xBA$dist_to_line <- abs((-1)*as.numeric(BA_xBA$BA_all) + as.numeric(BA_xBA$xBA_all)) / sqrt(2)
BA_xBA <- BA_xBA[order(BA_xBA$dist_to_line, decreasing = TRUE), ]
rownames(BA_xBA) <- 1:nrow(BA_xBA)

BA_xBA_under <- BA_xBA[BA_xBA$BA_all<=BA_xBA$xBA_all,][1:5,]
BA_xBA_over <- BA_xBA[BA_xBA$BA_all>BA_xBA$xBA_all,][1:5,]
BA_xBA_outliers <- rbind(BA_xBA_under, BA_xBA_over)
rownames(BA_xBA_outliers) <- 1:nrow(BA_xBA_outliers)

# The points highlighted in blue are the top 5 overperformers/underperformers
xba_ba_plot <- ggplot(data = BA_xBA, aes(x = as.numeric(BA_all), y = as.numeric(xBA_all), 
                                         color = ifelse(BA_xBA$batters_all %in% BA_xBA_outliers$batters_all, 'red', 'blue'))) + geom_point()
xba_ba_plot  + labs(x = 'BA', y = 'xBA', color = '', title = 'xBA vs BA (2023)') + 
  theme(plot.title = element_text(hjust = 0.5)) + guides(color = "none") + geom_abline(slope = 1, intercept = 0, color = 'blue') +
  geom_label_repel(aes(label = ifelse(BA_xBA$batters_all %in% BA_xBA_outliers$batters_all, as.character(BA_xBA$batters_all), '')),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   segment.size  = 0.2,
                   min.segment.length = 1)

'*****************************************************************************'

"You may enter the name of a hitter below in the format 'Lastname, Firstname' 
and run the xBA and BA functions to calculate his xBA and BA on the season"

batter_name = 'Crews, Dylan'

cat("Batter: ", batter_name, "/ BA: ", BA(batter_name, new_data),
 "/ xBA: ", xBA(batter_name, new_data, mutated_data))


