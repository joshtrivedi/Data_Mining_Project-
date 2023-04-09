ipums_data <- read.csv("2012-16_ipums_ancestry_extract.csv")

# Remove rows with missing values
ipums_data <- na.omit(ipums_data)

# Examine the Structure of the data
str(ipums_data)

# Scale numeric variables
ipums_data$RACE <- scale(ipums_data$RACE)

# Descriptive Analysis
summary(ipums_data$HHWT)
summary(ipums_data$RACE)
summary(ipums_data$PERWT)
summary(ipums_data$ANCESTR1)

# Frequency Table
table(ipums_data$RACE)
table(ipums_data$ANCESTR1)

# Loading the packaged
library(rpart)

# Assigining classes to Race Factors
ipums_data$RACE <- factor(ipums_data$RACE, labels = c("White", "Black", "American Indian/Alaskan Native", "Chinese", "Japanese", "Other Asian", "Other race","Two Major Races","Three of More Major Races"))

#Train Test Split
set.seed(123)
train_idx <- sample(nrow(ipums_data), 0.7*nrow(ipums_data))
train_data <- ipums_data[train_idx,]
test_data <- ipums_data[-train_idx,]

# Decision Tree
start_time = system.time()
dec_tree_race = rpart(RACE ~ ., data= train_data, method="class")

# Plotting decision tree
plot(dec_tree_race)
text(dec_tree_race, use.n = TRUE, all = TRUE, cex = 0.9)
end_time = system.time()
total_time = end_time - start_time
cat("Elapsed Time for 1st Decision Tree:", total_time)
# Predict the race of people in the test data
race_pred <- predict(dec_tree_race, newdata = test_data, type = "class")
# Calculate the accuracy of the model
accuracy1 <- sum(race_pred == test_data$RACE)/nrow(test_data)

# Plotting second decision tree based on Birthplace and Metropolitan Area
data <- ipums_data[c("RACE","BPL","MET2013")]
# At the same time check the time required fgor this
start_time1 <- system.time()
dec_tree_bplmet = rpart(RACE ~ BPL + MET2013, data = data, method = "class")
plot(dec_tree_bplmet)
text(dec_tree_bplmet, use.n = TRUE, all = TRUE, cex = 0.9)
end_time1 <- system.time()
total_time1 <- end_time - start_time
cat("Elapsed Time for 2nd Decision Tree:", total_time1)
# Predict on the Test Data
test_pred <- predict(dec_tree_bplmet, newdata = test_data, type="class")

# Accuracy
accuracy2 <- sum(test_pred == test_data$RACE)/nrow(test_data)


# Confusion Matrix
start_time2 = system.time()
conf_matrix <- table(test_data$RACE, test_pred)
tp <- diag(conf_matrix)
fp <- colSums(conf_matrix) - tp
fn <- rowSums(conf_matrix) - tp
tn <- sum(conf_matrix) - tp - fp - fn

# calculate precision, recall, and F1 score
precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
f1_score <- 2 * (precision * recall) / (precision + recall)
end_time2 = system.time()
total_time2 = end_time2 - start_time2
cat("Elapsed Time for Confusion Matrix and Evaluation Metrics:", total_time2)
# print results
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")
