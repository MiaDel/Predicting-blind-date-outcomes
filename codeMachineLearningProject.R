data<- read.csv('/Users/apple/Destop/speeddating_cleanedd.csv',sep = ';')
head(data)

# Iterate through the last three columns and convert b'1' to 1 and b'0' to 0
for (i in (ncol(data) - 2):ncol(data)) {
  data[, i] <- ifelse(data[, i] == "b'1'", '1', ifelse(data[, i] == "b'0'", '0', data[, i]))
}

data <- data[, -1]

for (i in 1:nrow(data)) {
  data[i, 1] <- ifelse(data[i, 1] == "b'male'", 'M', ifelse(data[i, 1] == "b'female'", 'F', data[i, 1]))
}

for (i in 1:nrow(data)) {
  data[i, 3] <- ifelse(data[i, 3] == "b'1'", '1', ifelse(data[i, 3] == "b'0'", "0", data[i, 3]))
}

data_new<- na.omit(data)

data_new$decision<- as.factor(data_new$decision)
data_new$gender<- as.factor(data_new$gender)
data_new$decision_o<- as.factor(data_new$decision_o)
data_new$match<- as.factor(data_new$match)
data_new$samerace<- as.factor(data_new$samerace)

#removing some columns
data_new <- subset(data_new, select = -c(sports, tvsports, exercise, dining, museums, art, hiking, gaming, clubbing, reading, tv, theater, movies, concerts, music, shopping, yoga))

str(data_new)


xtabs(~gender + decision, data=data_new)

data_decision<-subset(data_new, select = -c(decision_o, match))

## general_df is the data set with our chosen variables
general_df<- subset(data_new, select = -c(gender,decision_o, match, pref_o_attractive, pref_o_sincere, pref_o_intelligence, pref_o_funny, pref_o_ambitious, pref_o_shared_interests, attractive_o, sinsere_o, intelligence_o, funny_o, ambitous_o, shared_interests_o))
str(general_df)
model_all<- glm(decision~., data= general_df, family= binomial)



## assumption check
# car::vif(model_all) # Only attractive_important has high VIF! Might want to remove


# Perform Leave-One-Out Cross-Validation (LOOCV)
accuracies <- c()
for (i in 1:nrow(general_df)) {
  # Extract validation observation
  validation_data <- general_df[i, ]
  
  # Extract train data
  train_data <- general_df[-i, ]
  
  # Train your logistic regression model using the training data
  model<- glm(formula = decision ~., data = train_data, family = binomial)
  # Make prediction on the validation observation
  predicted_prob <- predict(model, newdata = validation_data, type = "response")
  binary_predictions <- ifelse(predicted_prob >= 0.5, 1, 0)
  y_actual<- general_df$decision[i]
  accuracies[i] <- ifelse(y_actual == binary_predictions, 1, 0)
}

# Calculate average accuracy across all validation observations
avg_accuracy <- mean(accuracies)
avg_accuracy


## backward regression

MASS::stepAIC(model_all, direction = "backward")

#LOOCV
mses <- numeric(length = nrow(general_df))
accuracies <- c()
# Perform Leave-One-Out Cross-Validation (LOOCV)
for (i in 1:nrow(general_df)) {
  # Extract validation observation
  validation_data <- general_df[i, ]
  
  # Extract train data
  train_data <- general_df[-i, ]
  
  # Train your logistic regression model using the training data
  model<- glm(formula = decision ~ importance_same_race + importance_same_religion +
                sincere_important + funny_important + shared_interests_important +
                attractive + sincere + funny + attractive_partner + sincere_partner +
                intelligence_partner + ambition_partner + interests_correlate +
                expected_happy_with_sd_people + expected_num_matches + like +
                guess_prob_liked + met, family = binomial, data = train_data)
  # Make prediction on the validation observation
  predicted_prob <- predict(model, newdata = validation_data, type = "response")
  binary_predictions <- ifelse(predicted_prob >= 0.5, 1, 0)
  y_actual<- general_df$decision[i]
  accuracies[i] <- ifelse(y_actual == binary_predictions, 1, 0)
  
  # Calculate MSE for the validation observation
  mse <- (as.numeric(validation_data$decision) - predicted_prob)^2
  
  # Store MSE
  mses[i] <- mse
}

# Calculate average MSE and accuracies across all validation observations
avg_mse <- mean(mses)
avg_accuracy <- mean(accuracies)

avg_accuracy

#accuracy
model<- glm(formula = decision ~ importance_same_race + importance_same_religion +
              sincere_important + funny_important + shared_interests_important +
              attractive + sincere + funny + attractive_partner + sincere_partner +
              intelligence_partner + ambition_partner + interests_correlate +
              expected_happy_with_sd_people + expected_num_matches + like +
              guess_prob_liked + met, family = binomial, data = general_df)

y_pred<- predict(model, type="response")
binary_predictions <- ifelse(y_pred >= 0.5, 1, 0)

y_actual<- general_df$decision

table(binary_predictions, y_actual)

accuracy<- (512+352)/(512+352+93+91)
accuracy
##############################
# General; Accuracies
# Before Stepwise: 0.81393
# After Stepwise: 0.81584

women_df <- data_new[data_new$gender == "F", ]
men_df <- data_new[data_new$gender == "M", ]

women_df_decision<- subset(women_df, select = -c(gender,decision_o, match, pref_o_attractive, pref_o_sincere, pref_o_intelligence, pref_o_funny, pref_o_ambitious, pref_o_shared_interests, attractive_o, sinsere_o, intelligence_o, funny_o, ambitous_o, shared_interests_o))

model_all_women<- glm(decision~., data= women_df_decision, family= binomial)

accuracies <- c()

# Perform Leave-One-Out Cross-Validation (LOOCV)
for (i in 1:nrow(women_df_decision)) {
  # Extract validation observation
  validation_data <- women_df_decision[i, ]
  
  # Extract train data
  train_data <- women_df_decision[-i, ]
  
  # Train your logistic regression model using the training data
  model<- glm(formula = decision ~., family = binomial,
              data = train_data)
  # Make prediction on the validation observation
  predicted_prob <- predict(model, newdata = validation_data, type = "response")
  binary_predictions <- ifelse(predicted_prob >= 0.5, 1, 0)
  y_actual<- validation_data$decision
  
  mse <- (as.numeric(validation_data$decision) - predicted_prob)^2
  
  # Store MSE & Accuracies
  mses[i] <- mse
  accuracies[i] <- ifelse(y_actual == binary_predictions, 1, 0)
}
avg_accuracy <- mean(accuracies)
avg_accuracy



MASS::stepAIC(model_all_women, direction = "backward")

model_women<- glm(formula = decision ~ importance_same_race + attractive_important +
                    sincere_important + intellicence_important + ambtition_important +
                    attractive + intelligence + funny + attractive_partner +
                    sincere_partner + shared_interests_partner + interests_correlate +
                    expected_happy_with_sd_people + expected_num_interested_in_me +
                    expected_num_matches + like + guess_prob_liked + met, family = binomial,
                  data = women_df_decision)

summary(model_women)

#LOOCV
mses <- numeric(length = nrow(women_df_decision))
accuracies <- c()
# Perform Leave-One-Out Cross-Validation (LOOCV)
for (i in 1:nrow(women_df_decision)) {
  # Extract validation observation
  validation_data <- women_df_decision[i, ]
  
  # Extract train data
  train_data <- women_df_decision[-i, ]
  
  # Train your logistic regression model using the training data
  model<- glm(formula = decision ~ importance_same_race + attractive_important +
                sincere_important + intellicence_important + ambtition_important +
                attractive + intelligence + funny + attractive_partner +
                sincere_partner + shared_interests_partner + interests_correlate +
                expected_happy_with_sd_people + expected_num_interested_in_me +
                expected_num_matches + like + guess_prob_liked + met, family = binomial,
              data = train_data)
  # Make prediction on the validation observation
  predicted_prob <- predict(model, newdata = validation_data, type = "response")
  binary_predictions <- ifelse(predicted_prob >= 0.5, 1, 0)
  y_actual<- validation_data$decision
  
  
  # Calculate MSE for the validation observation
  mse <- (as.numeric(validation_data$decision) - predicted_prob)^2
  
  # Store MSE & accuracies
  mses[i] <- mse
  accuracies[i] <- ifelse(y_actual == binary_predictions, 1, 0)
}

# Calculate average MSE across all validation observations
avg_mse <- mean(mses)
print("Women: Average MSE, after stepwise")
avg_mse
avg_accuracy <- mean(accuracies)
print("Women: Accuracy, after stepwise")
avg_accuracy

########################
# Women: Accuracies
# Before stepwise: 0.83050
# After stepwise: 0.83804

#accuracy
model_women<- glm(formula = decision ~ importance_same_race + attractive_important +
                    sincere_important + intellicence_important + ambtition_important +
                    attractive + intelligence + funny + attractive_partner +
                    sincere_partner + shared_interests_partner + interests_correlate +
                    expected_happy_with_sd_people + expected_num_interested_in_me +
                    expected_num_matches + like + guess_prob_liked + met, family = binomial,
                  data = women_df_decision)

y_pred<- predict(model_women, type="response")
binary_predictions <- ifelse(y_pred >= 0.5, 1, 0)

y_actual<- women_df_decision$decision

table(binary_predictions, y_actual)

accuracy<- (305+151)/531
accuracy

men_df_decision<- subset(men_df, select = -c(gender,decision_o, match, pref_o_attractive, pref_o_sincere, pref_o_intelligence, pref_o_funny, pref_o_ambitious, pref_o_shared_interests, attractive_o, sinsere_o, intelligence_o, funny_o, ambitous_o, shared_interests_o))

model_all_men<- glm(decision~., data= men_df_decision, family= binomial)


# Perform Leave-One-Out Cross-Validation (LOOCV)
accuracies <- c()
for (i in 1:nrow(men_df_decision)) {
  # Extract validation observation
  validation_data <- men_df_decision[i, ]
  
  # Extract train data
  train_data <- men_df_decision[-i, ]
  
  # Train your logistic regression model using the training data
  model<- glm(formula = decision ~., family = binomial, data = train_data)
  # Make prediction on the validation observation
  predicted_prob <- predict(model, newdata = validation_data, type = "response")
  binary_predictions <- ifelse(predicted_prob >= 0.5, 1, 0)
  y_actual<- validation_data$decision
  accuracies[i] <- ifelse(y_actual == binary_predictions, 1, 0)
  
  # Calculate MSE for the validation observation
  mse <- (as.numeric(validation_data$decision) - predicted_prob)^2
  
  # Store MSE
  mses[i] <- mse
}

# Calculate average MSE across all validation observations
avg_mse <- mean(mses)
print("Men: Average MSE, before stepwise")
avg_mse
avg_accuracy <- mean(accuracies)
print("Men: Accuracy, before stepwise")
avg_accuracy




MASS::stepAIC(model_all_men, direction = "backward")

model_men<- glm(formula = decision ~ importance_same_race + importance_same_religion +
                  attractive_important + sincere_important + intellicence_important +
                  funny_important + shared_interests_important + attractive +
                  sincere + funny + ambition + attractive_partner + intelligence_partner +
                  ambition_partner + interests_correlate + expected_happy_with_sd_people +
                  expected_num_interested_in_me + expected_num_matches + like +
                  guess_prob_liked + met, family = binomial, data = men_df_decision)

summary(model_men)

#LOOCV
mses <- numeric(length = nrow(women_df_decision))
accuracies <- c()
# Perform Leave-One-Out Cross-Validation (LOOCV)
for (i in 1:nrow(men_df_decision)) {
  # Extract validation observation
  validation_data <- men_df_decision[i, ]
  
  # Extract train data
  train_data <- men_df_decision[-i, ]
  
  # Train your logistic regression model using the training data
  model<- glm(formula = decision ~ importance_same_race + importance_same_religion +
                attractive_important + sincere_important + intellicence_important +
                funny_important + shared_interests_important + attractive +
                sincere + funny + ambition + attractive_partner + intelligence_partner +
                ambition_partner + interests_correlate + expected_happy_with_sd_people +
                expected_num_interested_in_me + expected_num_matches + like +
                guess_prob_liked + met, family = binomial, data = train_data)
  # Make prediction on the validation observation
  predicted_prob <- predict(model, newdata = validation_data, type = "response")
  binary_predictions <- ifelse(predicted_prob >= 0.5, 1, 0)
  y_actual<- validation_data$decision
  accuracies[i] <- ifelse(y_actual == binary_predictions, 1, 0)
  
  # Calculate MSE for the validation observation
  mse <- (as.numeric(validation_data$decision) - predicted_prob)^2
  
  # Store MSE
  mses[i] <- mse
}

# Calculate average MSE across all validation observations
avg_mse <- mean(mses)
print("Men: Average MSE, after stepwise")
avg_mse
avg_accuracy <- mean(accuracies)
print("Men: Accuracy, after stepwise")
avg_accuracy

######################
# Men; Accuracies
# Before stepwise: 0.83752
# After stepwise: 0.83559



#accuracy
model_men<- glm(formula = decision ~ importance_same_race + importance_same_religion +
                  attractive_important + sincere_important + intellicence_important +
                  funny_important + shared_interests_important + attractive +
                  sincere + funny + ambition + attractive_partner + intelligence_partner +
                  ambition_partner + interests_correlate + expected_happy_with_sd_people +
                  expected_num_interested_in_me + expected_num_matches + like +
                  guess_prob_liked + met, family = binomial, data = men_df_decision)

y_pred<- predict(model_men, type="response")
binary_predictions <- ifelse(y_pred >= 0.5, 1, 0)

y_actual<- men_df_decision$decision

table(binary_predictions, y_actual)

accuracy<-(230+217)/517
accuracy #slightly higher

