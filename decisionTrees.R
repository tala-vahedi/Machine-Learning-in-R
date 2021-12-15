titanic_data <- read.table('titanic.data')
#peek at the first few rows
head(titanic_data)

# make a list with variable names and set that list as column names 
colnames(titanic_data) <- c('class', 'age', 'sex', 'survive')
head(titanic_data)

# create a sequence of numbers
seq_len(5)

# randomly sample three elements from this sequence
sample(seq_len(5), 3)

# round to the nearest integer of 80% * rows
sample_size <- floor(0.8 * nrow(titanic_data))
# get the data frame indices of our random sample
training_index <- sample(seq_len(nrow(titanic_data)), size = sample_size)
# get the rows by the index from our random training sample
train_set <- titanic_data[training_index,]
# get the rows NOT in the training sample
test_set <- titanic_data[-training_index,]


###### Decision Trees in R ######

# select columns to use as predictors 
predictors <- c('class', 'age', 'sex')

# fit the model with our training set 
model_DT <- rpart(survive~., #predict 'survive' with all attributes
                  data = train_set, 
                  method = 'class')

# Show the details of the tree
summary(model_DT)

# Visualize the results of the plot
rpart.plot(model_DT)

#Vector of predicted values
tree_predictions <- predict(model_DT, newdata = test_set, type='class')

# binds the tree_predictions to the original dataframe for the test_set
evaluation_dt <- cbind(test_set, tree_predictions)
head(evaluation_dt)

# create a "correct" column 
# has value 1 if predicted value is same as the original value for "survival" 
evaluation_dt$correct <- ifelse(evaluation_dt$survive == evaluation_dt$tree_predictions,1,0)
# calculate simple accuracy 
head(evaluation_dt) 

# compute simple accuracy
sum(evaluation_dt$correct)/nrow(evaluation_dt)


###############################################################
#### Confusion Matrix for the Decision Tree ######


evaluation_decision_tree <- data.frame(actual = as.factor(evaluation_dt$tree_predictions), 
                                       predicted = as.factor(evaluation_dt$survive))
evaluation_decision_tree

decision_tree_confusion_matrix <- confusionMatrix(evaluation_decision_tree$actual,evaluation_decision_tree$predicted) 

decision_tree_confusion_matrix 


decision_tree_confusion_matrix$table
