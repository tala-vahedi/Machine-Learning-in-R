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



##### NN in R ######

# convert data to matrix 
titantic_mtx <- model.matrix(~ survive + class + sex+ age, # can select variables in the formula
                             data = titanic_data)
#peek at matrix 
head(titantic_mtx)

titantic_mdf <- data.frame(titantic_mtx)
#peek again
head(titantic_mdf)

mean(titantic_mdf$X.Intercept)

titantic_mdf$X.Intercept <- NULL

train_mtx <- titantic_mdf[training_index,]

test_mtx <- titantic_mdf[-training_index,]

# fit model using four hidden nodes
nnet_model <- nnet(formula = surviveyes~., #we predict 'surviveyes' with all attributes
                   data=train_mtx, # designate training data 
                   size = 4, #size of hidden layer
                   maxit = 400) #maximum number of training iterations

# fit testing data
nn_pred_raw <- predict(nnet_model, test_mtx)
head(nn_pred_raw) 

# the predictions are continuous values
# we round them to get binary predictions
nn_predictions <- round(nn_pred_raw)
head(nn_predictions)

evaluation_nn <- data.frame(actual = as.factor(test_mtx$surviveyes), 
                            predicted = as.factor(nn_predictions)) 
head(evaluation_nn) #what are the column names called now?


###############################################################
#### Confusion Matrix for the neural networks ######


# create a "correct" column 
evaluation_nn$correct <- ifelse(evaluation_nn$actual == evaluation_nn$predicted,1,0)
# calculate simple accuracy 
sum(evaluation_nn$correct)/nrow(evaluation_nn) 

nn_confusion_matrix <- confusionMatrix(evaluation_nn$predicted,
                                       evaluation_nn$actual) 
nn_confusion_matrix 





