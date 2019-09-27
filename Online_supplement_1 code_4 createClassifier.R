### Identifying weak and strong Islamophobia
  # 4, Create and applyclassifier

# Libraries
# Install these libraries first if they are not already installed - you will need them at one point in the pipeline
library(tokenizers)
library(text2vec)
library(tm)
library(stringi)
library(spacyr)
library(dplyr)
library(tidytext)
library(cleanNLP)
library(e1071)
library(ggplot2)

options(scipen = 999)
gc()
getwd()

# For this code you will need to have an annotated dataset of tweets
  # the guidelines for annotation are provided in: 'Islamophobia annotation guidelines'
  # It is strongly recommended that you have three people annotate the tweets, and then take the majority decision in each case

### Load Data
load('annotations.RData')
  # data frame called 'annotations'
  # With the annotations for the tweets - for your data, YOU will need to do the annotations
  # You want just one column in the dataframe - 'strength', which a 0, 1 or 2 for the type of Islamophobia which is expressed (none, weak or strong)

# Load input features
load("tweets.word.vecs.RData") # the word vector values we have just calculated 
load("features.tweets.RData") # the other input features

# combine the input features
model.in = cbind.data.frame(features.out, tweets.word.vecs) # order is very important here!
rm(features.out, tweets.word.vecs)

# sort out column names
colnames(model.in)
colnames(model.in)[8:57] = paste0('vector', 1:50)
colnames(model.in)
model.in$id_str = NULL # delete if you have still left the id_str attached; or save as a separate object for use later
model.in = model.in[,c(7:56, 1:6)] # keep the ordering consistent
row.names(model.in) = NULL
head(model.in, 6)

# attach the annotations
model.in = cbind.data.frame(annotations, model.in)
rm(annotations); gc()



### Train the model using SVM ###
# If you are totally new to the SVM algorithm, here is a good resource: https://www.r-bloggers.com/machine-learning-using-support-vector-machines/

# NOTE If you find that the model can't work because 'mosques' is empty, then:
  model.in$mosques = NULL

set.seed(1)
model.svm <- e1071::svm(strength~.,
                        model.in) # train the model on all input features, with strength as the output
model.svm.predict = predict(model.svm,
                            model.in) # the assigned predictions of the model
model.svm.results = table(round(model.svm.predict),
                          model.in$strength) # compare the predicted with actual results
caret::confusionMatrix(model.svm.results) # great info!
caret::confusionMatrix(model.svm.results)$overall[[1]] # the key metric for evaluating performance
  # NOTE: accuracy may differ from that reported in the full paper, as only an unbalanced subset of the data is provided here for testing
  # AND this model is fit on itself - see below for how to implement cross-fold validation (which is far more robust for testing, and what we use in the paper)

# Save the classifier model for use again
save(model.svm,
     file = "multi-classifier-model.RData")



# You can now use the classifier on unseen data, with the level of accuracy observed. Prepare some new data in exactly the same way as with the pipeline of code here.
  # to apply the classifier, take your new tweets (make sure they are prepared properly)
new.tweets = model.in[1:20,] # just to show the code, let's use the first 20 instances of the model.in dataframe
model.svm.predict = stats::predict(model.svm,
                                   new.tweets) # the assignments from the classifier
model.in$strength = round(model.svm.predict) # attach it back to our starting dataframe

save(model.in,
     file = 'annotations.output.RData')



### Model tuning [optional]
  # NOTE: These can take an extremely long time to run as it is a grid search; every combination is tried. Start with just a few options 
svm.tune.epsilon <- e1071::tune(method = svm,
                                strength ~ ., data = model.in,
                                ranges = list(
                                  #epsilon = seq(0,1, by = 0.01),
                                  cost = 2^(1:3), 
                                  gamma = seq(0, 1, by = 0.2),
                                  kernel = "radial"))
print(svm.tune.epsilon) # this allows you to adjust parameters in the model to improve performance




### Evaluate performance using k-folds (5 or 10 are typical)
n = nrow(model.in)
folds = 10 # how many splits in the data for testing/training purposes
set.seed(1)
splitfolds = sample(1:folds, n, replace = T)
table(splitfolds)
sum(table(splitfolds)) == n # TRUE - simple check

model.out = data.frame()
for (i in 1:folds){
  print(i)
  
  # set which model we test for (you may want to take this code and try different combinations of inputs)
  modelz = model.in
  
  # split into training and testing data sets
  train_set = modelz[splitfolds != i, ]
  test_set = modelz[splitfolds == i, ]
  
  # train the model on the train data set
  model.svm <- e1071::svm(strength ~ .,
                          data = train_set,
                          kernel = "radial",
                          cost = 1,
                          gamma = 0.01)
  
  # get the predictions using model.log on the test data set
  model.svm.predict = predict(model.svm, test_set)
  
  # calculate the accuracy - comparing the actual test_set values with the predicted values
  
  if(length(model.svm.predict) == length(test_set$strength)){
    model.svm.results = table(round(model.svm.predict), test_set$strength)
    rownames(model.svm.results) = colnames(model.svm.results)
  } else {
    model.svm.predict = as.factor(c(round(model.svm.predict), 1))
    model.svm.results = table(model.svm.predict, test_set$strength)
    rownames(model.svm.results) = colnames(model.svm.results)
  }
  caret::confusionMatrix(model.svm.results)$overall[[1]]
  
  model.out = rbind(
    model.out,
    caret::confusionMatrix(model.svm.results)$overall[[1]]
  )
}
model.out
mean(model.out[,1])



