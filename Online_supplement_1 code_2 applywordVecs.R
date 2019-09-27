### Identifying weak and strong Islamophobia
  # 2, Apply word vectors

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

# Load tweet data
load("text.clean.RData") # clean tweets from the previous script
tweets.clean = text.clean; rm(text.clean)


### Extract word embeddings model features
# Load word embeddings model
load("Online_supplement_1 word vectors.RData"); gc()
word_vectors[1:10, 1:10] # quick inspection
dim(word_vectors)

### Calculate average vectors for WE
numTweets = length(tweets.clean) #the number of tweets we want to apply the classifier to in each round. Set to length(tweets.clean) to do all at once
iterz = round(length(tweets.clean) / numTweets); iterz # how many 'iterations' it will take (we run through iterations to minimize overloading memory!!)
start = 1
results.out = data.frame() # the results of the classifications

for (i in seq(1, iterz)){
  if (i %% 10 == 0){
    save(results.out, # so that we always have some tweets labelled even if something goes wrong, e.g. computer dies
         file = "temp.RData")}  
  
  rangez = start: (numTweets*i)
  print(paste0(i, ' out of ', iterz))
  print(paste0(range(rangez), collapse = ' to '))
  print(base::Sys.time())
  
  tweets.temp = tweets.clean[rangez] # the numTweets we are extracting word vecs for
  model6.dtm = tm::Corpus(tm::VectorSource(tweets.temp)) # create a tm corpus
  model6.dtm = tm::DocumentTermMatrix(model6.dtm) # create a DTM
  df = tidytext::tidy(model6.dtm)
  
  print('Starting WE fitting')
  print(base::Sys.time())
  
  termz = df$term
  docz = as.numeric(df$document) + ((numTweets*i) - numTweets)
  
  # only keep terms in the word_vectors df which are in the list of termz in the tweets (cos word_vecs was taken from a large number of tweets with many words)
  wordy_vz_1 = base::intersect(rownames(word_vectors), termz)
  word_vectors.new = word_vectors[which(rownames(word_vectors) %in% wordy_vz_1),];
  #print(dim(word_vectors.new))
  if(all(rownames(word_vectors.new) ==  wordy_vz_1)){
    print('GOOD - word_vectors.new has same words as wordy_vz_1')
  }
  
  # Remove terms which are not in the wordy_vz_1 (i.e. the terms in this specific word vecs model - this is more efficient)
  del.rowz = which(!(termz %in% wordy_vz_1))
  if(length(del.rowz) > 0){
    docz = docz[-del.rowz] # remove the docz index
    termz = termz[-del.rowz]} # remove the term
  
  # For every token, get the position that it appears in the word_vector df (denoted here by wordy_vz_1)
  term.match = match(termz, wordy_vz_1)
  poi = word_vectors.new[term.match,]; #print(dim(poi)) # the word vectors, each lined up with the matching terms
  remove.na = which(is.na(poi[,1]))
  if(length(remove.na) > 0){
    poi = poi[-remove.na,]; #dim(poi)
    docz = docz[-remove.na]; #length(docz)
  }
  
  # combine the results! (this is what takes a long time to run)
  iterz.vecs = stats::aggregate(poi, list(docz), mean)
  
  # save our results
  results.out = rbind(results.out, iterz.vecs)
  
  # tidy up and start again...
  start = start + numTweets
  rm(termz, docz, wordy_vz_1, word_vectors.new, remove.na, del.rowz, term.match, iterz.vecs, poi, rangez)
  gc()
  Sys.sleep(10) # If your computer is overheating then increase this to e.g. 120 seconds (two minutes)
}
# NOTE: this code will delete any empty tweets (those without any words in the word_vectors object, which are sometimes created from the cleaning).
# But luckily the next bit of code will handle these empty tweets - though you may be happy to just use the non empty tweets

pan.add = data.frame(Group.1 = which(!(1:length(tweets.clean) %in% results.out$Group.1)))
pan.add[,2:51] = 0
colnames(pan.add) = colnames(results.out)
pan = rbind(results.out, pan.add)
pan = pan[order(as.numeric(pan$Group.1)),]
rownames(pan) = NULL
gc()
head(pan)
pan$Group.1 = NULL
tweets.word.vecs = pan

save(tweets.word.vecs,
     file = "tweets.word.vecs.RData")




