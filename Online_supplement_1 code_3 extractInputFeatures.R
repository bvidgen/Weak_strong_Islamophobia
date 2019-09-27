### Identifying weak and strong Islamophobia
  # 3, Extract input features

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
load("tweets.RData")
# 'tweets' dataframe with all of the tweets
# 'text' column is the text of the tweets
# 'id_str' column is the ID of the tweets


# Make a dataframe for the features
features.out = data.frame(matrix(ncol = 0, nrow = nrow(tweets)))
features.out$id_str = tweets$id_str
# The features to extract are:
  # Count of mentions of Mosques
  # presence of HTML
  # part of speech: ‘adverb’
  # part of speech: ‘conjunction’
  # named entity recognition: ‘location’
  # named entity recognition: 'organization'


# Mosques #
mosques = readLines("Wiki Mosques.txt") # from https://en.wikipedia.org/wiki/List_of_mosques_in_the_United_Kingdom
mosques = tolower(mosques)
mosques = unique(mosques)
mosques[1:5]

df = tm::Corpus(VectorSource(tweets$text)) # create a tm corpus
df = tm::DocumentTermMatrix(df) # create a DTM
df = tidytext::tidy(df)
mosque.index = which(df$term %in% mosques); # no mentions of mosques in this dataset
df.index = as.numeric(df$document[mosque.index]); 
features.out$mosques = 0
features.out$mosques[df.index] = 1;
rm(df, mosques, mosque.index, df.index)


# HTML variable #
length(grep('http\\S+\\s*',tweets$text))
features.out$html = 0
features.out$html[grep('http\\S+\\s*',tweets$text)] = 1


# RT variable #
length(grep('^rt ', tolower(tweets$text)))
index.rt = grep('^rt ', tolower(tweets$text))
features.out$rt = 0
features.out$rt[index.rt] = 1
rm(index.rt)





# Part of speech and NER using CleanNLP #
reticulate::use_python(python = '/anaconda/bin/python3')
cleanNLP::cnlp_init_spacy(model_name = 'en')

# In case you cannot run the remainder of this script because you do not have spacy installed on your computer then please load in this additional dataframe:
load('input.features.extra.RData')
features.out = cbind.data.frame(features.out, extra.features)
rm(extra.features)
head(features.out, 3)
save(features.out,
     file = "features.tweets.RData")


numTweets = 10000 # 10k
# rough time estimates (depends upon your computer)
# 10k = 45s-1m
# 1k = 10s
# 5k = 
# 100k = 15m +
# 20k = 3m30s-5m
n = round(length(tweets) / numTweets); n
start = 1

# dataframes for all of our output
adverb.index = data.frame()
conjunction.index = data.frame()
location.index = data.frame()
organization.index = data.frame()
print('starting to extract features')
print(base::Sys.time())
print(ls())

# run the code iteratively, to minimize chance of overloading memory
for (i in seq(1,n)){
  if (i %% 5 == 0){
    gc()
    Sys.sleep(60)
    save(adverb.index,
         conjunction.index,
         location.index,
         organization.index,
         start,
         file = "temp.features.RData")}
  
  if (i != n){
    rangez = (start: (start + (numTweets - 1)))
  } else {
    rangez = (start: length(tweets))
  }
  print(paste0('The time is: ', Sys.time(),
               '. i is equal to: ', i,' out of ', n, 
               '. The range of tweets is: ', paste(range(rangez), collapse = " "))) # record of what is going on
  
  tweets.temp = tweets[rangez]
  
  # Create 'entity' object
  obj = cleanNLP::cnlp_annotate(tweets.temp, as_strings = TRUE)
  entity = cleanNLP::cnlp_get_entity(obj);
  entity$id = as.numeric(substr(x = entity$id,
                                start = 4,
                                stop = nchar(entity$id)))
  entity$id = entity$id + (start - 1) # start is always e.g. 1 or 10,001 or 20,0001 etc. 
  
  
  # LOCATION
  loc.entity = subset(entity, entity_type == "GPE")
  location.temp = as.data.frame(table(loc.entity$id))
  location.index = rbind.data.frame(location.index, location.temp)
  # location.index[rangez] = location.temp # possible alternative, in case the above method is too slow?
  
  # ORGANIZATION
  org.entity = subset(entity, entity_type == "ORG")
  organization.temp = as.data.frame(table(org.entity$id))
  organization.index = rbind.data.frame(organization.index, organization.temp)
  
  # CONJUNCTION
  tokens = cleanNLP::cnlp_get_token(obj);
  tokens$id = as.numeric(substr(x = tokens$id,
                                start = 4,
                                stop = nchar(tokens$id)))
  tokens$id = tokens$id + (start -1)
  tokens = subset(tokens, upos == "CCONJ")
  
  conjunction.temp = as.data.frame(table(tokens$id))
  conjunction.index = rbind.data.frame(conjunction.index, conjunction.temp)
  
  # ADVERB
  tokens = cleanNLP::cnlp_get_token(obj);
  tokens$id = as.numeric(substr(x = tokens$id,
                                start = 4,
                                stop = nchar(tokens$id)))
  tokens$id = tokens$id + (start -1)
  tokens = subset(tokens, upos == "ADV")
  
  adverb.temp = as.data.frame(table(tokens$id))
  adverb.index = rbind.data.frame(adverb.index, adverb.temp)
  
  # TIDY UP
  rm(obj, tweets.temp, conjunction.temp, adverb.temp, location.temp, organization.temp, entity, loc.entity, org.entity)
  start = start + numTweets
  gc()
}



## Combine the input features to a single dataframe
# add conjunction.index to the features.out dataframe
nrow(conjunction.index)
features.out$pos_cconj = 0
features.out$pos_cconj[as.numeric(as.character(conjunction.index$Var1))] = as.numeric(as.character(conjunction.index$Freq))
print('POS - conjunction - DONE')
print(base::Sys.time())

# add location.index to the features.out dataframe
nrow(location.index)
features.out$ner_gpe = 0
features.out$ner_gpe[as.numeric(as.character(location.index$Var1))] = as.numeric(as.character(location.index$Freq))
print('NER - location - DONE')
print(base::Sys.time())

# add organization.index to the features.out dataframe
nrow(organization.index)
features.out$ner_org = 0
features.out$ner_org[as.numeric(as.character(organization.index$Var1))] = as.numeric(as.character(organization.index$Freq))
print('NER - organization - DONE')
print(base::Sys.time())

head(features.out); tail(features.out, 4) # the dataset should now be complete!



# Save
save(features.out,
     file = "features.tweets.RData")
rm(adverb.index, conjunction.index, location.index, organization.index, tokens, tweets)






