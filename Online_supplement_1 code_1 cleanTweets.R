### Identifying weak and strong Islamophobia
  # 1, Clean tweets for word vectors

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
# You need to collect and annotate your own data
# 'tweets' dataframe with all of the tweets
# 'text' column is the text of the actual tweets
text.clean = tweets$text


## Functions
clean.tweets = function(party.tweets, party.name){
  print(nrow(party.tweets))
  if(length(which(duplicated(party.tweets$id_str))) > 0){
    stop(print('Duplicates!'))
  } else {print('No duplicates')}
  
  party.tweets$party = party.name
  
  # Keep only tweets from 2017 onwards
  # convert dates to R dates
  party.tweets$created_atDate = base::strptime(party.tweets$created_at, format = "%a %b %d %H:%M:%S %z %Y"); party.tweets$created_atDate[1:10]
  party.tweets$created_atUNIX = as.numeric(party.tweets$created_atDate); party.tweets$created_atUNIX[1:10]
  
  # remove tweets from before October 1 2017 (earliest point which we have data collection for all parties)
  min.dat = factor('Sun Oct 01 00:00:01 +0000 2017')
  min.dat = strptime(min.dat, format = "%a %b %d %H:%M:%S %z %Y")
  min.dat = as.numeric(min.dat)
  party.tweets = party.tweets[which(party.tweets$created_atUNIX >= min.dat),]
  print(nrow(party.tweets))
  return(party.tweets)
}

# remove u' from the start of tweets
remove.u.at.start = function(text.clean){
  index.swap = which(substr(x = text.clean,
                            start = 1,
                            stop = 2) == "u'")
  if (length(index.swap) > 0){
    text.clean[index.swap] = substr(x = text.clean[index.swap],
                                    start = 3,
                                    stop = nchar(text.clean[index.swap]))}
  return(text.clean)
}

# general mgsub function
mgsub <- function(pattern, replacement, x) {
  if (length(pattern)!=length(replacement)) {
    stop("pattern and replacement do not have the same length.")
  }
  result <- x
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], result, fixed = T)
  }
  result
} # https://stackoverflow.com/questions/15253954/replace-multiple-letters-with-accents-with-gsub

# split hashtags function
split.hashtags = function(x){
  for (i in 1:length(x)){
    if (i %% 100000 == 0){print(i)}
    
    hashtags.original = unlist(stringr::str_extract_all(x[[i]], "#\\S+"))
    
    if (length(hashtags.original) > 0){
      
      hashtags = hashtags.original
      hashtags = gsub('[[:digit:]]+', '', hashtags) # https://stackoverflow.com/questions/13590139/remove-numbers-from-alphanumeric-characters
      # Remove all punctuation
      hashtags = gsub('[[:punct:] ]+', '',hashtags)
      
      # don't worry about capitalised acronyms - just separate them out; because we have a function later on that efficiently removes single letter words
      
      hashtag.change =  data.frame(
        hashtags.original,
        gsub('([[:upper:]])', ' \\1', hashtags, perl = T) # separates out spaces, based on where the capitalizations fall - https://stackoverflow.com/questions/7988959/splitting-string-based-on-letters-case
      )
      
      # NEW - adds the hashtag as separate words to the end, thereby maximizing information value
      # ACTUALLY not a good idea, replacement is better because:
      # 1. word ordering matters for word embeddings
      # 2. we could end up double counting words e.g. '#website' after cleaning is recorded as two instances of website (this problem could be resolved, but still...)
      
      x[[i]] = mgsub(hashtag.change[,1], 
                     hashtag.change[,2], 
                     x = x[[i]])
    }}
  return(x)
}

## Remove Handles - note that we are *removing* and not just *replacing* ... is this sound? I think so... probably. Doing this definitely has some limitations
remove.handles = function(text.clean){
  for (i in 1:length(text.clean)){
    if (i %% 100000 == 0){print(i)}
    
    handles = unlist(stringr::str_extract_all(text.clean[i], "@\\w+"))
    if(length(handles) > 0){
      text.clean[i] = mgsub(pattern = handles, 
                            replacement = rep('', length(handles)),
                            x = text.clean[i])}
  }
  return(text.clean)
}

clean.tweets = function(text.clean, numbs = T, stopz = T, stemz = F){
  text.clean = gsub('\\n', ' ', text.clean, fixed = T) # remove annoying \n values from @RichScriven at https://stackoverflow.com/questions/11936339/in-r-replace-text-within-a-string 
  text.clean = gsub('\\', '', text.clean, fixed = T)
  print('\n values and weird spaces removed')
  
  text.clean = gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "", text.clean) # remove URLs
  print('URLs removed')
  
  if(numbs == T){
    text.clean = gsub('[[:digit:]]+', ' ', text.clean) # https://stackoverflow.com/questions/13590139/remove-numbers-from-alphanumeric-characters
    print('numbers removed. This is the default setting')
  } else {
    print('not removing numbers as you set numbs == F')
  }
  
  text.clean = tolower(text.clean)
  text.clean = gsub("'", "", text.clean) # contractions should be removed
  text.clean = gsub('[[:punct:]]+', ' ', text.clean)
  print('text lowered, contractions removed, punctuation removed')
  
  text.clean = gsub('^rt', '', text.clean) # remove 'rt' at the start of words
  print('RTs removed')
  
  if(stopz == T){
    text.clean = tm::removeWords(text.clean, tm::stopwords(kind = 'english')) # remove stop words
    # YES! We remove stopwords but don't stem
    # discussion on SO: https://stackoverflow.com/questions/34721984/stopword-removing-when-using-the-word2vec
    print('stopwords removed. This is the default setting')
  } else {
    print('not removing stopwords as you set stopz == F')
  }
  
  if(stemz == T){
    # stem words
    print('NOTE: stemming is not yet implemented') #words stemmed
  } else {
    print('not stemming words as you set stemz == F. This is the default setting')
  }
  
  text.clean = gsub("htt\\w+ *", " ", text.clean) # remove anything beginning "htt" - nearly always a broken off URL string - from https://stackoverflow.com/questions/22615188/remove-all-words-that-start-with-from-a-string
  text.clean = tm::removeWords(text.clean, "htt") #remove just this specific word, which for some reason the previous line does not remove
  text.clean = gsub(" *\\b[[:alpha:]]\\b *", " ", text.clean) # remove single letter words
  text.clean = gsub("\\s+", " ", stringr::str_trim(text.clean)) # strip whitespace
  text.clean = trimws(text.clean)
  print('broken htt removed, single letter words removed, white space stripped')
  
  print(paste0('Done! The time is: ', Sys.time()))
  
  return(text.clean)
}

# remove non ASCII characters
remove.nonASCII = function(text.clean){
  ascii.valz = length(which(!(stringi::stri_enc_isascii(text.clean))))
  print(paste0('There are this many tweets with ASCII characters: ', ascii.valz))
  if (ascii.valz !=0){
    print(paste0('E.g. this tweet has ASCII characters: ', text.clean[which(!(stringi::stri_enc_isascii(text.clean)))[1]] ))
    Encoding(text.clean) = 'UTF-8'
    text.clean = base::iconv(text.clean, from = 'UTF-8', to = 'ASCII', sub = '') # removes any non-ASCII characters
    print('non-ASCII characters removed')
    
    ascii.valz = length(which(!(stringi::stri_enc_isascii(text.clean))))
    print(paste0('There are now this many tweets with ASCII characters: ', ascii.valz))
    
  } else {
    print('No tweets have ASCII characters. Good for you!')
  }
  return(text.clean)
}              

# Remove empty tweets
remove.emptyTweets = function(text.clean){
  index.remove = which(sapply(text.clean, function(x){
    base::nchar(x) == 0}))
  print(paste0('there are this many tweets which are now empty: ', length(index.remove)))
  
  if (length(index.remove) > 0){
    text.clean = text.clean[-index.remove,] # remove all of the empty tweets
  }
  return(text.clean)
}

# Remove duplicates
remove.duplicates = function(text.clean){
  dup.tweet = which(duplicated(text.clean))
  print(paste0('there are this many duplicated tweets: ', length(dup.tweet)))
  if (length(dup.tweet) >0){
    text.clean = text.clean[-dup.tweet] }
  dup.tweet = which(duplicated(text.clean))
  print(paste0('there are now this many: ', length(dup.tweet)))
  
  return(text.clean)
}




## Clean text
text.clean[1:3]
#text.clean = remove.u.at.start(text.clean) # you might need to use this function for your work, depending on how the tweets are pre-processed

text.clean = split.hashtags(text.clean)
text.clean[1:3]

text.clean = remove.handles(text.clean)
text.clean[1:3]

text.clean = clean.tweets(text.clean)
text.clean[1:3]

text.clean = remove.nonASCII(text.clean)
text.clean[1:3]

#text.clean = remove.emptyTweets(text.clean) # depending on your analysis, this may be useful
#text.clean = remove.duplicates(text.clean) # depending on your analysis, this may be useful

# Save output
save(text.clean,
     file = "text.clean.RData")


