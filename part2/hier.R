###
##
### Document Similarity Using Measures
##
## Gates
## ANother good resource:
## https://rstudio-pubs-static.s3.amazonaws.com/66739_c4422a1761bd4ee0b0bb8821d7780e12.html
## http://www.minerazzi.com/tutorials/cosine-similarity-tutorial.pdf
## Book: Text Mining in R
## https://www.tidytextmining.com/
######## Example 1 ----------------------
##
## Whenever you learn something new, always create a very small
## example that you can practice with. 

## I have created a small "Corpus" (collections of documents or books)
## They are called, Doc1, Doc2, ..., Doc5.
## The documents are in sentence format.

## The goal is to see how similar the documents are.

## First, we must read in the documents and convert them to 
## a format that we can evaluate.

##If you install from the source....
#Sys.setenv(NOAWT=TRUE)
## ONCE: install.packages("wordcloud")
library(wordcloud)
## ONCE: install.packages("tm")
library(tm)
# ONCE: install.packages("Snowball")
## NOTE Snowball is not yet available for R v 3.5.x
## So I cannot use it  - yet...
##library("Snowball")
##set working directory
## ONCE: install.packages("slam")
library(slam)
library(quanteda)
## ONCE: install.packages("quanteda")
## Note - this includes SnowballC
library(SnowballC)
library(arules)
##ONCE: install.packages('proxy')
library(proxy)

setwd("/home/akhil/CUB/Spring 2023/TM/TM_project")
## Next, load in the documents (the corpus)

### !!!!!!!!!
## Make your own corpus with 5 docs
## Make some docs similar to others so that they cluster!
##
## !!!!!!!!!!!!!!!!!!!!!!!!!!!
TheCorpus <- Corpus(DirSource("corpus"))
##The following will show you that you read in 5 documents
(TheCorpus)

##Next, there are several steps needed to prepare the texts
## You will need to remove punctuation, make everything lowercase
## normalize, remove common and useless words like "and", "the", "or"
## Uselses words are called "Stop Words"
## Don't forget to remove numbers as well. 

## The function : getTransformations() will show all the functions
## that process the data - such as removeNumbers, removePunctuation, etc
## run getTransformations() to see this.
## Also note that tolower() will change all case to lowercase.

## The tm_map function allows you to perform the same 
## transformations on all of your texts at once
CleanCorpus <- tm_map(TheCorpus, removePunctuation)

## Remove all Stop Words
CleanCorpus <- tm_map(CleanCorpus, removeWords, stopwords("english"))

## You can also remove words that you do not want
MyStopWords <- c("and","like", "very", "can", "I", "also", "lot")
CleanCorpus <- tm_map(CleanCorpus, removeWords, MyStopWords)

## NOTE: If you have many words that you do not want to include
## you can create a file/list
## MyList <- unlist(read.table("PATH TO YOUR STOPWORD FILE", stringsAsFactors=FALSE)
## MyStopWords <- c(MyList)

##Make everything lowercase
CleanCorpus <- tm_map(CleanCorpus, content_transformer(tolower))

## Next, we can apply lemmitization
## In other words, we can combine variations on words such as
## sing, sings, singing, singer, etc.
## NOTE: This will NOT WORK for R version 3.5.x yet - so its
## just for FYI. This required package Snowball which does not yet
## run under the new version of R
#CleanCorpus <- tm_map(CleanCorpus, stemDocument)
#inspect(CleanCorpus)



## Let's see where we are so far...
inspect(CleanCorpus)
## You can use this view/information to add Stopwords and then re-run.
## In other words, I see from inspection that the word "can" is all over
## the place. But it does not mean anything. So I added it to my MyStopWords

## Next, I will write all cleaned docs  - the entire cleaned and prepped corpus
## to a file - in case I want to use it for something else.

(Cdataframe <- data.frame(text=sapply(CleanCorpus, identity), 
                          stringsAsFactors=F, na.rm = T))
write.csv(Cdataframe, "Corpusoutput2.csv")

## Note: There are several other functions that also clean/prep text data
## stripWhitespace and
## myCorpus <- tm_map(myCorpus, content_transformer(removeURL)) 

## ------------------------------------------------------------------
## Now, we are ready to move forward.....
##-------------------------------------------------------------------

## View corpus as a document matrix
## TMD stands for Term Document Matrix
(MyTDM <- TermDocumentMatrix(Cdataframe))
(MyDTM2 <- DocumentTermMatrix(CleanCorpus))
inspect(MyTDM)
inspect(MyDTM2)


## By inspecting this matrix, I see that the words "also" and "lot" is there, but not useful
## I will add these to my MyStopWords and will re-run the above code....
##--------------NOTE
## ABOUT DocumentTermMatrix vs. TermDocumentMatrix - yes these are NOT the same :)
##TermDocument means that the terms are on the vertical axis and the documents are 
## along the horizontal axis. DocumentTerm is the reverse

## Before we normalize, we can look at the overall frequencies of words 
## This will find words that occur 3 or more times in the entire corpus
(findFreqTerms(MyDTM2, 3))
## Find assocations via correlation
## https://www.rdocumentation.org/packages/tm/versions/0.7-6/topics/findAssocs
findAssocs(MyDTM2, 'coffee', 0.20)
findAssocs(MyDTM2, 'dog', 0.20)
findAssocs(MyDTM2, 'hiking', 0.20)

## VISUALIZE XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
## For Document Term Matrix...........
(CleanDF <- as.data.frame(inspect(MyTDM)))
## For Term Doc Matrix...................
(CleanDF2 <- as.data.frame(inspect(MyDTM2)))
(CleanDFScale2 <- scale(CleanDF2))

(CleanDFScale <- scale(CleanDF))
(d_TDM_E <- dist(CleanDFScale,method="euclidean"))
(d_TDM_M <- dist(CleanDFScale,method="minkowski", p=1))
(d_TDM_M3 <- dist(CleanDFScale,method="minkowski", p=3))

library(stylo)
(d_TDM_C <- stylo::dist.cosine(CleanDFScale))


################ Distance Metrics...############
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/dist
###########################################################
(d2_DT_E <- dist(CleanDF2,method="euclidean"))

str(d2_DT_E)
(d2_DT_M2 <- dist(CleanDF2,method="minkowski", p=2))  ##same as Euclidean
(d2_DT_Man <- dist(CleanDFScale2,method="manhattan"))
(d2_DT_M1 <- dist(CleanDFScale2,method="minkowski", p=1)) ## same as Manhattan
(d2_DT_M4 <- dist(CleanDFScale2,method="minkowski", p=4))
(d_DT_C <- stylo::dist.cosine(CleanDFScale2))
#################
## Create hierarchical clustering and dendrograms......................
#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/hclust
################

## Term Doc  - to look at words......
fit_TD1 <- hclust(d_TDM_E, method="ward.D2")
plot(fit_TD1)   ## Ward starts with n clusters of size 1 and then combines
## https://en.wikipedia.org/wiki/Ward%27s_method

fit_TD2 <- hclust(d_TDM_M, method="ward.D2")
plot(fit_TD2)


fit_TD3 <- hclust(d_TDM_C, method="ward.D2")
plot(fit_TD3)

## Doc Term - to look at documents....
fit_DT1 <- hclust(d2_DT_E, method="ward.D")  
plot(fit_DT1)

fit_DT2 <- hclust(d2_DT_Man, method="average")
plot(fit_DT2)  
## Average distance between two clusters is defined as the mean distance 
## between an observation in one cluster and an observation in the other cluster

fit_DT3 <- hclust(d2_DT_M4, method="ward.D2")
plot(fit_DT3)

fit_DT4 <- hclust(d_DT_C, method="ward.D2")
plot(fit_DT4)


## NOw I have a good matrix that allows me to see all the key words of interest 
## and their frequency in each document
## HOWEVER - I still need to normalize!
## Even though this example is very small and all docs in this example are about the
## same size, this will not always be the case. If a document has 10,000 words, it
## will easily have a greater frequency of words than a doc with 1000 words.


head(CleanDF2)
str(CleanDF2)

inspect(MyDTM2)
str(MyDTM2)

## Visualize normalized DTM
## The dendrogram:
## Terms higher in the plot appear more frequently within the corpus
## Terms grouped near to each other are more frequently found together
CleanDF_N <- as.data.frame(inspect(MyDTM2))
CleanDFScale_N <- scale(CleanDF_N)
(d <- dist(CleanDFScale_N,method="euclidean"))
fit <- hclust(d, method="ward.D2")
#rect.hclust(fit, k = 4) # cut tree into 4 clusters 
plot(fit)
