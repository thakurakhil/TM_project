####################################################
###
### Association Rule Mining Examples
### This example will use the Apriori Alg.
### 
### Dr. Gates 
####################################################
## To perform Association Rule Mining, transaction 
## data is needed. 
## MANY libraries will also be required... 
## 
## To perform association rule mining, you must have
## transaction data AND you must create a datafile
## that presents that data in whatever way your library
## expects. I will use *basket* here.
##
## 

## TO GET this code to work and to get arulesViz to work - 
## The HOW TO Tutorial for this is above
## you wil have to carefully and patiently do the following:

## DO these installs once
## install.packages("arules")
## For arulesViz to work on R version 3.5.x, you will
## need to first go through an installation of RTools. 
## See HOW TO above.
## Next - once the RTools exe has been downloaded and installed
## per the instructions, then, do these install.packages here in RStudio:
#install.packages("TSP")
#install.packages("data.table")
## NOTE: If you are asked if you want to INSTALL FROM SOURCE - click YES!
#install.packages("arulesViz", dependencies = TRUE)
## IMPORTANT ## arules ONLY grabs rules with ONE item on the right
## install.packages("sp")
## NOTE R V3.5.0 does not use the older
## datasets packages
## install.packages("datasets.load") - not used here
## install.packages("ggplot2") - not used here

## install.packages("dplyr", dependencies = TRUE)
## install.packages("purrr", dependencies = TRUE)
## install.packages("devtools", dependencies = TRUE)
## install.packages("tidyr")
library(viridis)
library(arules)
library(TSP)
library(data.table)
#library(ggplot2)
#library(Matrix)
library(tcltk)
library(dplyr)
library(devtools)
library(purrr)
library(tidyr)
## DO THIS ONCE
## FIRST - you MUST register and log into github
## install_github("mhahsler/arulesViz")
## RE: https://github.com/mhahsler/arulesViz

##############
## IF YOUR CODE BREAKS - TRY THIS
##
## Error in length(obj) : Method length not implemented for class rules 
## DO THIS: 
## (1) detach("package:arulesViz", unload=TRUE)
## (2) detach("package:arules", unload=TRUE)
## (3) library(arules)
## (4) library(arulesViz)
###################################################################

## To see if you have tcltk run this on the console...
# capabilities()["tcltk"]
library(arulesViz)

library(tm)
## YOUR working dir goes here...
setwd("/home/akhil/CUB/Spring 2023/TM/TM_project")

TheCorpus <- Corpus(DirSource("corpus"))

###################################
## Example 1: Kumar's Beer Example
##
## Dataset: https://drive.google.com/file/d/1BvU3iXtrwxfjBsRShaDgI-Ldh7Y3XSEn/view?usp=sharing
## KumarGroceriesAS_Transactions
##
## ## Dataset:
## HealthyBasket
## https://drive.google.com/file/d/1qaWSTwjrj7tNB43zLss9KC_ecJKp6W4g/view?usp=sharing
###################################################
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
write.csv(Cdataframe, "Corpus_output_arm.csv",col.names = FALSE, row.names = FALSE, sep= ",")

gunsfile = "MyTextOutfile_count.csv"

df = read.csv(gunsfile, stringsAsFactors = F, na.strings=c("","NA"))

TransactiongunsFile = "gunsResults.csv"

Trans = file(TransactiongunsFile)
Tokens = tokenizers::tokenize_words(df$Post[1], stopwords = stopwords::stopwords("en"), lowercase = TRUE, 
                                    strip_punct = TRUE, strip_numeric = TRUE, simplify = TRUE)


cat(unlist(str_squish(Tokens)), "\n", file = Trans, sep = ",")
close(Trans)

Trans = file(TransactiongunsFile, open="a" )
for(i in 2:nrow(df)){
  Tokens = tokenizers::tokenize_words(df$Post[i], stopwords = stopwords::stopwords("en"), lowercase = TRUE, 
                                      strip_punct = TRUE, strip_numeric = TRUE, simplify = TRUE)
  cat(unlist(str_squish(Tokens)), "\n", file = Trans, sep = ",")
  
}
close(Trans)

gunsDF = read.csv(TransactiongunsFile, header = FALSE, sep= ",")
head(gunsDF)

(str(gunsDF))

gunsDF = gunsDF %>%
  mutate_all(as.character)
(str(gunsDF))


## Cleaning with grepl
myDF = NULL
for(i in 1:ncol(gunsDF)){
  MyList = c()
  MyList = c(MyList, grepl("[[:digit:]]", gunsDF[[i]]))
  myDF = cbind(myDF, MyList)
}

## For all true replace with blank

gunsDF[myDF] <- ""
(gunsDF)


## More cleaning
myDF = NULL
myDF2 = NULL
myDF3 = NULL

for(i in 1:ncol(gunsDF)){
  MyList = c()
  MyList = c(MyList, grepl("[[:digit:]]", gunsDF[[i]]))
  
  MyList2 = c()
  MyList2 = c(MyList, grepl("[A-Z]{4,}", gunsDF[[i]]))
  
  MyList3 = c()
  MyList3 = c(MyList, grepl("[A-Z]{12,}", gunsDF[[i]]))
  
  
  myDF = cbind(myDF, MyList)
  myDF2 = cbind(myDF, MyList2)
  myDF3 = cbind(myDF, MyList3)
  
}

gunsDF[myDF] <- ""
gunsDF[!myDF2] <- ""
gunsDF[myDF] <- ""
(gunsDF)


head(gunsDF, 10)

sortedRules_conf = sort(gunsTrans_rules, by="confidence", decreasing = TRUE)
sortedRules_sup = sort(gunsTrans_rules, by="support", decreasing = TRUE)
plot(sortedRules_sup[1:25], method = "graph", engine = 'interactive', shading = "confidence")
plot(sortedRules_sup[1:30], method = "graph", engine = 'htmlwidget', shading = "confidence")


