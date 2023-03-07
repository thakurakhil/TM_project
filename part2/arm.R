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

TransactionredditFile = "RedditResults.csv"

Trans = file(TransactionredditFile)
Tokens = tokenizers::tokenize_words(df$Post[1], stopwords = stopwords::stopwords("en"), lowercase = TRUE, 
                                    strip_punct = TRUE, strip_numeric = TRUE, simplify = TRUE)


cat(unlist(str_squish(Tokens)), "\n", file = Trans, sep = ",")
close(Trans)

Trans = file(TransactionredditFile, open="a" )
for(i in 2:nrow(df)){
  Tokens = tokenizers::tokenize_words(df$Post[i], stopwords = stopwords::stopwords("en"), lowercase = TRUE, 
                                      strip_punct = TRUE, strip_numeric = TRUE, simplify = TRUE)
  cat(unlist(str_squish(Tokens)), "\n", file = Trans, sep = ",")
  
}
close(Trans)

redditDF = read.csv(TransactionredditFile, header = FALSE, sep= ",")
head(redditDF)

(str(redditDF))

redditDF = redditDF %>%
  mutate_all(as.character)
(str(redditDF))


## Cleaning with grepl
myDF = NULL
for(i in 1:ncol(redditDF)){
  MyList = c()
  MyList = c(MyList, grepl("[[:digit:]]", redditDF[[i]]))
  myDF = cbind(myDF, MyList)
}

## For all true replace with blank

redditDF[myDF] <- ""
(redditDF)


## More cleaning
myDF = NULL
myDF2 = NULL
myDF3 = NULL

for(i in 1:ncol(redditDF)){
  MyList = c()
  MyList = c(MyList, grepl("[[:digit:]]", redditDF[[i]]))
  
  MyList2 = c()
  MyList2 = c(MyList, grepl("[A-Z]{4,}", redditDF[[i]]))
  
  MyList3 = c()
  MyList3 = c(MyList, grepl("[A-Z]{12,}", redditDF[[i]]))
  
  
  myDF = cbind(myDF, MyList)
  myDF2 = cbind(myDF, MyList2)
  myDF3 = cbind(myDF, MyList3)
  
}

redditDF[myDF] <- ""
redditDF[!myDF2] <- ""
redditDF[myDF] <- ""
(redditDF)


head(redditDF, 10)








gunsdata <- read.transactions("MyTextOutfile_count.csv",
                                rm.duplicates = TRUE, 
                                format = "basket",  ##if you use "single" also use cols=c(1,2)
                                sep=" ")  ## csv file) ## The dataset HAS row numbers
inspect(gunsdata)

##### Use apriori to get the RULES
FrulesK = arules::apriori(gunsdata, parameter = list(support=.35, 
                                                       confidence=.5, minlen=2))
inspect(FrulesK)

## Plot of which items are most frequent
itemFrequencyPlot(FoodsKumar, topN=20, type="absolute")

## Sort rules by a measure such as conf, sup, or lift
SortedRulesK <- sort(FrulesK, by="confidence", decreasing=TRUE)
inspect(SortedRulesK[1:3])
(summary(SortedRulesK))

## Selecting or targeting specific rules  RHS
BeerRules <- apriori(data=FoodsKumar,parameter = list(supp=.001, conf=.01, minlen=2),
                     appearance = list(default="lhs", rhs="Beer"),
                     control=list(verbose=FALSE))
BeerRules <- sort(BeerRules, decreasing=TRUE, by="confidence")
inspect(BeerRules[1:4])

## Selecting rules with LHS specified
BreadRules <- apriori(data=FoodsKumar,parameter = list(supp=.001, conf=.01, minlen=2),
                      appearance = list(default="rhs", lhs="Bread"),
                      control=list(verbose=FALSE))
BreadRules <- sort(BreadRules, decreasing=TRUE, by="support")
inspect(BreadRules[1:4])

## Visualize
## tcltk

subrulesK <- head(sort(SortedRulesK, by="lift"),10)
plot(subrulesK)

plot(subrulesK, method="graph", engine="interactive")




####################################################
## Example 2: Healthy Food Transaction Data
##
## HERE IS THE DATA - but you should make your own dataset!
## https://drive.google.com/file/d/1qaWSTwjrj7tNB43zLss9KC_ecJKp6W4g/view?usp=sharing
##
##############################################################

Foods <- read.transactions("HealthyBasketData.csv",
                           rm.duplicates = FALSE, 
                           format = "basket",  ##if you use "single" also use cols=c(1,2)
                           sep=",",  ## csv file
                           cols=NULL) ## The dataset has no row numbers
inspect(Foods)

##### Use apriori to get the RULES
Frules = arules::apriori(Foods, parameter = list(support=.35, 
                                                 confidence=.5, minlen=2))
inspect(Frules)

## Plot of which items are most frequent
itemFrequencyPlot(Foods, topN=20, type="absolute")

## Sort rules by a measure such as conf, sup, or lift
SortedRules <- sort(Frules, by="confidence", decreasing=TRUE)
inspect(SortedRules[1:10])
(summary(SortedRules))

## Selecting or targeting specific rules  RHS
ChocRules <- apriori(data=Foods,parameter = list(supp=.001, conf=.01, minlen=2),
                     appearance = list(default="lhs", rhs="chocloate"),
                     control=list(verbose=FALSE))
ChocRules <- sort(ChocRules, decreasing=TRUE, by="confidence")
inspect(ChocRules[1:4])

## Selecting rules with LHS specified
CarrotRules <- apriori(data=Foods,parameter = list(supp=.001, conf=.01, minlen=2),
                       appearance = list(default="rhs", lhs="carrot"),
                       control=list(verbose=FALSE))
CarrotRules <- sort(CarrotRules, decreasing=TRUE, by="support")
inspect(CarrotRules[1:4])

## Visualize
## tcltk

subrules <- head(sort(SortedRules, by="lift"),10)
plot(subrules)

#plot(subrules, method="graph", engine="interactive")
plot(subrules, method="graph", engine="htmlwidget")
