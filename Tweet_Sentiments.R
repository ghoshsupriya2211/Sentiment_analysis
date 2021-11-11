
#Basic Tweet Statistics and Sentiment Analysis
library(tidyverse)
# for Jaccard distance
#install.packages("stringdist")
library(stringdist)
# for sentiment analysis
#install.packages('sentimentr')
library(sentimentr)
# for textmining
library(tm)
library(wordcloud)
# load data
t1 <- Sys.time()
df_train <- read.csv("train.csv")
df_test <- read.csv("test.csv")
df_sub <- read.csv("sample_submission.csv")
t2 <- Sys.time()
print(t2-t1)

# Exploration of Training Data
# show distribution of sentiments
df <- df_train
df$sentiment <- as.factor(df$sentiment)
plot(df$sentiment)
# evaluate length of text and selected text
df$text <- as.character(df$text)
df$selected_text <- as.character(df$selected_text)
df$length <- str_length(df$text)
df$length_sel <- str_length(df$selected_text)
# remove empty entries from training
df <- dplyr::filter(df, length > 0)
# evaluate length of text
summary(df$length)
hist(df$length, main='Length of Text - Training Set')
# same for selected text
summary(df$length_sel)
hist(df$length_sel, main='Length of Selected Text - Training Set')
# Ratio of selected text vs full text
df$length_ratio <- df$length_sel / df$length
hist(df$length_ratio, main='Length Ratio Selected Text vs Full Text')
# Find start and end position of selected text in text
fun_find_first <- function(text, subtext) {
  foo <- stringr::str_locate(text, fixed(subtext))
  return(foo[1])
}
fun_find_second <- function(text, subtext) {
  foo <- stringr::str_locate(text, fixed(subtext))
  return(foo[2])
}
n <- nrow(df)
as <- 1:n
bs <- 1:n
for (i in 1:n) {
  # print(i)
  text <- df$text[i]
  subtext <- df$selected_text[i]
  a <- fun_find_first(text, subtext)
  b <- fun_find_second(text, subtext)
  as[i] <- a
  bs[i] <- b
}
df$begin <- as
df$end <- bs
df$begin_rel <- df$begin / df$length
df$end_rel <- df$end / df$length
summary(df$begin_rel)
hist(df$begin_rel, main='Relative position of begin of selected text')
summary(df$end_rel)
hist(df$end_rel, main='Relative position of end of selected text')
# Jaccard distances
df$jac <- stringdist::stringdist(df$text, df$selected_text, method='jaccard')
summary(df$jac)
hist(df$jac, 100, main='Jaccard distance selected vs full text')














