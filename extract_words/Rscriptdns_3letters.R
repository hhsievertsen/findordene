rm(list=ls())
setwd("C:\\Users\\hs17922\\Dropbox\\Private documents\\findordene\\admin\\extract_words")
#setwd("~/Dropbox/findordene/archive")
library("tidyverse")
library("stringi")
# load data
df<-read_delim("RO2012 fuldformsliste 2020.csv",delim = ";",col_names=FALSE)
out<-data.frame(ord=c(df$X1,df$X2))

# remove short words
df1<-out%>%filter(nchar(ord)>2)
# remove if it contains any special symbols
letters_only <- function(x) !grepl("[^A-Za-z]", x)
df2<-df1%>%filter(letters_only(ord))
# make all letters lowcase
df3<-df2%>%mutate(ord=tolower(ord))
# no duplicates
df4<-unique(df3)
# sample the key letter
letters<-c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","?","?","?")
set.seed(1909)
drwords <- function() {
  wordletters<-sample(letters,4,replace = FALSE)
  keyletter<-wordletters[1]
  nonletters<-letters[!letters%in%wordletters]
  # remove words not containing keyletter
  df5<-df4%>%filter(stri_detect_fixed(ord, keyletter))
  # remove words containing other letters
  for (i in 1:25){
    df5<-df5%>%filter(!stri_detect_fixed(ord, nonletters[i]))
  }
  # check if all letters used
  for (i in 1:4){
    a<-nrow(df5%>%filter(stri_detect_fixed(ord, wordletters[i])))
    if (a==0){
      df5<-df5%>%filter(ord=="bdsvbsa")
    }
  }
  # check if at least one uses all letters
  df6<-df5%>%mutate(number=0)
  for (i in 1:4){
    df6<-df6%>%mutate(number=ifelse(str_detect(ord,wordletters[i]),number+1,number))
  }
  b<-nrow(df6%>%filter(number==4))
  if (b==0){
    df6<-df6%>%filter(ord=="bdsvbsa")
  }
  return(wordlist=list(wordletters,df6))
}


N=500
x <- vector("list", N)
outdata<-data.frame(wordlist=NULL,worldlist=NULL)
count<-0
for(i in 1:N) {
  # check if sufficient words
  a=0
  while (a==0){
    Ps <- drwords()
    if (nrow(Ps[[2]])>2 & nrow(Ps[[2]])<16){
        a=1
    }
  }
  count=count+1
  print(count)
  # save letters
  outdata[i,1] <- paste(Ps[[1]],collapse=",")
  abs<-Ps[[2]]
  outdata[i,2] <- paste(as.vector(unlist(abs["ord"])),collapse=",")
  # save words
}
write_csv(outdata,"rawdata3.csv")

library(jsonlite)
letters<-toJSON(outdata$V1, dataframe="values")
words<-toJSON(outdata$V2, dataframe="values")
write(letters, "letters3.json")
write(words, "words3.json")