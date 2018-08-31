#Initial settings
rm(list=ls())
options(java.parameters = "-Xmx8192m")
library(rJava)
library(tm)
library("RWeka")
library(plyr)
library(tidytext)
library(sqldf)
library(data.table)
setwd("/Users/vitor/Documents/DataAnalyst/CouseraCourse/10-DataScienceCapstone/data3")
#
# Creating train Corpus
dirTrain=paste0(getwd(),"/train/en_US")
corpusTrain <- VCorpus(DirSource(dirTrain))
print(paste0("Train Corpus: ", round(object.size(corpusTrain)/1000000,1), " Mb"))
dirCorpus=paste0(getwd(),"/corpus")
if (!dir.exists(dirCorpus)) {
  dir.create(dirCorpus)
}
save(corpusTrain,file=paste0(dirCorpus,"/corpusTrain.data"))
#
# Creating test Corpus
dirTest=paste0(getwd(),"/test/en_US")
corpusTest <- VCorpus(DirSource(dirTest))
print(paste0("Test Corpus: ", round(object.size(corpusTest)/1000000,1), " Mb"))
save(corpusTest,file=paste0(dirCorpus,"/corpusTest.data"))
#
# Auxiliary functions for cleaning the corpus
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ", x))})
addSpace <- content_transformer(function(x, p) {return (gsub(paste0("[",p,"]"),paste0(" ",p," "), x))})
#
# Cleanig Corpus fucntion
cleanCorpus = function(corpus) {
  newCorpus <- tm_map(corpus, content_transformer(tolower))
  newCorpus <- tm_map(newCorpus,  content_transformer(removeURL))
  newCorpus <- tm_map(newCorpus, addSpace,".") 
  newCorpus <- tm_map(newCorpus, addSpace,",")
  newCorpus <- tm_map(newCorpus, addSpace,":") 
  newCorpus <- tm_map(newCorpus, addSpace,";") 
  newCorpus <- tm_map(newCorpus, removeNumbers)  
  newCorpus <- tm_map(newCorpus, toSpace,"[^\x20-\x7F]") 
  newCorpus <- tm_map(newCorpus, stripWhitespace)
  newCorpus
}
#
# Cleanig train Corpus
  corpusTrainClean = cleanCorpus(corpusTrain)
#Saving train Corpus cleanned
  save(corpusTrainClean,file=paste0(dirCorpus,"/corpusTrainClean.data"))
#
# Cleanig test Corpus
  corpusTestClean = cleanCorpus(corpusTest)
#Saving test Corpus cleanned
  save(corpusTestClean,file=paste0(dirCorpus,"/corpusTestClean.data"))
##
#
createNGram = function(corpus,N) {
  if (N==1) {
    dtm <- DocumentTermMatrix(corpus)
  } else {
    tokenizer = function(x) NGramTokenizer(x, Weka_control(min = N, max = N))
    dtm = DocumentTermMatrix(corpus, control = list(tokenize = tokenizer))
  }
  gramFreq=colSums(as.matrix(dtm))
  auxDtNgram = as.data.table(tidy(gramFreq))
  if (N==1) {
    names(auxDtNgram)=c("term1","count")
    return(auxDtNgram)
  } else {
    termList = list()
    names(auxDtNgram)=c("terms","count")
    lWords=strsplit(auxDtNgram$terms," ")
    validgrams=sapply(lWords, function(x) {if (length(x)==N) {TRUE} else {FALSE}})
    dtNgram=data.table(count=auxDtNgram$count[validgrams])
    for (i in 1:N) {
      colName=quote(paste0("term",as.character(i)))
      dtNgram=dtNgram[,eval(colName):=sapply(lWords[validgrams], function(x) {x[[i]]})]
    }
    nameColumns=names(dtNgram)
    numColumns=length(nameColumns)
    nameColumns=c(nameColumns[2:numColumns],nameColumns[1])
    setcolorder(dtNgram,nameColumns)
    return(dtNgram)
  }
}
#
# Creating de  train n-grams
dirTrainNgrams=paste0(getwd(),"/train-ngrams")
if (!dir.exists(dirTrainNgrams)) {
  dir.create(dirTrainNgrams)
}
# 1-gram
print(paste0("Criando o 1-gram"))
dt1gram=createNGram(corpusTrainClean,1)
save(dt1gram,file=paste0(dirTrainNgrams,"/dt1gram.data"))
# 2-gram
print(paste0("Criando o 2-gram"))
dt2gram=createNGram(corpusTrainClean,2)
save(dt2gram,file=paste0(dirTrainNgrams,"/dt2gram.data"))
# 3-gram
print(paste0("Criando o 3-gram"))
dt3gram=createNGram(corpusTrainClean,3)
save(dt3gram,file=paste0(dirTrainNgrams,"/dt3gram.data"))
# 4-gram
print(paste0("Criando o 4-gram"))
dt4gram=createNGram(corpusTrainClean,4)
save(dt4gram,file=paste0(dirTrainNgrams,"/dt4gram.data"))
# 5-gram
print(paste0("Criando o 5-gram"))
dt5gram=createNGram(corpusTrainClean,5)
save(dt5gram,file=paste0(dirTrainNgrams,"/dt5gram.data"))
#
# Creating de  test n-gram ( only the 5-gram)
dirTestNgrams=paste0(getwd(),"/test-ngrams")
if (!dir.exists(dirTestNgrams)) {
  dir.create(dirTestNgrams)
}
# 5-gram
print(paste0("Criando o 5-gram test"))
test5gram=createNGram(corpusTestClean,5)
save(test5gram,file=paste0(dirTestNgrams,"/test5gram.data"))
#
#
# Filtering trash
# --------------------------------------------------------------------------------------
# dt1gram
nri=nrow(dt1gram)
dt1gram=dt1gram[!(term1 %like% "[\x21-\x26]")]
dt1gram=dt1gram[!(term1 %like% "[\x28-\x2F]")]
dt1gram=dt1gram[!(term1 %like% "[\x3A-\x40]")]
dt1gram=dt1gram[!(term1 %like% "[\x5B-\x60]")]
dt1gram=dt1gram[!(term1 %like% "[\x7B-\x7F]")]
dt1gram=dt1gram[!(term1 %like% "aa")]
dt1gram=dt1gram[!(term1 %like% "zz")]
nrf=nrow(dt1gram)
print(sprintf("Deleted rows [%i]", (nrf-nri)))
save(dt1gram,file=paste0(dirTrainNgrams,"/fdt1gram.data"))
#
# dt2gram
nri=nrow(dt1gram)
dt2gram=dt2gram[!(term1 %like% "[\x21-\x26]") & !(term2 %like% "[\x21-\x26]")]
dt2gram=dt2gram[!(term1 %like% "[\x28-\x2F]") & !(term2 %like% "[\x28-\x2F]")]
dt2gram=dt2gram[!(term1 %like% "[\x3A-\x40]") & !(term2 %like% "[\x3A-\x40]")]
dt2gram=dt2gram[!(term1 %like% "[\x5B-\x60]") & !(term2 %like% "[\x5B-\x60]")]
dt2gram=dt2gram[!(term1 %like% "[\x7B-\x7F]") & !(term2 %like% "[\x7B-\x7F]")]
dt2gram=dt2gram[!(term1 %like% "aa") & !(term2 %like% "aa")]
dt2gram=dt2gram[!(term1 %like% "zz") & !(term2 %like% "zz")]
nrf=nrow(dt2gram)
print(sprintf("Deleted rows [%i]", (nrf-nri)))
save(dt2gram,file=paste0(dirTrainNgrams,"/fdt2gram.data"))
#
# dt3gram
nri=nrow(dt3gram)
dt3gram=dt3gram[!(term1 %like% "[\x21-\x26]") & !(term2 %like% "[\x21-\x26]") & !(term3 %like% "[\x21-\x26]") ]
dt3gram=dt3gram[!(term1 %like% "[\x28-\x2F]") & !(term2 %like% "[\x28-\x2F]") & !(term3 %like% "[\x28-\x2F]") ]
dt3gram=dt3gram[!(term1 %like% "[\x3A-\x40]") & !(term2 %like% "[\x3A-\x40]") & !(term3 %like% "[\x3A-\x40]") ]
dt3gram=dt3gram[!(term1 %like% "[\x5B-\x60]") & !(term2 %like% "[\x5B-\x60]") & !(term3 %like% "[\x5B-\x60]") ]
dt3gram=dt3gram[!(term1 %like% "[\x7B-\x7F]") & !(term2 %like% "[\x7B-\x7F]") & !(term3 %like% "[\x7B-\x7F]") ]
dt3gram=dt3gram[!(term1 %like% "aa") & !(term2 %like% "aa") & !(term3 %like% "aa")]
dt3gram=dt3gram[!(term1 %like% "zz") & !(term2 %like% "zz") & !(term3 %like% "zz")]
nrf=nrow(dt3gram)
print(sprintf("Deleted rows [%i]", (nrf-nri)))
save(dt3gram,file=paste0(dirTrainNgrams,"/fdt3gram.data"))
#
# dt4gram
nri=nrow(dt4gram)
dt4gram=dt4gram[!(term1 %like% "[\x21-\x26]") & !(term2 %like% "[\x21-\x26]") & !(term3 %like% "[\x21-\x26]") & !(term4 %like% "[\x21-\x26]") ]
dt4gram=dt4gram[!(term1 %like% "[\x28-\x2F]") & !(term2 %like% "[\x28-\x2F]") & !(term3 %like% "[\x28-\x2F]") & !(term4 %like% "[\x28-\x2F]") ]
dt4gram=dt4gram[!(term1 %like% "[\x3A-\x40]") & !(term2 %like% "[\x3A-\x40]") & !(term3 %like% "[\x3A-\x40]") & !(term4 %like% "[\x3A-\x40]") ]
dt4gram=dt4gram[!(term1 %like% "[\x5B-\x60]") & !(term2 %like% "[\x5B-\x60]") & !(term3 %like% "[\x5B-\x60]") & !(term4 %like% "[\x5B-\x60]") ]
dt4gram=dt4gram[!(term1 %like% "[\x7B-\x7F]") & !(term2 %like% "[\x7B-\x7F]") & !(term3 %like% "[\x7B-\x7F]") & !(term4 %like% "[\x7B-\x7F]") ]
dt4gram=dt4gram[!(term1 %like% "aa") & !(term2 %like% "aa") & !(term3 %like% "aa") & !(term4 %like% "aa")]
dt4gram=dt4gram[!(term1 %like% "zz") & !(term2 %like% "zz") & !(term3 %like% "zz") & !(term4 %like% "zz")]
nrf=nrow(dt4gram)
print(sprintf("Deleted rows [%i]", (nrf-nri)))
save(dt4gram,file=paste0(dirTrainNgrams,"/fdt4gram.data"))
#
# dt5gram
nri=nrow(dt5gram)
dt5gram=dt5gram[!(term1 %like% "[\x21-\x26]") & !(term2 %like% "[\x21-\x26]") & !(term3 %like% "[\x21-\x26]") & !(term4 %like% "[\x21-\x26]") & !(term5 %like% "[\x21-\x26]") ]
dt5gram=dt5gram[!(term1 %like% "[\x28-\x2F]") & !(term2 %like% "[\x28-\x2F]") & !(term3 %like% "[\x28-\x2F]") & !(term4 %like% "[\x21-\x26]") & !(term5 %like% "[\x21-\x26]") ]
dt5gram=dt5gram[!(term1 %like% "[\x3A-\x40]") & !(term2 %like% "[\x3A-\x40]") & !(term3 %like% "[\x3A-\x40]") & !(term4 %like% "[\x3A-\x40]") & !(term5 %like% "[\x3A-\x40]") ]
dt5gram=dt5gram[!(term1 %like% "[\x5B-\x60]") & !(term2 %like% "[\x5B-\x60]") & !(term3 %like% "[\x5B-\x60]") & !(term4 %like% "[\x5B-\x60]") & !(term5 %like% "[\x5B-\x60]") ]
dt5gram=dt5gram[!(term1 %like% "[\x7B-\x7F]") & !(term2 %like% "[\x7B-\x7F]") & !(term3 %like% "[\x7B-\x7F]") & !(term4 %like% "[\x7B-\x7F]") & !(term5 %like% "[\x7B-\x7F]") ]
dt5gram=dt5gram[!(term1 %like% "aa") & !(term2 %like% "aa") & !(term3 %like% "aa") & !(term4 %like% "aa") & !(term5 %like% "aa")]
dt5gram=dt5gram[!(term1 %like% "zz") & !(term2 %like% "zz") & !(term3 %like% "zz") & !(term4 %like% "zz") & !(term5 %like% "zz")]
nrf=nrow(dt5gram)
print(sprintf("Deleted rows [%i]", (nrf-nri)))
save(dt5gram,file=paste0(dirTrainNgrams,"/fdt5gram.data"))
#
# test5gram
nri=nrow(test5gram)
test5gram=test5gram[!(term1 %like% "[\x21-\x26]") & !(term2 %like% "[\x21-\x26]") & !(term3 %like% "[\x21-\x26]") & !(term4 %like% "[\x21-\x26]") & !(term5 %like% "[\x21-\x26]") ]
test5gram=test5gram[!(term1 %like% "[\x28-\x2F]") & !(term2 %like% "[\x28-\x2F]") & !(term3 %like% "[\x28-\x2F]") & !(term4 %like% "[\x21-\x26]") & !(term5 %like% "[\x21-\x26]") ]
test5gram=test5gram[!(term1 %like% "[\x3A-\x40]") & !(term2 %like% "[\x3A-\x40]") & !(term3 %like% "[\x3A-\x40]") & !(term4 %like% "[\x3A-\x40]") & !(term5 %like% "[\x3A-\x40]") ]
test5gram=test5gram[!(term1 %like% "[\x5B-\x60]") & !(term2 %like% "[\x5B-\x60]") & !(term3 %like% "[\x5B-\x60]") & !(term4 %like% "[\x5B-\x60]") & !(term5 %like% "[\x5B-\x60]") ]
test5gram=test5gram[!(term1 %like% "[\x7B-\x7F]") & !(term2 %like% "[\x7B-\x7F]") & !(term3 %like% "[\x7B-\x7F]") & !(term4 %like% "[\x7B-\x7F]") & !(term5 %like% "[\x7B-\x7F]") ]
test5gram=test5gram[!(term1 %like% "aa") & !(term2 %like% "aa") & !(term3 %like% "aa") & !(term4 %like% "aa") & !(term5 %like% "aa")]
test5gram=test5gram[!(term1 %like% "zz") & !(term2 %like% "zz") & !(term3 %like% "zz") & !(term4 %like% "zz") & !(term5 %like% "zz")]
nrf=nrow(test5gram)
print(sprintf("Deleted rows [%i]", (nrf-nri)))
save(test5gram,file=paste0(dirTestNgrams,"/ftest5gram.data"))
#
# Eliminating  the rows with low counts
#
dt1gramF=dt1gram
dt2gramF=dt2gram
dt3gramF=dt3gram[count>3]
dt4gramF=dt4gram[count>3]
dt5gramF=dt5gram[count>3]
#
# Saving a RDS version for fast read
# --------------------------------------------------------------------------------------
saveRDS(dt1gramF,file=paste0(dirTrainNgrams,"/ddt1gramF.rds"))
saveRDS(dt2gramF,file=paste0(dirTrainNgrams,"/ddt2gramF.rds"))
saveRDS(dt3gramF,file=paste0(dirTrainNgrams,"/ddt3gramF.rds"))
saveRDS(dt4gramF,file=paste0(dirTrainNgrams,"/ddt4gramF.rds"))
saveRDS(dt5gramF,file=paste0(dirTrainNgrams,"/ddt5gramF.rds"))
#
print("END")


