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
setwd("/Users/vitor/Documents/DataAnalyst/CouseraCourse/10-DataScienceCapstone")
#
#
# Sampling
#  obs: filesEspec.df is created in project.rmd
#
load(file=paste0(getwd(),"/data/filesEspec.df"))
dirsample=paste0(getwd(),"/data3")
if (!dir.exists(dirsample)) {
  dir.create(dirsample)
}
dirsample=paste0(dirsample,"/final_sampled")
if (!dir.exists(dirsample)) {
  dir.create(dirsample)
}
dirsample=paste0(dirsample,"/en_US")
if (!dir.exists(dirsample)) {
  dir.create(dirsample)
}
# 
dirFullTexts=paste0(getwd(),"/data/final/en_US")
list.files(dirFullTexts,recursive=TRUE,include.dirs=TRUE)

fsamplesize.list=rep(0,nrow(df))
set.seed(234)
for (i in 1:nrow(df)) {
  inputfilename = paste(dirFullTexts,df$FileName[i],sep="/")
  con <- file(inputfilename, "r")
  filelinesRead.list=readLines(con)
  close(con)
  thirtyPerCent = ceiling(df$No.Lines[i] * 0.3)
  toWrite = sample(1:df$No.Lines[i], thirtyPerCent, replace=FALSE)
  filelinesWrite.list = filelinesRead.list[toWrite]
  outputfilename = paste(dirsample,df$FileName[i],sep="/")
  con <- file(outputfilename, "w")
  writeLines(filelinesWrite.list,con,sep = "\n")
  close(con)
  fsamplesize.list[i] = file.info(outputfilename)[[1]]
}
dfs=data.frame(FileName=df$FileName,
               OriginalSize=round(df$FileSize/1000/1000,1),
               SampleSize=round(fsamplesize.list/1000/1000,1),
               SampleLines=ceiling(df$No.Lines * 0.3))
save(dfs,file="filesEspecS.df")
dfs
list.files(dirsample,recursive=TRUE,include.dirs=TRUE)
#
# creating training and teste datasets
# 
dirTrain=paste0(getwd(),"/data3","/train")
if (!dir.exists(dirTrain)) {
  dir.create(dirTrain)
}
dirTrain=paste0(dirTrain,"/en_US")
if (!dir.exists(dirTrain)) {
  dir.create(dirTrain)
}
#
dirTest=paste0(getwd(),"/data3","/test")
if (!dir.exists(dirTest)) {
  dir.create(dirTest)
}
dirTest=paste0(dirTest,"/en_US")
if (!dir.exists(dirTest)) {
  dir.create(dirTest)
}
#
list.files(dirsample,recursive=TRUE,include.dirs=TRUE)

ftrainsize.list=rep(0,nrow(dfs))
ftestsize.list=rep(0,nrow(dfs))
set.seed(234)
for (i in 1:nrow(dfs)) {
  inputfilename = paste(dirsample,dfs$FileName[i],sep="/")
  con <- file(inputfilename, "r")
  filelinesRead.list=readLines(con)
  close(con)
  thirtyPerCent = ceiling(dfs$SampleLines[i] * 0.3)
  toTest = sample(1:dfs$SampleLines[i], thirtyPerCent, replace=FALSE)
# gravando test
  filelinesWrite.list = filelinesRead.list[toTest]
  outputfilename = paste(dirTest,dfs$FileName[i],sep="/")
  con <- file(outputfilename, "w")
  writeLines(filelinesWrite.list,con,sep = "\n")
  close(con)
  ftestsize.list[i] = file.info(outputfilename)[[1]]
#gravando trainning
  filelinesWrite.list = filelinesRead.list[-toTest]
  outputfilename = paste(dirTrain,dfs$FileName[i],sep="/")
  con <- file(outputfilename, "w")
  writeLines(filelinesWrite.list,con,sep = "\n")
  close(con)
  ftrainsize.list[i] = file.info(outputfilename)[[1]]
}
dfs2=data.frame(FileName=dfs$FileName,
               OriginalSize=dfs$OriginalSize,
               SampleSize=dfs$SampleSize,
               Train.Size=round(ftrainsize.list/1000/1000,1),
               Test.Size=round(ftestsize.list/1000/1000,1)
)
save(dfs2,file="filesEspecS2.df")
dfs2
list.files(dirTrain,recursive=TRUE,include.dirs=TRUE)
list.files(dirTest,recursive=TRUE,include.dirs=TRUE)
