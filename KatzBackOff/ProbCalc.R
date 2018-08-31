# Initial settings
rm(list=ls())
options(java.parameters = "-Xmx8192m")
library(rJava)
library(tm)
library("RWeka")
library(plyr)
library(tidytext)
library(sqldf)
library(data.table)


#
# --------------------------------------------------------------------------------------
# Calculate the probability of a word complete the text
# These n-grams should be contained in DataTable 
# These n-grams are contained in DataTable with the following format
# 1-gram   -> term1 (character), count (integer)
# 2-gram   -> term1 (character), term2 (character), count (integer)
# 3-gram   -> term1 (character), term2 (character), term3 (character), count (integer)
# 4-gram   -> term1 (character), term2 (character), term3 (character), term4 (character), count (integer)
# 5-gram   -> term1 (character), term2 (character), term3 (character), term4 (character), term5 (character), count (integer)
# For each of this n-gram will be added a whole column to contain the value of the descounto and also will be created a dataTable of leftovers
# These DataTables objects must have the following names
# 1-gram -> dt1gram
# 2-gram -> dt2gram
# 3-gram -> dt3gram
# 4-gram -> dt4gram
# 5-gram -> dt5gram



#-------------------------------------------------------------------------

#
# Katz Backoff
#
# The following routines have been adapted from the implementation contained in 
# https://github.com/ThachNgocTran/KatzBackOffModelImplementationInR
#
# frase => text entered
# inputTerms => vector where the words of the text are store each word in one vector element

#------------------------------------------------------------------------------------------ 

# Analyze a set of words contained in ioptions to check the probabilities of each to continue the phrase (frase)
testOptions= function(frase,options) {
  print(sprintf("frase [%s] - Option=[%s]", frase,paste(options, collapse = ', ')))
  vProb = vector()
  for (i in 1:length(options)) {
    vProb[i]=getProbabilityFromNGram(paste(frase,options[i]))
  }
  df=data.frame(option=options,probability=vProb)
  print(df)
  df=df[order(-df$probability),]
  df[,]
}

getProbabilityFromNGram = function(frase) {
  # Preprocessing
  nfrase=cleanFrase(frase)
  inputTerms=toVector(nfrase)
  if(flagDebug)  print(inputTerms)
  # getting the probabilities
  probability=getProb5gramI(inputTerms)
  if (probability>0) {
    return(probability)
  } else {
    stop(sprintf("[%s] not found in the 5-gram model.", frase))
  }
}


# 1 gram ------------------
getProb1gram = function(inputTerms,MatchAllIn2gram,beta_leftoverprob) {
  if(flagDebug)  print("getProb1gram")
  inputTerms=setLength(inputTerms,1)
  if(flagDebug)   print(inputTerms)
  Match0In1gram=dt1gram
  MatchAllIn1gram=dt1gram[term1==inputTerms[1]]
  RemainMatch0In1gram = Match0In1gram[!(MatchAllIn1gram$term1 %in% MatchAllIn2gram$term2)]
  all_freq = sum(Match0In1gram$count)
  alpha=beta_leftoverprob/sum((RemainMatch0In1gram$count*RemainMatch0In1gram$discount)/all_freq)  
  probability=alpha * ((MatchAllIn1gram$count * MatchAllIn1gram$discount) / all_freq)
  return(probability)
}

getProb1gramI = function(inputTerms,MatchAllIn2gram) {
  if(flagDebug)  print("getProb1gramI")
  inputTerms=setLength(inputTerms,1)
  if(flagDebug)   print(inputTerms)
  Match0In1gram=dt1gram
  MatchAllIn1gram=dt1gram[ term1==inputTerms[1]]
  if (nrow(MatchAllIn1gram)>0) {
    all_freq = sum(Match0In1gram$count)
    probability = ((MatchAllIn1gram$count * MatchAllIn1gram$discount) / all_freq)
    return(probability)
  } else {
    return(-1)
  }  
}

# 2 gram ------------------
getProb2gram = function(inputTerms,MatchAllIn3gram,beta_leftoverprob) {
  if(flagDebug)  print("getProb2gram")
  inputTerms=setLength(inputTerms,2)
  if(flagDebug)  print(inputTerms)
  Match1In2gram=dt2gram[term1==inputTerms[1]]
  MatchAllIn2gram=dt2gram[ term1==inputTerms[1] & term2==inputTerms[2]]
  if (nrow(MatchAllIn2gram)>0) {
    RemainMatch1In2gram = Match1In2gram[!(MatchAllIn2gram$term2 %in% MatchAllIn3gram$term3)]
    all_freq = sum(Match1In2gram$count)
    alpha = beta_leftoverprob  / sum((RemainMatch1In2gram$count * RemainMatch1In2gram$discount) / all_freq)  
    probability=alpha * ((MatchAllIn2gram$count * MatchAllIn2gram$discount) / all_freq)
    return(probability)
  } else {
    return(getProb1gram(inputTerms,MatchAllIn2gram,beta_leftoverprob))
  }            
}

getProb2gramI = function(inputTerms,MatchAllIn3gram) {
  if(flagDebug)  print("getProb2gramI")
  inputTerms=setLength(inputTerms,2)
  if(flagDebug)  print(inputTerms)
  Match1In2gram=dt2gram[term1==inputTerms[1]]
  if (nrow(Match1In2gram)>0) {
    MatchAllIn2gram=dt2gram[ term1==inputTerms[1] & term2==inputTerms[2]]
    if (nrow(MatchAllIn2gram)>0) {
      all_freq = sum(Match1In2gram$count)
      probability=((MatchAllIn2gram$count * MatchAllIn2gram$discount) / all_freq)
      return(probability)
    } else {
      beta_leftoverprob = lo2gram[term1==inputTerms[1]]$leftOver
      return(getProb1gram(inputTerms,MatchAllIn2gram,beta_leftoverprob))
    }            
  } else {
    return(getProb1gramI(inputTerms,MatchAllIn2gram))
  }
}

# 3 gram -----------------------
getProb3gram = function(inputTerms,MatchAllIn4gram,beta_leftoverprob) {
  if(flagDebug)  print("getProb3gram")
  inputTerms=setLength(inputTerms,3)
  if(flagDebug)  print(inputTerms)
  Match2In3gram=dt3gram[term1==inputTerms[1] & term2==inputTerms[2]]
  MatchAllIn3gram=dt3gram[ term1==inputTerms[1] & term2==inputTerms[2] & term3==inputTerms[3]]
  if (nrow(MatchAllIn3gram)>0) {
    RemainMatch2In3gram = Match2In3gram[!(MatchAllIn3gram$term3 %in% MatchAllIn4gram$term4)]
    all_freq = sum(Match2In3gram$count)
    alpha = beta_leftoverprob / sum((RemainMatch2In3gram$count * RemainMatch2In3gram$discount) / all_freq)  
    probability=alpha * ((MatchAllIn3gram$count * MatchAllIn3gram$discount) / all_freq)
    return(probability)
  } else {
    # 2 gram -------------------
    return(getProb2gram(inputTerms,MatchAllIn3gram,beta_leftoverprob))
  }
}

getProb3gramI = function(inputTerms,MatchAllIn4gram,beta_leftoverprob) {
  if(flagDebug)  print("getProb3gramI")
  inputTerms=setLength(inputTerms,3)
  if(flagDebug)  print(inputTerms)
  Match2In3gram=dt3gram[term1==inputTerms[1] & term2==inputTerms[2]]
  if (nrow(Match2In3gram)>0) {
    MatchAllIn3gram=dt3gram[ term1==inputTerms[1] & term2==inputTerms[2] & term3==inputTerms[3]]
    if (nrow(MatchAllIn3gram)>0) {
      all_freq = sum(Match2In3gram$count)
      probability=((MatchAllIn3gram$count * MatchAllIn3gram$discount) / all_freq)
      return(probability)
    } else {  
      beta_leftoverprob = lo3gram[term1==inputTerms[1] & term2==inputTerms[2]]$leftOver
      getProb2gram(inputTerms,MatchAllIn3gram,beta_leftoverprob)  
    }
  } else {
    return(getProb2gramI(inputTerms,MatchAllIn3gram))
  } 
}

# 4 gram -----------------------
getProb4gram = function(inputTerms,MatchAllIn5gram,beta_leftoverprob) {
  if(flagDebug)  print("getProb4gram")
  inputTerms=setLength(inputTerms,4)
  if(flagDebug)  print(inputTerms)
  Match3In4gram=dt4gram[term1==inputTerms[1] & term2==inputTerms[2] & term3==inputTerms[3]]
  MatchAllIn4gram=dt4gram[ term1==inputTerms[1] & term2==inputTerms[2] & term3==inputTerms[3] & term4==inputTerms[4]]
  if (nrow(MatchAllIn4gram)>0) {
    RemainMatch3In4gram = Match3In4gram[!(MatchAllIn4gram$term4 %in% MatchAllIn5gram$term5)]
    all_freq = sum(Match3In4gram$count)
    alpha = beta_leftoverprob  / sum((RemainMatch3In4gram$count * RemainMatch3In4gram$discount) / all_freq)  
    probability=alpha * ((MatchAllIn4gram$count * MatchAllIn4gram$discount) / all_freq)
    return(probability)
  } else {
    return(getProb3gram(inputTerms,MatchAllIn4gram,beta_leftoverprob))
  }
}

getProb4gramI = function(inputTerms,MatchAllIn4gram) {
  if(flagDebug)  print("getProb4gramI")
  inputTerms=setLength(inputTerms,4)
  if(flagDebug)  print(inputTerms)
  Match3In4gram=dt4gram[term1==inputTerms[1] & term2==inputTerms[2] & term3==inputTerms[3]]
  if (nrow(Match3In4gram)>0) {
    MatchAllIn4gram=dt4gram[ term1==inputTerms[1] & term2==inputTerms[2] & term3==inputTerms[3] & term4==inputTerms[4]]
    if (nrow(MatchAllIn4gram)>0) {
      all_freq = sum(Match3In4gram$count)
      probability=((MatchAllIn4gram$count * MatchAllIn4gram$discount) / all_freq)
      return(probability)
    } else {  
      beta_leftoverprob = lo4gram[term1==inputTerms[1] & term2==inputTerms[2] & term3==inputTerms[3]]$leftOver
      return(getProb3gram(inputTerms,MatchAllIn4gram,beta_leftoverprob))
    }
  } else {
    return(getProb3gramI(inputTerms,MatchAllIn4gram)) 
  } 
}

# 5 gram -------------------
getProb5gramI = function(inputTerms) {
  if(flagDebug)  print("getProb5gramI")
  inputTerms=setLength(inputTerms,5)
  if(flagDebug)  print(inputTerms)
  Match4In5gram=dt5gram[ term1==inputTerms[1] & term2==inputTerms[2] 
                         & term3==inputTerms[3] & term4==inputTerms[4]]
  if (nrow(Match4In5gram)>0) {
    MatchAllIn5gram=dt5gram[ term1==inputTerms[1] & term2==inputTerms[2] 
                             & term3==inputTerms[3] & term4==inputTerms[4]
                             & term5==inputTerms[5]]
    if (nrow(MatchAllIn5gram)>0) {
      all_freq = sum(Match4In5gram$count)
      probability=((MatchAllIn5gram$count * MatchAllIn5gram$discount) / all_freq)
      return(probability)
    } else {
      beta_leftoverprob = lo5gram[ term1==inputTerms[1] & term2==inputTerms[2] 
                                   & term3==inputTerms[3] & term4==inputTerms[4]]$leftOver
      return(getProb4gram(inputTerms,MatchAllIn5gram,beta_leftoverprob)) 
    }
  } else {
    return(getProb4gramI(inputTerms,MatchAllIn5gram))  
  }
}



#------------------------------------------------------------------------------------------ 
# Auxliar functons
#
#
#
#
removeURL <- function(x) { gsub("http[[:alnum:]]* - ","", x) }

toSpace <- function(x, pattern) {return (gsub(pattern," ", x))}
addSpace <- function(x, p) {return (gsub(paste0("[",p,"]"),paste0(" ",p," "), x))}
#
cleanFrase = function(frase) {
  newfrase <- tolower(frase)
  newfrase <- removeURL(newfrase)
  newfrase <- addSpace(newfrase,".") 
  newfrase <- addSpace(newfrase,",")
  newfrase <- addSpace(newfrase,":") 
  newfrase <- addSpace(newfrase,";") 
  newfrase <- removeNumbers(newfrase)  
  newfrase <- toSpace(newfrase,"[^\x20-\x7F]") 
  newfrase <- stripWhitespace(newfrase)
  newfrase
}

#
isStopWord = function(w) {
  (w %in% stopwords())
}

#
toVector = function(frase) {
  wordsInFrase = strsplit(frase," ")[[1]]
  as.vector(wordsInFrase)
}

#
setLength = function(x,l) {
  t <- length(x)
  n=t-l
  xShifted <- character(0)
  xShifted[1:(t-n)] <- x[(n+1):t]
  return(xShifted)
}


