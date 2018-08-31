# Loading the necessary libraries
library(stringr)
library(shinyjs) 
library(tm)
library(data.table)
library(DT)



function(input, output) {
  dtx <- eventReactive(input$goButton, {
    if (nchar(input$txt)==0) {
      return(NULL)
    } else {
      nfrase=cleanFrase(input$txt)
      inputTerms=toVector(nfrase)
      maxNumWords=10
      if (length(inputTerms)>3) { 
        dtWords=pesqNextsWordMatch4(inputTerms,maxNumWords)
      } else  {
        if (length(inputTerms)>2) { 
          dtWords=pesqNextsWordMatch3(inputTerms,maxNumWords)
        } else {
          if (length(inputTerms)>1) { 
            dtWords=pesqNextsWordMatch2(inputTerms,maxNumWords)
          } else {
            if (length(inputTerms)>0) { 
              dtWords=pesqNextsWordMatch1(inputTerms,maxNumWords)
            } else {
              dtWords=pesqNextsWordMatch0(inputTerms,maxNumWords)
            }    
          }
        }    
      }
    }
    dtWords
    print(dtWords)
})
  
   output$value = renderText({
     paste0("[",input$txt,"]")
   })
   
   output$ngramInfo = renderText({
     ngramInfo
   })
 
   output$nextWordTitle = renderUI({
     if(is.null(dtx())) {
       return()
     }
     h3('Next word')
   })  
   output$nextWord = renderText({
         options=as.vector(dtx()$term)
         options[1] 
   })

   output$optionsTitle = renderUI({
     if(is.null(dtx())) {
       return()
     }
     h4('Other options for next word in order of probability')
   })  
   
   output$options = renderText({
     options=as.vector(dtx()$term)
     options=options[2:length(options)]
     paste(options, collapse=", ")
   })
   
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
   
   toVector <- function(frase) {
     wordsInFrase = strsplit(frase," ")[[1]]
     as.vector(wordsInFrase)
   }
   
   setLength = function(x,l) {
     t <- length(x)
     if (t<=l) {
       return(x)
     } else {
       n=t-l
       xShifted <- character(0)
       xShifted[1:(t-n)] <- x[(n+1):t]
       return(xShifted)
     }
   }
   
   isStopWord = function(w) {
        (w %in% stopwords())
   }
   
   pesqNextsWordMatch4 = function(inputTerms,maxNumWords) {
     inputTerms=setLength(inputTerms,4)
     result=dt5gram[term1==inputTerms[1] & term2==inputTerms[2] & term3==inputTerms[3] & term4==inputTerms[4],.(origem=5,term=term5,count=count)]
     result=result[order(-count)]
     rowsSelected = nrow(result)
     if (is.null(rowsSelected)) 
     {
       rowsSelected=0
     }
     if (rowsSelected < maxNumWords) {
       result = rbind(result,pesqNextsWordMatch3(inputTerms, maxNumWords - rowsSelected))    
     }
     return(result[1:maxNumWords,])
   }
   
   pesqNextsWordMatch3 = function(inputTerms,maxNumWords) {
     inputTerms=setLength(inputTerms,3)
     result=dt4gram[term1==inputTerms[1] & term2==inputTerms[2] & term3==inputTerms[3],.(origem=4,term=term4,count=count)]
     
     result=result[order(-count)]
     rowsSelected = nrow(result)
     if (is.null(rowsSelected)) 
     {
       rowsSelected=0
     }  
     if (rowsSelected < maxNumWords) {
       result = rbind(result,pesqNextsWordMatch2(inputTerms, maxNumWords - rowsSelected))    
     }
     return(result[1:maxNumWords,])
   }
   
   pesqNextsWordMatch2 = function(inputTerms,maxNumWords) {
     inputTerms=setLength(inputTerms,2)
     result=dt3gram[term1==inputTerms[1] & term2==inputTerms[2],.(origem=3,term=term3,count=count)]
     result=result[order(-count)]
     rowsSelected = nrow(result)
     if (is.null(rowsSelected)) 
     {
       rowsSelected=0
     }  
     if (rowsSelected < maxNumWords) {
       result = rbind(result,pesqNextsWordMatch1(inputTerms, maxNumWords - rowsSelected))    
     }
     return(result[1:maxNumWords,])
   }
   
   
   
   
   pesqNextsWordMatch1 = function(inputTerms,maxNumWords) {
     inputTerms=setLength(inputTerms,1)
     result=dt2gram[term1==inputTerms[1],.(origem=2,term=term2,count=count)]
     result=result[order(-count)]
     rowsSelected = nrow(result)
     if (is.null(rowsSelected)) 
     {
       rowsSelected=0
     }
     if (rowsSelected < maxNumWords) {
       result = rbind(result,pesqNextsWordMatch0(inputTerms,(maxNumWords - rowsSelected)))    
     }
     return(result[1:maxNumWords,])
   }
   
   pesqNextsWordMatch0 <- function(inputTerms,maxNumWords) {
     if (isStopWord(inputTerms[1])) {
       result=dt1gram[!isStopWord(term1),.(origem=1,term=term1,count=count)]
     } else {
       result=dt1gram[!isStopWord(term1),.(origem=1,term=term1,count=count)]
     }
     result=result[order(-count)]
     result=result[1:maxNumWords,]
     return(result)
   }
   
   
   
   return()

} 

