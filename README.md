# Next word's prediction
### Coursera  Data Science Capstone - Final Project - Next Word Prediction

This repository contains the archives of the final design of the Coursera data science course. The purpose of this Shiny application is given a text, predict the next word based on n-grams previously created. 
The scripts anda data for this shiny application are contained in the are contained in PredicNextWord directory
The texts used as the basis for the creation of these n-grams were downloaded from The data from "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip" N-grams were generated using the package "tm". The application uses data tables containing 5-grams to 1-grams and the R scripts used to generate these data tables are included in the ngramGeneration directory. Initially I used a simple text match algorithm entered with the n-grams data database trying to match from 5-grams to 2-gram to find the next word, then implemented a Katz BackOff Model algorithm to get probabilities of the next words (based on the ideas described in

https://thachtranerc.wordpress.com/2016/04/12/katzs-backoff-model-implementation-in-r/ e
https://en.wikipedia.org/wiki/Katz%27s_back-off_model
and the implementation contained in: -https: //github.com/ThachNgocTran/Katz BackOff ModeImplementationInR

but the use of this algorithm for the purpose of the application did not present significant gains of precision but increased the response time, so I came back to the simpler idea.


The scripts for creating n-grams are in sss

My Katz BackOff Model implementation is contained in the KatzBackOff directory.
