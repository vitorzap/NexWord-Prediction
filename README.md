# Next word's prediction
##Coursera  Data Science Capstone - Final Project - Next Word Prediction

Este repositório contem os arquivos do projeto final do curso de data science da Coursera.
O objetivo deste aplicativo Shiny é dado um texto, prever a próxima palavra baseado em n-grams préviamente criados.
Os textos usados como base para a criação destes n-grams foram baixados de The data from "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
Os N-grams foram gerados  utilizando-se o pacote "tm".
O  aplicativo utiliza data tables contendo 5-grams a 1-grams e os scrips R utilizados para gerar estas data tables esta incluidos no diretório ngramGeneration.
Inicialmente utilizei um algoritmo simples de match do texto entrado com a base de dados de dados de n-grams  tentando match desde 5-grams até 2-gram para encontrar a proxima palavra, depois  implementei um algoritmo Katz BackOff Model para obter probabilidades das proximas palavras  (basei-me nas ideias descritas em 
- https://thachtranerc.wordpress.com/2016/04/12/katzs-backoff-model-implementation-in-r/ e 
- https://en.wikipedia.org/wiki/Katz%27s_back-off_model

e na implementação contida em:
-https://github.com/ThachNgocTran/Katz BackOff ModeImplementationInR

porém a utlização deste algoritmo para o propósito do aplicativo não apresentou ganhos de precisão  significativos  mas aumentou o tempo de resposta, assim voltei a ideia mais simples.

A minha implementação do Katz BackOff Mode esta cotida no diretório Katz BackOff Model.
