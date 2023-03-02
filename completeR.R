#laden aller nötigen Pakete
library(dplyr)
library(tidytext)
library(quanteda)
library(quanteda.textplots)
library(reshape2)
library(wordcloud)
library(udpipe)
library(jsonlite)
library(tidyverse)
library(word2vec)
library(uwot)
library(plotly)
library(doc2vec)
library(modeltools)
library(ROCR)
library(tm)
library(corpus)
library(posterior)
library(ldatuning)

#Working Directory setzen und Daten entpacken
setwd("C:\\Users\\danie\\Documents\\UNI SHIT POWI\\Semester 5")
kreml <- lapply(readLines(".\\kremlin_transcripts_en_2021_03_01.json"), fromJSON)

#datensatz mit kategorien versehen
kreml_data <- data.frame(id = as.integer(), date = as.character(), title = as.character(), text = as.character(), tag = as.character(), place = as.character())
for (i in 1:length(kreml)){
  
  doc <- data.frame(id = kreml[[i]]$`_source`$kremlin_id,
                    date = kreml[[i]]$`_source`$date,
                    title = kreml[[i]]$`_source`$title,
                    text = kreml[[i]]$`_source`$transcript,
                    tag = ifelse(!length(kreml[[i]]$`_source`$tags) , NA, kreml[[i]]$`_source`$tags),
                    place = ifelse(!length(kreml[[i]]$`_source`$place) , NA, kreml[[i]]$`_source`$place)
  )
  
  kreml_data <- bind_rows(kreml_data, doc )
}

#datensatz kopieren
kreml_data2 <- kreml_data

#Doc ID hinzufügen
kreml_data2$doc_id <- gsub(" ", paste0(kreml_data2$id, kreml_data2$date, kreml_data2$title, kreml_data2$text, kreml_data2$tag, kreml_data2$place, kreml_data2$Year, '_' ,1:nrow(kreml_data2)), runif(1, min=0, max=100000000000)
                           ,replacement =  "")

#"T" aus orginalem $date removen
kreml_data2$date<-gsub("T","",as.character(kreml_data2$date))

#reformatieren
kreml_data2$date<-as.POSIXct(kreml_data2$date, format = "%Y-%m-%d%H:%M:%S", tz = "GMT")

#Jahresspalte einfügen
kreml_data2$Year<- format(kreml_data2$date, format= "%Y")

#bereinigen
kreml_clean2 <- kreml_data2 %>% 
  corpus %>%  
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_remove( c(stopwords("english"))) %>% 
  dfm(verbose = FALSE) %>%  
  dfm_group(groups = tag)

#lemmatization
lemma_en2 <- udpipe_download_model(language = "english") 
lemma_en2 <- udpipe_load_model(file = lemma_en2$file_model) 
kreml_corpus2 <- kreml_data2 %>% corpus()
lemma_en2 <- udpipe(kreml_corpus2, lemma_en2, parallel.cores = 8) 
lemma_en2 <- lemma_en2 %>% filter(upos != 'PUNCT' & is.na(lemma) == F)

kreml_token2 <- kreml_corpus2 %>%  
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_remove (c(stopwords("english"),"c") ) %>%
  tokens_ngrams (n = 1)

kreml_token2 <- tokens_replace(tokens(kreml_token2), pattern = lemma_en2$token, replacement = lemma_en2$lemma)

kremltokencsv <- kreml_token2

write.csv(kremltokencsv)

#kremltag_dfm2 zu dataframe, dfm3 zu document feature matrix, dfm4 nach Jahren sortiert
kremltag_dfm2 <- kreml_token2  %>%
  dfm(verbose = FALSE) %>% 
  dfm_group(groups = tag) 
convert(kremltag_dfm2, to = "data.frame")

kremltagdfmcsv <- kremltag_dfm2

write.csv(kremltagdfmcsv, "kremltag.csv")

kreml_token2 %>% dfm(verbose = F) %>% 
  dfm_group(groups = tag) %>% 
  dfm_tfidf() %>% 
  tidy() %>% 
  acast(term ~ document, value.var = "count", fill = 0) %>% 
  comparison.cloud(title.size = 1, random.order = FALSE, max.words = 600)
