# This file contains functions used to get item data, admin data, etc. , and generate dataframes for analysis
import_all_library<- function(){
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(langcog)
library(boot)
library(lazyeval)
library(dplyr)
library(wordbankr)
library(directlabels)
library(scales)
library(stringr)
}
######################################################################################################################
get_lang_item_data <- function(lang, lang_form = "WS", lex_class = "nouns") {
  #Every English item
  item_data <- get_item_data(language = lang, form = lang_form) %>%
    select(num_item_id, definition, type, lexical_class, uni_lemma) %>%
    filter(type == "word", lexical_class == lex_class) %>%
    rename(item = num_item_id)
  
  #Initialize every item as "NOT yet learnt"
  lang_item <- item_data %>%
    select(item, definition, uni_lemma) %>%
    mutate(month = "no")
  
  return(lang_item)
}

######################################################################################################################
get_lang_admin_data <- function(lang, lang_form = "WS") {
  #get the kids' data_id from every month
  admin_data <- get_administration_data() %>%
    filter(form == lang_form, !is.na(production), language == lang) %>%
    select(data_id, age) %>%
    arrange(age)
  
  return(admin_data)
}

######################################################################################################################
get_kids_by_month <- function(admin_data) {
  #get number of kids by month
  nkids_by_month <- admin_data %>%
    group_by(age) %>%
    summarise(n = n())
  return(nkids_by_month)
}

######################################################################################################################
#get instrument data
get_lang_instr_data <- function(lang, lang_form = "WS") {
  instr_data <- get_instrument_data(instrument_language = lang,
                                    instrument_form = lang_form) %>%
    filter(value == "produces") %>%
    arrange(num_item_id) %>%
    rename(item = num_item_id)
  
  return(instr_data)
}

######################################################################################################################
#calculate the month of acquisition
get_lang_aoa <- function(lang_item, admin_data, instr_data) {
  nkids_by_month <- get_kids_by_month(admin_data)
  first<- nkids_by_month$age[1] 
  last<- nkids_by_month$age[length(nkids_by_month$age)]

  for (month in first:last) {
    rem_item <- lang_item %>% filter(month == "no")
    current_month_id <- admin_data %>% filter(age == month)
    current_instr <- instr_data %>% 
      filter(data_id %in% current_month_id$data_id)
    for (w in rem_item$item) {
      proportion <-
        sum(current_instr$item == w) / nkids_by_month$n[month - first+1]
      if (proportion >= 0.5) {
        lang_item$month[which(lang_item$item == w)] = month
      }
    }
  }
  word_aoa<- lang_item %>% filter(month != "no")
  return(word_aoa)
}

######################################################################################################################

#check how many words are learnt each month
get_nwords_by_month <- function(word_aoa) {
  nwords_by_month <- word_aoa %>%
    arrange(month) %>%
    group_by(month) %>%
    summarise(n = n())
  
  return(nwords_by_month)
}

######################################################################################################################
#make a final dataframe
make_aoa_dataframe_helper <- function(word_aoa) {
  months<- get_nwords_by_month(word_aoa)
  first<- months$month[1]
  last<- months$month[length(months$month)]
  df <- data.frame()
  for (i in first:last) {
    rem_words <- word_aoa %>% filter(month >= i)
    rem_lemma <- c(rem_words$uni_lemma)
    rem_def <- c(rem_words$definition)
    corr_month <- rep(i, times = length(rem_lemma))
    curr_df <- data.frame(corr_month, rem_lemma, rem_def)
    df <- rbind(df, curr_df)
  }  
    df <- df %>% rename(uni_lemma = rem_lemma, definition=rem_def)
    df
    df <- df %>%
      left_join(word_aoa) %>%
      mutate(learned = as.numeric(month == corr_month)) %>%
      select(corr_month, item, definition, uni_lemma,learned) %>%
      rename(month = corr_month)
  return(df)
}


######################################################################################################################
make_aoa_dataframe <- function(lang, lang_form = "WS", lex_class = "nouns") {
    item_data <- get_lang_item_data(lang = lang,
                                    lang_form = lang_form,
                                    lex_class = lex_class)
    admin_data <-
      get_lang_admin_data(lang = lang, lang_form = lang_form)
    instr_data <-
      get_lang_instr_data(lang = lang, lang_form = lang_form)
    word_aoa <-
      get_lang_aoa(lang_item = item_data,
                   admin_data = admin_data,
                   instr_data = instr_data)
    
    return(make_aoa_dataframe_helper(word_aoa))
}

######################################################################################################################
trim_all_unilemma<-function(unilemma_list){
  unilemma_list<- unilemma_list %>%
    mutate(uni_lemma=if_else(grepl(" \\s*\\([^\\)]+\\)",uni_lemma),gsub(" \\s*\\([^\\)]+\\)","", uni_lemma),uni_lemma)) %>%
    mutate(uni_lemma=if_else(grepl("[*].*$",uni_lemma),gsub("[*].*$","", uni_lemma),uni_lemma)) %>%
    filter(!is.na(uni_lemma))
  return(unilemma_list)
}

######################################################################################################################
trim_all_definition<-function(def_list){
  def_list<- def_list %>%
    mutate(definition= gsub(" \\s*\\([^\\)]+\\)","", definition)) %>%
    mutate(definition= gsub("[*].*$","", definition)) %>%
    mutate(definition= gsub("\\/.*", "", definition)) %>%
    filter(!is.na(uni_lemma), 
           definition !="babysitter's name", 
           definition!= "child's own name", 
           definition!="pet's name") %>%
    mutate(definition= gsub("[[:punct:]]", "", definition)) 
    
  return(def_list)
}
######################################################################################################################
trim_unilemma<-function(unilemma_list){
  unilemma_list<- unilemma_list %>%
    mutate(uni_lemma=if_else(grepl(" \\(animal\\)",uni_lemma),gsub(" \\(animal\\)","", uni_lemma),uni_lemma)) %>%
    mutate(uni_lemma=if_else(grepl(" \\(object\\)",uni_lemma),gsub(" \\(object\\)","", uni_lemma),uni_lemma)) %>%
    filter(!is.na(uni_lemma))
  return(unilemma_list)
}

######################################################################################################################
write_out_csv<-function(name){
  write.csv(paste(getwd(),"/out_files/",name, sep = "" ))
}
######################################################################################################################
write_out_csv<- function(var, lang, type){
  write.csv(var, paste(getwd(),"/out_files/",lang, "_",type,".csv",sep = ""), row.names = F)
}




######################################################################################################
#change representation of CMUdict
dict.conv<-function(str_list){
  phoneme<-c("AA","AE","AH","AO","AW","AY","B","CH","D","DH",
             "EH","ER","EY","F","G","HH","IH","IY","JH","K",
             "L","M","N","NG","OW","OY","P","R","S","SH","T",
             "TH","UH","UW","V","W","Y","Z","ZH","1","2")
  converted<-c("a","b","c","d","e","f","g","h","i","j","k","l",
               "m","n","o","p","q","r","s","t","u","v","w","x",
               "y","z","A","B","C","D","E","F","G","H","I","J",
               "K","L","M","1","2")
  for (x in str_list){
    index<- which(str_list==x)
    str_list[index]=converted[which(phoneme==x)]
  }
  return(str_list)
}

######################################################################################################
#look up the phonetic transcription in CMUdict
#phon<- read.csv(paste(getwd(),"/in_files/phon_dict.csv",sep = ""), as.is = TRUE)
lookup<- function(someword){
  phon<- read.csv(paste(getwd(),"/in_files/phon_dict.csv",sep = ""), as.is = TRUE)
  index<- which(phon$word==someword)
  return(phon$phonetic[index])
}

######################################################################################################
#Define phonological distance function
phon.distance <- function(x,y){
  x<-lookup(x)
  y<-lookup(y)
  x<-gsub("0","",x)
  y<-gsub("0","",y)
  x<-gsub("1",":1",x)
  x<-gsub("2",":2",x)
  y<-gsub("1",":1",y)
  y<-gsub("2",":2",y)
  x_vec <- unlist(strsplit(x, ":","1","2"))
  y_vec <- unlist(strsplit(y, ":","1","2"))
  x_conv <- dict.conv(x_vec)
  y_conv <- dict.conv(y_vec)
  x_str <- paste(x_conv,collapse = "")
  y_str <- paste(y_conv,collapse = "")
  return(adist(x_str,y_str))
}


######################################################################################################
# Espeak function
Speak<- function(lang, word){
langs<- c("English (American)", "Spanish", "Croatian", "Italian", "Danish", "Norwegian", "Turkish")
abbr <- c("en-us",              "es",      "hr",      "it",       "da",     "no",        "tr")
lang_abbr<- abbr[which(langs==lang)]
IPA<- system(paste("speak -q -v ", lang_abbr, " --ipa ", "\"", word, "\"",sep=""), intern=T)
return(IPA)
}


######################################################################################################
# trim IPA
trim_IPA<- function(IPA_list){
  IPA_list<- IPA_list %>%
    mutate(IPA= gsub(" ","", IPA)) %>%
    mutate(IPA= gsub("ː","",IPA)) %>%
    mutate(IPA= gsub("[[:punct:]]", "",IPA))
  return(IPA_list)
}

######################################################################################################
# add 1 value to polysemy
add_polysemy_value<- function(list){
  list<- list %>%
    mutate(value=if_else(definition %in% c("chicken","fish","water"),value+1,value))
  return(list)
}