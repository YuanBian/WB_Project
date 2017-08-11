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
    mutate(IPA= gsub("[[:punct:]]", "",IPA))
  return(IPA_list)
}

######################################################################################################
# trim IPA, more fiercely
trim_IPA_completely<- function(IPA_list){
  IPA_list<- IPA_list %>%
    mutate(IPA= gsub(" ","", IPA)) %>%
    mutate(IPA= gsub("ː","",IPA)) %>%
    mutate(IPA= gsub("[[:punct:]]", "",IPA))
  return(IPA_list)
}