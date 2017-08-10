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