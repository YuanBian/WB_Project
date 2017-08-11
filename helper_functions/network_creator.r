create_McRae_PAC<-function(aoa_frame, McRae_pairs){
  McRae_PAC<- PAC_generator(vocab_month = aoa_frame, word_pairs = McRae_pairs)
  write_out_csv(var = McRae_PAC, lang = lang_name, type = paste("McRae_PAC", suffix, sep = ""))
}

create_McRae_PAT<-function(aoa_frame, McRae_pairs){
  McRae_PAT<- PAT_generator(vocab_month = aoa_frame, word_pairs = McRae_pairs )
  write_out_csv(var = McRae_PAT, lang = lang_name, type = paste("McRae_PAT", suffix, sep = ""))
}

create_assoc_PAC<- function(aoa_frame, assoc_pairs){
  assoc_PAC<- PAC_generator(vocab_month = aoa_frame, word_pairs = assoc_pairs )
  write_out_csv(var = assoc_PAC, lang = lang_name, type = paste("assoc_PAC", suffix, sep = ""))
}

create_assoc_PAT<- function(aoa_frame, assoc_pairs){
  assoc_PAT<- PAT_generator(vocab_month = aoa_frame, word_pairs = assoc_pairs )
  write_out_csv(var = assoc_PAT, lang = lang_name, type = paste("assoc_PAT", suffix, sep = ""))
}

create_phono_PAC<- function(aoa_frame, phono_pairs){
  
}

create_phono_PAT<- function(aoa_frame, phono_pairs){
  
}
