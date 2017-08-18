create_network_haspoly<- function(languages, features, thresholds){
  suffix<<- "_p"
  for (language in languages){
    
    lang_name<<- language
    # age of acquisition data frame
    aoa_frame<- make_aoa_dataframe(lang = lang_name)
    # list of lemmas of learnt words
    first_age<- aoa_frame$age[1]
    lemma_list<- aoa_frame %>%
      trim_all_unilemma() %>%
      filter(age==first_age) %>%
      select(item, uni_lemma) 
    # list of definitions of learnt words
    def_list<- aoa_frame %>%
      trim_all_definition() %>% 
      filter(age==first_age) %>%
      filter(item %in% lemma_list$item) %>%
      select(item, definition)
    
    if ("assoc_PAC" %in% features){
      # make pairs of words and show whether they have connection via associative norms
      assoc_pairs<<- make_assoc_pairs(lemma_list = lemma_list)
      create_assoc_PAC(aoa_frame = aoa_frame, assoc_pairs = assoc_pairs)
    }
    if ("assoc_PAT" %in% features){
      create_assoc_PAT(aoa_frame = aoa_frame, assoc_pairs = assoc_pairs)
    }
    if ("McRae_PAC" %in% features){
      # make pairs of words and show how many McRae features they share
      McRae_pairs<<- make_McRae_pairs(words_list = lemma_list) 
      for (threshold in thresholds){
        create_McRae_PAC(aoa_frame = aoa_frame, 
                         McRae_pairs = McRae_pairs %>% McRae_threshold(threshold = 1),
                         threshold = threshold)
      }
    }
    if ("McRae_PAT" %in% features){
      for (threshold in thresholds){
        create_McRae_PAT(aoa_frame = aoa_frame, 
                         McRae_pairs = McRae_pairs %>% McRae_threshold(threshold = 1),
                         threshold = threshold)
      }
    }
    if ("phono_PAC" %in% features){
      IPA_pairs<<- make_IPA_pairs(def_list = def_list, lang = lang_name)
      for (threshold in thresholds){
        create_phono_PAC(aoa_frame = aoa_frame, 
                         phono_pairs = IPA_pairs %>% IPA_threshold(threshold = threshold),
                         threshold = threshold)
      }
    }
    if ("phono_PAT" %in% features){
      for (threshold in thresholds){
        create_phono_PAT(aoa_frame = aoa_frame, 
                         phono_pairs = IPA_pairs %>% IPA_threshold(threshold = threshold),
                         threshold = threshold)
      }
    }
  }
}

###################################################################################################
create_network_nopoly<- function(languages, features, thresholds){
  suffix<<- "_np"
  for (language in languages){
    
    lang_name<<- language
    # age of acquisition data frame
    aoa_frame<- make_aoa_dataframe(lang = lang_name)
    # list of lemmas of learnt words
    first_age<- aoa_frame$age[1]
    lemma_list<- aoa_frame %>%
      filter(age==first_age) %>%
      select(item, uni_lemma)
    
    if ("assoc_PAC" %in% features){
      # make pairs of words and show whether they have connection via associative norms
      assoc_pairs<<- make_assoc_pairs(lemma_list = lemma_list)
      create_assoc_PAC(aoa_frame = aoa_frame, assoc_pairs = assoc_pairs)
    }
    if ("assoc_PAT" %in% features){
      create_assoc_PAT(aoa_frame = aoa_frame, assoc_pairs = assoc_pairs)
    }
    if ("McRae_PAC" %in% features){
      # make pairs of words and show how many McRae features they share
      McRae_pairs<<- make_McRae_pairs(words_list = lemma_list) 
      for (threshold in thresholds){
        create_McRae_PAC(aoa_frame = aoa_frame, 
                         McRae_pairs = McRae_pairs %>% McRae_threshold(threshold = 1),
                         threshold = threshold)
      }
    }
    if ("McRae_PAT" %in% features){
      for (threshold in thresholds){
        create_McRae_PAT(aoa_frame = aoa_frame, 
                         McRae_pairs = McRae_pairs %>% McRae_threshold(threshold = 1),
                         threshold = threshold)
      }
    }
    
  }
}

###################################################################################################
create_McRae_PAC<-function(aoa_frame, McRae_pairs, threshold){
  McRae_PAC<- PAC_generator(vocab_age = aoa_frame, word_pairs = McRae_pairs)
  write_out_csv(var = McRae_PAC, lang = lang_name, type = paste("McRae_PAC_t", threshold, suffix, sep = ""))
}

create_McRae_PAT<-function(aoa_frame, McRae_pairs, threshold){
  McRae_PAT<- PAT_generator(vocab_age = aoa_frame, word_pairs = McRae_pairs )
  write_out_csv(var = McRae_PAT, lang = lang_name, type = paste("McRae_PAT_t", threshold, suffix, sep = ""))
}

create_McRae_distinct_PAC<-function(aoa_frame, McRae_distinct_pairs, threshold){
  McRae_PAC<- PAC_generator(vocab_age = aoa_frame, word_pairs = McRae_pairs)
  write_out_csv(var = McRae_PAC, lang = lang_name, type = paste("McRaeD_PAC_t", threshold, suffix, sep = ""))
}

create_McRae_distinct_PAT<-function(aoa_frame, McRae_distinct_pairs, threshold){
  McRae_PAT<- PAT_generator(vocab_age = aoa_frame, word_pairs = McRae_pairs)
  write_out_csv(var = McRae_PAT, lang = lang_name, type = paste("McRaeD_PAT_t", threshold, suffix, sep = ""))
}


create_assoc_PAC<- function(aoa_frame, assoc_pairs){
  assoc_PAC<- PAC_generator(vocab_age = aoa_frame, word_pairs = assoc_pairs )
  write_out_csv(var = assoc_PAC, lang = lang_name, type = paste("assoc_PAC", suffix, sep = ""))
}

create_assoc_PAT<- function(aoa_frame, assoc_pairs){
  assoc_PAT<- PAT_generator(vocab_age = aoa_frame, word_pairs = assoc_pairs )
  write_out_csv(var = assoc_PAT, lang = lang_name, type = paste("assoc_PAT", suffix, sep = ""))
}

create_phono_PAC<- function(aoa_frame, phono_pairs, threshold){
  phono_PAC<- PAC_generator(vocab_age = aoa_frame, word_pairs = phono_pairs)
  write_out_csv(var = phono_PAC, lang = lang_name, type = paste("phono_PAC_t", threshold, sep = ""))
}

create_phono_PAT<- function(aoa_frame, phono_pairs, threshold){
  phono_PAT<- PAT_generator(vocab_age = aoa_frame, word_pairs = phono_pairs)
  write_out_csv(var = phono_PAT, lang = lang_name, type = paste("phono_PAT_t", threshold, sep = ""))
}

create_phono_net<- function(aoa_frame, phono_pairs, growth, threshold){
  
}