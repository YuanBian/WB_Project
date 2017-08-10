# compute the semantic distance between pair of words

get_McRae <- function(words_list) {
  #Get the McRae features
  
  features <-
    read_delim(paste(getwd(), "/in_files/", "features.csv", sep = ""), delim = ",") %>%
    select(Concept, Feature, WB_Label, BR_Label)
  
  #Intersection of Wordbank with McRae concepts
  
  #List of word types in WordBank
  wb <- words_list %>%
    select(item, uni_lemma) %>%
    rename(Concept = uni_lemma) %>%
    unique()
  
  # List of word bank with feature
  #Here I should deal with words in parenthesis (homophones?), e.g., chiken (animal) vs. chiken (food), etc...
  
  wb.feat <- wb %>%
    left_join(features) %>%
    rename(item.definition = Concept,
           item.feat = Feature) %>%
    filter(!is.na(item.feat)) %>%
    select(item, item.definition, item.feat)
  
  wb.feat.list <- wb.feat %>%
    select(item) %>%
    unique()
  
  mcrae_wb_intersect <- wb.feat.list %>%
    left_join(wb %>% rename(definition = Concept))
  
  #write.csv(mcrae_wb_intersect, "mcrae_wb_intersect.csv", row.names = FALSE)
  
  # List these words pair-wise and compate the number of shared feature for each pair
  
  item.pair <- expand.grid(item = wb.feat.list$item,
                           pair = wb.feat.list$item)
  
  pair.feat <- wb.feat %>%
    rename(pair = item,
           pair.feat = item.feat,
           pair.definition = item.definition)
  
  
  item.feat.pair <- left_join(wb.feat, item.pair)
  
  item.pair.feat <- item.feat.pair %>%
    left_join(pair.feat)
  
  item.pair.shared <- item.pair.feat %>%
    group_by(item, item.definition, pair, pair.definition) %>%
    summarise(shared = sum(pair.feat == item.feat)) %>%
    filter(item!=pair)
  
  return(item.pair.shared)
}

##############################################################################################################################

McRae_threshold<- function(item.pair.shared, threshold){
  sem_feat<-item.pair.shared %>%
#    filter(item!=pair) %>%
    mutate(link=as.numeric(shared>=threshold)) %>%
    select(-shared)
  return(sem_feat)
}



