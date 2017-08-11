# compute the semantic distance between pair of words

make_McRae_pairs <- function(words_list) {
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

##############################################################################################################################
make_assoc_pairs <- function(lemma_list) {
  # filter until words in lemma_list remain
  lemma<- lemma_list$uni_lemma
  cue_target<- read.csv("in_files/association_cue_target.csv", as.is = T) %>% 
    filter(cue %in% lemma, 
           target %in% lemma, 
           normed=="YES") %>% 
    select(cue, target) %>% 
    mutate(link=1)
  
  assoc_table<- expand.grid(cue= lemma, target= lemma) %>% 
    left_join(cue_target) %>% 
    mutate(link=if_else(is.na(link),0,link))
  
  #make a association network dataframe with item number
  #rename stuffs so it could conform to the format PAT_generator needs
  #item corresponds to target ;  pair corresponds to cue
  assoc_link <- assoc_table %>%
    rename(pair.definition = cue) %>%
    left_join(lemma_list, c("pair.definition" = "uni_lemma")) %>%
    rename(pair = item, item.definition = target) %>%
    left_join(lemma_list, c("item.definition" = "uni_lemma")) %>%
    select(item, item.definition, pair, pair.definition, link) %>%
    arrange(item, pair)
  
  return(assoc_link)
}

