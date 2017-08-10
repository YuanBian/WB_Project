
make_assoc_pairs <- function(lemma_list) {
  #Filter all the cues and targets that are not learnt after 30 months and are not normed
  cue_target <- read.csv(paste(getwd(), "/in_files/assoc_table.csv", sep = ""), as.is = T) %>%
    filter(cue %in% lemma_list$uni_lemma,
           target %in% lemma_list$uni_lemma)
  
  #make a association network dataframe with item number
  #rename stuffs so it could conform to the format PAT_generator needs
  #item corresponds to target ;  pair corresponds to cue
  assoc_link <- cue_target %>%
    rename(pair.definition = cue) %>%
    left_join(lemma_list, c("pair.definition" = "uni_lemma")) %>%
    rename(pair = item, item.definition = target) %>%
    left_join(lemma_list, c("item.definition" = "uni_lemma")) %>%
    select(item, item.definition, pair, pair.definition, link) %>%
    arrange(item, pair)
  
  return(assoc_link)
}

#########################################################################################################

# make_assoc_pairs <- function(lemma_list) {
#   #Filter all the cues and targets that are not learnt after 30 months and are not normed
#   cue_target <- read.csv(paste(getwd(), "/in_files/assoc_table.csv", sep = ""), as.is = T) %>%
#     filter(cue %in% lemma_list$uni_lemma,
#            target %in% lemma_list$uni_lemma)
# 
#   #make a association network dataframe with item number
#   #rename stuffs so it could conform to the format PAT_generator needs
#   #item corresponds to target ;  pair corresponds to cue
#   assoc_link <- cue_target %>%
#     select(cue, target) %>%
#     rename(pair.definition = cue) %>%
#     left_join(lemma_list, c("pair.definition" = "uni_lemma")) %>%
#     rename(pair = item, item.definition = target) %>%
#     left_join(lemma_list, c("item.definition" = "uni_lemma")) %>%
#     select(item, item.definition, pair, pair.definition) %>%
#     arrange(item, pair) %>%
#     mutate(link = 1)
#   
#   return(assoc_link)
# }

#########################################################################################################

make_assoc_pairs_wotrim <- function(lemma_list) {
  cue_target <- read.csv(paste(getwd(), "/in_files/association_pairs_wotrim.csv", sep = ""), as.is = T)
  
  #Filter all the cues and targets that are not learnt after 30 months and are not normed
  cue_target <- cue_target %>%
    filter(cue %in% lemma_list$uni_lemma,
           target %in% lemma_list$uni_lemma) %>%
    filter(normed == "YES")
  
  #make a association network dataframe with item number
  #rename stuffs so it could conform to the format PAT_generator needs
  #item corresponds to target ;  pair corresponds to cue
  assoc_link <- cue_target %>%
    select(cue, target) %>%
    rename(pair.definition = cue) %>%
    left_join(lemma_list, c("pair.definition" = "uni_lemma")) %>%
    rename(pair = item, item.definition = target) %>%
    left_join(lemma_list, c("item.definition" = "uni_lemma")) %>%
    select(item, item.definition, pair, pair.definition) %>%
    arrange(item, pair) %>%
    mutate(link = 1)
  
  return(assoc_link)
}

##############
# item_data<- get_lang_item_data(lang = "English (American)")
# ct<- read.csv("in_files/association_cue_target.csv", as.is = T)
# deflist<- item_data %>% trim_all_definition()
# deflist
# ct<- ct %>% filter(cue %in% deflist$definition, target %in% deflist$definition, normed=="YES") %>% select(cue, target) %>% mutate(link=1)
# assoc_table<- expand.grid(cue= deflist$definition, target= deflist$definition) %>% left_join(ct) %>% mutate(link=if_else(is.na(link),0,link))
# assoc_table
# write.csv(assoc_table, file = "assoc_table.csv", row.names = F)