# PAT_generator
# not just for semantic feature networks
# First argument should have the format below:
# column name: 
# item, item.definition, pair, pair.definition, link

PAT_generator<-function(vocab_month, word_pairs){
  vocab_month<-vocab_month %>% 
    filter(item %in% word_pairs$item) %>%
    mutate(value=NA) %>% 
    arrange(month, item)
  first<- vocab_month$month[1]+1
  last<- vocab_month$month[length(vocab_month$month)]
  for (i in first:last){
    #items learnt in this month
    curr_items<- (vocab_month %>% 
                    filter(month==i) %>% 
                    select(item))$item 
    #item learnt in previous months
    exist_words<-(vocab_month %>% 
                    filter(month<i,learned==1) %>%
                    select(item))$item
    #calculating the degree of nodes in the existing network
    sem_acq<- word_pairs %>%
      filter(item %in% exist_words, pair %in% exist_words) %>% 
      group_by(item) %>% 
      summarise(value=sum(link))
    
    row_n<-which(vocab_month$month==i & vocab_month$item==curr_items[1])
    for (j in curr_items){
      #calculating d
      link_to_exist<-(word_pairs %>% filter(pair==j, item %in% exist_words, link==1))$item
      PAT_value<-sem_acq %>%
        filter(item %in% link_to_exist) 
      vocab_month$value[row_n]<- sum(PAT_value$value)
      row_n<-row_n+1
    }
  }
  #filter first month since there are no PAT value
  vocab_month<-vocab_month %>% filter(month!=(first-1))
  return(vocab_month)
}

######################################################################################################################

PAC_generator<- function(vocab_month, word_pairs){
  vocab_month<- vocab_month %>% filter((item %in% word_pairs$item) | (item %in% word_pairs$pair) )
  word_pairs<- word_pairs %>% filter(item %in% vocab_month$item, pair %in% vocab_month$item)
  
  item_value<- word_pairs %>% 
    group_by(item) %>%
    summarise(value=sum(link))
  
  PAC<-vocab_month %>%
    mutate(value=0) %>%
    rowwise() %>%
    mutate(value=ifelse((item %in% item_value$item), item_value$value[which(item_value$item ==item)], 0)) 
  
  return(PAC)
}

######################################################################################################################

PAC_generator_od<- function(vocab_month, pairs){
  vocab_month<- vocab_month %>% filter((item %in% assoc_pairs$item) | (item %in% assoc_pairs$pair) )
  
  item_value<- pairs %>% 
    group_by(pair) %>%
    summarise(value=sum(link)) %>%
    rename(item=pair)
  
  PAC<-vocab_month %>%
    mutate(value=0) %>%
    rowwise() %>%
    mutate(value=ifelse((item %in% item_value$item), item_value$value[which(item_value$item ==item)], 0)) 
  
  return(PAC)
}

######################################################################################################################
