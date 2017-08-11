# A bunch of optims
######################################################################################################
# Return the nLLK of each model
optim_nllk <- function(par_n) {
  if (par_n == 2) {
    random_model <- optim(c(0, 0), loglike00)$value
    two_par_model <- optim(c(0, 0), loglike11)$value
    first_par_model <- optim(c(0, 0), loglike10)$value
    second_par_model <- optim(c(0, 0), loglike01)$value
    
    return(data.frame(random_model,
      two_par_model,
      first_par_model,
      second_par_model, row.names = "nLLK"))
  }
  else {
    random_model <- optim(0, loglike0)$value
    model <- optim(0, loglike1)$value
    return(data.frame(random_model, model, row.names = "nLLK"))
  }
}

######################################################################################################
# Return the optimized parameters
optim_par <- function(par_n) {
  if (par_n == 2) {
    random_model <- optim(c(0, 0), loglike00)$par
    two_par_model <- optim(c(0, 0), loglike11)$par
    first_par_model <- optim(c(0, 0), loglike10)$par
    second_par_model <- optim(c(0, 0), loglike01)$par
    return(data.frame(random_model,
                      two_par_model,
                      first_par_model,
                      second_par_model,
                      row.names = c("parameter_1", "parameter_2")))
  }
  else {
    random_model <- optim(0, loglike0)$par
    model <- optim(0, loglike1)$par
    return(data.frame(random_model, model,row.names = "parameter"))
  }
}
######################################################################################################
# This is the loglikelihood function we're trying to MINIMIZE
loglike1 <- function(beta){
  
  # calculate numerator
  learned_words<- data %>%
    filter(learned==1) %>%
    mutate(numerator=exp(value*beta)) %>%
    select(month, numerator)
  
  # calculate denumerator
  all_words<- data %>%
    group_by(month) %>%
    summarise(denominator=sum(exp(value*beta))) %>%
    select(month, denominator)
  
  # calculate the return value
  ret<- learned_words %>%
    left_join(all_words) %>%
    summarise(nLLK=sum(log(numerator/denominator)))
  
  return(-ret$nLLK)
  
}

######################################################################################################
loglike0 <- function(beta){
  beta=0
  
  # calculate numerator
  learned_words<- data %>%
    filter(learned==1) %>%
    mutate(numerator=exp(value*beta)) %>%
    select(month, numerator)
  
  # calculate denumerator
  all_words<- data %>%
    group_by(month) %>%
    summarise(denominator=sum(exp(value*beta))) %>%
    select(month, denominator)
  
  # calculate the return value
  ret<- learned_words %>%
    left_join(all_words) %>%
    summarise(nLLK=sum(log(numerator/denominator)))
  
  return(-ret$nLLK)
  
}
######################################################################################################
loglike10 <- function(betas){
  beta<-betas[1]

  # calculate numerator
  learned_words<- data %>%
    filter(learned==1) %>%
    mutate(numerator=exp(value*beta)) %>%
    select(month, numerator)
  
  # calculate denumerator
  all_words<- data %>%
    group_by(month) %>%
    summarise(denominator=sum(exp(value*beta))) %>%
    select(month, denominator)
  
  # calculate the return value
  ret<- learned_words %>%
    left_join(all_words) %>%
    summarise(nLLK=sum(log(numerator/denominator)))
  
  return(-ret$nLLK)
  
}

######################################################################################################
loglike01 <- function(betas){

  beta2<- betas[2]

  # calculate numerator
  learned_words<- data %>%
    filter(learned==1) %>%
    mutate(numerator=exp(value2*beta2)) %>%
    select(month, numerator)
  
  # calculate denumerator
  all_words<- data %>%
    group_by(month) %>%
    summarise(denominator=sum(exp(value2*beta2))) %>%
    select(month, denominator)
  
  # calculate the return value
  ret<- learned_words %>%
    left_join(all_words) %>%
    summarise(nLLK=sum(log(numerator/denominator)))
  
  return(-ret$nLLK)
  
}

######################################################################################################
loglike_11 <- function(betas){
  beta<-betas[1]
  beta2<- betas[2]

  # calculate numerator
  learned_words<- data %>%
    filter(learned==1) %>%
    mutate(numerator=exp(value*beta+value2*beta2)) %>%
    select(month, numerator)
  
  # calculate denumerator
  all_words<- data %>%
    group_by(month) %>%
    summarise(denominator=sum(exp(value*beta+value2*beta2))) %>%
    select(month, denominator)
  
  # calculate the return value
  ret<- learned_words %>%
    left_join(all_words) %>%
    summarise(nLLK=sum(log(numerator/denominator)))
  
  return(-ret$nLLK)
  
}

######################################################################################################
loglike00 <- function(betas){

  beta=0
  beta2=0
  # calculate numerator
  learned_words<- data %>%
    filter(learned==1) %>%
    mutate(numerator=exp(value*beta+value2*beta2)) %>%
    select(month, numerator)
  
  # calculate denumerator
  all_words<- data %>%
    group_by(month) %>%
    summarise(denominator=sum(exp(value*beta+value2*beta2))) %>%
    select(month, denominator)
  
  # calculate the return value
  ret<- learned_words %>%
    left_join(all_words) %>%
    summarise(nLLK=sum(log(numerator/denominator)))
  
  return(-ret$nLLK)
  
}

######################################################################################################
loglike111 <- function(betas, control){
  
  beta<- betas[1]
  beta2<- betas[2]
  beta3<- betas[3]
  
  # calculate numerator
  learned_words<- data %>%
    filter(learned==1) %>%
    mutate(numerator=exp(value*beta+value2*beta2+value3*beta3)) %>%
    select(month, numerator)
  
  # calculate denumerator
  all_words<- data %>%
    group_by(month) %>%
    summarise(denominator=sum(exp(value*beta+value2*beta2+value3*beta3))) %>%
    select(month, denominator)
  
  # calculate the return value
  ret<- learned_words %>%
    left_join(all_words) %>%
    summarise(nLLK=sum(log(numerator/denominator)))
  
  return(-ret$nLLK)
  
}