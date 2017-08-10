# A bunch of optims
######################################################################################################
# Return the nLLK of each model
optim_nllk <- function(par_n) {
  if (par_n == 2) {
    random_model <- optim(c(0, 0), loglike00)$value
    two_par_model <- optim(c(0, 0), loglike_all)$value
    first_par_model <- optim(c(0, 0), loglike1)$value
    second_par_model <- optim(c(0, 0), loglike2)$value
    
    return(data.frame(random_model,
      two_par_model,
      first_par_model,
      second_par_model, row.names = "nLLK"))
  }
  else {
    random_model <- optim(0, loglike0)$value
    model <- optim(0, loglike)$value
    return(data.frame(random_model, model, row.names = "nLLK"))
  }
}

######################################################################################################
# Return the optimized parameters
optim_par <- function(par_n) {
  if (par_n == 2) {
    random_model <- optim(c(0, 0), loglike00)$par
    two_par_model <- optim(c(0, 0), loglike_all)$par
    first_par_model <- optim(c(0, 0), loglike1)$par
    second_par_model <- optim(c(0, 0), loglike2)$par
    return(data.frame(random_model,
                      two_par_model,
                      first_par_model,
                      second_par_model,
                      row.names = c("parameter_1", "parameter_2")))
  }
  else {
    random_model <- optim(0, loglike0)$par
    model <- optim(0, loglike)$par
    return(data.frame(random_model, model,row.names = "parameter"))
  }
}
######################################################################################################
# This is the loglikelihood function we're trying to MINIMIZE
loglike <- function(beta){
  #beta=0
  
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
loglike1 <- function(betas){
  beta<-betas[1]
  beta2<- betas[2]
  #beta=0
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
loglike2 <- function(betas){
  beta<-betas[1]
  beta2<- betas[2]
  beta=0
  #beta2=0
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
loglike_all <- function(betas){
  beta<-betas[1]
  beta2<- betas[2]
  #beta=0
  #beta2=0
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
  beta<-betas[1]
  beta2<- betas[2]
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