### Homework 6
### Helen Casendino

# Q 1A

trials<- rep(NA, length.out=1000000)

for(i in 1:1000000){
  cards <- rep(c(2:10, "J", "Q", "K", "A"), times=4)
  a <- sample(cards, 7, replace = F)
   if((sum(a == "A")) == 4 ){
    trials[i]<- "success"
   }
  else{
     trials[i]<- "failure"
   }
}

sum(trials=="success") > 0 
# TRUE 
sum(trials=="success")
# 156 

156/1000000
# Probability of getting 4 aces: 0.000156, or 0.0156 % 

# Q 1B 

n_players_prob <- matrix(NA, ncol=10000, nrow=7)

for(k in 1:10000){
  for(i in 1:7){
    n_players <- i
    cards <- rep(c(2:10, "J", "Q", "K", "A"), times=4)
    
    if(i == 1){
      trials<- rep(NA, length.out=i)
      for(a in 1:i){
        hand <- sample(cards, 7, replace = F)
        if((sum(hand == "A")) == 4 ){
          trials[a]<- "success"
        }
        else{
          trials[a]<- "failure"
        }
      }
      n_players_prob[i,k] <- sum(trials=="success")/i
    }
    
    
    if(i == 2){
      trials<- rep(NA, length.out=i)
      for(a in 1:i){
        hand <- sample(cards, 7, replace = F)
        if((sum(hand == "A")) == 4 ){
          trials[a]<- "success"
        }
        else{
          trials[a]<- "failure"
        }
      }
      n_players_prob[i,k] <- sum(trials=="success")/i
    }
    
    
    if(i == 3){
      trials<- rep(NA, length.out=i)
      for(a in 1:i){
        hand <- sample(cards, 7, replace = F)
        if((sum(hand == "A")) == 4 ){
          trials[a]<- "success"
        }
        else{
          trials[a]<- "failure"
        }
      }
      n_players_prob[i,k] <- sum(trials=="success")/i
    }
    
    
    if(i == 4){
      trials<- rep(NA, length.out=i)
      for(a in 1:i){
        hand <- sample(cards, 7, replace = F)
        if((sum(hand == "A")) == 4 ){
          trials[a]<- "success"
        }
        else{
          trials[a]<- "failure"
        }
      }
      n_players_prob[i,k] <- sum(trials=="success")/i
    }
    
    
    if(i == 5){
      trials<- rep(NA, length.out=i)
      for(a in 1:i){
        hand <- sample(cards, 7, replace = F)
        if((sum(hand == "A")) == 4 ){
          trials[a]<- "success"
        }
        else{
          trials[a]<- "failure"
        }
      }
      n_players_prob[i,k] <- sum(trials=="success")/i
    }
    
    
    if(i == 6){
      trials<- rep(NA, length.out=i)
      for(a in 1:i){
        hand <- sample(cards, 7, replace = F)
        if((sum(hand == "A")) == 4 ){
          trials[a]<- "success"
        }
        else{
          trials[a]<- "failure"
        }
      }
      n_players_prob[i,k] <- sum(trials=="success")/i
    }
    
    
    if(i == 7){
      trials<- rep(NA, length.out=i)
      for(a in 1:i){
        hand <- sample(cards, 7, replace = F)
        if((sum(hand == "A")) == 4 ){
          trials[a]<- "success"
        }
        else{
          trials[a]<- "failure"
        }
      }
      n_players_prob[i,k] <- sum(trials=="success")/i
    }
  }
}
   
# Getting probability of 1 or more player getting 4 aces with 1-7 players.
# All non-zero probabilities in n_players_prob indicate this outcome. 

ncol(n_players_prob)*nrow(n_players_prob)
# Number of possible game outcomes: 700000

sum(n_players_prob > 0) 
# Number of non-zero probabilities: 352

# P(1 or more player getting 4 aces with 1-7 players) = 
# Number of non-zero probabilities / Number of game outcomes
(sum(n_players_prob > 0)) / (ncol(n_players_prob)*nrow(n_players_prob))
# P =  0.0007














