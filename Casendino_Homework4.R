# Name: Helen Casendino 
# Homework 4

#Q 1A

length.weight <- function(lengths,a,b){
  weight_mat<- rep(NA, length.out=length(lengths))
  
  for(i in 1:length(lengths)){
    weight_mat[i]<- a*lengths[i]^b
  }
  return(weight_mat)
}

hoplo_lengths<- c(20, 25, 30, 35, 40)
length.weight(hoplo_lengths, 0.0631, 2.81)

#Q 1B

age.length <- function(ages, Lmax, K, t0){
  ages_mat<- rep(NA, length.out=length(ages))
  
  for(i in 1: length(ages)){
   ages_mat[i] <- Lmax*(1 - exp(-K*(ages[i] - t0)))
  }
  return(ages_mat)
}

roughy_age<- c(5, 10, 20, 50, 100)
age.length(roughy_age, 40, 0.04, -2.7)


#Q 1C

total.weight<- function(num_vec, weight_vec){
  mat<- rep(NA, length(num_vec))
  
   for(i in 1:length(num_vec)){
    mat[i]<- num_vec[i]*weight_vec[i]
  }
  return(sum(mat))
}

total.weight(c(1000,800,700,500,450,300,100),
             c(100, 400, 650, 800, 900, 960, 1000))

# Q2

weight.from.age <- function(ages, num, a, b, Lmax, K, t0){
     lengths<- age.length(ages, Lmax, K, t0)
     weights<- length.weight(lengths, a, b)
     sum_weight<- total.weight(num,weights)
     return(sum_weight)
  }

weight.from.age(c(1,2,3,4,5,6,7,8,9,10), 
                c(100, 90, 81, 70, 65, 60, 30, 20, 10, 5),
                0.0631, 2.81, 40, 0.04, -2.7)
