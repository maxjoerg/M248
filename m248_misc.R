##########
### median and quartiles - the way minitab does it
##########
test <- c (20.77,
           22.56,
           22.71,
           22.99,
           26.39,
           27.08,
           27.32,
           27.33,
           27.57,
           27.81,
           28.69,
           29.36,
           30.25,
           31.89,
           32.88,
           33.23,
           33.28,
           33.40,
           33.52,
           33.83,
           33.95,
           34.82)
########
## my median
#######
med_max <- function(liste){
  
  n <- length(liste)
  temp <- sort(liste)
  med <- 0
  
  if (n%%2 == 0){
    a <- temp[n/2]
    b <- temp[n/2 + 1]
    med <- (a + b) /2
    
  }
  med
}
######
med_max(test)
### R function
median(test)

###################
##### quartiles
##################

quart_max <- function(liste){
  len <- length(liste)
  lower <- 0.25 * (len + 1)
  upper <- 0.75 * (len + 1)
  first_ind = floor(lower)
  second_ind = floor(upper)
  factor_1 <- lower%%1
  factor_2 <- upper%%1
  first_quart = liste[first_ind] + factor_1 * (liste[first_ind + 1] - liste[first_ind])
  second_quart = liste[second_ind] + factor_2 * (liste[second_ind + 1] - liste[second_ind] )
  res <- c(first_quart, second_quart)
  res
}

### some testdata
test77 <- c(66, 72, 79, 84, 102, 110, 123, 144, 162, 169, 414)
quart_max(test77)
####################

####################

### unit 3

#####

my_cdf_binom <- function(x, size, prob){
  
  prob_sum <- 0
  
  ind = 0
  
  while(ind <= x) {
    
    proba = choose(size,ind) * (prob^ind) * ((1 - prob)^(size - ind))
    print(proba)
    prob_sum = prob_sum + proba
    ind = ind + 1
  } 
  
  prob_sum
  
}

my_cdf_binom(1,7,0.6)  #### 0.0188416

### using R directly - pmf or pdf
dbinom(0,7,0.6) + dbinom(1,7,0.6) ##0.0188416 
###cdf
pbinom(1,7,0.6) #### 0.0188416
######


###################
### my poisson pmf - using recursive definition 
###################
pmf_poisson <- function(lambda, x){
  
  if ( x == 0){
    
    temp = exp(-lambda)
    temp
  }
  else{
    
    temp = (lambda/x) * pmf_poisson(lambda, x - 1)
    temp
  }
}
#### check for lambda = 0.5 and x = 3
pmf_poisson(0.5, 3) #### 0.01263606
### using R directly
dpois(3, 0.5)  ### 0.01263606

#########
### my poisson cdf - using recursive definition

cdf_poisson <- function(lambda, y){
  
  temp = 0
  for (ind in 0:y){
    temp = temp + (lambda**ind) / factorial(ind)
  }  
  
  temp = exp(-lambda) * temp
  
  temp
}

cdf_poisson(0.5, 4) ### 0.9998279
#### using R directly 
ppois(4,0.5) ### 0.9998279


################################


tma01 <- read.csv("tma01.csv", header = FALSE)

hist(tma01$V1, breaks = 18)

library(ggplot2)

ggplot(tma01, aes(V1)) +
  geom_histogram(aes(y = stat(density)), bins = 15 ) +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(tma01$V1), sd = sd(tma01$V1)), 
    lwd = 1, 
    col = 'red'
  )

library(ggplot2)
library(dplyr)
##############
### overlay of 4 different distributions
###############
year <- c( rep(1,100), rep(2,100), rep(3,100), rep(4,100))
height <- c( round(rnorm(100, 1000, 100)), round(rnorm(100, 2000, 200)), round(rnorm(100, 2800, 300)),
             round(rnorm(100, 3200, 400 )))

trees <- data.frame("height" = height, "year" = as.factor(year))
hist(trees$height)

ggplot(trees, aes(x = height, y=..density..)) +
  geom_histogram(aes(color = year, fill = year), 
                 position = "identity", bins = 30, alpha = 0.2) 
## scale_color_manual(values = c("#00AFBB", "#E7B800")) +
## scale_fill_manual(values = c("#00AFBB", "#E7B800"))

write.csv(trees,"trees.csv", row.names = FALSE, sep = ";")

###################
#########
#####################
###M248
## unit 3

### activity 8
p <- 0.14
n <- 6
p0 <- round(dbinom(0,n,p),3)  ###  0.405
p1 <- round(dbinom(1,n,p),3)  ##  0.395 
## p x <= 1
p0_1 <- round(pbinom(1,n,p),3) ###  0.8

print(p0)
print(p1)
print(p0_1)
print(p0 + p1 == p0_1)  ### TRUE

##p x >= 2
pgt1 = 1 - p0_1 ## 0.2

### calculate p: x >= 2 by hand
values  <- 2:6  ## 2 3 4 5 6

prob = 0
for( value in values){
  prob <- prob + dbinom(value,n,p)
}
print(round(prob,3)) ## 0.2

### generate values For X for the given distrib
### mean delivers the proportion of number of ssuccesses = probs
## generate 100000 values for X
mean(rbinom(100000,6,0.14) == 1)  ### approx = p1, proportions of success = 1 in 6
mean(rbinom(100000,6,0.14) == 0)  ### approx = p0, of 0 in 6

#############
## Example 9
### simulation of multiple choice problem
### 20 questions - 5 possibilities, 1 true answer, 4 false
########
questions <- c(0,0,0,0,1)
answers <- function(){
  temp <- sample(questions,20,1)
  res <- sum(temp)
  res
}
tries <- replicate(1000000, answers())
### prob to pass
1 - mean(tries <= 9)   #### 0.00253
#####
##R directly
#####
1 - pbinom(9,20,0.2)  ### 0.002594827
##### simulation comes reallyy close

#######
### geom ditribution according to unit, not the way, R does it
#######
#### doing bernoulli trials, p = 0.2
geom_sim <- function(p){
  count = 1
  while(rbinom(1,1,0.2) < 1){
    count = count + 1
  }
  count
}
##### 
### experiment with 100 samples
sample <- replicate(100, geom_sim(0.2))
mean(sample)
df = as.data.frame(sample)
ggplot(data = df ) + 
  geom_histogram ( mapping = aes(x = sample ), 
                   binwidth = 1, color="black", fill="blue" )

#########
### 1000 samples - closer
sample <- replicate(1000, geom_sim(0.2))
mean(sample)
df = as.data.frame(sample)
ggplot(data = df ) + 
  geom_histogram ( mapping = aes(x = sample ), 
                   binwidth = 1, color="black", fill="blue" )


##### even closer 
sample <- replicate(10000, geom_sim(0.2))
mean(sample)
df = as.data.frame(sample)
ggplot(data = df ) + 
  geom_histogram ( mapping = aes(x = sample ), 
                   binwidth = 1, color="black", fill="blue" ) +
  geom_vline(aes(xintercept = mean(sample)),col='red',size=0.2) +
  geom_text(aes(x=mean(sample), label="\n mean", y=1500),
            colour="red", angle=90, size = 4) +
  geom_text(aes(x=mean(sample), label=mean(sample), y=-100),
            colour="red", size = 3)  

####  
### uses alternative definition of distrib 
### R : does not include the success case, so mean is one below
sample <- rgeom(10000,0.2)
mean(sample)
df = as.data.frame(sample)
ggplot(data = df ) + 
  geom_histogram ( mapping = aes(x = sample ), 
                   binwidth = 1, color="black", fill="blue" ) +
  geom_vline(aes(xintercept = mean(sample)),col='red',size=0.2) +
  geom_text(aes(x=mean(sample), label="\n mean", y=3000),
            colour="red", angle=90, size = 4) +
  geom_text(aes(x=mean(sample), label=mean(sample), y=-100),
            colour="red", size = 3) 

################################










