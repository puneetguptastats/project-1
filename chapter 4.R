### Chapter 4 

# Ques 1
mean_integrand<-function(x){
0.00075*x*(100-(x-5)^2)
}

mean_value<-integrate(mean_integrand,-5,15)
mean_value ##  mean = 5


var_integrand<-function(x){
  0.00075*(x-5)*(x-5)*(100-(x-5)^2)
}

variance_value<-integrate(var_integrand,-5,15)
variance_value  ##  variance = 20

# Ques 2

x<-c(-7,5.5)
p<-c(0.04,0.96)

mean_x<-sum(x*p)
mean_x

var_x<-sum((x-mean_x)*(x-mean_x)*p)
var_x

# Ques 3
semi_variance<- variance_value<-integrate(var_integrand,-5,5)
semi_variance ## Since, semi_variance = var/2 = 10 (distribution is symmetrical)

################################################################################
# check symmetry
## mean = 5

## median
findprob <- function(f, interval, target) {
  optimize(function(x) {
    abs(integrate(f, -5, x)$value-target)
  }, interval)$minimum
}

mydensity <- function(x) {
  0.00075*(100-(x-5)^2)
}
findprob(mydensity, interval=c(-5,15), target=.5) ## median = 5

## mode
mydensity <- function(x) {
  0.00075*(100-(x-5)^2)
}
optimise(mydensity, interval=c(-5,15), maximum =TRUE) ## mode 5

## hence mean = median = mode, function is symmetrical
###########################################################################

# Ques 7 (Benchmark 0%)
shortfall_integrad<-function(x)
{
  0.00075*(100-(x-5)^2)
}

shortfall_value<-integrate(shortfall_integrad,-5,0)
shortfall_value


## Ques 10 - VaR
prob_func <- function(f, interval, target) {
  optimize(function(x) {
    abs(integrate(f, -5, x)$value-target)
  }, interval)$minimum
}

mydensity <- function(x) {
  0.00075*(100-(x-5)^2)
}
t = (prob_func(mydensity, interval=c(-5,15), target=.05))/100
t ## t = -2.292974%

VaR<- -t *100
VaR ## We are 95% certain that we will not loss more than 2.29297m


## Ques 12
mu = 8
sigma = 8
prob_func <- function(f, interval, target) {
  optimize(function(x) {
    abs(integrate(f, -Inf, x)$value-target)
  }, interval)$minimum
}

mydensity <- function(x) {
  (exp((-0.5*(x-mu)^2)/(sigma^2)))/(sqrt(2*pi*sigma*sigma))
}
t = (prob_func(mydensity, interval=c(-Inf,Inf), target=.025))/100
t ## t = -2.292974%

VaR<- -t *100
VaR ## We are 95% certain that we will not loss more than 2.29297m




## Ques 13 - Tail VaR or expected shortfall below 2.293 %

expected_shortfall_integrand<-function(x){
  0.00075*(-2.293 - x)*(100-(x-5)^2)
}

Expected_shortfall<-integrate(expected_shortfall_integrand,-5,-2.293)
Expected_shortfall  ##  variance = 20


## Ques 15
a = 0
b = 10
prob_density<- 1/(b-a)

mean_integrand<-function(x){
  x*prob_density
}

mean_value<-integrate(mean_integrand,0,10)
mean_value ##  mean = 5


var_integrand<-function(x){
  ((x-5)^2)*prob_density
  }

variance_value<-integrate(var_integrand,0,10)
variance_value  ##  variance = 20


semi_variance_integrand<-function(x){
  ((x-5)^2)*prob_density
}

semi_variance_value<-integrate(semi_variance_integrand,0,5)
semi_variance_value  ##  variance = 20

Shortfall_prob <- (benchmark-a)*prob_density
Shortfall_prob


benchmark = 3
integrand<-function(x){
  (3-x)*prob_density
}

expected_shortfall<-integrate(integrand,0,3)
expected_shortfall


## Ques 16

A<-c(-3,2,7)
B<-c(-5,2,3)
p<-c(1/3,1/3,1/3)

mean_A<-sum(A*p)
mean_A

var_A<-sum((A-mean_A)*(A-mean_A)*p)
var_A


mean_B<-sum(B*p)
mean_B

var_B<-sum((B-mean_B)*(B-mean_B)*p)
var_B
## A is more riskyacc. to variance

table_A<-data.frame(A,p)
table_A

value_filter_A<- which(table_A$A < 0)
filter_table_A<-table_A[value_filter_A,]

shortfall_prob_A <- sum(filter_table_A$p)
shortfall_prob_A

benchmark = 0
expected_shortfall_A<-sum((benchmark - filter_table_A$A)*filter_table_A$p)
expected_shortfall_A


table_B<-data.frame(B,p)
table_B

value_filter_B<- which(table_B$B < 0)
filter_table_B<-table_B[value_filter_B,]

shortfall_prob_B <- sum(filter_table_B$p)
shortfall_prob_B
## Both A and B are equally risky

benchmark = 0
expected_shortfall_B<-sum((benchmark - filter_table_B$B)*filter_table_B$p)
expected_shortfall_B
## B is risky acc. to expected shortfall