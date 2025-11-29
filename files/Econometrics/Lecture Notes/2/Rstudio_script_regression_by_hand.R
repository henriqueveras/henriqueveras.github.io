#Estimating the OLS model NA TORA

rm(list = ls())            # Remove todos os objetos do ambiente

y <- matrix(c(6,11,4,3),4,1)
y

X <- matrix(c(1,1,1,1,4,7,2,1,2,6,9,4),4,3)
X

X.prime <- t(X)
X.prime

X.prime.X <- X.prime %*% X
X.prime.X

X.prime.X.inv<-solve(X.prime.X)
X.prime.X.inv

X.prime.X.inv.X.prime<-X.prime.X.inv %*% X.prime
X.prime.X.inv.X.prime

b<-X.prime.X.inv.X.prime %*% y
b

#Confirming our calculations
lm(y~0+X)

#Find the fitted values:
y.hat <- X %*% b 
y.hat

#Find the residuals
e <- y - y.hat
e

#Get sum of fitted and resid
y.model <- y.hat + e
y.model
y

#Getting the M and P matrices
X.X.prime.X.inv.X.prime<-X %*% X.prime.X.inv.X.prime
X.X.prime.X.inv.X.prime

identity<-diag(4)

M<-identity - X.X.prime.X.inv.X.prime
P<- X.X.prime.X.inv.X.prime

#Compare residuals with the ones obtained before
M %*% y
e
P %*% y
y.hat

#Create partition with X1=(1 1 1 1):
X1 <- matrix(c(1,1,1,1),4,1)
X1

X1.prime <- t(X1)
X1.prime

X1.prime.X1 <- X1.prime %*% X1
X1.prime.X1

X1.prime.X1.inv<-solve(X1.prime.X1)
X1.prime.X1.inv

X1.prime.X1.inv.X1.prime<-X1.prime.X1.inv %*% X1.prime
X1.prime.X1.inv.X1.prime

#Getting the M1 and P1 matrices
X1.X1.prime.X1.inv.X1.prime<-X1 %*% X1.prime.X1.inv.X1.prime
X1.X1.prime.X1.inv.X1.prime

identity<-diag(4)

M1<-identity - X1.X1.prime.X1.inv.X1.prime
P1<- X1.X1.prime.X1.inv.X1.prime

#Getting variables as deviations from mean
y_deviations <- M1 %*% y
y_deviations
X2 <- matrix(c(4,7,2,1,2,6,9,4),4,2)
X2
X2_deviations <- M1 %*% X2

#Estimating the model using variables in deviation format
lm(y_deviations ~ X2_deviations)

lm(y~0+X)

#Find squared sum of total
y.prime<-t(y)
sst <- y.prime %*% M1 %*% y

#Find sum of squared residuals
e.prime<-t(e)
e.prime.e <- e.prime %*% e
e.prime.e

#Find R-squared
R2 <- 1- e.prime.e/sst
R2

#Find adjusted R-squared
adj_R2 <- 1 - ((nrow(X)-1)/(nrow(X)-ncol(X)))*(1-R2)
adj_R2