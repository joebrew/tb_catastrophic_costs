
# TB costs - sample size calculation - A G Basteiro#

#### Sample size ####

d <- 0.2
k <- 0.4

N <- NULL

for (p in c(0.05,0.1,0.2)) {
  for (m in c(20,30,50,75,100,150)) {
    aux <- (1.96^2*(1-p)/(d^2*p)) * (1+(m-1)*k^2*p/(1-p))
    N <- rbind(N,cbind(p,m,N=aux))
  }
}

N<-as.data.frame(N)


# number of clusters
53272/2
N$Ncorr <- N$N/(1+(N$N-1)/26636)
N$Nperd <- N$Ncorr/0.9
N$clusters <- ceiling(N$Nperd/N$m)

Naux <- N[,c("p","m","clusters")]
res <- reshape(Naux, idvar = "p", timevar = "m", direction = "wide")
colnames(res)<-c("p",20,30,50,75,100,150)
View(res)

# corresponding total sample size
View(t(apply(res,1,function(x){x*c(1,20,30,50,75,100,150)})))


# plot
library(ggplot2)

p1 <- ggplot(data=N, aes(x=m, y=clusters, group=factor(p), colour=factor(p))) +
  geom_line() +
  scale_y_continuous(name="Number of clusters") +
  scale_x_continuous(breaks=c(20,30,50,75,100,150), name="Cluster size") + 
  scale_colour_discrete(name=expression(paste("assumed ",pi))) +
  geom_point()

print(p1)





#### SCENARIO: 500 pacients and simple random sampling ####
f <- function(p) sqrt(1.96^2/500 * (1-p)/p)

x <- c(0.05,0.1,0.2)
View(cbind(x,f(x)))


#### SCENARIO: 500 pacients and 10 clusters (mida m=50) ####
k <- 0.4
m <- 50

f <- function(p) sqrt(1.96^2/(500/(1+(m-1)*k^2*p/(1-p))) * (1-p)/p)

x <- c(0.05,0.1,0.2)
View(cbind(x,f(x)))


#### SCENARIO: 540 pacients and 27 clusters (mida m=20) ####
#tenint en compte les pèrdues
k <- 0.4
m <- 20

f <- function(p) sqrt(1.96^2/(540*0.9/(1+(m-1)*k^2*p/(1-p))) * (1-p)/p)

x <- c(0.05,0.1,0.2)
View(cbind(x,f(x)))


#### SCENARIO: 540 pacients and 18 clusters (mida m=30) ####
#tenint en compte les pèrdues
k <- 0.4
m <- 30

f <- function(p) sqrt(1.96^2/(540*0.9/(1+(m-1)*k^2*p/(1-p))) * (1-p)/p)

x <- c(0.05,0.1,0.2)
View(cbind(x,f(x)))


#### SCENARIO: 540 pacients and 12 clusters (mida m=45) ####
#tenint en compte les pèrdues
k <- 0.4
m <- 45

f <- function(p) sqrt(1.96^2/(540*0.9/(1+(m-1)*k^2*p/(1-p))) * (1-p)/p)

x <- c(0.05,0.1,0.2)
View(cbind(x,f(x)))





