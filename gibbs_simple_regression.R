data <- read.csv("http://www.stat.yale.edu/~jtc5/238/data/crying.csv")
x <- data$crying
y <- data$IQ
n <- length(x)
# Hyperparameters
r <- 0; u <- .000001
s <- 0; v <- .000001
g <- .001; h <- .001
nit <- 10000
alphas <- numeric(nit)
betas <- numeric(nit)
taus <- numeric(nit)
liks <- numeric(nit)
# initial values
alpha <- alphas[1] <- 0
beta <- betas[1] <- 0
tau <- taus[1] <- 1
lik <- liks[1] <- sum(dnorm(y, mean = alpha+beta*x, sd = 1/sqrt(tau), log = TRUE))
for(i in 1:(nit-1)){
alpha <- rnorm(1,
(u*r+tau*sum(y-beta*x))/(u+tau*n),
1/sqrt(u+tau*n))
alphas[i+1] <- alpha
beta <- rnorm(1,
(v*s+tau*sum(x*(y-alpha))) / (v+tau*sum(x^2)),
1/sqrt(v+tau*sum(x^2)))
betas[i+1] <- beta
tau <- rgamma(1, g+n/2, h+(1/2)*sum((y-alpha-beta*x)^2))
taus[i+1] <- tau
liks[i+1] <- sum(dnorm(y, mean = alpha+beta*x, sd = 1/sqrt(tau), log = TRUE))
}
plot(liks[1:2000])
