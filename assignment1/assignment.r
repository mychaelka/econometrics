library(gganimate)
library(gifski)
library(tidyverse)
library(jpeg)
library(pacman)
library(grid)
library(broom)
library(gridExtra)

rm(list=ls())
set.seed(666)

############### 1 REGRESSION BASICS ###############
# Auxiliary functions
# Find significant regressors
significant_regressors <- function(y, X, signif_level) {
  m <- lm(y~X)
  m_summary <- summary(m)
  signif <- which(m_summary$coefficients[,4] < signif_level)
  
  return(signif)
}

# Remove insignificant regressors
remove_insignificant <- function(y, X, signif_level) {
  m <- lm(y~X)
  signif <- significant_regressors(y, X, signif_level) - 1
  X_reduced <- X[,signif]
  
  return(X_reduced)
}


# Algorithm for steps 1-5
algorithm <- function(m, n, nsteps = 4) {
  # m = number of regressors
  # n = number of observations
  # return value = number of significant regressors after 5 steps
  
  if (nsteps > 4)  {
    nsteps = 4
  }
  
  y <- rnorm(n, 0, 1)
  X <- replicate(m, rnorm(n, 0, 1))
  alpha <- c(0.5, 0.25, 0.1, 0.05)
  
  for (i in 1:nsteps) {
    X <- remove_insignificant(y, X, alpha[i])
  }
  
  return(ncol(X))
}


# Simulation
simulation <- function(n, nsteps) {
  num_signif <- c()
  pb <- txtProgressBar(0, n, style = 3)
  
  for (i in 1:n) {
    setTxtProgressBar(pb, i)
    num_signif[i] <- algorithm(200, 1000, nsteps)
  }
  close(pb)
  
  return(num_signif)
}


# how many regressors are significant at level 0.05 in step 4?
num_signif <- simulation(1, 4)
num_signif

# Get histogram of no. significant regressors
number_signif_histogram <- function(nsteps = 4) {
  num_signif <- simulation(1000, nsteps)
  data <- data.frame(num_signif)
  
  p <- ggplot(data, aes(num_signif)) +
    theme_bw() +
    geom_histogram(aes(y = ..density..), bins = 15, col = "black", alpha = 0.7,
                   fill = "navyblue") +
    geom_density(col = "red", lwd = 1.5) +
    labs(x = "Number of significant regressors", y = "Density", 
         title = sprintf("Steps: %s", nsteps+1)) +
    theme(text = element_text(size = 16, family = "Times"))
  
  return(p)
}


# Compare the distributions of the numbers of significant regressors in
# different steps of the algorithm -> number of significant regressors is
# higher with fewer steps, distribution seems to be normal in each case
# !! takes some time to evaluate !! 
number_signif_histogram(4)
number_signif_histogram(3)
number_signif_histogram(2)
number_signif_histogram(1)


############### 2 MAXIMUM LIKELIHOOD ###############
set.seed(44)
bet <- c()  # vector for beta_1 coefficient estimations

k <- 2^seq(0,7,by=1)
n <- 1
C <- data.frame(matrix(NA, nrow = 10*k[8], ncol = 8))

for (j in k) {
  for (i in 1:(10*j)){
    # generate random data
    y <- rbinom(100, size=1, prob=0.2)
    x <- rnorm(100)
    
    # calculate the estimation of beta_1
    model <- glm(y ~ x, family = 'binomial')
    bet[i] <- model$coefficients[2]
  }
  C[1:i,n] <- bet
  bet <- c()
  n <- n+1
}


# Animated histogram for distribution of MLE estimator of beta_1
names(C) <- c(10*k)
X <- stack(C)
X <- na.omit(X)

img <- readJPEG("meme.jpg", native = FALSE)
p <- ggplot(X, aes(x=values)) +
  annotation_custom(rasterGrob(img,
                               width = unit(1,"npc"),
                               height = unit(1,"npc")),
                    -Inf, Inf, -Inf, Inf) +
  theme_bw() +
  geom_histogram(binwidth=0.1, col = "black", alpha = 0.8,
                 fill = "pink") +
  scale_y_continuous(expand = expansion(mult = c(0.17, 0.1))) +
  transition_states(ind, wrap = F) +
  labs(title = 'Number of iterations: {closest_state}') +
  view_follow()

animate(p, fps = 25, duration = 20, end_pause = 100, renderer = gifski_renderer())


# How does sample size affect the variance of the estimator?
sample_size <- 15
vars <- c()  # vector for calculated variances
size <- c()  # vector for sample sizes
m <- 1

for (i in 1:10) {
  y <- rbinom(sample_size, size=1, prob=0.2)
  x <- rnorm(sample_size)
  
  model <- glm(y ~ x, family = 'binomial')
  vars[m] <- (summary(model)$coef[2,2])^2
  size[m] <- sample_size
  sample_size = round(sample_size * 1.5)  # increase sample size
  m = m + 1
}

data <- data.frame(size, vars)
data %>% ggplot(aes(x = size, y = vars)) +
  geom_point(size = 3, col = "navyblue") +
  geom_line(size = 0.1) +
  theme_bw() +
  labs(x = "Sample size", y = "Estimator variance") +
  theme(text = element_text(size = 16, family = "Times"))
  

############### 3 BOOTSTRAP ###############
set.seed(55)

# Constructs a 95% confidence interval based on nonparametric bootstrap
# Does this nb-times as a simulation
# Returns the percentage of cases in which the CI covers the true value 
confint_bootstrap <- function(nb, x, y, model) {
  n = length(x)
  result = NULL
  ptm <- proc.time()
  
  for (i in 1:nb) {
    rand_indices = sample(c(1:n), n, replace = T)
    
    m1 <- glm(y[rand_indices] ~ x[rand_indices], family = 'binomial')
    suppressMessages(conf <- confint(m1, 2, 0.95))
    result[i] <- conf[1] < summary(model)$coefficients[2,1] & 
      summary(model)$coefficients[2,1] < conf[2]
  }
  proc.time() - ptm
  
  return(sum(result)/nb)
  
}

nb = 5000
confidence <- confint_bootstrap(nb, x, y, model)
confidence


