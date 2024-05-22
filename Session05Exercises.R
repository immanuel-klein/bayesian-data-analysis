library(ggplot2)
library("DescTools") 
library(HDInterval)

sim_tosses <- function(n, p){
  sample(c("L", "W"), size=n, replace=TRUE, prob=c(p, 1-p))
}

posterior <- function(data, cp, prior){ 
  likelihood <- dbinom(sum(data=="L"), length(data), prob = cp)
  posterior <- (prior*likelihood)/sum(prior*likelihood)
  
  posterior
}

cp <- seq(0,1,.1)
prior <- 0.091
p1 <- posterior(sim_tosses(10, .1), cp, prior)
df <- data.frame(cp = cp, prior = prior, posterior = p1)
ggplot(df, aes(x = cp)) +
  geom_line(aes(y = prior, color = "Prior")) +
  geom_line(aes(y = posterior, color = "Posterior")) +
  labs(title = "Prior and Posterior Distributions",
       x = "Candidate Probability",
       y = "Probability") +
  scale_color_manual(values = c("Prior" = "blue", "Posterior" = "red"))

cp <- seq(0,1,.1)
prior <- p1
p2 <- posterior(sim_tosses(10, .1), cp, prior)
df <- data.frame(cp = cp, prior = prior, posterior = p2)
ggplot(df, aes(x = cp)) +
  geom_line(aes(y = prior, color = "Prior")) +
  geom_line(aes(y = posterior, color = "Posterior")) +
  labs(title = "Prior and Posterior Distributions",
       x = "Candidate Probability",
       y = "Probability") +
  scale_color_manual(values = c("Prior" = "blue", "Posterior" = "red"))

set.seed(123)
sample <- sample(cp, 1000, replace = TRUE, p1)
mean <- mean(sample)
sd <- sd(sample)
mode <- Mode(sample)
pi <- quantile(sample, probs = c(0.055, 0.945))
hdpi <- hdi(sample, .89)
print(paste("Mean:", mean))
print(paste("SD:", sd))
print(paste("Mode:", mode))
print(paste("PI:", pi[1], "-", pi[2]))
print(paste("HDPI:", hdpi[1], "-", hdpi[2]))


