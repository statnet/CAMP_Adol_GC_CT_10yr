#########################################################
# Testing that logistic regression with proportions and weights 
#   works the same as full data 

x1 <- 1:4
x2 <- 1:5
coefs <- c(-1.2, -0.1, 0.2)
samplesize <- 1e4

my_df <- data.frame(x1=sample(x1,samplesize,replace=TRUE),
                    x2=sample(x2,samplesize,replace=TRUE))
logodds <- coefs[1] + my_df$x1*coefs[2] + my_df$x2*coefs[3] 
probs <- exp(1)^logodds / (1+ exp(1)^logodds)
my_df$y <- rbinom(samplesize, 1, probs)

my_summary_df <- expand.grid(x1, x2, 0:1)
colnames(my_summary_df) <- c('x1', 'x2', 'y')
my_summary_df$n <- NA
for(i in 1:nrow(my_summary_df)) {
  my_summary_df$n[i] <- sum(my_df$x1==my_summary_df$x1[i] & 
                         my_df$x2==my_summary_df$x2[i] &
                         my_df$y==my_summary_df$y[i])
}

ncombos_x1x2 <- nrow(my_summary_df)/2
my_prop_df <- expand.grid(x1, x2)
colnames(my_prop_df) <- c('x1', 'x2')
my_prop_df$prop <- NA
nocounts <- my_summary_df$n[1:ncombos_x1x2]
yescounts <- my_summary_df$n[(1:ncombos_x1x2) + ncombos_x1x2]

for(i in 1:nrow(my_prop_df)) {
  my_prop_df$prop <- yescounts / (yescounts + nocounts)
  my_prop_df$n <- yescounts + nocounts
}

(logreg_full <- glm(y~x1+x2, data=my_df, family='binomial'))
(logreg_wts <- glm(y~x1+x2, data=my_summary_df, weights=n, family='binomial'))
(logreg_props <- glm(prop~x1+x2, data=my_prop_df, weights=n, family='binomial'))

summary(logreg_full)
summary(logreg_wts)
summary(logreg_props)

## All three exactly the same!!!



