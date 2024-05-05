# LIBRARIES
library("brms")
library(gplots)
library(ggplot2)
library(colorspace)

# LOAD DATA
setwd("C:/Users/g_gna/Documents/TCD/Modules/CS7DS3_AppliedStatisticalModelling/Assignments/MainAssignment/code")
wine_reviews <- read.csv("wine_review_processed.csv")
wine_reviews$price_log10 <- log10(wine_reviews$price)
min_max_normalize <- function(x) {(x-min(x))/(max(x)-min(x))}
wine_reviews$tfidf_tsne_1_norm <- min_max_normalize(wine_reviews$tfidf_tsne_1)
wine_reviews$tfidf_tsne_2_norm <- min_max_normalize(wine_reviews$tfidf_tsne_2)

# MODEL DATA
# RESPONSE VARIABLE = "superior_rating"
# PREDICTOR VARIABLES = [
#     price_log10, price_log10*tfidf_tsne_1_norm,
#     price_log10*body, alcohol, tannin, Rich,
# ]
# GROUPING VARIABLE = "variety": Type of wine.
prior_beta_j <- prior(normal(0, 10), class=b)
fit <- brm(
  superior_rating ~ 1 + price_log10 + tannin + alcohol + Rich + 
                    price_log10*body + price_log10*tfidf_tsne_1_norm + 
                    (1|variety),
  family = bernoulli(logit),
  data = wine_reviews, 
  prior = prior_beta_j
)

# INTERPRET MODEL OVERALL
prior_summary(fit)
summary(fit)

# INVESTIGATING FIXED EFFECTS
plot(fit, variable =c(
  "b_Intercept", "b_price_log10", "b_price_log10:tfidf_tsne_1_norm",
  "b_price_log10:body"
)) 
plot(fit, variable =c("b_Rich", "b_tannin", "b_alcohol"))

# INVESTIGATING RANOM EFFECTS
fit_mat <- brms::as_draws_matrix(fit)
sample <- as.numeric(fit_mat[, 11:45])
variety_r_effect <- data.frame(
  sample = sample, 
  variety = rep(unique(wine_reviews$variety_c), each = 4000),
  probability = exp(sample) / (1 + exp(sample))
)
ggplot(variety_r_effect) + geom_boxplot(aes(
  reorder(variety, probability, median), probability, 
  fill = reorder(variety, probability, median),
), show.legend=FALSE) + geom_hline(
  aes(yintercept=0.5), linetype = "dashed", 
  color = "red", linewidth=1
) + labs(
  title = "Wine Variety Level Random Intercept Samples",
  x = "Variety", 
  y = "Probability of wine being rated superior after accounting for fixed effects."
)

### Visualize prices against baseline variety based average
### posterior probability of wine being rated as superior.
variety_freq <- table(wine_reviews$variety_c)
variety_freq_post <- table(variety_r_effect$variety)

# Observed probability of wines rated superior per variety (x axis)
# against mean price per variety (y axis).
prob_superior <- aggregate(
  wine_reviews$superior_rating, 
  by = list(wine_reviews$variety_c), 
  FUN = function(x) mean(x == 1)
)
price_mean <- aggregate(
  wine_reviews$price, 
  by = list(wine_reviews$variety_c), 
  FUN = mean
)
plot(
  prob_superior$x, price_mean$x,
  xlab = "Observed probability of wine being rated as superior.", 
  ylab = "Mean Price", 
  col = 0
)
text(
  prob_superior$x,
  price_mean$x,
  prob_superior$Group.1
)

# Calculate mean baseline probability under fixed population effects
# of wines of a particular variety getting rated as superior.
prob_superior_reffect_mean <- tapply(
  variety_r_effect$probability, 
  variety_r_effect$variety, 
  mean
)

plot(
  prob_superior_reffect_mean, 
  price_mean$x,
  xlab = "Baseline posterior probability of wines getting rated as superior after accounting for fixed effects.", 
  ylab = "Observed mean price.", 
  col = 0
)
text(
  prob_superior_reffect_mean, 
  price_mean$x,
  levels(price_mean$Group.1),
)

# PREDICT
input_data <- wine_reviews[, c(
  "price_log10", "tannin", "alcohol", "body",
  "Rich", "variety", "tfidf_tsne_1_norm")]
pp <- predict(fit, newdata = input_data)
boxplot(
  pp[ ,1] ~ wine_reviews$superior_rating, 
  ylab = "Predicted probability"
)

classification_threshold = 0.5
cm <- table(
  pp[ ,1] > classification_threshold, 
  wine_reviews$superior_rating
)

compute_metrics <- function(cm) {
  ### Function that returns accuracy and F2 Score.
  tp <- cm[2, 2]
  tn <- cm[1, 1]
  fp <- cm[1, 2]
  fn <- cm[2, 1]
  acc <- (tp + tn) / sum(cm)
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  metrics <- list(
    tp = tp,
    tn = tn,
    fp = fp,
    fn = fn,
    accuracy = acc,
    f1_score = f1
  )
  
  return(metrics)
}

tp <- cm[2, 2]
tn <- cm[1, 1]
fp <- cm[1, 2]
fn <- cm[1, 1]
acc <- (tp+tn)/(tp+tn+fp+fn)
f1 <- tp/(tp + ((1/2)*(tp/fp+fn)))





