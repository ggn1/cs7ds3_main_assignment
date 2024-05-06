# LIBRARIES
library(gplots)
library(ggplot2)
library(colorspace)

# LOAD DATA
setwd("C:/Users/g_gna/Documents/TCD/Modules/CS7DS3_AppliedStatisticalModelling/Assignments/MainAssignment/code")
wine_reviews <- read.csv("wine_review_processed.csv")

### RESPONSE VARIABLE DISTRIBUTION
tab_sup <- table(wine_reviews$superior_rating)
pie(
  x=tab_sup, 
  main=paste(
    "Proportion of Superior Wines = ", 
    tab_sup[2]/(tab_sup[1]+tab_sup[2])*100,
    "%"
  ),
  labels=c("not superior", "superior"),
  col = c("lightgray", "lightgoldenrod1")
)
# OBSERVATIONS:
# * Portion of wines ranked superior in this data set is close
#   to half at around 40%. Hence, this data set is fairly
#   balanced w.r.t superior and non-superior wines.

### TOP 6
wine_reviews[sample(1:length(wine_reviews), 6), ]

### WINE PRICES

### Skewed distribution.
hist(
  wine_reviews$price, 
  main = "Distribution of Wine Prices",
  xlab = "Prices",
  ylab = "Frequency",
  col = "lightpink",
  border = "white"
)

### Adjusted for skewness.
price_log10 = log10(wine_reviews$price)
hist(
  price_log10, 
  main = "Distribution of Wine Prices (Log 10)",
  xlab = "Prices",
  ylab = "Frequency",
  col = "skyblue",
  border = "white"
)
wine_reviews["price_log10"] <- price_log10

### COUNTS + PROPORTIONS

### Wine count per price bracket.
price_bracket_count = table(reorder(price_bracket, wine_reviews$price))
barplot(
  price_bracket_count,
  main = "Distribution of Wines Per Price Bracket",
  xlab = "Price Bracket",
  ylab = "Count",
  col = "slateblue",
  border = "white"
)
# OBSERVATIONS:
# * No. of wines per price bracket is normally distributed.
# * Wine prices themselves are heavily skewed right and thus
#   can benefit from a log transformation before being provided
#   as input to a model.

### Proportion of superior wines per price bracket.
mosaicplot(
  table(
    reorder(wine_reviews$price_bracket, wine_reviews$price), 
    reorder(wine_reviews$superior_rating, wine_reviews$price)
  ), 
  col = c("lightgray", "lightgoldenrod"), 
  main = "Wine Superiority by Price",
  xlab = "Wine Price Bracket (Increasing Order)",
  ylab = "Rated Superior"
)
# OBSERVATIONS
# * There is a clear linear relationship between probability 
#   of a wine having been rated superior and its price. Thus,
#   when using a linear model, the "price" variable is likely 
#   a promising predictor.
# * No. of data points per wine price bracket is uneven in this
#   data set with wines belonging to the "value", "super luxury".
#   and "icon" price brackets being significantly rarer than those
#   those corresponding to other price brackets.

### Wine count v/s proportion rated superior per variety.
variety_count <- tapply(
  wine_reviews$superior_rating, 
  wine_reviews$variety_c, length
)
variety_proportion_superior <- tapply(
  wine_reviews$superior_rating, 
  wine_reviews$variety_c, mean
)
plot(
  variety_proportion_superior, variety_count,
  xlab = "Proportion of Wines Rated Superior", 
  ylab = "No. of Wines", 
  col = 0
)
text(
  variety_proportion_superior, variety_count,
  names(variety_proportion_superior)
)
abline(
  v = mean(wine_reviews$superior_rating=="1"), 
  col = "red", lwd = 2, lty = 2
)
# OBSERVATIONS
# * The most common variety of wine in the data set is 
#   "Bordeaux-style Red Blend" and "Chardonnay". 
# * The distribution of the number of wines per variety appears 
#   to follow a bell-shaped curve with a positive skew in relation to 
#   the proportion of wines rated superior within each variety.
# * For most wine varieties in this data set, there is a
#   20% to 60% chance of wines being rated as being superior.
# * It appears to be the case that the more no. of data points 
#   there are for a variety the more likely it is that nearly 
#   half of those wines get rated superior while the other half
#   gets rated lower.

### Mean price and count per variety.
price_mean_sorted <- price_mean[order(price_mean$x), ]
plot(
  price_mean_sorted$x,
  xlab = "Variety",
  ylab = "Mean Price",
  type = "b",
  col='darkgreen',
  xaxt = "n"  # Hide x-axis labels
)
axis(
  1, at = 1:length(price_mean_sorted$Group.1), 
  labels = price_mean_sorted$Group.1, las = 2
)
title(main = "Mean Price of Wine Varieties")

variety_count <- as.data.frame(
  table(wine_reviews$variety_c)
)
variety_count_sorted <- variety_count[
  order(variety_count$Freq), 
]
plot(
  variety_count_sorted$Freq,
  xlab = "Variety",
  ylab = "Frequency",
  type = "b",
  col = 'blue',
  xaxt = "n"  # Hide x-axis labels
)
axis(
  1, 
  at = 1:length(variety_count_sorted$Var1), 
  labels = variety_count_sorted$Var1, 
  las = 2
)
title(main = "Wine Count per Variety")
# OBSERVATIONS
# * There is a wide gap between no. of wines in the 
#   the category with highest no. of wines "Chardonnay" (417)
#   and the variety with the lowest no. of data points
#   "Petit Manseng" (3) = 417 - 3 = 414.

### WINE CHARACTERISTICS

### Binary indicators.
layout_matrix <- matrix(
  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
  nrow = 2, byrow = TRUE
)
layout(layout_matrix, heights = c(1, 1))
for (characteristic in c(
  "Crisp", "Dry", "Finish", "Firm", "Fresh", "Fruit",
  "Rich", "Ready", "Round", "Soft", "Sweet", "Full"
)) {
  mosaicplot(
    table(
      wine_reviews[, characteristic], 
      wine_reviews$superior_rating
    ), 
    col = c("lightgray", "lightgoldenrod"), 
    main = NULL,
    xlab = characteristic,
    ylab = "Superior",
  )
}
layout(1)
# OBSERVATIONS:
# * The most common characteristic is "Fruity" but
#   that doesn't tell us much about the superiority
#   of the wine as both superior and non-superior
#   rated wines have a fruity flavor.
# * Rich wines, although small in proportion, are
#   apparently more likely to be rated highly.
# * Soft wines, although small in proportion, are
#   apparently less likely to be rated highly.
# * No feature alone is a strong enough indicator
#   of superiority.
# * Positive indicators = Rich, Sweet, Firm, Dry, Fruit.
# * Negative indicators = Soft, Crisp, Finish, Fresh, Round.
# * Possibly useful indicators = Rich, Soft, Crisp.

### ENGINEERED SCORES
layout_matrix <- matrix(
  c(1, 2, 3, 4, 5), 
  nrow = 1, byrow = TRUE
)
layout(layout_matrix, heights = c(1, 1))
for (characteristic in c(
  "sweetness", "acidity", "tannin", "alcohol", "body"
)) {
  hist(
    wine_reviews[,characteristic], 
    main = NULL,
    xlab = characteristic,
    ylab = "Frequency",
    col = 'purple',
    border = "white"
  )
}
layout(1)
# OBSERVATIONS:
# * All key wine characteristic scores are normally distributed.

### WINE CHARACTERISTICS CROSS PLOT
colors <- c("royalblue", "lightgoldenrod1")
selection <- wine_reviews[, c(
  "alcohol", "tannin", "sweetness",
  "body", "acidity", "price_log10"
)]
pairs(selection, col=colors[wine_reviews$superior_rating + 1], pch = 19)
# OBSERVATIONS:
# * Price log 10 against all others show linear separability.
# * Body is good predictor. Others not needed as apparently no
#   useful mutual interactions.
# * Interaction between price and body is important.
selection <- wine_reviews[, c(
  'body', 'tfidf_tsne_1', 'tfidf_tsne_2', "price_log10"
)]
pairs(selection, col=colors[wine_reviews$superior_rating + 1], pch = 19)
# OBSERVATIONS:
# * From linear separation point of view,
#   interaction between price and tfidf_tsne_1 
#   as well as between price and body may be important.

### SENTIMENT
sentiment_rounded = round(wine_reviews[, "sentiment"], 1)
mosaicplot(
  table(
    reorder(sentiment_rounded, sentiment_rounded), 
    reorder(wine_reviews$superior_rating, sentiment_rounded)
  ), 
  col = c("lightgray", "lightgoldenrod"), 
  main = NULL,
  xlab = "Sentiment",
  ylab = "Superior",
)
# OBSERVATIONS.
# * Sentiment in reviews follow a fairly normal distribution that's
#   centered around 0.2 with most wine reviews being neutral to 
#   slightly positive with sentiments ranging between 0 and 0.4.
# * Overall, reviews are generally positive with no extremely
#   negative comments.
# * Sentiment is likely not a good predictor because w.r.t most 
#   data points, the differences in sentiment do not significantly
#   affect the proportion of superior to non-superior ratings.
# * Also, it seems there may be mismatches between the sentiment
#   expressed in the review and the score assigned because the 
#   most positive sentiments (>= 0.8) hasve superior_rating = 0
#   despite that.

### CORRELATIONS
### Visualize correlations between potential predictor variables
### and response variable as well as each other.
correlation_matrix <- cor(wine_reviews[,c(
  "superior_rating", "price_log10", "Finish", "Rich", "Soft", "Ready", 
  "sweetness", "acidity", "tannin", "alcohol", "body", 
  "tfidf_tsne_1", "tfidf_tsne_2"
)])
heatmap.2(
  correlation_matrix,
  col = colorRampPalette(c("blue", "white", "red"))(100),  # Define color palette
  symm = FALSE,  # Do not show symmetric scale
  margins = c(10, 10),  # Add margins for row and column names
  scale = "none",  # Do not scale data
  main = "Correlation Matrix",  # Title of the plot
  cex.main = 1.2,  # Font size of the title
  key = TRUE,  # Show color key (legend)
  keysize = 1.5,  # Size of the color key
  key.title = NA,  # Do not show color key title
  trace = "none",  # Do not show trace lines
  revC = TRUE,  # Reverse color scale
  dendrogram = "none" # Do not calculate dendrograms
)
# OBSERVATIONS:
# * Based on the correlation matrix, good choices
#   for predictor variables when the response variable
#   is "superior_rating", are "price_log10", "tannin",
#   "alcohol", "body", acidity", "Soft", "Rich" and "tfidf_tsne_2".
# * As previously suspected, "price_log10" is the 
#   variable that is most correlated with "superior_rating".
# * Variables "acidity", "sweetness" and "body" are not 
#   chosen in order to minimize multicollinearity. Variable
#   "Soft" was also dropped due to this reason.
# * Only indicator variable considered is "Rich" because it
#   has minimal correlation with other predictors while having
#   a higher correlation with the response variable.
