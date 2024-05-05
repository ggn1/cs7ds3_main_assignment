# LIBRARIES
library(ggplot2)

# RESEARCH QUESTION (Did the _ depend on _? By how much?)
# Do points assigned to wine 
# depend on the variety of the wine? 
# By how much?

# LOAD DATA
setwd("C:/Users/g_gna/Documents/TCD/Modules/CS7DS3_AppliedStatisticalModelling/Assignments/MainAssignment/code")
data <- read.csv("wine_review.csv")

# PREPROCESSING
# Make superior_rating a factor.
data['superior_rating'] = factor(data$superior_rating)
# Add a new column "variety_f" such that variety
# is a numeric factor because this is more 
# compact to plot and also easier to remember
# during comparison.
data['variety_f'] = factor(as.numeric(
  factor(data$variety)
))

# VARIABLES
# Response variable = superior_rating
# Predictor variables = Crisp, Dry, Finish, Firm, Fresh, 
#                       Fruit, Rich, Round, Soft, Sweet
# Grouping variable = variety

# EXAMINE DATA

# Q. Of all rated wines, what proportion
# is deemed superior?
tab_sup <- table(data$superior_rating)
pie(
  x=tab_sup, 
  labels=c("not superior", "superior"),
  col = c("#ff7a6e", "#92f5f1")
)
print(paste(
  "Superior % =", 
  (tab_sup[2]/sum(tab_sup))*100
))
# A. Around 40% of all rated wines 
#    in this data set are superior.

# Q. How good of an indicator is price 
#    when it comes to wine ratings?

ggplot(data) + 
  aes(points, price) + 
  geom_point(aes(
    col = superior_rating,
    size = 5, alpha = 0.5
  )) + 
  ggtitle("Wine Points v/s Price") + 
  xlab("Points") + ylab("Price") + 
  labs(color = "Superior") +
  theme(
    legend.position = "top",
    plot.margin = margin(1, 2, 1, 1, "cm"),
  )

ggplot(data) + 
  aes(superior_rating, price) + 
  geom_point(aes(
    col = superior_rating,
    size = 5, alpha = 0.5
  )) + 
  ggtitle("Wine Superiority v/s Price") + 
  xlab("Superior") + ylab("Price") + 
  labs(color = "Superior") +
  theme(
    legend.position = "top",
    plot.margin = margin(1, 2, 1, 1, "cm"),
  )

# A. There is very weak positive correlation between
#    wine ratings and their prices.
#    All non-superior wines are relatively cheap.
#    There are many superior wines that are affordable.
#    The most expensive wine in the data set is 
#    also the most highly rated.
#    There is likely at least one other factor
#    beyond points, that determines wine price.

# Q. Is there a relationship between the variety
#    of wine and it's price?

ggplot(data) + 
  aes(variety, price) + 
  geom_point(aes(
    col = superior_rating,
    size = 5, alpha = 0.5
  )) + 
  ggtitle("Wine Variety v/s Price") + 
  xlab("Variety") + ylab("Price") + 
  labs(color = "Superior") +
  theme(
    legend.position = "top",
    plot.margin = margin(1, 2, 1, 1, "cm"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# A. Within each wine variety, the positive
#    correlation between wine price and 
#    superiority as per ratings, is stronger.
#    So, another factor influencing the price
#    of the wine seems to be its variety.

test <- factor(apply(data[, c("Crisp", "Dry", "Finish", "Firm", "Fresh", "Fruit", "Rich", "Round", "Soft", "Sweet")], 1, paste, collapse = "-"))

# Q. Are some taste factors better indicators
#    of the wine superiority than others?
layout_matrix <- matrix(
  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
  nrow = 2, byrow = TRUE
)
layout(layout_matrix, heights = c(1, 1))
for (taste_factor in c(
  "Crisp", "Dry", "Finish", "Firm", "Fresh",
  "Fruit", "Rich", "Round", "Soft", "Sweet"
)) {
  mosaicplot(
    table(data[, taste_factor], data$superior_rating), 
    col = c("#ff7a6e", "#92f5f1"), 
    main = NULL,
    xlab = taste_factor,
    ylab = "Superior",
  )
}
layout(1)
# A. The most common taste factor is "Fruity" but
#    that doesn't tell us much about the superiority
#    of the wine as but superior and non-superior
#    rated wines have a fruity flavor.
#    Rich wines, although small in proportion, are
#    apparently more likely to be rated highly.
#    Soft wines, although small in proportion, are
#    apparently less likely to be rated highly.
#    No feature alone is a strong enough indicator
#    of superiority.
#    Positive indicators = Rich, Sweet, Firm, Dry, Fruit.
#    Negative indicators = Soft, Crisp, Finish, Fresh, Round.

# Q. What proportion of the data is composed of
#    each type (variety) of wine?
mosaicplot(
  table(data$variety_f,data$superior_rating), 
  col = c("#ff7a6e", "#92f5f1"),,
  main = NULL,
  xlab = "Variety",
  ylab = "Superior Rating"
)
# A. The discrepancy between various wine varieties
#    w.r.t how many instances of each are available
#    in the data set is very wide.
#    For the most common varieties (2, 7, 22),
#    the likelihood of a wine getting rated as 
#    superior is very similar to that of it getting
#    rated as non-superior.
#    Many varieties within which it is significantly 
#    more/less likely to find superior wines make up
#    are fewer in number.

# Q. How does the spread and centrality of 
#    points within each variety differ from
#    each other and overall points?
ggplot(data) + geom_boxplot(aes(
    reorder(variety, points, median), points, 
    fill = reorder(variety, points, median)
  ), show.legend=FALSE) + 
  labs(
      title = "Box Plot of Points by Variety",
      x = "Variety", 
      y = "Points"
  ) + coord_flip()

features = c("min", "q1", "median", "q2", "max")
five_num_sum = fivenum(data$points)
print(data.frame(features, five_num_sum))
#   features five_num_sum
# 1      min           80
# 2       q1           86
# 3   median           88
# 4       q2           91
# 5      max          100

# A. The distribution of points within each
#    wine variety does not vary much between
#    varieties when compared to overall
#    five number summary.

# Q. What is the frequency distribution of
#    each variety?
barplot(
  sort(table(data$variety_f)), 
  main = "Wine Variety Counts",
  xlab = "Variety",
  ylab = "Count",
  cex.names = 0.8
)
# A. Variety 7 and 2 are the most abundant ones.
#    For a lot of the varieties of wine, there are
#    very few instances in the data set.

# MODEL DATA
# RESPONSE VARIABLE = "superior_rating": Indicates whether the wine has received >= 90 points. 
# PREDICTOR VARIABLES = ["Crisp", "Dry", "Finish", "Firm", "Fresh", "Fruit", "Full", "Rich", "Round", "Soft", "Sweet"]: These variables capture wine taste/mouth feel characteristics. They take values of 0 or 1. In each case, they indicate whether the wine characteristic is related to the predictor variable name. For example, if the wine was fruity, then indicator predictor variable "Fruit" = 1.
# GROUPING VARIABLE = "variety": Indicates type/brand of wine.




