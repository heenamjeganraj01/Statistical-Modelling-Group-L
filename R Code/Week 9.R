
# ==========================================================
# MTH6134 Statistical Modelling II
# Week 9 - Contingency Tables II
# Midland Premier League 2023/24
# ==========================================================

# Read in the data
league_players <- read.csv("league_players(1).csv")
ashford_matches <- read.csv("ashford_matches(1).csv")

head(league_players)
summary(league_players)
str(league_players)


# ==========================================================
# 1. Proportional odds - training load and position
# ==========================================================

library(MASS)

# Training load is an ordinal response (1 to 5)
league_players$training_load <- ordered(
  league_players$training_load,
  levels = 1:5
)

league_players$position <- as.factor(
  league_players$position
)

# Explore the relationship between position and training load
table(league_players$position,
      league_players$training_load)

prop.table(
  table(league_players$position,
        league_players$training_load),
  margin = 1
)

# Fit proportional odds model
polr.fit1 <- polr(
  training_load ~ position,
  data = league_players,
  Hess = TRUE
)

polr.fit1
summary(polr.fit1)


# 95% asymptotic confidence intervals for odds ratios
# Same approach as the lecture example

round(
  exp(cbind(
    coef(polr.fit1) -
      1.96 * sqrt(diag(vcov(polr.fit1)))[names(coef(polr.fit1))],
    coef(polr.fit1) +
      1.96 * sqrt(diag(vcov(polr.fit1)))[names(coef(polr.fit1))]
  )),
  2
)

# Estimated odds ratios
round(exp(coef(polr.fit1)), 2)


# ==========================================================
# 2. Fitted category probabilities
# ==========================================================

# Predicted probabilities for each playing position
position.data <- data.frame(
  position = factor(
    levels(league_players$position),
    levels = levels(league_players$position)
  )
)

position.probs <- predict(
  polr.fit1,
  position.data,
  type = "probs"
)

cbind(
  position.data,
  round(position.probs, 2)
)


# ==========================================================
# 3. Proportional odds - additional explanatory variables
# ==========================================================

# Include player characteristics
polr.fit2 <- polr(
  training_load ~ position + age +
    minutes_played + prev_injuries + bmi,
  data = league_players,
  Hess = TRUE
)

polr.fit2
summary(polr.fit2)

# Estimated odds ratios
round(exp(coef(polr.fit2)), 2)

# 95% asymptotic confidence intervals
round(
  exp(cbind(
    coef(polr.fit2) -
      1.96 * sqrt(diag(vcov(polr.fit2)))[names(coef(polr.fit2))],
    coef(polr.fit2) +
      1.96 * sqrt(diag(vcov(polr.fit2)))[names(coef(polr.fit2))]
  )),
  2
)

# Compare models using AIC
AIC(polr.fit1, polr.fit2)

# Likelihood ratio test
anova(polr.fit1, polr.fit2)


# ==========================================================
# 4. Multinomial model - checking proportional odds
# ==========================================================

library(nnet)

# Treat training load as an unordered response
# for comparison with proportional odds regression

league_players$training_nominal <- factor(
  league_players$training_load,
  ordered = FALSE
)

# Multinomial model
mult.fit <- multinom(
  training_nominal ~ position,
  data = league_players,
  trace = FALSE
)

summary(mult.fit)

# Fitted category probabilities
mult.probs <- predict(
  mult.fit,
  position.data,
  type = "probs"
)

cbind(
  position.data,
  round(mult.probs, 2)
)

# Check proportional odds assumption using
# cumulative logits, as in the lecture

mult.cpr <- t(apply(mult.probs, 1, cumsum))

logit <- function(x) log(x / (1 - x))

# Four cumulative logits for five response categories
cum.logits <- logit(mult.cpr[, 1:4])

# Differences in cumulative logits between positions
# relative to the first position

logit.differences <- sweep(
  cum.logits,
  2,
  cum.logits[1, ],
  "-"
)

round(logit.differences, 3)

# Under proportional odds, each row of differences
# should be approximately constant across cut-points.


# ==========================================================
# 5. Fitted probabilities - proportional odds model
# ==========================================================

# Same approach as the housing example in the lecture

polr.probs <- predict(
  polr.fit1,
  position.data,
  type = "probs"
)

cbind(
  position.data,
  round(polr.probs, 2)
)

# Plot estimated probabilities for each position
matplot(
  1:5,
  t(polr.probs),
  type = "b",
  pch = 19,
  lty = 1,
  xlab = "Training Load",
  ylab = "Estimated Probability",
  main = "Estimated Training Load Probabilities",
  xaxt = "n"
)

axis(1, at = 1:5, labels = 1:5)

legend(
  "topright",
  legend = position.data$position,
  col = 1:nrow(position.data),
  pch = 19,
  lty = 1,
  cex = 0.8
)


# ==========================================================
# 6. Match outcomes - additional categorical analysis
# ==========================================================

# Create match result from goals scored and conceded
ashford_matches$result <- with(
  ashford_matches,
  ifelse(
    goals_scored > goals_conceded, "Win",
    ifelse(goals_scored == goals_conceded,
           "Draw", "Loss")
  )
)

ashford_matches$result <- factor(
  ashford_matches$result,
  levels = c("Loss", "Draw", "Win")
)

# Home/away and match outcome
match.table <- table(
  ashford_matches$home_away,
  ashford_matches$result
)

match.table

# Proportions within home/away groups
prop.table(match.table, margin = 1)

# Chi-squared test of association
chisq.test(match.table)

# Check expected frequencies
chisq.test(match.table)$expected

# Plot match outcome frequencies
barplot(
  t(match.table),
  beside = TRUE,
  legend.text = TRUE,
  main = "Match Outcomes by Home/Away",
  xlab = "Home/Away",
  ylab = "Number of Matches"
)


# ==========================================================
# 7. Model comparison
# ==========================================================

# Compare proportional odds and multinomial models
# fitted to the same response and position predictor

AIC(polr.fit1, mult.fit)

# Lower AIC indicates preferred relative fit,
# subject to model assumptions.

# ==========================================================
# End of Week 9 Analysis
# ==========================================================
