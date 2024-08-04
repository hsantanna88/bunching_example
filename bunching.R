# Load necessary libraries
library(MASS)     # for negative binomial distribution
library(tidyverse) # tidy data manipulation
library(AER)

# Set seed for reproducibility
set.seed(123)


n = 1000 # number of women

educ <- rbeta(n, 5, 2)
educ <- round(educ * 21) 

Pi = -2 # Coefficient for cigs_star versus educ
eta = rnorm(n, 10, 20) # Set of unobservables

# Generating the confounder
cigs_star  <- Pi * educ + eta

# cigs_star path
hist(cigs_star)

# Proportions of cigs_star that are non positive
sum(cigs_star <= 0)/length(cigs_star)

# Censoring the data
cigs <- ifelse(cigs_star < 0, 0, cigs_star)

# cigs path
hist(cigs)


# Generate the birth weight
intercept = 3000 # some average birth weight
beta = -20 # Coefficient for birth weight versus cigs
gamma = 5 # Coefficient for birth weight versus educ
delta = -10 # Coefficient for birth weight versus the unobservables

# Generate the birth weight
bw = intercept + beta * cigs + gamma * educ + delta * eta + rnorm(n, 0, 100)

#Histogram of birth weights
hist(bw)

# plot of birth weight versus cigs
plot(cigs, bw)

# naive regression
naive_model <- lm(bw ~ cigs + educ)
summary(naive_model)

# Fit Tobit model
tobit_model <- tobit(cigs ~ educ, left = 0)
summary(tobit_model)

cigs_input <- ifelse(cigs == 0, predict(tobit_model), cigs)


# Fit linear model
lm_model <- lm(bw ~ cigs + educ + cigs_input)
summary(lm_model)

lm_model$coefficients[3] + tobit_model$coefficients[2] * lm_model$coefficients[4]


# Making cool DAGs with ggdag
library(tidyverse)
library(ggdag)


# Create the DAG
dag <- dagify(
  bw ~ cigs + educ + eta,
  cigs ~ educ + eta,
  labels = c(
    "bw" = "Birth Weight",
    "cigs" = "Cigarettes",
    "educ" = "Education",
    "eta" = "Unobserved\nFactors"
  ),
  exposure = "cigs",
  outcome = "bw"
)

# Create a prettier plot
X11()

ggdag_dseparated(dag, "eta", text = FALSE, use_labels = "label") +
  geom_dag_text(aes(label = name)) +
  theme_dag() +
  theme(legend.position = "none")

data <- data.frame(cigs = cigs, bw = round(bw, 0))

ggplot(data, aes(x = cigs, y = bw)) +
  geom_point(aes(color = cigs), alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "red", fill = "pink", alpha = 0.2) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Cigarette Consumption versus Birth Weight",
       x = "Number of Cigarettes Smoked per Day",
       y = "Birth Weight (grams)",
       color = "Cigarettes\nper Day") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )