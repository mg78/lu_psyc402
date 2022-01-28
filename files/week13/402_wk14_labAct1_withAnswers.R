# PSYC402: Week 14 - Lab activities

# Our research question: Do professors' beauty score and age predict how
# students evaluate their teaching?

# Step 1: Set-up ------------------------------------------------------------------
#
# Empty R environment
rm(list=ls())                            

# Load relevant libraries
library(broom)
library(car)
library(pwr)  
library(tidyverse)
library(lsr)

# TASK: Read in the data file beauty.csv; call it "beauty". Have
# a look at the data file.
# HINT: Use the read_csv() and head() functions.

beauty <- read_csv("beauty.csv")    
head(beauty)

# QUESTION: Do you notice anything about the name of one of the variables and the data table
# of the data table?
# ANSWER: Both the data table and one of the variables are called 'beauty'. Not a problem,
# as such as long as you don't get confused.

# Step 2: Descriptive statistics ------------------------------------------
#
# TASK: Calculate some descriptive statistics for the variables
# of interest (eval, beauty and age).
# HINT: We have used summarise() to calculate the mean, sd, min and max values.

descriptives <- beauty %>% 
  summarise(mean_age = mean(age, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE),
            min_age = min(age, na.rm = TRUE),
            max_age = max(age, na.rm = TRUE),
            mean_eval = mean(eval, na.rm = TRUE),
            sd_eval = sd(eval, na.rm = TRUE),
            min_eval = min(eval, na.rm = TRUE),
            max_eval = max(eval, na.rm = TRUE),
            mean_beauty = mean(beauty, na.rm = TRUE),
            sd_beauty = sd(beauty, na.rm = TRUE),
            min_beauty = min(beauty, na.rm = TRUE),
            max_beauty = max(beauty, na.rm = TRUE))
            
# TASK: Visualise the distributions of the variables of interest in histograms
# HINT: Use ggplot() and geom_historgram() in the following way:
# ggplot(data, aes(variable)) + geom_histogram()

ggplot(data = beauty, aes(beauty)) + geom_histogram()

ggplot(data = beauty, aes(eval)) + geom_histogram()

ggplot(data = beauty, aes(age)) + geom_histogram()


# Step 3: Center and standardise ------------------------------------------
# As mentioned before, it will make it easier to interpret regression models with multiple
# predictors if we center and standardise our predictors. Before we go any further, we'll
# do that.

# TASK: Center and standardise the predictor variables.
# HINT: Centering involves subtracting the mean; standardising involves dividing by the 
# standard deviation.

beauty_z <- beauty %>%
  mutate(age_z = (age - mean(age, na.rm = TRUE)) / sd(age),
         beauty_z = (beauty - mean(beauty, na.rm = TRUE)) / sd(beauty))

# Step 4: Scatterplots ----------------------------------------------------
#
# TASK: Visualise the relationships between the variables of interest in scatterplots. To remind
# yourself of what centering and standardising does, do this for both the raw data and the
# centered and standardised data.
# HINT: Create six different scatterplots using ggplot() with geom_point() and geom_smooth()

ggplot(beauty, aes(x = beauty, y = age)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw()

ggplot(beauty, aes(x = beauty, y = eval)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw()

ggplot(beauty, aes(x = age, y = eval)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw()

ggplot(beauty_z, aes(x = beauty_z, y = age_z)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw()

ggplot(beauty_z, aes(x = beauty_z, y = eval)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw()

ggplot(beauty_z, aes(x = age_z, y = eval)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_bw()

# QUESTION: Write an interpretation of the above plots in plain English.
# POSSIBLE ANSWER:
# - A moderate negative association seems present between beauty score and age: with increasing age, beauty score decreases.
# - A moderate positive association seems present between beauty score and teaching evaluation: professors with higher beauty scores also receive higher teaching evaluations.
# - Not much of a association seems present between age and teaching evaluation (the line is pretty horizontal).
#
# QUESTION: What is the difference between the scatterplots plotting the raw data and the ones
# plotting the centered and standardised data?
# ANSWER: The units of the x-axis have changed from years (for age) and scores (for beauty)
# to standard units, with zero in the middle.

# Nothing wrong with making the scatterplots individually, but R does have functions
# that let you make several in one go in what is called a 'scatterplot matrix'. To
# be able to do that, we first have to create an object that only includes the variables
# of interest. Then we need to tell R this is a 'data frame' (a specific type of
# data table). Finally, we use the pairs() function to create the matrix of
# scatterplots. The code below does these things:

# TASK add comments to each line of code to summarise what that line does
beauty_matrix <- beauty_z %>%     # use 'beauty' and assigne outcome to new object 'beauty_matrix'
  select(age_z, beauty_z, eval) %>%     # only keep the variables age, beauty and eval
  as.data.frame() # make sure to tell R that it is a data frame

pairs(beauty_matrix) # create a matrix of scatterplots

# Although handy to get a quick overview, the scatterplots made using ggplot() are often
# clearer.

# It is useful to have a quick look at the bivariate correlations between the variables
# of interest, before you run a regression model. We can easily generate a correlation
# matrix for these variables:
intercor_results <- correlate(x = beauty_matrix, # our data
                              test = TRUE, # compute p-values
                              corr.method = "pearson", # run a spearman test 
                              p.adjust.method = "bonferroni") # use the bonferroni correction
intercor_results

# Look at the output in the console. It creates three tables, one with correlation coefficients,
# one with p-values for these coefficients and one with sample sizes.

# Step 5: The regression model  ------------------------------------------------------------------

# 5.1 A model without an interaction term
# TASK: Construct a regression model without an interaction term
# HINT: Call your output "mod", and use the centered and standardised data (beauty_z),
# for the regression model. Use the following formula, lm(y ~ x1 + x2, data); go back to
# the research question for your outcome and predictor variables.

mod <- lm(eval ~ age_z + beauty_z, data = beauty_z)

# TASK: Call and save the summary of your model as "mod_summary"; then have a look at it.

mod_summary <- summary(mod)
mod_summary

# QUESTION: Is the overall model significant?
# ANSWER: Yes, F(2, 460) = 8.53, p = .0002

# QUESTION: Are the predictors significant? What does this mean?
# ANSWER: The beauty score significantly predicts teaching evaluation score, but age
# does not. Professors with higher beauty scores, received better teaching evaluations.

# 5.2:
# TASK: Now create a model that includes an interaction term for the two predictors.
# HINT: Call your output "mod_int", and use the centered and standardised data (beauty_z),
# for the regression model. Use the following formula, lm(y ~ x1 + x2 + x1:x2, data); go back to
# the research question for your outcome and predictor variables.

mod_int <- lm(eval ~ age_z + beauty_z + age_z:beauty_z, data = beauty_z)
mod_int_summary <- summary(mod_int)
mod_int_summary

# QUESTION: Is the overall model significant?
# ANSWER: Yes, F(3, 459) = 9.32, p = 5.451e-06

# QUESTION: Have a good look at the coefficients. Can you interpret each one of them
# in turn and then formulate an overall interpretation?
# HINT: Remember that after centering and standardising, the meaning of 0 has changed for
# both predictor variables.
# ANSWER:
# The intercept is predicted teaching evaluation score for a professor with average age
# and average beauty score.
# The slope of 'age' is positive; this means that for higher age, teaching evaluation scores
# were better. However the coefficient is not significant, therefore has little predictive
# power.
# The slope of 'beauty' is positive; this means that with higher beauty score,
# professors receive higher teaching evaluations. This predictor is significant.
# The slope for the interaction is also positive. This can be read as follows:
# When age and beauty both increase, teaching evaluation score also increases. The
# interaction is significant.

# Interpretation of coefficients in a multiple regression can be facilitated by 'added variable' plots
avPlots(mod_int)

# Creating a scatterplot with our outcome variable on the y-axis and the significant
# predictor on the x-axis and then plotting our third variable (age) using different colours
# gives some information. Do you see how high age scores (light blue + 2 SD) seem
# to be more frequent in the bottom left corner?

ggplot(data = beauty_z, aes(x = beauty_z, y = eval, colour = age_z)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, colour = 'black') +
  theme_bw() +
  labs(x = "Beauty score", y = "Teaching evaluation score")


# But it might be more useful to plot different regression lines for different
# values of age. We can do this be transforming age into a categorical variable
# for plotting purposes. The code below creates three categories, based on eye-balling
# the histogram for age:
# - youngest (40 and younger)
# - average (between 41 and 53)
# - oldest (54 and older).

oldest <- beauty_z %>%
  filter(age >= 54)

average <- beauty_z %>%
  filter(age > 40) %>%
  filter(age < 54)

youngest <- beauty_z %>%
  filter(age <= 40)

ggplot() +
  geom_point(data = oldest, aes(x = beauty_z, y = eval), colour = 'blue') +
  geom_smooth(data = oldest, aes(x = beauty_z, y = eval), method = "lm", se = TRUE, colour = 'blue') +
  geom_point(data = average, aes(x = beauty_z, y = eval), colour = 'black') +
  geom_smooth(data = average, aes(x = beauty_z, y = eval), method = "lm", se = TRUE, colour = 'black') +
  geom_point(data = youngest, aes(x = beauty_z, y = eval), colour = 'green') +
  geom_smooth(data = youngest, aes(x = beauty_z, y = eval), method = "lm", se = TRUE, colour = 'green') +
  theme_bw() +
  labs(x = "Beauty score", y = "Teaching evaluation score")

# The line the oldest participants seems much steeper than for the other two groups,
# suggesting that the interaction between age and beauty is mostly driven by older
# participants who have received more extreme beauty scores. 

# Step 6: Checking assumptions  ------------------------------------------------------------------

# Now that we've fit a model, let's check whether it meets the assumptions of
# linearity, normality and homoscedasticity. 
#
# Linearity:
# Unlike when we did simple regression we can’t use crPlots() to test for linearity when there is an interaction,
# but we know from looking at the grouped scatterplot that this assumption has been met.
#
# Normality:
# Normally we would test for normality with a qqplot and a Shapiro-Wilk test. However, because this dataset
# is so large, the Shapiro-Wilk is not appropriate (if you try to run the test it will produce a warning
# telling you that the sample size must be between 3 and 5000). This is because
# with extremely large sample sizes the Shapiro-Wilk test will find that any deviation from normality is
# significant. Therefore we should judge normality based upon the qqplot.
#
# TASK: Create a qq-plot to check the residuals are normally distributed
qqPlot(mod_int$residuals)       

# QUESTION: What do you conclude from the qq-plot?
# ANSWER: The residuals are normally distributed.

# Homoscedasticity:
# Here we have the same problem as with testing for normality: with such a large sample the ncvTest() will produce
# a significant result for any deviation from homoscedasticity. So we need to rely on plots again.
# To check for homoscedasticity we can use plot() from Base R that will produce a bunch of helpful plots
# (more information here: https://www.r-bloggers.com/2016/01/how-to-detect-heteroscedasticity-and-rectify-it/).

par(mfrow=c(2,2))                 # 4 charts in 1 panel
plot(mod_int)                     # this may take a few seconds to run

# QUESTION: What do you conclude from the residuals vs leverage plot?
# ANSWER: The residuals vs leverage plot shows a flat red line so, whilst it isn’t 
# perfect, we can assume that with regression is still an appropriate analysis.

# Finally, lets check for multicollinearity using the vif() function. Essentially, this function estimates
# how much the variance of a coefficient is “inflated” because of linear dependence with other predictors, 
# i.e., that a predictor isn’t actually adding any unique variance to the model, it’s just really strongly
# related to other predictors. Thankfully, vif is not affected by large samples like the other tests.
# There are various rules of thumb, but most converge on a VIF of above 2 - 2.5 for any one predictor being problematic.

vif(mod_int)                      # Check for multi-collinearity

## QUESTION: Do any of the predictors show evidence of multicollinearity?
## ANSWER: No

# Step 9: Power and effect size -----------------------------------------------------------------

# TASK: Use the pwr.f2.test() function to calculate the minimal effect size
# for this dataset at 99% power level and alpha level of .05. Also calculate the observed effect size
# and compare the two.
# HINT: Look back on the previous lab scripts for this code.

pwr.f2.test(u = 3, v = 459, f2 = NULL, sig.level = .05, power = .99)

# Calculate observed effect size
f2 <- mod_int_summary$adj.r.squared/(1 - mod_int_summary$adj.r.squared)             
f2

# QUESTION: What is the minimally detectable effect size for the study to three decimal places?
# ANSWER: 0.051

# QUESTION: What is the observed effect size for the study to three decimal places?
# ANSWER: 0.054

# QUESTION: Is the study sufficiently powered?
# ANSWER: Yes, the observed effect size is larger than the minimally detectable effect size.

# TASK: Use the pwr.f2.test() function again, but now use the observed effect size as f2 and set v to NULL.
# In this way we can estimate the number of participants needed for a sufficiently powered study.

pwr.f2.test(u = 3, v = NULL, f2 = 0.05399955, sig.level = .05, power = .99)

# QUESTION: How many participants would you need for a sufficiently powered study given
# the observed effect size?
# ANSWER: 439. You arrive at this number by looking at the value for v (the degrees of
# freedom of the error), which is defined as n-p in which n is the sample size and p is
# number of predictors (in which you should include the intercept).
# So, 435 df + 3 predictors + 1 intercept = 439

# Step 10: Write up -----------------------------------------------------------------
# ANSWER: 
# The results of the regression indicated that the model significantly predicted teaching
# evaluation scores (F(3, 459) = 9.316, p < .001, Adjusted R2 = 0.05, f2 = .03), accounting for 5%
# of the variance.
# A professor's beauty score was a significant positive predictor of teaching evaluation score (beta = 0.12, p < .001,
# This effect was moderated by a significant positive interaction between beauty score
# and age (beta = 0.08, p < .001), suggesting that when age and beauty score both increased, teaching evaluation
# score also increased.
