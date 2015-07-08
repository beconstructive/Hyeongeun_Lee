
#####################################
### Assignment #2 - Hyeongeun Lee ###
#####################################



## Q1. (5 pts) The data below are the number of points scored in 30 games by the Portland Trailblazers.
q1_data <- c(90,95,89,71,73,96,87,95,107,89,96,80,97,95,102,97,93,101,82,83,74,91,83,98,95,111,99,120,93,84)

# a. Estimate the sample mean score. What does the quantity estimate?
mean(q1_data)

# b. Is the estimate in part(a) likely to equal the pop. parameter? Why or why not?
# No. Because it is a sample of the pop., the sample mean doesn't equate to the true pop. mean.

# c. Calculate the standard error for your sample estimate.
sqrt(var(q1_data)/length(q1_data))

# d. What does the quantity in part(c) measure?
# The spread of the sampling dist. of the sample mean.

# e. Calculate a 95% confidence interval for the population mean.
q1_samplemean - 2 * sqrt(var(q1_data)/length(q1_data)) # lower bound
q1_samplemean + 2 * sqrt(var(q1_data)/length(q1_data)) # upper bound

# f. Provide an interpretation for the interval you calculated in part (e).
# In 95% of the random samples from pop., the interval will include the true pop. mean.



## Q2. (5 pts) Using the following data, test the null hypothesis that male and females have the same mean cholesterol concentrations.
# Include descriptive statistics, hypothesis testing (e.g., t-test) and 95% confidence intervals. 

male = c(220.1, 218.6, 229.6, 228.8, 222.0, 224.1, 226.5)
female = c(223.4, 221.5, 230.2, 224.3, 223.8, 230.8)

par(mfrow=c(2,1))
hist(male); hist(female)

mean(male); sd(male)
mean(female); sd(female)

t.test(male,female, var.equal=F)

# The result of C.I. contains zero, so we can't reject the null.


## Q3. (5 pts) A clinical trail was carried out to test whether a new treatment has an effect on the rate of recovery of patients.
# The null hypothesis ¡°H0: the treatment has no effect¡± was rejected with a P-value of 0.04.
# The researchers used a significance level of 5%. State whether the following conclusions is correct. If not, explain why.

# a. The treatment has only a small effect. => False. P-value says nothing about the size of the effect.
# b. The treatment has some effect. => True. The null was rejected, so we can conclude there was some effect.
# c. The probability of committing a Type I error is 0.04. => False. Our alpha, significance level, is 0.05.
# d. The probability of committing a Type II error is 0.04. => False. It could be 0.95, since our alpha is 0.05.
# e. The null hypothesis would not have been rejected if the significance level was ¥á=0.01. => True. The p-value of the test is 0.04.



## Q4. (5 pts) The data below are volumes of red blood cells from two individuals.
# Test the hypothesis (using the Mann-Whitney test) that the red blood cells of person B are 1.5 times the volume of person A.
person_A <- c(248, 236, 269, 254, 249, 251, 260, 245, 239, 255)
person_B <- c(380, 391, 377, 392, 398, 374)
person_A_dash <- person_A* 1.5

par(mfrow=c(2,1))
hist(person_A_dash); hist(person_B)

# h0 : person A cells have vol. 1.5 multiplied
# ha : person A cells don't have vol. 1.5 multiplied

wilcox.test(person_A_dash, person_B)

# The result says the p-value of test is 0.1471. Therefore, we cannot reject the null hyphothesis.



## Q5. (5 pts) What is the difference between the standard error of mean and the standard deviation?
# Provide example data that illustrates their difference.

# The big difference is, the standard error is about estimator of parameter, and the standard deviation is about the sample distribution that is a measure of dispersion.