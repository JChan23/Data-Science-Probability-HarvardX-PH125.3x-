#The ACT is a standardized college admissions test used in the United States. The four multi-part questions in this assessment all involve simulating some ACT test scores and answering probability questions about them.
#For the three year period 2016-2018, ACT standardized test scores were approximately normally distributed with a mean of 20.9 and standard deviation of 5.7. (Real ACT scores are integers between 1 and 36, but we will ignore this detail and use continuous values instead.)

#First we'll simulate an ACT test score dataset and answer some questions about it.
#Set the seed to 16, then use rnorm() to generate a normal distribution of 10000 tests with a mean of 20.9 and standard deviation of 5.7. Save these values as act_scores. You'll be using this dataset throughout these four multi-part questions.
set.seed(16, sample.kind = "Rounding")
mean <- 20.9 
sd <- 5.7
act_scores <- rnorm(10000, mean, sd)

#1.a. What is the mean of act_scores?
mean(act_scores)

#1.b. What is the standard deviation of act_scores?
sd(act_scores)

#1.c. A perfect score is 36 or greater (the maximum reported score is 36).
#In act_scores, how many perfect scores are there out of 10,000 simulated tests?
sum(act_scores >= 36)

#1.d. In act_scores, what is the probability of an ACT score greater than 30?
mean(act_scores > 30)

#1.e. In act_scores, what is the probability of an ACT score less than or equal to 10?
mean(act_scores <= 10)


#2. Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the value of the probability density function over x given a mean of 20.9 and standard deviation of 5.7; save the result as f_x. Plot x against f_x.
x <- seq(1, 36)
f_x <- dnorm(x, mean, sd)
plot(x, f_x)


#In this 3-part question, you will convert raw ACT scores to Z-scores and answer some questions about them.
#Convert act_scores to Z-scores (code it using statistics formula)
z_scores <- (act_scores-mean(act_scores))/sd(act_scores)

#3.a. What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?
mean(z_scores > 2)

#3.b. What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
2*sd(act_scores) + mean(act_scores)

#3.c. A Z-score of 2 corresponds roughly to the 97.5th percentile.
#Use qnorm() to determine the 97.5th percentile of normally distributed data with the mean and standard deviation observed in act_scores.
#What is the 97.5th percentile of act_scores?
qnorm(0.975, mean, sd)


#In this 4-part question, you will write a function to create a CDF for ACT scores.
#Write a function that takes a value and produces the probability of an ACT score less than or equal to that value (the CDF). Apply this function to the range 1 to 36.
cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})

#4.a. What is the minimum integer score such that the probability of that score or lower is at least .95?
for (i in 1:36) {
  if (cdf[i] >= 0.95) {
    print(i)
    break
  }
}

#4.b. Use qnorm() to determine the expected 95th percentile, the value for which the probability of receiving that score or lower is 0.95, given a mean score of 20.9 and standard deviation of 5.7.
#What is the expected 95th percentile of ACT scores?
qnorm(0.95, mean, sd)

#4.c. As discussed in the Data Visualization course, we can use quantile() to determine sample quantiles from the data.
#Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st through 99th percentiles of the act_scores data. Save these as sample_quantiles.
#In what percentile is a score of 26?
round(pnorm(26, mean(act_scores), sd(act_scores))*100, digits=0) #asking for percentile, x100 and round up
