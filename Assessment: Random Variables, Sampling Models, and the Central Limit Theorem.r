#The SAT is a standardized college admissions test used in the United States. The following two multi-part questions will ask you some questions about SAT testing.
#This is a 6-part question asking you to determine some probabilities of what happens when a student guessed for all of their answers on the SAT. 
#Use the information below to inform your answers for the following questions.
#An old version of the SAT college entrance exam had a -0.25 point penalty for every incorrect answer and awarded 1 point for a correct answer. 
#The quantitative test consisted of 44 multiple-choice questions each with 5 answer choices. Suppose a student chooses answers by guessing for all questions on the test.

#1.a. What is the probability of guessing correctly for one question?
correct_guess <- 1/5
correct_guess

#1.b. What is the expected value of points for guessing on one question?
point_incorrect <- -0.25
incorrect_guess <- 1-correct_guess
mu <- correct_guess*point_correct + incorrect_guess*point_incorrect #expected value
mu

#1.c. What is the expected score of guessing on all 44 questions?
num_questions <- 44
mu*num_questions

#1.d. What is the standard error of guessing on all 44 questions?
sigma <- sqrt(num_questions)*abs(point_incorrect-point_correct)*sqrt(correct_guess*incorrect_guess) #standard error
sigma

#1.e. Use the Central Limit Theorem to determine the probability that a guessing student scores 8 points or higher on the test.
1-pnorm(8, mu, sigma)

#Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing on the test.
#(IMPORTANT! If you use R 3.6 or later, you will need to use the command set.seed(x, sample.kind = "Rounding") instead of set.seed(x). 
#Your R version will be printed at the top of the Console window when you start RStudio.)
#1.f. What is the probability that a guessing student scores 8 points or higher?
set.seed(21, sample.kind = "Rounding")
B <- 10000
questions <- 44
simulation <- replicate(B, {
  sim <- sample(c(point_correct, point_incorrect), questions, replace=TRUE, c(correct_guess, incorrect_guess))
  sum(sim)
})
mean(simulation>=8)


