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


#The SAT was recently changed to reduce the number of multiple choice options from 5 to 4 and also to eliminate the penalty for guessing.
#In this two-part question, you'll explore how that affected the expected values for the test.

#2.a. Suppose that the number of multiple choice options is 4 and that there is no penalty for guessing - that is, an incorrect question gives a score of 0.
#What is the expected value of the score when guessing on this new test?
point_correct <- 1
correct_guess <- 1/4
point_incorrect <- 0
incorrect_guess <- 1-correct_guess
mu <- num_questions*(correct_guess*point_correct + incorrect_guess*point_incorrect) #expected value #time by n, now calculating for whole test
mu

#2.b. Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) representing a range of student skills.
#What is the lowest p such that the probability of scoring over 35 exceeds 80%?
p <- seq(0.25, 0.95, 0.05)
for (i in 1:length(p)) {
  mu <- num_questions*(p[i]*point_correct + (1-p[i])*point_incorrect)
  sigma <- sqrt(num_questions)*abs(point_incorrect-point_correct)*sqrt(p[i]*(1-p[i]))
  if ((1-pnorm(35, mu, sigma)) > 0.8) {
    print(p[i])
    break
  }
}
#alternative method using sapply
p <- seq(0.25, 0.95, 0.05)
score <- sapply(p, function(v){
  mu <- num_questions*(v*point_correct + (1-v)*point_incorrect)
  sigma <- sqrt(num_questions)*abs(point_incorrect-point_correct)*sqrt(v*(1-v))
  1-pnorm(35, mu, sigma)
})
min(p[which(score > 0.8)])


#A casino offers a House Special bet on roulette, which is a bet on five pockets (00, 0, 1, 2, 3) out of 38 total pockets. 
#The bet pays out 6 to 1. In other words, a losing bet yields -$1 and a successful bet yields $6. 
#A gambler wants to know the chance of losing money if he places 500 bets on the roulette House Special.
#The following 7-part question asks you to do some calculations related to this scenario.

#3.a. What is the expected value of the payout for one bet?
money_win <- 6
money_lose <- -1
win <- 5/38
lose <- 1-win
mu <- money_win*win + money_lose*lose
mu

#3.b. What is the standard error of the payout for one bet?
sigma <- abs(money_lose-money_win)*sqrt(win*lose)
sigma

#3.c. What is the expected value of the average payout over 500 bets?
mu #average payout

#3.d. What is the standard error of the average payout over 500 bets?
num_games <- 500
sigma/sqrt(num_games)

#3.e. What is the expected value of the sum of 500 bets?
mu*num_games

#3.f. What is the standard error of the sum of 500 bets?
sigma*sqrt(num_games)

#3.g. Use pnorm() with the expected value of the sum and standard error of the sum to calculate the probability of losing money over 500 bets, Pr(x <= 0).
pnorm(0, mu*num_games, sigma*sqrt(num_games))
