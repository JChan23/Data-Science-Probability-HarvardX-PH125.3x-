#Exercise 1. The Cavs and the Warriors
#Two teams, say the Cavs and the Warriors, are playing a seven game championship series. The first to win four games wins the series. The teams are equally good, so they each have a 50-50 chance of winning each game.
#If the Cavs lose the first game, what is the probability that they win the series?

n <- 6
outcomes <- c(0, 1)
l <- rep(list(outcomes), n)
possibilities <- expand.grid(l)
results <- rowSums(possibilities)>=4
mean(results)


#Exercise 2. The Cavs and the Warriors - Monte Carlo
#Confirm the results of the previous question with a Monte Carlo simulation to estimate the probability of the Cavs winning the series after losing the first game.

B <- 10000
set.seed(1)
results <- replicate(B, {
  simulated_games <- sample(c(0, 1), 6, replace = TRUE)
  sum(simulated_games)>=4 
})
mean(results)


#Exercise 3. A and B play a series - part 1
#Two teams, A and B, are playing a seven series game series. Team A is better than team B and has a p>0.5 chance of winning each game.

# Let's assign the variable 'p' as the vector of probabilities that team A will win.
p <- seq(0.5, 0.95, 0.025)

# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
    })
  mean(result)
}

# Apply the 'prob_win' function across the vector of probabilities that team A will win to determine the probability that team B will win. Call this object 'Pr'.
Pr <- sapply(p, prob_win)

# Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
plot(p, Pr)


#Exercise 4. A and B play a series - part 2
#Repeat the previous exercise, but now keep the probability that team A wins fixed at p <- 0.75 and compute the probability for different series lengths. For example, wins in best of 1 game, 3 games, 5 games, and so on through a series that lasts 25 games.

# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(N, p=0.75){
      B <- 10000
      result <- replicate(B, {
        b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
        sum(b_win)>=(N+1)/2
        })
      mean(result)
    }

# Assign the variable 'N' as the vector of series lengths. Use only odd numbers ranging from 1 to 25 games.
N <- seq(1, 25, by=2)

# Apply the 'prob_win' function across the vector of series lengths to determine the probability that team B will win. Call this object `Pr`.
Pr <- sapply(N, prob_win)

# Plot the number of games in the series 'N' on the x-axis and 'Pr' on the y-axis.
plot(N, Pr)
