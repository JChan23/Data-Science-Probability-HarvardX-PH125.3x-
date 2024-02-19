#In the 200m dash finals in the Olympics, 8 runners compete for 3 medals (order matters). In the 2012 Olympics, 3 of the 8 runners were from Jamaica and the other 5 were from different countries. The three medals were all won by Jamaica (Usain Bolt, Yohan Blake, and Warren Weir).
#Use the information above to help you answer the following four questions.

#1.a. How many different ways can the 3 medals be distributed across 8 runners?
library(gtools)
medals <- permutations(8,3)
nrow(medals)

#1.b. How many different ways can the three medals be distributed among the 3 runners from Jamaica?
jamaica < permutations(3,3)
nrow(jamaica)

#1.c. What is the probability that all 3 medals are won by Jamaica?
nrow(jamaica)/nrow(medals)

#1.d. Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race: runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
#For each iteration of the Monte Carlo simulation, within a replicate() loop, select 3 runners representing the 3 medalists and check whether they are all from Jamaica. Repeat this simulation 10,000 times. Set the seed to 1 before running the loop.
#Calculate the probability that all the runners are from Jamaica.
set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B <- 10000
jamaica <- replicate(B, {
  results <- sample(runners, 3)
  all(results == "Jamaica")
})
mean(jamaica)


#Use the information below to answer the following five questions.
#A restaurant manager wants to advertise that his lunch special offers enough choices to eat different meals every day of the year. He doesn't think his current special actually allows that number of choices, but wants to change his special if needed to allow at least 365 choices.
#A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. He currently offers a choice of 1 entree from a list of 6 options, a choice of 2 different sides from a list of 6 options, and a choice of 1 drink from a list of 2 options.

#2.a. How many meal combinations are possible with the current menu?
library(gtools)
6 * nrow(combinations(6,2)) * 2

#2.b. The manager has one additional drink he could add to the special.
#How many combinations are possible if he expands his original special to 3 drink options?
6 * nrow(combinations(6,2)) * 3

#2.c. The manager decides to add the third drink but needs to expand the number of options. The manager would prefer not to change his menu further and wants to know if he can meet his goal by letting customers choose more sides.
#How many meal combinations are there if customers can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?
6 * nrow(combinations(6,3)) * 3

#2.d. The manager is concerned that customers may not want 3 sides with their meal. He is willing to increase the number of entree choices instead, but if he adds too many expensive options it could eat into profits. He wants to know how many entree choices he would have to offer in order to meet his goal.
#Write a function that takes a number of entree choices and returns the number of meal combinations possible given that number of entree options, 3 drink choices, and a selection of 2 sides from 6 options.
#Use sapply() to apply the function to entree option counts ranging from 1 to 12.
#What is the minimum number of entree options required in order to generate more than 365 combinations?
entree <- function(x){
  x * nrow(combinations(6,2)) * 3
}
combos <- sapply(1:12, entree)
for (i in 1:12) {
  if (combos[i] > 365) {
    print(i)
    break
  }
}

#2.e. The manager isn't sure he can afford to put that many entree choices on the lunch menu and thinks it would be cheaper for him to expand the number of sides. He wants to know how many sides he would have to offer to meet his goal of at least 365 combinations.
#Write a function that takes a number of side choices and returns the number of meal combinations possible given 6 entree choices, 3 drink choices, and a selection of 2 sides from the specified number of side choices.
#Use sapply() to apply the function to side counts ranging from 2 to 12.
#What is the minimum number of side options required in order to generate more than 365 combinations?
sides <- function(x){
  6 * nrow(combinations(x, 2)) * 3
}
combos <- sapply(2:12, sides)
for (i in 1:11) {
  if (combos[i] > 365) {
    print(i+1)
    break
  }
}


#Case-control studies help determine whether certain exposures are associated with outcomes such as developing cancer. 
#The built-in dataset esoph contains data from a case-control study in France comparing people with esophageal cancer (cases, counted in ncases) to people without esophageal cancer (controls, counted in ncontrols) that are carefully matched on a variety of demographic and medical characteristics. 
#The study compares alcohol intake in grams per day (alcgp) and tobacco intake in grams per day (tobgp) across cases and controls grouped by age range (agegp).
#The dataset is available in base R and can be called with the variable name esoph: head(esoph)
#You will be using this dataset to answer the following four multi-part questions.
#Each row contains one group of the experiment. Each group has a different combination of age, alcohol consumption, and tobacco consumption. The number of cancer cases and number of controls (individuals without cancer) are reported for each group.

#3.a. How many groups are in the study?
nrow(esoph)

#3.b. How many cases are there?
all_cases <- sum(esoph$ncases)
all_cases

#3.c. How many controls are there?
all_controls <- sum(esoph$ncontrols)
all_controls


#4.a. For cases, what is the probability of being in the highest alcohol group?
library(tidyverse)
esoph %>% filter(alcgp == "120+") %>%
  summarize(sum_cases=sum(ncases))/all_cases

#4.b. For cases, what is the probability of being in the highest tobacco group?
esoph %>% filter(tobgp == "30+") %>%
  summarize(sum_cases=sum(ncases))/all_cases

#4.c. For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?
esoph %>% filter(alcgp == "120+" & tobgp =="30+") %>%
  summarize(sum_cases = sum(ncases))/all_cases

#4.d. For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?
esoph %>% filter(alcgp == "120+" | tobgp =="30+") %>%
  summarize(sum_cases = sum(ncases))/all_cases
