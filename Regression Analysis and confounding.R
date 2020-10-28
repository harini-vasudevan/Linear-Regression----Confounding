#regression analysis and baseball
#Game attendance in baseball varies partly as a function of how well a team is playing.
library(tidyverse)
library(broom)
library(Lahman)
#The Teams data frame contains an attendance column. This is the total attendance for the season.
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)
# find regression line predicting attendance from R and take slope
Teams_small %>% 
  mutate(R_per_game = R/G) %>% 
  lm(avg_attendance ~ R_per_game, data = .) %>% 
  .$coef %>%
  .[2]
Teams_small %>% 
  mutate(HR_per_game = HR/G) %>% 
  lm(avg_attendance ~ HR_per_game, data = .) %>% 
  .$coef %>%
  .[2]
Teams_small %>% 
  lm(avg_attendance ~ W, data = .) %>%
  .$coef %>%
  .[2]
Teams_small %>% 
  lm(avg_attendance ~ W, data = .) %>% 
  .$coef %>%
  .[1]
Teams_small %>% 
  lm(avg_attendance ~ yearID, data = .) %>% 
  .$coef %>%
  .[2]
#Game wins, runs per game and home runs per game are positively correlated with attendance. 
#Are wins and runs per game or wins and home runs per game correlated?
#2 correlation coefficient for wins and runs per game
cor(Teams_small$W, Teams_small$R/Teams_small$G)
cor(Teams_small$W, Teams_small$HR/Teams_small$G)
#3Stratify Teams_small by wins
dat <- Teams_small %>%
  mutate(W_strata = round(W/10)) %>%
  filter(W_strata >= 5 & W_strata <= 10)

sum(dat$W_strata == 8)
# calculate slope of regression line after stratifying by R per game
dat %>%  
  group_by(W_strata) %>%
  summarize(slope = cor(R/G, avg_attendance)*sd(avg_attendance)/sd(R/G))
# calculate slope of regression line after stratifying by HR per game
dat %>%  
  group_by(W_strata) %>%
  summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))
Looking at the data, we can see that runs per game are positively correlated with average attendance, that home runs per game have the strongest effect on attendance when teams don't win many games, and that teams with fewer wins have a larger average attendance with more home runs per game.

#We also see that runs per game have a stronger effect when teams win few, not many, games, 
#and that home runs per game are in fact positively correlated with attendance in all win strata.
#4 multivariate regression determining the effects of runs per game, home runs per game, wins, and year on average attendancefit <- Teams_small %>% 

fit <- Teams_small %>%  mutate(R_per_game = R/G,
         HR_per_game = HR/G) %>%
  lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data = .)
tidy(fit) %>%
  filter(term == "R_per_game") %>%
  pull(estimate)

tidy(fit) %>%
  filter(term == "HR_per_game") %>%
  pull(estimate)
tidy(fit) %>%
  filter(term == "W") %>%
  pull(estimate)
  
#5
predict(fit, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 2002))
predict(fit, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 1960))
#6
newdata <- Teams %>%
  filter(yearID == 2002) %>%
  mutate(avg_attendance = attendance/G,
         R_per_game = R/G,
         HR_per_game = HR/G)
preds <- predict(fit, newdata)
#the correlation between the predicted attendance and actual attendance
cor(preds, newdata$avg_attendance)

#confounding
#we examine the data from a 2014 PNAS paper that analyzed success rates from funding agencies
#in the Netherlands and concluded: "our results reveal gender bias favoring male applicants
#over female applicants in the prioritization of their "quality of researcher" 
#(but not "quality of proposal") evaluations and success rates, as well as in the language 
#used in instructional and evaluation materials."

#A response was published a few months later titled No evidence that gender contributes to 
#personal research funding success in The Netherlands:
#A reaction to Van der Lee and Ellemers , which concluded:
#However, the overall gender effect borders on statistical significance, despite the large sample.
#Moreover, their conclusion could be a prime example of Simpson's paradox; 
#if a higher percentage of women apply for grants in more competitive scientific disciplines 
#(i.e., with low application success rates for both men and women), 
#then an analysis across all disciplines could incorrectly show "evidence" of gender inequality. 
library(dslabs)
library(tidyverse)
library(broom)
data("research_funding_rates")
research_funding_rates

# compute totals that were successful or not successful
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum))%>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)
totals

# compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))

# construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(men)

two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(women)

#chi-squared test on the two-by-two table to determine whether 
#the difference in the two success rates is significant

two_by_two %>% select(-awarded) %>% chisq.test() %>% tidy() %>% pull(p.value)

#this "could be a prime example of Simpson's paradox; 
#if a higher percentage of women apply for grants in more competitive scientific disciplines,
#then an analysis across all disciplines could incorrectly show evidence of gender inequality.
dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat




