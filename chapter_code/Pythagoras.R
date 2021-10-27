## ----setup, include=FALSE------------------------------------------------
source("global_config.R")

## ----message=FALSE-------------------------------------------------------
library(tidyverse)
library(Lahman)
tail(Teams, 3)

## ------------------------------------------------------------------------
my_teams <- Teams %>%
  filter(yearID > 2000) %>%
  select(teamID, yearID, lgID, G, W, L, R, RA)
my_teams %>%
  tail()

## ------------------------------------------------------------------------
my_teams <- my_teams %>%
  mutate(RD = R - RA, Wpct = W / (W + L))

## ----rundiff-------------------------------------------------------------
run_diff <- ggplot(my_teams, aes(x = RD, y = Wpct)) + 
  geom_point() + 
  scale_x_continuous("Run differential") + 
  scale_y_continuous("Winning percentage")

## ----lm------------------------------------------------------------------
linfit <- lm(Wpct ~ RD, data = my_teams)
linfit

## ----rundiff_wpct, fig.cap="Scatterplot of team run differential against team winning percentage for major league teams from 2001 to 2016.  A best-fitting line is overlaid on top of the scatterplot."----
run_diff + 
  geom_smooth(method = "lm", se = FALSE, color = crcblue)

## ----include=FALSE-------------------------------------------------------
slope <- coef(linfit)["RD"]

## ----broom, warning=FALSE------------------------------------------------
library(broom)
my_teams_aug <- augment(linfit, data = my_teams)

## ----linearResiduals, fig.cap="Residuals versus run differential for the fitted linear model.  Four teams with large residuals are labeled."----
base_plot <- ggplot(my_teams_aug, aes(x = RD, y = .resid)) +
  geom_point(alpha = 0.3) + 
  geom_hline(yintercept = 0, linetype = 3) + 
  xlab("Run differential") + ylab("Residual")

highlight_teams <- my_teams_aug %>%
  arrange(desc(abs(.resid))) %>%
  head(4)

library(ggrepel)
base_plot +
  geom_point(data = highlight_teams, color = crcblue) + 
  geom_text_repel(data = highlight_teams, color = crcblue,
                  aes(label = paste(teamID, yearID)))

## ----resid-mean----------------------------------------------------------
resid_summary <- my_teams_aug %>%
  summarize(N = n(), avg = mean(.resid), 
            RMSE = sqrt(mean(.resid^2)))
resid_summary
rmse <- resid_summary %>%
  pull(RMSE)

## ------------------------------------------------------------------------
my_teams_aug %>%
  summarize(N = n(), 
            within_one = sum(abs(.resid) < rmse),
            within_two = sum(abs(.resid) < 2 * rmse)) %>%
  mutate(within_one_pct = within_one / N, 
         within_two_pct = within_two / N)

## ----pytWpct-------------------------------------------------------------
my_teams <- my_teams %>%
  mutate(Wpct_pyt = R ^ 2 / (R ^ 2 + RA ^ 2))

## ------------------------------------------------------------------------
my_teams <- my_teams %>%
  mutate(residuals_pyt = Wpct - Wpct_pyt) 
my_teams %>%
  summarize(rmse = sqrt(mean(residuals_pyt^2)))

## ----pyt-exponent--------------------------------------------------------
my_teams <- my_teams %>%
  mutate(logWratio = log(W / L), 
         logRratio = log(R / RA))

pytFit <- lm(logWratio ~ 0 + logRratio, data = my_teams)
pytFit

## ----read-game-log-------------------------------------------------------
glheaders <- read_csv("../data/game_log_header.csv")	
gl2011 <- read_csv("../data/gl2011.txt", 
                   col_names = names(glheaders),
                   na = character())

BOS2011 <- gl2011 %>%
  filter(HomeTeam == "BOS" | VisitingTeam == "BOS") %>%
  select(VisitingTeam, HomeTeam, 
         VisitorRunsScored, HomeRunsScore)
head(BOS2011)

## ------------------------------------------------------------------------
BOS2011 <- BOS2011 %>%
  mutate(ScoreDiff = ifelse(HomeTeam == "BOS", 
                            HomeRunsScore - VisitorRunsScored, 
                            VisitorRunsScored - HomeRunsScore), 
         W = ScoreDiff > 0)

## ----include=FALSE-------------------------------------------------------
library(skimr)
skim_with(integer = list(hist = NULL, complete = NULL))
bos_skim <- BOS2011 %>%
  group_by(W) %>%
  skim(ScoreDiff)

## ----error=TRUE----------------------------------------------------------
library(skimr)
BOS2011 %>%
  group_by(W) %>%
  skim(ScoreDiff)

## ------------------------------------------------------------------------
results <- gl2011 %>%
  select(VisitingTeam, HomeTeam, 
         VisitorRunsScored, HomeRunsScore) %>%
  mutate(winner = ifelse(HomeRunsScore > VisitorRunsScored, 
                         HomeTeam, VisitingTeam),
         diff = abs(VisitorRunsScored - HomeRunsScore))

## ----one-run-wins--------------------------------------------------------
one_run_wins <- results %>%
  filter(diff == 1) %>%
  group_by(winner) %>%
  summarize(one_run_w = n())

## ------------------------------------------------------------------------
teams2011 <- my_teams %>%
  filter(yearID == 2011) %>%
  mutate(teamID = ifelse(teamID == "LAA", "ANA", 
                         as.character(teamID))) %>%
  inner_join(one_run_wins, by = c("teamID" = "winner"))

## ----onerun, fig.cap="Scatterplot of number of one-run games won and Pythagorean residuals for major league teams in 2011."----

ggplot(data = teams2011, aes(x = one_run_w, y = residuals_pyt)) +
  geom_point() +
  geom_text_repel(aes(label = teamID)) + 
  xlab("One run wins") + ylab("Pythagorean residuals")

## ------------------------------------------------------------------------
top_closers <- Pitching %>%
  filter(GF > 50 & ERA < 2.5) %>%
  select(playerID, yearID, teamID)

## ----include=FALSE-------------------------------------------------------
skim_with(integer = list(hist = NULL, complete = NULL))
mean_resid <- my_teams %>%
  inner_join(top_closers) %>%
  pull(residuals_pyt) %>%
  summary()

## ------------------------------------------------------------------------
my_teams %>%
  inner_join(top_closers) %>%
  pull(residuals_pyt) %>%
  summary()

## ----derivative----------------------------------------------------------
D(expression(G * R ^ 2 / (R ^ 2 + RA ^ 2)), "R")

## ----ir-function---------------------------------------------------------
IR <- function(RS = 5, RA = 5) {
  (RS ^ 2 + RA ^ 2)^2 / (2 * RS * RA ^ 2)
}

## ------------------------------------------------------------------------
ir_table <- expand.grid(RS = seq(3, 6, .5), 
                        RA = seq(3, 6, .5))
head(ir_table)
tail(ir_table)

## ----xtabs---------------------------------------------------------------
ir_table %>%
  mutate(IRW = IR(RS, RA)) %>%
  spread(key = RA, value = IRW, sep = "=") %>%
  round(1)

