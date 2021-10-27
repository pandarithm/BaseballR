## ----setup, include=FALSE------------------------------------------------
source("global_config.R")

## ------------------------------------------------------------------------
library(tidyverse)
hof <- read_csv("../data/hofbatting.csv")

## ------------------------------------------------------------------------
hof <- hof %>%
  mutate(MidCareer = (From + To) / 2,
         Era = cut(MidCareer,
                   breaks = c(1800, 1900, 1919, 1941, 
                              1960, 1976, 1993, 2050),
                   labels = c("19th Century", "Dead Ball",
                              "Lively Ball", "Integration", 
                              "Expansion", "Free Agency", 
                              "Long Ball")))

## ------------------------------------------------------------------------
hof_eras <- summarize(group_by(hof, Era), N = n())
hof_eras

## ----hof_barplot, fig.cap="Bar graph of the era of the Hall of Fame nonpitchers."----
ggplot(hof, aes(x = Era)) + geom_bar()

## ----barplot2, fig.cap="Era of the non-pitching Hall of Famers."---------
ggplot(hof, aes(Era)) + 
  geom_bar() +
  xlab("Baseball Era") +
  ylab("Frequency") +
  ggtitle("Era of the Nonpitching Hall of Famers")

## ----linepie, fig.cap="Dot plot of era of the Hall of Fame non-pitchers."----
ggplot(hof_eras, aes(Era, N)) + 
  geom_point() +
  xlab("Baseball Era") +
  ylab("Frequency") +
  ggtitle("Era of the Nonpitching Hall of Famers") +
  coord_flip()

## ----eval=FALSE----------------------------------------------------------
## ggplot(hof, aes(Era)) +
##   geom_bar() +
##   xlab("Baseball Era") +
##   ylab("Frequency") +
##   ggtitle("Era of the Nonpitching Hall of Famers")
## ggsave("bargraph.png")

## ------------------------------------------------------------------------
pdf("graphs.pdf")
ggplot(hof, aes(Era)) + geom_bar() 
ggplot(hof_eras, aes(Era, N)) + geom_point()
dev.off()

## ----stripchart, fig.cap="One-dimensional scatterplot of the OPS values of the Hall of Fame players."----
ggplot(hof, aes(x = OPS, y = 1)) +
  geom_jitter(height = 0.6) + ylim(-1, 3) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_fixed(ratio = 0.03)

## ----histogram, fig.cap="Histogram of the OPS values of the Hall of Fame players."----
ggplot(hof, aes(x = OPS)) + 
   geom_histogram()

## ----other_plot, fig.cap="Histogram of the OPS values of the Hall of Fame players using different bins and different color and fill options."----
ggplot(hof, aes(x = OPS)) + 
  geom_histogram(breaks = seq(0.4, 1.2, by = 0.1),
                 color = crcblue, fill = "white")

## ----tplot1, fig.cap="Scatterplot of the OPS and Midcareer values of the Hall of Fame players."----
ggplot(hof, aes(MidCareer, OPS)) + 
  geom_point() +
  geom_smooth()

## ----tplot2, fig.cap="Scatterplot of the OPS and Midcareer values of the Hall of Fame players with points identified."----
library(ggrepel)
ggplot(hof, aes(MidCareer, OPS)) + 
  geom_point() +
  geom_smooth() +
  geom_text_repel(data = filter(hof, OPS > 1.05 | OPS < .5),
             aes(MidCareer, OPS, label = Player))

## ----makeplot1, fig.cap="Scatterplot of the OPS and SLG values of the Hall of Fame players."----
(p <- ggplot(hof, aes(OBP, SLG)) + geom_point())

## ----makeplot2, fig.cap="Scatterplot of the OPS and SLG values of the Hall of Fame players with the outlier removed."----
(p <- p + 
   xlim(0.25, 0.50) + ylim(0.28, 0.75) +
   xlab("On Base Percentage") +
   ylab("Slugging Percentage"))

## ----eval=FALSE----------------------------------------------------------
## p <- p +
##    scale_x_continuous("On Base Percentage",
##                       limits = c(0.25, 0.50)) +
##    scale_y_continuous("Slugging Percentage",
##                       limits = c(0.28, 0.75))

## ----makeplot3, warning=FALSE, fig.cap="Scatterplot of the OPS and SLG values of the Hall of Fame players with reference lines."----
(p <- p + geom_abline(slope = -1,
                      intercept = seq(0.7, 1, by = 0.1)))


## ----makeplot4, warning=FALSE, fig.cap="Scatterplot of the OPS and SLG values of the Hall of Fame players with reference lines and labels."----
p + annotate("text", 
             x = rep(.27, 4) , y = c(.42, .52, .62, .72),
             label = paste("OPS = ",
                     c(0.7, 0.8, 0.9, 1.0)))

## ----eval=FALSE----------------------------------------------------------
## ops_labels <- tibble(
##   OBP = rep(0.3, 4),
##   SLG = seq(0.4, 0.7, by = 0.1),
##   label = paste("OPS =", OBP + SLG)
## )
## p + geom_text(data = ops_labels, hjust = "right",
##               aes(label = label))

## ------------------------------------------------------------------------
hof <- mutate(hof, hr_rate = HR / AB)

## ----stripchart2, fig.cap="One-dimensional scatterplots of HR Rates by era."----
ggplot(hof, aes(hr_rate, Era)) + 
  geom_jitter(height = 0.1)

## ----boxplot, fig.cap="Parallel boxplots of HR Rates by era."------------
ggplot(hof, aes(Era, hr_rate)) + 
  geom_boxplot() + coord_flip()

## ------------------------------------------------------------------------
library(Lahman)

## ------------------------------------------------------------------------
get_birthyear <- function(Name) {
  Names <- unlist(strsplit(Name, " "))
  Master %>%
    filter(nameFirst == Names[1],
           nameLast == Names[2])  %>%
    mutate(birthyear = ifelse(birthMonth >= 7,
                              birthYear + 1, birthYear),
           Player = paste(nameFirst, nameLast)) %>%
    select(playerID, Player, birthyear)
}

## ------------------------------------------------------------------------
PlayerInfo <- bind_rows(get_birthyear("Babe Ruth"),
                        get_birthyear("Hank Aaron"),
                        get_birthyear("Barry Bonds"),
                        get_birthyear("Alex Rodriguez")
)

## ------------------------------------------------------------------------
Batting %>% 
  inner_join(PlayerInfo, by = "playerID") %>%
  mutate(Age = yearID - birthyear) %>%
  select(Player, Age, HR) %>%
  group_by(Player) %>%
  mutate(CHR = cumsum(HR)) -> HRdata

## ----homerun1, fig.cap="Cumulative home run counts against age for four ballplayers."----
ggplot(HRdata, aes(x = Age, y = CHR, linetype = Player)) +
  geom_line()

## ----warning=FALSE-------------------------------------------------------
fields <- read_csv("../data/fields.csv")
data1998 <- read_csv("../data/all1998.csv", 
                     col_names = pull(fields, Header))

## ------------------------------------------------------------------------
sosa_id <- Master %>%
  filter(nameFirst == "Sammy", nameLast == "Sosa") %>%
  pull(retroID)
mac_id <- Master %>%
  filter(nameFirst == "Mark", nameLast == "McGwire") %>%
  pull(retroID)

## ------------------------------------------------------------------------
hr_race <- data1998 %>%
  filter(BAT_ID %in% c(sosa_id, mac_id))

## ------------------------------------------------------------------------
library(lubridate)
cum_hr <- function(d) {
  d %>% 
    mutate(Date = ymd(str_sub(GAME_ID, 4, 11))) %>%
    arrange(Date) %>%
    mutate(HR = ifelse(EVENT_CD == 23, 1, 0),
           cumHR = cumsum(HR)) %>%
    select(Date, cumHR)
}

## ------------------------------------------------------------------------
hr_ytd <- hr_race %>%
  split(pull(., BAT_ID)) %>%
  map_df(cum_hr, .id = "BAT_ID") %>%
  inner_join(Master, by = c("BAT_ID" = "retroID"))

## ----homerun2, fig.cap="Graph of the 1998 home run race between Sammy Sosa and Mark McGwire."----
ggplot(hr_ytd, aes(Date, cumHR, linetype = nameLast)) + 
  geom_line() +
  geom_hline(yintercept = 62, color = crcblue) +
  annotate("text", ymd("1998-04-15"), 65, 
           label = "62", color = crcblue) +
  ylab("Home Runs in the Season")

## ----eval = FALSE--------------------------------------------------------
## hofpitching <- read_csv("hofpitching.csv")

## ----eval = FALSE--------------------------------------------------------
## hofpitching <- hofpitching %>%
##   mutate(BF.group = cut(BF,
##                         c(0, 10000, 15000, 20000, 30000),
##          labels = c("Less than 10000", "(10000, 15000)",
##                  "(15000, 20000)", "more than 20000")))

## ----eval = FALSE--------------------------------------------------------
## hofpitching <- hofpitching %>%
##   mutate(WAR.Season = WAR / Yrs)

## ----eval = FALSE--------------------------------------------------------
## hofpitching <- hofpitching %>%
##   mutate(MidYear = (From + To) / 2)
## hofpitching.recent <- hofpitching %>%
##   filter(MidYear >= 1960)

## ----eval = FALSE--------------------------------------------------------
## mac.data <- filter(mac.data, BAT_EVENT_FL == TRUE)
## sosa.data <- filter(sosa.data, BAT_EVENT_FL == TRUE)

## ----eval = FALSE--------------------------------------------------------
## mac.data <- mutate(mac.data, PA = 1:nrow(.))
## sosa.data <- mutate(sosa.data, PA = 1:nrow(.))

## ----eval = FALSE--------------------------------------------------------
## mac.HR.PA <- mac.data %>%
##   filter(EVENT_CD == 23) %>%
##   pull(PA)
## sosa.HR.PA <- sosa.data %>%
##   filter(EVENT_CD == 23) %>%
##   pull(PA)

## ----eval = FALSE--------------------------------------------------------
## mac.spacings <- diff(c(0, mac.HR.PA))
## sosa.spacings <- diff(c(0, sosa.HR.PA))	

