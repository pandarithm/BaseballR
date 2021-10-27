## ----setup, include=FALSE------------------------------------------------
source("global_config.R")

## ----career_hr, echo=FALSE, fig.cap="Career home runs by age for the top four home run hitters in baseball history."----
library(tidyverse)
library(Lahman)
Batting %>%
  filter(playerID %in% c("ruthba01", "bondsba01", "aaronha01", "rodrial01")) %>%
  left_join(Master, by = "playerID") %>%
  group_by(playerID) %>%
  mutate(Age = yearID - birthYear, 
         Player = paste(nameFirst, nameLast),
         cum_HR = cumsum(HR)) %>%
  ggplot(aes(x = Age, y = cum_HR)) +
  geom_line(aes(linetype = Player)) + 
  scale_y_continuous("Career Home Runs")

## ----eval=FALSE----------------------------------------------------------
## install.packages("Lahman")

## ------------------------------------------------------------------------
Master %>%
  head(1)

## ----echo=FALSE, results='asis'------------------------------------------
library(xtable)
Master %>%
  head(1) %>%
  t() %>%
  as.data.frame() %>%
  tibble::rownames_to_column() %>%
  rename(`field name` = rowname, value = `1`) %>%
  xtable::xtable(align = "lrl", label = "tab:Master",
                 caption = "First row of the \\cmd{Master.csv} file.") %>%
  print(include.rownames = FALSE, caption.placement = "top")

## ----echo=FALSE----------------------------------------------------------
Batting %>%
  filter(playerID == "brocklo01", yearID == 1964) %>%
  head(2) %>%
  as_tibble()

## ----echo=FALSE, results='asis'------------------------------------------
library(xtable)
Batting %>%
  filter(playerID == "ruthba01") %>%
  xtable::xtable(label = "tab:batting",
                 caption = "Batting statistics for Babe Ruth, taken from the \\cmd{Batting} table.") %>%
  print(include.rownames = FALSE, caption.placement = "top",
        floating.environment = "sidewaystable", size = "\\small\\addtolength{\\tabcolsep}{-3pt}")

## ----echo=FALSE, results='asis'------------------------------------------
library(xtable)
Pitching %>%
  filter(playerID == "ruthba01") %>%
  select(-SH, -SF, -GIDP) %>%
  xtable::xtable(label = "tab:pitching",
                 caption = "Pitching statistics for Babe Ruth, taken from the \\cmd{Pitching} table. A few extra columns not reported here are available.") %>%
  print(include.rownames = FALSE, caption.placement = "top",
        floating.environment = "sidewaystable", size = "\\small\\addtolength{\\tabcolsep}{-4pt}")

## ----echo=FALSE----------------------------------------------------------
Fielding %>%
  filter(playerID == "ruthba01", POS == "OF") %>%
  mutate(RF = (PO + A) / G) %>%
  pull(RF)

## ----echo=FALSE, results='asis'------------------------------------------
library(xtable)
Fielding %>%
  filter(playerID == "ruthba01") %>%
  select(-PB, -WP, -SB, -CS, -ZR) %>%
  xtable::xtable(label = "tab:fielding",
                 caption = "Fielding statistics for Babe Ruth, taken from the \\cmd{Fielding} table. Columns featuring statistics relevant only to catchers are not reported.") %>%
  print(include.rownames = FALSE, caption.placement = "top",
        size = "\\small\\addtolength{\\tabcolsep}{-4pt}")

## ----echo=FALSE----------------------------------------------------------
Teams %>%
  filter(yearID == 1927, teamID == "NYA")

## ----stanton2017, warning=FALSE, echo=FALSE, fig.cap="Pitch type and location for Giancarlo Stanton's 59 home runs of the 2017 season."----
statcast2017 <- read_csv("data/statcast2017.csv")
stanton_hr <- statcast2017 %>% 
  filter(player_name == "Giancarlo Stanton", events == "home_run") %>%
  mutate(is_fb = pitch_type %in% c("FF", "FT", "SI", "FC"))

plate_width <- 17 + 2 * (9/pi)
k_zone_plot <- ggplot(NULL, aes(x = plate_x, y = plate_z)) + 
  geom_rect(xmin = -(plate_width/2)/12, 
            xmax = (plate_width/2)/12, 
            ymin = 1.5, 
            ymax = 3.6, color = crcblue, fill = "lightgray", 
            linetype = 2, alpha = 0.01) + 
  coord_equal() + 
  scale_x_continuous("Horizontal location (ft.)", 
                     limits = c(-1.5, 1.5)) + 
  scale_y_continuous("Vertical location (ft.)", 
                     limits = c(1, 4))

k_zone_plot %+% stanton_hr +
  aes(color = is_fb) + 
  geom_point() + 
  scale_color_manual("Type", values = c(crcblue, "gray60"),
                     labels = c("off-speed", "fastball")
                     )

## ----echo=FALSE----------------------------------------------------------
Pitching %>%
  filter(playerID == "gibsobo01", yearID == 1968)

## ----echo=FALSE----------------------------------------------------------
load_gamelog <- function(season) {
  glheaders <- read.csv("data/game_log_header.csv")
  remote <- paste0("http://www.retrosheet.org/gamelogs/gl", 
                   season, ".zip")
  local <- paste0("gl", season, ".zip")
  download.file(url = remote, destfile = local)
  unzip(local)
  local_txt <- gsub(".zip", ".txt", local) %>%
    toupper()
  gamelog <- read_csv(local_txt, 
                      col_names = names(glheaders),
                      na = character())
  file.remove(local)
  file.remove(local_txt)
  return(gamelog)
}
gl <- load_gamelog(1964)
gl %>%
  filter(Date == 19640621, VisitingTeam == "PHI",
         HomeTeamGameNumber == 67) %>%
  select(1:38) %>%
  print.data.frame()

## ----retro,include=FALSE-------------------------------------------------
library(retro)
db <- src_mysql_cnf("retrosheet")
retro <- etl("retro", db = db, dir = "~/dumps/retro/")

## ----retro_populate, eval=FALSE, include=FALSE---------------------------
## retro %>%
##   etl_update(season = 1992)

## ----triple_play, warning=FALSE, echo=FALSE------------------------------
retro %>%
  tbl("events") %>%
  filter(GAME_ID == 'PIT199209200', INN_CT == 6, BAT_HOME_ID == 1) %>%
  select(BAT_HOME_ID, AWAY_SCORE_CT, HOME_SCORE_CT, BAT_ID, 
         PIT_ID, PITCH_SEQ_TX, EVENT_CD, EVENT_TX, BASE1_RUN_ID, BASE2_RUN_ID) %>%
  collect() %>%
  print.data.frame()

## ----eval=FALSE----------------------------------------------------------
## start_speed end_speed pfx_x pfx_z     px    pz sz_bot sz_top
##          73      66.3 -0.64 -7.58 -0.047 2.475    1.5   3.35
## 
## start_speed end_speed pfx_x pfx_z    px    pz sz_bot sz_top
##        81.2      75.4 -4.99 -7.67 -1.99 2.963    1.5   3.43

