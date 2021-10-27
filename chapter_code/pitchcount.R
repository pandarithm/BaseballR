## ----setup, include=FALSE------------------------------------------------
source("global_config.R")

## ----include=FALSE, eval=FALSE-------------------------------------------
## library(rvest)
## moose <- read_html("https://www.baseball-reference.com/players/split.fcgi?id=mussimi01&year=Career&t=p") %>%
##   html_node(css = "html.firefox.js.cors.cookies.localstorage.sessionstorage.canvas.no-touchevents.unicode.pointerevents.flexwrap.matchmedia.vibrate.desktop.is_live.is_modern.overthrow-enabled body.br div#wrap div#content div#all_count.table_wrapper.table_controls div.table_outer_container div#div_count.overthrow.table_container table#count.row_summable.sortable.stats_table.now_sortable")

## ----mussina-data--------------------------------------------------------
mussina <- expand.grid(balls = 0:3, strikes = 0:2) %>%
  mutate(value = c(100, 118, 157, 207, 72, 82, 
                  114, 171, 30, 38, 64, 122))
mussina

## ----mussina, fig.cap="Heat map of tOPS+ for Mike Mussina through each ball/strike count. Data from Baseball Reference website."----
count_plot <- mussina %>%
  ggplot(aes(x = strikes, y = balls, fill = value)) + 
  geom_tile() + 
  geom_text(aes(label = round(value, 3))) + 
  scale_fill_gradient2("tOPS+", low = "grey10", high = crcblue,
                       mid = "white", midpoint = 100)
count_plot

## ----mussina-table, include=FALSE, eval=FALSE----------------------------
## xtabs(value ~ balls + strikes, mussina)

## ----nchar---------------------------------------------------------------
nchar("BBSBFFFX")

## ----grep----------------------------------------------------------------
sequences <- c("BBX", "C11BBC1S", "1X")
grep("1", sequences)
grepl("1", sequences)

## ------------------------------------------------------------------------
grepl("11", sequences)

## ------------------------------------------------------------------------
gsub("1", "", sequences)

## ----warning=FALSE-------------------------------------------------------
headers <- read_csv("../data/fields.csv")
pbp2016 <- read_csv("../data/all2016.csv", 
                    col_names = pull(headers, Header),
                    na = character())

## ------------------------------------------------------------------------
pbp2016 <- pbp2016 %>%
  mutate(pseq = gsub("[.>123N+*]", "", PITCH_SEQ_TX))

## ------------------------------------------------------------------------
pbp2016 <- pbp2016 %>%
  mutate(c10 = grepl("^[BIPV]", pseq))

## ------------------------------------------------------------------------
pbp2016 <- pbp2016 %>%
  mutate(c01 = grepl("^[CFKLMOQRST]", pseq))

## ------------------------------------------------------------------------
pbp2016 %>%
  select(PITCH_SEQ_TX, c10, c01) %>%
  head(10)

## ----echo=FALSE----------------------------------------------------------
pbp16rc <- pbp2016 %>%
  mutate(RUNS = AWAY_SCORE_CT + HOME_SCORE_CT,
         HALF.INNING = paste(GAME_ID, INN_CT, BAT_HOME_ID),
         RUNS.SCORED = (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + 
           (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3),
         BASES = 
           paste(pmin(str_length(BASE1_RUN_ID), 1),
                 pmin(str_length(BASE2_RUN_ID), 1),
                 pmin(str_length(BASE3_RUN_ID), 1), sep = ""),
         STATE = paste(BASES, OUTS_CT),
         NRUNNER1 = as.numeric(RUN1_DEST_ID == 1 | BAT_DEST_ID == 1),
         NRUNNER2 = as.numeric(RUN1_DEST_ID == 2 | RUN2_DEST_ID == 2 | 
                                 BAT_DEST_ID == 2),
         NRUNNER3 = as.numeric(RUN1_DEST_ID == 3 | RUN2_DEST_ID == 3 |
                                 RUN3_DEST_ID == 3 | BAT_DEST_ID == 3),
         NOUTS = OUTS_CT + EVENT_OUTS_CT,
         NEW.BASES = paste(NRUNNER1, NRUNNER2, NRUNNER3,  sep = ""),
         NEW.STATE = paste(NEW.BASES, NOUTS))

half_innings <- pbp16rc %>%
  group_by(HALF.INNING) %>%
  summarize(Outs.Inning = sum(EVENT_OUTS_CT), 
            Runs.Inning = sum(RUNS.SCORED),
            Runs.Start = first(RUNS),
            MAX.RUNS = Runs.Inning + Runs.Start)

pbp16rc <- pbp16rc %>%
  inner_join(half_innings, by = "HALF.INNING") %>%
  mutate(RUNS.ROI = MAX.RUNS - RUNS)

RUNS <- pbp16rc %>%
  filter(Outs.Inning == 3) %>%
  group_by(STATE) %>%
  summarize(rv = mean(RUNS.ROI)) %>%
  mutate(Outs = substr(STATE, 5, 5)) %>%
  arrange(Outs)

pbp16rc <- pbp16rc %>%
  left_join(select(RUNS, -Outs), by = "STATE") %>%
  rename(Runs.State = rv) %>%
  left_join(select(RUNS, -Outs), by = c("NEW.STATE" = "STATE")) %>%
  rename(Runs.New.State = rv) %>%
  replace_na(list(Runs.New.State = 0))

b <- "[BIPV]"
s <- "[CFKLMOQRST]"
pbp16rc <- pbp16rc %>%
  mutate(RUNS.VALUE = Runs.New.State - Runs.State + RUNS.SCORED, 
         sequence = gsub("[.>123+*N]", "", PITCH_SEQ_TX),
         c00 = TRUE,
         c10 = grepl("^[BIPV]", sequence),
         c01 = grepl("^[CFKLMOQRST]", sequence),
         c20 = grepl("^[BIPV]{2}", sequence),
         c30 = grepl("^[BIPV]{3}", sequence), 
         c02 = grepl("^[CFKLMOQRST]{2}", sequence),
         c11 = grepl(paste0("^", s, b, 
                            "|", b, s), sequence),
         c21 = grepl(paste0("^", s, b, b, 
                            "|", b, s, b, 
                            "|", b, b, s), sequence), 
         c31 = grepl(paste0("^", s, b, b, b, 
                            "|", b, s, b, b, 
                            "|", b, b, s, b, 
                            "|", b, b, b, s), sequence),
         c12 = grepl(paste0("^", b, s, s, 
                            "|", s, b, s, 
                            "|", s, s, "[FR]*", b), sequence), 
         c22 = grepl(paste0("^", b, b, s, s, 
                            "|", b, s, b, s, 
                            "|", b, s, s, "[FR]*", b,
                            "|", s, b, b, s, 
                            "|", s, b, s, "[FR]*", b,
                            "|", s, s, "[FR]*", b, "[FR]*", b), sequence), 
         c32 = grepl(paste0("^",  s, "*", b, s, "*", b, s, "*", b), sequence)
             & grepl(paste0("^", b, "*", s, b, "*", s), sequence))

## ------------------------------------------------------------------------
pbp16rc %>%
  select(GAME_ID, EVENT_ID, RUNS.VALUE, c00, c10, c20, 
         c11, c01, c30, c21, c31, c02, c12, c22, c32) %>%
  head()

## ------------------------------------------------------------------------
pbp16rc %>%
  filter(c10 == 1 | c01 == 1) %>%
  group_by(c10, c01) %>%
  summarize(N = n(), mean_run_value = mean(RUNS.VALUE))

## ------------------------------------------------------------------------
pbp_counts <- pbp16rc %>%
  select(starts_with("c"), RUNS.VALUE)

## ------------------------------------------------------------------------
pbp_counts_tidy <- pbp_counts %>%
  gather(key = "count", value = "passed_thru", -RUNS.VALUE)
sample_n(pbp_counts_tidy, 6)

## ------------------------------------------------------------------------
run_value_by_count <- pbp_counts_tidy %>%
  filter(passed_thru == 1) %>%
  group_by(count) %>%
  summarize(N = n(), value = mean(RUNS.VALUE))

## ----RVcount, message=FALSE, fig.cap="Average change in expected runs for plate appearances passing through each ball/strike count. Values estimated on data from the 2016 season."----
run_value_by_count <- run_value_by_count %>%
  mutate(balls = str_sub(count, 2, 2), 
         strikes = str_sub(count, 3, 3))

count_plot %+% run_value_by_count + 
  scale_fill_gradient2("xRV", low = "grey10", high = crcblue,
                       mid = "white")

## ----count22-------------------------------------------------------------
count22 <- pbp16rc %>%
  filter(c22 == 1)
count22 %>%
  summarize(N = n(), mean_run_value = mean(RUNS.VALUE))

## ----count22-mutate------------------------------------------------------
count22 %>%
  mutate(after2 = case_when(
    c20 == 1 ~ "2-0", 
    c02 == 1 ~ "0-2", 
    c11 == 1 ~ "1-1",
    TRUE ~ "other")) %>%
  group_by(after2) %>%
  summarize(N = n(), mean_run_value = mean(RUNS.VALUE))

## ----include=FALSE-------------------------------------------------------
after02 <- pbp16rc %>%
  filter(c02 == 1, c22 == 0) %>%
  summarize(N = n(), 
            outs = sum(EVENT_OUTS_CT) / sum(BAT_EVENT_FL), 
            mean_run_value = mean(RUNS.VALUE))

## ----count11-------------------------------------------------------------
count11 <- pbp16rc %>%
  filter(c11 == 1)
count11 %>%
  mutate(after2 = ifelse(c10 == 1, "1-0", "0-1")) %>%
  group_by(after2) %>%
  summarize(N = n(), mean_run_value = mean(RUNS.VALUE))

## ----remove, include=FALSE-----------------------------------------------
rm(list = ls())

## ----load-balls----------------------------------------------------------
load("../data/balls_strikes_count.RData")
ls()

## ----global_options, include=FALSE---------------------------------------
source("global_config.R")

## ----cabreraScatter, warning=FALSE, fig.cap="Scatterplot of Miguel Cabrera's swinging tendency by location. Sample of 500 pitches. View from the catcher's perspective.", fig.height=4----
cabrera_sample <- cabrera %>%
  sample_n(500)
k_zone_plot <- ggplot(cabrera_sample, aes(x = px, y = pz)) + 
  geom_rect(xmin = -0.947, xmax = 0.947, ymin = 1.5, 
            ymax = 3.6, fill = "lightgray", alpha = 0.01) + 
  coord_equal() + 
  scale_x_continuous("Horizontal location (ft.)", 
                     limits = c(-2, 2)) + 
  scale_y_continuous("Vertical location (ft.)", 
                     limits = c(0, 5))   
k_zone_plot + 
  geom_point(aes(color = factor(swung))) +
  scale_color_manual("Swung", values = c("gray70", crcblue), 
                     labels = c("No", "Yes"))

## ------------------------------------------------------------------------
miggy_loess <- loess(swung ~ px + pz, data = cabrera, 
  control = loess.control(surface = "direct"))

## ------------------------------------------------------------------------
pred_area <- expand.grid(px = seq(-2, 2, by = 0.1), 
                         pz = seq(0, 6, by = 0.1))
pred_area_fit <- pred_area %>%
  mutate(fit = as.numeric(predict(miggy_loess, 
                                  newdata = .)))

## ------------------------------------------------------------------------
pred_area_fit %>%
  filter(px == 0 & pz == 2.5)  # down Broadway
pred_area_fit %>%
  filter(px == 0 & pz == 0)    # ball in the dirt
pred_area_fit %>%
  filter(px == 2 & pz == 2.5)  # way outside

## ----cabreraAll, warning=FALSE, fig.cap="Contour plot of Miguel Cabrera's swinging tendency by location, where the view is from the catcher's perspective.  The contour lines are labeled by the probability of swinging at the pitch."----
cabrera_plot <- k_zone_plot %+% 
  filter(pred_area_fit, fit >= 0, fit <= 1) +
  stat_contour(aes(z = fit, color = ..level..), 
               binwidth = 0.2) + 
  scale_color_gradient(low = "white", high = crcblue)

cabrera_plot <- cabrera_plot %>%
  directlabels::direct.label(method = "bottom.pieces")
cabrera_plot

## ------------------------------------------------------------------------
counts <- c("0-0", "0-2", "2-0")
count_dfs <- cabrera %>%
  mutate(count = paste(balls, strikes, sep = "-")) %>%
  filter(count %in% counts) %>%
  split(pull(., count))

## ------------------------------------------------------------------------
count_fits <- count_dfs %>%
  map(~loess(swung ~ px + pz, data = ., 
               control = loess.control(surface = "direct"))) %>%
  map(predict, newdata = pred_area) %>%
  map(~data.frame(fit = as.numeric(.))) %>%
  map_df(bind_cols, pred_area, .id = "count") %>%
  mutate(balls = str_sub(count, 1, 1), 
         strikes = str_sub(count, 3, 3))

## ----cabreraCntPnl, warning=FALSE, fig.cap="Contour plots of Miguel Cabrera's swinging tendency by selected ball/strike counts, viewed from the catcher's perspective.  The contour lines are labeled by the probability of swinging at the pitch."----
cabrera_plot %+% 
  filter(count_fits, fit > 0.1, fit < 0.7) + 
  facet_wrap( ~ count)

## ------------------------------------------------------------------------
verlander %>%
  group_by(pitch_type) %>%
  summarize(N = n()) %>%
  mutate(pct = N / nrow(verlander)) %>%
  arrange(desc(pct))

## ------------------------------------------------------------------------
verlander %>%
  group_by(batter_hand, pitch_type) %>%
  summarize(N = n()) %>%
  spread(key = batter_hand, value = N) %>%
  mutate(L_pct = L / sum(L), R_pct = R / sum(R))

## ------------------------------------------------------------------------
verlander %>%
  filter(batter_hand == "R") %>%
  group_by(balls, strikes, pitch_type) %>%
  summarize(N = n()) %>%
  spread(key = pitch_type, value = N, fill = 0) %>%
  mutate(num_pitches = CH + CU + FF + FT + SL) %>%
  mutate_if(is.numeric, funs(. / num_pitches)) %>%
  select(-num_pitches)

## ----eval=FALSE----------------------------------------------------------
## sample_n(umpires, 20)

## ----echo=FALSE, results='asis'------------------------------------------
library(xtable)
umpires %>%
  sample_n(20) %>%
  xtable::xtable(label = "tab:umpires",
                 caption = "A twenty row sample of the \\cmd{umpires} dataset.") %>%
  print(include.rownames = FALSE, caption.placement = "top",
        floating.environment = "sidewaystable")

## ------------------------------------------------------------------------
umpires_rhb <- umpires %>%
  filter(batter_hand == "R", 
         balls == 0 & strikes == 0 | 
           balls == 3 & strikes == 0 |
           balls == 0 & strikes == 2)

## ------------------------------------------------------------------------
ump_count_fits <- umpires_rhb %>%
  mutate(count = paste(balls, strikes, sep = "-")) %>%
  split(pull(., count)) %>%
  map(sample_n, 3000) %>%
  map(~loess(called_strike ~ px + pz, data = ., 
               control = loess.control(surface = "direct"))) %>%
  map(predict, newdata = pred_area) %>%
  map(~data.frame(fit = as.numeric(.))) %>%
  map_df(bind_cols, pred_area, .id = "count") %>%
  mutate(balls = str_sub(count, 1, 1), 
         strikes = str_sub(count, 3, 3))

## ----umpireCnt, warning=FALSE, fig.height=4, fig.cap="Umpires' 50/50 strike calling zone in different balls/strikes counts  viewed from the catcher's perspective."----
k_zone_plot %+% filter(ump_count_fits, fit < 0.6 & fit > 0.4) +
  geom_contour(aes(z = fit, color = count, linetype = count), 
               binwidth = 0.1) +
  scale_color_manual(values = crc_3)

