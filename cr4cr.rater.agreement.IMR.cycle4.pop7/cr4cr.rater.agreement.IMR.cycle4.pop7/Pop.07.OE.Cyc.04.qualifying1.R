# scores exported from the rater assignment on BASS
# responses exported from the Report module on BASS


# how to git from R-studio: https://happygitwithr.com/rstudio-git-github.html
#install.packages("writexl") -- install all packages below if needed
library(tidyverse)
library(readxl)
library(janitor)
library(writexl)

# set data and output folders in your directry
data_folder <- "data"
out_folder <- "output"

# set paths for reading csv data files
read_path<-paste(data_folder, "0.IMR_resps.xlsx", sep="/")
read_path2<-paste(data_folder, "0.Qual1.IMR.scores.csv", sep="/")

# get tab names from the resp.xlsx
excel_sheets(path = read_path)
resps <- read_excel(path = read_path, sheet = "resps_cleaned", 
                  na = c("", "-")) # wide format
scores <- read_csv(read_path2) # long format

# get column names from resps
colnmaes(resps)

# create two dfs, one for pop7 and another for cycle4
resp.pop7 <- resps %>%
  select(sid = Respondent, 
         aid = Assignment, starts_with("Pop.07ab"))
resp.cyc4 <- resps %>%
  select(sid = Respondent, 
         aid = Assignment,starts_with("Cycle.04ab"))

# make resps to a long format
resps <- resps %>% 
  pivot_longer(
  cols = starts_with(c("Pop", "Cycle")),
  names_to = "item",
  values_to = "resp",
  values_drop_na = FALSE)
head(resps)

#shorten col names
head(scores)
colnames(scores)<- c("sid", "aid", "activity", "item", "NS", "AR","JJ")

# filter only Pop.07 and Cycle scores 
# then create four variables comparing scores
scores <- scores %>% 
  filter(str_detect(item, "Pop.07|Cycle"))%>% 
  mutate(all.agr = ifelse(AR==NS & NS==JJ, "yes", "no"),
         ARvNS = ifelse(AR==NS, "yes", "no"),
         ARvJJ = ifelse(AR==JJ, "yes", "no"),
         NSvJJ= ifelse(NS==JJ, "yes", "no")) %>% 
  select(-activity) %>% 
  filter(!is.na(all.agr))

# separate scores for two items
scores.pop7 <- scores %>% 
  filter(str_detect(item, "Pop.07")) %>% 
  select(-item)

scores.cyc4 <- scores %>% 
  filter(str_detect(item, "Cycle.04")) %>% 
  select(-item)

# freq + % tables:: all3 + 3 pairs; keep just yes %
f1 <- df %>% tabyl(item, all.agr) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()

f2 <- df %>% tabyl(item, ARvJJ) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()
arjj <- f2 %>% select(c(item, ARvJJ = yes))

f3 <- df %>% tabyl(item, NSvJJ) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()
nsjj <- f3 %>%  select(c(item, NSvJJ = yes))

f4 <- df %>% tabyl(item, ARvNS) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns()
arns <- f4 %>% select(c(item, ARvNS = yes))

# merge 4 tables into one
sum.agr <- f1 %>% 
  select(c(item, all3 = yes))
sum.agr <- left_join(sum.agr, arjj) 
sum.agr <- left_join(sum.agr, nsjj)
sum.agr <- left_join(sum.agr, arns)
head(sum.agr)

# merge Round 2 scores with responses
df.cyc4 <- left_join(scores.cyc4, resp.cyc4)
df.pop7 <- left_join(scores.pop7, resp.pop7)

# set ouput paths
out_path<-paste(out_folder, "01.14.21.Pop.07.Cycle.04.Round1.xlsx", sep="/")

# save summary
sheets <- list("summary" = sum.agr, "cyc.04" = df.cyc4, "pop.07"= df.pop7)
write_xlsx(sheets, out_path)

# merge scores (with those who haven't scored) with responses
score.resps <- left_join(resps, scores)

# write out score.resps as excel to be used for Round 3 assignment
out_path2<-paste(out_folder, "12.17.20.Pop.04.ScoreResps.xlsx", sep="/")
write_xlsx(score.resps, out_path2)
