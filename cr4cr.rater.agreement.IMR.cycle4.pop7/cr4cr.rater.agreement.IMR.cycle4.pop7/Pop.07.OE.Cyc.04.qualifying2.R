# scores exported from the rater assignment on BASS
# responses exported from the Report module on BASS
# restart Rstudio Command+Shift+F10

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
read_path2<-paste(data_folder, "0.Qual2.IMR.scores.csv", sep="/")
read_path3<-paste(data_folder, "0.Qual2.Assignments.csv", sep="/")


# get tab names from the resp.xlsx
excel_sheets(path = read_path)
resps <- read_excel(path = read_path, sheet = "resps_cleaned", 
                  na = c("", "-")) # wide format
scores <- read_csv(read_path2) # long format

assigned.cyc4 <- read_csv(read_path3) %>% 
  filter(!is.na(Cycle.04ab_MCOE)) %>% 
  select(sid = `Student ID`)

assigned.pop7 <- read_csv(read_path3) %>%
  filter(!is.na(Pop.07ab_OE)) %>% 
  select(sid = `Student ID`)

# get column names from resps
colnames(resps)

# create two dfs, one for pop7 and another for cycle4
resp.pop7 <- resps %>%
  select(sid = Respondent, 
         aid = Assignment, starts_with("Pop.07ab"))
resp.cyc4 <- resps %>%
  select(sid = Respondent, 
         aid = Assignment,starts_with("Cycle.04ab"))

#shorten col names
colnames(scores)
colnames(scores)<- c("sid", "aid", "activity", "item", "NS", "AR","YT", "JJ")

# filter only Pop.07 and Cycle scores 
# then create four variables comparing scores
scores <- scores %>% 
  filter(str_detect(item, "Pop.07|Cycle"))%>%
  mutate(all.agr = ifelse(AR==NS & NS==JJ, "yes", "no"),
         ARvNS = ifelse(AR==NS, "yes", "no"),
         ARvJJ = ifelse(AR==JJ, "yes", "no"),
         NSvJJ= ifelse(NS==JJ, "yes", "no")) %>% 
  select(-c(activity, YT)) %>% 
  filter(!is.na(all.agr))

# separate scores for two items
scores.pop7 <- scores %>% 
  filter(str_detect(item, "Pop.07")) %>%
  filter(sid %in% assigned.pop7$sid) %>%
  select(-item)

scores.cyc4 <- scores %>% 
  filter(str_detect(item, "Cycle.04")) %>%
  filter(sid %in% assigned.cyc4$sid) %>% 
  select(-item)

# freq + % tables for all3 + 3 pairs for pop.7
#f1 <- scores.pop7 %>% tabyl(all.agr) %>% filter(all.agr == "yes")
#f2 <- scores.pop7 %>% tabyl(ARvJJ) %>% filter(ARvJJ == "yes")
#f3 <- scores.pop7 %>% tabyl(NSvJJ) %>% filter(NSvJJ == "yes")
#f4 <- scores.pop7 %>% tabyl(ARvNS) %>% filter(ARvNS == "yes")


#f5 <- scores.cyc4 %>% tabyl(all.agr) %>% filter(all.agr == "yes")
#f6 <- scores.cyc4 %>% tabyl(ARvJJ) %>% filter(ARvJJ == "yes")
#f7 <- scores.cyc4 %>% tabyl(NSvJJ) %>% filter(NSvJJ == "yes")
#f8 <- scores.cyc4 %>% tabyl(ARvNS) %>% filter(ARvNS == "yes")

f1 = subset(scores.pop7 %>% tabyl(all.agr) %>% filter(all.agr == "yes"), select = c(n,percent) )
f2 = subset(scores.pop7 %>% tabyl(ARvJJ) %>% filter(ARvJJ == "yes"), select = c(n,percent) )
f3 = subset(scores.pop7 %>% tabyl(NSvJJ) %>% filter(NSvJJ == "yes"), select = c(n,percent) )
f4 = subset(scores.pop7 %>% tabyl(ARvNS) %>% filter(ARvNS == "yes"), select = c(n,percent) )
f5 = subset(scores.cyc4 %>% tabyl(all.agr) %>% filter(all.agr == "yes"), select = c(n,percent) )
f6 = subset(scores.cyc4 %>% tabyl(ARvJJ) %>% filter(ARvJJ == "yes"), select = c(n,percent) )
f7 = subset(scores.cyc4 %>% tabyl(NSvJJ) %>% filter(NSvJJ == "yes"), select = c(n,percent) )
f8 = subset(scores.cyc4 %>% tabyl(ARvNS) %>% filter(ARvNS == "yes"), select = c(n,percent) )


table1<- data.frame(f1, f2, f3, f4, check.names=F)
table2<- data.frame(f5, f6, f7, f8, check.names=F)
table<-data.frame("Item name"=c('scores.pop7', 'scores.cyc4'))
table3<-cbind(table, rbind(table1, table2))
library(kableExtra)

kbl(table3, caption = "Summary of frency and percentage for all3 + 3 pairs for pop.7",
  col.names = c('Item.name', 'n', 'percent','n', 'percent','n', 'percent','n', 'percent'), booktabs = T) %>%
  kable_styling() %>%
  add_header_above(c( " "= 1 , "all.agr" = 2, "ARvJJ" = 2, "NSvJJ" = 2, "ARvNS" = 2)) 
# merge 4 tables into one
sum.agr.pop7 <- f1 %>%
  select(all3 = all.agr, count = n, agreement=percent) %>%
  filter(all3 == "yes")
sum.agr.pop7 <- left_join(sum.agr.pop7, arjj) 
sum.agr.pop7 <- left_join(sum.agr.pop7, nsjj)
sum.agr.pop7 <- left_join(sum.agr.pop7, arns)


# merge Round 2 scores with responses
df.cyc4 <- left_join(scores.cyc4, resp.cyc4)
df.pop7 <- left_join(scores.pop7, resp.pop7)

# set ouput paths
out_path<-paste(out_folder, "01.18.21.Pop.07.Cycle.04.Round2.xlsx", sep="/")

# save summary
sheets <- list("summary" = sum.agr, "cyc.04" = df.cyc4, "pop.07"= df.pop7)
write_xlsx(sheets, out_path)

# merge scores (with those who haven't scored) with responses
score.resps <- left_join(resps, scores)

# write out score.resps as excel to be used for Round 3 assignment
out_path2<-paste(out_folder, "12.17.20.Pop.04.ScoreResps.xlsx", sep="/")
write_xlsx(score.resps, out_path2)
