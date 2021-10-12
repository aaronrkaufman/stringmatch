setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(data.table)
library(tidyverse)

set.seed(12345)

## Load amicus data
load("orgs.RData")

## Load data from Crosson et al (2021)
crosson = read.csv("static_scores_withints321.csv") %>%
  mutate(name = tolower(orgname_new)) %>% select(-org_index) %>%
  mutate(realorg = is.na(as.numeric(orgname_new))) %>%
  filter(realorg == TRUE) %>% select(-orgname_new, -realorg)

## check exact matches
sum(crosson$name %in% out$name) # 866/2646

dat = merge(crosson, out, by ="name")

crosson2 = crosson[!crosson$name %in% out$name,]

## Load AFSM library
source("stringmatch.R")
source("get_features.R")
load("train.rda")
#load("m.rda")

train2 = train %>% mutate(A = amicus, B = bonica, y = match) %>%
  select(A,B,y)

## Do some fuzzy string matching
## Either run this line or load m directly
m = ranger::ranger(x = train %>% select(osa:soundex),
                   y = factor(train$match),
                   probability = TRUE)

## Start with 1:500 of string1 for computational tractability;
## Lower as necessary for your machine!
sm1 = stringmatch(string1 = crosson2$name[1:500], string2 = out$name,
                  feature.importances = FALSE)

## Check quality
matches = sm1[[1]]
matches = matches %>% arrange(desc(pred))

matches2 = matches %>% rename(amicus = Var1, bonica = Var2)
matches2 = matches2[1:1000,]
matches2 = matches2 %>% rename(match = pred)
matches2$match = 0

## Here is where you scan through the top matches and list which ones are correct
matches2$match[c(1,3,13,15,17,23,38,39,41,43,56,57,62,64,66,73,80,
                     82:83,97,99,105,147,162,182,199,209,219,238,247:249,
                     274,295,296,306,308:311,320:322,331,345,351,396,
                     475,524,589,608,642,660,662,663,665,669,670:674,
                     677,679,680,709,735:737,749:750,755,758,765,800,816,
                     908)] = 1

## Add those new matches to the training set
matches3 = matches2 %>%
  filter(match==1)%>%
  select(amicus, bonica) %>%
  inner_join(crosson2, by=c("amicus" = "name")) %>%
  select(-amicus) %>%
  inner_join(out, by=c("bonica" = "name")) %>%
  mutate(name = bonica) %>% select(-bonica)

dat2 = rbind(dat, matches3)

## Retrain the model and go again
train = rbind(train, matches2)

m = ranger::ranger(x = train %>% select(osa:soundex),
                   y = factor(train$match),
                   probability = TRUE)

sm2 = stringmatch(string1 = crosson2$name[501:1000], string2 = out$name,
                  feature.importances = FALSE)

matches = sm2[[1]]
matches = matches %>% arrange(desc(pred))

matches2 = matches %>% rename(amicus = Var1, bonica = Var2)
matches2 = matches2[1:1000,]
matches2 = matches2 %>% rename(match = pred)
matches2$match = 0
matches2$match[c(1:13,16,19,23,25,35,36,146)] = 1

matches3 = matches2 %>%
  filter(match==1)%>%
  select(amicus, bonica) %>%
  inner_join(crosson2, by=c("amicus" = "name")) %>%
  select(-amicus) %>%
  inner_join(out, by=c("bonica" = "name")) %>%
  mutate(name = bonica) %>% select(-bonica)

dat2 = rbind(dat2, matches3)

## One more iteration if needed
train = rbind(train, matches2)

m = ranger::ranger(x = train %>% select(osa:soundex),
                   y = factor(train$match),
                   probability = TRUE)

sm3 = stringmatch(string1 = crosson2$name[1001:nrow(crosson)], string2 = out$name,
                  feature.importances = FALSE)

matches = sm3[[1]]
matches = matches %>% arrange(desc(pred))

matches2 = matches %>% rename(amicus = Var1, bonica = Var2)
matches2 = matches2[1:1000,]
matches2 = matches2 %>% rename(match = pred)
matches2$match = 0

matches2$match[c(1:61,63,65,74,90,96,99,103,108,115,125,141)] = 1

matches3 = matches2 %>%
  filter(match==1)%>%
  select(amicus, bonica) %>%
  inner_join(crosson2, by=c("amicus" = "name")) %>%
  select(-amicus) %>%
  inner_join(out, by=c("bonica" = "name")) %>%
  mutate(name = bonica) %>% select(-bonica)

dat2 = rbind(dat2, matches3)


cor(dat2$mean_score_new, dat2$crossover, use = "complete.obs") # 0.61
