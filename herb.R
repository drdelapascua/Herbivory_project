## Cleaning data Ch. 4 Herbivory
## Elena Suglia

# Libraries ----
library(wesanderson)
library(RColorBrewer)
library(cowplot)
library(reshape2)
#library(scales)
#library(vistime)
library(directlabels)
library(export)
library(gridExtra)
#library(arm)
library(lme4)
#library(lattice)
#library(vioplot)
#library(lme4)
#library(arm)
#library(bbmle) # Ben Bolker's library of mle functions
#library(MASS)
library(MuMIn)
#library(nlme)
library(tidyverse)
library(stringr)
library(lubridate)
library(dplyr)

# Cleaning ----

# starting from wide format ----

d = read.csv("herb.csv") # read in file
separate(variable, c("var", "col")) %>%
  #gather(key = "date_var", value = "date_val", starts_with("date"), -site, -site_plot, -mf, -ind, -uniqueID, -treat, na.rm = FALSE) %>%
  arrange(uniqueID) #%>%
#spread(col, value)

# example of what I want to do
dw <- read.table(header=T, text='
 sbj f1.avg f1.sd f2.avg f2.sd  blabla
   A   10    6     50     10      bA
   B   12    5     70     11      bB
   C   20    7     20     8       bC
   D   22    8     22     9       bD
 ')

dw %>% 
  gather(v, value, f1.avg:f2.sd) %>% 
  separate(v, c("var", "col")) %>% 
  arrange(sbj) %>% 
  spread(col, value)

#gather(key = "date", value = "date_values", starts_with("date"), -site, -site_plot, -mf, -ind, -uniqueID, -treat) %>% # gather dates from wide to long format

#mutate(date_values = mdy(date_values)) %>% # tell r to recognize dates as dates

# creating a dates column

#d = d %>% pivot_longer(
#  cols = starts_with("date"), 
#  names_to = c("values", "dates"),
#  names_sep = "_"
#)

# d %>% variable[which(var, ".1$")]
# d %>% variable[str_detect(variable, ".1$")]
# d %>% filter(str_detect(variable, ".1$"))
# select(variable, ends_with(".1")) --> select is used on columns; filter on rows
#d %>% filter(variable, ends_with(".1"))

nrow = nrow(d)
weeks = c(1, 2, 3, 5, 7, 9, 10, 11, 12, 13)
nweeks = length(dates)
row = seq(1:nrow)

d[,"dates"] <- NA # make an empty column in which to place date values

for(i in 1:nweeks) { # loop over sampling times
  d$dates[i] = weeks[i]
  for(row in 1:nrow) {
    if (d$variable == "date.{i}") d$dates[row] = value[str_which(d$variable, ".{i}$")] # if variable = date.1, then assign a value to d$dates of that row equal to the value in column "value" that corresponds to d$variable ending in ".1" (e.g. flrs.1)
  }
}

#case_when(
#  str_detect(variable, ".1$") ~ value[variable == date.1]
#)


# example double for loop
r = c(1.5, 2.3, 2.6, 3.0) # vector of growth rate values looking over
nr = length(r) # how many growth rate values looking over

# initialize n trajectories as a matrix, with a row for each initial population size

n = matrix(NA, nrow = tf, ncol = nr) # creates a matrix with 100 rows (each row is for a different time step) and 4 columns (each column is for a different r value)
for(stepr in 1:nr) { # stepr starts at 1, then loops through as many times as nr, which is 4 because nr = length(r). Each time a loop is run, stepr takes on a new value, runs the for loop, and then takes on the next value. The name "stepr" is arbitrary, and is often denoted as "i".
  n[1,stepr] = r[stepr] # first loop through r's
  for(t in 1:(tf-1)) {
    n[t+1,stepr] = ricker(n[t,stepr], r[stepr], K)
  }
}

# starting from long format ----

d = read.csv("herb_long.csv") # read in file
treat_dates = read.csv("treat_dates.csv")
treat_dates = treat_dates %>%
  select(-X.1, -X, -ends_with("notes")) %>% # couldn't figure out why but csv kept reading in two extra columns with only NAs labeled X and X.1
  mutate(treat1_date = mdy(treat1_date)) %>%
  mutate(treat2_date = mdy(treat2_date)) %>%
  mutate(treat2_jday = yday(treat2_date)) %>%
  mutate(treat1_jday = yday(treat1_date))

# setwd("~/Box Sync/Graduate School/PhD Research/Ch. 4) Herbivory experiment/herbivory") # set working directory to R project (on GitHub here: https://github.com/esuglia/herbivory)

# site = LVTR, LV1, LV2, LV3
# plot = LVTR 1, 2, etc.

d <- d %>%
  separate(uniqueID, sep = " ", into = c("site_plot", "blegh", "mf", "ind"), remove = FALSE) %>% # deconcatenate uniqueID column into site, mf, individual - "blegh" is the column that will contain only the string "mf"
  mutate(ind = str_remove_all(ind, "#")) %>% # remove "#" from individual numbers in ind column
  select(-"blegh") %>%
  mutate(site = case_when(
    site_plot == "LVTR1" ~ "LVTR",
    site_plot == "LVTR2" ~ "LVTR",
    site_plot == "LVTR3" ~ "LVTR",
    site_plot == "LV1" ~ "LV1",
    site_plot == "LV2" ~ "LV2",
    site_plot == "LV3" ~ "LV3")) %>% # create separate columns for site and plot

  select(-starts_with("notes"), -starts_with("protocol"), -starts_with("height"), -starts_with("stem"), -starts_with("longest"), -starts_with("total"), -starts_with("VB"), -starts_with("secondary"), -starts_with("primary"), -starts_with("branch"), -ends_with(".4")) %>%

  full_join(treat_dates, "uniqueID") %>% # merge treatment date data
  
  gather(key = "variable", value = "value", starts_with("buds"), starts_with("fruits"), starts_with("flrs"), starts_with("phen"), -site, -site_plot, -mf, -ind, -uniqueID, -treat, na.rm = FALSE) %>% # changed data from wide to long format

mutate(date = mdy(date)) %>% 
mutate(jday = yday(date)) %>%

mutate(treat = str_replace_all(treat, "extra", "control"))

d$site_plot = factor(d$site_plot,levels=c("LVTR3", "LVTR2", "LVTR1", "LV1", "LV2", "LV3"))
d$site = factor(d$site,levels=c("LVTR", "LV1", "LV2", "LV3"))
d$treat = factor(d$treat, levels =c("control", "early", "late", "both"))




# plots ----

# libraries ----
library(wesanderson)
library(RColorBrewer)
library(cowplot)
library(reshape2)
#library(scales)
#library(vistime)
library(directlabels)
library(export)
library(gridExtra)
#library(lattice)
#library(vioplot)
library(tidyverse)
library(stringr)
library(lubridate)
library(dplyr)

# casting to wide ----

d1 = as_tibble(dcast(d, uniqueID + site + site_plot + ind + site_pop_mf + mf + date + jday + treat + treat1_date + treat2_date + treat1_jday + treat2_jday ~ variable, value.var = "value", fun.aggregate = max)) %>%
  mutate(lasttreat = max())

# phenology ~ treatment ----

relphenology = d1 %>% 
  drop_na(fruits) %>% # to do a summarize, can't have NAs in column
  drop_na(flrs) %>%
  group_by(site, site_plot, site_pop_mf, mf, treat, treat1_date, treat2_date, treat1_jday, treat2_jday, uniqueID) %>%
  summarise(
    # metrics for absolute flowering time (disregarding time of treatment):
   firstflr = ifelse(treat == "early", min(jday[min(which(jday > treat1_jday & is.na(flrs) == FALSE & flrs > 0))]),
                     ifelse (treat == "control", min(jday[is.na(flrs) == FALSE & flrs > 0]),
                             ifelse (treat == "late", min(jday[min(which(jday > treat2_jday & is.na(flrs) == FALSE & flrs > 0))]), 
                                    ifelse (treat == "both", min(min(jday[min(which(jday > treat2_jday & is.na(flrs) == FALSE & flrs > 0))])), NA))))
   )
# make a column for last treatment date for each plant

foo = d1 %>%
  drop_na(flrs) %>%
  group_by(uniqueID) %>%
  summarize(
    firstflr = ifelse(treat == "early", 1, 0)
  )


lastflr = jday[max(which(jday > treat1_jday & is.na(flrs) == FALSE & flrs > 0))]
),
    peakflr = median(jday[which(flrs == max(flrs))]),
    flrduration = lastflr - firstflr,
    fitness = max(fruits),
    
    # analogous metrics but for flowering times relative to treatments:
    firstflr = min(jday[is.na(flrs) == FALSE & flrs > 0])
    
  ) %>%
  mutate(firstflr = na_if(firstflr, -Inf)) %>%
  mutate(firstflr = na_if(firstflr, Inf)) %>%
  mutate(lastflr = na_if(lastflr, -Inf)) %>%
  mutate(lastflr = na_if(lastflr, Inf)) %>%
  mutate(peakflr = na_if(peakflr, -Inf)) %>%
  mutate(peakflr = na_if(peakflr, Inf)) %>%
  mutate(flrduration = na_if(flrduration, -Inf)) %>%
  mutate(flrduration = na_if(flrduration, Inf)) %>%
  mutate(fitness = na_if(fitness, -Inf)) %>%
  mutate(fitness = na_if(fitness, Inf)) %>%
  drop_na(site)

absphenology = d1 %>% 
  drop_na(fruits) %>% # to do a summarize, can't have NAs in column
  drop_na(flrs) %>%
  group_by(site, site_plot, site_pop_mf, mf, treat, uniqueID) %>%
  summarise(
    # metrics for absolute flowering time (disregarding time of treatment):
    firstflr = min(jday[is.na(flrs) == FALSE & flrs > 0]),
    lastflr = max(jday[is.na(flrs) == FALSE & flrs > 0]),
    peakflr = median(jday[which(flrs == max(flrs))]),
    flrduration = lastflr - firstflr,
    fitness = max(fruits),
    
    # analogous metrics but for flowering times relative to treatments:
    firstflr = min(jday[is.na(flrs) == FALSE & flrs > 0])
    
  ) %>%
  mutate(firstflr = na_if(firstflr, -Inf)) %>%
  mutate(firstflr = na_if(firstflr, Inf)) %>%
  mutate(lastflr = na_if(lastflr, -Inf)) %>%
  mutate(lastflr = na_if(lastflr, Inf)) %>%
  mutate(peakflr = na_if(peakflr, -Inf)) %>%
  mutate(peakflr = na_if(peakflr, Inf)) %>%
  mutate(flrduration = na_if(flrduration, -Inf)) %>%
  mutate(flrduration = na_if(flrduration, Inf)) %>%
  mutate(fitness = na_if(fitness, -Inf)) %>%
  mutate(fitness = na_if(fitness, Inf)) %>%
  drop_na(site)

phenology$fitness = as.numeric(phenology$fitness)

# peakflr ~ treatment
ggplot(data = phenology, mapping = aes(treat, peakflr)) +
  #facet_wrap(~treat) +
  geom_boxplot()
  #geom_histogram()
  #geom_point() +
  #geom_smooth(method = lm)

# lastflr ~ treatment
ggplot(data = phenology, mapping = aes(treat, lastflr)) +
  #facet_wrap(~treat) +
  geom_boxplot()

# flrduration ~ treatment
ggplot(data = phenology, mapping = aes(treat, flrduration)) +
  #facet_wrap(~treat) +
  geom_boxplot()

# ANOVA & Tukey HSD Test ----
# one-way ANOVA: http://www.sthda.com/english/wiki/one-way-anova-test-in-r#what-is-one-way-anova-test

peak.res.aov = aov(peakflr ~ treat, data = phenology)
summary(peak.res.aov) # treatments are significantly different from each other
TukeyHSD(peak.res.aov) # adjusted p values are significant for late vs control, both vs control, late vs early, and 0.06 for both vs early & for both vs late (early-control not sig diff at 0.35)

ggplot(data = phenology, mapping = aes(flrduration, fitness, color = treat, group = treat)) +
  facet_wrap(~site) +
  geom_smooth(method = lm)

# fitness ~ treatment
ggplot(data = phenology, mapping = aes(treat, fitness)) +
  geom_boxplot()

fitness.res.aov = aov(fitness ~ treat, data = phenology)
summary(fitness.res.aov) # treatments are significantly different from each other
TukeyHSD(fitness.res.aov) # adjusted p value is significant only for both vs control (0.0036)













