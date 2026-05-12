###############################################
##     More mates, less care?     ##
###############################################

#### Packages to load ----
library(lme4)
library(glmmTMB)
library(ggplot2)
library(car)
library(DHARMa)

### dataset clucthes ----
clutches <- read.delim("INSERT DIRECTION OF FILE -> clutches.txt")
clutches$year <- as.factor(clutches$year)
clutches <- subset(clutches, !is.na(stage))

sd(clutches$brooding_duration)
sd(clutches$brooding_frequency)

## Does brooding time per clutch decrease with an increasing total number of clutches present at the site?----
m1 <- glmmTMB(brooding_duration ~ nb_clutches + stage + max_temp + rain_previous_day +
                (1|maleID:videoID), 
              family= beta_family(), ziformula = ~1, data = clutches)
summary(m1)
simRes1<-simulateResiduals(m1, plot = TRUE)


## Does brooding frequency per clutch decrease with an increasing total number of clutches present at the site? ----
m2 <- glmmTMB(brooding_frequency ~ nb_clutches + stage + 
                max_temp + rain_previous_day + (1|maleID:videoID), 
              family = gaussian, data = clutches)
summary(m2)
simulateResiduals(m2, plot = TRUE)


### dataset males ----
males <- read.delim("INSERT DIRECTION OF FILE -> males.txt")
males$year <- as.factor(males$year)

sd(males$total_brooding_time)

## Does total brooding time increase with an increasing total number of clutches present at the site? ----
m3 <-glmmTMB(total_brooding_time ~ nb_clutches + max_temp + rain_previous_day + (1|maleID) , data = males, ziformula = ~1, 
             family= beta_family())
summary(m3)
simulateResiduals(m3, plot = TRUE)
