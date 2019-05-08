# Empirical Bayes
rm(list = ls())
library("spdep")
library("magrittr")
library("dplyr")

# White 2012
w2012 <- readRDS('../../data/nchs_births/R/Data/model1.rda') %>%
  filter(racehisp_recode == 2) %>%
  filter(dob_yy == 2012) %>%
  group_by(combfips) %>%
  arrange(combfips) %>%
  summarise(births = sum(births) + 1,
            w2012ptb = sum(ptb) + 1,
            w2012ratep1000 = w2012ptb / births * 1000) %>%
  as.data.frame()
w2012$w2012avgptb <- mean(w2012$w2012ptb)
w2012$w2012avgptbratep100 <- mean(w2012$w2012ratep1000)

# White 2016
w2016 <- readRDS('../../data/nchs_births/R/Data/model1.rda') %>%
  filter(racehisp_recode == 2) %>%
  filter(dob_yy == 2016) %>%
  group_by(combfips) %>%
  arrange(combfips) %>%
  summarise(births = sum(births) + 1,
            w2016ptb = sum(ptb) + 1,
            w2016ratep1000 = w2016ptb / births * 1000) %>%
  as.data.frame()
w2016$w2016avgptb <- mean(w2016$w2016ptb)
w2016$w2016avgptbratep100 <- mean(w2016$w2016ratep1000)

# Black 2012
b2012 <- readRDS('../../data/nchs_births/R/Data/model1.rda') %>%
  filter(racehisp_recode == 3) %>%
  filter(dob_yy == 2012) %>%
  group_by(combfips) %>%
  arrange(combfips) %>%
  summarise(births = sum(births) + 1,
            b2012ptb = sum(ptb) + 1,
            b2012ratep1000 = b2012ptb / births * 1000) %>%
  as.data.frame()
b2012$b2012avgptb <- mean(b2012$b2012ptb)
b2012$b2012avgptbratep100 <- mean(b2012$b2012ratep1000)

# Black 2016
b2016 <- readRDS('../../data/nchs_births/R/Data/model1.rda') %>%
  filter(racehisp_recode == 3) %>%
  filter(dob_yy == 2016) %>%
  group_by(combfips) %>%
  arrange(combfips) %>%
  summarise(births = sum(births) + 1,
            b2016ptb = sum(ptb) + 1,
            b2016ratep1000 = b2016ptb / births * 1000) %>%
  as.data.frame()
b2016$b2016avgptb <- mean(b2016$b2016ptb)
b2016$b2016avgptbratep100 <- mean(b2016$b2016ratep1000)

# Empirical Bayes
# Case Count, At Risk
white2012 <- EBest(w2012$w2012ptb, w2012$births, family = "poisson")
white2016 <- EBest(w2016$w2016ptb, w2016$births, family = "poisson")
black2012 <- EBest(b2012$b2012ptb, b2012$births, family = "poisson")
black2016 <- EBest(b2016$b2016ptb, b2016$births, family = "poisson")

# Assign year to Empirical Bayes
white2012$year <- 2012
black2012$year <- 2012
white2016$year <- 2016
black2016$year <- 2016

# Assign county to Empirical Bayes
white2012$combfips <- w2012$combfips
white2016$combfips <- w2016$combfips
black2012$combfips <- b2012$combfips
black2016$combfips <- b2016$combfips

# Create combined data frame
combined <- as.data.frame(rbind(
  cbind(white2012$combfips,
        white2012$year,
        (1000 * white2012$estmm),
        (1000 * black2012$estmm),
        ((1000 * black2012$estmm) / (1000 * white2012$estmm)),
        ((1000 * black2012$estmm) - (1000 * white2012$estmm))),
  cbind(white2016$combfips,
        white2016$year,
        (1000 * white2016$estmm),
        (1000 * black2016$estmm),
        ((1000 * black2016$estmm) / (1000 * white2016$estmm)),
        ((1000 * black2016$estmm) - (1000 * white2016$estmm)))
  ))
colnames(combined) <- c("FIPS", "Year", "White PTB/1000", "Black PTB/1000", 
                        "Black - White Rate Ratio", "Black-White Rate Difference")

saveRDS(combined, '../../data/nchs_births/R/Data/EBestrates.rda')
