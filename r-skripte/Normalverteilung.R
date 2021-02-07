library(mosaic)

xpnorm(44.5, mean = 55, sd = 15)

zscore(c(0,1,2))

zscore(c(100,200,300))

xpnorm(60, mean = 55, sd = 15)

xpnorm(c(-2,2))

xqnorm(0.9, mean = 55, sd = 15)

# tips einlesen in R
tips <- read.csv2("tips.csv")

# QQ-Plot
gf_qq( ~ total_bill,
       data = tips) %>%
  gf_qqline()