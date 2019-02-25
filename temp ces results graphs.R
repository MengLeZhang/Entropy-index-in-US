##  Here's some temporary graphs for Rory
##  Start: 20/3/2018

library(tidyverse)
library(gridExtra)


ces.res <- 'ces MSA results redone quntiles 27032018.csv' %>% read.csv
head(ces.res)

ces1 <- ggplot(data = ces.res, aes(x = ces00, y = ces16)) + geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('2016') + xlab('2000') +
  ggtitle('MSA entropy in 2000 and 2016')
ces1

ces2 <- ggplot(data = ces.res, aes(x = log(pop2000), y = diff0016)) + 
  geom_point() + geom_smooth(alpha = 0.2) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Change in CES') + xlab('Population (log)')+
  ggtitle('CES change by population (loess curve)') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed')
ces2

