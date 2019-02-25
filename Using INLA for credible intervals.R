##  Practise using INLA

pkgs <- c('tidyverse',
          'sf',
          'INLA')
lapply(pkgs, library, character.only = T)


## The example -- using stuff

example.sf <-
  'C:/Users/Meng Le Zheng/Synced Google Drive/Google Drive/Social frontiers paper/Social frontiers R code/Data/Cleaned Pardubice sf.RDS' %>%
  readRDS


example.sf %>% head

example.mod <- inla(crime_total ~ 1, data = example.sf, 
                    control.compute = list(config=TRUE)) # need this option to later draw posterior
?INLA::inla.hyperpar.sample() #hmm
?INLA::inla.posterior.sample() ## okay is it this

example.samples <- inla.posterior.sample(n = 10, example.mod)
example.samples %>% summary # okay so it's in a list
example.samples[1] %>% str # hyper paramter == random effect/ not fixed effect?
example.mod %>% summary #okay no idea what these hyper paramters mean
## latent? is err why are there 95 when we have 94 obs
example.samples[[1]]$latent # okay so 95th is the intercept -- the rest is the predicted var 

##  We can use inla.posterior.sample.eval(fun, r.samples) to exaluate sampels from
##  inla.posterior.sample
inla.posterior.sample.eval(function(...){Intercept + 1}, example.samples)
## okay this sort of works.