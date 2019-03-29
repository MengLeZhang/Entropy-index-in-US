##  Code for running the analysis in parallel
library(foreach)
library(doParallel)


local.dir <- 'Local temp files'
us2000.sf <- readRDS(local.dir %>% file.path('us2000_sf.RDS'))

msa.sf <-
  us2000.sf %>%
  filter(MSA == 100) %>%
  mutate(total2000 = quintile1 + quintile2 + quintile3 + quintile4 + quintile5,
         total2000 = total2000 %>% round)

burnin <- 20000
n.sample <- 40000 + burnin #because sample includes burin
thin <- 10

cl <- makePSOCKcluster(n.cores)


foreach (i = 1:n.cores, .combine = c) %dopar% {
  source('Analysis template/Template for analysis.R')
}

stopImplicitCluster(cl)
