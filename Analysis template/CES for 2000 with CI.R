##  Code for running the analysis in parallel
library(foreach)
library(doParallel)


local.dir <- 'Local temp files'
us2000.sf <- readRDS(local.dir %>% file.path('us2000_sf.RDS'))
ci_function <- source('Analysis template/Template for analysis.R')


msa.sf <-
  us2000.sf %>%
  filter(MSA == 100) %>%
  mutate(total = quintile1 + quintile2 + quintile3 + quintile4 + quintile5,
         total = total %>% round)

burnin <- 20000
n.sample <- 40000 + burnin #because sample includes burin
thin <- 10

n.cores <- detectCores() - 1
cl <- makePSOCKcluster(n.cores)

foreach (i = 1:n.cores, .combine = c) %dopar% {
  library(tidyverse)
  library(sf)
  ces.confidence(msa.sf)
}
## okay so dopar doesn't export libraries -- other than that it will export if in
# function
stopImplicitCluster(cl)
