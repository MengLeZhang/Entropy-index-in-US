##  Recreating duncan's code in MSA
##  First run source -- laptop version
library(spdep)
library(CARBayes)
library(foreach)
library(doParallel)


us2000.sf <- readRDS('Example/us2000_sf.RDS')

example.sf <-
  us2000.sf %>%
  filter(MSA == 100) %>%
  mutate(total2000 = quintile1 + quintile2 + quintile3 + quintile4 + quintile5,
         total2000 = total2000 %>% round)
##  So pick not a big MSA
##  example.sf %>% summary

##  So we want to basically create a single version 
###############################################
#### Specify the number of samples to generate
burnin <- 20000
n.sample <- 100000
thin <- 10

#### Create a spatial neighbourhood matrix
W.nb <- poly2nb(example.sf %>% as('Spatial'), row.names = 1:nrow(example.sf))
W <- nb2mat(W.nb, style="B")

n.cores <- 2

doParallel::registerDoParallel(n.cores)
?foreach

out.list <-
  foreach(i = 1:n.cores) %dopar% ({

    library(CARBayes)
####################################################################
#### Model the Quintile 1 vs the rest with the MVS.CARleroux() model
#####################################################################
#### Format the variables
## Fit the model for each quantile 

mod1 <- 
  MVS.CARleroux(formula = I(round(example.sf$quintile1)) ~ 1, 
                family = "binomial", 
                trials = example.sf$total2000, 
                W=W, burnin=burnin, n.sample=n.sample, thin=thin)

mod2 <- 
  MVS.CARleroux(formula = I(round(example.sf$quintile2)) ~ 1, 
                family = "binomial", 
                trials = example.sf$total2000, 
                W=W, burnin=burnin, n.sample=n.sample, thin=thin)

mod3 <- 
  MVS.CARleroux(formula = I(round(example.sf$quintile3)) ~ 1, 
                family = "binomial", 
                trials = example.sf$total2000, 
                W=W, burnin=burnin, n.sample=n.sample, thin=thin)

mod4 <- 
  MVS.CARleroux(formula = I(round(example.sf$quintile4)) ~ 1, 
                family = "binomial", 
                trials = example.sf$total2000, 
                W=W, burnin=burnin, n.sample=n.sample, thin=thin)

mod5 <- 
  MVS.CARleroux(formula = I(round(example.sf$quintile5)) ~ 1, 
                family = "binomial", 
                trials = example.sf$total2000, 
                W=W, burnin=burnin, n.sample=n.sample, thin=thin)

##  Around 70-80secs per run

##  Saved fitted results
fitted <- list(
  q1 = round(mod1$samples$fitted, 4),
  q2 = round(mod2$samples$fitted, 4),
  q3 = round(mod3$samples$fitted, 4),
  q4 = round(mod4$samples$fitted, 4),
  q5 = round(mod4$samples$fitted, 4)
)

})

##  each is a matrix -- each row is a draw, each col is place
fitted.scale <- list(NULL)



##  First just extract dataframes with the data then we change the numbers in 
##  each quintile so it is scaled to equal the total
for(i in 1:nrow(fitted$q1)){
  fitted.scale[[i]] <- 
    data.frame(q1.mod = fitted$q1[i, ],
               q2.mod = fitted$q2[i, ],
               q3.mod = fitted$q3[i, ],
               q4.mod = fitted$q4[i, ],
               q5.mod = fitted$q5[i, ],
               real.pop = example.sf$total2000) %>%
    ##  Now to count the model total and find the amount to rescale it so it is the same
    ##  as the real pop
    mutate(model.pop = q1.mod + q2.mod + q3.mod + q4.mod + q5.mod,
           scale = real.pop / model.pop) %>%
    mutate(q1 = q1.mod * scale,
           q2 = q2.mod * scale,
           q3 = q3.mod * scale,
           q4 = q4.mod * scale,
           q5 = q5.mod * scale) %>%
    dplyr::select(q1:q5, real.pop)
}

##  Now we need to scale all the quantile so it fits

##  The entropy function
source('Entropy function.R')

ces.df <- data.frame(MSA = 1:length(fitted.scale), 
                     ces00 = NA)

##  Do not run -- non-parallel implementation 
# x <- proc.time()
# for (i in 1:nrow(ces.df)){
#   ces.df$ces00[i] <- entropy(fitted.scale[[i]], sub.nms = c('q1', 'q2', 'q3', 'q4', 'q5'))$ces
# }
# proc.time() - x #27 secs
# ces.df %>% summary

##  All steps are necessary 
registerDoParallel(2) # 2 cores, impliciting creates cluster
##  We msut register these clusters first (or the parallel backend)

x <- proc.time()
foreach(i = 1:nrow(ces.df)) %dopar% {
  ces.df$ces00[i] <- entropy(fitted.scale[[i]], sub.nms = c('q1', 'q2', 'q3', 'q4', 'q5'))$ces
}
proc.time() - x #18 secs

x <- proc.time()
foreach(i = 1:nrow(ces.df), .combine = c) %dopar% {
  entropy(fitted.scale[[i]], sub.nms = c('q1', 'q2', 'q3', 'q4', 'q5'))$ces
}
proc.time() - x #16 secs


##  using iter
t <- iter(fitted.scale)
t %>% nextElem() #okay.. so calls next element

x <- proc.time()
foreach(i = iter(fitted.scale), .combine = c) %dopar% {
  entropy(i, sub.nms = c('q1', 'q2', 'q3', 'q4', 'q5'))$ces
}
proc.time() - x #17 secs

stopImplicitCluster()
##  still not that fast .. why is

## Takes ages!
##  Any point in doing this in c++?