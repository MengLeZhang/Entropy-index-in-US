##  Template code to be run in parallel
##  Recreating duncan's code in MSA
##  First run source -- laptop version
library(spdep)
library(CARBayes)

##  Needs; msa.sf and its W matrix to be passed in 
##  other arguments are burinin, n.sample, and thin
### Useful example case
##  INPUT is a file called msa.sf
##  OUTPUT is a literally just a vector called ces.vec giving the ces

# local.dir <- 'Local temp files'
# us2000.sf <- readRDS(local.dir %>% file.path('us2000_sf.RDS'))
# 
# msa.sf <-
#   us2000.sf %>%
#   filter(MSA == 100) %>%
#   mutate(total = quintile1 + quintile2 + quintile3 + quintile4 + quintile5,
#          total = total %>% round)
# 
# burnin <- 20000
# n.sample <- 40000 + burnin #because sample includes burin
# thin <- 10



# Wrapper function --------------------------------------------------------

ces.confidence <- 
  function(msa.sf, burnin = 20000, n.sample = 40000 + 20000, thin = 10){

library(spdep)
library(CARBayes)
    
# 1) Starting MCMC routine ------------------------------------------------
##  Need to create model for each group


#### Create a spatial neighbourhood matrix
W.nb <- poly2nb(msa.sf %>% as('Spatial'), row.names = 1:nrow(msa.sf))
W <- nb2mat(W.nb, style="B")
###



mod1 <- 
  MVS.CARleroux(formula = I(round(msa.sf$quintile1)) ~ 1, 
                family = "binomial", 
                trials = msa.sf$total, 
                W=W, burnin=burnin, n.sample=n.sample, thin=thin)

mod2 <- 
  MVS.CARleroux(formula = I(round(msa.sf$quintile2)) ~ 1, 
                family = "binomial", 
                trials = msa.sf$total, 
                W=W, burnin=burnin, n.sample=n.sample, thin=thin)

mod3 <- 
  MVS.CARleroux(formula = I(round(msa.sf$quintile3)) ~ 1, 
                family = "binomial", 
                trials = msa.sf$total, 
                W=W, burnin=burnin, n.sample=n.sample, thin=thin)

mod4 <- 
  MVS.CARleroux(formula = I(round(msa.sf$quintile4)) ~ 1, 
                family = "binomial", 
                trials = msa.sf$total, 
                W=W, burnin=burnin, n.sample=n.sample, thin=thin)

mod5 <- 
  MVS.CARleroux(formula = I(round(msa.sf$quintile5)) ~ 1, 
                family = "binomial", 
                trials = msa.sf$total, 
                W=W, burnin=burnin, n.sample=n.sample, thin=thin)


# 2) Save the fitted results  ---------------------------------------------
fitted <- list(
  q1 = round(mod1$samples$fitted, 4),
  q2 = round(mod2$samples$fitted, 4),
  q3 = round(mod3$samples$fitted, 4),
  q4 = round(mod4$samples$fitted, 4),
  q5 = round(mod4$samples$fitted, 4)
)



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
               real.pop = msa.sf$total) %>%
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


# 3) Running the entropy function -----------------------------------------

##  The entropy function
source('Entropy function.R')

ces.vec <- rep(NA_real_, length(fitted.scale) ) 

for(i in 1:length(ces.vec)) {
  ces.vec[i] <- entropy(fitted.scale[[i]], sub.nms = c('q1', 'q2', 'q3', 'q4', 'q5'))$ces
}

return(ces.vec)
}