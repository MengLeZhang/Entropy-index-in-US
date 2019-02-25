##  Entropy point estimates pt.2
##  This time we will collapse the middle quintiles
##  Start: 27/3/2018

pkgs <- list('sf', 'tidyverse', 'tmap', 'stringr')
lapply(pkgs, library, character.only = T)


##  1) read in the data
us2000.df <- 'C:/Users/Meng Le Zheng/Google Drive/Data for Meng/2000data.csv' %>%
  read.csv
us2009.df <- 'C:/Users/Meng Le Zheng/Google Drive/Data for Meng/2009data.csv' %>%
  read.csv
us2016.df <- 'C:/Users/Meng Le Zheng/Google Drive/Data for Meng/2016data.csv' %>%
  read.csv

summary(us2000.df)

##now to collapse the quntiles 
us2000.df$quintile234 <- us2000.df$quintile2 + us2000.df$quintile3 + us2000.df$quintile4
us2009.df$quintile234 <- us2009.df$quintile2 + us2009.df$quintile3 + us2009.df$quintile4
us2016.df$quintile234 <- us2016.df$quintile2 + us2016.df$quintile3 + us2016.df$quintile4

##  2) So we need a function that calculate the LES for each MSA: currently we have
##  100 MSAs
##  We will create a function called entropy that will calculate the ACES and CES
##  from the Kramer paper. We need x which is the dataset and sub.nms which is
##  the names of subgroup colmns (which contain number in each Nhood)
##  Will return 2 objects: the ACES and CES as well as the pop

entropy <- function(x, sub.nms){
  
  i <- length(sub.nms)
  nhoods <- nrow(x[, sub.nms])
  
  p_i <- colSums(x[, sub.nms]) / sum(x[, sub.nms])
  n_u <- rowSums(x[, sub.nms], na.rm = T)
  n_u.mat <- matrix(rep(n_u, i), ncol = i, byrow = F)
  p_iu.mat <- x[, sub.nms] / n_u.mat
  
  p_i.mat <- matrix(rep(p_i, nhoods), ncol = i, byrow = T) ## of course it is 0.2
  
  result.mat <- p_iu.mat * log(p_iu.mat / p_i.mat)
  
  
  les <- rowSums(result.mat, na.rm = T) ## so if there is na due to log(0) we ignore
  ##  matrix(c(NA, 1, NA, 2), ncol = 2, byrow = T ) %>% rowSums(na.rm = T) ## example
  
  aces <- colSums(result.mat * (n_u / sum(n_u)), na.rm = T) #will sum vector by vector (or until rep)
  ces <- sum(aces)
  
  ##  Return results
  return(list(aces = aces, ces = ces, pop = sum(n_u, na.rm = T)))
}


##  3) Now to try and get results for all 100 MSAs 
ces.df <- data.frame(MSA = 1:100, pop2000 = NA, 
                     ces00 = NA, ces09 = NA, ces16 = NA)

for (i in 1:100){
  ##  Select MSA
  temp00 <- us2000.df %>% subset(MSA == i)
  temp09 <- us2009.df %>% subset(MSA == i)
  temp16 <- us2016.df %>% subset(MSA == i)
  
  sub.nms <- c('quintile1', 'quintile234', 'quintile5')
  
  ##  Calculate results
  ces.df$pop2000[i] <- temp00[, sub.nms] %>% sum(na.rm = T)
  ces.df$ces00[i] <- entropy(x = temp00, sub.nms)$ces
  ces.df$ces09[i] <- entropy(x = temp09, sub.nms)$ces
  ces.df$ces16[i] <- entropy(x = temp16, sub.nms)$ces
}

ces.df$diff0016 <- ces.df$ces16 - ces.df$ces09

## 3b) aces
aces.df <- list(NULL)

for (i in 1:100){
  ##  Select MSA
  temp00 <- us2000.df %>% subset(MSA == i)
  temp09 <- us2009.df %>% subset(MSA == i)
  temp16 <- us2016.df %>% subset(MSA == i)
  
  sub.nms <- c('quintile1', 'quintile234', 'quintile5')
  
  ##  Calculate results
  
  t1 <- entropy(x = temp00, sub.nms)$aces
  t2 <- entropy(x = temp09, sub.nms)$aces
  t3 <- entropy(x = temp16, sub.nms)$aces
  aces.df[[i]] <- data.frame(MSA = i, year = c(2000, 2009, 2016), 
                             matrix(c(t1,t2,t3), byrow = T, nrow =3))
  names(aces.df[[i]]) <- c('msa', 'year', 'acesq1', 'acesq234', 'acesq5')
}
aces.df <- do.call(rbind, aces.df)

write.csv(aces.df, 'aces MSA results redone quntiles 27032018.csv')






##  4) Now let's just save and exit
write.csv(ces.df, 'ces MSA results redone quntiles 27032018.csv')


##  5) Read in the old dataset
ces.dfold <- read.csv('ces MSA results 20032018.csv', row.names = F)
summary(ces.df - ces.dfold[,-1])
ces.dfold

## well there are differences in absolute terms-- its just smallish