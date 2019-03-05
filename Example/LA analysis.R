######################
#### Model the LA data
######################

#### Set the working directory
setwd("/Volumes/duncan/papers/AQMENpaper3/analysis")


#### Load the libraries required
library(Rcpp)
library(shapefiles)
library(sp)
library(spdep)
library(CARBayes)
library(rgeos)
library(ggplot2)
library(maptools)
library(ggsn)
library(RColorBrewer)
library(ggmap)
library(rgdal)


################################
#### Read in and format the data
################################
#### Read in the data and subset so that only tracts with a total popualtion 
#### above 50 in all 4 time periods are kept and those with no missing values
# Read in the data
dat <- read.csv(file="LA family income data.csv")
#### Think there could be an error in the column labels, Q2 and Q3 switched around

# remove tracts with missing values
dat2 <- dat[apply(is.na(dat),1,sum)==0, ] 

# Remove tracts with population totals less than 50 in any year
above50 <- dat2$total_1990 > 50 & dat2$total_2000 > 50 & dat2$total_20052009 > 50 & dat2$total_20102014 > 50
dat3 <- dat2[above50, ]



#### Read in the shapefile and combine with the data to create a spatialpolygonsdataframe
# Read in the shapefile
shp <- read.shp(shp.name="LA_MSA.shp")
dbf <- read.dbf(dbf.name="LA_MSA.dbf")

# For combing ensure the unique tract identifier is the rownames of dat3 and the first column in the dbf file
dbf2 <- dbf
dbf2$dbf <- data.frame(code=substring(dbf2$dbf$GEOID10, first=2, last=11), dbf2$dbf)
dbf2$dbf$code <- as.character(dbf2$dbf$code)
rownames(dat3) <- dat3$ID
dat3$ID <- NULL

# Create the spatial data object
sp.dat <- combine.data.shapefile(dat3, shp, dbf2)
plot(sp.dat)



##########################################
#### Exploratory plotting and data summary
##########################################
#### Check for odd values
summary(dat3)
pairs(dat3[ ,c(5,10,15,20)])
pairs(dat3[,c(1:4)])


#### Create proportions for each quartile and year
dat.prop <- dat3[ c(1:4, 6:9, 11:14, 16:19)]

dat.prop$q1fam_1990 <- dat.prop$q1fam_1990 / dat3$total_1990
dat.prop$q2fam_1990 <- dat.prop$q2fam_1990 / dat3$total_1990
dat.prop$q3fam_1990 <- dat.prop$q3fam_1990 / dat3$total_1990
dat.prop$q4fam_1990 <- dat.prop$q4fam_1990 / dat3$total_1990

dat.prop$q1fam_2000 <- dat.prop$q1fam_2000 / dat3$total_2000
dat.prop$q2fam_2000 <- dat.prop$q2fam_2000 / dat3$total_2000
dat.prop$q3fam_2000 <- dat.prop$q3fam_2000 / dat3$total_2000
dat.prop$q4fam_2000 <- dat.prop$q4fam_2000 / dat3$total_2000

dat.prop$q1fam_20052009 <- dat.prop$q1fam_20052009 / dat3$total_20052009
dat.prop$q2fam_20052009 <- dat.prop$q2fam_20052009 / dat3$total_20052009
dat.prop$q3fam_20052009 <- dat.prop$q3fam_20052009 / dat3$total_20052009
dat.prop$q4fam_20052009 <- dat.prop$q4fam_20052009 / dat3$total_20052009

dat.prop$q1fam_20102014 <- dat.prop$q1fam_20102014 / dat3$total_20102014
dat.prop$q2fam_20102014 <- dat.prop$q2fam_20102014 / dat3$total_20102014
dat.prop$q3fam_20102014 <- dat.prop$q3fam_20102014 / dat3$total_20102014
dat.prop$q4fam_20102014 <- dat.prop$q4fam_20102014 / dat3$total_20102014

# Plot the proportions for each time period
pairs(dat.prop[,c(1:4)])
pairs(dat.prop[,c(5:8)])
pairs(dat.prop[,c(9:12)])
pairs(dat.prop[,c(13:16)])

# Plot the proportions for each quanrtile
pairs(dat.prop[,c(1,5,9,13)])
pairs(dat.prop[,c(2,6,10,14)])
pairs(dat.prop[,c(3,7,11,15)])
pairs(dat.prop[,c(4,8,12,16)])


#### Crete example maps of the proportions
# Create the spatial data object
sp.dat.prop <- combine.data.shapefile(dat.prop, shp, dbf2)

# Turn it into an object the ggplot2 package can handle
sp.dat.prop@data$id <- rownames(sp.dat.prop@data)
data.spatial <- fortify(sp.dat.prop, region = "id")
data.spatial2 <- merge(data.spatial, sp.dat.prop@data, by = "id")


#### Example map - proportion in lowest income in 2010-2014
map1 <- ggplot(data = data.spatial2, aes(x=long, y=lat, goup=group, fill = c(q1fam_20102014)))
B <- map1 + geom_polygon() + coord_equal() + xlab("Longitude") + ylab("Latitude") + 
    labs(title = "Proportion in the lowest income quartile in 2010 - 2014", fill = "Proportion") +  
    theme(title = element_text(size=20), plot.title=element_text(size=25))
B



###############################################
#### Specify common quantities for all 4 models
###############################################
#### Specify the number of samples to generate
burnin <- 20000
n.sample <- 100000
thin <- 10

#### Create a spatial neighbourhood matrix
W.nb <- poly2nb(sp.dat.prop, row.names = rownames(dat.prop))
W <- nb2mat(W.nb, style="B")


#### Total number of people families in each tract and year as the trials argument
trials.mat <- cbind(dat3$total_1990, dat3$total_2000, dat3$total_20052009, dat3$total_20102014) 
trials <- as.numeric(t(trials.mat))



####################################################################
#### Model the Quartile 1 vs the rest with the MVS.CARleroux() model
#####################################################################
#### Format the variables
Y.mat <- cbind(dat3$q1fam_1990, dat3$q1fam_2000, dat3$q1fam_20052009, dat3$q1fam_20102014) 
Y <- as.numeric(t(Y.mat))


#### Run the model
## Fit the model
mod <- MVS.CARleroux(formula=Y~1, family="binomial", trials=trials, W=W, burnin=burnin, n.sample=n.sample, thin=thin)
print(mod)

## Check MCMC convergence
plot(mod$samples$beta)
plot(mod$samples$rho)
plot(mod$samples$Sigma[ , 1,1])
plot(mod$samples$Sigma[ , 2,1])
plot(mod$samples$phi[ ,1])

## Compute the between year correlation matrix
Sigma.median <- apply(mod$samples$Sigma, c(2,3), median)
C <- array(1, c(4,4))
C[1,2] <- C[2,1] <-  Sigma.median[1,2] / sqrt(Sigma.median[1,1] * Sigma.median[2,2])
C[1,3] <- C[3,1] <-  Sigma.median[1,3] / sqrt(Sigma.median[1,1] * Sigma.median[3,3])
C[1,4] <- C[4,1] <-  Sigma.median[1,4] / sqrt(Sigma.median[1,1] * Sigma.median[4,4])
C[2,3] <- C[3,2] <-  Sigma.median[2,3] / sqrt(Sigma.median[2,2] * Sigma.median[3,3])
C[2,4] <- C[4,2] <-  Sigma.median[2,4] / sqrt(Sigma.median[4,4] * Sigma.median[2,2])
C[4,3] <- C[3,4] <-  Sigma.median[4,3] / sqrt(Sigma.median[4,4] * Sigma.median[3,3])
C

## Save the elements required
fit <- round(mod$samples$fitted, 4)
write.csv(fit, "Q1fitted.csv")
rm(mod)
rm(fit)



####################################################################
#### Model the Quartile 2 vs the rest with the MVS.CARleroux() model
#####################################################################
#### Format the variables
Y.mat <- cbind(dat3$q2fam_1990, dat3$q2fam_2000, dat3$q2fam_20052009, dat3$q2fam_20102014) 
Y <- as.numeric(t(Y.mat))


#### Run the model
## Fit the model
mod <- MVS.CARleroux(formula=Y~1, family="binomial", trials=trials, W=W, burnin=burnin, n.sample=n.sample, thin=thin)
print(mod)

## Check MCMC convergence
plot(mod$samples$beta)
plot(mod$samples$rho)
plot(mod$samples$Sigma[ , 1,1])
plot(mod$samples$Sigma[ , 2,1])
plot(mod$samples$phi[ ,1])

## Compute the between year correlation matrix
Sigma.median <- apply(mod$samples$Sigma, c(2,3), median)
C <- array(1, c(4,4))
C[1,2] <- C[2,1] <-  Sigma.median[1,2] / sqrt(Sigma.median[1,1] * Sigma.median[2,2])
C[1,3] <- C[3,1] <-  Sigma.median[1,3] / sqrt(Sigma.median[1,1] * Sigma.median[3,3])
C[1,4] <- C[4,1] <-  Sigma.median[1,4] / sqrt(Sigma.median[1,1] * Sigma.median[4,4])
C[2,3] <- C[3,2] <-  Sigma.median[2,3] / sqrt(Sigma.median[2,2] * Sigma.median[3,3])
C[2,4] <- C[4,2] <-  Sigma.median[2,4] / sqrt(Sigma.median[4,4] * Sigma.median[2,2])
C[4,3] <- C[3,4] <-  Sigma.median[4,3] / sqrt(Sigma.median[4,4] * Sigma.median[3,3])
C

## Save the elements required
fit <- round(mod$samples$fitted, 4)
write.csv(fit, "Q2fitted.csv")
rm(mod)
rm(fit)



####################################################################
#### Model the Quartile 3 vs the rest with the MVS.CARleroux() model
#####################################################################
#### Format the variables
Y.mat <- cbind(dat3$q3fam_1990, dat3$q3fam_2000, dat3$q3fam_20052009, dat3$q3fam_20102014) 
Y <- as.numeric(t(Y.mat))


#### Run the model
## Fit the model
mod <- MVS.CARleroux(formula=Y~1, family="binomial", trials=trials, W=W, burnin=burnin, n.sample=n.sample, thin=thin)
print(mod)

## Check MCMC convergence
plot(mod$samples$beta)
plot(mod$samples$rho)
plot(mod$samples$Sigma[ , 1,1])
plot(mod$samples$Sigma[ , 2,1])
plot(mod$samples$phi[ ,1])

## Compute the between year correlation matrix
Sigma.median <- apply(mod$samples$Sigma, c(2,3), median)
C <- array(1, c(4,4))
C[1,2] <- C[2,1] <-  Sigma.median[1,2] / sqrt(Sigma.median[1,1] * Sigma.median[2,2])
C[1,3] <- C[3,1] <-  Sigma.median[1,3] / sqrt(Sigma.median[1,1] * Sigma.median[3,3])
C[1,4] <- C[4,1] <-  Sigma.median[1,4] / sqrt(Sigma.median[1,1] * Sigma.median[4,4])
C[2,3] <- C[3,2] <-  Sigma.median[2,3] / sqrt(Sigma.median[2,2] * Sigma.median[3,3])
C[2,4] <- C[4,2] <-  Sigma.median[2,4] / sqrt(Sigma.median[4,4] * Sigma.median[2,2])
C[4,3] <- C[3,4] <-  Sigma.median[4,3] / sqrt(Sigma.median[4,4] * Sigma.median[3,3])
C

## Save the elements required
fit <- round(mod$samples$fitted, 4)
write.csv(fit, "Q3fitted.csv")
rm(mod)
rm(fit)



####################################################################
#### Model the Quartile 4 vs the rest with the MVS.CARleroux() model
#####################################################################
#### Format the variables
Y.mat <- cbind(dat3$q4fam_1990, dat3$q4fam_2000, dat3$q4fam_20052009, dat3$q4fam_20102014) 
Y <- as.numeric(t(Y.mat))


#### Run the model
## Fit the model
mod <- MVS.CARleroux(formula=Y~1, family="binomial", trials=trials, W=W, burnin=burnin, n.sample=n.sample, thin=thin)
print(mod)

## Check MCMC convergence
plot(mod$samples$beta)
plot(mod$samples$rho)
plot(mod$samples$Sigma[ , 1,1])
plot(mod$samples$Sigma[ , 2,1])
plot(mod$samples$phi[ ,1])

## Compute the between year correlation matrix
Sigma.median <- apply(mod$samples$Sigma, c(2,3), median)
C <- array(1, c(4,4))
C[1,2] <- C[2,1] <-  Sigma.median[1,2] / sqrt(Sigma.median[1,1] * Sigma.median[2,2])
C[1,3] <- C[3,1] <-  Sigma.median[1,3] / sqrt(Sigma.median[1,1] * Sigma.median[3,3])
C[1,4] <- C[4,1] <-  Sigma.median[1,4] / sqrt(Sigma.median[1,1] * Sigma.median[4,4])
C[2,3] <- C[3,2] <-  Sigma.median[2,3] / sqrt(Sigma.median[2,2] * Sigma.median[3,3])
C[2,4] <- C[4,2] <-  Sigma.median[2,4] / sqrt(Sigma.median[4,4] * Sigma.median[2,2])
C[4,3] <- C[3,4] <-  Sigma.median[4,3] / sqrt(Sigma.median[4,4] * Sigma.median[3,3])
C

## Save the elements required
fit <- round(mod$samples$fitted, 4)
write.csv(fit, "Q4fitted.csv")
rm(mod)
rm(fit)




#################################
#### Compute the Entropy measures
#################################
#### Read in the samples
Q1 <- read.csv(file="Q1fitted.csv")
Q1$X <- NULL
Q2 <- read.csv(file="Q2fitted.csv")
Q2$X <- NULL
Q3 <- read.csv(file="Q3fitted.csv")
Q3$X <- NULL
Q4 <- read.csv(file="Q4fitted.csv")
Q4$X <- NULL
m <- nrow(Q1)


#### Compute the population totals and the area population totals
N.u <- cbind(dat3$total_1990, dat3$total_2000, dat3$total_20052009, dat3$total_20102014) 
colnames(N.u) <- c("N1990", "N2000", "N20052009", "N20102014")
N <- apply(N.u,2,sum)
N.u.vec <- as.numeric(t(N.u))
K <- nrow(W)


#### Compute the metrics
CES <- array(NA, c(m, 4))
colnames(CES) <- c("N1990", "N2000", "N20052009", "N20102014")

    for(i in 1:m)
    {
     #### Compute the unweighted probabilities   
     p.q1u.unscaled <- as.numeric(Q1[i, ]) / as.numeric(N.u.vec)   
     p.q2u.unscaled <- as.numeric(Q2[i, ]) / as.numeric(N.u.vec)   
     p.q3u.unscaled <- as.numeric(Q3[i, ]) / as.numeric(N.u.vec)   
     p.q4u.unscaled <- as.numeric(Q4[i, ]) / as.numeric(N.u.vec)   
     p.unscaled.sum <- p.q1.unscaled + p.q2.unscaled + p.q3.unscaled + p.q4.unscaled
     
     #### Compute the weighted probabilities
     p.q1u <- p.q1u.unscaled / p.unscaled.sum
     p.q2u <- p.q2u.unscaled / p.unscaled.sum        
     p.q3u <- p.q3u.unscaled / p.unscaled.sum
     p.q4u <- p.q4u.unscaled / p.unscaled.sum
        
     #### Compute the proportion of people in each quartile in each time period (unscaled)
     p.q1.unscaled <- apply((N.u * matrix(p.q1u, ncol=4, byrow=TRUE)),2,sum) / N
     p.q2.unscaled <- apply((N.u * matrix(p.q2u, ncol=4, byrow=TRUE)),2,sum) / N
     p.q3.unscaled <- apply((N.u * matrix(p.q3u, ncol=4, byrow=TRUE)),2,sum) / N
     p.q4.unscaled <- apply((N.u * matrix(p.q4u, ncol=4, byrow=TRUE)),2,sum) / N
     
     #### Compute the proportion of people in each quartile in each time period (scaled)
     p.q1 <- p.q1.unscaled / sum(p.q1.unscaled)
     p.q2 <- p.q2.unscaled / sum(p.q2.unscaled)
     p.q3 <- p.q3.unscaled / sum(p.q3.unscaled)
     p.q4 <- p.q4.unscaled / sum(p.q4.unscaled)
     
     #### Compute the Local Entropy Score LES_u (equation 6 in the paper)
     LES.q1u <- matrix(p.q1u * log(p.q1u / rep(p.q1,K)), ncol=4, byrow=TRUE)
     LES.q2u <- matrix(p.q2u * log(p.q2u / rep(p.q2,K)), ncol=4, byrow=TRUE)
     LES.q3u <- matrix(p.q3u * log(p.q3u / rep(p.q3,K)), ncol=4, byrow=TRUE)
     LES.q4u <- matrix(p.q4u * log(p.q4u / rep(p.q4,K)), ncol=4, byrow=TRUE)
     LES.u <- LES.q1u + LES.q2u + LES.q3u + LES.q4u
     
     #### Compute the Citywide Entropy Score CES (equation 9 in the paper)
     CES[i, ] <- apply((N.u * LES.u),2,sum) / N
    }



#### CES score for each year - posterior median and 95% credible interval
t(apply(CES, 2, quantile, c(0.5, 0.025, 0.975)))

#### CES score difference between 20102014 - 1990
quantile(CES[ ,4] - CES[ ,1], c(0.5, 0.025, 0.975))

#### CES score difference between 20100 - 1990
quantile(CES[ ,2] - CES[ ,1], c(0.5, 0.025, 0.975))

#### CES score difference between 20052009 - 2000
quantile(CES[ ,3] - CES[ ,2], c(0.5, 0.025, 0.975))

