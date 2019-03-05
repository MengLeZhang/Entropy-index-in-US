#### This file merges the latest data set (2010-2014) with the other 3 (1990, 2000, 2005-2009)
#### as they are in different orders and differing numbers of rows. The first 3 data sets seem the same


#### Set the working directory
setwd("/Volumes/duncan/papers/AMENpaper3/analysis")


#### Read in the data
dat1 <- read.csv(file="familyincome.csv")
dat2 <- read.csv(file="LA2010_2014dunc.csv")
n <- nrow(dat1)


#### Re-order the 2010-2014 data
temp <- as.data.frame(array(NA, c(n,5)))
colnames(temp) <- c("ID", colnames(dat2)[3:6])
temp$ID <- dat1$ID

    for(i in 1:n)
    {
    id <- temp$ID[i]    
    row <- which(id==dat2$id2)   
        if(length(row)>0)
        {
        temp[i, 2:5] <- dat2[row, 3:6]   
        }else
        {}
    }


#### Create the combined data set
dat <- data.frame(dat1, temp[ ,-1])


#### Round the non-integer values
dat <- round(dat)

#### Create the total populations for each area and time period
total_1990 <- dat$q1fam_1990 + dat$q2fam_1990 + dat$q3fam_1990 + dat$q4fam_1990
total_2000 <- dat$q1fam_2000 + dat$q2fam_2000 + dat$q3fam_2000 + dat$q4fam_2000
total_20052009 <- dat$q1fam_20052009 + dat$q2fam_20052009 + dat$q3fam_20052009 + dat$q4fam_20052009
total_20102014 <- dat$q1fam_20102014 + dat$q2fam_20102014 + dat$q3fam_20102014 + dat$q4fam_20102014


#### Create the final data set
data.final <- data.frame(dat[ ,1:5], total_1990, dat[ ,6:9], total_2000, dat[ ,10:13], total_20052009, dat[ ,14:17], total_20102014)
write.csv(data.final, file="LA family income data.csv")
