##  Stitching together shapefiles and merging data
## start: 20/3/2018
##  Right so we have data tables and we have shapes files to sorto ut.

pkgs <- list('sf', 'tidyverse', 'tmap', 'stringr')
lapply(pkgs, library, character.only = T)

##  First we have many many shap files to stitch together ----
sf.path <- 'C:/Users/Meng Le Zheng/Google Drive/Data for Meng/State level shapefiles/'
zeros <- paste('0', 1:9, sep = '')

folder.nms <- paste('gz_2010_', c(zeros, 10:56), '_140_00_500k', sep = '')


sf.list <- list(NULL)

## We use try so if there is an error we just skip
for (i in 1:56){
try(
sf.list[[i]] <- st_read(dsn = paste(sf.path, folder.nms[i], sep = ''), 
             layer = folder.nms[i])
)
}


missing <- lapply(sf.list, is.null) %>% unlist %>% which
missing # 3, 7, 14, 43, 52 missing

nms <- lapply(sf.list, names) %>% unlist
table(nms) #all same names
lapply(sf.list, st_crs)

names(sf.list[[1]])

str(sf.list[1])
all.sf <- do.call(rbind, sf.list[-missing])
summary(all.sf) ## well all geoids are unique; 51 states what happened to some

all.sf$geoid <- paste(all.sf$STATE, all.sf$COUNTY, all.sf$TRACT,
                      sep = '')
all.sf$geoid %>% unique %>% length

##  2) Read in the data table file now

## 2000
us2000.df <- 'C:/Users/Meng Le Zheng/Google Drive/Data for Meng/2000data.csv' %>%
  read.csv
us200.
##  Rory's geoid variable hasn't quite saved properly, we need to reconstruct with
##  leading zeroes
us2000.df$geoid <- paste(us2000.df$state %>% formatC(width = 2, flag = 0, format = 'd'), 
                         us2000.df$county %>% formatC(width = 3, flag = 0, format = 'd'), 
                         us2000.df$tract %>% str_pad(width = 6, side = 'right', pad = '0'),
                         sep = '')

us2000.sf <- all.sf %>% merge(us2000.df, by = 'geoid') #32417 matches hmmm around
##  7209 missing
missing.2000 <- us2000.df %>% subset(!(us2000.df$geoid %in% all.sf$geoid))
table(missing.2000$state)
table(missing.2000$MSA) / table(us2000.df$MSA) #big % missing
table(missing.2000$tract)
table(us2000.sf$tract)

##  2009
us2009.df <- 'C:/Users/Meng Le Zheng/Google Drive/Data for Meng/2009data.csv' %>%
  read.csv

us2009.df$geoid <- paste(us2009.df$state %>% formatC(width = 2, flag = 0, format = 'd'), 
                         us2009.df$county %>% formatC(width = 3, flag = 0, format = 'd'), 
                         us2009.df$tract %>% formatC(width = 6, flag = 0, format = 'd'),
                         sep = '')

us2009.sf <- all.sf %>% merge(us2009.df, by = 'geoid') #32434 matches hmmm around
##  7214 missing
missing.2009 <- us2009.df %>% subset(!(us2009.df$geoid %in% all.sf$geoid))
table(missing.2009$state)

## 2016
us2016.df <- 'C:/Users/Meng Le Zheng/Google Drive/Data for Meng/2016data.csv' %>%
  read.csv

us2016.df$geoid <- paste(us2016.df$state %>% formatC(width = 2, flag = 0, format = 'd'), 
                         us2016.df$county %>% formatC(width = 3, flag = 0, format = 'd'), 
                         us2016.df$tract %>% formatC(width = 6, flag = 0, format = 'd'),
                         sep = '')

us2016.sf <- all.sf %>% merge(us2016.df, by = 'geoid') #44825 matches hmmm around
##  60 missing
missing.2016 <- us2016.df %>% subset(!(us2016.df$geoid %in% all.sf$geoid))
table(missing.2016$MSA) # tiny parts missing
##  Right well we have random bits missing

