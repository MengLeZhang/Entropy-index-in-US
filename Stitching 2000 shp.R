##  Stitching together shapefiles and merging data
## start: 20/3/2018
##  Right so we have data tables and we have shapes files to sorto ut.

pkgs <- list('sf', 'tidyverse', 'tmap', 'stringr')
lapply(pkgs, library, character.only = T)

##  First we have many many shap files to stitch together ----
sf.path <- 'C:/Users/Meng Le Zheng/Documents/US census shp/tracts 2000'
format.nums <- 1:56 %>% formatC(width = 2, flag = 0, format = 'd')

folder.nms <- paste('tr', format.nums, '_d00', sep = '')
folder.nms
sf.list <- list(NULL)

## We use try so if there is an error we just skip
for (i in 1:56){
  try(
    sf.list[[i]] <- st_read(dsn = sf.path, 
                            layer = folder.nms[i])
  )
}


missing <- lapply(sf.list, is.null) %>% unlist %>% which
missing # 3, 7, 14, 43, 52 missing

head(sf.list[[2]])

nms <- lapply(sf.list, names) %>% unlist
table(nms) #all same names
lapply(sf.list, st_crs)

##  Not all have the same amount of cols so we have to restrict 
not.miss <- sf.list[-missing]
for (i in 1:length(not.miss)){
  not.miss[[i]] <- not.miss[[i]][c('NAME', 'TRACT', 'COUNTY', 'STATE')]
}

all.sf <- do.call(rbind, not.miss)
summary(all.sf) ## well all geoids are unique; 51 states what happened to some
head(all.sf)

all.sf$geoid <- paste(all.sf$STATE, all.sf$COUNTY, 
                      all.sf$TRACT %>% str_pad(width = 6, side = 'right', pad = '0'),
                      sep = '')
all.sf$TRACT %>% str_pad(width = 6, side = 'right', pad = '0')
?formatC
all.sf$geoid2 <- paste(all.sf$STATE, all.sf$COUNTY,
                      sep = '')


##  2) Read in the data table file now
## 2000
us2000.df <- 'C:/Users/Meng Le Zheng/Google Drive/Data for Meng/2000data.csv' %>%
  read.csv

us2000.df$geoid2 <- paste(us2000.df$state %>% formatC(width = 2, flag = 0, format = 'd'), 
                         us2000.df$county %>% formatC(width = 3, flag = 0, format = 'd'),
                         sep = '')


table(us2000.df$geoid2 %in% all.sf$geoid2)
##  Rory's geoid variable hasn't quite saved properly, we need to reconstruct with
##  leading zeroes
us2000.df$geoid <- paste(us2000.df$state %>% formatC(width = 2, flag = 0, format = 'd'), 
                         us2000.df$county %>% formatC(width = 3, flag = 0, format = 'd'), 
                         us2000.df$tract %>% formatC(width = 6, flag = 0, format = 'd'),
                         sep = '')

table(us2000.df$geoid  %in% all.sf$geoid) #38 do not match
us2000.sf <- all.sf %>% merge(us2000.df, by = 'geoid') #so good we have double matches!!

missing.2000 <- us2000.df %>% subset(!(us2000.df$geoid %in% all.sf$geoid))
table(missing.2000$MSA) # so tiny the amount of non matches

##  2009
us2009.df <- 'C:/Users/Meng Le Zheng/Google Drive/Data for Meng/2009data.csv' %>%
read.csv

us2009.df$geoid <- paste(us2009.df$state %>% formatC(width = 2, flag = 0, format = 'd'), 
                         us2009.df$county %>% formatC(width = 3, flag = 0, format = 'd'), 
                         us2009.df$tract %>% formatC(width = 6, flag = 0, format = 'd'),
                         sep = '')

us2009.sf <- all.sf %>% merge(us2009.df, by = 'geoid') #again almost all matches
table(us2009.df$geoid  %in% all.sf$geoid) #74 do not match
missing.2009 <- us2009.df %>% subset(!(us2009.df$geoid %in% all.sf$geoid))
table(missing.2009$state)
table(us2009.df$MSA)

## So in conclusion 2009 is a perfect match

