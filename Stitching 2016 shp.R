##  Stitching together shapefiles and merging data
## start: 20/3/2018
##  Right so we have data tables and we have shapes files to sorto ut.

pkgs <- list('sf', 'tidyverse', 'tmap')
lapply(pkgs, library, character.only = T)

##  First we have many many shap files to stitch together ----
sf.path <- 'C:/Users/Meng Le Zheng/Documents/US census shp/tracts 2016'
format.nums <- 1:56 %>% formatC(width = 2, flag = 0, format = 'd')

folder.nms <- paste('tl_2016_', format.nums, '_tract', sep = '')
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

nms <- lapply(sf.list, names) %>% unlist
table(nms) #all same names
lapply(sf.list, st_crs)

all.sf <- do.call(rbind, sf.list[-missing])
summary(all.sf) ## well all geoids are unique; 51 states what happened to some
head(all.sf)

all.sf$geoid <- all.sf$GEOID %>% as.character
all.sf$geoid %>% unique %>% length

##  2) Read in the data table file now
## 2016
us2016.df <- 'C:/Users/Meng Le Zheng/Google Drive/Data for Meng/2016data.csv' %>%
  read.csv

us2016.df$geoid <- paste(us2016.df$state %>% formatC(width = 2, flag = 0, format = 'd'), 
                         us2016.df$county %>% formatC(width = 3, flag = 0, format = 'd'), 
                         us2016.df$tract %>% formatC(width = 6, flag = 0, format = 'd'),
                         sep = '')

us2016.sf <- all.sf %>% merge(us2016.df, by = 'geoid') # Perfect fit!
missing.2016 <- us2016.df %>% subset(!(us2016.df$geoid %in% all.sf$geoid))
table(missing.2016$MSA) # tiny parts missing
##  Right well we have random bits missing

