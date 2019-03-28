##  Stitching together shapefiles and merging data
## start: 20/3/2018
##  Right so we have data tables and we have shapes files to sorto ut.

##  First we have many many shap files to stitch together ----
##  They all have a smiliar naming convention of
##  trxx_d00 where xx is a 2 digit id for state
##  So we look and read in folder for load in files with xx from 1 to 56
##  Some numbers are missing
format.nums <- 1:56 %>% formatC(width = 2, flag = 0, format = 'd')
folder.nms <- paste('tr', format.nums, '_d00', sep = '')

sf.list <- list(NULL)

for (i in 1:56){
  ## We use try so if there is an error we just skip
  try(
    sf.list[[i]] <- st_read(dsn = tracts.path, 
                            layer = folder.nms[i])
  )
}


missing <- lapply(sf.list, is.null) %>% unlist %>% which ## let's see which ids are
#missing
missing # 3, 7, 14, 43, 52 missing

##  Check to see their variable names are the same
nms <- lapply(sf.list, names) %>% unlist
table(nms) #all same names

##  Not all have the same amount of cols so we have to restrict 
sf.list <- sf.list[-missing] #drop missing index numbers
for (i in 1:length(sf.list)){
  sf.list[[i]] <- 
    sf.list[[i]] %>%
    select(c('NAME', 'TRACT', 'COUNTY', 'STATE'))
}


all.sf <- 
  do.call(rbind, sf.list)


summary(all.sf) ## well all geoids are unique; 51 states what happened to some
head(all.sf)
##  There is a geo id variablae made up of state, country, tract and some padding
##  str_pad pads out the string.
##  Another geoid2 variable is just combo of state and county
all.sf<- 
  all.sf %>%
  mutate(geoid = paste0(STATE, 
                       COUNTY,
                       TRACT %>% 
                         str_pad(width = 6, side = 'right', pad = '0'))) %>%
  mutate(geoid2 = paste0(STATE, 
                     COUNTY))

##  1a) Taking duplicated geoid entries out ---- 
##  First there are duplicated geoid entries and this is due to non-contiguous
##  tracts (i.e. includes inland and islands). My solution is to keep the largest
##  polygon in the tract (hoping its main land)

##  Proof (don't run)
# all.sf$geoid %>% duplicated %>% sum ## okay so some geoids are duplicated
# all.sf %>% duplicated %>% sum # .. but no perfect duplciation
# duped_geoid <- all.sf$geoid[ all.sf$geoid %>% duplicated ]
# duped_sf <- all.sf %>% filter(geoid %in% duped_geoid) %>% arrange(geoid)
# duped_sf ## so exactly same id
# duped_sf$geoid %>% table ## mutiple reps
# qtm(duped_sf[1:2, ])
# qtm(duped_sf[3:7, ]) 


## Solution -- take largest polygon for each tract
# duped_geoid <- all.sf$geoid[ all.sf$geoid %>% duplicated ]
# duped_sf <- all.sf %>% filter(geoid %in% duped_geoid)

all.sf <-
  all.sf %>% 
  mutate(area = st_area(all.sf)) %>%
  group_by(geoid) %>%
  mutate(area_rank = rank(-1 * area)) %>% ##rank by area
  ungroup %>%
  filter(area_rank == 1) %>%
  select.sf(-area, - area_rank)

all.sf$geoid %>% duplicated %>% sum

##  2) Read in the data table file now
## 2000
us2000.df <- 
  google.drive.path %>% 
  file.path('2000data.csv') %>%
  read.csv




##  Rory's geoid (not geoid2) variable hasn't quite saved properly, we need to reconstruct with
##  leading zeroes

us2000.df <-
  us2000.df %>%
  mutate(geoid = paste0(state %>% formatC(width = 2, flag = 0, format = 'd'), 
                        county %>% formatC(width = 3, flag = 0, format = 'd'),
                        tract %>% formatC(width = 6, flag = 0, format = 'd'))) %>%
  mutate(geoid2 = paste0(state %>% formatC(width = 2, flag = 0, format = 'd'), 
                         county %>% formatC(width = 3, flag = 0, format = 'd')))


table(us2000.df$geoid  %in% all.sf$geoid) #38 do not match
us2000.df %>% filter(!(geoid  %in% all.sf$geoid) ) ## No population hence reason

# Checkign unique entries
us2000.df %>% duplicated %>% sum
us2000.df$geoid %>% duplicated %>% sum

##  I'm sure it's for a good reason --- no wait no!
all.sf$geoid 


us2000.sf <- 
  all.sf %>% right_join(us2000.df, by = 'geoid') # right join -- only include where we have data
##  So some duplicated but hmm

missing.2000 <- us2000.df %>% subset(!(us2000.df$geoid %in% all.sf$geoid))
table(missing.2000$MSA) # so tiny the amount of non matches per MSA

##  2009
us2009.df <- 
  google.drive.path %>% 
  file.path('2009data.csv') %>%
  read.csv

us2009.df <- 
  us2009.df %>%
  mutate(geoid = paste0(state %>% formatC(width = 2, flag = 0, format = 'd'), 
                        county %>% formatC(width = 3, flag = 0, format = 'd'),
                        tract %>% formatC(width = 6, flag = 0, format = 'd'))) %>%
  mutate(geoid2 = paste0(state %>% formatC(width = 2, flag = 0, format = 'd'), 
                         county %>% formatC(width = 3, flag = 0, format = 'd')))



us2009.sf <- all.sf %>% right_join(us2009.df, by = 'geoid') #again almost all matches
table(us2009.df$geoid  %in% all.sf$geoid) #74 do not match
missing.2009 <- us2009.df %>% subset(!(us2009.df$geoid %in% all.sf$geoid))
table(missing.2009$state)
table(us2009.df$MSA)

## So in conclusion 2009 is a perfect match (Wait why)

##  Save the data for an example:
us2000.sf %>% saveRDS('Example/us2000_sf.RDS')



