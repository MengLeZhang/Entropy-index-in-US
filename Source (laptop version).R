##  Entropy project source (laptop):
##  This file is used to identify file paths, load libraries and for user functions

pkgs <- list('sf', 'tidyverse', 'tmap', 'stringr')
lapply(pkgs, library, character.only = T)

##  
tracts.path <-
  'C:/Users/Meng Le Zheng/Documents/US census shp/tracts 2000'

google.drive.path <-
  'C:/Users/Meng Le Zheng/Synced Google Drive/Google Drive/Data for Meng' 
