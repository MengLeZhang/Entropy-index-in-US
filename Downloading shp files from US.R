##  Mass download of shp files from the us gov website
##  Start: 21/3/2018


dest.path <- 'C:/Users/Meng Le Zheng/Downloads/us census shape/'
numbers <- formatC(1:56, width = 2, flag = 0, format = 'd')
numbers <- numbers[ - 1 * c(3, 7, 14, 43, 52)] #omit not use numbers
##  2016 shp files ......
url16.pt1 <- 'ftp://ftp2.census.gov/geo/tiger/TIGER2016/TRACT/tl_2016_'
url16.pt2 <- '_tract.zip'
url16 <- paste(url16.pt1, numbers, url16.pt2, sep = '')

dest.file16 <- paste(dest.path, 'census 16 ', numbers, '.zip', sep = '')

for (i in 1:56){
  try(download.file(url16[i], destfile = dest.file16[i]))
}
download.file(url16, destfile = dest.file16)

#10, 11, 36, 44, 53 are missing

##  2000 files...

url00.pt1 <- 'https://www2.census.gov/geo/tiger/PREVGENZ/tr/tr00shp/tr'
url00.pt2 <- '_d00_shp.zip'
url00 <- paste(url09.pt1, numbers, url09.pt2, sep = '')

dest.file00 <- paste(dest.path, 'census 00 ', numbers, '.zip', sep = '')

miss00 <- c(NULL)
for (i in 1:56){
  ## if there is an error it will save to the miss vector
  miss00[i] <- tryCatch(download.file(url00[i], destfile = dest.file00[i]), 
           error = function(e){return(dest.file00[i])})

}
