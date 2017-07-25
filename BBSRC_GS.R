#' Ethiopia BBSRC GeoSurvey  
#' M. Walsh, July 2017

# install.packages(c("downloader","rgdal","arm"), dependencies=T)
suppressPackageStartupMessages({
  require(downloader)
  require(rgdal)
  require(arm)
})

# Data setup --------------------------------------------------------------
# Create a data folder in your current working directory
dir.create("GS_data", showWarnings=F)
setwd("./GS_data")

# Download GeoSurvey data
download("https://www.dropbox.com/s/9zjhbd76zyuud2n/ET_BBSRC_GS.csv?raw=1", "ET_BBSRC_GS.csv", mode="wb")
geos <- read.table("ET_BBSRC_GS.csv", header=T, sep=",")
geos <- geos[!duplicated(geos[,c("Box","Lat","Lon")]),] ## drop duplicates

# Project survey coords to grid CRS
geos.proj <- as.data.frame(project(cbind(geos$Lon, geos$Lat), "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"))
colnames(geos.proj) <- c("x","y") ## laea coordinates
geos <- cbind(geos, geos.proj)

# generate grid / GPS waypoint ID's
res.pixel <- 1000
xgid <- ceiling(abs(geos$x)/res.pixel)
ygid <- ceiling(abs(geos$y)/res.pixel)
gidx <- ifelse(geos$x<0, paste("W", xgid, sep=""), paste("E", xgid, sep=""))
gidy <- ifelse(geos$y<0, paste("S", ygid, sep=""), paste("N", ygid, sep=""))
GID <- paste(gidx, gidy, sep="-")
geos <- cbind(geos, GID)

# Data summaries ----------------------------------------------------------
# Cropland by box size (GeoSurvey box/quadrat size is in hectares)
cp <- glmer(CP~Box+(1|GID), family=binomial, data=geos) ## random intercept model
display(cp)

# Presence of buildings by box size
bp <- glmer(BP~Box+(1|GID), family=binomial, data=geos)
display(bp)

# Presence of woody vegetation cover >60% by box size
wp <- glmer(WP~Box+(1|GID), family=binomial, data=geos)
display(wp)
