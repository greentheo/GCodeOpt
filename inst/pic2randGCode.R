#### picture to G-code
library(dplyr)
library(TSP)
library(ggplot2)
library(GCodeROpt)
library(readbitmap)
library(EBImage)
library(DEoptim)

file = '~/BTSync/TheoPersonal/Photos/maliaAndAriCropped.jpg'

xrsz = .5
yrsz = xrsz

xmax = 11
ymax = 11

pcnt = .1
threshold = 50/100
img=readImage(file)
pic = imageData(flip(resize(img,w = xrsz*ncol(img), h = yrsz*nrow(img))))

picBW = .21*pic[,,1]+.72*pic[,,2] +.07*pic[,,3]
picDF = data.frame(y=sort(rep(1:ncol(picBW), nrow(picBW))), 
                   x=rep((1:nrow(picBW)), ncol(picBW)), 
                   bw=as.numeric(picBW))

ggplot(subset(picDF, bw<threshold), aes(x=x, y=y, fill=bw))+geom_raster()

picDF = subset(picDF, bw<quantile(bw,threshold))
picDFSamp = picDF[sample(1:nrow(picDF), .4*nrow(picDF),prob = 1/2/(picDF$bw+.001)),]

ggplot(picDFSamp, aes(x=x, y=y, fill=bw))+geom_raster()


#break it up into clusters of size 1k or less to keep the tsp problem within a good memory range
#use DEoptim to find the centers so that each cluster is 1k or so
optFun = function(centerVec, picDFSamp){
  centers = data.frame(x=centerVec[1:(length(centerVec)/2)], 
                       y=centerVec[(length(centerVec)/2+1):length(centerVec)])
  clusts = try(kmeans(x = picDFSamp[,c("x","y")], centers = centers))
  if(class(clusts)=="try-error"){
    return(length(centerVec))
  }else{
    return(sum(table(clusts$cluster)>1000))  
  }
  
}
centerLow = rep(0,(ceiling(nrow(picDFSamp)/1000)+1)*2)
centerHigh = rep(max(picDFSamp),(ceiling(nrow(picDFSamp)/1000)+1)*2)
DEoptim(optFun, lower = centerLow, upper = centerHigh,picDFSamp=picDFSamp)

picDFSamp[["clust"]] = clusts$cluster

ggplot(picDFSamp, aes(x=x, y=y, fill=factor(clust)))+geom_raster()

for(i in 1:nrow(clusts$centers)){
  distOpt = dist(subset(picDFSamp, clust=i)[,c("x","y")])
    
  tsp = TSP(distOpt)
  
  picDFSampOrd = data.frame(picDFSamp[ as.integer(solve_TSP(tsp,"nn")), ], sort=1:nrow(picDFSamp))  
  ggplot(picDFSampOrd, aes(x=x, y=y))+geom_path()
}




