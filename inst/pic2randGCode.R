#### picture to G-code
library(dplyr)
library(TSP)
library(ggplot2)
library(GCodeROpt)
library(readbitmap)
library(EBImage)
library(DEoptim)

file = '~/BTSync/TheoPersonal/Photos/maliaAndAriCropped.jpg'
file= '~/BTSync/TheoPersonal/CNC/miscLaserEtch/spring_snow_on_the_flatirons_l.jpg'

xrsz = .5
yrsz = xrsz

xmax = 11
ymax = 11

pcnt = .1
threshold = 30/100
img=readImage(file)
pic = imageData(flip(resize(img,w = xrsz*ncol(img), h = yrsz*nrow(img))))

picBW = .21*pic[,,1]+.72*pic[,,2] +.07*pic[,,3]
picDF = data.frame(y=sort(rep(1:ncol(picBW), nrow(picBW))), 
                   x=rep((1:nrow(picBW)), ncol(picBW)), 
                   bw=as.numeric(picBW))

ggplot(subset(picDF, bw<threshold), aes(x=x, y=y, fill=bw))+geom_raster()

picDF = subset(picDF, bw<quantile(bw,threshold))
sampP = .5
picDFSamp = picDF[sample(1:nrow(picDF), sampP*nrow(picDF),prob = 1/2/(picDF$bw+.001)),]

ggplot(picDFSamp, aes(x=x, y=y, fill=bw))+geom_raster()


#break it up into clusters of size 1k or less to keep the tsp problem within a good memory range
#use DEoptim to find the centers so that each cluster is 1k or so
optFun = function(centerVec, picDFSamp){
  centers = data.frame(x=centerVec[1:(length(centerVec)/2)], 
                       y=centerVec[(length(centerVec)/2+1):length(centerVec)],
                       clust=1:(length(centerVec)/2))
  
  clusts = picDFSamp %>% 
    group_by(x, y) %>%
    summarize(clust = which.min(sqrt((x-centers$x)^2+(y-centers$y)^2)))
              #,dist = min(sqrt((x-centers$x)^2+(y-centers$y)^2)))
  
  
  #browser()
  #clusts = try(kmeans(x = picDFSamp[,c("x","y")], centers = centers))
#   if(class(clusts)=="try-error"){
#     return(length(centerVec))
#   }else{
    score = -length(unique(clusts$clust))/(length(centerVec)/2)+
            sd(table(clusts$clust))
    return(score)  
#   }
  
}
centerLow = rep(0,(ceiling(nrow(picDFSamp)/1000)+1)*2)
centerHigh = rep(max(picDFSamp),(ceiling(nrow(picDFSamp)/1000)+1)*2)

optFun(runif(ceiling(nrow(picDFSamp)/1000)*2, 0, max(picDFSamp)), picDFSamp)

#create an initial population based on kMeans to make it go faster
iPop = kmeans(picDFSamp, centers = ceiling(nrow(picDFSamp)/1000)+1)
initalPop = iPop$centers

centers = data.frame(x=iPop$centers[,"x"], y=iPop$centers[,"y"],clust=1:nrow(centers))

optSol = DEoptim(optFun, lower = centerLow, upper = centerHigh,picDFSamp=picDFSamp,
        control = DEoptim.control(VTR = 0,NP = 200,itermax = 50))

centers = data.frame(x=optSol$optim$bestmem[1:(length(optSol$optim$bestmem)/2)], 
                     y=optSol$optim$bestmem[(length(optSol$optim$bestmem)/2+1):length(optSol$optim$bestmem)],
                     clust=1:(length(optSol$optim$bestmem)/2))
optFun(optSol$optim$bestmem, picDFSamp) 

clusts = picDFSamp %>% 
  group_by(x, y) %>%
  summarize(clust = which.min(sqrt((x-centers$x)^2+(y-centers$y)^2)))

#picDFSamp[["clust"]] = clusts$clust

ggplot(clusts, aes(x=x, y=y, fill=factor(clust)))+geom_raster()+
  geom_point(data = centers, aes(x=x, y=y,size=5))

sum(table(clusts$clust)
 
df = data.frame(x=0, y=0, clust=0, sort=0)    

#for(i in 1:length(unique(clusts$clust))){
i=1
while(i<=length(unique(clusts$clust)) ){
  # find the closest point to the last point on df...
  # work on that cluster next
  subClust=subset(clusts, !clust %in% df$clust)
  iClust = subClust$clust[which.min(sqrt((df$x[nrow(df)]-subClust$x)^2
                            +(df$y[nrow(df)]-subClust$y)^2 ) )]
  
  subClust = subset(clusts, clust==iClust)
  distOpt = dist(subClust[,c("x","y")])
    
  tsp = TSP(distOpt)
  cat(i,iClust, '\n')
  opt=data.frame(subClust[ as.integer(solve_TSP(tsp,"two_opt")), c("x","y", "clust")], 
                 sort=nrow(df):(nrow(df)-1+nrow(subClust)))
  df = rbind(df, opt)  
  i = i+1
}

ggplot(df, aes(x=x, y=y, color=factor(clust)))+geom_path()
ggplot(df, aes(x=x, y=y))+geom_path()
ggplot(df, aes(x=x, y=y, group=factor(clust)))+geom_path()



#get gcode and write it to file
gcode = XYtoGCode(df, maxy = ymax, maxx = xmax, feedRate = 12)
writeLines(gcode,con = paste0(file, '.nc'))

timeToComplete(gcode)/c(60,12)
