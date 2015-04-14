library(dplyr)
library(TSP)
library(ggplot2)
library(GCodeROpt)
file = '~/BTSync/TheoPersonal/CNC/dustBoot/dustboot_sheet2.nc'
file = '~/BTSync/TheoPersonal/CNC/rbpiBox//Box_2x4x8.nc'
file = '~/BTSync/TheoPersonal/CNC/laserMount/ElCheapo Laser Mount Laser.nc'
#file = '~/BTSync/TheoPersonal/CNC/MaliaAndAri/MaliaAndAri.nc'
file = '~/BTSync/TheoPersonal/CNC/fractalTree//fractalTree.nc'
file = '~/BTSync/TheoPersonal/CNC/fractalTree//fractalTreeFiled.nc'
file = '~/BTSync/TheoPersonal/CNC/fractalTree//Fractal Tree for Arthur.nc'
file= '~/BTSync/TheoPersonal/CNC/fractalTree/inscription.nc'


fileIn = readLines(file)

### router settings
laser=F
oldFeedRate = "5.0"
newFeedRate = "12.0"
###

### laser Settings - 1/8inch mdf
laser=T
cutReps = 11
layers = 6
cutDepth = .125

oldFeedRate = "5.0"
newFeedRate = "5.0"
### - laser

### laser Settings - paper (or engrave)
laser=T
cutReps = 1
layers = 1
cutDepth = .125

oldFeedRate = "5.0"
newFeedRate = "12.0"
### - laser


  
#divide the file into G0 initiated paths (where the drill moves to and then starts)
g0 = grep("G0 ", fileIn)

diffg0 = diff(g0)

move = rep("start", g0[1]-1)
for(i in 1:length(g0)-1){
  move = c(move, rep(g0[i], diffg0[i]))
}
move = c(move, "end")
fileData = data.frame(g0=fileIn, class=move)

fileData = fileData %>%
  group_by(class) %>%
  mutate(order=1:length(class))


#this is where we can easily create reverse paths

optPaths = data.frame(g0=fileIn[g0[1:(length(g0)-1)]], class=g0[1:(length(g0)-1)])
optPaths$x = as.numeric(gsub('[ ][Y][0-9].*$', '', gsub('^G0 X','',optPaths$g0)))
optPaths$y = as.numeric(gsub('^G0 X.*[ ]Y', '', optPaths$g0))
optPaths$z = as.numeric(gsub(" F.*$", '',
                             gsub("G1 Z", "", fileIn[g0[1:(length(g0)-1)]+1])))
  
fileData$x = as.numeric(gsub('*.Z.*', 'NA', 
                             gsub('[ ][Y][0-9].*$', '', 
                                  gsub('^G[0-9] X','',fileData$g0))))
fileData$y = as.numeric(gsub('*.Z.*', 'NA', 
                             gsub('^G[0-9] X.*[ ]Y', '', 
                              gsub('F.*$', '',fileData$g0))))


distOpt = dist(optPaths[,c("x","y","z")])

tsp = TSP(distOpt)

optPathsOrd = data.frame(optPaths[ as.integer(solve_TSP(tsp,"2-opt")), ], sort=1:nrow(optPaths))
optPathsOrdBase = optPathsOrd
#if it's a laser cut, and cutReps >1 then repeat each pathSection cutReps number of times 
#(and change everyother class to a Rev class so that it just goes in opposite order) (so that it will cut out of the sheet)
if(laser & cutReps>1){
 for(i in 2:cutReps){
   bindDF = optPathsOrdBase
   if(i%%2==0){
     bindDF$class = paste0(bindDF$class, "Rev")
   }
     bindDF$sort = bindDF$sort+(i-1)/cutReps
     optPathsOrd = rbind(optPathsOrd, bindDF)
   
   #now just need to get a filePaths with reverse paths and classes
 }
}


fileData = rbind(fileData, fileData %>% group_by(class) %>% mutate(order=rev(order)) %>% group_by() %>%
                                                                   mutate(class=paste0(class, "Rev")))

optMerge = merge(optPathsOrd, fileData, by="class")
optMerge = optMerge[with(optMerge, order(-z,sort, class,order)), ]

ggplot(na.omit(optMerge), aes(x=x.y, y=as.numeric(y.y), color=factor(sort)))+geom_path()
#plot(optPaths$x, optPaths$y,type="line")
#plot(optPathsOrd$x, optPathsOrd$y,type = "line")
#with(na.omit(fileData), plot(x, y, type="line"))
# with(na.omit(optMerge), plot(x.y, y.y, type="line"))


#now just reassemble the gcode and put it in the right order

gCode = with(optMerge, data.frame(g0=g0.y, class=class))
gCode = rbind(data.frame(g0=subset(fileData, class=="start")$g0, class="start"), gCode)
gCode= rbind(gCode, data.frame(g0=fileData$g0[nrow(fileData)], class="end"))

#change out the feedrates
gCode$g0=gsub(paste('F', oldFeedRate, sep=''), paste('F',newFeedRate, sep=''), gCode$g0)

#if laser then remove any of the up and down g1Z movements
if(laser){
    g0Code = (gCode$g0[-grep("G1 Z", gCode$g0)])
    if(layers>1){
      for(i in 2:layers){
        g0Code = c(g0Code, paste0("G1 Z", -(i-1)/layers*cutDepth," F9.0"))
        g0Code = c(g0Code, gCode$g0[-grep("G1 Z", gCode$g0)]) 
      }
      g0Code = c(g0Code, paste0("G1 Z0.0 F9.0"))
    }
    
   writeLines(as.character(g0Code), paste(file, ".opt.nc", sep=''))
   print(timeToComplete(g0Code)/c(60,12))
   
   
}else{
  writeLines(as.character(gCode$g0), paste(file, ".opt.nc", sep=''))  
}

