#' A function to take in gCode and return the distance and time it will take to traverse
#' @export
timeToComplete = function(g0Code, feedRate=120){
  x = c(0,as.numeric(gsub('*.Z.*', 'NA', 
                                   gsub('[ ][Y][0-9].*$', '', 
                                        gsub('^G[0-9] X','',g0Code)))))
  x=zoo::na.locf(x)
  y = c(0,as.numeric(gsub('*.Z.*', 'NA', 
                               gsub('^G[0-9] X.*[ ]Y', '', 
                                    gsub('F.*$', '',g0Code)))))
  y=zoo::na.locf(y)
  z = c(0,as.numeric( gsub('^G[0-9] X.*[ ]Y', 'NA',
                      gsub('^G1 Z', '', 
                           gsub('F.*$', '',g0Code)))))
  z=zoo::na.locf(z)
  
  FeedRate = as.numeric(gsub('^G[029]', 'NA',  
                                  gsub('^G1.*F', '',g0Code)))
  FeedRate[is.na(FeedRate)] = feedRate
  
  dist = sqrt(
                (x[2:length(x)]-x[1:(length(x)-1)])^2+
                (y[2:length(x)]-y[1:(length(x)-1)])^2+
                (z[2:length(x)]-z[1:(length(x)-1)])^2
  )
  time = dist/FeedRate
  return(data.frame(time=sum(time), dist=sum(dist)))
}

#'  A function to take a set of XY coordinates and a "group" parameter
#'  And turn it into GCode
#'  @export
XYtoGCode = function(df, cluster="clust",maxy=1, maxx=1, feedRate=5){
  #start with a G0 to the first point
  ymult = maxy/max(df$y)
  xmult = maxx/max(df$x)
  
  gcode = sprintf("G0 X%f Y%f", df$x[1]*xmult, df$y[1]*ymult)
  for(i in 2:nrow(df)){
    if(df$clust[i]!=df$clust[i-1]){
      #then g0 move
      gcode = rbind(gcode, 
                    sprintf("G0 X%f Y%f", df$x[i]*xmult, df$y[i]*ymult))
    }else{
      #g1 traverse with feedrate
      
      gcode = rbind(gcode, 
                    sprintf("G1 X%f Y%f F%f", df$x[i]*xmult, df$y[i]*ymult, feedRate))
    }
  }
  #go back to the beginnning
  gcode = rbind(gcode,
                sprintf("G0 X%f Y%f", 0, 0))
  return(as.character(gcode))
}