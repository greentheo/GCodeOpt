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