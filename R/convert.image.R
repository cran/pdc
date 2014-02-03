convert.image<- function(imgrgb, threshold=.5) {

img <- ifelse((imgrgb[,,1]+imgrgb[,,2]+imgrgb[,,3])/3>threshold,1,0)
return(img);

}
