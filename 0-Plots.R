

plot0 <- function(adt,ventt,intime,save = F){
  # plotr <- c(0,difftime(max(ad$IN_DTTM),min(ad$IN_DTTM),unit = "d"))
  if(save) png("Plot0.png",height = 600,width = 1200)
  ad.time <- difftime(intime,min(intime),unit = "d")
  vent.time <- difftime(ventt,min(intime),unit = "d")
  adt <- head(adt,-1)
  rng <- c(min(ad.time),max(ad.time))
  
  
  plot(rng,c(-0.5,0.5),ylim = c(-0.5,0.5),pch = "",ylab = "",yaxt = "n",xlab = "Days since Admission")
  
  rect(xleft = tail(ad.time,-1),xright = head(ad.time,-1),ybottom = 0.-0.2,ytop = 0.2,border = NA,col = if_else(grepl("ICU",adt),
                                                                                                                rgb(0,0,1,alpha = 0.75),
                                                                                                                rgb(0,0,0,alpha = 0.75)))
  rect(xleft = vent.time,xright = c(tail(vent.time,-1),max(vent.time)+1/24),ybottom = -0.1,ytop = 0.1,border = NA,col = rgb(1,0,0,alpha = 0.75))
  
  abline(h = 0,lwd = 2)
  legend("bottomright",legend = c("Hospital Stay","ICU Stay","Ventilation"),pch = 15,col = c(rgb(0,0,1,alpha = 0.75),rgb(0,0,0,alpha = 0.75),rgb(1,0,0,alpha = 0.75)))
  if(save) dev.off()
}