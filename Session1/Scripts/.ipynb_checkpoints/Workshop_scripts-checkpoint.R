palette255 <- function(palette, ramp=TRUE){
   if(length(palette)> 1){
      clrs <- palette
   }else{
      if( palette %in% c("RdGn", "RdGr")){
            clrs <- c(rev(RColorBrewer::brewer.pal(7,"Greens")), "white", RColorBrewer::brewer.pal(7,"Reds"))
       # OR reverse divergent color palette
       }else if(palette %in% c("BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu","RdYlGn","Spectral")){
          clrs <- rev( RColorBrewer::brewer.pal(11, palette))
       }else{
          clrs <- c("white", RColorBrewer::brewer.pal(9, palette))    
       }
    }
    if(ramp) clrs <- grDevices::colorRampPalette(clrs)(255)
    clrs
}
