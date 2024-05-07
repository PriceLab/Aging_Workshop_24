vc_palette <- function(palette, ramp=TRUE){
   if(length(palette)> 1){
      clrs <- palette
   }else{
      if( palette %in% c("RdGn", "RdGr")){
            clrs <- c(rev(RColorBrewer::brewer.pal(7,"Greens")), "white", RColorBrewer::brewer.pal(7,"Reds"))
       }else if(palette %in% c("BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu","RdYlGn","Spectral")){
          clrs <- rev( RColorBrewer::brewer.pal(11, palette))
       }else{
          clrs <- c("white", RColorBrewer::brewer.pal(9, palette))    
       }
    }
    if(ramp) clrs <- grDevices::colorRampPalette(clrs)(255)
    clrs
}



showSet <- function(Z, analytes, rcolors=NULL, ccolors=NULL, ...) {
    if (is.null(rcolors)) {
        if (is.null(ccolors)) {
            H <- heatmap(Z[analytes,analytes], col=RWB, symm=TRUE, zlim=c(-1,1)*1.01, na.rm = TRUE, ...)
        } else {
            H <- heatmap(Z[analytes,analytes], col=RWB, symm=TRUE, zlim=c(-1,1)*1.01, na.rm = TRUE,
                         ColSideColors = ccolors[analytes], ...)
        }
    } else if (is.null(ccolors)) {
        H <- heatmap(Z[analytes,analytes], col=RWB, symm=TRUE, zlim=c(-1,1)*1.01, na.rm = TRUE,
                     RowSideColors = rcolors[analytes], ...)
    } else {
        H <- heatmap(Z[analytes,analytes], col=RWB, symm=TRUE, zlim=c(-1,1)*1.01, na.rm = TRUE,
                     ColSideColors = ccolors[analytes],RowSideColors = rcolors[analytes], ...)
    }
}