
corner.plot = function(mcmcList, upper.panels=FALSE, filled.contours=FALSE){
    library(MASS)
    dev.new()
    #function for plotting histograms on the diagonal of the matrix plots
    panel.hist <- function(x, ...)
    {
        dens = density(x)
        # the default axis limits are not what we want,
        # so we overwrite them
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.1*max(dens$y)) )
        lines(dens,xlab="",ylab="",main="")
    }
    
    library(RColorBrewer)
    k <- 11
    my.cols <- rev(brewer.pal(k, "RdYlBu"))

    # function for plotting filled contour plots 
    panel.filled.contours <- function(x,y,...){
        cont <- kde2d(x, y, n=50)
        levels = seq(from = 0, to = max(cont$z), length.out = k)
        # filled.contour can't be put into a subplot, so we have to
        # use the lower-level .filled.contour here
        .filled.contour(cont$x,cont$y,cont$z, col=my.cols, levels=levels)
    }
    
    # function for plotting unfilled contour plots 
    panel.contours <- function(x,y,...){
        cont <- kde2d(x, y, n=50)
        contour(cont, col=my.cols, add=TRUE, drawlabels=FALSE)
    }
    lower.panel = panel.contours;
    if(filled.contours){
        lower.panel = panel.filled.contours
    }
    upper.panel = NULL
    if(upper.panels){
        upper.panel = lower.panel
    }
    # only use the first chain, and text.panel is text to display on the diagonals
    pairs(data.frame(mcmcList[[1]]), upper.panel = upper.panel, diag.panel = panel.hist, lower.panel = lower.panel, text.panel = NULL)
}
