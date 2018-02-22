#==========================================================
if(!require(grDevices))
  warning("Esta fun??o requer o pacote [grDevices].\nInstale o pacote antes de usar a fun??o.")
if(!require(scales))
  warning("Esta fun??o requer o pacote [scales].\nInstale o pacote antes de usar a fun??o.")
#==========================================================
bubbleplot = function (x, ...) UseMethod("bubbleplot")
#==========================================================
bubbleplot.default = function(x, y, col="black", bg="grey",
                              pch=19, alpha=0.5, mcex=5, add=FALSE, ...)
{
  cols = c(col, bg)
  cols = scales::alpha(cols, alpha=alpha)
  
  dat = data.frame(grDevices::xyTable(x,y))
 
  dat$radius = sqrt(dat[,3])
  maxr = max(dat$radius)
  dat$radius = mcex * dat$radius/maxr
  
  n = pretty(dat[,3])
  if(0 %in% n)
    n[n == 0] = 1
  
  cexs = mcex * sqrt(n)/maxr
  
  if(!add)
    plot(dat$x, dat$y, cex=dat$radius, col=cols[1], bg=cols[2], pch=pch,...)
  else
    points(dat$x, dat$y, cex=dat$radius, col=cols[1], bg=cols[2], pch=pch,...)
  
  invisible(list(n=n, cex=cexs))
}
#==========================================================
bubbleplot.formula = function(formula, col="black", bg="grey",
                              pch=19, alpha=0.5, mcex=5, add=FALSE,
                              data=NULL,...)
{
  if (is.null(data))
    mf = model.frame(formula)
  else
    mf = model.frame(formula, data)
  
  ans = list()
  
  ans = bubbleplot.default(x = mf[,2], y=mf[,1], col = col, 
                           bg=bg, pch=pch, alpha=alpha, mcex=mcex, add=add, ...)

  invisible(ans)  
} 
#==========================================================
#Example
#x11()
#par(mfrow=c(1,2))
#x = round(rnorm(100, 10, 2))
#z = round(rnorm(100, 10, 2))
#y = round(2*x-z + sample((1:5)/2, 100, replace=TRUE))
#bubbleplot(y~x+z, alpha=c(1,0.5))
#==========================================================
