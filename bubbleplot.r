#==========================================================
if(!require(grDevices))
  warning("This function requires the package [grDevices].\nInstalll it to continue.")
if(!require(scales))
  warning("This function requires the package [scales].\nInstalll it to continue.")

#==========================================================
bubbleplot = function (x, ...) UseMethod("bubbleplot")
#==========================================================
bubbleplot.default = function(x, y, col="black", bg="grey",
                              pch=19, alpha=0.5, mcex=5, add=FALSE, 
                              mobs = NULL, ...)
{
  cols = c(col, bg)
  cols = scales::alpha(cols, alpha=alpha)
  
  dat = data.frame(grDevices::xyTable(x,y))
 
  if(is.null(mobs))
    mobs = max(dat[,3])
  
  dat$radius = sqrt(dat[,3])
  maxr = sqrt(mobs)
  
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
                              pch=19, alpha=0.5, mcex=5, add=FALSE, mobs=NULL,
                              data=NULL,...)
{
  if (is.null(data))
    mf = model.frame(formula)
  else
    mf = model.frame(formula, data)
  
  ans = list()
  
  ans = bubbleplot.default(x = mf[,2], y=mf[,1], col = col, 
                           bg=bg, pch=pch, alpha=alpha, mcex=mcex, add=add, mobs = mobs, ...)

  invisible(ans)  
} 
#==========================================================
#Example
#x11()
#par(mfrow=c(1,2))
#x = round(rnorm(100, 10, 2))
#z = round(rnorm(100, 10, 2))
#y = round(2*x-z + sample((1:5)/2, 100, replace=TRUE))

#bubbleplot(y~x, alpha=0.5)
#bubbleplot(y=y, x=z, alpha=0.5)
#==========================================================