#====================================================================

#==============================================================================
meansd = function(x)
{
  m = mean(x)
  s = sd(x)
  ans = c(m-s, m, m+s)
  names(ans) = c("Mean-SD","Mean","Mean+SD")
  return(ans)
}
#==============================================================================
meansdplot = function (x, ...) UseMethod("meansdplot")
#------------------------------------------------------------------------------
meansdplot.default = function(x, y, headwidth=0.25, lwd=1.5, ylim=NULL, xlim=NULL, 
                              xaxt="s",xlab="x", ylab="y", col="black", add=FALSE,
                              pch=1, bg="black",cex=1, arrow=TRUE, sdcol=col,...)
{
  #debug
  #x = rep(5:1, each=10)
  #y = rnorm(50, mean=x, sd=1)
  #headwidth=0.5;lwd=1.5;ylim=NULL;xlim=NULL
  
  #x=x.var;y=y.var
  
  if (class(x) %in% c("data.frame","matrix"))
    if(ncol(x)>1)
      x = apply(x, 1, paste, collapse=" ")
  
  rnames = as.character(unique(x))
  
  agr = aggregate(y, list(x), meansd)
  
  rownames(agr) = agr[,1]
  agr = agr[rnames,]
  
  labels = agr[,1]
  agr = agr[,2]

  xpos = 1:length(labels)
  
  if(is.null(ylim))
    ylim = c(min(agr[,1]),max(agr[,3]))
  if(is.null(xlim))
    xlim = range(xpos)+c(-0.5,0.5)

  if(!add)
  {
    plot(y=0, x=0,type="n", ylim = ylim, 
         xlim=xlim, xaxt="n", xlab=xlab, ylab=ylab,...)
    
    if(arrow)
      arrows(x0 = xpos, x1 = xpos, y0 = agr[,1], y1 = agr[,3], angle=90, 
             code=3, lwd=lwd, length=headwidth, lty=1, col=sdcol)
    else
    {
      points(x=xpos, y=agr[,1], lty=3, col=sdcol, type="l")
      points(x=xpos, y=agr[,3], lty=3, col=sdcol, type="l")
    
    }
    
    
    points(y=agr[,2], x=xpos, type="p", pch=pch, bg=bg, col=col, cex=cex, lwd=lwd,...)
    
    if(xaxt!="n")
      axis(side=1, at = xpos, labels = labels)  
  }
  else
  {
    if(arrow)
      arrows(x0 = xpos, x1 = xpos, y0 = agr[,1], y1 = agr[,3], angle=90, 
             code=3, lwd=lwd, length=headwidth, lty=1, col=sdcol)
    else
    {
      points(x=xpos, y=agr[,1], lty=3, col=sdcol, type="l")
      points(x=xpos, y=agr[,3], lty=3, col=sdcol, type="l")
    }
    
    points(y=agr[,2], x=xpos,type="p", pch=pch, bg=bg, col=col, cex=cex,lwd=lwd,...)
  }
  
  colnames(agr)= c("Mean-SD","Mean","Mean+SD")
  rownames(agr) = labels

  invisible(agr)
}
#------------------------------------------------------------------------------
meansdplot.formula = function(formula, data=NULL, headwidth=0.25, lwd=1.5, 
                              ylim=NULL,xlim=NULL, xaxt="s", xlab="x", 
                              ylab="y", col="black",add=FALSE, pch=1,
                              bg="black",cex=1,arrow=TRUE,sdcol=col,...)
{
  
  if (is.null(data))
    mf = model.frame(formula)
  else
    mf = model.frame(formula, data)
  
  if (ncol(mf)>2)
    x.var = apply(mf[,2:ncol(mf)], 1, paste, collapse=" ")
  else
    x.var = mf[,2]
  
  y.var= mf[,1]
  
  agr = meansdplot.default(x.var, y.var, headwidth, lwd, ylim, xlim, xaxt, 
                           xlab, ylab, col, add, pch, bg, cex,arrow,sdcol,...)
  invisible(agr)
  
  
}
#--------------------------------------------------------------------
meansdplot.matrix = function(x, data=NULL, headwidth=0.25, lwd=1.5, 
                             ylim=NULL,xlim=NULL, xaxt="s", xlab="x", 
                             ylab="y", col="black",add=FALSE, pch=1,
                             bg="black",cex=1,arrow=TRUE,sdcol=col,...)
{
  
  
  x = stack(data.frame(x))
  x.var = x[,2]
  y.var = x[,1]
  
  
  agr = meansdplot.default(x.var, y.var, headwidth, lwd, ylim, xlim, xaxt, 
                           xlab, ylab, col, add, pch, bg,cex,arrow,sdcol,...)
  invisible(agr)
  
}
#====================================================================
# 
# x = rep(2:6, each=10)
# y = rnorm(50, mean=x, sd=1)
# par(mfrow=c(1,3))
# meansdplot(x, y, main="Default")
# meansdplot(y~x, main="Formula")
# m = matrix(round(rnorm(25, mean=4, sd=1),2), ncol=5)
# meansdplot(m, main="Matrix", col="red", add=TRUE)
#====================================================================

