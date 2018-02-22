#====================================================================
invlogit = function(x)
{
  1/(1+exp(-x))
}
#====================================================================
lrbarplot = function(x, y, coefs = NULL, autofit = FALSE,
                     bcol="darkgrey", blwd = 6, llwd = 4, spacer = 1,
                     lcol="black", axis2.lab = "Number of observations", decimal.mark=".",...)
{
  #blwd = 6; llwd = 4; bcol="black";lcol="red";ylab2 = "Number of observations";
  
  #tabulating the numbers of observations
  tab = as.matrix(table(y, x))
  upper = (2+spacer)*max(tab)
  xtab = sort(unique(x))
    
  #original plot (lower bars)
  plot(x=xtab, y=tab[1,], type='h', lend=2, col=bcol,
       xlim=range(x), ylim=c(0,upper), lwd=blwd, yaxt="n", ...)
  
  #plot(x=xtab, y=tab[1,], type='h', lend=2, col=bcol,
  #     xlim=range(x), ylim=c(0,upper), lwd=blwd, yaxt="n")
  
  #adding the upper bars
  segments(x0 = xtab, x1 = xtab, y0 = upper, y1=upper-tab[2,],
         col=bcol, lwd=6, lend=2)

  #1st y axis, probability of event
  axis(side=2, at = pretty(0:1)*upper, 
       labels=format(pretty(0:1), digits = 1, decimal.mark = decimal.mark))
  
  #2nd y axis, number of observations
  ats = pretty(tab, n = 3)
  axis(side=4, at = c(ats, upper-ats), labels = c(ats,ats),
lwd=par()$lwd)

  #2nd y label
  opar = par()
  originalLas = opar$las
  par(las = 3)
  mtext(side=4, text = axis2.lab, line = opar$mgp[1], cex=opar$cex.lab)
  par(las = originalLas)
  
  #adding the predicted line, or not.
  if(!is.null(coefs[1]))
  {
    par(new=TRUE)
    curve(invlogit(coefs[1]+coefs[2]*x), col=lcol, lwd=llwd,
          xlim=range(x), ylim=c(0,1), xlab="", ylab="", xaxt="n", yaxt="n")
  }
  else if (autofit)
  {
    lr = glm(y~x, family=binomial)
    coefs = coef(lr)
    par(new=TRUE)
    curve(invlogit(coefs[1]+coefs[2]*x), col=lcol, lwd=llwd,
          xlim=range(x), ylim=c(0,1), xlab="", ylab="", xaxt="n",
          yaxt="n")
  }
  invisible(cbind(x = xtab, b0 = tab[1,], b1 = tab[2,]))
}
#====================================================================
#example
#x = round(rnorm(2000),1)
#y = rbinom(2000, 1, invlogit(1+3*x))
#library(extrafont)
#png("lrbarplot.png", width = 5*4, height = 5*3, units = "cm", res=450)
#par(las=1, bty="l", mar=c(6,6,2,6), family="Arial", cex.lab=2)

#lrbarplot(x=x, y=y, autofit = TRUE, bcol = "grey30", lcol="red",
#          xlab="Standardized centrality", ylab="Jump probability",
#          axis2.lab = "Number of observations", spacer = 1)
#dev.off()

#====================================================================