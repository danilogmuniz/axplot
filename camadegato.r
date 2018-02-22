#====================================================================
#Funcao que faz o grafico de cama de gato, com os pontos "pareados"
#colocados lado a lado e conectados por uma linha

#versão curso de campo 2017

#Axpira
#====================================================================


camadegato = function(m, col="black", lcol="black", lty=1, llwd=1,
                      xlab="", ylab="", bg="white", cex=1,
                      pch=19, xspc = 0.25, labels = NULL, xaxt="s",...)
{
  
  if(!is.null(labels))
    colnames(m) = labels
  
  #se a matriz n?o tem nomes de colunas, nomeia apenas com letras
  if(length(colnames(m))==0)
    colnames(m) = LETTERS[1:ncol(m)]

  #coloca em formato de data.frame pro stripchart functionar
  #mdata = stack(data.frame(m))
  #criando um plot vazio
  plot(0, type = "n", xlim=c(1-xspc, ncol(m)+xspc),xlab=xlab, ylab=ylab, 
       ylim = range(pretty(m)),xaxt="n",...)
  #plot(0, type = "n", xlim=c(1-xspc, ncol(m)+xspc),xlab=xlab, ylab=ylab, 
  #     ylim = range(pretty(m)),xaxt="n")
  
  #eixo x
  if(xaxt!="n")
  
    axis(side=1, at = 1:ncol(m), labels = colnames(m))
  
  #aplicando a regra da ciclagem nos mais de mil parametros graficos
  lcol = rep(lcol, length.out = nrow(m))
  lty = rep(lty, length.out = nrow(m))
  llwd = rep(llwd, length.out = nrow(m))
  col = rep(col, length.out = nrow(m))
  pch = rep(pch, length.out = nrow(m))
  cex = rep(cex, length.out = nrow(m))
  
  #agora sim, plotando as linhas!
  for(i in 1:(ncol(m)-1))
    segments(x0=i, x1=i+1, y0=m[,i], y1=m[,i+1], col=lcol, lty=lty, lwd=llwd)
  
  #plotando os pontos
  for(i in 1:(ncol(m)))
    points(x=rep(i, nrow(m)), y=m[,i], col=col, pch=pch, bg=bg, cex=cex)
  
}
#====================================================================
#exemplo

#antes = rnorm(10, 2, 1)
#depois = antes + rnorm(10,1,1)

#tiff("camadegato.tiff", width = 4*300, height= 3.5*300, res=300, units = "px")
#par(las=1, bty="l", lwd=2, cex=1, cex.axis=2, cex.lab=2, mar=c(6,6,2,2), cex=0.75)
#camadegato(cbind(antes, depois), pch=21, bg="grey", xlab="Tratamento",
#           ylab="Comprimento de y", cex=2)
#dev.off()
#====================================================================