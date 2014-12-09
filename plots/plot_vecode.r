

library(myr)



## Load data
if (TRUE) {
    veg = read.table("vegout.txt",header=TRUE)

}


plot_vecode <- function(dat,xnm="tann",leg=FALSE)
{
    col = c("darkgreen","green","salmon2",1)
    bg  = alpha(col,70)
    pch = c(21,21,21,4)
    cex = c(0.5,0.5,0.5,0.3)

    plot(range(dat[[xnm]]),ylim,type="n",ann=FALSE)
    mtext(side=1,line=1.6,xnm,cex=0.9)
    mtext(side=2,line=1.9,las=0,"Fraction",cex=0.9)

    points(dat[[xnm]],dat$needles,col=col[4],pch=pch[4],cex=cex[4])

    points(dat[[xnm]],dat$forest,col=col[1],bg=bg[1],pch=pch[1],cex=cex[1])
    points(dat[[xnm]],dat$grass, col=col[2],bg=bg[2],pch=pch[2],cex=cex[2])
    points(dat[[xnm]],dat$desert,col=col[3],bg=bg[3],pch=pch[3],cex=cex[3])

    
    if (leg) legend("topright",bg=alpha("white",90),inset=0.01,col=col,pt.bg=bg,pch=pch,pt.cex=cex,
                    c("forest","grass","desert","needle ratio"))

}

## Plots
if (TRUE) {

    
    ylim = range(veg$forest,veg$grass,veg$desert,veg$needles)

    myfigure("./","vecode_summary",date=TRUE,asp=1.2,pointsize=12,type="png")

    par(mfrow=c(2,2),plt=c(0.15,0.95,0.18,0.95))

    plot_vecode(veg,"tann")
    plot_vecode(veg,"pann")
    plot_vecode(veg,"pann5")
    plot_vecode(veg,"gdd",leg=TRUE)
    # plot_vecode(veg,"pco2")
    
    graphics.off()
}