pt <- pt[order(pt$yes.h, decreasing = TRUE),]

offset= 0.01
s = 3 # scaling factor
mtext.title = 2.2*s
mtext.subtitle = 1.6*s
mtext.sign = 1.0*s
mtext.sign.emo = 1.3*s

showtext_opts(dpi = 96) #set the resolution: 96 is default


x.min=0
x.max=53
png ('./figures/F_vaccince_hesitancy_country_publication_horizontal.png', width=1280*s, height=905.5*s, res=96)

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(0,0,0,0), # size of the outer margins in lines of text
    mar=c(5,5,1,1), # number of lines of margin to be specified on the four sides of the plot 
    bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE,
    family='Quattrocento'
)

plot(NULL, ylim=c(1, dim(pt)[1]), xlim=c(x.min, x.max), yaxt = 'n', xaxt = 'n') 

title(xlab='Percentage of the country population', font=2, cex.lab=1.25)

axis (1, 
      line = 0, # position
      tck = -0.01,
      lwd = 1*s,
      col = 'grey', # the actual axis (line) 
      col.axis = dark.color, # colors of the actual labels
      cex.axis = 1, 
      font=2, # font type (bold)
      at=seq(x.min, 60, 10), # where to put labels  
      labels= paste0(seq(x.min, 60, 10), "%"), # text of labels 
      las=1 # orientation of the labels
)


abline(v=seq(0,100,10), col='grey', lwd=1*s)

rect (ybottom=(1:dim(pt)[1])-0.42, ytop=(1:dim(pt)[1]) + 0.42, xleft = rep(0,27), xright = pt$yes.h, col= 'darkgrey', border ='darkgrey', lwd=1.5*s)
rect (ybottom=(1:dim(pt)[1])-0.32, ytop=(1:dim(pt)[1]) + 0.32, xleft = rep(0,27), xright = pt$yes.never, col= 'black', border = 'black', lwd=1.5*s)


par(xpd = TRUE)
for (i in 1:dim(pt)[1]){
  text (paste0(round(pt$yes.h[i]),'%'), y = i, x = 0 + pt$yes.h[i] - 1.2, font=3, cex=0.60, col='white')
  text (paste0(round(pt$yes.never[i]),'%'), y = i, x = 0 + pt$yes.never[i] - 1.2, font=3, cex=0.60, col='white')
  
  text (countrycode(pt$country[i], origin='iso2c', destination='country.name'), y = i, x = -0, 
        pos=2, font=1, cex=1.2, col=ifelse(pt$region3[i]=='Eastern Europe', 'red', ifelse(pt$region3[i]=='Western Europe', 'blue', 'black')))
}  

text (y= 1, x=0.5, "vaccine refusal", col='white', cex=0.75, srt=0, pos=4)
text (y= 1, x=24, "vaccine hesitancy", col='white', cex=0.75, srt=0, pos=4)


rect(ybottom=19.4+7.5, ytop=20+7.5, xleft=48, xright=53, col= 'darkgrey', border ='darkgrey', lwd=1.5*s)
rect(ybottom=18.2+7.5, ytop=18.8+7.5, xleft=48, xright=53, col= 'black', border ='black', lwd=1.5*s)

text (y= 18.5+7.5, x=48.2, "vaccine refusal", col='black', cex=0.75, srt=0, pos=2)
text (y= 19.7+7.5, x=48.2, "vaccine hesitancy", col='black', cex=0.75, srt=0, pos=2)


dev.off()
