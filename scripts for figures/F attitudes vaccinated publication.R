s = 3
x.min = 5
x.max = 55
y.min = 15 
y.max = 85
  
offset = 0.01
mtext.title = 2.2*s
mtext.subtitle = 1.5*s
mtext.sign = 1*s
mtext.sign.emo = 1.3*s

png ('./figures/F_attitudes_vaccinated_pulication.png', width=1280*s, height=905.5*s, res=96)

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(0,0,0,0), # size of the outer margins in lines of text (can be specified in inches as well with `omi`)
    mar=c(5,6,1,1), # number of lines of margin to be specified on the four sides of the plot (can be specified in inches as well with `mai`) 
    bty='o', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE, # switch off titles,
    family='Quattrocento' # font family
    
)

plot(NULL, xlim=c(x.min, x.max), ylim=c(y.min, y.max), yaxt = 'n', xaxt = 'n') 

title (xlab = 'Share of the population who hesitate or say they will "never" get vaccinated agasint COVID-19', line = 2.5, cex.lab = 1.2, font.lab=3)
title (ylab = 'Share of the population who is fully vaccinated, as of 19 September 2021', line = 4, cex.lab = 1.1, font.lab=3)

axis (1, 
      line = 0, # position
      tck = -0.01,
      lwd = 0.5*s,
      col = dark.color, # the actual axis (line) 
      col.axis = dark.color, # colors of the actual labels
      cex.axis = 1,
      font=2, # font type (bold)
      at = seq(0,100,10), # where to put labels  
      labels = paste0(seq(0,100,10), '%') , # text of labels 
      las=1 # orientation of the labels
)

axis (2, 
      line = 0, # position
      tck = -0.01,
      lwd = 0.5*s,
      col = dark.color, # the actual axis (line) 
      col.axis = dark.color, # colors of the actual labels
      cex.axis = 1, 
      font=2, # font type (bold)
      at = seq(0,100,10), # where to put labels  
      labels = paste0(seq(0,100,10), '%') , # text of labels 
      las=1 # orientation of the labels
)

abline(h = seq(0,100,10), col='lightgrey', lwd=1*s)
abline(v = seq(0,100,10), col='lightgrey', lwd=1*s)

abline (lm(pt$vaccinated~pt$yes.h), col=dark.color, lwd=3*s, lty=3)

points (x = pt$yes.h, y = pt$vaccinated, col=ifelse(pt$region3=='Eastern Europe', 'red', ifelse(pt$region3=='Western Europe', 'blue', 'black')), bg=ifelse(pt$region3=='Eastern Europe', 'red', ifelse(pt$region3=='Western Europe', 'blue', 'black')),  cex = 1.3, pch=21)

text (pt$country[-c(18,21,24)], x = pt$yes.h[-c(18,21,24)], y = pt$vaccinated[-c(18,21,24)]-2, col=ifelse(pt$region3[-c(18,21,24)]=='Eastern Europe', 'red', ifelse(pt$region3[-c(18,21,24)]=='Western Europe', 'blue', 'black')), cex = 1.2)
text (pt$country[c(18,21,24)], x = pt$yes.h[c(18,21,24)], y = pt$vaccinated[c(18,21,24)]+2, col=ifelse(pt$region3[-c(18,21,24)]=='Eastern Europe', 'red', ifelse(pt$region3[-c(18,21,24)]=='Western Europe', 'blue', 'black')), cex = 1.2)

text ('Black dotted line is linear regression fit. Correlation is -0.88.',
      x = 55, y=85, col = dark.color, font=3, cex=0.95, adj=1)

box(which = "plot", lty = "solid", col = dark.color,lwd = 0.5*s)

dev.off()
