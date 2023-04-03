offset= 0.01
s = 3 # scaling factor
mtext.title = 2.2*s
mtext.subtitle = 1.6*s
mtext.sign = 1.0*s
mtext.sign.emo = 1.3*s

showtext_opts(dpi = 96) #set the resolution: 96 is default

png ('./figures/F_reasons no per region.png', width=1280*s, height=905.5*s, res=96)

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(0,0,0,0), # size of the outer margins in lines of text
    mar=c(5,8,1,1), # number of lines of margin to be specified on the four sides of the plot 
    bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE,
    family='Quattrocento'
)


plot(NULL, xlim=c(0, 70), ylim=c(1, 7*3), yaxt = 'n', xaxt = 'n') 

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
      lwd = 1*s,
      col = 'white', # the actual axis (line) 
      col.axis = dark.color, # colors of the actual labels
      cex.axis = 1, 
      font=2, # font type (bold)
      at=seq(2, 7*3, 3), # where to put labels  
      labels= c('Pandemic will\nbe over soon','Infection risk\nfor me is low',
                'COVID-19 risk\nis exaggerated','Worried about\nside effects',
                'Vaccines not\ntested yet','Vaccines are\nnot effective','Against vaccines\nin general'), # text of labels 
      las=1 # orientation of the labels
)

title (xlab='Share of the vaccine-hesitant population that agree with each statement', col='black', cex.lab=1.2, font.lab=2)

abline(v=seq(0,100,10), col='grey', lwd=1*s)
abline(h=seq(3.5, 7*3,3), col='grey', lwd=1*s)


points (x= c(sum(d1.e$reason.no.covid.over, na.rm=TRUE)/sum(d1.e$hesitant, na.rm=TRUE)*100,
             sum(d1.e$reason.no.risk.me.low, na.rm=TRUE)/sum(d1.e$hesitant, na.rm=TRUE)*100,
             sum(d1.e$reason.no.risk.gen.over, na.rm=TRUE)/sum(d1.e$hesitant, na.rm=TRUE)*100,
             sum(d1.e$reason.no.side.effects, na.rm=TRUE)/sum(d1.e$hesitant, na.rm=TRUE)*100,
             sum(d1.e$reason.no.limit.test, na.rm=TRUE)/sum(d1.e$hesitant, na.rm=TRUE)*100,
             sum(d1.e$reason.no.limit.effect, na.rm=TRUE)/sum(d1.e$hesitant, na.rm=TRUE)*100,
             sum(d1.e$reason.no.gen.against, na.rm=TRUE)/sum(d1.e$hesitant, na.rm=TRUE)*100),
        y=seq(1,7*3,3), col='red', bg='red', pch=21, cex=1)

points (x= c(sum(d1.w$reason.no.covid.over, na.rm=TRUE)/sum(d1.w$hesitant, na.rm=TRUE)*100,
             sum(d1.w$reason.no.risk.me.low, na.rm=TRUE)/sum(d1.w$hesitant, na.rm=TRUE)*100,
             sum(d1.w$reason.no.risk.gen.over, na.rm=TRUE)/sum(d1.w$hesitant, na.rm=TRUE)*100,
             sum(d1.w$reason.no.side.effects, na.rm=TRUE)/sum(d1.w$hesitant, na.rm=TRUE)*100,
             sum(d1.w$reason.no.limit.test, na.rm=TRUE)/sum(d1.w$hesitant, na.rm=TRUE)*100,
             sum(d1.w$reason.no.limit.effect, na.rm=TRUE)/sum(d1.w$hesitant, na.rm=TRUE)*100,
             sum(d1.w$reason.no.gen.against, na.rm=TRUE)/sum(d1.w$hesitant, na.rm=TRUE)*100),
        y=seq(2,7*3,3), col='blue', bg='blue', pch=21, cex=1)


points (x= c(sum(d1.s$reason.no.covid.over, na.rm=TRUE)/sum(d1.s$hesitant, na.rm=TRUE)*100,
             sum(d1.s$reason.no.risk.me.low, na.rm=TRUE)/sum(d1.s$hesitant, na.rm=TRUE)*100,
             sum(d1.s$reason.no.risk.gen.over, na.rm=TRUE)/sum(d1.s$hesitant, na.rm=TRUE)*100,
             sum(d1.s$reason.no.side.effects, na.rm=TRUE)/sum(d1.s$hesitant, na.rm=TRUE)*100,
             sum(d1.s$reason.no.limit.test, na.rm=TRUE)/sum(d1.s$hesitant, na.rm=TRUE)*100,
             sum(d1.s$reason.no.limit.effect, na.rm=TRUE)/sum(d1.s$hesitant, na.rm=TRUE)*100,
             sum(d1.s$reason.no.gen.against, na.rm=TRUE)/sum(d1.s$hesitant, na.rm=TRUE)*100),
        y=seq(3,7*3,3), col='black', bg='black', pch=21, cex=1)


text ('Eastern Europe', x = sum(d1.e$reason.no.covid.over, na.rm=TRUE)/sum(d1.e$hesitant, na.rm=TRUE)*100, y = 0.95 , col='red', pos=4, cex=0.85)
text ('Western Europe', x = sum(d1.w$reason.no.covid.over, na.rm=TRUE)/sum(d1.w$hesitant, na.rm=TRUE)*100, y = 1.95, col='blue', pos=4, cex=0.85)
text ('Southern Europe', x = sum(d1.s$reason.no.covid.over, na.rm=TRUE)/sum(d1.s$hesitant, na.rm=TRUE)*100, y = 2.95, col='black', pos=4, cex=0.85)

text ('Eastern Europe', x = sum(d1.e$reason.no.gen.against, na.rm=TRUE)/sum(d1.e$hesitant, na.rm=TRUE)*100, y = 19-0.05 , col='red', pos=4, cex=0.85)
text ('Western Europe', x = sum(d1.w$reason.no.gen.against, na.rm=TRUE)/sum(d1.w$hesitant, na.rm=TRUE)*100, y = 20-0.05, col='blue', pos=4, cex=0.85)
text ('Southern Europe', x = sum(d1.s$reason.no.gen.against, na.rm=TRUE)/sum(d1.s$hesitant, na.rm=TRUE)*100, y = 21-0.05, col='black', pos=4, cex=0.85)

dev.off()
