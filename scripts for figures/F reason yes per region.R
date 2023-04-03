offset= 0.01
s = 3 # scaling factor
mtext.title = 2.2*s
mtext.subtitle = 1.6*s
mtext.sign = 1.0*s
mtext.sign.emo = 1.3*s

showtext_opts(dpi = 96) #set the resolution: 96 is default

png ('./figures/F_reasons yes per region.png', width=1280*s, height=905.5*s, res=96)

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(0,0,0,0), # size of the outer margins in lines of text
    mar=c(5,8,1,1), # number of lines of margin to be specified on the four sides of the plot 
    bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE,
    family='Quattrocento'
)


plot(NULL, xlim=c(50, 100), ylim=c(1, 7*3), yaxt = 'n', xaxt = 'n') 

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
      labels= c('Vaccines will\nend pandemic','Vaccines will\nprotect me',
                'Vaccines will\nprotect others','Vaccines will\nenable work',
                'Vaccines will\nenable travel','Vaccines will\nenable meetings','Vaccines will\nenable going out'), # text of labels 
      las=1 # orientation of the labels
)

title (xlab='Share of the vaccine-approving population that agree with each statement', col='black', cex.lab=1.2, font.lab=2)

abline(v=seq(0,100,10), col='grey', lwd=1*s)
abline(h=seq(3.5, 7*3,3), col='grey', lwd=1*s)


points (x= c(sum(d1.e$reason.yes.covid.end, na.rm=TRUE)/sum(d1.e$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.e$reason.yes.protect.me, na.rm=TRUE)/sum(d1.e$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.e$reason.yes.protect.others, na.rm=TRUE)/sum(d1.e$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.e$reason.yes.to.work, na.rm=TRUE)/sum(d1.e$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.e$reason.yes.to.travel, na.rm=TRUE)/sum(d1.e$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.e$reason.yes.to.socialize, na.rm=TRUE)/sum(d1.e$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.e$reason.yes.to.go.out, na.rm=TRUE)/sum(d1.e$hesitant==FALSE, na.rm=TRUE)*100),
        y=seq(1,7*3,3), col='red', bg='red', pch=21, cex=1)


points (x= c(sum(d1.w$reason.yes.covid.end, na.rm=TRUE)/sum(d1.w$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.w$reason.yes.protect.me, na.rm=TRUE)/sum(d1.w$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.w$reason.yes.protect.others, na.rm=TRUE)/sum(d1.w$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.w$reason.yes.to.work, na.rm=TRUE)/sum(d1.w$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.w$reason.yes.to.travel, na.rm=TRUE)/sum(d1.w$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.w$reason.yes.to.socialize, na.rm=TRUE)/sum(d1.w$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.w$reason.yes.to.go.out, na.rm=TRUE)/sum(d1.w$hesitant==FALSE, na.rm=TRUE)*100),
        y=seq(2,7*3,3), col='blue', bg='blue', pch=21, cex=1)


points (x= c(sum(d1.s$reason.yes.covid.end, na.rm=TRUE)/sum(d1.s$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.s$reason.yes.protect.me, na.rm=TRUE)/sum(d1.s$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.s$reason.yes.protect.others, na.rm=TRUE)/sum(d1.s$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.s$reason.yes.to.work, na.rm=TRUE)/sum(d1.s$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.s$reason.yes.to.travel, na.rm=TRUE)/sum(d1.s$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.s$reason.yes.to.socialize, na.rm=TRUE)/sum(d1.s$hesitant==FALSE, na.rm=TRUE)*100,
             sum(d1.s$reason.yes.to.go.out, na.rm=TRUE)/sum(d1.s$hesitant==FALSE, na.rm=TRUE)*100),
        y=seq(3,7*3,3), col='black', bg='black', pch=21, cex=1)


text ('Eastern Europe', x = sum(d1.e$reason.yes.covid.end, na.rm=TRUE)/sum(d1.e$hesitant==FALSE, na.rm=TRUE)*100, y = 1 , col='red', pos=2, cex=0.85)
text ('Western Europe', x = sum(d1.w$reason.yes.covid.end, na.rm=TRUE)/sum(d1.w$hesitant==FALSE, na.rm=TRUE)*100, y = 2, col='blue', pos=2, cex=0.85)
text ('Southern Europe', x = sum(d1.s$reason.yes.covid.end, na.rm=TRUE)/sum(d1.s$hesitant==FALSE, na.rm=TRUE)*100, y = 3, col='black', pos=2, cex=0.85)

text ('Eastern Europe', x = sum(d1.e$reason.yes.to.go.out, na.rm=TRUE)/sum(d1.e$hesitant==FALSE, na.rm=TRUE)*100, y = 19 , col='red', pos=2, cex=0.85)
text ('Western Europe', x = sum(d1.w$reason.yes.to.go.out, na.rm=TRUE)/sum(d1.w$hesitant==FALSE, na.rm=TRUE)*100, y = 20, col='blue', pos=2, cex=0.85)
text ('Southern Europe', x = sum(d1.s$reason.yes.to.go.out, na.rm=TRUE)/sum(d1.s$hesitant==FALSE, na.rm=TRUE)*100, y = 21, col='black', pos=2, cex=0.85)

dev.off()
