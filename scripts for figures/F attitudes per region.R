offset= 0.01
s = 3 # scaling factor
mtext.title = 2.2*s
mtext.subtitle = 1.6*s
mtext.sign = 1.0*s
mtext.sign.emo = 1.3*s

showtext_opts(dpi = 96) #set the resolution: 96 is default

png ('./figures/F_attitudes per region.png', width=1280*s, height=905.5*s, res=96)

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(0,0,0,0), # size of the outer margins in lines of text
    mar=c(2,12,1,1), # number of lines of margin to be specified on the four sides of the plot 
    bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE,
    family='Quattrocento'
)


plot(NULL, xlim=c(0, 100), ylim=c(1, 9*3), yaxt = 'n', xaxt = 'n') 

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
      at=seq(2, 9*3, 3), # where to put labels  
      labels= c('Vaccine refusal','Vaccine hesitancy','Vaccines are safe','Vaccines are effective', 'Compuslory vaccines',
                'Knows ill from COVID','Been ill from COVID','Fears to get COVID','Can avoid COVID'), # text of labels 
      las=1 # orientation of the labels
)


abline(v=seq(0,100,10), col='grey', lwd=1*s)
abline(h=seq(3.5, 9*3,3), col='grey', lwd=1*s)


points (x= c(prop.table(table(d1.e$never))[2]*100,
             prop.table(table(d1.e$hesitant))[2]*100,
             prop.table(table(d1.e$v.safe))[2]*100,
             prop.table(table(d1.e$v.effective))[2]*100,
             prop.table(table(d1.e$v.compulsory))[2]*100,
             prop.table(table(d1.e$know.ill.covid))[2]*100,
             prop.table(table(d1.e$was.ill.covid))[2]*100,
             prop.table(table(d1.e$fear.covid))[2]*100,
             prop.table(table(d1.e$can.avoid.covid))[2]*100),
        y=seq(1,9*3,3), col='red', bg='red', pch=21, cex=1)

segments (x1 = c(binom.confint(sum(d1.e$never), length(d1.e$never), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$hesitant), length(d1.e$hesitant), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$v.safe), length(d1.e$v.safe), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$v.effective), length(d1.e$v.effective), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$v.compulsory), length(d1.e$v.compulsory), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$know.ill.covid), length(d1.e$know.ill.covid), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$was.ill.covid), length(d1.e$was.ill.covid), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$fear.covid), length(d1.e$fear.covid), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$can.avoid.covid), length(d1.e$can.avoid.covid), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$never), length(d1.e$never), conf.level = 0.95, methods = 'wilson')$upper*100),
          
          x0 = c(binom.confint(sum(d1.e$never), length(d1.e$never), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.e$hesitant), length(d1.e$hesitant), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.e$v.safe), length(d1.e$v.safe), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.e$v.effective), length(d1.e$v.effective), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.e$v.compulsory), length(d1.e$v.compulsory), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.e$know.ill.covid), length(d1.e$know.ill.covid), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.e$was.ill.covid), length(d1.e$was.ill.covid), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.e$fear.covid), length(d1.e$fear.covid), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.e$can.avoid.covid), length(d1.e$can.avoid.covid), conf.level = 0.95, methods = 'wilson')$lower*100),
          y0 = seq(1,9*3,3),
          y1 = seq(1,9*3,3),
          col='red', lwd=3)




points (x= c(prop.table(table(d1.w$never))[2]*100,
             prop.table(table(d1.w$hesitant))[2]*100,
             prop.table(table(d1.w$v.safe))[2]*100,
             prop.table(table(d1.w$v.effective))[2]*100,
             prop.table(table(d1.w$v.compulsory))[2]*100,
             prop.table(table(d1.w$know.ill.covid))[2]*100,
             prop.table(table(d1.w$was.ill.covid))[2]*100,
             prop.table(table(d1.w$fear.covid))[2]*100,
             prop.table(table(d1.w$can.avoid.covid))[2]*100),
             y=seq(2,9*3,3), col='blue', bg='blue', pch=21, cex=1)

segments (x1 = c(binom.confint(sum(d1.w$never), length(d1.w$never), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$hesitant), length(d1.w$hesitant), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$v.safe), length(d1.w$v.safe), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$v.effective), length(d1.w$v.effective), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$v.compulsory), length(d1.w$v.compulsory), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$know.ill.covid), length(d1.w$know.ill.covid), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$was.ill.covid), length(d1.w$was.ill.covid), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$fear.covid), length(d1.w$fear.covid), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$can.avoid.covid), length(d1.w$can.avoid.covid), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$never), length(d1.w$never), conf.level = 0.95, methods = 'wilson')$upper*100),
          
          x0 = c(binom.confint(sum(d1.w$never), length(d1.w$never), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.w$hesitant), length(d1.w$hesitant), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.w$v.safe), length(d1.w$v.safe), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.w$v.effective), length(d1.w$v.effective), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.w$v.compulsory), length(d1.w$v.compulsory), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.w$know.ill.covid), length(d1.w$know.ill.covid), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.w$was.ill.covid), length(d1.w$was.ill.covid), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.w$fear.covid), length(d1.w$fear.covid), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.w$can.avoid.covid), length(d1.w$can.avoid.covid), conf.level = 0.95, methods = 'wilson')$lower*100),
          y0 = seq(2,9*3,3),
          y1 = seq(2,9*3,3),
          col='blue', lwd=3)



points (x= c(prop.table(table(d1.s$never))[2]*100,
               prop.table(table(d1.s$hesitant))[2]*100,
               prop.table(table(d1.s$v.safe))[2]*100,
               prop.table(table(d1.s$v.effective))[2]*100,
               prop.table(table(d1.s$v.compulsory))[2]*100,
               prop.table(table(d1.s$know.ill.covid))[2]*100,
               prop.table(table(d1.s$was.ill.covid))[2]*100,
               prop.table(table(d1.s$fear.covid))[2]*100,
               prop.table(table(d1.s$can.avoid.covid))[2]*100),
             y=seq(3,9*3,3), col='black', bg='black',  pch=21, cex=1)

segments (x1 = c(binom.confint(sum(d1.s$never), length(d1.s$never), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.s$hesitant), length(d1.s$hesitant), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.s$v.safe), length(d1.s$v.safe), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.s$v.effective), length(d1.s$v.effective), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.s$v.compulsory), length(d1.s$v.compulsory), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.s$know.ill.covid), length(d1.s$know.ill.covid), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.s$was.ill.covid), length(d1.s$was.ill.covid), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.s$fear.covid), length(d1.s$fear.covid), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.s$can.avoid.covid), length(d1.s$can.avoid.covid), conf.level = 0.95, methods = 'wilson')$upper*100),
          
          x0 = c(binom.confint(sum(d1.s$never), length(d1.s$never), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.s$hesitant), length(d1.s$hesitant), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.s$v.safe), length(d1.s$v.safe), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.s$v.effective), length(d1.s$v.effective), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.s$v.compulsory), length(d1.s$v.compulsory), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.s$know.ill.covid), length(d1.s$know.ill.covid), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.s$was.ill.covid), length(d1.s$was.ill.covid), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.s$fear.covid), length(d1.s$fear.covid), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.s$can.avoid.covid), length(d1.s$can.avoid.covid), conf.level = 0.95, methods = 'wilson')$lower*100),
          y0 = seq(3,9*3,3),
          y1 = seq(3,9*3,3),
          col='black', lwd=3)

#text ('Eastern Europe', x = binom.confint(sum(d1.e$can.avoid.covid), length(d1.e$can.avoid.covid), conf.level = 0.95, methods = 'wilson')$upper*100, y = 25 , col='red', pos=4, cex=0.85)
#text ('Western Europe', x = binom.confint(sum(d1.w$can.avoid.covid), length(d1.w$can.avoid.covid), conf.level = 0.95, methods = 'wilson')$upper*100, y = 26, col='blue', pos=4, cex=0.85)
#text ('Southern Europe', x = binom.confint(sum(d1.s$can.avoid.covid), length(d1.s$can.avoid.covid), conf.level = 0.95, methods = 'wilson')$upper*100, y = 27, col='black', pos=4, cex=0.85)

#text ('Eastern Europe', x = binom.confint(sum(d1.e$never), length(d1.e$never), conf.level = 0.95, methods = 'wilson')$upper*100, y = 1 , col='red', pos=4, cex=0.85)
#text ('Western Europe', x = binom.confint(sum(d1.w$never), length(d1.w$never), conf.level = 0.95, methods = 'wilson')$upper*100, y = 2, col='blue', pos=4, cex=0.85)
#text ('Southern Europe', x = binom.confint(sum(d1.s$never), length(d1.s$never), conf.level = 0.95, methods = 'wilson')$upper*100, y = 3, col='black', pos=4, cex=0.85)

text ('Eastern Europe', x = 52.15+30 , y = 25 , col='red', pos=4, cex=0.85)
text ('Western Europe', x = 52.15+30, y = 26, col='blue', pos=4, cex=0.85)
text ('Southern Europe', x = 52.15+30, y = 27, col='black', pos=4, cex=0.85)

points (x = 51+30 , y = 25 , col='red', bg='red', pch=21, cex=1)
points (x = 51+30, y = 26, col='blue', bg='blue', pch=21, cex=1)
points (x = 51+30, y = 27, col='black', bg='black', pch=21, cex=1)

segments (x0 = 50.14+30, x1=52.14+30, y0 = 25, y1 = 25, col='red', bg='red', pch=21, cex=1)
segments (x0 = 50.14+30, x1=52.14+30, y0 = 26, y1 = 26, col='blue', bg='blue', pch=21, cex=1)
segments (x0 = 50.14+30, x1=52.14+30, y0 = 27, y1 = 27, col='black', bg='black', pch=21, cex=1)


dev.off()
