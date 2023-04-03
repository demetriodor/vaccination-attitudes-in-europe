offset= 0.01
s = 3 # scaling factor
mtext.title = 2.2*s
mtext.subtitle = 1.6*s
mtext.sign = 1.0*s
mtext.sign.emo = 1.3*s

showtext_opts(dpi = 96) #set the resolution: 96 is default

png ('./figures/F_trust per region.png', width=1280*s, height=905.5*s, res=96)

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(0,0,0,0), # size of the outer margins in lines of text
    mar=c(2,8,1,1), # number of lines of margin to be specified on the four sides of the plot 
    bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE,
    family='Quattrocento'
)


plot(NULL, xlim=c(0, 70), ylim=c(1, 9*3), yaxt = 'n', xaxt = 'n') 

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
      labels= c('European Union','Nat. government','Nat. health auth.','Reg./local auth.','Health proff-s','Trad. media','Websites','Online networks', 'People around'), # text of labels 
      las=1 # orientation of the labels
)


abline(v=seq(0,100,10), col='grey', lwd=1*s)
abline(h=seq(3.5, 9*3,3), col='grey', lwd=1*s)


points (x= c(prop.table(table(d1.e$trust.eu.info))[2]*100,
             prop.table(table(d1.e$trust.gov.info))[2]*100,
             prop.table(table(d1.e$trust.health.info))[2]*100,
             prop.table(table(d1.e$trust.local.info))[2]*100,
             prop.table(table(d1.e$trust.doctors.info))[2]*100,
             prop.table(table(d1.e$trust.media.info))[2]*100,
             prop.table(table(d1.e$trust.web.info))[2]*100,
             prop.table(table(d1.e$trust.networks.info))[2]*100,
             prop.table(table(d1.e$trust.people.info))[2]*100),
        y=seq(1,9*3,3), col='red', bg='red', pch=21, cex=1)

segments (x1 = c(binom.confint(sum(d1.e$trust.eu.info), length(d1.e$trust.eu.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$trust.gov.info), length(d1.e$trust.gov.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$trust.health.info), length(d1.e$trust.health.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$trust.local.info), length(d1.e$trust.local.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$trust.doctors.info), length(d1.e$trust.doctors.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$trust.media.info), length(d1.e$trust.media.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$trust.web.info), length(d1.e$trust.web.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$trust.networks.info), length(d1.e$trust.networks.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$trust.people.info), length(d1.e$trust.people.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.e$trust.eu.info), length(d1.e$trust.eu.info), conf.level = 0.95, methods = 'wilson')$upper*100),
          
          x0 = c(binom.confint(sum(d1.e$trust.eu.info), length(d1.e$trust.eu.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.e$trust.gov.info), length(d1.e$trust.gov.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.e$trust.health.info), length(d1.e$trust.health.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.e$trust.local.info), length(d1.e$trust.local.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.e$trust.doctors.info), length(d1.e$trust.doctors.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.e$trust.media.info), length(d1.e$trust.media.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.e$trust.web.info), length(d1.e$trust.web.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.e$trust.networks.info), length(d1.e$trust.networks.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.e$trust.people.info), length(d1.e$trust.people.info), conf.level = 0.95, methods = 'wilson')$lower*100),
          y0 = seq(1,9*3,3),
          y1 = seq(1,9*3,3),
          col='red', lwd=3)




points (x= c(prop.table(table(d1.w$trust.eu.info))[2]*100,
             prop.table(table(d1.w$trust.gov.info))[2]*100,
             prop.table(table(d1.w$trust.health.info))[2]*100,
             prop.table(table(d1.w$trust.local.info))[2]*100,
             prop.table(table(d1.w$trust.doctors.info))[2]*100,
             prop.table(table(d1.w$trust.media.info))[2]*100,
             prop.table(table(d1.w$trust.web.info))[2]*100,
             prop.table(table(d1.w$trust.networks.info))[2]*100,
             prop.table(table(d1.w$trust.people.info))[2]*100),
             y=seq(2,9*3,3), col='blue', bg='blue', pch=21, cex=1)

segments (x1 = c(binom.confint(sum(d1.w$trust.eu.info), length(d1.w$trust.eu.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$trust.gov.info), length(d1.w$trust.gov.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$trust.health.info), length(d1.w$trust.health.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$trust.local.info), length(d1.w$trust.local.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$trust.doctors.info), length(d1.w$trust.doctors.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$trust.media.info), length(d1.w$trust.media.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$trust.web.info), length(d1.w$trust.web.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$trust.networks.info), length(d1.w$trust.networks.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$trust.people.info), length(d1.w$trust.people.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.w$trust.eu.info), length(d1.w$trust.eu.info), conf.level = 0.95, methods = 'wilson')$upper*100),
          
          x0 = c(binom.confint(sum(d1.w$trust.eu.info), length(d1.w$trust.eu.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.w$trust.gov.info), length(d1.w$trust.gov.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.w$trust.health.info), length(d1.w$trust.health.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.w$trust.local.info), length(d1.w$trust.local.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.w$trust.doctors.info), length(d1.w$trust.doctors.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.w$trust.media.info), length(d1.w$trust.media.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.w$trust.web.info), length(d1.w$trust.web.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.w$trust.networks.info), length(d1.w$trust.networks.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.w$trust.people.info), length(d1.w$trust.people.info), conf.level = 0.95, methods = 'wilson')$lower*100),
          y0 = seq(2,9*3,3),
          y1 = seq(2,9*3,3),
          col='blue', lwd=3)



points (x= c(prop.table(table(d1.s$trust.eu.info))[2]*100,
               prop.table(table(d1.s$trust.gov.info))[2]*100,
               prop.table(table(d1.s$trust.health.info))[2]*100,
               prop.table(table(d1.s$trust.local.info))[2]*100,
               prop.table(table(d1.s$trust.doctors.info))[2]*100,
               prop.table(table(d1.s$trust.media.info))[2]*100,
               prop.table(table(d1.s$trust.web.info))[2]*100,
               prop.table(table(d1.s$trust.networks.info))[2]*100,
               prop.table(table(d1.s$trust.people.info))[2]*100),
             y=seq(3,9*3,3), col='black', bg='black',  pch=21, cex=1)

segments (x1 = c(binom.confint(sum(d1.s$trust.eu.info), length(d1.s$trust.eu.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.s$trust.gov.info), length(d1.s$trust.gov.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.s$trust.health.info), length(d1.s$trust.health.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.s$trust.local.info), length(d1.s$trust.local.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.s$trust.doctors.info), length(d1.s$trust.doctors.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.s$trust.media.info), length(d1.s$trust.media.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.s$trust.web.info), length(d1.s$trust.web.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.s$trust.networks.info), length(d1.s$trust.networks.info), conf.level = 0.95, methods = 'wilson')$upper*100,
                 binom.confint(sum(d1.s$trust.people.info), length(d1.s$trust.people.info), conf.level = 0.95, methods = 'wilson')$upper*100),
          
          x0 = c(binom.confint(sum(d1.s$trust.eu.info), length(d1.s$trust.eu.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.s$trust.gov.info), length(d1.s$trust.gov.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.s$trust.health.info), length(d1.s$trust.health.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.s$trust.local.info), length(d1.s$trust.local.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.s$trust.doctors.info), length(d1.s$trust.doctors.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.s$trust.media.info), length(d1.s$trust.media.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.s$trust.web.info), length(d1.s$trust.web.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.s$trust.networks.info), length(d1.s$trust.networks.info), conf.level = 0.95, methods = 'wilson')$lower*100,
                 binom.confint(sum(d1.s$trust.people.info), length(d1.s$trust.people.info), conf.level = 0.95, methods = 'wilson')$lower*100),
          y0 = seq(3,9*3,3),
          y1 = seq(3,9*3,3),
          col='black', lwd=3)

#text ('Eastern Europe', x = binom.confint(sum(d1.e$trust.people.info), length(d1.e$trust.people.info), conf.level = 0.95, methods = 'wilson')$upper*100, y = 25 , col='red', pos=4, cex=0.85)
#text ('Western Europe', x = binom.confint(sum(d1.w$trust.people.info), length(d1.w$trust.people.info), conf.level = 0.95, methods = 'wilson')$upper*100, y = 26, col='blue', pos=4, cex=0.85)
#text ('Southern Europe', x = binom.confint(sum(d1.s$trust.people.info), length(d1.s$trust.people.info), conf.level = 0.95, methods = 'wilson')$upper*100, y = 27, col='black', pos=4, cex=0.85)

#text ('Eastern Europe', x = binom.confint(sum(d1.e$trust.eu.info), length(d1.e$trust.eu.info), conf.level = 0.95, methods = 'wilson')$upper*100, y = 1 , col='red', pos=4, cex=0.85)
#text ('Western Europe', x = binom.confint(sum(d1.w$trust.eu.info), length(d1.w$trust.eu.info), conf.level = 0.95, methods = 'wilson')$upper*100, y = 2, col='blue', pos=4, cex=0.85)
#text ('Southern Europe', x = binom.confint(sum(d1.s$trust.eu.info), length(d1.s$trust.eu.info), conf.level = 0.95, methods = 'wilson')$upper*100, y = 3, col='black', pos=4, cex=0.85)

text ('Eastern Europe', x = 52.15 , y = 25 , col='red', pos=4, cex=0.85)
text ('Western Europe', x = 52.15, y = 26, col='blue', pos=4, cex=0.85)
text ('Southern Europe', x = 52.15, y = 27, col='black', pos=4, cex=0.85)

points (x = 51 , y = 25 , col='red', bg='red', pch=21, cex=1)
points (x = 51, y = 26, col='blue', bg='blue', pch=21, cex=1)
points (x = 51, y = 27, col='black', bg='black', pch=21, cex=1)

segments (x0 = 50.14, x1=52.14, y0 = 25, y1 = 25, col='red', bg='red', pch=21, cex=1)
segments (x0 = 50.14, x1=52.14, y0 = 26, y1 = 26, col='blue', bg='blue', pch=21, cex=1)
segments (x0 = 50.14, x1=52.14, y0 = 27, y1 = 27, col='black', bg='black', pch=21, cex=1)

dev.off()
