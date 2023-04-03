### This script analyses Flash Eurobarometer data from May 2021 for the paper 'Predictors of COVID-19 Vaccine Hesitancy in Eastern, Southern and Western Europe'
### Published in the journal Vaccine in 2023. Script by Dimiter Toshkov d.d.toshkov@fgga.leidenuniv.nl.
# Libraries and common settings -------------------------------------------
library(haven)
library(tidyverse)
library(questionr)
library(binom)
library(pscl)

library(patchwork)
library(magrittr)
library(gam)
library(jtools)

library(sjPlot)
library(htmlTable)
library(gtsummary)
library(sjlabelled)
library(vtable)

library(lavaan)

source ('graphical settings.R')
# Read and prepare data ---------------------------------------------------
d<-read_sav('./data/ZA7771_v1-0-0.sav')
names(d)
head(d)

d1 <- d %>%
  mutate (country = substr(isocntry, 1, 2),
          weight = w1,
          
          never = ifelse (q1==4, 1, 0),
          uncertain = ifelse (q1==3 | q1==998 | q1==999, 1, 0),
          hesitant = ifelse (q1==4 | q1==3 | q1==998 | q1==999, 1, 0),
          vaccinated = ifelse (q1==5, 1, 0),
          
          region3 = recode (country, 'BG' = 'Eastern Europe','RO' = 'Eastern Europe','CZ' = 'Eastern Europe','PL' = 'Eastern Europe',
                            'SK' = 'Eastern Europe','HU' = 'Eastern Europe','SI' = 'Eastern Europe','HR' = 'Eastern Europe',
                            'EE' = 'Eastern Europe','LT' = 'Eastern Europe','LV' = 'Eastern Europe',
                            'PT' = 'Southern Europe', 'ES' = 'Southern Europe', 'IT' = 'Southern Europe', 'GR' = 'Southern Europe',
                            'CY' = 'Southern Europe', 'MT' = 'Southern Europe', .default = 'Western Europe'),
          
          male = ifelse (d2==1, 1, 0),
          age = d1,
          edu = ifelse (d4>82, NA, (ifelse(d4<14, NA, d4))),
          city = ifelse (d13==3, 1, 0),
          empl = dplyr::recode(d5r, '1'='self-employed','2'='employee', '3'='manual','4'='no activity', .default=NA_character_),
          
          v.safe = ifelse (sd2_1==1 | sd2_1==2, 1, 0),
          v.effective = ifelse (sd2_2==1 | sd2_2==2, 1, 0),
          v.ever = ifelse (sd1_1==1 | sd1_2==1, 1, 0),
          
          know.covid = ifelse (q9_1==1, 1, 0),
          know.ill.covid = ifelse (q9_2==1, 1, 0),
          had.covid = ifelse (q9_3==1, 1, 0),
          was.ill.covid = ifelse (q9_4==1, 1, 0),
          fear.covid = ifelse (q9_5==1, 1, 0),
          
          can.avoid.covid = ifelse (q5_1==1 | q5_1==2, 1, 0),
          v.compulsory = ifelse (q5_4==1 | q5_4==2, 1, 0),
          
          trust.eu.info = q6.1,
          trust.gov.info = q6.2,
          trust.health.info = q6.3,
          trust.local.info = q6.4,
          trust.doctors.info = q6.5,
          trust.media.info = q6.6,
          trust.web.info = q6.7,
          trust.networks.info = q6.8,
          trust.people.info = q6.9,
          
          reason.yes.covid.end =  ifelse (q2a_1==1 | q2a_1==2, 1, 0),
          reason.yes.protect.me =  ifelse (q2a_2==1 | q2a_2==2, 1, 0),
          reason.yes.protect.others =  ifelse (q2a_3==1 | q2a_3==2, 1, 0),
          reason.yes.to.work =  ifelse (q2a_4==1 | q2a_4==2, 1, 0),
          reason.yes.to.travel =  ifelse (q2a_5==1 | q2a_5==2, 1, 0),
          reason.yes.to.socialize =  ifelse (q2a_6==1 | q2a_6==2, 1, 0),
          reason.yes.to.go.out =  ifelse (q2a_7==1 | q2a_7==2, 1, 0),
          
          reason.no.covid.over =  ifelse (q2b_1==1 | q2b_1==2, 1, 0),
          reason.no.risk.me.low =  ifelse (q2b_2==1 | q2b_2==2, 1, 0),
          reason.no.risk.gen.over =  ifelse (q2b_3==1 | q2b_3==2, 1, 0),
          reason.no.side.effects =  ifelse (q2b_4==1 | q2b_4==2, 1, 0),
          reason.no.limit.test =  ifelse (q2b_5==1 | q2b_5==2, 1, 0),
          reason.no.limit.effect =  ifelse (q2b_6==1 | q2b_6==2, 1, 0),
          reason.no.gen.against =  ifelse (q2b_7==1 | q2b_7==2, 1, 0),
          
          v.benefits =  ifelse (q4_1==1 | q4_1==2, 1, 0),
          v.eu.safe =  ifelse (q4_2==1 | q4_2==2, 1, 0),
          v.toofast =  ifelse (q4_3==1 | q4_3==2, 1, 0),
          v.sideeffects =  ifelse (q4_4==1 | q4_4==2, 1, 0),
          v.onlyway =  ifelse (q4_5==1 | q4_5==2, 1, 0),
          v.duty =  ifelse (q5_3==1 | q5_3==2, 1, 0),
          )
d1$hesitant1 <- ifelse (d1$uncertain==1, NA, ifelse(d1$hesitant==1, 1, 0))

# Subsets
d1.e <- d1[d1$region3=='Eastern Europe',]
d1.w <- d1[d1$region3=='Western Europe',]
d1.s <- d1[d1$region3=='Southern Europe',]

# Prepare a dataframe of country-level data -------------------------------
temp.table<-questionr::wtd.table(factor(d1$hesitant), d1$country, weights=d1$weight, digits = 0, useNA = NULL)
temp.cprop.table<-questionr::cprop(temp.table, digits=0, total=FALSE, n=FALSE, percent=TRUE)
pt1 <-as.data.frame.matrix(t(temp.cprop.table)) 
pt1 <- pt1[order(pt1$`1`, decreasing = TRUE),]
pt1$Row.names = row.names(pt1)

temp.table<-questionr::wtd.table(factor(d1$never), d1$country, weights=d1$weight, digits = 0, useNA = NULL)
temp.cprop.table<-questionr::cprop(temp.table, digits=0, total=FALSE, n=FALSE, percent=TRUE)
pt2 <-as.data.frame.matrix(t(temp.cprop.table)) 
pt2 <- pt2[order(pt2$`1`, decreasing = TRUE),]
pt2$Row.names = row.names(pt2)

temp.table<-questionr::wtd.table(factor(d1$vaccinated), d1$country, weights=d1$weight, digits = 0, useNA = NULL)
temp.cprop.table<-questionr::cprop(temp.table, digits=0, total=FALSE, n=FALSE, percent=TRUE)
pt3 <-as.data.frame.matrix(t(temp.cprop.table)) 
pt3 <- pt3[order(pt3$`1`, decreasing = TRUE),]
pt3$Row.names = row.names(pt3)
  
pt <- merge(pt1, pt2, by="Row.names")
pt <- merge(pt, pt3, by="Row.names")

colnames(pt) <- c('country','no.h','yes.h','no.never','yes.never', 'no.vaccinated','yes.vaccinated' )

pt <- pt[order(pt$yes.h, decreasing = TRUE),]

vaccinated <- read.csv('./data/vaccinated_19092021.csv', sep=';')

pt<-merge(pt, vaccinated, by='country')
pt <- pt[order(pt$yes.h, decreasing = TRUE),]

pt <- pt %>% mutate (region3 = recode(country, 'BG' = 'Eastern Europe','RO' = 'Eastern Europe','CZ' = 'Eastern Europe','PL' = 'Eastern Europe',
                                      'SK' = 'Eastern Europe','HU' = 'Eastern Europe','SI' = 'Eastern Europe','HR' = 'Eastern Europe',
                                      'EE' = 'Eastern Europe','LT' = 'Eastern Europe','LV' = 'Eastern Europe',
                                      'PT' = 'Southern Europe', 'ES' = 'Southern Europe', 'IT' = 'Southern Europe', 'GR' = 'Southern Europe',
                                      'CY' = 'Southern Europe', 'MT' = 'Southern Europe', .default = 'Western Europe'))

fear <- data.frame(round(prop.table(table(d1$country, d1$fear.covid),1),2)[,2])
fear$country = row.names(fear)
pt <- merge(pt, fear, by="country")
colnames(pt)[10] = 'fear'

# Correlations
cor (pt$vaccinated, pt$no.h)
cor (pt$vaccinated, pt$no.never)
cor (pt$vaccinated, pt$yes.vaccinated)
cor (pt$no.never, pt$fear) # very high fear in DK

# Table of descriptives ---------------------------------------------------
# Descriptive tables and t-tests T1 before scaling ----
st(d1f[,c('region3','age', 'edu', 'male', 'city', 'empl', 
          'trust.eu.info', 'trust.gov.info', 'trust.health.info', 'trust.local.info', 'trust.doctors.info', 
          'trust.media.info', 'trust.web.info', 'trust.networks.info', 'trust.people.info', 
          'v.safe', 'v.effective','know.ill.covid', 'was.ill.covid', 'fear.covid', 
          'hesitant', 'never')], 
   summ=c('mean(x)','sd(x)','min(x)','max(x)'),
   summ.names = list(c('Mean','St.Dev.','Min','Max')),
   digits=2, fixed.digits=TRUE,
   title='Descriptive statistics',
   out='htmlreturn', file='./tables/Descr_table.html')

# Barchart of vaccine hesitancy and refusal -----------------------------
#source ('./scripts for figures/F hesitancy per country publication.R')
source ('./scripts for figures/F hesitancy per country publication horizontal.R')
# Barchart of vaccine hesitancy and refusal -----------------------------
source ('./scripts for figures/F attitudes vaccinated publication.R')
# Distribution of information trust per region -----------------------------
source ('./scripts for figures/F trust per region.R')
# Distribution of demographic variables per region -----------------------------
source ('./scripts for figures/F attitudes per region.R')
# GAM graphs and analysis (age and education) -----------------------------
source ('./scripts for figures/F_age_edu_gams2.R')
# GLM models and marginal effects figures -----------------------------
source ('logreg models for tables.R')
source ('./scripts for figures/F demographics marginal effects.R')
# Reasons for vaccination and vaccine hesitancy and refusal -----------------------------
source ('./scripts for figures/F reason yes per region.R')
source ('./scripts for figures/F reason no per region.R')
# Mediation models -----------------------------
source ('mediation models.R')
