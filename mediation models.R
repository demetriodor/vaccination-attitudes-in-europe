
# Mediation models
set.seed(1234)
boots = 1000  #10000 for replication

model.out2 <- '
# outcome model 
hesitant ~ a1*age + a2*edu + a3*trust.networks.info + a4*v.safe

# mediator 1 model
v.safe ~ b1*age + b2*edu + b3*trust.networks.info

# mediator 2 model
trust.networks.info ~ c1*age + c2*edu

# direct effects (DE)
age_out_DE := a1
edu_out_DE := a2
trust_out_DE  := a3
safe_out_DE  := a4

age_safe_DE := b1
edu_safe_DE := b2
trust_safe_DE := b3

age_trust_DE := c1
edu_trust_DE := c2

# indirect effects (IDE)
age_via_safe_IDE  := b1*a4
edu_via_safe_IDE  := b2*a4

age_via_trust_IDE  := c1*a3
edu_via_trust_IDE  := c2*a3

age_via_trust_via_safe_IDE  := c1*a3*a4
edu_via_trust_via_safe__IDE  := c2*a3*a4

# total effect
total_age_out := a1 + (b1*a4) + (c1*a3) + (c1*a3*a4) 
total_edu_out := a2 + (b2*a4) + (c2*a3) + (c2*a3*a4)
total_trust_out := a3 + (b3*a4)
'

fit.model.out2 <- sem(model.out2, data=d1, se = "bootstrap",bootstrap = boots, link='logit')
summary(fit.model.out2, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)

fit.model.out2 <- sem(model.out2, data=d1.e, se = "bootstrap",bootstrap = boots, link='logit')
summary(fit.model.out2, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)

fit.model.out2 <- sem(model.out2, data=d1.w, se = "bootstrap",bootstrap = boots, link='logit')
summary(fit.model.out2, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)

fit.model.out2 <- sem(model.out2, data=d1.s, se = "bootstrap",bootstrap = boots, link='logit')
summary(fit.model.out2, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)
