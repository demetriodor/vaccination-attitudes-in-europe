d1f <- remove_all_labels(d1)

# ### Models of vaccine hesitancy on trust, + demographic, + other variables -------------------
summary(m1 <- glm(hesitant ~ region3 + trust.eu.info + trust.gov.info + trust.health.info + trust.local.info + trust.doctors.info + 
                    trust.media.info + trust.web.info + trust.networks.info + trust.people.info, 
                  data = d1f, family='binomial'(link = "logit")))


summary(m2 <- glm(hesitant ~ region3 + trust.eu.info + trust.gov.info + trust.health.info + trust.local.info + trust.doctors.info + 
                    trust.media.info + trust.web.info + trust.networks.info + trust.people.info + 
                    age  + edu + male + city + empl, 
                  data = d1f, family='binomial'(link = "logit")))

summary(m3 <- glm(hesitant ~ region3 + trust.eu.info + trust.gov.info + trust.health.info + trust.local.info + trust.doctors.info + 
                    trust.media.info + trust.web.info + trust.networks.info + trust.people.info + 
                    age  + edu + male + city + empl +
                    v.safe + v.effective + know.ill.covid + was.ill.covid + fear.covid, 
                  data = d1f, family='binomial'(link = "logit")))

c(nobs(m1), nobs(m2), nobs(m3))
c(round(pR2(m1)[[4]],2),round(pR2(m2)[[4]],2),round(pR2(m3)[[4]],2))

t1 <- tbl_regression(m1,  exponentiate = TRUE) %>% add_significance_stars(pattern = "{estimate} [{conf.low}, {conf.high}]{stars}", hide_ci = TRUE, hide_se = TRUE) %>%
  modify_header(estimate ~ "**OR (95% CI)**") %>% modify_footnote(estimate ~ NA, abbreviation = TRUE) %>% italicize_levels() 
t2 <- tbl_regression(m2,  exponentiate = TRUE) %>% add_significance_stars(pattern = "{estimate} [{conf.low}, {conf.high}]{stars}", hide_ci = TRUE, hide_se = TRUE) %>%
  modify_header(estimate ~ "**OR (95% CI)**") %>% modify_footnote(estimate ~ NA, abbreviation = TRUE) %>% italicize_levels() 
t3 <- tbl_regression(m3,  exponentiate = TRUE) %>% add_significance_stars(pattern = "{estimate} [{conf.low}, {conf.high}]{stars}", hide_ci = TRUE, hide_se = TRUE) %>%
  modify_header(estimate ~ "**OR (95% CI)**") %>% modify_footnote(estimate ~ NA, abbreviation = TRUE) %>% italicize_levels() 

tbl <- tbl_merge(tbls = list(t1, t2, t3), tab_spanner = c("**Model 1**", "**Model 2**", "**Model 3**"))
tbl %>%
  as_gt() %>%
  gt::gtsave(filename = "./tables/reported_table1.html") # use extensions .html .tex .ltx .rtf


# ### Models of vaccine hesitancy and refusal with interactions -------------------
summary(m4 <- glm(hesitant ~ trust.eu.info*region3 + trust.gov.info*region3 + trust.health.info*region3 + trust.local.info*region3 + trust.doctors.info*region3 + 
                     trust.media.info*region3 + trust.web.info *region3 + trust.networks.info*region3 + trust.people.info*region3 + 
                     age*region3  + edu*region3 + male*region3 + city*region3 + empl*region3 +
                     v.safe*region3 + v.effective*region3 + know.ill.covid*region3 + was.ill.covid*region3 + fear.covid*region3 + region3, 
                   data = d1f, family='binomial'))

summary(m5 <- update (m4, never ~ .))
c(round(pR2(m4)[[4]],2),round(pR2(m5)[[4]],2))


t4 <- tbl_regression(m4,  exponentiate = TRUE) %>% add_significance_stars(pattern = "{estimate} [{conf.low}, {conf.high}]{stars}", hide_ci = TRUE, hide_se = TRUE) %>%
  modify_header(estimate ~ "**OR (95% CI)**") %>% modify_footnote(estimate ~ NA, abbreviation = TRUE) %>% italicize_levels() 
t5 <- tbl_regression(m5,  exponentiate = TRUE) %>% add_significance_stars(pattern = "{estimate} [{conf.low}, {conf.high}]{stars}", hide_ci = TRUE, hide_se = TRUE) %>%
  modify_header(estimate ~ "**OR (95% CI)**") %>% modify_footnote(estimate ~ NA, abbreviation = TRUE) %>% italicize_levels() 


tbl <- tbl_merge(tbls = list(t4, t5), tab_spanner = c("**Model A4**", "**Model A5**"))
tbl %>%
  as_gt() %>%
  gt::gtsave(filename = "./tables/reported_tableA2.html") # use extensions .html .tex .ltx .rtf


# ### Models of vaccine refusal on trust, + demographic, + other variables -------------------
summary(m1 <- glm(never ~ region3 + trust.eu.info + trust.gov.info + trust.health.info + trust.local.info + trust.doctors.info + 
                    trust.media.info + trust.web.info + trust.networks.info + trust.people.info, 
                  data = d1f, family='binomial'(link = "logit")))


summary(m2 <- glm(never ~ region3 + trust.eu.info + trust.gov.info + trust.health.info + trust.local.info + trust.doctors.info + 
                    trust.media.info + trust.web.info + trust.networks.info + trust.people.info + 
                    age  + edu + male + city + empl, 
                  data = d1f, family='binomial'(link = "logit")))

summary(m3 <- glm(never ~ region3 + trust.eu.info + trust.gov.info + trust.health.info + trust.local.info + trust.doctors.info + 
                    trust.media.info + trust.web.info + trust.networks.info + trust.people.info + 
                    age  + edu + male + city + empl +
                    v.safe + v.effective + know.ill.covid + was.ill.covid + fear.covid, 
                  data = d1f, family='binomial'(link = "logit")))
c(nobs(m1), nobs(m2), nobs(m3))
c(round(pR2(m1)[[4]],2),round(pR2(m2)[[4]],2),round(pR2(m3)[[4]],2))

t1 <- tbl_regression(m1,  exponentiate = TRUE) %>% add_significance_stars(pattern = "{estimate} [{conf.low}, {conf.high}]{stars}", hide_ci = TRUE, hide_se = TRUE) %>%
  modify_header(estimate ~ "**OR (95% CI)**") %>% modify_footnote(estimate ~ NA, abbreviation = TRUE) %>% italicize_levels() 
t2 <- tbl_regression(m2,  exponentiate = TRUE) %>% add_significance_stars(pattern = "{estimate} [{conf.low}, {conf.high}]{stars}", hide_ci = TRUE, hide_se = TRUE) %>%
  modify_header(estimate ~ "**OR (95% CI)**") %>% modify_footnote(estimate ~ NA, abbreviation = TRUE) %>% italicize_levels() 
t3 <- tbl_regression(m3,  exponentiate = TRUE) %>% add_significance_stars(pattern = "{estimate} [{conf.low}, {conf.high}]{stars}", hide_ci = TRUE, hide_se = TRUE) %>%
  modify_header(estimate ~ "**OR (95% CI)**") %>% modify_footnote(estimate ~ NA, abbreviation = TRUE) %>% italicize_levels() 

tbl <- tbl_merge(tbls = list(t1, t2, t3), tab_spanner = c("**Model A1**", "**Model A2**", "**Model A3**"))
tbl %>%
  as_gt() %>%
  gt::gtsave(filename = "./tables/reported_tableA1.html")



# ### Models of vaccine hesitancy (narrow definition) on trust, + demographic, + other variables -------------------
summary(m1 <- glm(hesitant1 ~ region3 + trust.eu.info + trust.gov.info + trust.health.info + trust.local.info + trust.doctors.info + 
                    trust.media.info + trust.web.info + trust.networks.info + trust.people.info, 
                  data = d1f, family='binomial'(link = "logit")))


summary(m2 <- glm(hesitant1 ~ region3 + trust.eu.info + trust.gov.info + trust.health.info + trust.local.info + trust.doctors.info + 
                    trust.media.info + trust.web.info + trust.networks.info + trust.people.info + 
                    age  + edu + male + city + empl, 
                  data = d1f, family='binomial'(link = "logit")))

summary(m3 <- glm(hesitant1 ~ region3 + trust.eu.info + trust.gov.info + trust.health.info + trust.local.info + trust.doctors.info + 
                    trust.media.info + trust.web.info + trust.networks.info + trust.people.info + 
                    age  + edu + male + city + empl +
                    v.safe + v.effective + know.ill.covid + was.ill.covid + fear.covid, 
                  data = d1f, family='binomial'(link = "logit")))

c(nobs(m1), nobs(m2), nobs(m3))
c(round(pR2(m1)[[4]],2),round(pR2(m2)[[4]],2),round(pR2(m3)[[4]],2))

t1 <- tbl_regression(m1,  exponentiate = TRUE) %>% add_significance_stars(pattern = "{estimate} [{conf.low}, {conf.high}]{stars}", hide_ci = TRUE, hide_se = TRUE) %>%
  modify_header(estimate ~ "**OR (95% CI)**") %>% modify_footnote(estimate ~ NA, abbreviation = TRUE) %>% italicize_levels() 
t2 <- tbl_regression(m2,  exponentiate = TRUE) %>% add_significance_stars(pattern = "{estimate} [{conf.low}, {conf.high}]{stars}", hide_ci = TRUE, hide_se = TRUE) %>%
  modify_header(estimate ~ "**OR (95% CI)**") %>% modify_footnote(estimate ~ NA, abbreviation = TRUE) %>% italicize_levels() 
t3 <- tbl_regression(m3,  exponentiate = TRUE) %>% add_significance_stars(pattern = "{estimate} [{conf.low}, {conf.high}]{stars}", hide_ci = TRUE, hide_se = TRUE) %>%
  modify_header(estimate ~ "**OR (95% CI)**") %>% modify_footnote(estimate ~ NA, abbreviation = TRUE) %>% italicize_levels() 

tbl <- tbl_merge(tbls = list(t1, t2, t3), tab_spanner = c("**Model A6**", "**Model A7**", "**Model A8**"))
tbl %>%
  as_gt() %>%
  gt::gtsave(filename = "./tables/reported_tableA4.html")
# ### Models of vaccine hesitancy (narrow definition) with interactions -------------------
summary(m4 <- glm(hesitant ~ trust.eu.info*region3 + trust.gov.info*region3 + trust.health.info*region3 + trust.local.info*region3 + trust.doctors.info*region3 + 
                    trust.media.info*region3 + trust.web.info *region3 + trust.networks.info*region3 + trust.people.info*region3 + 
                    age*region3  + edu*region3 + male*region3 + city*region3 + empl*region3 +
                    v.safe*region3 + v.effective*region3 + know.ill.covid*region3 + was.ill.covid*region3 + fear.covid*region3 + region3, 
                  data = d1f, family='binomial'))

summary(m5 <- update (m4, hesitant1 ~ .))

c(nobs(m4), nobs(m5))
c(round(pR2(m4)[[4]],2),round(pR2(m5)[[4]],2))

t4 <- tbl_regression(m4,  exponentiate = TRUE) %>% add_significance_stars(pattern = "{estimate} [{conf.low}, {conf.high}]{stars}", hide_ci = TRUE, hide_se = TRUE) %>%
  modify_header(estimate ~ "**OR (95% CI)**") %>% modify_footnote(estimate ~ NA, abbreviation = TRUE) %>% italicize_levels() 
t5 <- tbl_regression(m5,  exponentiate = TRUE) %>% add_significance_stars(pattern = "{estimate} [{conf.low}, {conf.high}]{stars}", hide_ci = TRUE, hide_se = TRUE) %>%
  modify_header(estimate ~ "**OR (95% CI)**") %>% modify_footnote(estimate ~ NA, abbreviation = TRUE) %>% italicize_levels() 


tbl <- tbl_merge(tbls = list(t4, t5), tab_spanner = c("**Model A9**", "**Model A10**"))
tbl %>%
  as_gt() %>%
  gt::gtsave(filename = "./tables/reported_tableA5.html") # use extensions .html .tex .ltx .rtf






