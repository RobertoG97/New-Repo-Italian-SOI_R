rm(list = ls()) 
dev.off()


###### Load data

df_191 <- read.csv(file.choose())# DATA_SOIR copia 3
df_191 <- df_191[1:191,4:17]
df_191 <- dplyr:: select(df_191, AGE, EDU, GENDER, SEXUAL_ORIENTATION=Sexual_Orientation, 
                         RELATIONSHIP, SOI_1, SOI_2,, SOI_3, SOI_4,SOI_5,SOI_6, SOI_7, SOI_8, SOI_9)


df_522 <- read.csv(file.choose())# DATA_SOIR522
df_522 <- df_522[, 2:15]
df <- rbind(df_191, df_522)

df$AGE <- as.numeric(df$AGE)

psych::describe(df$AGE)# age descriptive
length(which(df$GENDER=="M"))
length(which(df$GENDER=="F"))# 179 male, 524 female



######################Confirmatory factor analysis

library(lavaan)


### One factor model
mod_uni <- "SOI =~ NA*SOI_1 + SOI_1 + SOI_2 + SOI_3 + SOI_4 + SOI_5 + SOI_6 + SOI_7 + SOI_8 + SOI_9"
ordSOI_2 <- c("SOI_1", "SOI_2", "SOI_3","SOI_4", "SOI_5", "SOI_6", "SOI_7"
              , "SOI_8", "SOI_9")
cfa_uni <- cfa(mod_uni, df,  
               ordered =ordSOI_2, std.lv = TRUE)

summary(cfa_uni)

fitmeasures(cfa_uni, fit.measures = c("chisq", "df", "tli", "cfi", "rmsea", "srmr")
            , output = "matrix")

### 3 factor model

mod_tri <- "Beh=~NA*SOI_1 + SOI_1 + SOI_2 + SOI_3
            Att =~ NA*SOI_4 + SOI_4 + SOI_5 + SOI_6 
            Des =~ NA*SOI_7 + SOI_7 + SOI_8 + SOI_9
"
cfa_tri <- cfa(mod_tri, df,
               ordered = ordSOI_2, std.lv = TRUE)
summary(cfa_tri)


fitmeasures(cfa_tri, fit.measures = c("chisq", "df", "tli", "cfi", "rmsea", "srmr"), 
                                       output = "matrix")

### Bifactor model 

mod_bif  <- "SOI =~ NA*SOI_1 + SOI_1 + SOI_2 + SOI_3 + SOI_4 + SOI_5 + SOI_6 + SOI_7 + SOI_8 + SOI_9
            Beh=~NA*SOI_1 + SOI_1 + SOI_2 + SOI_3
            Att =~ NA*SOI_4 + SOI_4 + SOI_5 + SOI_6 
            Des =~ NA*SOI_7 + SOI_7 + SOI_8 + SOI_9
"
cfa_bif <- cfa(mod_bif, df, ordered = ordSOI_2, std.lv = TRUE, orthogonal = T)

summary(cfa_bif)
fitmeasures(cfa_bif, fit.measures = c("chisq", "df", "tli", "cfi", "rmsea", "srmr"), 
            output = "matrix")


anova(cfa_tri, cfa_uni, cfa_bif)# compare model



######################### Internal consistency

psych::omegaFromSem(cfa_bif)


# Omega total for total scores and subscales    0.92 0.82 0.83 0.87
# Omega general for total scores and subscales  0.73 0.41 0.69 0.35
# Omega group for total scores and subscales    0.19 0.41 0.15 0.52



######################### Test-retest reliability 

df.retest <- read.csv(file.choose(), stringsAsFactors = T)# TestRetest

x <- Hmisc::rcorr(as.matrix(df.retest[, c('SOI_BEH_t0', 'SOI_BEH_t1', "SOI_ATT_t0", 
                                        "SOI_ATT_t1","SOI_DES_t0", "SOI_DES_t1", 
                                        "SOI_t0_TOT", "SOI_t1_TOT")]), type = "spearman")

pander::pander(x$r, )
x$r# spearman rank coefficient 
x$P# p-value



######################### Measurement invariance 

library(rstatix)
FE <- which(df$GENDER=="F")
M <-  which(df$GENDER=="M")
freq_table(df[FE, 6]) #item 1, frequency of category  8 and 9 = 0
freq_table(df[FE, 7]) 
freq_table(df[FE, 8]) 
freq_table(df[FE, 9]) 
freq_table(df[FE, 10]) 
freq_table(df[FE, 11])
freq_table(df[FE, 12])
freq_table(df[FE, 13])
freq_table(df[FE, 14])

freq_table(df[M, 6]) 
freq_table(df[M, 7]) 
freq_table(df[M, 8]) 
freq_table(df[M, 9]) 
freq_table(df[M, 10]) 
freq_table(df[M, 11])
freq_table(df[M, 12])
freq_table(df[M, 13])
freq_table(df[M, 14])

## Collapsed  categories 
library(admisc)
df_lav <- df

df_lav$SOI_1 <- lapply(df_lav$SOI_1,FUN = function(X)recode(X, "1=1; 2=2; 3=3; 4=4;5=5; 6=6; 7=7; 8=7; 9=7"))## Collapsed  categories ITEM 1
df_lav$SOI_1 <- as.integer(df_lav$SOI_1)

## Compute models
library(semTools)

df_lav <- df_lav[complete.cases(df_lav$GENDER), ]
base_model.3 <-  measEq.syntax(configural.model = mod_tri,
                               data = df_lav,
                               ordered = ordSOI_2,parameterization = "delta", ID.fac = "std.lv",
                               ID.cat = "Wu.Estabrook.2016", 
                               group = "GENDER",group.equal = "configural")

base_model.3 <- as.character(base_model.3)

fit_base3 <- cfa(base_model.3, ordered = ordSOI_2, data = df_lav, group = "GENDER")
summary(fit_base3)
fitmeasures(fit_base3, fit.measures = c("chisq", "df", "tli", "cfi", "rmsea", "srmr"),
            output  = "matrix")   

thresholds <-   measEq.syntax(configural.model = mod_tri,
                              data = df_lav,
                              ordered = ordSOI_2,parameterization = "delta", ID.fac = "std.lv",
                              ID.cat = "Wu.Estabrook.2016", 
                              group = "GENDER",group.equal = c("thresholds"))

thresholds <- as.character(thresholds)

fit_thr <- cfa(thresholds, ordered = ordSOI_2, data = df_lav, group = "GENDER")
summary(fit_thr)
fitmeasures(fit_thr, fit.measures = c("chisq", "df", "tli", "cfi", "rmsea", "srmr"),
            output ="matrix")

lavTestLRT(fit_base3, fit_thr)

load_thre <-  measEq.syntax(configural.model = mod_tri,
                            data = df_lav,
                            ordered = ordSOI_2,parameterization = "delta", ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016", 
                            group = "GENDER",group.equal = c("thresholds", "loadings"))

load_thre <- as.character(load_thre)
fit_loth <- cfa(load_thre, ordered = ordSOI_2, data = df_lav,group = "GENDER")
fitmeasures(fit_loth, fit.measures = c("chisq", "df", "tli", "cfi", "rmsea", "srmr"),
            output ="matrix")

lavTestLRT(fit_loth, fit_thr, fit_base3)

######################### Correlation matrix
### Sub-scales' means score and Gender=factor
library(apaTables)
df$Beh <- rowMeans(df[, 8:10])
df$Att <-  rowMeans(df[, 11:13])
df$Des <-  rowMeans(df[, 14:16],na.rm = T)
df$SOI <- rowMeans(df[, 8:16])
df$GENDER <- as.character(df$GENDER)

df

apa_cor <- apa.cor.table(df[, c(1,2,3,4,5,15,16,17,18)], filename="Table2_COR.doc")# correlation matrix APA-style

######################### ANCOVA ~gender+age

### Test groups' equality
library(emmeans)
library(compareGroups)
library(ggplot2)
library(ggpubr)
df$RELATIONSHIP <- as.factor(df$RELATIONSHIP)
df$SEXUAL_ORIENTATION<- as.factor(df$SEXUAL_ORIENTATION)
m_VS_f2 <- compareGroups(GENDER~AGE+EDU+RELATIONSHIP+SEXUAL_ORIENTATION, data= df, method = c(AGE = NA))

des_t <- summary(m_VS_f2)

#write.csv(des_t$Sexual_Orientation, "SEx_OR.csv")

gender_T <- createTable(m_VS_f2, show.all = TRUE)
gender_T# no equality for AGEE



m_Beh <- lm(Beh~GENDER+AGE, data= df)#ANCOVA
summary(m_Beh)

res1 <- anova_test(Beh~AGE+GENDER, data= df)#ANCOVA for plot

pwc <- emmeans_test(Beh~GENDER, covariate = AGE,  
                    p.adjust.method = "bonferroni",data = df)

t <- t.test(Beh~GENDER, data = df)## t test 
adj <- attr(pwc, "emmeans")## adjusted means

round(t$estimate,1)== round(adj$emmean,1)## difference between adj and means of groups
## No differences= no age influence 

##Plot + eta square
pwc1 <- add_xy_position(pwc, x = "GENDER", fun = "mean_se" )
p <- ggline(get_emmeans(pwc1), x = "GENDER", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc1, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(res1, detailed = TRUE),
    caption = get_pwc_label(pwc1)
  )


effectsize::cohens_d(Beh~GENDER, data = df)#Cohen D

## Att
m_Att <- lm(Att~GENDER+AGE, data= df)#ANCOVA
summary(m_Att)

res2 <- anova_test(Att~AGE+GENDER, data= df)#ANCOVA for plot

pwc2 <- emmeans_test(Att~GENDER, covariate = AGE,  
                     p.adjust.method = "bonferroni",data = df)
t2 <- t.test(Att~GENDER, data = df)## t test 
adj2 <- attr(pwc2, "emmeans")## adjusted means

round(t2$estimate,1)== round(adj2$emmean,1)## difference between adj and means of groups
## No differences= no age influence 

### Plot + eta square
pwc2.0 <- add_xy_position(pwc2, x = "GENDER", fun = "mean_se" )
p2 <- ggline(get_emmeans(pwc2.0), x = "GENDER", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc2.0, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(res2, detailed = TRUE),
    caption = get_pwc_label(pwc2.0)
  )


effectsize::cohens_d(Att~GENDER, data = df)#Cohen D

## Des

m_Des <- lm(Des~GENDER+AGE, data= df)
summary(m_Des)

res3<- anova_test(Des~AGE+GENDER, data= df)#ANCOVA for plot

pwc3 <- emmeans_test(Des~GENDER, covariate = AGE,  
                     p.adjust.method = "bonferroni",data = df)

t3 <- t.test(Des~GENDER, data = df)## t test 
adj3 <- attr(pwc3, "emmeans")## adjusted means

round(t3$estimate,1)== round(adj3$emmean,1)## difference between adj and means of groups
## No differences= no age influence 


### Plot + eta square
pwc3.0 <- add_xy_position(pwc3, x = "GENDER", fun = "mean_se" )
p3 <- ggline(get_emmeans(pwc3.0), x = "GENDER", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc3.0, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(res3, detailed = TRUE),
    caption = get_pwc_label(pwc3.0))

effectsize::cohens_d(Des~GENDER, data = df)#Cohen D


m_SOI <- lm(SOI~GENDER+AGE, data= df)
summary(m_SOI)

res4 <- anova_test(SOI~AGE+GENDER, data= df)#ANCOVA for plot

pwc4 <- emmeans_test(SOI~GENDER, covariate = AGE,  
                     p.adjust.method = "bonferroni",data = df)
t4 <- t.test(SOI~GENDER, data = df)## t test 
adj4 <- attr(pwc4, "emmeans")## adjusted means

round(t4$estimate,1)== round(adj4$emmean,1)## difference between adj and means of groups
## No differences= no age influence 

### Plot + eta square
pwc4.0 <- add_xy_position(pwc4, x = "GENDER", fun = "mean_se" )
p4 <- ggline(get_emmeans(pwc4.0), x = "GENDER", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc3.0, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(res4, detailed = TRUE),
    caption = get_pwc_label(pwc4.0))

effectsize::cohens_d(SOI~GENDER, data = df)#cohen D

library(gridExtra)
grid.arrange(p,p2,p4,p3, top= "SOI differences between gender",
             widths = c(1, 1, 1),
             layout_matrix = rbind(c(3, 1, NA),
                                   c(3, 2, 4)))

######################### Interaction 

##Long data-frame
library(tidyr)
library(lme4)
library(lmerTest)

SOG <- c(1:713)
df <- cbind(df, SOG)
B <- rep("B", 2139)
A <- rep("A", 2139)
D <- rep("D", 2139)
SUB <- c(B,A,D)
df_l <- gather(df, Item, score, SOI_1:SOI_9)
df_l$Item <- factor(df_l$Item)
df_l$SOG <- factor(df_l$SOG)
df_l$GENDER <- factor(df_l$GENDER)
df_l$RELATIONSHIP <- factor(df_l$RELATIONSHIP)
df_l <- cbind(df_l, SUB)
df_l$SUB <- factor(df_l$SUB)

m1 <- lmer(score ~ 1 + SUB + GENDER + RELATIONSHIP + SUB:RELATIONSHIP + 
             SUB:GENDER + GENDER:RELATIONSHIP + SUB:GENDER:RELATIONSHIP + 
             (1 | Item) + (1 | SOG), data = df_l)

car::Anova(m1, type = "III") 
summary(m1)

library(sjPlot)
library(effects)
plot_model(m1, type = "pred", terms = "SUB")
plot_model(m1, type = "pred", terms = "GENDER")
plot_model(m1, type = "pred", terms = c("SUB", "RELATIONSHIP"))
plot_model(m1, type = "pred", terms = c("SUB", "GENDER"))

t.test(df$Beh~df$RELATIONSHIP)
t.test(df$Att~df$RELATIONSHIP)
t.test(df$Des~df$RELATIONSHIP)

effectsize::cohens_d(Beh~df$RELATIONSHIP, data = df)
effectsize::cohens_d(Att~df$RELATIONSHIP, data = df)
effectsize::cohens_d(Des~df$RELATIONSHIP, data = df)# .88, strong effect



























