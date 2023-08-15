options(OutDec=",", scipen=100, digits=3)

################## 1 Data ----
mydata <- read.csv2("/Users/Gustavo/Desktop/GitHUb/Data/dataset_logistic_regression.csv", h=T, sep=",", na.strings=c("","NA"))
mydata <- mydata[,!names(mydata) %in% c("X")]
mydata <-  mydata[complete.cases(mydata), ] 
mydata["EVOLUCAO"][mydata["EVOLUCAO"] == "Cura"] <- 0
mydata["EVOLUCAO"][mydata["EVOLUCAO"] == "Óbito"] <- 1
index <- 1:ncol(mydata)
mydata[ , index] <- lapply(mydata[ , index], as.numeric)



################## 2 Descriptive analysis ----
library(arsenal)
table_one <- tableby(EVOLUCAO ~ as.factor(COMPL_IDENT) + as.factor(COMPL_SINTO) +
                                as.factor(COMPL_RISCO) + as.factor(COMPL_HOSP_LAB) +
                                as.factor(COMPL_INVEST_EPID) + as.factor(COMPL_TOTAL) + 
                                as.factor(OPORT_IDENT) + as.factor(OPORT_NOTIFC) +
                                as.factor(OPORT_DIGITA) + as.factor(OPORT_COL) +
                                as.factor(OPORT_RES) + as.factor(OPORT_ENC), data=mydata, test=FALSE)
#write2word(table_one,
#           "C:\\Users\\Gustavo\\Downloads\\script_pw\\dissertacao\\supplementary_tab_3_descriptive_analysis.doc",
#           title="Table. Descriptive analysis")

# Plot
library(ggplot2)

a=ggplot(mydata, aes(x=as.factor(COMPL_IDENT), y=as.factor(COMPL_IDENT), fill=as.factor(EVOLUCAO))) + 
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count)))) +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_x_discrete(breaks=c(0,1), labels=c("Low","High")) +
  scale_fill_discrete(name="Outcome", labels=c("Discharge","Death")) +
  ylab("(%)") + xlab("Completeness: Sociodemographics") + theme_bw() + theme(legend.position="top"); a

b=ggplot(mydata, aes(x=as.factor(COMPL_SINTO), y=as.factor(COMPL_SINTO), fill=as.factor(EVOLUCAO))) + 
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count)))) +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_x_discrete(breaks=c(0,1), labels=c("Low","High")) +
  scale_fill_discrete(name="Outcome", labels=c("Discharge","Death")) +
  ylab("(%)") + xlab("Completeness: Signs and symptoms") + theme_bw() + theme(legend.position="top"); b

c=ggplot(mydata, aes(x=as.factor(COMPL_RISCO), y=as.factor(COMPL_RISCO), fill=as.factor(EVOLUCAO))) + 
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count)))) +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_x_discrete(breaks=c(0,1), labels=c("Low","High")) +
  scale_fill_discrete(name="Outcome", labels=c("Discharge","Death")) +
  ylab("(%)") + xlab("Completeness: Comorbidities") + theme_bw() + theme(legend.position="top"); c

d=ggplot(mydata, aes(x=as.factor(COMPL_HOSP_LAB), y=as.factor(COMPL_HOSP_LAB), fill=as.factor(EVOLUCAO))) + 
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count)))) +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_x_discrete(breaks=c(0,1), labels=c("Low","High")) +
  scale_fill_discrete(name="Outcome", labels=c("Discharge","Death")) +
  ylab("(%)") + xlab("Completeness: Hospital care") + theme_bw() + theme(legend.position="top"); d

e=ggplot(mydata, aes(x=as.factor(COMPL_INVEST_EPID), y=as.factor(COMPL_INVEST_EPID), fill=as.factor(EVOLUCAO))) + 
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count)))) +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_x_discrete(breaks=c(0,1), labels=c("Low","High")) +
  scale_fill_discrete(name="Outcome", labels=c("Discharge","Death")) +
  ylab("(%)") + xlab("Completeness: Case investigation") + theme_bw() + theme(legend.position="top"); e

f=ggplot(mydata, aes(x=as.factor(COMPL_TOTAL), y=as.factor(COMPL_TOTAL), fill=as.factor(EVOLUCAO))) + 
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count)))) +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_x_discrete(breaks=c(0,1), labels=c("Low","High")) +
  scale_fill_discrete(name="Outcome", labels=c("Discharge","Death")) +
  ylab("(%)") + xlab("Completeness: All variables") + theme_bw() + theme(legend.position="top"); f

g=ggplot(mydata, aes(x=as.factor(OPORT_IDENT), y=as.factor(OPORT_IDENT), fill=as.factor(EVOLUCAO))) + 
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count)))) +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_x_discrete(breaks=c(0,1), labels=c("Untimely","Timely")) +
  scale_fill_discrete(name="Outcome", labels=c("Discharge","Death")) +
  ylab("(%)") + xlab("Timeliness: Identification") + theme_bw() + theme(legend.position="top"); g

h=ggplot(mydata, aes(x=as.factor(OPORT_NOTIFC), y=as.factor(OPORT_NOTIFC), fill=as.factor(EVOLUCAO))) + 
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count)))) +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_x_discrete(breaks=c(0,1), labels=c("Untimely","Timely")) +
  scale_fill_discrete(name="Outcome", labels=c("Discharge","Death")) +
  ylab("(%)") + xlab("Timeliness: Communication") + theme_bw() + theme(legend.position="top"); h

i=ggplot(mydata, aes(x=as.factor(OPORT_DIGITA), y=as.factor(OPORT_DIGITA), fill=as.factor(EVOLUCAO))) + 
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count)))) +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_x_discrete(breaks=c(0,1), labels=c("Untimely","Timely")) +
  scale_fill_discrete(name="Outcome", labels=c("Discharge","Death")) +
  ylab("(%)") + xlab("Timeliness: Registration") + theme_bw() + theme(legend.position="top"); i

j=ggplot(mydata, aes(x=as.factor(OPORT_COL), y=as.factor(OPORT_COL), fill=as.factor(EVOLUCAO))) + 
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count)))) +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_x_discrete(breaks=c(0,1), labels=c("Untimely","Timely")) +
  scale_fill_discrete(name="Outcome", labels=c("Discharge","Death")) +
  ylab("(%)") + xlab("Timeliness: Sample collection") + theme_bw() + theme(legend.position="top"); j

k=ggplot(mydata, aes(x=as.factor(OPORT_RES), y=as.factor(OPORT_RES), fill=as.factor(EVOLUCAO))) + 
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count)))) +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_x_discrete(breaks=c(0,1), labels=c("Untimely","Timely")) +
  scale_fill_discrete(name="Outcome", labels=c("Discharge","Death")) +
  ylab("(%)") + xlab("Timeliness: Laboratory processing") + theme_bw() + theme(legend.position="top"); k

l=ggplot(mydata, aes(x=as.factor(OPORT_ENC), y=as.factor(OPORT_ENC), fill=as.factor(EVOLUCAO))) + 
  geom_bar(aes(y=after_stat(count)/sum(after_stat(count)))) +
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) +
  scale_x_discrete(breaks=c(0,1), labels=c("Untimely","Timely")) +
  scale_fill_discrete(name="Outcome", labels=c("Discharge","Death")) +
  ylab("(%)") + xlab("Timeliness: Case investigation") + theme_bw() + theme(legend.position="top"); l

library(ggpubr)
descriptive_analysis = ggarrange(a,b,c,d,e,f,g,h,i,j,k,l, ncol=3,nrow=4, common.legend=TRUE,
                              labels = c("A","B","C","D","E","F","G","H","I","J","K","L"))
ggsave("supplementary_fig_2_descriptive_analysis.png", descriptive_analysis, height=10, width=12, dpi=320)



################## 3 Bivariate Logistic Regression ----
# Models
biv_log_reg_COMPL_IDENT <- glm(mydata$EVOLUCAO ~ mydata$COMPL_IDENT, family="binomial")
biv_log_reg_COMPL_SINTO <- glm(mydata$EVOLUCAO ~ mydata$COMPL_SINTO, family="binomial")
biv_log_reg_COMPL_RISCO <- glm(mydata$EVOLUCAO ~ mydata$COMPL_RISCO, family="binomial")
biv_log_reg_COMPL_HOSP_LAB <- glm(mydata$EVOLUCAO ~ mydata$COMPL_HOSP_LAB, family="binomial")
biv_log_reg_COMPL_INVEST_EPID <- glm(mydata$EVOLUCAO ~ mydata$COMPL_INVEST_EPID, family="binomial")
biv_log_reg_COMPL_TOTAL <- glm(mydata$EVOLUCAO ~ mydata$COMPL_TOTAL, family="binomial")
biv_log_reg_OPORT_IDENT <- glm(mydata$EVOLUCAO ~ mydata$OPORT_IDENT, family="binomial")
biv_log_reg_OPORT_NOTIFC <- glm(mydata$EVOLUCAO ~ mydata$OPORT_NOTIFC, family="binomial")
biv_log_reg_OPORT_DIGITA <- glm(mydata$EVOLUCAO ~ mydata$OPORT_DIGITA, family = "binomial")
biv_log_reg_OPORT_COL <- glm(mydata$EVOLUCAO ~ mydata$OPORT_COL, family = "binomial")
biv_log_reg_OPORT_RES <- glm(mydata$EVOLUCAO ~ mydata$OPORT_RES, family = "binomial")
biv_log_reg_OPORT_ENC <- glm(mydata$EVOLUCAO ~ mydata$OPORT_ENC, family="binomial")

# Summary
summary(biv_log_reg_COMPL_IDENT)
exp(cbind(OR=coef(biv_log_reg_OPORT_ENC), confint.default(biv_log_reg_OPORT_ENC)))


# Plots
library(Rmisc)
library(ggplot2)
library(dplyr)

a=mydata %>%
  ggplot(aes(x=COMPL_IDENT, y=EVOLUCAO)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Completeness: Sociodemographics", y = "Probability of death") +
  ylim(0,1) + scale_x_continuous(breaks=c(0, 1), labels=c("Low","High")) + theme_bw(); a

b=mydata %>%
  ggplot(aes(as.numeric(COMPL_SINTO), as.numeric(EVOLUCAO))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Completeness: Signs and symptoms",y = "Probability of death") +
  ylim(0,1) + scale_x_continuous(breaks=c(0, 1), labels=c("Low","High")) + theme_bw(); b

c=mydata %>%
  ggplot(aes(as.numeric(COMPL_RISCO), as.numeric(EVOLUCAO))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Completeness: Comorbidities",y = "Probability of death") +
  ylim(0,1) + scale_x_continuous(breaks=c(0, 1), labels=c("Low","High")) + theme_bw(); c

d=mydata %>%
  ggplot(aes(as.numeric(COMPL_HOSP_LAB), as.numeric(EVOLUCAO))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Completeness: Hospital care",y = "Probability of death") +
  ylim(0,1) + scale_x_continuous(breaks=c(0, 1), labels=c("Low","High")) + theme_bw(); d

e=mydata %>%
  ggplot(aes(as.numeric(COMPL_INVEST_EPID), as.numeric(EVOLUCAO))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Completeness: Case investigation",y = "Probability of death") +
  ylim(0,1) + scale_x_continuous(breaks=c(0, 1), labels=c("Low","High")) + theme_bw(); e

f=mydata %>%
  ggplot(aes(as.numeric(COMPL_TOTAL), as.numeric(EVOLUCAO))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Completeness: All variables",y = "Probability of death") +
  ylim(0,1) + scale_x_continuous(breaks=c(0, 1), labels=c("Low","High")) + theme_bw(); f

g=mydata %>%
  ggplot(aes(as.numeric(OPORT_IDENT), as.numeric(EVOLUCAO))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Timeliness: Identification",y = "Probability of death") +
  ylim(0,1) + scale_x_continuous(breaks=c(0, 1), labels=c("Untimely","Timely")) + theme_bw(); g

h=mydata %>%
  ggplot(aes(as.numeric(OPORT_NOTIFC), as.numeric(EVOLUCAO))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Timeliness: Communication",y = "Probability of death") +
  ylim(0,1) + scale_x_continuous(breaks=c(0, 1), labels=c("Untimely","Timely")) + theme_bw(); h

i=mydata %>%
  ggplot(aes(as.numeric(OPORT_DIGITA), as.numeric(EVOLUCAO))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Timeliness: Registration",y = "Probability of death") +
  ylim(0,1) + scale_x_continuous(breaks=c(0, 1), labels=c("Untimely","Timely")) + theme_bw(); i

j=mydata %>%
  ggplot(aes(as.numeric(OPORT_COL), as.numeric(EVOLUCAO))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Timeliness: Sample collection",y = "Probability of death") +
  ylim(0,1) + scale_x_continuous(breaks=c(0, 1), labels=c("Untimely","Timely")) + theme_bw(); j

k=mydata %>%
  ggplot(aes(as.numeric(OPORT_RES), as.numeric(EVOLUCAO))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Timeliness: Laboratory processing",y = "Probability of death") +
  ylim(0,1) + scale_x_continuous(breaks=c(0, 1), labels=c("Untimely","Timely")) + theme_bw(); k

l=mydata %>%
  ggplot(aes(as.numeric(OPORT_ENC), as.numeric(EVOLUCAO))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Timeliness: Case investigation",y = "Probability of death") +
  ylim(0,1) + scale_x_continuous(breaks=c(0, 1), labels=c("Untimely","Timely")) + theme_bw(); l

library(ggpubr)
bivariate_logistic_regression = ggarrange(a,b,c,d,e,f,g,h,i,j,k,l, ncol=3,nrow=4,
                                          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"))
ggsave("supplementary_fig_3_bivariate_logistic_regression.png", bivariate_logistic_regression, height=10, width=12, dpi=320)



################## 4 Multivariate Logistic Regression ----
# Full model
mult_log_reg_model <- glm(mydata$EVOLUCAO ~
                            mydata$COMPL_IDENT + # 
                            mydata$COMPL_SINTO +
                            mydata$COMPL_RISCO +
                            mydata$COMPL_HOSP_LAB +
                            mydata$COMPL_INVEST_EPID +
                            mydata$COMPL_TOTAL + #
                            mydata$OPORT_IDENT +
                            mydata$OPORT_NOTIFC +
                            mydata$OPORT_DIGITA +
                            mydata$OPORT_COL +
                            mydata$OPORT_RES +
                            mydata$OPORT_ENC,
                          family="binomial")

options(scipen=100, digits=3)
summary(mult_log_reg_model)
exp(cbind(OR=coef(mult_log_reg_model), confint.default(mult_log_reg_model)))

# VIF
library(blorr)
VIF_mult_log_reg_model = blr_vif_tol(mult_log_reg_model)

VIF_mult_log_reg_model$Tipo = 1
VIF_mult_log_reg_model["Tipo"][VIF_mult_log_reg_model["Variable"] == "mydata$COMPL_IDENT"] <- "Completeness"
VIF_mult_log_reg_model["Tipo"][VIF_mult_log_reg_model["Variable"] == "mydata$COMPL_SINTO"] <- "Completeness"
VIF_mult_log_reg_model["Tipo"][VIF_mult_log_reg_model["Variable"] == "mydata$COMPL_RISCO"] <- "Completeness"
VIF_mult_log_reg_model["Tipo"][VIF_mult_log_reg_model["Variable"] == "mydata$COMPL_HOSP_LAB"] <- "Completeness"
VIF_mult_log_reg_model["Tipo"][VIF_mult_log_reg_model["Variable"] == "mydata$COMPL_INVEST_EPID"] <- "Completeness"
VIF_mult_log_reg_model["Tipo"][VIF_mult_log_reg_model["Variable"] == "mydata$COMPL_TOTAL"] <- "Completeness"
VIF_mult_log_reg_model["Tipo"][VIF_mult_log_reg_model["Variable"] == "mydata$OPORT_IDENT"] <- "Timeliness"
VIF_mult_log_reg_model["Tipo"][VIF_mult_log_reg_model["Variable"] == "mydata$OPORT_NOTIFC"] <- "Timeliness"
VIF_mult_log_reg_model["Tipo"][VIF_mult_log_reg_model["Variable"] == "mydata$OPORT_DIGITA"] <- "Timeliness"
VIF_mult_log_reg_model["Tipo"][VIF_mult_log_reg_model["Variable"] == "mydata$OPORT_COL"] <- "Timeliness"
VIF_mult_log_reg_model["Tipo"][VIF_mult_log_reg_model["Variable"] == "mydata$OPORT_RES"] <- "Timeliness"
VIF_mult_log_reg_model["Tipo"][VIF_mult_log_reg_model["Variable"] == "mydata$OPORT_ENC"] <- "Timeliness"

VIF_mult_log_reg_model["Variable"][VIF_mult_log_reg_model["Variable"] == "mydata$COMPL_IDENT"] <- "Sociodemographics"
VIF_mult_log_reg_model["Variable"][VIF_mult_log_reg_model["Variable"] == "mydata$COMPL_SINTO"] <- "Signs and symptoms"
VIF_mult_log_reg_model["Variable"][VIF_mult_log_reg_model["Variable"] == "mydata$COMPL_RISCO"] <- "Comorbidities"
VIF_mult_log_reg_model["Variable"][VIF_mult_log_reg_model["Variable"] == "mydata$COMPL_HOSP_LAB"] <- "Hospital care"
VIF_mult_log_reg_model["Variable"][VIF_mult_log_reg_model["Variable"] == "mydata$COMPL_INVEST_EPID"] <- "Case investigation"
VIF_mult_log_reg_model["Variable"][VIF_mult_log_reg_model["Variable"] == "mydata$COMPL_TOTAL"] <- "All variables"
VIF_mult_log_reg_model["Variable"][VIF_mult_log_reg_model["Variable"] == "mydata$OPORT_IDENT"] <- "Identification"
VIF_mult_log_reg_model["Variable"][VIF_mult_log_reg_model["Variable"] == "mydata$OPORT_NOTIFC"] <- "Communication"
VIF_mult_log_reg_model["Variable"][VIF_mult_log_reg_model["Variable"] == "mydata$OPORT_DIGITA"] <- "Registration"
VIF_mult_log_reg_model["Variable"][VIF_mult_log_reg_model["Variable"] == "mydata$OPORT_COL"] <- "Sample collection"
VIF_mult_log_reg_model["Variable"][VIF_mult_log_reg_model["Variable"] == "mydata$OPORT_RES"] <- "Laboratory processing"
VIF_mult_log_reg_model["Variable"][VIF_mult_log_reg_model["Variable"] == "mydata$OPORT_ENC"] <- "Case investigation "

library(ggplot2)
vif_grafico = ggplot(VIF_mult_log_reg_model, aes(x=reorder(Variable, VIF),
                                        y=VIF, fill=Tipo)) +
                     xlab("") + ylab("VIF Values") +
                     geom_bar(stat="identity") +
                     coord_flip() +
                     ylim(0,5) +
                     theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=12),
                           axis.text=element_text(size=14),
                           axis.title=element_text(face="bold", size=15)) +
                     geom_text(aes(label=round(VIF, digits=2)), hjust=1.5, colour="white") +
                     scale_fill_manual("legend", values=c("Completeness"="grey40", "Timeliness"="grey60"))
ggsave("vif_grafico.png", vif_grafico, height=10, width=12, dpi=320); vif_grafico

# Stepwise model
# Model without high VIF (>2.5) variables
mult_log_reg_model <- glm(mydata$EVOLUCAO ~
                            mydata$COMPL_IDENT +
                            mydata$COMPL_SINTO +
                            mydata$COMPL_RISCO +
                            mydata$COMPL_HOSP_LAB +
                            mydata$COMPL_INVEST_EPID +
                            #mydata$COMPL_TOTAL +
                            mydata$OPORT_IDENT +
                            mydata$OPORT_NOTIFC +
                            mydata$OPORT_DIGITA +
                            mydata$OPORT_COL +
                            mydata$OPORT_RES +
                            mydata$OPORT_ENC,
                          family="binomial")
library(MASS)
library(tidyverse)
step_mult_log_reg_model <- mult_log_reg_model %>% stepAIC(trace=TRUE, direction='both')

options(scipen=100, digits=3)
summary(step_mult_log_reg_model)
exp(cbind(OR=coef(step_mult_log_reg_model), confint.default(step_mult_log_reg_model)))

# McFadden's R-squared
library(DescTools)
PseudoR2(step_mult_log_reg_model)
with(summary(step_mult_log_reg_model), 1 - deviance/null.deviance)


# AUROC
library(pROC)
rocobj <- roc(mydata$EVOLUCAO, step_mult_log_reg_model$fitted.values, ci=TRUE)
rocobj$ci
auc <- round(auc(mydata$EVOLUCAO, step_mult_log_reg_model$fitted.values), 3)

roc_grafico = ggroc(rocobj, colour='steelblue', size=2) +
  xlab('False Positive Rate (1 - Specificity)') + ylab('True Positive Rate (Sensitivity)') +
  annotate(geom="text", x=0.4, y=0.4, label=paste0('AUROC = ', auc), color="black", size=5) +
  theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=12),
        axis.text=element_text(size=14),
        axis.title=element_text(face="bold", size=15))
ggsave("roc_curve.png", roc_grafico, height=10, width=10, dpi=320)


library(ggpubr)
fig_regressao_multivariada = ggarrange(roc_grafico, vif_grafico, ncol=1, nrow=2, labels=c("A", "B"))
ggsave("fig_regressao_multivariada.png", fig_regressao_multivariada, height=16, width=12, dpi=320)




# Tabela
# load package
library(gtsummary)
tbl_regression(step_mult_log_reg_model, exponentiate=TRUE)


