# Packages ----
library(data.table)
library(arsenal)

# Data ----
df <- fread(file='/Users/Gustavo/Desktop/GitHUb/Data/dataset_chi_squared.csv', encoding='UTF-8')
df$REGIAO_NOT <- factor(df$REGIAO_NOT, levels=c('Norte','Nordeste','Centro-Oeste','Sudeste','Sul'))
df$PORTE <- factor(df$PORTE, levels=c('Pequeno','Médio','Grande'))
df$FRONTEIRA <- factor(df$FRONTEIRA, levels=c('Sim','Não'))

# Pearson Chi-Squared Test ----
COMPL_IDENT = tableby(COMPL_IDENT ~ REGIAO_NOT + PORTE + FRONTEIRA, data=df, test=TRUE, cat.stats="countrowpct")
COMPL_SINTO = tableby(COMPL_SINTO ~ REGIAO_NOT + PORTE + FRONTEIRA, data=df, test=TRUE, cat.stats="countrowpct")
COMPL_RISCO = tableby(COMPL_RISCO ~ REGIAO_NOT + PORTE + FRONTEIRA, data=df, test=TRUE, cat.stats="countrowpct")
COMPL_HOSP_LAB = tableby(COMPL_HOSP_LAB ~ REGIAO_NOT + PORTE + FRONTEIRA, data=df, test=TRUE, cat.stats="countrowpct")
COMPL_INVEST_EPID = tableby(COMPL_INVEST_EPID ~ REGIAO_NOT + PORTE + FRONTEIRA, data=df, test=TRUE, cat.stats="countrowpct")
COMPL_TOTAL = tableby(COMPL_TOTAL ~ REGIAO_NOT + PORTE + FRONTEIRA, data=df, test=TRUE, cat.stats="countrowpct")

OPORT_IDENT = tableby(OPORT_IDENT ~ REGIAO_NOT + PORTE + FRONTEIRA, data=df, test=TRUE, cat.stats="countrowpct")
OPORT_NOTIFC = tableby(OPORT_NOTIFC ~ REGIAO_NOT + PORTE + FRONTEIRA, data=df, test=TRUE, cat.stats="countrowpct")
OPORT_DIGITA = tableby(OPORT_DIGITA ~ REGIAO_NOT + PORTE + FRONTEIRA, data=df, test=TRUE, cat.stats="countrowpct")
OPORT_COL = tableby(OPORT_COL ~ REGIAO_NOT + PORTE + FRONTEIRA, data=df, test=TRUE, cat.stats="countrowpct")
OPORT_RES = tableby(OPORT_RES ~ REGIAO_NOT + PORTE + FRONTEIRA, data=df, test=TRUE, cat.stats="countrowpct")
OPORT_ENC = tableby(OPORT_ENC ~ REGIAO_NOT + PORTE + FRONTEIRA, data=df, test=TRUE, cat.stats="countrowpct")

summary(OPORT_ENC, text=TRUE)
