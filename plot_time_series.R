# Packages ----
library(readxl)
library(Rmisc)
library(ggplot2)
library(dplyr)
library("lubridate")
library(ggpubr)

# Data ----
mydata <- read_excel("/Users/Gustavo/Desktop/GitHUb/Data/dataset_temporal_analysis.xlsx")

# Figure 1. Completeness ----
a = ggplot(data=mydata) +
    scale_x_date(breaks=seq(as.Date("2009-04-01"), as.Date("2021-12-01"), by="12 months"), date_labels="%Y\n%b") +
    # Pandemia
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2020-02-01"), xmax=as.Date("2021-12-01"), ymin=-Inf, ymax=Inf) +
    geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", color="black", lwd=0.5) + # Pandemia
    # IC95% e Predição
    geom_ribbon(aes(x=as.Date(data), ymin=compl_ident_preds_upper, ymax=compl_ident_preds_lower), fill="#482677FF", alpha=0.25) + 
    geom_line(aes(x=as.Date(data), y=compl_ident_preds, colour="Expected"), na.rm=TRUE, size=1, linetype="dashed", alpha=1) + # Predito
    # Observado
    geom_line(aes(x=as.Date(data), y=compl_ident, colour="Observed"), na.rm=TRUE, size=1, alpha=1) + # Observado
    # Detalhes
    scale_color_manual(name="", values=c("Observed"="black","Expected"="#482677FF")) +
    scale_y_continuous(minor_breaks=seq(0,100,20), breaks=seq(0,100, by=20)) +
    xlab("") + ylab("Sociodemographic (%)") +
    theme_bw() + theme(legend.position="top", axis.text.x=element_blank()) + coord_cartesian(ylim=c(0, 110), expand=FALSE) +
    # Comentário
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2010-10-01"), xmax=as.Date("2018-04-01"), ymin=20, ymax=50) +
    geom_segment(aes(x=as.Date("2020-02-01"), y=35, xend=as.Date("2018-04-01"), yend=35), arrow=arrow(length=unit(0.03, "npc"))) +
    annotate("text", x=as.Date("2011-01-01"), y=45, size=3, label="During the COVID-19 pandemic:", fontface="bold", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=40, size=3, label="Expected month average: 94.09 (95% CI: 84.11, 103.65)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=35, size=3, label="Observed month average: 60.31", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=30, size=3, label="Relative change: -35.9% (95% CI: -46.06%, -25.3%)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=25, size=3, label="Bayesian one-sided tail-area probability p: <0.05", hjust="left", vjust="center"); a



b = ggplot(data=mydata) +
    scale_x_date(breaks=seq(as.Date("2009-04-01"), as.Date("2021-12-01"), by="12 months"), date_labels="%Y\n%b") +
    # Pandemia
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2020-02-01"), xmax=as.Date("2021-12-01"), ymin=-Inf, ymax=Inf) +
    geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", color="black", lwd=0.5) + # Pandemia
    # IC95% e Predição
    geom_ribbon(aes(x=as.Date(data), ymin=compl_sint_preds_upper, ymax=compl_sint_preds_lower), fill="#33638DFF", alpha=0.25) + 
    geom_line(aes(x=as.Date(data), y=compl_sint_preds, colour="Expected"), na.rm=TRUE, size=1, linetype="dashed", alpha=1) + # Predito
    # Observado
    geom_line(aes(x=as.Date(data), y=compl_sint, colour="Observed"), na.rm=TRUE, size=1, alpha=1) + # Observado
    # Detalhes
    scale_color_manual(name="", values=c("Observed"="black","Expected"="#33638DFF")) +
    scale_y_continuous(minor_breaks=seq(0,100,20), breaks=seq(0,100, by=20)) +
    xlab("") + ylab("Signs and symptoms (%)") +
    theme_bw() + theme(legend.position="top", axis.text.x=element_blank()) + coord_cartesian(ylim=c(0, 110), expand=FALSE) +
    # Comentário
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2010-10-01"), xmax=as.Date("2018-04-01"), ymin=20, ymax=50) +
    geom_segment(aes(x=as.Date("2020-02-01"), y=35, xend=as.Date("2018-04-01"), yend=35), arrow=arrow(length=unit(0.03, "npc"))) +
    annotate("text", x=as.Date("2011-01-01"), y=45, size=3, label="During the COVID-19 pandemic:", fontface="bold", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=40, size=3, label="Expected month average: 94.24 (95% CI: 82.53, 104.62)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=35, size=3, label="Observed month average: 67.36", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=30, size=3, label="Relative change: -28.52% (95% CI: -39.53%, -16.1%)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=25, size=3, label="Bayesian one-sided tail-area probability p: <0.05", hjust="left", vjust="center"); b



c = ggplot(data=mydata) +
    scale_x_date(breaks=seq(as.Date("2009-04-01"), as.Date("2021-12-01"), by="12 months"), date_labels="%Y\n%b") +
    # Pandemia
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2020-02-01"), xmax=as.Date("2021-12-01"), ymin=-Inf, ymax=Inf) +
    geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", color="black", lwd=0.5) + # Pandemia
    # IC95% e Predição
    geom_ribbon(aes(x=as.Date(data), ymin=compl_risco_preds_upper, ymax=compl_risco_preds_lower), fill="#1F968BFF", alpha=0.25) + 
    geom_line(aes(x=as.Date(data), y=compl_risco_preds, colour="Expected"), na.rm=TRUE, size=1, linetype="dashed", alpha=1) + # Predito
    # Observado
    geom_line(aes(x=as.Date(data), y=compl_risco, colour="Observed"), na.rm=TRUE, size=1, alpha=1) + # Observado
    # Detalhes
    scale_color_manual(name="", values=c("Observed"="black","Expected"="#1F968BFF")) +
    scale_y_continuous(minor_breaks=seq(0,100,20), breaks=seq(0,100, by=20)) +
    xlab("") + ylab("Comorbidities (%)") +
    theme_bw() + theme(legend.position="top", axis.text.x=element_blank()) + coord_cartesian(ylim=c(0, 110), expand=FALSE) +
    # Comentário
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2010-10-01"), xmax=as.Date("2018-04-01"), ymin=20, ymax=50) +
    geom_segment(aes(x=as.Date("2020-02-01"), y=5, xend=as.Date("2018-04-01"), yend=20), arrow=arrow(length=unit(0.03, "npc"))) +
    annotate("text", x=as.Date("2011-01-01"), y=45, size=3, label="During the COVID-19 pandemic:", fontface="bold", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=40, size=3, label="Expected month average: 44.67 (95% CI: 11.31, 77.15)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=35, size=3, label="Observed month average: 35.89", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=30, size=3, label="Relative change: -19.67% (95% CI: -92.38%, 55.03%)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=25, size=3, label="Bayesian one-sided tail-area probability p: 0.302", hjust="left", vjust="center"); c



d = ggplot(data=mydata) +
    scale_x_date(breaks=seq(as.Date("2009-04-01"), as.Date("2021-12-01"), by="12 months"), date_labels="%Y\n%b") +
    # Pandemia
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2020-02-01"), xmax=as.Date("2021-12-01"), ymin=-Inf, ymax=Inf) +
    geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", color="black", lwd=0.5) + # Pandemia
    # IC95% e Predição
    geom_ribbon(aes(x=as.Date(data), ymin=compl_hosp_preds_upper, ymax=compl_hosp_preds_lower), fill="#3CBB75FF", alpha=0.25) + 
    geom_line(aes(x=as.Date(data), y=compl_hosp_preds, colour="Expected"), na.rm=TRUE, size=1, linetype="dashed", alpha=1) + # Predito
    # Observado
    geom_line(aes(x=as.Date(data), y=compl_hosp, colour="Observed"), na.rm=TRUE, size=1, alpha=1) + # Observado
    # Detalhes
    scale_color_manual(name="", values=c("Observed"="black","Expected"="#3CBB75FF")) +
    scale_y_continuous(minor_breaks=seq(0,100,20), breaks=seq(0,100, by=20)) +
    xlab("") + ylab("Hospital care (%)") +
    theme_bw() + theme(legend.position="top", axis.text.x=element_blank()) + coord_cartesian(ylim=c(0, 110), expand=FALSE) +
    # Comentário
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2010-10-01"), xmax=as.Date("2018-04-01"), ymin=20, ymax=50) +
    geom_segment(aes(x=as.Date("2020-02-01"), y=35, xend=as.Date("2018-04-01"), yend=35), arrow=arrow(length=unit(0.03, "npc"))) +
    annotate("text", x=as.Date("2011-01-01"), y=45, size=3, label="During the COVID-19 pandemic:", fontface="bold", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=40, size=3, label="Expected month average: 96.81 (95% CI: 75.39, 118.54)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=35, size=3, label="Observed month average: 81.82", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=30, size=3, label="Relative change: -15.49% (95% CI: -37.93%, 6.65%)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=25, size=3, label="Bayesian one-sided tail-area probability p: 0.107", hjust="left", vjust="center"); d



e = ggplot(data=mydata) +
    scale_x_date(breaks=seq(as.Date("2009-04-01"), as.Date("2021-12-01"), by="12 months"), date_labels="%Y\n%b") +
    # Pandemia
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2020-02-01"), xmax=as.Date("2021-12-01"), ymin=-Inf, ymax=Inf) +
    geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", color="black", lwd=0.5) + # Pandemia
    # IC95% e Predição
    geom_ribbon(aes(x=as.Date(data), ymin=compl_epid_preds_upper, ymax=compl_epid_preds_lower), fill="#95D840FF", alpha=0.25) + 
    geom_line(aes(x=as.Date(data), y=compl_epid_preds, colour="Expected"), na.rm=TRUE, size=1, linetype="dashed", alpha=1) + # Predito
    # Observado
    geom_line(aes(x=as.Date(data), y=compl_epid, colour="Observed"), na.rm=TRUE, size=1, alpha=1) + # Observado
    # Detalhes
    scale_color_manual(name="", values=c("Observed"="black","Expected"="#95D840FF")) +
    scale_y_continuous(minor_breaks=seq(0,100,20), breaks=seq(0,100, by=20)) +
    xlab("Month") + ylab("Epidemiological investigation (%)") +
    theme_bw() + theme(legend.position="top") + coord_cartesian(ylim=c(0, 110), expand=FALSE) +
    # Comentário
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2010-10-01"), xmax=as.Date("2018-04-01"), ymin=20, ymax=50) +
    geom_segment(aes(x=as.Date("2020-02-01"), y=35, xend=as.Date("2018-04-01"), yend=35), arrow=arrow(length=unit(0.03, "npc"))) +
    annotate("text", x=as.Date("2011-01-01"), y=45, size=3, label="During the COVID-19 pandemic:", fontface="bold", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=40, size=3, label="Expected month average: 90.37 (95% CI: 78.4, 102.95)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=35, size=3, label="Observed month average: 87.82", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=30, size=3, label="Relative change: -2.82% (95% CI: -16.74%, 10.42%)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=25, size=3, label="Bayesian one-sided tail-area probability p: 0.374", hjust="left", vjust="center"); e


f = ggplot(data=mydata) +
    scale_x_date(breaks=seq(as.Date("2009-04-01"), as.Date("2021-12-01"), by="12 months"), date_labels="%Y\n%b") +
    # Pandemia
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2020-02-01"), xmax=as.Date("2021-12-01"), ymin=-Inf, ymax=Inf) +
    geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", color="black", lwd=0.5) + # Pandemia
    # IC95% e Predição
    geom_ribbon(aes(x=as.Date(data), ymin=compl_total_preds_upper, ymax=compl_total_preds_lower), fill="#DCE319FF", alpha=0.25) + 
    geom_line(aes(x=as.Date(data), y=compl_total_preds, colour="Expected"), na.rm=TRUE, size=1, linetype="dashed", alpha=1) + # Predito
    # Observado
    geom_line(aes(x=as.Date(data), y=compl_total, colour="Observed"), na.rm=TRUE, size=1, alpha=1) + # Observado
    # Detalhes
    scale_color_manual(name="", values=c("Observed"="black","Expected"="#DCE319FF")) +
    scale_y_continuous(minor_breaks=seq(0,100,20), breaks=seq(0,100, by=20)) +
    xlab("Month") + ylab("All variables (%)") +
    theme_bw() + theme(legend.position="top") + coord_cartesian(ylim=c(0, 110), expand=FALSE) +
    # Comentário
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2010-10-01"), xmax=as.Date("2018-04-01"), ymin=20, ymax=50) +
    geom_segment(aes(x=as.Date("2020-02-01"), y=35, xend=as.Date("2018-04-01"), yend=35), arrow=arrow(length=unit(0.03, "npc"))) +
    annotate("text", x=as.Date("2011-01-01"), y=45, size=3, label="During the COVID-19 pandemic:", fontface="bold", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=40, size=3, label="Expected month average: 74.66 (95% CI: 56.15, 92.96)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=35, size=3, label="Observed month average: 48.69", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=30, size=3, label="Relative change: -34.79% (95% CI: -59.31%, -10.0%)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=25, size=3, label="Bayesian one-sided tail-area probability p: <0.05", hjust="left", vjust="center"); f

completitude_fig = ggarrange(a,b,c,d,e,f, ncol=2,nrow=3, labels = c("A","B","C","D","E","F"), common.legend=TRUE) + bgcolor("White")
ggsave("completitude_fig.png", completitude_fig, height=10, width=12, dpi=320)



# Figure 2. Timeliness ----
g = ggplot(data=mydata) +
    scale_x_date(breaks=seq(as.Date("2009-04-01"), as.Date("2021-12-01"), by="12 months"), date_labels="%Y\n%b") +
    # Pandemia
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2020-02-01"), xmax=as.Date("2021-12-01"), ymin=-Inf, ymax=Inf) +
    geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", color="black", lwd=0.5) + # Pandemia
    # IC95% e Predição
    geom_ribbon(aes(x=as.Date(data), ymin=oport_ident_preds_upper, ymax=oport_ident_preds_lower), fill="#482677FF", alpha=0.25) + 
    geom_line(aes(x=as.Date(data), y=oport_ident_preds, colour="Expected"), na.rm=TRUE, size=1, linetype="dashed", alpha=1) + # Predito
    # Observado
    geom_line(aes(x=as.Date(data), y=oport_ident, colour="Observed"), na.rm=TRUE, size=1, alpha=1) + # Observado
    # Detalhes
    scale_color_manual(name="", values=c("Observed"="black","Expected"="#482677FF")) +
    scale_y_continuous(minor_breaks=seq(0,100,20), breaks=seq(0,100, by=20)) +
    xlab("") + ylab("Identification (%)") +
    theme_bw() + theme(legend.position="top", axis.text.x=element_blank()) + coord_cartesian(ylim=c(0, 110), expand=FALSE) +
    # Comentário
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2010-10-01"), xmax=as.Date("2018-04-01"), ymin=75, ymax=105) +
    geom_segment(aes(x=as.Date("2020-02-01"), y=90, xend=as.Date("2018-04-01"), yend=90), arrow=arrow(length=unit(0.03, "npc"))) +
    annotate("text", x=as.Date("2011-01-01"), y=100, size=3, label="During the COVID-19 pandemic:", fontface="bold", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=95, size=3, label="Expected month average: 12.78 (95% CI: -1.12, 26.48)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=90, size=3, label="Observed month average: 12.27", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=85, size=3, label="Relative change: -3.96% (95% CI: -111.19%, 104.78%)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=80, size=3, label="Bayesian one-sided tail-area probability p: 0.487", hjust="left", vjust="center"); g



h = ggplot(data=mydata) +
    scale_x_date(breaks=seq(as.Date("2009-04-01"), as.Date("2021-12-01"), by="12 months"), date_labels="%Y\n%b") +
    # Pandemia
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2020-02-01"), xmax=as.Date("2021-12-01"), ymin=-Inf, ymax=Inf) +
    geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", color="black", lwd=0.5) + # Pandemia
    # IC95% e Predição
    geom_ribbon(aes(x=as.Date(data), ymin=oport_notific_preds_upper, ymax=oport_notific_preds_lower), fill="#33638DFF", alpha=0.25) + 
    geom_line(aes(x=as.Date(data), y=oport_notific_preds, colour="Expected"), na.rm=TRUE, size=1, linetype="dashed", alpha=1) + # Predito
    # Observado
    geom_line(aes(x=as.Date(data), y=oport_notific, colour="Observed"), na.rm=TRUE, size=1, alpha=1) + # Observado
    # Detalhes
    scale_color_manual(name="", values=c("Observed"="black","Expected"="#33638DFF")) +
    scale_y_continuous(minor_breaks=seq(0,100,20), breaks=seq(0,100, by=20)) +
    xlab("") + ylab("Communication (%)") +
    theme_bw() + theme(legend.position="top", axis.text.x=element_blank()) + coord_cartesian(ylim=c(0, 110), expand=FALSE) +
    # Comentário
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2010-10-01"), xmax=as.Date("2018-04-01"), ymin=75, ymax=105) +
    geom_segment(aes(x=as.Date("2020-02-01"), y=90, xend=as.Date("2018-04-01"), yend=90), arrow=arrow(length=unit(0.03, "npc"))) +
    annotate("text", x=as.Date("2011-01-01"), y=100, size=3, label="During the COVID-19 pandemic:", fontface="bold", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=95, size=3, label="Expected month average: 53.43 (95% CI: 30.52, 77.27)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=90, size=3, label="Observed month average: 66.4", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=85, size=3, label="Relative change: +24.27% (95% CI: -20.35%, 67.16%)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=80, size=3, label="Bayesian one-sided tail-area probability p: 0.135", hjust="left", vjust="center"); h



i = ggplot(data=mydata) +
    scale_x_date(breaks=seq(as.Date("2009-04-01"), as.Date("2021-12-01"), by="12 months"), date_labels="%Y\n%b") +
    # Pandemia
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2020-02-01"), xmax=as.Date("2021-12-01"), ymin=-Inf, ymax=Inf) +
    geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", color="black", lwd=0.5) + # Pandemia
    # IC95% e Predição
    geom_ribbon(aes(x=as.Date(data), ymin=oport_digita_preds_upper, ymax=oport_digita_preds_lower), fill="#1F968BFF", alpha=0.25) + 
    geom_line(aes(x=as.Date(data), y=oport_digita_preds, colour="Expected"), na.rm=TRUE, size=1, linetype="dashed", alpha=1) + # Predito
    # Observado
    geom_line(aes(x=as.Date(data), y=oport_digita, colour="Observed"), na.rm=TRUE, size=1, alpha=1) + # Observado
    # Detalhes
    scale_color_manual(name="", values=c("Observed"="black","Expected"="#1F968BFF")) +
    scale_y_continuous(minor_breaks=seq(0,100,20), breaks=seq(0,100, by=20)) +
    xlab("") + ylab("Registration (%)") +
    theme_bw() + theme(legend.position="top", axis.text.x=element_blank()) + coord_cartesian(ylim=c(0, 110), expand=FALSE) +
    # Comentário
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2010-10-01"), xmax=as.Date("2018-04-01"), ymin=75, ymax=105) +
    geom_segment(aes(x=as.Date("2020-02-01"), y=90, xend=as.Date("2018-04-01"), yend=90), arrow=arrow(length=unit(0.03, "npc"))) +
    annotate("text", x=as.Date("2011-01-01"), y=100, size=3, label="During the COVID-19 pandemic:", fontface="bold", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=95, size=3, label="Expected month average: 37.71 (95% CI: 6.98, 70.39)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=90, size=3, label="Observed month average: 46.0", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=85, size=3, label="Relative change: +21.96% (95% CI: -64.69%, 103.45%)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=80, size=3, label="Bayesian one-sided tail-area probability p: 0.321", hjust="left", vjust="center"); i


j = ggplot(data=mydata) +
    scale_x_date(breaks=seq(as.Date("2009-04-01"), as.Date("2021-12-01"), by="12 months"), date_labels="%Y\n%b") +
    # Pandemia
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2020-02-01"), xmax=as.Date("2021-12-01"), ymin=-Inf, ymax=Inf) +
    geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", color="black", lwd=0.5) + # Pandemia
    # IC95% e Predição
    geom_ribbon(aes(x=as.Date(data), ymin=oport_col_preds_upper, ymax=oport_col_preds_lower), fill="#3CBB75FF", alpha=0.25) + 
    geom_line(aes(x=as.Date(data), y=oport_col_preds, colour="Expected"), na.rm=TRUE, size=1, linetype="dashed", alpha=1) + # Predito
    # Observado
    geom_line(aes(x=as.Date(data), y=oport_col, colour="Observed"), na.rm=TRUE, size=1, alpha=1) + # Observado
    # Detalhes
    scale_color_manual(name="", values=c("Observed"="black","Expected"="#3CBB75FF")) +
    scale_y_continuous(minor_breaks=seq(0,100,20), breaks=seq(0,100, by=20)) +
    xlab("") + ylab("Sample collection (%)") +
    theme_bw() + theme(legend.position="top", axis.text.x=element_blank()) + coord_cartesian(ylim=c(0, 110), expand=FALSE) +
    # Comentário
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2010-10-01"), xmax=as.Date("2018-04-01"), ymin=75, ymax=105) +
    geom_segment(aes(x=as.Date("2020-02-01"), y=90, xend=as.Date("2018-04-01"), yend=90), arrow=arrow(length=unit(0.03, "npc"))) +
    annotate("text", x=as.Date("2011-01-01"), y=100, size=3, label="During the COVID-19 pandemic:", fontface="bold", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=95, size=3, label="Expected month average: 29.54 (95% CI: 16.09, 44.05)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=90, size=3, label="Observed month average: 30.52", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=85, size=3, label="Relative change: +3.31% (95% CI: -45.81%, 48.84%)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=80, size=3, label="Bayesian one-sided tail-area probability p: 0.467", hjust="left", vjust="center"); j


k = ggplot(data=mydata) +
    scale_x_date(breaks=seq(as.Date("2009-04-01"), as.Date("2021-12-01"), by="12 months"), date_labels="%Y\n%b") +
    # Pandemia
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2020-02-01"), xmax=as.Date("2021-12-01"), ymin=-Inf, ymax=Inf) +
    geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", color="black", lwd=0.5) + # Pandemia
    # IC95% e Predição
    geom_ribbon(aes(x=as.Date(data), ymin=oport_res_preds_upper, ymax=oport_res_preds_lower), fill="#95D840FF", alpha=0.25) + 
    geom_line(aes(x=as.Date(data), y=oport_res_preds, colour="Expected"), na.rm=TRUE, size=1, linetype="dashed", alpha=1) + # Predito
    # Observado
    geom_line(aes(x=as.Date(data), y=oport_res, colour="Observed"), na.rm=TRUE, size=1, alpha=1) + # Observado
    # Detalhes
    scale_color_manual(name="", values=c("Observed"="black","Expected"="#95D840FF")) +
    scale_y_continuous(minor_breaks=seq(0,100,20), breaks=seq(0,100, by=20)) +
    xlab("Month") + ylab("Laboratory processing (%)") +
    theme_bw() + theme(legend.position="top") + coord_cartesian(ylim=c(0, 110), expand=FALSE) +
    # Comentário
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2010-10-01"), xmax=as.Date("2018-04-01"), ymin=75, ymax=105) +
    geom_segment(aes(x=as.Date("2020-02-01"), y=105, xend=as.Date("2018-04-01"), yend=100), arrow=arrow(length=unit(0.03, "npc"))) +
    annotate("text", x=as.Date("2011-01-01"), y=100, size=3, label="During the COVID-19 pandemic:", fontface="bold", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=95, size=3, label="Expected month average: 47.44 (95% CI: 7.89, 87.52)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=90, size=3, label="Observed month average: 66.09", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=85, size=3, label="Relative change: +39.31%% (95% CI: -45.19%, 122.69%)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=80, size=3, label="Bayesian one-sided tail-area probability p: 0.198", hjust="left", vjust="center"); k


l = ggplot(data=mydata) +
    scale_x_date(breaks=seq(as.Date("2009-04-01"), as.Date("2021-12-01"), by="12 months"), date_labels="%Y\n%b") +
    # Pandemia
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2020-02-01"), xmax=as.Date("2021-12-01"), ymin=-Inf, ymax=Inf) +
    geom_vline(xintercept = as.numeric(as.Date("2020-02-01")), linetype="dashed", color="black", lwd=0.5) + # Pandemia
    # IC95% e Predição
    geom_ribbon(aes(x=as.Date(data), ymin=oport_enc_preds_upper, ymax=oport_enc_preds_lower), fill="#DCE319FF", alpha=0.25) + 
    geom_line(aes(x=as.Date(data), y=oport_enc_preds, colour="Expected"), na.rm=TRUE, size=1, linetype="dashed", alpha=1) + # Predito
    # Observado
    geom_line(aes(x=as.Date(data), y=oport_enc, colour="Observed"), na.rm=TRUE, size=1, alpha=1) + # Observado
    # Detalhes
    scale_color_manual(name="", values=c("Observed"="black","Expected"="#DCE319FF")) +
    scale_y_continuous(minor_breaks=seq(0,100,20), breaks=seq(0,100, by=20)) +
    xlab("Month") + ylab("Epidemiologic investigation (%)") +
    theme_bw() + theme(legend.position="top") + coord_cartesian(ylim=c(0, 110), expand=FALSE) +
    # Comentário
    annotate("rect", fill="black", alpha=0.15, xmin=as.Date("2010-10-01"), xmax=as.Date("2018-04-01"), ymin=20, ymax=50) +
    geom_segment(aes(x=as.Date("2020-02-01"), y=35, xend=as.Date("2018-04-01"), yend=35), arrow=arrow(length=unit(0.03, "npc"))) +
    annotate("text", x=as.Date("2011-01-01"), y=45, size=3, label="During the COVID-19 pandemic:", fontface="bold", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=40, size=3, label="Expected month average: 81.0 (95% CI: 53.45, 107.62)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=35, size=3, label="Observed month average: 78.44", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=30, size=3, label="Relative change: -3.15% (95% CI: -36.02%, 30.86%)", hjust="left", vjust="center") +
    annotate("text", x=as.Date("2011-01-01"), y=25, size=3, label="Bayesian one-sided tail-area probability p: 0.457", hjust="left", vjust="center"); l

oportunidade_fig = ggarrange(g,h,i,j,k,l, ncol=2,nrow=3, labels = c("G","H","I","J","K","L"), common.legend=TRUE) + bgcolor("White")
ggsave("oportunidade_fig.png", oportunidade_fig, height=10, width=12, dpi=320)