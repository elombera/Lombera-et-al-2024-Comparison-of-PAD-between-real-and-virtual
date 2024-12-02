library(tidyverse)
library(lme4)
library(nlme)
library(sjPlot)
library(MuMIn)
library(lmerTest)
library(ggstatsplot)
library(ggpubr)
library(effects)
library(flextable)
library(webshot)
library(officer)


rm(list=ls())

load("./data_final.RData")
figures_folder = "figuras"
results_tbl = filter(results_tbl, subject != "S020")
results_tbl_bias = filter(results_tbl_bias, subject != "S020")
results_tbl_dis = filter(results_tbl_dis, subject != "S020")

results_tbl$condition <- factor(results_tbl$condition,
                                levels = c("Real speakers", "Individualized HRTF",
                                           "Non-individualized HRTF","Simulated HRTF",
                                           "Incongruous room","Closed headphones"))


results_tbl_bias$condition <- factor(results_tbl_bias$condition,
                                     levels = c("Real speakers", "Individualized HRTF",
                                                "Non-individualized HRTF","Simulated HRTF",
                                                "Incongruous room","Closed headphones"))
results_tbl$logperc_dist_sem = log10(results_tbl$sem_perc_dist) 
#EXPERIMENT 1 -----
    ### Distance ----
results_tbl_EXP1 = filter(results_tbl, condition != "Incongruous room" & condition != "Closed headphones")

results_tbl_EXP1_pob = results_tbl_EXP1 %>%
  group_by(target_distance,condition) %>%
  summarise(logperc_dist_pob = mean(logperc_dist),
            semlogperc_dist_pob = sd(logperc_dist),
            perc_dist_pob = mean(perc_dist),
            sd_respuesta_pob = sd(perc_dist),
            sem_respuesta_pob = sd(perc_dist)/sqrt(n()),
            div = sqrt(n()))

#Log
m.Dist <-  lmer(log10(perc_dist) ~ log10(target_distance)*condition+(1+log10(target_distance)|subject)+(0+condition|subject),
                 data = results_tbl_EXP1)

extract_stats(ggcoefstats(m.Dist))
r.squaredGLMM(m.Dist)
anova(m.Dist)


eq_real <- substitute("REAL"~~~italic(y) == k %.% italic(X)^italic((a)), 
                  list(k = round(10^coef(summary(m.Dist))[1], digits = 2),
                       a = round(coef(summary(m.Dist))[2], digits = 2)))
eq_ind <- substitute("I-BRIR"~~~italic(y) == k %.% italic(X)^italic((a)), 
                  list(k = round(10^(coef(summary(m.Dist))[1]+coef(summary(m.Dist))[3]), digits = 2),
                       a = round(coef(summary(m.Dist))[2]+coef(summary(m.Dist))[6], digits = 2)))
eq_noind <- substitute("NI-BRIR"~~~italic(y) == k %.% italic(X)^italic((a)), 
                  list(k = round(10^(coef(summary(m.Dist))[1]+coef(summary(m.Dist))[4]), digits = 2),
                       a = round(coef(summary(m.Dist))[2]+coef(summary(m.Dist))[7], digits = 2)))
eq_sim <- substitute("S-BRIR"~~~italic(y) == k %.% italic(X)^italic((a)), 
                  list(k = round(10^(coef(summary(m.Dist))[1]+coef(summary(m.Dist))[5]), digits = 2),
                       a = round(coef(summary(m.Dist))[2]+coef(summary(m.Dist))[8], digits = 2)))

Final.FixedS<-effect(c("log10(target_distance)*condition"), m.Dist)
Final.FixedS<-as.data.frame(Final.FixedS)


Final.Fixed<-effect(c("log10(target_distance)*condition"), m.Dist)
Final.Fixed<-as.data.frame(Final.Fixed)

cbPalette <- c("#000000","#E69F00","#CC79A7", "#0072B2","#999999","#009E73", "#D55E00", "#CC79A7", "#F0E442")
cblegend = c(eq_real,eq_ind,
             eq_noind,eq_sim)
Final.Fixed.Plot <-ggplot(data = Final.Fixed, aes(x = target_distance, y =10^fit, group=condition))+
  geom_abline(intercept = 0, slope = 1, linetype=3) +
  geom_line(aes(color=condition,linetype= condition), size=1.2)+
  geom_pointrange(data = results_tbl_EXP1_pob, mapping= aes(x = target_distance, y =10^logperc_dist_pob,
                                                            ymin =10^logperc_dist_pob-sem_respuesta_pob,
                                                            ymax=10^logperc_dist_pob+sem_respuesta_pob,
                                                            shape = condition,group=condition, color=condition,fill=condition),
             size = 1,alpha = .8,position = position_jitter(width = 0.1,seed = 42))+

  scale_x_continuous(name="Distance source (m)",breaks=c(0,2,4,6), limits = c(0.3,6.3)) +
  scale_y_continuous(name="Perceived distance (m)",breaks=c(0,2,4,6), limits = c(0.3,6.3)) +

  scale_shape_manual(values=c(19, 15, 17, 25), labels = cblegend)+
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid"), labels = cblegend)+
  scale_colour_manual(values = cbPalette, labels = cblegend) + 
  scale_fill_manual(values = cbPalette, labels = cblegend) + 
  theme_bw()+
  guides(shape = guide_legend(override.aes = list(size = 1)))+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(text=element_text(size=10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.title=element_blank(),
        legend.key.size = unit(.1, "cm"),
        legend.position = c(0.01, .74),
        legend.justification = c(0,0),
        legend.key.width = unit(.9,"cm"))
Final.Fixed.Plot



    ###Intercept----
results_tbl_intercept_EXP1 = filter(results_tbl_bias, condition != "Incongruous room" & condition != "Closed headphones")
results_tbl_intercept_EXP1$intercept = 0
results_tbl_intercept_EXP1[results_tbl_intercept_EXP1$condition == "Non-individualized HRTF",]$intercept = coef(m.Dist)$subject[[2]]
results_tbl_intercept_EXP1[results_tbl_intercept_EXP1$condition == "Individualized HRTF",]$intercept = coef(m.Dist)$subject[[2]]+coef(m.Dist)$subject[[4]]
results_tbl_intercept_EXP1[results_tbl_intercept_EXP1$condition == "Real speakers",]$intercept = coef(m.Dist)$subject[[2]]+coef(m.Dist)$subject[[5]]
results_tbl_intercept_EXP1[results_tbl_intercept_EXP1$condition == "Simulated HRTF",]$intercept = coef(m.Dist)$subject[[2]]+coef(m.Dist)$subject[[6]]


fig.intercept <- results_tbl_intercept_EXP1 %>%
  group_by(subject, condition) %>%
  summarise(Mintercept = mean(intercept),
            N = n()) %>%
  ungroup() %>%
  ggplot(aes(x = condition, 
             y = Mintercept, 
             colour = condition, 
             fill = condition,
             shape = condition)) +
  geom_point(alpha = 0.4, 
             position = position_jitterdodge(jitter.width = .3,
                                             jitter.height = 0,
                                             dodge.width = 1 )) +
  geom_violin(alpha=.2)+
  scale_shape_manual(values=c(19, 15, 17, 25))+
  geom_abline(slope = 0, 
              intercept = 0, 
              alpha = 0.5, 
              linetype = "dashed") +
  stat_summary(fun.data = "mean_se", 
               geom = "point", 
               alpha = 1,
               size = 3,
               position = position_dodge(width = 1.5)) +
  stat_summary(fun.data = "mean_se", 
               geom = "linerange",  
               size=1, 
               position = position_dodge(width = 1)) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  # scale_x_discrete(name="Condition",labels = c("Real\nspeakers","Individualized\nHRTF","Non\nindividualized\nHRTF","Simulated\nHRTF"))+
  scale_x_discrete(name="Condition",labels = c("R","I","NI","S"))+
  scale_y_continuous(name="Intercept",breaks=c(-1.5,-1,-0.5, 0,0.5,1),
                     limits = c(-1.2,1.1),expand= c(0,0)) +
  expand_limits(y = c(0.5, 13.5))+
  annotate("text", x = 3, y = 0.73,  label = "***", size = 4) +
  annotate("segment", x = 2, xend = 4, y = 0.7, yend = 0.7, colour = "black", size=.3, alpha=1,)+
  annotate("text", x = 3.5, y = 0.53,  label = "***", size = 4) +
  annotate("segment", x = 3, xend = 4, y = 0.5, yend = 0.5, colour = "black", size=.3, alpha=1,)+
  annotate("text", x = 2.5, y = 0.93,  label = "***", size = 4) +
  annotate("segment", x = 1, xend = 4, y = 0.9, yend = 0.9, colour = "black", size=.3, alpha=1,)+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(legend.position = "none",axis.text.x=element_text(size=10))

fig.intercept
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Sesgo_POB+ind+pvalor", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot = fig.biasRelVR, width=14, height=5, units="cm", limitsize=FALSE, dpi=200)


    ###Signed bias----
results_tbl_bias_EXP1 = filter(results_tbl_bias, condition != "Incongruous room" & condition != "Closed headphones")

table.signed_bias <- results_tbl_bias_EXP1 %>%
  group_by(subject, condition) %>%
  summarise(Msignedbias = mean(signed_bias_ind),
            N = n())
m.signedBias <- lm(Msignedbias ~ condition, 
                   data = table.signed_bias)
extract_stats(ggcoefstats(m.signedBias))
anov = anova(m.signedBias)
anov

t.test(filter(table.signed_bias, condition == "Real speakers")$Msignedbias,
       filter(table.signed_bias, condition == "Simulated HRTF")$Msignedbias,
       paired = FALSE)
t.test(filter(table.signed_bias, condition == "Individualized HRTF")$Msignedbias,
       filter(table.signed_bias, condition == "Simulated HRTF")$Msignedbias,
       paired = FALSE)
t.test(filter(table.signed_bias, condition == "Simulated HRTF")$Msignedbias,
       filter(table.signed_bias, condition == "Non-individualized HRTF")$Msignedbias,
       paired = FALSE)

fig.signed_bias <- results_tbl_bias_EXP1 %>%
  group_by(subject, condition) %>%
  summarise(Msignedbias = mean(signed_bias_ind),
            N = n()) %>%
  ungroup() %>%
  ggplot(aes(x = condition, 
             y = Msignedbias, 
             colour = condition, 
             fill = condition,
             shape = condition)) +
  geom_point(alpha = 0.4, 
             position = position_jitterdodge(jitter.width = .3,
                                             jitter.height = 0,
                                             dodge.width = 1 )) +
  geom_violin(alpha=.2)+
  scale_shape_manual(values=c(19, 15, 17, 25))+
  geom_abline(slope = 0, 
              intercept = 0, 
              alpha = 0.5, 
              linetype = "dashed") +
  stat_summary(fun.data = "mean_se", 
               geom = "point", 
               alpha = 1,
               size = 3,
               position = position_dodge(width = 1.5)) +
  stat_summary(fun.data = "mean_se", 
               geom = "linerange",  
               size=1, 
               position = position_dodge(width = 1)) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  # scale_x_discrete(name="Condition",labels = c("Real\nspeakers","Individualized\nHRTF","Non\nindividualized\nHRTF","Simulated\nHRTF"))+
  scale_x_discrete(name="Condition",labels = c("R","I","NI","S"))+
  scale_y_continuous(name="Signed bias",breaks=c(-1.5,-1,-0.5, 0,0.5,1,1.5,2),
                     limits = c(-1,2.51),expand= c(0,0)) +
  expand_limits(y = c(0.5, 13.5))+
  annotate("text", x = 3, y = 1.96,  label = "***", size = 4) +
  annotate("segment", x = 2, xend = 4, y = 1.9, yend = 1.9, colour = "black", size=.3, alpha=1,)+
  annotate("text", x = 3.5, y = 1.58,  label = "***", size = 4) +
  annotate("segment", x = 3, xend = 4, y = 1.52, yend = 1.52, colour = "black", size=.3, alpha=1,)+
  annotate("text", x = 2.5, y = 2.34,  label = "***", size = 4) +
  annotate("segment", x = 1, xend = 4, y = 2.28, yend = 2.28, colour = "black", size=.3, alpha=1,)+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(legend.position = "none",axis.text.x = element_text(size=10))

fig.signed_bias


# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Sesgo_POB+ind+pvalor", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot = fig.biasRelVR, width=14, height=5, units="cm", limitsize=FALSE, dpi=200)

  ##FIGURE 1-----
# Figure.1 =  ggarrange(Final.Fixed.Plot,
#                       ggarrange(fig.intercept+rremove("x.title"),
#                                 fig.signed_bias+rremove("x.title"), 
#                                 ncol = 2, labels = c("B", "C")),
#                       labels = "A",
#                       ncol = 1, nrow = 2,widths = c(1,1), heights =c(1,.7))
# 
# Figure.1
# 
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "FIGURE1a", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot = Figure.1, width=16, height=18, units="cm", limitsize=FALSE, dpi=400)
# 

Figure.1 =  ggarrange(Final.Fixed.Plot,
                      ggarrange(fig.intercept+rremove("x.title"),
                                fig.signed_bias, 
                                nrow = 2, labels = c("B", "C")),
                      labels = "A",
                      nrow = 1, ncol = 2,widths = c(2,1))

Figure.1

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "FIGURE1", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot = Figure.1, width=16, height=9, units="cm", limitsize=FALSE, dpi=400)


# Figure.2 =  ggarrange(Final.Fixed.Plot,
#                       ggarrange(fig.signed_bias+rremove("x.title") ,
#                                 fig.intercept,
#                                 nrow = 2, labels = c("B", "C")),
#                       labels = "A",
#                       ncol = 2, nrow = 1,widths = c(2,1))
# 
# Figure.2
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "FIGURE2", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot = Figure.2, width=10, height=8, units="cm", limitsize=FALSE, dpi=400)


#EXPERIMENT 2 -----
    ### Distance ----
results_tbl_EXP2 = filter(results_tbl, perc_dist != 0, condition == "Incongruous room" | condition == "Non-individualized HRTF")

results_tbl_EXP2_pob = results_tbl_EXP2 %>%
  group_by(target_distance,condition) %>%
  summarise(logperc_dist_pob = mean(logperc_dist,na.rm = TRUE),
            sdlogperc_dist_pob = sd(logperc_dist),
            semlogperc_dist_pob = sd(logperc_dist)/sqrt(n()),
            perc_dist_pob = mean(perc_dist),
            sd_respuesta_pob = sd(perc_dist),
            sem_respuesta_pob = sd(perc_dist)/sqrt(n()))




m.Dist <-  lmer(log10(perc_dist) ~ log10(target_distance)*condition+(1+log10(target_distance)|subject)+(0+condition|subject),
                data = results_tbl_EXP2)

extract_stats(ggcoefstats(m.Dist))
r.squaredGLMM(m.Dist)
anova(m.Dist)


# results_tbl_EXP2$Modelfitted3<-predict(m.Dist)
# # Todos los sujetos
# FittedlmPlot1 <-ggplot()+
#   facet_grid(subject ~ condition, labeller=label_both)+
#   geom_line(data = results_tbl_EXP2, aes(x = target_distance, y = 10^Modelfitted3))+
#   geom_point(data = results_tbl_EXP2, aes(x = target_distance, y =perc_dist, group=subject,colour = subject), size=3)+
#   #  coord_cartesian(ylim = c(.03,.074))+ 
#   geom_abline(intercept = 0, slope = 1, linetype=3) +
#   xlab("Targent_distance")+ylab("Perceived_distance")+theme_bw()
# FittedlmPlot1
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "xxxx", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot = FittedlmPlot1, width=14, height=60, units="cm", limitsize=FALSE, dpi=200)
# 



eq_noind_con <- substitute("NI-BRIR (CR)"~~~italic(y) == k %.% italic(X)^italic((a)), 
                       list(k = round(10^(coef(summary(m.Dist))[1]), digits = 2),
                            a = round(coef(summary(m.Dist))[2], digits = 2)))
eq_noind_inc <- substitute("NI-BRIR (IR)"~~~italic(y) == k %.% italic(X)^italic((a)), 
                     list(k = round(10^(coef(summary(m.Dist))[1]+coef(summary(m.Dist))[3]), digits = 2),
                          a = round(coef(summary(m.Dist))[2]+coef(summary(m.Dist))[4], digits = 2)))
eq_2001 <- substitute("Zahorik 2001"~~~italic(y) == k %.% italic(X)^italic((a)), 
                           list(k = 1.17,
                                a = 0.45))
# eq_2002 <- substitute("Zahorik 2002"~~~italic(y) == k %.% italic(X)^italic((a)), 
#                            list(k = 1.32,
#                                 a = 0.39))
mi_df = data.frame("target_distance" = seq(1,6, by =1))
mi_df$fit = 1.17*(mi_df$target_distance^0.45)
mi_df$fit = log10(mi_df$fit)
mi_df$logperc_dist_pob = mi_df$fit
# mi_df$funcion02 = 1.32*(mi_df$target_distance^0.39)
mi_df$condition = "Zahorik (2001)"

# 
# extract_stats(ggcoefstats(m.Dist))
# r.squaredGLMM(m.Dist)
# anova(m.Dist)
# 
# as.data.frame(valores,funciony)

Final.Fixed<-effect(c("log10(target_distance)*condition"), m.Dist)
Final.Fixed<-as.data.frame(Final.Fixed)

Final.Fixed = merge(Final.Fixed,filter(Final.FixedS,condition == "Simulated HRTF"), all = TRUE )
Final.Fixed = merge(Final.Fixed,mi_df, all = TRUE )
results_tbl_EXP2_pob = merge(results_tbl_EXP2_pob,filter(results_tbl_EXP1_pob, condition == "Simulated HRTF"), all = TRUE )
results_tbl_EXP2_pob = merge(results_tbl_EXP2_pob,mi_df, all = TRUE )

results_tbl_EXP2_pob$condition <- factor(results_tbl_EXP2_pob$condition,
                                levels = c("Non-individualized HRTF","Incongruous room","Simulated HRTF","Zahorik (2001)"))

results_tbl_EXP2_pob[results_tbl_EXP2_pob$condition == "Simulated HRTF",]$sem_respuesta_pob = 0

cbPalette <- c("#CC79A7","#009E73", "#0072B2", "black","#999999", "#D55E00", "#CC79A7", "#F0E442")
cblegend = c(eq_noind_con,eq_noind_inc,eq_sim,eq_2001)
Final.Fixed.Plot <-ggplot(data = Final.Fixed, aes(x = target_distance, y =10^fit, group=condition))+
  geom_abline(intercept = 0, slope = 1, linetype=3) +
  geom_line(aes(color=condition,linetype= condition), size=1.2)+
  geom_point(data = results_tbl_EXP2_pob, mapping= aes(x = target_distance, y =10^logperc_dist_pob,
                                                            shape = condition,group=condition, color=condition,
                                                            fill=condition),
                  size = 4,alpha =1)+
  geom_errorbar(data = results_tbl_EXP2_pob, mapping= aes(x = target_distance, y =10^logperc_dist_pob,
                                                            ymin =10^logperc_dist_pob-sem_respuesta_pob,
                                                            ymax=10^logperc_dist_pob+sem_respuesta_pob,
                                                            shape = condition,group=condition, color=condition,
                                                            fill=condition),
                  size = 1,alpha =.8,width=0,show.legend=FALSE)+
  # geom_pointrange(data = results_tbl_EXP2_pob, mapping= aes(x = target_distance, y =10^logperc_dist_pob,
  #                                                      ymin =10^logperc_dist_pob-sem_respuesta_pob,
  #                                                      ymax=10^logperc_dist_pob+sem_respuesta_pob,
  #                                                      shape = condition,group=condition, color=condition,
  #                                                      fill=condition),
  #            size = 1,alpha =.8,position = position_jitter(width = 0.25),show.legend=FALSE)+
  scale_x_continuous(name="Distance source (m)", limits = c(0.3,6.3)) +
  scale_y_continuous(name="Perceived distance (m)",   limits = c(0.3,6.3)) +

  scale_shape_manual(values=c(17,23,NA,NA), labels = cblegend)+
  # scale_linetype_manual(values=c("dashed", "dotdash"), labels = cblegend)+
  scale_linetype_manual(values=c("solid", "solid","solid","dashed"), labels = cblegend)+
  scale_colour_manual(values = cbPalette, labels = cblegend) + 
  scale_fill_manual(values = cbPalette, labels = cblegend) + 
  # geom_line(data = Final.Fixed2, mapping = aes(x = target_distance, y =fit, group = condition), size = 2)+
  # geom_line(data = mi_df, mapping = aes(x = target_distance, y =funcion01, group = condition), size = 1,linetype = "dotted")+
  # geom_line(data = filter(Final.FixedS, condition == "Simulated HRTF"), mapping = aes(x = target_distance, y =10^fit, group = condition), size = 1,color =  "#0072B2",linetype = "dashed")+
  # geom_line(data = mi_df, mapping = aes(x = target_distance, y =funcion02, group = condition), size = 1, linetype = "dashed")+
  # geom_text(x = 0.2, y = 5.1, label = as.character(as.expression(eq_2001)), hjust = 0, nudge_x =  0, parse = TRUE, size = 3.2, color = "#000000")+
  # geom_text(x = 0.2, y = 4.7, label = as.character(as.expression(eq_2002)), hjust = 0, nudge_x =  0,parse = TRUE, size = 3.2, color = "#000000")+
  # geom_text(x = 0, y = 3.7, label = as.character(as.expression(eq_real)), hjust = 0, nudge_x =  0, parse = TRUE, size = 3.2, color = "#009E73")+
  # geom_text(x = 0, y = 3.4, label = as.character(as.expression(eq_sim)), hjust = 0, nudge_x =  0, parse = TRUE, size = 3.2, color =  "#0072B2")+
  theme_bw()+
  # guides(fill = guide_legend(text.vjust = .1) )+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(text=element_text(size=10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
                legend.title=element_blank(),
        legend.key.size = unit(.1, "cm"),
                legend.position = c(0.01, .78),
                legend.justification = c(0,0),
                legend.key.width = unit(.8,"cm"))
Final.Fixed.Plot

    ###Intercept----
results_tbl_intercept_EXP2 = filter(results_tbl_bias, condition == "Incongruous room" | condition == "Non-individualized HRTF")
results_tbl_intercept_EXP2$intercept = 0
results_tbl_intercept_EXP2[results_tbl_intercept_EXP2$condition == "Non-individualized HRTF",]$intercept = coef(m.Dist)$subject[[2]][c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,21,22,23,24)]
results_tbl_intercept_EXP2[results_tbl_intercept_EXP2$condition == "Incongruous room",]$intercept = coef(m.Dist)$subject[[2]][c(1,2,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,21,22,23,24,25)]+coef(m.Dist)$subject[[4]][c(1,2,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,21,22,23,24,25)]

fig.intercept <- results_tbl_intercept_EXP2 %>%
  group_by(subject, condition) %>%
  summarise(Mintercept = mean(intercept),
            N = n()) %>%
  ungroup() %>%
  ggplot(aes(x = condition, 
             y = Mintercept, 
             colour = condition, 
             fill = condition,
             shape = condition)) +
  # scale_colour_colorblind()+
  # scale_fill_colorblind()+
  geom_point(alpha = 0.4, 
             position = position_jitterdodge(jitter.width = .3,
                                             jitter.height = 0,
                                             dodge.width = 1 )) +
  geom_violin(alpha=.2)+
  scale_shape_manual(values=c(17, 23))+
  geom_abline(slope = 0, 
              intercept = 0, 
              alpha = 0.5, 
              linetype = "dashed") +
  stat_summary(fun.data = "mean_se", 
               geom = "point", 
               alpha = 1,
               size = 3,
               position = position_dodge(width = 1.5)) +
  stat_summary(fun.data = "mean_se", 
               geom = "linerange",  
               size=1, 
               position = position_dodge(width = 1)) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  scale_x_discrete(name="Condition",labels = c("CR","IR"))+
  scale_y_continuous(name="Intercept",breaks=c(-1.5,-1,-0.5, 0,0.5,1),
                     limits = c(-1.2,1),expand= c(0,0)) +
  expand_limits(y = c(0.5, 13.5))+
  annotate("text", x = 1.5, y = 0.78,  label = "*", size = 5) +
  annotate("segment", x = 1, xend = 2, y = 0.75, yend = 0.75, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(legend.position = "none",axis.text=element_text(size=10))

fig.intercept
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Sesgo_POB+ind+pvalor", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot = fig.biasRelVR, width=14, height=5, units="cm", limitsize=FALSE, dpi=200)


    ###Signed bias----
results_tbl_bias_EXP2 = filter(results_tbl_bias,  condition == "Incongruous room" | condition == "Non-individualized HRTF")

fig.signed_bias <- results_tbl_bias_EXP2 %>%
  group_by(subject, condition) %>%
  summarise(Msignedbias = mean(signed_bias_ind),
            N = n()) %>%
  ungroup() %>%
  ggplot(aes(x = condition, 
             y = Msignedbias, 
             colour = condition, 
             fill = condition,
             shape = condition)) +
  # scale_colour_colorblind()+
  # scale_fill_colorblind()+
  geom_point(alpha = 0.4, 
             position = position_jitterdodge(jitter.width = .3,
                                             jitter.height = 0,
                                             dodge.width = 1 )) +
  geom_violin(alpha=.2)+
  scale_shape_manual(values=c(17, 23))+
  geom_abline(slope = 0, 
              intercept = 0, 
              alpha = 0.5, 
              linetype = "dashed") +
  stat_summary(fun.data = "mean_se", 
               geom = "point", 
               alpha = 1,
               size = 3,
               position = position_dodge(width = 1.5)) +
  stat_summary(fun.data = "mean_se", 
               geom = "linerange",  
               size=1, 
               position = position_dodge(width = 1)) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  scale_x_discrete(name="Condition",labels = c("CR","IR"))+
  scale_y_continuous(name="Signed bias",breaks=c(-1.5,-1,-0.5, 0,0.5,1,1.5,2),
                     limits = c(-1,2),expand= c(0,0)) +
  expand_limits(y = c(0.5, 13.5))+
  annotate("text", x = 1.5, y = 1.73,  label = "*", size = 5) +
  annotate("segment", x = 1, xend = 2, y = 1.7, yend = 1.7, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(legend.position = "none",axis.text=element_text(size=10))

fig.signed_bias
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Sesgo_POB+ind+pvalor", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot = fig.biasRelVR, width=14, height=5, units="cm", limitsize=FALSE, dpi=200)

table.signed_bias <- results_tbl_bias_EXP2 %>%
  group_by(subject, condition) %>%
  summarise(Msignedbias = mean(signed_bias_ind),
            N = n())
m.signedBias <- lm(Msignedbias ~ condition, 
                   data = table.signed_bias)
extract_stats(ggcoefstats(m.signedBias))
anov = anova(m.signedBias)
anov

  ##FIGURE 2-----
#HORIZONTAL
# Figure.2 =  ggarrange(Final.Fixed.Plot,
#                       ggarrange(fig.intercept+rremove("x.title") ,
#                                 fig.signed_bias,
#                                 nrow = 2, labels = c("B", "C")),
#                       labels = "A",
#                       ncol = 2, nrow = 1,widths = c(2,1))
# 
# Figure.2
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "FIGURE2", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot = Figure.2, width=10, height=8, units="cm", limitsize=FALSE, dpi=400)

#VERTICAL
Figure.2 =  ggarrange(Final.Fixed.Plot,
                      ggarrange(fig.intercept+rremove("x.title") ,
                                fig.signed_bias+rremove("x.title"),
                                ncol = 2, labels = c("B", "C")),
                      labels = "A",
                      ncol = 1, nrow = 2,heights =c(2,1))

Figure.2
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "FIGURE2", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot = Figure.2, width=8, height=14, units="cm", limitsize=FALSE, dpi=400)

# p2 = ggplot(data = Final.Fixed, aes(x = target_distance, y =10^fit, group=condition))+
#   geom_line(aes(color=condition,linetype= condition), size=1.2)+
#   geom_point(data = results_tbl_EXP2_pob, mapping= aes(x = target_distance, y =10^logperc_dist_pob,
#                                                        shape = condition,group=condition, color=condition,
#                                                        fill=condition),
#              size = 4,alpha =1)+
#   geom_errorbar(data = results_tbl_EXP2_pob, mapping= aes(x = target_distance, y =10^logperc_dist_pob,
#                                                           ymin =10^logperc_dist_pob-sem_respuesta_pob,
#                                                           ymax=10^logperc_dist_pob+sem_respuesta_pob,
#                                                           shape = condition,group=condition, color=condition,
#                                                           fill=condition),
#                 size = 1,alpha =.8,width=0,show.legend=FALSE)+
#   scale_x_continuous(name="Distance source (m)", limits = c(0,0)) +
#   scale_y_continuous(name="Perceived distance (m)",   limits = c(0,0)) +
#   scale_shape_manual(values=c(17,23,NA,NA), labels = cblegend)+
#   # scale_linetype_manual(values=c("dashed", "dotdash"), labels = cblegend)+
#   scale_linetype_manual(values=c("solid", "solid","solid","dashed"), labels = cblegend)+
#   scale_colour_manual(values = cbPalette, labels = cblegend) + 
#   scale_fill_manual(values = cbPalette, labels = cblegend) + 
#   theme_pubr(base_size = 10, margin = TRUE)+
#   easy_remove_axes()+
#   theme(text=element_text(size=10),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         legend.title=element_blank(),
#         legend.position = c(0.37, .96),
#         legend.justification = c(.35,.8),
#         legend.key.width = unit(1.3,"cm"))
# p2
# # p2 = p2 
# #   theme_void()+
# #   theme(legend.position = c(0.5,0.5),
# #         legend.key.size = unit(1.1, "cm"),
# #         legend.title = element_blank())+
# #   guides(colour = guide_legend(override.aes = list(size=8)))
# 
# Figure.2 =  ggarrange(Final.Fixed.Plot,
#                       ggarrange(p2,
#                                 ggarrange(fig.intercept,fig.signed_bias, ncol = 2, labels = c("B", "C")), 
#                                 ncol = 1,nrow = 2, heights =c(.6,1.4), labels = c("","")),
#                       labels = "A",
#                       ncol = 2, nrow = 1,widths = c(1,1))
# 
# Figure.2
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "FIGURE2", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot = Figure.2, width=16, height=11, units="cm", limitsize=FALSE, dpi=400)


#EXPERIMENT 3 ----
results_tbl_dis$condition <- factor(results_tbl_dis$condition,
                                     levels = c("Individualized HRTF",
                                                "Non-individualized HRTF","Simulated HRTF",
                                                "Incongruous room"))

    ### Discrimination - loudspeakers -----
results_tbl_dis_mean = filter(results_tbl_dis, target_distance == 1)%>%
  group_by(subject,condition) %>%
  summarise(Merror = sum(Error)/n())


m.disError <- lm(Merror ~ condition, 
                   data = results_tbl_dis_mean)
extract_stats(ggcoefstats(m.disError))
anova(m.disError)

t.test(filter(results_tbl_dis_mean, condition == "Incongruous room")$Merror,
       filter(results_tbl_dis_mean, condition == "Simulated HRTF")$Merror,
       paired = FALSE)
t.test(filter(results_tbl_dis_mean, condition == "Incongruous room")$Merror,
       filter(results_tbl_dis_mean, condition == "Individualized HRTF")$Merror,
       paired = FALSE)
t.test(filter(results_tbl_dis_mean, condition == "Incongruous room")$Merror,
       filter(results_tbl_dis_mean, condition == "Non-individualized HRTF")$Merror,
       paired = FALSE)


t.test(filter(results_tbl_dis_mean, condition == "Individualized HRTF")$Merror,
       filter(results_tbl_dis_mean, condition == "Non-individualized HRTF")$Merror,
       paired = FALSE)
t.test(filter(results_tbl_dis_mean, condition == "Individualized HRTF")$Merror,
       filter(results_tbl_dis_mean, condition == "Simulated HRTF")$Merror,
       paired = FALSE)
t.test(filter(results_tbl_dis_mean, condition == "Simulated HRTF")$Merror,
       filter(results_tbl_dis_mean, condition == "Non-individualized HRTF")$Merror,
       paired = FALSE)


cbPalette <- c("#E69F00","#CC79A7", "#0072B2","#009E73", "#D55E00", "#CC79A7", "#F0E442")
fig.error_s <- ggplot(results_tbl_dis_mean, aes(x = condition,
                                        y = Merror*100, color = condition, fill = condition, shape = condition))+
  geom_point(alpha = 0.4,
             position = position_jitterdodge(jitter.width = .3,
                                             jitter.height = 0,
                                             dodge.width = 1 )) +
  scale_shape_manual(values=c(15, 17, 25, 23))+
  geom_abline(slope = 0, 
              intercept = 0, 
              alpha = 0.5, 
              linetype = "dashed") +
  stat_summary(fun.data = "mean_se", 
               geom = "bar", 
               alpha = .4,
               size = .5,
               position = position_dodge(width = 1.5)) +
  stat_summary(fun.data = "mean_se", 
               geom = "linerange",  
               size=1, 
               position = position_dodge(width = 1)) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  scale_x_discrete(name="Condition",labels = c("I","NI-CR",
                                                       "S","NI-IR"))+
  scale_y_continuous(name="% Error discrimination",  limits = c(0,140)) +
  expand_limits(y = c(0.5, 13.5))+
  ggtitle("Stimuli via speaker")+
  annotate("text", x = 3.5, y = 106,  label = "**", size = 5) +
  annotate("segment", x = 3, xend = 4, y = 104, yend = 104, colour = "black", size=.3, alpha=1,)+
  annotate("text", x = 3, y = 116,  label = "**", size = 5) +
  annotate("segment", x = 2, xend = 4, y = 114, yend = 114, colour = "black", size=.3, alpha=1,)+
  annotate("text", x = 2.5, y = 126,  label = "***", size = 5) +
  annotate("segment", x = 1, xend = 4, y = 124, yend = 124, colour = "black", size=.3, alpha=1,)+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(legend.position = "none",axis.text=element_text(size=8),
        plot.title = element_text(size=11.5,hjust = 0.5,face="bold"))
fig.error_s

    ### Discrimination - Headphones ----
results_tbl_dis_mean = filter(results_tbl_dis, target_distance == 2)%>%
  group_by(subject,condition) %>%
  summarise(Merror = sum(Error)/n())


m.disError <- lm(Merror ~ condition, 
                 data = results_tbl_dis_mean)
extract_stats(ggcoefstats(m.disError))
anova(m.disError)

t.test(filter(results_tbl_dis_mean, condition == "Incongruous room")$Merror,
       filter(results_tbl_dis_mean, condition == "Simulated HRTF")$Merror,
       paired = FALSE)
t.test(filter(results_tbl_dis_mean, condition == "Individualized HRTF")$Merror,
       filter(results_tbl_dis_mean, condition == "Incongruous room")$Merror,
       paired = FALSE)
t.test(filter(results_tbl_dis_mean, condition == "Non-individualized HRTF")$Merror,
       filter(results_tbl_dis_mean, condition == "Incongruous room")$Merror,
       paired = FALSE)


t.test(filter(results_tbl_dis_mean, condition == "Individualized HRTF")$Merror,
       filter(results_tbl_dis_mean, condition == "Non-individualized HRTF")$Merror,
       paired = FALSE)
t.test(filter(results_tbl_dis_mean, condition == "Individualized HRTF")$Merror,
       filter(results_tbl_dis_mean, condition == "Simulated HRTF")$Merror,
       paired = FALSE)
t.test(filter(results_tbl_dis_mean, condition == "Non-individualized HRTF")$Merror,
       filter(results_tbl_dis_mean, condition == "Simulated HRTF")$Merror,
       paired = FALSE)



cbPalette <- c("#E69F00","#CC79A7", "#0072B2","#009E73", "#D55E00", "#CC79A7", "#F0E442")
fig.error_h <- ggplot(results_tbl_dis_mean, aes(x = condition,
                                                y = Merror*100, color = condition, fill = condition, shape = condition))+
  geom_point(alpha = 0.4,
             position = position_jitterdodge(jitter.width = .3,
                                             jitter.height = 0,
                                             dodge.width = 1 )) +
  scale_shape_manual(values=c(15, 17, 25, 23))+
  geom_abline(slope = 0, 
              intercept = 0, 
              alpha = 0.5, 
              linetype = "dashed") +
  stat_summary(fun.data = "mean_se", 
               geom = "bar", 
               alpha = .4,
               size = .5,
               position = position_dodge(width = 1.5)) +
  stat_summary(fun.data = "mean_se", 
               geom = "linerange",  
               size=1, 
               position = position_dodge(width = 1)) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  scale_x_discrete(name="Condition",labels = c("I","NI-CR",
                                                       "S","NI-IR"))+
  scale_y_continuous(name="% Error discrimination",  limits = c(0,140)) +
  expand_limits(y = c(0.5, 13.5))+
  ggtitle("Stimuli via headphones")+
  annotate("text", x = 2.5, y = 106,  label = "***", size = 5) +
  annotate("segment", x = 2, xend = 3, y = 104, yend = 104, colour = "black", size=.3, alpha=1,)+
  annotate("text", x = 3, y = 126,  label = "*", size = 5) +
  annotate("segment", x = 2, xend = 4, y = 124, yend = 124, colour = "black", size=.3, alpha=1,)+
  annotate("text", x = 2, y = 116,  label = "**", size = 5) +
  annotate("segment", x = 1, xend = 3, y = 114, yend = 114, colour = "black", size=.3, alpha=1,)+
  annotate("text", x = 2.5, y = 136,  label = "**", size = 5) +
  annotate("segment", x = 1, xend = 4, y = 134, yend = 134, colour = "black", size=.3, alpha=1,)+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(legend.position = "none",axis.text=element_text(size=8),
        plot.title = element_text(size=11.5,hjust = 0.5,face="bold"))
fig.error_h

  ##FIGURE 3-----
# Figure.3 =  ggarrange(fig.error_s+rremove("x.title") ,
#                       fig.error_h, 
#                                 nrow = 2, labels = c("A", "B"))
# 
# Figure.3
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "FIGURE3", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot = Figure.3, width=8, height=16, units="cm", limitsize=FALSE, dpi=400)
# 

Figure.3 =  ggarrange(fig.error_h,
                      fig.error_s, 
                      nrow = 2, labels = c("A", "B"))

Figure.3
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "FIGURE3", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot = Figure.3, width=7, height=14, units="cm", limitsize=FALSE, dpi=400)




# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "error4.png", sep = '')
# ggsave(mi_nombre_de_archivo, plot=fig.error, width=10, height=10, units="cm", limitsize=FALSE, dpi=300)









# SUPPLEMENTARY FIGURES ----
###Standar Desviation EXP1----- 
results_tbl_SD_EXP1 = filter(results_tbl, condition != "Incongruous room" & condition != "Closed headphones")

results_tbl_EXP1_SD_pob = results_tbl_SD_EXP1 %>%
  group_by(target_distance,condition) %>%
  summarise(sd_intra_pob = sd(sd_perc_dist,na.rm = TRUE),
            sd_entre_pob = sd(perc_dist),
            sem_intra_pob = sd(sd_perc_dist)/sqrt(n()),
            sem_entre_pob = sd(perc_dist)/sqrt(n()))

cbPalette <- c("#000000","#E69F00","#009E73", "#0072B2","#999999", "#D55E00", "#CC79A7", "#F0E442")
fig_sd_intra <-ggplot(data = results_tbl_EXP1_SD_pob, aes(x = target_distance, y =sd_intra_pob, group=condition,
                                                          color=condition,fill=condition))+
  geom_line(aes(linetype= condition), size=1.2)+
  geom_point(aes(shape = condition),
             size = 3,position = position_jitter(width = 0.25))+
  scale_x_continuous(name="Distance source (m)", limits = c(0,7)) +
  scale_y_continuous(name="Intra-subject standard deviation",   limits = c(-.1,1)) +
  geom_abline(intercept = 0, slope = 0, linetype=3) +
  scale_shape_manual(values=c(19, 15, 17, 25))+
  scale_linetype_manual(values=c("solid", "longdash", "dashed", "dotted"))+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  theme_bw()+
  guides(shape = guide_legend(override.aes = list(size = 4)))+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(text=element_text(size=10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.title=element_blank(),
        legend.position = c(.35, .9),
        legend.key.width = unit(1.1,"cm"))
fig_sd_intra

fig_sd_intra_colapsed <- results_tbl_SD_EXP1 %>%
  group_by(subject, condition) %>%
  summarise(Mintra_colapsed = sd(sd_perc_dist,na.rm = TRUE),
            N = n()) %>%
  ungroup() %>%
  ggplot(aes(x = condition, 
             y = Mintra_colapsed, 
             colour = condition, 
             fill = condition,
             shape = condition)) +
  # scale_colour_colorblind()+
  # scale_fill_colorblind()+
  geom_point(alpha = 0.4, 
             position = position_jitterdodge(jitter.width = .3,
                                             jitter.height = 0,
                                             dodge.width = 1 )) +
  geom_violin(alpha=.2)+
  scale_shape_manual(values=c(19, 15, 17, 25))+
  geom_abline(slope = 0, 
              intercept = 0, 
              alpha = 0.5, 
              linetype = "dashed") +
  stat_summary(fun.data = "mean_se", 
               geom = "point", 
               alpha = 1,
               size = 3,
               position = position_dodge(width = 1.5)) +
  stat_summary(fun.data = "mean_se", 
               geom = "linerange",  
               size=1, 
               position = position_dodge(width = 1)) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  scale_x_discrete(name="Condition",labels = c("Non\nindividualized\nHRTF","Individualized\nHRTF","Real\nspeakers","Simulated\nHRTF"))+
  scale_y_continuous(name="Intra-subject\nstandard deviation\ncollapsed",breaks=c(-1.5,-1.25,-1,-0.75,-0.5,-0.25, 0, 0.25,0.5,0.75,1,1.25,1.5),
                     limits = c(-.1,1),expand= c(0,0)) +
  expand_limits(y = c(0.5, 13.5))+
  # annotate("text", x = 3, y = 1.23,  label = "***", size = 5) +
  # annotate("segment", x = 2, xend = 4, y = 1.2, yend = 1.2, colour = "black", size=.5, alpha=1,)+
  # annotate("text", x = 3.5, y = 1.03,  label = "***", size = 5) +
  # annotate("segment", x = 3, xend = 4, y = 1, yend = 1, colour = "black", size=.5, alpha=1,)+
  # annotate("text", x = 2.5, y = 1.43,  label = "***", size = 5) +
  # annotate("segment", x = 1, xend = 4, y = 1.4, yend = 1.4, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(legend.position = "none",axis.text=element_text(size=8))

fig_sd_intra_colapsed

t.test(filter(a, condition == "Real speakers")$Mintra_colapsed,
       filter(a, condition == "Non-individualized HRTF")$Mintra_colapsed,
       paired = FALSE)
t.test(filter(a, condition == "Non-individualized HRTF")$Mintra_colapsed,
       filter(a, condition == "Real speakers")$Mintra_colapsed,
       paired = FALSE)
t.test(filter(a, condition == "Individualized HRTF")$Mintra_colapsed,
       filter(a, condition == "Simulated HRTF")$Mintra_colapsed,
       paired = FALSE)
t.test(filter(a, condition == "Individualized HRTF")$Mintra_colapsed,
       filter(a, condition == "Non-individualized HRTF")$Mintra_colapsed,
       paired = FALSE)
t.test(filter(a, condition == "Real speakers")$Mintra_colapsed,
       filter(a, condition == "Simulated HRTF")$Mintra_colapsed,
       paired = FALSE)
t.test(filter(a, condition == "Real speakers")$Mintra_colapsed,
       filter(a, condition == "Non-individualized HRTF")$Mintra_colapsed,
       paired = FALSE)
t.test(filter(a, condition == "Simulated HRTF")$Mintra_colapsed,
       filter(a, condition == "Non-individualized HRTF")$Mintra_colapsed,
       paired = FALSE)


results_tbl_EXP1_SD_pob = results_tbl_SD_EXP1 %>%
  group_by(target_distance,condition) %>%
  summarise(sd_intra_pob = sd(sd_perc_dist,na.rm = TRUE),
            sd_entre_pob = sd(perc_dist),
            sem_intra_pob = sd(sd_perc_dist)/sqrt(n()),
            sem_entre_pob = sd(perc_dist)/sqrt(n()))

fig_sd_entre <-ggplot(data = results_tbl_EXP1_SD_pob, aes(x = target_distance, y =sd_entre_pob, group=condition,
                                                          color=condition,fill=condition))+
  geom_line(aes(linetype= condition), size=1.2)+
  geom_point(aes(shape = condition),
             size = 3,position = position_jitter(width = 0.25))+
  scale_x_continuous(name="Distance source (m)", limits = c(0,7)) +
  scale_y_continuous(name="Between-subject standard deviation",   limits = c(-.1,3)) +
  geom_abline(intercept = 0, slope = 0, linetype=3) +
  scale_shape_manual(values=c(19, 15, 17, 25))+
  scale_linetype_manual(values=c("solid", "longdash", "dashed", "dotted"))+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  theme_bw()+
  guides(shape = guide_legend(override.aes = list(size = 4)))+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(text=element_text(size=10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.title=element_blank(),
        legend.position = c(.35, .9),
        legend.key.width = unit(1.1,"cm"))
fig_sd_entre

fig_sd_entre_colapsed <- results_tbl_EXP1_SD_pob %>%
  group_by(condition) %>%
  summarise(Mentre_colapsed = mean(sd_entre_pob),
            N = n()) %>%
  ungroup() %>%
  ggplot(aes(x = condition, 
             y = Mentre_colapsed, 
             colour = condition, 
             fill = condition,
             shape = condition)) +
  # scale_colour_colorblind()+
  # scale_fill_colorblind()+
  geom_point(alpha = 0.4, 
             position = position_jitterdodge(jitter.width = .3,
                                             jitter.height = 0,
                                             dodge.width = 1 )) +
  geom_violin(alpha=.2)+
  scale_shape_manual(values=c(19, 15, 17, 25))+
  geom_abline(slope = 0, 
              intercept = 0, 
              alpha = 0.5, 
              linetype = "dashed") +
  stat_summary(fun.data = "mean_se", 
               geom = "point", 
               alpha = 1,
               size = 3,
               position = position_dodge(width = 1.5)) +
  stat_summary(fun.data = "mean_se", 
               geom = "linerange",  
               size=1, 
               position = position_dodge(width = 1)) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  scale_x_discrete(name="Condition",labels = c("Non\nindividualized\nHRTF","Individualized\nHRTF","Real\nspeakers","Simulated\nHRTF"))+
  scale_y_continuous(name="Between-subject\nstandard deviation\ncollapsed",
                     limits = c(-.1,5),expand= c(0,0)) +
  expand_limits(y = c(0.5, 13.5))+
  annotate("text", x = 3, y = 1.23+3,  label = "***", size = 5) +
  annotate("segment", x = 2, xend = 4, y = 1.2+3, yend = 1.2+3, colour = "black", size=.5, alpha=1,)+
  annotate("text", x = 3.5, y = 1.03+3,  label = "***", size = 5) +
  annotate("segment", x = 3, xend = 4, y = 1+3, yend = 1+3, colour = "black", size=.5, alpha=1,)+
  annotate("text", x = 2.5, y = 1.43+3,  label = "***", size = 5) +
  annotate("segment", x = 1, xend = 4, y = 1.4+3, yend = 1.4+3, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(legend.position = "none",axis.text=element_text(size=8))

fig_sd_entre_colapsed

a =  results_tbl_EXP1_SD_pob %>%
  group_by(condition) %>%
  summarise(Mentre_colapsed = mean(sd_entre_pob),
            N = n())
# t.test(filter(a, condition == "Real speakers")$Mentre_colapsed,
#        filter(a, condition == "Non-individualized HRTF")$Mentre_colapsed,
#        paired = FALSE)
# t.test(filter(a, condition == "Non-individualized HRTF")$Mentre_colapsed,
#        filter(a, condition == "Real speakers")$Mentre_colapsed,
#        paired = FALSE)
t.test(filter(a, condition == "Individualized HRTF")$Mentre_colapsed,
       filter(a, condition == "Simulated HRTF")$Mentre_colapsed,
       paired = FALSE)
# t.test(filter(a, condition == "Individualized HRTF")$Mentre_colapsed,
#        filter(a, condition == "Non-individualized HRTF")$Mentre_colapsed,
#        paired = FALSE)
t.test(filter(a, condition == "Real speakers")$Mentre_colapsed,
       filter(a, condition == "Simulated HRTF")$Mentre_colapsed,
       paired = FALSE)
# t.test(filter(a, condition == "Real speakers")$Mentre_colapsed,
#        filter(a, condition == "Non-individualized HRTF")$Mentre_colapsed,
#        paired = FALSE)
t.test(filter(a, condition == "Simulated HRTF")$Mentre_colapsed,
       filter(a, condition == "Non-individualized HRTF")$Mentre_colapsed,
       paired = FALSE)




Figure.sd =  ggarrange(fig_sd_intra,
                       fig_sd_entre,
                       fig_sd_intra_colapsed,
                       fig_sd_entre_colapsed,
                       nrow = 2,ncol = 2, labels = c("A", "B","C","D"),align = "hv")

Figure.sd
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "FIGURESDEXP1", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot = Figure.sd, width=28, height=20, units="cm", limitsize=FALSE, dpi=400)
###Standar Desviation EXP2----- 
results_tbl_SD_EXP2 = filter(results_tbl, condition == "Incongruous room" | condition == "Non-individualized HRTF")

results_tbl_EXP2_SD_pob = results_tbl_SD_EXP2 %>%
  group_by(target_distance,condition) %>%
  summarise(sd_intra_pob = sd(sd_perc_dist,na.rm = TRUE),
            sd_entre_pob = sd(perc_dist),
            sem_intra_pob = sd(sd_perc_dist)/sqrt(n()),
            sem_entre_pob = sd(perc_dist)/sqrt(n()))

cbPalette <- c("#000000","#CC79A7")
cblegend = c("Non-individualized HRTF (congruent room)","Non-individualized HRTF (incongruous room)")
fig_sd_intra <-ggplot(data = results_tbl_EXP2_SD_pob, aes(x = target_distance, y =sd_intra_pob, group=condition,
                                                          color=condition,fill=condition))+
  geom_line(aes(linetype= condition), size=1.2)+
  geom_point(aes(shape = condition),
             size = 3,position = position_jitter(width = 0.25))+
  scale_x_continuous(name="Distance source (m)", limits = c(0,7)) +
  scale_y_continuous(name="Intra-subject standard deviation",   limits = c(-.1,1)) +
  geom_abline(intercept = 0, slope = 0, linetype=3) +
  scale_shape_manual(values=c(19, 15, 17, 25), labels = cblegend)+
  scale_linetype_manual(values=c("solid", "longdash", "dashed", "dotted"), labels = cblegend)+
  scale_colour_manual(values = cbPalette, labels = cblegend) + 
  scale_fill_manual(values = cbPalette, labels = cblegend) + 
  theme_bw()+
  guides(shape = guide_legend(override.aes = list(size = 4)))+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(text=element_text(size=10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.title=element_blank(),
        legend.position = c(.35, .9),
        legend.key.width = unit(1.1,"cm"))
fig_sd_intra

fig_sd_intra_colapsed <- results_tbl_SD_EXP2 %>%
  group_by(subject, condition) %>%
  summarise(Mintra_colapsed = sd(sd_perc_dist,na.rm = TRUE),
            N = n()) %>%
  ungroup() %>%
  ggplot(aes(x = condition, 
             y = Mintra_colapsed, 
             colour = condition, 
             fill = condition,
             shape = condition)) +
  geom_point(alpha = 0.4, 
             position = position_jitterdodge(jitter.width = .3,
                                             jitter.height = 0,
                                             dodge.width = 1 )) +
  geom_violin(alpha=.2)+
  scale_shape_manual(values=c(19, 15, 17, 25))+
  geom_abline(slope = 0, 
              intercept = 0, 
              alpha = 0.5, 
              linetype = "dashed") +
  stat_summary(fun.data = "mean_se", 
               geom = "point", 
               alpha = 1,
               size = 3,
               position = position_dodge(width = 1.5)) +
  stat_summary(fun.data = "mean_se", 
               geom = "linerange",  
               size=1, 
               position = position_dodge(width = 1)) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  scale_x_discrete(name="Condition",labels = c("Non individualized HRTF\n(congruent room)","Non-individualized HRTF\n(incongruous room)"))+
  scale_y_continuous(name="Intra-subject\nstandard deviation\ncollapsed",breaks=c(-1.5,-1.25,-1,-0.75,-0.5,-0.25, 0, 0.25,0.5,0.75,1,1.25,1.5),
                     limits = c(-.1,1.2),expand= c(0,0)) +
  expand_limits(y = c(0.5, 13.5))+
  # annotate("text", x = 1.5, y = 1.43,  label = "*", size = 5) +
  # annotate("segment", x = 1, xend = 2, y = 1.4, yend = 1.4, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(legend.position = "none",axis.text=element_text(size=8))

fig_sd_intra_colapsed


fig_sd_entre <-ggplot(data = results_tbl_EXP2_SD_pob, aes(x = target_distance, y =sd_entre_pob, group=condition,
                                                          color=condition,fill=condition))+
  geom_line(aes(linetype= condition), size=1.2)+
  geom_point(aes(shape = condition),
             size = 3,position = position_jitter(width = 0.25))+
  scale_x_continuous(name="Distance source (m)", limits = c(0,7)) +
  scale_y_continuous(name="Between-subject standard deviation",   limits = c(-.1,3)) +
  geom_abline(intercept = 0, slope = 0, linetype=3) +
  scale_shape_manual(values=c(19, 15, 17, 25), labels = cblegend)+
  scale_linetype_manual(values=c("solid", "longdash", "dashed", "dotted"), labels = cblegend)+
  scale_colour_manual(values = cbPalette, labels = cblegend) + 
  scale_fill_manual(values = cbPalette, labels = cblegend) + 
  theme_bw()+
  guides(shape = guide_legend(override.aes = list(size = 4)))+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(text=element_text(size=10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.title=element_blank(),
        legend.position = c(.35, .9),
        legend.key.width = unit(1.1,"cm"))
fig_sd_entre

fig_sd_entre_colapsed <- results_tbl_EXP2_SD_pob %>%
  group_by(condition) %>%
  summarise(Mentre_colapsed = mean( sd_entre_pob),
            N = n()) %>%
  ungroup() %>%
  ggplot(aes(x = condition, 
             y = Mentre_colapsed, 
             colour = condition, 
             fill = condition,
             shape = condition)) +
  geom_point(alpha = 0.4, 
             position = position_jitterdodge(jitter.width = .3,
                                             jitter.height = 0,
                                             dodge.width = 1 )) +
  geom_violin(alpha=.2)+
  scale_shape_manual(values=c(19, 15, 17, 25))+
  geom_abline(slope = 0, 
              intercept = 0, 
              alpha = 0.5, 
              linetype = "dashed") +
  stat_summary(fun.data = "mean_se", 
               geom = "point", 
               alpha = 1,
               size = 3,
               position = position_dodge(width = 1.5)) +
  stat_summary(fun.data = "mean_se", 
               geom = "linerange",  
               size=1, 
               position = position_dodge(width = 1)) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  scale_x_discrete(name="Condition",labels = c("Non individualized HRTF\n(congruent room)","Non-individualized HRTF\n(incongruous room)"))+
  scale_y_continuous(name="Between-subject\nstandard deviation\ncollapsed",
                     limits = c(-.1,3.6),expand= c(0,0)) +
  expand_limits(y = c(0.5, 13.5))+
  annotate("text", x = 1.5, y = 3.53,  label = "*", size = 5) +
  annotate("segment", x = 1, xend = 2, y = 3.5, yend = 3.5, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(legend.position = "none",axis.text=element_text(size=8))

fig_sd_entre_colapsed

Figure.sd =  ggarrange(fig_sd_intra,
                       fig_sd_entre,
                       fig_sd_intra_colapsed,
                       fig_sd_entre_colapsed,
                       nrow = 2,ncol = 2, labels = c("A", "B","C","D"),align = "hv")

Figure.sd
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "FIGURESDEXP2", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot = Figure.sd, width=28, height=20, units="cm", limitsize=FALSE, dpi=400)




#DEBUG ------




t.test(coef(m.Dist)$subject["(Intercept)"][[1]],
       coef(m.Dist)$subject["(Intercept)"][[1]]+coef(m.Dist)$subject["conditionIndividualized HRTF"][[1]],
       paired = FALSE)
t.test(coef(m.Dist)$subject["(Intercept)"][[1]],
       coef(m.Dist)$subject["(Intercept)"][[1]]+coef(m.Dist)$subject["conditionReal speakers"][[1]],
       paired = FALSE)
t.test(coef(m.Dist)$subject["(Intercept)"][[1]],
       coef(m.Dist)$subject["(Intercept)"][[1]]+coef(m.Dist)$subject["conditionSimulated HRTF"][[1]],
       paired = FALSE)

t.test(coef(m.Dist)$subject["(Intercept)"][[1]]+coef(m.Dist)$subject["conditionIndividualized HRTF"][[1]],
       coef(m.Dist)$subject["(Intercept)"][[1]]+coef(m.Dist)$subject["conditionReal speakers"][[1]],
       paired = FALSE)
t.test(coef(m.Dist)$subject["(Intercept)"][[1]]+coef(m.Dist)$subject["conditionIndividualized HRTF"][[1]],
       coef(m.Dist)$subject["(Intercept)"][[1]]+coef(m.Dist)$subject["conditionSimulated HRTF"][[1]],
       paired = FALSE)

t.test(coef(m.Dist)$subject["(Intercept)"][[1]]+coef(m.Dist)$subject["conditionNon-individualized HRTF"][[1]],
       coef(m.Dist)$subject["(Intercept)"][[1]]+coef(m.Dist)$subject["conditionSimulated HRTF"][[1]],
       paired = FALSE)

t.test(coef(m.Dist)$subject["log10(target_distance)"][[1]]+coef(m.Dist)$subject["conditionReal speakers"][[1]],
       coef(m.Dist)$subject["log10(target_distance)"][[1]]+coef(m.Dist)$subject["conditionSimulated HRTF"][[1]],
       paired = FALSE)



t.test(coef(m.Dist)$subject["log10(target_distance)"][[1]],
       coef(m.Dist)$subject["log10(target_distance)"][[1]]+coef(m.Dist)$subject["log10(target_distance):conditionIndividualized HRTF"][[1]],
       paired = FALSE)
t.test(coef(m.Dist)$subject["log10(target_distance)"][[1]],
       coef(m.Dist)$subject["log10(target_distance)"][[1]]+coef(m.Dist)$subject["log10(target_distance):conditionReal speakers"][[1]],
       paired = FALSE)
t.test(coef(m.Dist)$subject["log10(target_distance)"][[1]],
       coef(m.Dist)$subject["log10(target_distance)"][[1]]+coef(m.Dist)$subject["log10(target_distance):conditionSimulated HRTF"][[1]],
       paired = FALSE)

t.test(coef(m.Dist)$subject["log10(target_distance)"][[1]]+coef(m.Dist)$subject["log10(target_distance):conditionIndividualized HRTF"][[1]],
       coef(m.Dist)$subject["log10(target_distance)"][[1]]+coef(m.Dist)$subject["log10(target_distance):conditionReal speakers"][[1]],
       paired = FALSE)
t.test(coef(m.Dist)$subject["log10(target_distance)"][[1]]+coef(m.Dist)$subject["log10(target_distance):conditionIndividualized HRTF"][[1]],
       coef(m.Dist)$subject["log10(target_distance)"][[1]]+coef(m.Dist)$subject["log10(target_distance):conditionSimulated HRTF"][[1]],
       paired = FALSE)

t.test(coef(m.Dist)$subject["log10(target_distance)"][[1]]+coef(m.Dist)$subject["log10(target_distance):conditionReal speakers"][[1]],
       coef(m.Dist)$subject["log10(target_distance)"][[1]]+coef(m.Dist)$subject["log10(target_distance):conditionSimulated HRTF"][[1]],
       paired = FALSE)






# results_tbl = filter(tabla.ind, perc_dist != 0, condition == "VIRTUAL" | condition == "ABIERTO")

tabla.ind = tabla.ind %>% rename(perc_dist = respuesta_ind)
tabla.ind = tabla.ind %>% rename(target_distance = Distancia)
tabla.ind = tabla.ind %>% rename(subject = Sujeto)
tabla.ind = tabla.ind %>% rename(condition = Bloque)
tabla.ind$condition = factor(tabla.ind$condition, levels = c("VIRTUAL","ABIERTO","CERRADO"))
levels(tabla.ind$condition) = c("Non-individualized HRTF (congruent room)","Incongruous room","CERRADO")
tabla.ind = tabla.ind %>% rename(sd_perc_dist = sd_respuesta_ind)
tabla.ind = tabla.ind %>% rename(sem_perc_dist = sem_respuesta_ind)

results_tbl = filter(tabla.ind, perc_dist != 0, condition == "Non-individualized HRTF (congruent room)" | condition == "Incongruous room")

results_tbl$logperc_dist = log10(results_tbl$perc_dist)

tabla.pob = results_tbl %>%
  group_by(target_distance,condition) %>%
  summarise(logperc_dist_pob = mean(logperc_dist),
            sdlogperc_dist_pob = sd(logperc_dist),
            semlogperc_dist_pob = sd(logperc_dist)/sqrt(n()),
            perc_dist_pob = mean(perc_dist),
            sd_respuesta_pob = sd(perc_dist),
            sem_respuesta_pob = sd(perc_dist)/sqrt(n()))

# # Estadistica 
# # Sesgo
# 
# # tabla_ADP.ind.summ_dist = tabla.ind  %>%
# #   group_by(Sujeto, Bloque, Dist_fis) %>%
# #   summarise(mSesgoRel  = mean(SesgoRel[,"mean"]))  %>%
# #   ungroup()
# # 
# # tabla_ADP.ind.summ_Reach <- tabla_ADP.ind.summ_dist %>% 
# #   group_by(Sujeto, Bloque) %>%
# #   summarise(MSesgoRel = mean(mSesgoRel),
# #             N = n()) %>%
# #   ungroup()
# # 
# # m.VR <- lm(MSesgoRel ~ Bloque, 
# #            data = tabla_ADP.ind.summ_Reach)
# # ggcoefstats(m.VR, output = "tidy") %>% select(-label)
# # summary(m.VR)
# # anova(m.VR)
# 
# t.test(filter(results_tbl_bias_EXP1, condition == "Non-individualized HRTF")$unsigned_bias_ind,
#        filter(results_tbl_bias_EXP1, condition == "Real speakers")$unsigned_bias_ind,
#        paired = FALSE)
# t.test(filter(results_tbl_bias_EXP1, condition == "Individualized HRTF")$unsigned_bias_ind,
#        filter(results_tbl_bias_EXP1, condition == "Simulated HRTF")$unsigned_bias_ind,
#        paired = FALSE)
# t.test(filter(results_tbl_bias_EXP1, condition == "Individualized HRTF")$unsigned_bias_ind,
#        filter(results_tbl_bias_EXP1, condition == "Non-individualized HRTF")$unsigned_bias_ind,
#        paired = FALSE)
# t.test(filter(results_tbl_bias_EXP1, condition == "Real speakers")$unsigned_bias_ind,
#        filter(results_tbl_bias_EXP1, condition == "Simulated HRTF")$unsigned_bias_ind,
#        paired = FALSE)
# t.test(filter(results_tbl_bias_EXP1, condition == "Real speakers")$unsigned_bias_ind,
#        filter(results_tbl_bias_EXP1, condition == "Non-individualized HRTF")$unsigned_bias_ind,
#        paired = FALSE)
# t.test(filter(results_tbl_bias_EXP1, condition == "Simulated HRTF")$unsigned_bias_ind,
#        filter(results_tbl_bias_EXP1, condition == "Non-individualized HRTF")$unsigned_bias_ind,
#        paired = FALSE)

# EXPERIMENTO 2 -
# m.Dist3 <-  lmer(perc_dist ~ target_distance*condition+(1+target_distance|subject)+(0+condition|subject),
#                  data = results_tbl)

m.Dist3 <-  lmer(log10(perc_dist) ~ log10(target_distance)*condition+(1+log10(target_distance)|subject)+(0+condition|subject),
                 data = results_tbl)

extract_stats(ggcoefstats(m.Dist3))
r.squaredGLMM(m.Dist3)
anova(m.Dist3)







results_tbl$Modelfitted3<-predict(m.Dist3)

# # Todos los sujetos
# FittedlmPlot1 <-ggplot()+
#   facet_grid(subject ~ condition, labeller=label_both)+
#   geom_line(data = results_tbl, aes(x = target_distance, y = 10^Modelfitted3))+
#   geom_point(data = results_tbl, aes(x = target_distance, y =perc_dist, group=subject,colour = subject), size=3)+
#   #  coord_cartesian(ylim = c(.03,.074))+ 
#   xlab("Targent_distance")+ylab("Perceived_distance")
# FittedlmPlot1


Final.Fixed<-effect(c("log10(target_distance)*condition"), m.Dist3)

# Final.Fixed<-effect(c("target_distance*condition"), m.Dist3)
# Grafico poblacional
Final.Fixed<-as.data.frame(Final.Fixed)
cbPalette <- c("#000000","#E69F00","#009E73", "#999999", "#D55E00", "#0072B2", "#CC79A7", "#F0E442")

eq1 <- substitute("Incongruous room :"~~~italic(y) == k %.% italic(X)^italic(a), 
                  list(k = round(10^fixef(m.Dist3)[[1]],digits = 2),
                       a = round(fixef(m.Dist3)[[2]], digits = 2)))
eq2 <- substitute("Congruent room:"~~~italic(y) == k %.% italic(X)^italic(a), 
                  list(k = round(10^(fixef(m.Dist3)[[1]]+fixef(m.Dist3)[[3]]), digits = 2),
                       a = round(fixef(m.Dist3)[[2]]+fixef(m.Dist3)[[4]], digits = 2)))

Final.Fixed.Plot <-ggplot(data = Final.Fixed, aes(x = target_distance, y =10^fit, group=condition))+
  # coord_cartesian(xlim=c(0,4),ylim = c(0,.7))+ 
  geom_line(aes(color=condition,linetype= condition), size=1.2)+
  geom_point(data = tabla.pob, mapping= aes(x = target_distance, y =10^logperc_dist_pob, shape = condition,group=condition, color=condition),size = 3)+
  # geom_pointrange(data = tabla.pob, mapping= aes(x = target_distance, y =10^logperc_dist_pob, 
  #                                                ymin = 10^logperc_dist_pob-10^semlogperc_dist_pob,
  #                                                ymax = 10^logperc_dist_pob+10^semlogperc_dist_pob,
  #                                                group=condition, color=condition))+
  # geom_ribbon(aes(ymin=10^fit-10^se, ymax=10^fit+10^se,fill=condition),alpha=.2)+
  scale_x_continuous(name="Distance source (m)", limits = c(0,7)) +
  scale_y_continuous(name="Perceived distance (m)",   limits = c(0,7)) +
  geom_abline(intercept = 0, slope = 1, linetype=3) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  geom_text(x = 0.2, y = 5, label = as.character(as.expression(eq1)), hjust = 0, nudge_x =  0, parse = TRUE, size = 4, color = "#000000")+
  geom_text(x = 0.2, y = 5.5, label = as.character(as.expression(eq2)), hjust = 0, nudge_x =  0,parse = TRUE, size = 4, color = "#000000")+
  
  
  theme_bw()+
  theme(text=element_text(face="bold", size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "NA"),
        axis.line = element_line(size = 1, colour = "grey80"),
        legend.title=element_blank(),
        legend.position = c(.3, .92))
Final.Fixed.Plot

mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "graf1", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot=Final.Fixed.Plot, width=10, height=10, units="cm", limitsize=FALSE, dpi=600)

### Distance 
results_tbl_EXP3 = filter(results_tbl, perc_dist != 0, condition == "Closed headphones" | condition == "Non-individualized HRTF")

results_tbl_EXP3_pob = results_tbl_EXP3 %>%
  group_by(target_distance,condition) %>%
  summarise(logperc_dist_pob = mean(logperc_dist,na.rm = TRUE),
            sdlogperc_dist_pob = sd(logperc_dist),
            semlogperc_dist_pob = sd(logperc_dist)/sqrt(n()),
            perc_dist_pob = mean(perc_dist),
            sd_respuesta_pob = sd(perc_dist),
            sem_respuesta_pob = sd(perc_dist)/sqrt(n()))

m.Dist <-  lmer(log10(perc_dist) ~ log10(target_distance)*condition+(1+log10(target_distance)|subject)+(0+condition|subject),
                data = results_tbl_EXP3)

extract_stats(ggcoefstats(m.Dist))
r.squaredGLMM(m.Dist)
anova(m.Dist)

results_tbl_EXP3$Modelfitted3<-predict(m.Dist)
# Todos los sujetos
FittedlmPlot1 <-ggplot()+
  facet_grid(subject ~ condition, labeller=label_both)+
  geom_line(data = results_tbl_EXP3, aes(x = target_distance, y = 10^Modelfitted3))+
  geom_point(data = results_tbl_EXP3, aes(x = target_distance, y =perc_dist, group=subject,colour = subject), size=3)+
  #  coord_cartesian(ylim = c(.03,.074))+
  geom_abline(intercept = 0, slope = 1, linetype=3) +
  xlab("Targent_distance")+ylab("Perceived_distance")+theme_bw()
FittedlmPlot1
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "AAAA", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot = FittedlmPlot1, width=14, height=60, units="cm", limitsize=FALSE, dpi=200)



eq_noind <- substitute("Non-individualized HRTF"~~~italic(y) == k %.% italic(X)^italic((a)), 
                       list(k = round(10^coef(summary(m.Dist))[1], digits = 2),
                            a = round(coef(summary(m.Dist))[2], digits = 2)))
eq_ch <- substitute("Closed headphones"~~~italic(y) == k %.% italic(X)^italic((a)), 
                    list(k = round(10^(coef(summary(m.Dist))[1]+coef(summary(m.Dist))[3]), digits = 2),
                         a = round(coef(summary(m.Dist))[2]+coef(summary(m.Dist))[4], digits = 2)))


Final.Fixed<-effect(c("log10(target_distance)*condition"), m.Dist)
Final.Fixed<-as.data.frame(Final.Fixed)
cbPalette <- c("#000000","#F0E442")
cblegend = c(eq_noind,eq_ch)
Final.Fixed.Plot <-ggplot(data = Final.Fixed, aes(x = target_distance, y =10^fit, group=condition))+
  geom_line(aes(color=condition,linetype= condition), size=1.2)+
  geom_point(data = results_tbl_EXP3_pob, mapping= aes(x = target_distance, y =10^logperc_dist_pob, 
                                                       shape = condition,group=condition, color=condition,fill=condition),
             size = 3,position = position_jitter(width = 0.25))+
  scale_x_continuous(name="Distance source (m)", limits = c(0,7)) +
  scale_y_continuous(name="Perceived distance (m)",   limits = c(0,6)) +
  geom_abline(intercept = 0, slope = 1, linetype=3) +
  scale_shape_manual(values=c(19, 15, 17, 25), labels = cblegend)+
  scale_linetype_manual(values=c("solid", "longdash", "dashed", "dotted"), labels = cblegend)+
  scale_colour_manual(values = cbPalette, labels = cblegend) + 
  scale_fill_manual(values = cbPalette, labels = cblegend) + 
  # geom_text(x = 0, y = 4.3, label = as.character(as.expression(eq_noind)), hjust = 0, nudge_x =  0, parse = TRUE, size = 3.2, color = "#000000")+
  # geom_text(x = 0, y = 4, label = as.character(as.expression(eq_ind)), hjust = 0, nudge_x =  0,parse = TRUE, size = 3.2, color = "#E69F00")+
  # geom_text(x = 0, y = 3.7, label = as.character(as.expression(eq_real)), hjust = 0, nudge_x =  0, parse = TRUE, size = 3.2, color = "#009E73")+
  # geom_text(x = 0, y = 3.4, label = as.character(as.expression(eq_sim)), hjust = 0, nudge_x =  0, parse = TRUE, size = 3.2, color =  "#0072B2")+
  theme_bw()+
  guides(shape = guide_legend(override.aes = list(size = 4)))+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(text=element_text(size=10),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.title=element_blank(),
        legend.position = c(.35, .94),
        legend.key.width = unit(1.1,"cm"))
Final.Fixed.Plot

###Intercept-
results_tbl_intercept_EXP3 = filter(results_tbl_bias, condition == "Closed headphones" | condition == "Non-individualized HRTF")
results_tbl_intercept_EXP3$intercept = 0
results_tbl_intercept_EXP3[results_tbl_intercept_EXP3$condition == "Non-individualized HRTF",]$intercept = coef(m.Dist)$subject[[2]][c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,21,22,23)]
results_tbl_intercept_EXP3[results_tbl_intercept_EXP3$condition == "Closed headphones",]$intercept = coef(m.Dist)$subject[[2]][c(1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]+coef(m.Dist)$subject[[4]][c(1,2,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]

fig.intercept <- results_tbl_intercept_EXP3 %>%
  group_by(subject, condition) %>%
  summarise(Mintercept = mean(intercept),
            N = n()) %>%
  ungroup() %>%
  ggplot(aes(x = condition, 
             y = Mintercept, 
             colour = condition, 
             fill = condition,
             shape = condition)) +
  # scale_colour_colorblind()+
  # scale_fill_colorblind()+
  geom_point(alpha = 0.4, 
             position = position_jitterdodge(jitter.width = .3,
                                             jitter.height = 0,
                                             dodge.width = 1 )) +
  geom_violin(alpha=.2)+
  scale_shape_manual(values=c(19, 15, 17, 25))+
  geom_abline(slope = 0, 
              intercept = 0, 
              alpha = 0.5, 
              linetype = "dashed") +
  stat_summary(fun.data = "mean_se", 
               geom = "point", 
               alpha = 1,
               size = 3,
               position = position_dodge(width = 1.5)) +
  stat_summary(fun.data = "mean_se", 
               geom = "linerange",  
               size=1, 
               position = position_dodge(width = 1)) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  scale_x_discrete(name="Condition",labels = c("Non individualized\nHRTF","Closed\nheadphones"))+
  scale_y_continuous(name="Log10 Intercept\n(LMER model)",breaks=c(-1.5,-1.25,-1,-0.75,-0.5,-0.25, 0, 0.25,0.5,0.75,1,1.25,1.5),
                     limits = c(-1.2,1),expand= c(0,0)) +
  expand_limits(y = c(0.5, 13.5))+
  # annotate("text", x = 1.5, y = 0.78,  label = "*", size = 5) +
  # annotate("segment", x = 1, xend = 2, y = 0.75, yend = 0.75, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(legend.position = "none",axis.text=element_text(size=8))

fig.intercept
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Sesgo_POB+ind+pvalor", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot = fig.biasRelVR, width=14, height=5, units="cm", limitsize=FALSE, dpi=200)
a <- results_tbl_intercept_EXP3 %>%
  group_by(subject, condition) %>%
  summarise(Mintercept = mean(intercept),
            N = n())
t.test(filter(a, condition == "Non-individualized HRTF")$Mintercept,
       filter(a, condition == "Closed headphones")$Mintercept,
       paired = FALSE)
###Signed bias-
results_tbl_bias_EXP3 = filter(results_tbl_bias,  condition == "Closed headphones" | condition == "Non-individualized HRTF")

fig.signed_bias <- results_tbl_bias_EXP3 %>%
  group_by(subject, condition) %>%
  summarise(Msignedbias = mean(signed_bias_ind),
            N = n()) %>%
  ungroup() %>%
  ggplot(aes(x = condition, 
             y = Msignedbias, 
             colour = condition, 
             fill = condition,
             shape = condition)) +
  # scale_colour_colorblind()+
  # scale_fill_colorblind()+
  geom_point(alpha = 0.4, 
             position = position_jitterdodge(jitter.width = .3,
                                             jitter.height = 0,
                                             dodge.width = 1 )) +
  geom_violin(alpha=.2)+
  scale_shape_manual(values=c(19, 15, 17, 25))+
  geom_abline(slope = 0, 
              intercept = 0, 
              alpha = 0.5, 
              linetype = "dashed") +
  stat_summary(fun.data = "mean_se", 
               geom = "point", 
               alpha = 1,
               size = 3,
               position = position_dodge(width = 1.5)) +
  stat_summary(fun.data = "mean_se", 
               geom = "linerange",  
               size=1, 
               position = position_dodge(width = 1)) +
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  scale_x_discrete(name="Condition",labels = c("Non individualized\nHRTF","Closed\nheadphones"))+
  scale_y_continuous(name="Signed bias",breaks=c(-1.5,-1.25,-1,-0.75,-0.5,-0.25, 0, 0.25,0.5,0.75,1,1.25,1.5),
                     limits = c(-1.5,1.6),expand= c(0,0)) +
  expand_limits(y = c(0.5, 13.5))+
  # annotate("text", x = 1.5, y = 1.43,  label = "*", size = 5) +
  # annotate("segment", x = 1, xend = 2, y = 1.4, yend = 1.4, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(legend.position = "none",axis.text=element_text(size=8))

fig.signed_bias
# mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Sesgo_POB+ind+pvalor", ".png", sep = '')
# ggsave(mi_nombre_de_archivo, plot = fig.biasRelVR, width=14, height=5, units="cm", limitsize=FALSE, dpi=200)
a <- results_tbl_bias_EXP3 %>%
  group_by(subject, condition) %>%
  summarise(Msignedbias = mean(signed_bias_ind),
            N = n())
t.test(filter(a, condition == "Non-individualized HRTF")$Msignedbias,
       filter(a, condition == "Closed headphones")$Msignedbias,
       paired = FALSE)

##FIGURE 3-
Figure.3 =  ggarrange(Final.Fixed.Plot,
                      ggarrange(fig.signed_bias+rremove("x.title") ,
                                fig.intercept, 
                                nrow = 2, labels = c("B", "C")),
                      labels = "A",
                      ncol = 2, nrow = 1,widths = c(1,0.75))

Figure.3
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "FIGURE3", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot = Figure.3, width=20, height=14, units="cm", limitsize=FALSE, dpi=400)
