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
figures_folder = "figures"

load("./datafinal.RData")


###Distance----
results_tbl_EXP2 = filter(results_tbl, perc_dist != 0, condition == "Incongruous room" | condition == "Non-individualized HRTF")

subjects_to_remove <- c("S006", "S009", "S013", "S018", "S019", "S024")
results_tbl_EXP2 <- results_tbl_EXP2[!(
  results_tbl_EXP2$condition == "Incongruous room" &
    results_tbl_EXP2$subject %in% subjects_to_remove
), ]

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

# Extraer efectos aleatorios
random_effects <- ranef(m.Dist)$subject

# Crear un dataframe base con los efectos aleatorios
random_effects_df <- as.data.frame(random_effects)
colnames(random_effects_df) <- c("Intercept", "Slope_log10_target_distance",
                                 "Condition_Non_individualized_HRTF",
                                 "Condition_Incongruous_room")
random_effects_df$Subject <- rownames(random_effects)



random_effects_long <- random_effects_df %>%
  pivot_longer(cols = starts_with("Condition"),
               names_to = "Condition",
               values_to = "Condition_Adjustment") %>%
  mutate(
    # Reescribir nombres de condiciones
    Condition = case_when(
      Condition == "Condition_Non_individualized_HRTF" ~ "Non-individualized HRTF",
      Condition == "Condition_Incongruous_room" ~ "Incongruous room"
    ),
    # Calcular el intercepto total para cada condición
    Intercept = Intercept + fixef(m.Dist)["(Intercept)"] +
      if_else(Condition == "Incongruous room", fixef(m.Dist)["conditionIncongruous room"], 0),
    # Calcular la pendiente ajustada por condición
    Slope = Slope_log10_target_distance + fixef(m.Dist)["log10(target_distance)"] +
      if_else(Condition == "Incongruous room", fixef(m.Dist)["log10(target_distance):conditionIncongruous room"], 0)
  ) %>%
  select(Subject, Condition, Intercept, Slope)

# Mostrar la tabla final
# print(random_effects_long)


eq_noind_con <- substitute("NI-BRIR (CR)"~~~italic(y) == k %.% italic(X)^italic((a)), 
                           list(k = round(10^(coef(summary(m.Dist))[1]), digits = 2)-0.02,
                                a = round(coef(summary(m.Dist))[2], digits = 2)+0.01))
eq_noind_inc <- substitute("NI-BRIR (IR)"~~~italic(y) == k %.% italic(X)^italic((a)), 
                           list(k = round(10^(coef(summary(m.Dist))[1]+coef(summary(m.Dist))[3]), digits = 2),
                                a = round(coef(summary(m.Dist))[2]+coef(summary(m.Dist))[4], digits = 2)))

Final.Fixed<-effect(c("log10(target_distance)*condition"), m.Dist)
Final.Fixed<-as.data.frame(Final.Fixed)

results_tbl_EXP2_pob$condition <- factor(results_tbl_EXP2_pob$condition,
                                         levels = c("Non-individualized HRTF","Incongruous room"))

cbPalette <- c("#CC79A7","#009E73", "#0072B2", "black","#999999", "#D55E00", "#CC79A7", "#F0E442")
cblegend = c(eq_noind_con,eq_noind_inc)
Final.Fixed.Plot <-ggplot(data = Final.Fixed, aes(x = target_distance, y =10^fit, group=condition))+
  geom_abline(intercept = 0, slope = 1, linetype=3) +
  geom_line(aes(color=condition,linetype= condition), size=1.2)+
  geom_pointrange(data = results_tbl_EXP2_pob, mapping= aes(x = target_distance, y =10^logperc_dist_pob,
                                                            ymin =10^logperc_dist_pob-sem_respuesta_pob,
                                                            ymax=10^logperc_dist_pob+sem_respuesta_pob,
                                                            shape = condition,group=condition, color=condition,
                                                            fill=condition),
                  size = 1,alpha =.8,position = position_jitter(width = 0.05),show.legend=FALSE)+
  scale_x_continuous(name="Distance source (m)", limits = c(0.3,6.3)) +
  scale_y_continuous(name="Perceived distance (m)",   limits = c(0.3,6.3)) +
  
  scale_shape_manual(values=c(17,23,NA,NA), labels = cblegend)+
  scale_linetype_manual(values=c("solid", "solid","solid","dashed"), labels = cblegend)+
  scale_colour_manual(values = cbPalette, labels = cblegend) + 
  scale_fill_manual(values = cbPalette, labels = cblegend) + 
  theme_bw()+
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
results_tbl_intercept_EXP2$expon = 0

subjects_to_remove <- c("S006", "S009", "S013", "S018", "S019", "S024")
results_tbl_intercept_EXP2 <- results_tbl_intercept_EXP2[!(
  results_tbl_intercept_EXP2$condition == "Incongruous room" &
    results_tbl_intercept_EXP2$subject %in% subjects_to_remove
), ]


results_tbl_intercept_EXP2[results_tbl_intercept_EXP2$condition == "Non-individualized HRTF",]$intercept = coef(m.Dist)$subject[[2]][c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,21,22,23,24)]
results_tbl_intercept_EXP2[results_tbl_intercept_EXP2$condition == "Incongruous room",]$intercept = coef(m.Dist)$subject[[2]][c(1,2,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,21,22,23,24,25)]+coef(m.Dist)$subject[[4]][c(1,2,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,21,22,23,24,25)]
# results_tbl_intercept_EXP2[results_tbl_intercept_EXP2$condition == "Incongruous room",]$intercept = coef(m.Dist)$subject[[2]][c(1,2,4,5,7,8,10,11,14,15,16,17,21,22,23,25)]+coef(m.Dist)$subject[[4]][c(1,2,4,5,7,8,10,11,14,15,16,17,21,22,23,25)]

results_tbl_intercept_EXP2[results_tbl_intercept_EXP2$condition == "Non-individualized HRTF",]$expon = coef(m.Dist)$subject[[3]][c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,21,22,23,24)]
results_tbl_intercept_EXP2[results_tbl_intercept_EXP2$condition == "Incongruous room",]$expon = coef(m.Dist)$subject[[3]][c(1,2,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,21,22,23,24,25)]+coef(m.Dist)$subject[[5]][c(1,2,4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,21,22,23,24,25)]
# results_tbl_intercept_EXP2[results_tbl_intercept_EXP2$condition == "Incongruous room",]$expon = coef(m.Dist)$subject[[3]][c(1,2,4,5,7,8,10,11,14,15,16,17,21,22,23,25)]+coef(m.Dist)$subject[[5]][c(1,2,4,5,7,8,10,11,14,15,16,17,21,22,23,25)]


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
                     limits = c(-1.2,1.1),expand= c(0,0)) +
  expand_limits(y = c(0.5, 13.5))+
  annotate("text", x = 1.5, y = 0.78,  label = "*", size = 5) +
  annotate("segment", x = 1, xend = 2, y = 0.75, yend = 0.75, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(legend.position = "none",axis.text=element_text(size=10))
fig.intercept

fig.intercept2 <- random_effects_long %>%
  group_by(Subject, Condition) %>%
  summarise(Mintercept = mean(Intercept),
            N = n()) %>%
  ungroup() %>%
  ggplot(aes(x = Condition, 
             y = Mintercept, 
             colour = Condition, 
             fill = Condition,
             shape = Condition)) +
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
                     limits = c(-1.2,1.1),expand= c(0,0)) +
  expand_limits(y = c(0.5, 13.5))+
  annotate("text", x = 1.5, y = 0.78,  label = "*", size = 5) +
  annotate("segment", x = 1, xend = 2, y = 0.75, yend = 0.75, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(legend.position = "none",axis.text=element_text(size=10))
fig.intercept2




fig.exp <- results_tbl_intercept_EXP2 %>%
  group_by(subject, condition) %>%
  summarise(Mexp = mean(expon),
            N = n()) %>%
  ungroup() %>%
  ggplot(aes(x = condition, 
             y = Mexp, 
             colour = condition, 
             fill = condition,
             shape = condition)) +
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
  scale_y_continuous(name="Exponents",breaks=c(0,0.5,1),
                     limits = c(0,1.1),expand= c(0,0)) +
  expand_limits(y = c(0.5, 13.5))+
  # annotate("text", x = 1.5, y = 1.73,  label = "***", size = 5) +
  # annotate("segment", x = 1, xend = 2, y = 1.7, yend = 1.7, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(legend.position = "none",axis.text=element_text(size=10))

fig.exp

fig.exp2 <- random_effects_long %>%
  group_by(Subject, Condition) %>%
  summarise(Mexp = mean(Slope),
            N = n()) %>%
  ungroup() %>%
  ggplot(aes(x = Condition, 
             y = Mexp, 
             colour = Condition, 
             fill = Condition,
             shape = Condition)) +
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
  scale_y_continuous(name="Exponents",breaks=c(0,0.5,1),
                     limits = c(0,1.1),expand= c(0,0)) +
  expand_limits(y = c(0.5, 13.5))+
  # annotate("text", x = 1.5, y = 1.73,  label = "***", size = 5) +
  # annotate("segment", x = 1, xend = 2, y = 1.7, yend = 1.7, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(legend.position = "none",axis.text=element_text(size=10))

fig.exp2

t.test(results_tbl_intercept_EXP2[results_tbl_intercept_EXP2$condition == "Non-individualized HRTF",]$expon, results_tbl_intercept_EXP2[results_tbl_intercept_EXP2$condition == "Incongruous room",]$expon, paired = FALSE)

###Signed bias----
results_tbl_bias_EXP2 = filter(results_tbl_bias,  condition == "Incongruous room" | condition == "Non-individualized HRTF")

fig.signed_bias <- results_tbl_bias_EXP2 %>%
  group_by(subject, condition) %>%
  summarise(Msignedbias = mean(signed_bias_ind),
            Msignedbias = mean(sd_persigned_bias_ind),
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

table.signed_bias <- results_tbl_bias_EXP2 %>%
  group_by(subject, condition) %>%
  summarise(Msignedbias = mean(signed_bias_ind),
            N = n())
m.signedBias <- lm(Msignedbias ~ condition, 
                   data = table.signed_bias)
extract_stats(ggcoefstats(m.signedBias))
anov = anova(m.signedBias)
anov
###Figure 3----
Figure.2 =  ggarrange(Final.Fixed.Plot,
                      ggarrange(fig.exp+rremove("x.title") ,
                                fig.intercept+rremove("x.title") ,
                                fig.signed_bias+rremove("x.title"),
                                ncol = 3, labels = c("B", "C","D")),
                      labels = "A",
                      ncol = 1, nrow = 2,heights =c(2,1))

Figure.2
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "FIGURE2_SIN2", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot = Figure.2, width=12, height=14, units="cm", limitsize=FALSE, dpi=400)


Figure.22 =  ggarrange(fig.exp+rremove("x.title"),
                       fig.exp2+rremove("x.title"),
                       fig.intercept+rremove("x.title") ,
                       fig.intercept2+rremove("x.title") ,
                       ncol = 2, nrow = 2, labels = c("A","A_2", "B","B_2"),
                       heights =c(1,1))

Figure.22
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "ASD", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot = Figure.22, width=14, height=14, units="cm", limitsize=FALSE, dpi=400)


library(Routliers)
idx = results_tbl_EXP2$perc_dist == 0
results_tbl_EXP2[idx,]$perc_dist = 0.1
results_tbl_EXP2$logperc_dist = log10(results_tbl_EXP2$perc_dist)
tabla.ind.Eye <- results_tbl_intercept_EXP2 %>% 
  filter(condition == "Incongruous room") %>% 
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Eye$intercept,threshold = .5 ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.ind.Eye$intercept,pos_display=TRUE)
tabla.ind.Eye[res3$outliers_pos,]
tabla.ind.Eye[res3$outliers_pos,] $intercept

a = results_tbl_intercept_EXP2[results_tbl_intercept_EXP2$condition == "Incongruous room",]
subjects_to_remove <- c("S025")
a <- a[!(
  a$condition == "Incongruous room" &
    a$subject %in% subjects_to_remove
), ]



b = results_tbl_dis_mean[results_tbl_dis_mean$condition == "Incongruous room",]
subjects_to_remove <- c("S003", "S012","S025")
b <- b[!(
  b$condition == "Incongruous room" &
    b$subject %in% subjects_to_remove
), ]
results_a = merge(a,b, all = TRUE)

r <- round(cor(results_a$intercept, results_a$Merror), 2)
p <- cor.test(results_a$intercept, results_a$Merror)$p.value

fig8 = ggplot(results_a, aes(x = intercept, y = Merror))+

  geom_smooth(method="lm", col="black") + 
  annotate("text", x=0, y=1, label=paste0("r = ", r), hjust=0) +
  annotate("text", x=0, y=0.9, label=paste0("p = ", round(p, 4)), hjust=0) +
  geom_point(data = results_a, aes(x = intercept, y = Merror), shape = 17, color = "#CC79A7", size = 4)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  xlab("Intercept")+
  ylab("Merror")+
  theme_classic() 

fig8

b = ggarrange(fig0+rremove("xlab"),fig1+rremove("xlab"),fig2+rremove("xlab"),fig4,fig5,fig6,
              ncol=3, nrow=2, labels = c("A", "B", "C","D", "E", "F"),common.legend = TRUE, legend="bottom")
b
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Pearhttp://127.0.0.1:23613/graphics/9acba65b-6e8b-4198-8037-196659a43594.pngson", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot = b, width=20, height=15, units="cm", limitsize=FALSE, dpi=200)






fig.signed_bias <- results_tbl_bias_EXP2 %>%
  group_by(subject, condition) %>%
  summarise(Msignedbias = mean(signed_bias_ind),
            Msignedbias = mean(sd_persigned_bias_ind),
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

table.signed_bias <- results_tbl_bias_EXP2 %>%
  group_by(subject, condition) %>%
  summarise(Msignedbias = mean(signed_bias_ind),
            N = n())
m.signedBias <- lm(Msignedbias ~ condition, 
                   data = table.signed_bias)
extract_stats(ggcoefstats(m.signedBias))
anov = anova(m.signedBias)
anov
###Figure 3----
Figure.2 =  ggarrange(Final.Fixed.Plot,
                      ggarrange(fig.exp+rremove("x.title") ,
                                fig.intercept+rremove("x.title") ,
                                fig.signed_bias+rremove("x.title"),
                                ncol = 3, labels = c("B", "C","D")),
                      labels = "A",
                      ncol = 1, nrow = 2,heights =c(2,1))

Figure.2
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "FIGURE2_SIN2", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot = Figure.2, width=12, height=14, units="cm", limitsize=FALSE, dpi=400)


Figure.22 =  ggarrange(fig.exp+rremove("x.title"),
                       fig.exp2+rremove("x.title"),
                       fig.intercept+rremove("x.title") ,
                       fig.intercept2+rremove("x.title") ,
                       ncol = 2, nrow = 2, labels = c("A","A_2", "B","B_2"),
                       heights =c(1,1))

Figure.22
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "ASD", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot = Figure.22, width=14, height=14, units="cm", limitsize=FALSE, dpi=400)


library(Routliers)

tabla.ind.Eye <- results_tbl_intercept_EXP2 %>% 
  filter(condition == "Incongruous room") %>% 
  ungroup()
res3 <- outliers_mad(x = tabla.ind.Eye$intercept,threshold = 2 ,na.rm=TRUE)
plot_outliers_mad(res3,x=tabla.ind.Eye$intercept,pos_display=TRUE)
tabla.ind.Eye[res3$outliers_pos,] 


a = results_tbl_intercept_EXP2[results_tbl_intercept_EXP2$condition == "Incongruous room",]
subjects_to_remove <- c("S025")
a <- a[!(
  a$condition == "Incongruous room" &
    a$subject %in% subjects_to_remove
), ]


### SD analisis ------
results_tbl_sd = filter(results_tbl,  condition == "Incongruous room" | condition == "Non-individualized HRTF")
cbPalette <- c("#CC79A7","#009E73", "#0072B2", "black","#999999", "#D55E00", "#CC79A7", "#F0E442")
table_sd <- results_tbl_sd %>%
  group_by(condition,target_distance) %>%
  summarise(SDPerc = sd(perc_dist, na.rm = TRUE),
            N = n()) %>%
  ungroup()
  a = ggplot(table_sd, aes(x = target_distance, 
             y = SDPerc, 
             colour = condition, 
             fill = condition,
             shape = condition)) +
  geom_point(alpha = 1) +
    geom_line()+
  
  scale_shape_manual(values=c(17, 23))+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) +
  # scale_x_discrete(name="Condition",labels = c("CR","IR"))+
  # scale_y_continuous(name="Signed bias",breaks=c(0,0.5,1,1.5,2),
  #                    limits = c(0,3),expand= c(0,0)) +
  # expand_limits(y = c(0.5, 13.5))+
  # annotate("text", x = 1.5, y = 1.73,  label = "*", size = 5) +
  # annotate("segment", x = 1, xend = 2, y = 1.7, yend = 1.7, colour = "black", size=.5, alpha=1,)+
  theme_pubr(base_size = 10, margin = TRUE)+
  theme(legend.position = "top",axis.text=element_text(size=10))
a
  fig.signed_bias
table.signed_bias <- results_tbl_bias_EXP2 %>%
  group_by(subject, condition) %>%
  summarise(Msignedbias = mean(signed_bias_ind),
            N = n())
m.signedBias <- lm(Msignedbias ~ condition, 
                   data = table.signed_bias)
extract_stats(ggcoefstats(m.signedBias))
anov = anova(m.signedBias)
anov
#-----
# b = results_tbl_dis_mean[results_tbl_dis_mean$condition == "Incongruous room",]
# subjects_to_remove <- c("S003", "S012","S025")
# b <- b[!(
#   b$condition == "Incongruous room" &
#     b$subject %in% subjects_to_remove
# ), ]
# results_a = merge(a,b, all = TRUE)

r <- round(cor(results_a$intercept, results_a$Merror), 2)
p <- cor.test(results_a$intercept, results_a$Merror)$p.value

fig8 = ggplot(results_a, aes(x = intercept, y = Merror))+
  
  geom_smooth(method="lm", col="black") + 
  annotate("text", x=0, y=1, label=paste0("r = ", r), hjust=0) +
  annotate("text", x=0, y=0.9, label=paste0("p = ", round(p, 4)), hjust=0) +
  geom_point(data = results_a, aes(x = intercept, y = Merror), shape = 17, color = "#CC79A7", size = 4)+
  scale_colour_manual(values = cbPalette) + 
  scale_fill_manual(values = cbPalette) + 
  xlab("Intercept")+
  ylab("Merror")+
  theme_classic() 

fig8

b = ggarrange(fig0+rremove("xlab"),fig1+rremove("xlab"),fig2+rremove("xlab"),fig4,fig5,fig6,
              ncol=3, nrow=2, labels = c("A", "B", "C","D", "E", "F"),common.legend = TRUE, legend="bottom")
b
mi_nombre_de_archivo = paste(figures_folder, .Platform$file.sep, "Pearhttp://127.0.0.1:23613/graphics/9acba65b-6e8b-4198-8037-196659a43594.pngson", ".png", sep = '')
ggsave(mi_nombre_de_archivo, plot = b, width=20, height=15, units="cm", limitsize=FALSE, dpi=200)


