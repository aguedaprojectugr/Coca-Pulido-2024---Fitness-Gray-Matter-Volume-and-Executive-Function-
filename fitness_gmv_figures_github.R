pacman::p_load(jtools, broom, sjPlot,rlang,readr,readxl,tidyverse,hrbrthemes,openxlsx,ggsci,ggpmisc,data.table,zoo,gtools,
               pipeR,tableone,survival,RNOmni,reshape2,showtext,ppcor,Hmisc,corrplot,psych,ggpubr,openxlsx,patchwork,cowplot,DescTools, GGally, ggplot2)

# Scatter plot modelo 1: handgrip -----------------------------------------
data <- read_excel("datafinal_9abr24.xlsx", sheet = "m1_handgrip")
handgrip_inf_front_gyrus <- lm(inf_fron_gyrus ~ handgrip_score_mean + screen_gender + screen_age + screen_years_edu + dxa_bmi, data = data)
hs_inf_fron_gy <- effect_plot(handgrip_inf_front_gyrus, pred = handgrip_score_mean, 
                              interval = T, partial.residuals = T,
                              colors = "#068934", point.size = 4, line.thickness = 1, line.color = "black") + 
  labs( x = "Handgrip strength (kg)",
        y = "GMV" ) + 
  theme(
    # Modify the color of the lines
    text = element_text(family = "Times New Roman", face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Change axis line color to black
    axis.text = element_text(color = "black", size = 30, face = "bold"),  # Change axis text color to black
    axis.title = element_text(color = "black",size = 30, face = "bold"), 
    plot.title = element_text(color = "black", size = 30, face = "bold"),  # Modify title text color, size, and style
    plot.subtitle = element_text(color = "black", size = 30, face = "bold") , # Modify subtitle text color and size
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("hs_inf_fron_gy.png", hs_inf_fron_gy, dpi = 300)

#Additional settings for labs -> caption = "Data from the 1974 Motor Trend US magazine.",  tag = "Figure 1",
handgrip_cin_gyrus <- lm(cin_gyrus ~ handgrip_score_mean + screen_gender + screen_age + screen_years_edu + dxa_bmi, data = data)
hs_cin_gyrus <- effect_plot(handgrip_cin_gyrus, pred = handgrip_score_mean, 
                              interval = T, partial.residuals = T,
                              colors = "#068934", point.size = 4, line.thickness = 1, line.color = "black") + 
  labs( x = "Handgrip strength (kg)",
        y = "GMV" ) + 
  theme(
    # Modify the color of the lines
    text = element_text(family = "Times New Roman", face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Change axis line color to black
    axis.text = element_text(color = "black", size = 30, face = "bold"),  # Change axis text color to black
    axis.title = element_text(color = "black",size = 30, face = "bold"), 
    plot.title = element_text(color = "black", size = 30, face = "bold"),  # Modify title text color, size, and style
    plot.subtitle = element_text(color = "black", size = 30, face = "bold") , # Modify subtitle text color and size
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("hs_cin_gyrus.png", hs_cin_gyrus, dpi = 300)

handgrip_med_orb_gyrus <- lm(med_orb_gyrus ~ handgrip_score_mean + screen_gender + screen_age + screen_years_edu + dxa_bmi, data = data)
hs_med_orb_gyrus <- effect_plot(handgrip_med_orb_gyrus, pred = handgrip_score_mean, 
                            interval = T, partial.residuals = T,
                            colors = "#C20150", point.size = 4, line.thickness = 1, line.color = "black") + 
  labs( x = "Handgrip strength (kg)",
        y = "GMV" ) + 
  theme(
    # Modify the color of the lines
    text = element_text(family = "Times New Roman", face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Change axis line color to black
    axis.text = element_text(color = "black", size = 30, face = "bold"),  # Change axis text color to black
    axis.title = element_text(color = "black",size = 30, face = "bold"), 
    plot.title = element_text(color = "black", size = 30, face = "bold"),  # Modify title text color, size, and style
    plot.subtitle = element_text(color = "black", size = 30, face = "bold") , # Modify subtitle text color and size
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("hs_med_orb_gyrus.png", hs_med_orb_gyrus, dpi = 300)

handgrip_ant_orb_gyrus <- lm(ant_orb_gyrus ~ handgrip_score_mean + screen_gender + screen_age + screen_years_edu + dxa_bmi, data = data)
hs_ant_orb_gyrus <- effect_plot(handgrip_ant_orb_gyrus, pred = handgrip_score_mean, 
                                interval = T, partial.residuals = T,
                                colors = "#068934", point.size = 4, line.thickness = 1, line.color = "black") + 
  labs( x = "Handgrip strength (kg)",
        y = "GMV" ) + 
  theme(
    # Modify the color of the lines
    text = element_text(family = "Times New Roman", face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Change axis line color to black
    axis.text = element_text(color = "black", size = 30, face = "bold"),  # Change axis text color to black
    axis.title = element_text(color = "black",size = 30, face = "bold"), 
    plot.title = element_text(color = "black", size = 30, face = "bold"),  # Modify title text color, size, and style
    plot.subtitle = element_text(color = "black", size = 30, face = "bold") , # Modify subtitle text color and size
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("hs_ant_orb_gyrus.png", hs_ant_orb_gyrus, dpi = 300)

handgrip_insula <- lm(insula ~ handgrip_score_mean + screen_gender + screen_age + screen_years_edu + dxa_bmi, data = data)
hs_insula <- effect_plot(handgrip_insula, pred = handgrip_score_mean, 
                                interval = T, partial.residuals = T,
                                colors = "#068934", point.size = 4, line.thickness = 1, line.color = "black") + 
  labs( x = "Handgrip strength (kg)",
        y = "GMV" ) + 
  theme(
    # Modify the color of the lines
    text = element_text(family = "Times New Roman", face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Change axis line color to black
    axis.text = element_text(color = "black", size = 30, face = "bold"),  # Change axis text color to black
    axis.title = element_text(color = "black",size = 30, face = "bold"), 
    plot.title = element_text(color = "black", size = 30, face = "bold"),  # Modify title text color, size, and style
    plot.subtitle = element_text(color = "black", size = 30, face = "bold") , # Modify subtitle text color and size
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("hs_insula.png", hs_insula, dpi = 300)

handgrip_parahippo <- lm(parahippo ~ handgrip_score_mean + screen_gender + screen_age + screen_years_edu + dxa_bmi, data = data)
hs_parahippo <- effect_plot(handgrip_parahippo, pred = handgrip_score_mean, 
                         interval = T, partial.residuals = T,
                         colors = "#068934", point.size = 4, line.thickness = 1, line.color = "black") + 
  labs( x = "Handgrip strength (kg)",
        y = "GMV" ) + 
  theme(
    # Modify the color of the lines
    text = element_text(family = "Times New Roman", face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Change axis line color to black
    axis.text = element_text(color = "black", size = 30, face = "bold"),  # Change axis text color to black
    axis.title = element_text(color = "black",size = 30, face = "bold"), 
    plot.title = element_text(color = "black", size = 30, face = "bold"),  # Modify title text color, size, and style
    plot.subtitle = element_text(color = "black", size = 30, face = "bold") , # Modify subtitle text color and size
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("hs_parahippo.png", hs_parahippo, dpi = 300)

handgrip_cer_iv_v_ling_gyrus <- lm(cer_iv_v_ling_gyrus ~ handgrip_score_mean + screen_gender + screen_age + screen_years_edu + dxa_bmi, data = data)
hs_cer_iv_v_ling_gyrus <- effect_plot(handgrip_cer_iv_v_ling_gyrus, pred = handgrip_score_mean, 
                            interval = T, partial.residuals = T,
                            colors = "#068934", point.size = 4, line.thickness = 1, line.color = "black") + 
  labs( x = "Handgrip strength (kg)",
        y = "GMV" ) + 
  theme(
    # Modify the color of the lines
    text = element_text(family = "Times New Roman", face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Change axis line color to black
    axis.text = element_text(color = "black", size = 30, face = "bold"),  # Change axis text color to black
    axis.title = element_text(color = "black",size = 30, face = "bold"), 
    plot.title = element_text(color = "black", size = 30, face = "bold"),  # Modify title text color, size, and style
    plot.subtitle = element_text(color = "black", size = 30, face = "bold") , # Modify subtitle text color and size
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("hs_cer_iv_v_ling_gyrus.png", hs_cer_iv_v_ling_gyrus, dpi = 300)


handgrip_cer_vi_fus_gyrus <- lm(cer_vi_fus_gyrus ~ handgrip_score_mean + screen_gender + screen_age + screen_years_edu + dxa_bmi, data = data)
hs_cer_vi_fus_gyrus <- effect_plot(handgrip_cer_vi_fus_gyrus, pred = handgrip_score_mean, 
                                      interval = T, partial.residuals = T,
                                      colors = "#C20150", point.size = 4, line.thickness = 1, line.color = "black") + 
  labs( x = "Handgrip strength (kg)",
        y = "GMV" ) + 
  theme(
    # Modify the color of the lines
    text = element_text(family = "Times New Roman", face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Change axis line color to black
    axis.text = element_text(color = "black", size = 30, face = "bold"),  # Change axis text color to black
    axis.title = element_text(color = "black",size = 30, face = "bold"), 
    plot.title = element_text(color = "black", size = 30, face = "bold"),  # Modify title text color, size, and style
    plot.subtitle = element_text(color = "black", size = 30, face = "bold") , # Modify subtitle text color and size
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("hs_cer_vi_fus_gyrus.png", hs_cer_vi_fus_gyrus, dpi = 300)

handgrip_cer_crus_ii <- lm(cer_crus_ii ~ handgrip_score_mean + screen_gender + screen_age + screen_years_edu + dxa_bmi, data = data)
hs_cer_crus_ii <- effect_plot(handgrip_cer_crus_ii, pred = handgrip_score_mean, 
                                   interval = T, partial.residuals = T,
                                   colors = "#6D1CF1", point.size = 4, line.thickness = 1, line.color = "black") + 
  labs( x = "Handgrip strength (kg)",
        y = "GMV" ) + 
  theme(
    # Modify the color of the lines
    text = element_text(family = "Times New Roman", face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Change axis line color to black
    axis.text = element_text(color = "black", size = 30, face = "bold"),  # Change axis text color to black
    axis.title = element_text(color = "black",size = 30, face = "bold"), 
    plot.title = element_text(color = "black", size = 30, face = "bold"),  # Modify title text color, size, and style
    plot.subtitle = element_text(color = "black", size = 30, face = "bold") , # Modify subtitle text color and size
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("hs_cer_crus_ii.png", hs_cer_crus_ii, dpi = 300)

#  Scatter plot modelo 5: squats ------------------------------------------
data <- read_excel("datafinal_9abr24.xlsx", sheet = "m5_squats")
squats_thalamus <- lm(thalamus ~ pf_sft_chair_score + screen_gender + screen_age + screen_years_edu + dxa_bmi, data = data)
s_thalamus <- effect_plot(squats_thalamus, pred = pf_sft_chair_score, 
                              interval = T, partial.residuals = T,
                              colors = "#068934", point.size = 4, line.thickness = 1, line.color = "black") + 
  labs( x = "Squats (rep)",
        y = "GMV" ) + 
  theme(
    # Modify the color of the lines
    text = element_text(family = "Times New Roman", face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Change axis line color to black
    axis.text = element_text(color = "black", size = 30, face = "bold"),  # Change axis text color to black
    axis.title = element_text(color = "black",size = 30, face = "bold"), 
    plot.title = element_text(color = "black", size = 30, face = "bold"),  # Modify title text color, size, and style
    plot.subtitle = element_text(color = "black", size = 30, face = "bold") , # Modify subtitle text color and size
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("s_thalamus.png", s_thalamus, dpi = 300)


squats_culmen <- lm(culmen ~ pf_sft_chair_score + screen_gender + screen_age + screen_years_edu + dxa_bmi, data = data)
s_culmen <- effect_plot(squats_culmen, pred = pf_sft_chair_score, 
                          interval = T, partial.residuals = T,
                          colors = "#068934", point.size = 4, line.thickness = 1, line.color = "black") + 
  labs( x = "Squats (rep)",
        y = "GMV" ) + 
  theme(
    # Modify the color of the lines
    text = element_text(family = "Times New Roman", face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Change axis line color to black
    axis.text = element_text(color = "black", size = 30, face = "bold"),  # Change axis text color to black
    axis.title = element_text(color = "black",size = 30, face = "bold"), 
    plot.title = element_text(color = "black", size = 30, face = "bold"),  # Modify title text color, size, and style
    plot.subtitle = element_text(color = "black", size = 30, face = "bold") , # Modify subtitle text color and size
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("s_culmen.png", s_culmen, dpi = 300)


# Scatter plot modelo 9: knee extension -----------------------------------

data <- read_excel("datafinal_9abr24.xlsx", sheet = "m9_knee-extension")
knee_lat_sup_front_gyrus <- lm(lat_sup_front_gyrus ~ Peak_iso_Low + screen_gender + screen_age + screen_years_edu + dxa_bmi, data = data)
ke_lat_sup_front_gyrus <- effect_plot(knee_lat_sup_front_gyrus, pred = Peak_iso_Low, 
                        interval = T, partial.residuals = T,
                        colors = "#068934", point.size = 4, line.thickness = 1, line.color = "black") + 
  labs( x = "Knee extension strength (Nm)",
        y = "GMV" ) + 
  theme(
    # Modify the color of the lines
    text = element_text(family = "Times New Roman", face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Change axis line color to black
    axis.text = element_text(color = "black", size = 30, face = "bold"),  # Change axis text color to black
    axis.title = element_text(color = "black",size = 30, face = "bold"), 
    plot.title = element_text(color = "black", size = 30, face = "bold"),  # Modify title text color, size, and style
    plot.subtitle = element_text(color = "black", size = 30, face = "bold") , # Modify subtitle text color and size
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("ke_lat_sup_front_gyrus.png", ke_lat_sup_front_gyrus, dpi = 300)

knee_left_cer_crus_i <- lm(left_cer_crus_i ~ Peak_iso_Low + screen_gender + screen_age + screen_years_edu + dxa_bmi, data = data)
ke_left_cer_crus_i <- effect_plot(knee_left_cer_crus_i, pred = Peak_iso_Low, 
                                      interval = T, partial.residuals = T,
                                      colors = "#068934", point.size = 4, line.thickness = 1, line.color = "black") + 
  labs( x = "Knee extension strength (Nm)",
        y = "GMV" ) + 
  theme(
    # Modify the color of the lines
    text = element_text(family = "Times New Roman", face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Change axis line color to black
    axis.text = element_text(color = "black", size = 30, face = "bold"),  # Change axis text color to black
    axis.title = element_text(color = "black",size = 30, face = "bold"), 
    plot.title = element_text(color = "black", size = 30, face = "bold"),  # Modify title text color, size, and style
    plot.subtitle = element_text(color = "black", size = 30, face = "bold") , # Modify subtitle text color and size
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("ke_left_cer_crus_i.png", ke_left_cer_crus_i, dpi = 300)

knee_rigth_cer_crus_i <- lm(rigth_cer_crus_i ~ Peak_iso_Low + screen_gender + screen_age + screen_years_edu + dxa_bmi, data = data)
ke_rigth_cer_crus_i <- effect_plot(knee_rigth_cer_crus_i, pred = Peak_iso_Low, 
                                  interval = T, partial.residuals = T,
                                  colors = "#C20150", point.size = 4, line.thickness = 1, line.color = "black") + 
  labs( x = "Knee extension strength (Nm)",
        y = "GMV" ) + 
  theme(
    # Modify the color of the lines
    text = element_text(family = "Times New Roman", face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Change axis line color to black
    axis.text = element_text(color = "black", size = 30, face = "bold"),  # Change axis text color to black
    axis.title = element_text(color = "black",size = 30, face = "bold"), 
    plot.title = element_text(color = "black", size = 30, face = "bold"),  # Modify title text color, size, and style
    plot.subtitle = element_text(color = "black", size = 30, face = "bold") , # Modify subtitle text color and size
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("ke_rigth_cer_crus_i.png", ke_rigth_cer_crus_i, dpi = 300)

# Scatter plot modelo 2km -----------------------------------
data <- read_excel("datafinal_9abr24.xlsx", sheet = "twokm")
twokm_thalamus <- lm(thalamus ~ pf_2km_walk_time_revert + screen_gender + screen_age + screen_years_edu + dxa_bmi, data = data)
tkm_thalamus <- effect_plot(twokm_thalamus, pred = pf_2km_walk_time_revert, 
                                   interval = T, partial.residuals = T,
                                   colors = "#068934", point.size = 4, line.thickness = 1, line.color = "black") + 
  labs( x = "2-km walking time (sec)",
        y = "GMV" ) + 
  theme(
    # Modify the color of the lines
    text = element_text(family = "Times New Roman", face = "bold"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black"),  # Change axis line color to black
    axis.text = element_text(color = "black", size = 30, face = "bold"),  # Change axis text color to black
    axis.title = element_text(color = "black",size = 30, face = "bold"), 
    plot.title = element_text(color = "black", size = 30, face = "bold"),  # Modify title text color, size, and style
    plot.subtitle = element_text(color = "black", size = 30, face = "bold") , # Modify subtitle text color and size
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave("tkm_thalamus.png", tkm_thalamus, dpi = 300)

