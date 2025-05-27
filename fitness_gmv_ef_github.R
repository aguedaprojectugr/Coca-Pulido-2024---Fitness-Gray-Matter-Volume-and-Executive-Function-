pacman::p_load(rlang,readr,readxl,tidyverse,hrbrthemes,openxlsx,ggsci,ggpmisc,data.table,zoo,gtools, rstatix, generics, broom, lm.beta,
               pipeR,tableone,survival,RNOmni,reshape2,showtext,ppcor,Hmisc,corrplot,psych,ggpubr,openxlsx,patchwork,cowplot,DescTools, GGally)

# Table 1 descriptive -----------------------------------------------------
data <- read_excel("database_2.xlsx" , sheet = "variables")
data$screen_age = as.numeric(data$screen_age)
data$screen_years_edu = as.numeric(data$screen_years_edu)
data$screen_gender = as.factor(data$screen_gender)
data$dxa_wt_ave = as.numeric(data$dxa_wt_ave)
data$dxa_ht_ave = as.numeric(data$dxa_ht_ave)
data$dxa_bmi = as.numeric(data$dxa_bmi)
data$dxa_bmi_cat = as.factor(data$dxa_bmi_cat)
data$moca_total_score = as.numeric(data$moca_total_score)
data$mmse_total_score = as.numeric(data$mmse_total_score)
data$pf_2km_walk_time = as.numeric(data$pf_2km_walk_time)
data$handgrip_score_mean = as.numeric(data$handgrip_score_mean)
data$pf_sft_arm_score = as.numeric(data$pf_sft_arm_score)
data$pf_5stands_time = as.numeric(data$pf_5stands_time)
data$pf_sft_chair_score = as.numeric(data$pf_sft_chair_score)
data$Peak_iso_Low = as.numeric(data$Peak_iso_Low)
data$Peak_iso_Upp = as.numeric(data$Peak_iso_Upp)
data$stics_total_score = as.numeric(data$stics_total_score)
data$pf_sft_6minutewalk = as.numeric(data$pf_sft_6minutewalk)
data$comp_ex_func_mean_z = as.numeric(data$comp_ex_func_mean_z)
Overall <- print(tableone::CreateTableOne(vars = c("screen_age","screen_years_edu","dxa_wt_ave",
                                                   "dxa_ht_ave", "dxa_bmi","moca_total_score","mmse_total_score", "stics_total_score" , "comp_ex_func_mean_z", "pf_2km_walk_time","handgrip_score_mean", "pf_sft_arm_score" , "pf_5stands_time" , "pf_sft_chair_score" , "Peak_iso_Low" , "Peak_iso_Upp"  , "pf_sft_6minutewalk"), addOverall = F,
                                          data=data), showAllLevels = F)

Bysex <- print(tableone::CreateTableOne(vars = c("screen_age","screen_years_edu","dxa_wt_ave",
                                                 "dxa_ht_ave", "dxa_bmi","moca_total_score","mmse_total_score", "stics_total_score" ,  "comp_ex_func_mean_z", "pf_2km_walk_time","handgrip_score_mean" , "pf_sft_arm_score" , "pf_5stands_time" , "pf_sft_chair_score" , "Peak_iso_Low" , "Peak_iso_Upp" , "pf_sft_6minutewalk"),
                                        strata = "screen_gender", data=data), showAllLevels = F)
Table1 <- as.data.frame(cbind(Overall, Bysex)) %>% 
  rename("Sex Diff p-value"=p)
Table1$test = NULL
setDT(Table1, keep.rownames = TRUE)[]
write.xlsx(Table1,  "Desc_andrea_cambiosEF.xlsx", rownames = FALSE)
plot(data$screen_gender, data$comp_ex_func_mean_z)


# Correlation matrix ------------------------------------------------------
data <- read_excel("/Users/Agueda/neurodesktop-storage/muscular_strength_ACP/r_projects/Analysis/FIRST_PAPER_ACP/database.xlsx")

excel_sheets("/Users/Agueda/neurodesktop-storage/muscular_strength_ACP/r_projects/Analysis/FIRST_PAPER_ACP/database.xlsx")

variables = subset(data, select = -c( record_id))

variables <- data.frame(lapply(variables, function(x) as.numeric(as.character(x))))

cormat <- round(cor(variables),2)
cormat <- rcorr(as.matrix(variables) , type = "pearson")
print(cormat)

correlacion = rcorr(as.matrix(variables), type = "pearson")
R.correlacion = correlacion$r
p.correlacion = correlacion$P
tiff("cor_biomarkers.tiff", units="in", width=20, height=20, res=300)
corrplot(R.correlacion  , p.mat = p.correlacion , tl.col = "black" , tl.srt=45 ,  
         number.cex = 1 , cl.cex = 1  , type = "lower" , method = "color" , sig.level = 0.05, tl.cex = 1  ,  addCoef.col = "black" )
dev.off()
insig = "blank"
# Create the correlation matrix
correlacion = rcorr(as.matrix(variables), type = "pearson")
R.correlacion = correlacion$r
p.correlacion = correlacion$P

# Define the new names for the variables in descending order
new_colnames <- c("Age", "BMI", "Education", "Handgrip strength", 
                  "Biceps curl", "Squats", "Elbow extension", 
                  "Knee extension", "2-km walking test", "6-minutes walk")


# Change the variable names of the correlation matrix and the p-value matrix.
colnames(R.correlacion) <- new_colnames
rownames(R.correlacion) <- new_colnames
colnames(p.correlacion) <- new_colnames
rownames(p.correlacion) <- new_colnames

colores_personalizados <- colorRampPalette(c("#6f10ce", "white" ,"#F05D0E"))(200)  # De rojo a azul pasando por blanco


# Generate the correlation graph with the new names
tiff("cor_descript_3.tiff", units="in", width=15, height=15, res=250)
par(mar = c(5, 7, 7, 5))  # Ajustar márgenes (inferior, izquierdo, superior, derecho)

corrplot(R.correlacion, 
         p.mat = p.correlacion, 
         col = colores_personalizados, 
         tl.col = "black", 
         tl.srt = 90, 
         number.cex = 1.7,  # Tamaño de los números de la matriz
         cl.cex = 1.5,  # Tamaño de la leyenda de la barra de colores
         insig = "blank", 
         type = "lower", 
         method = "color", 
         sig.level = 0.05, 
         tl.cex = 2,
         tl.offset = 0.8,
         cl.pos = "r", # Tamaño de los nombres de las variables
         addCoef.col = "black")


dev.off()


# Identify outliers -------------------------------------------------

outlier_values_4sd <- list()

for (col in names(data_fitness)) {
  mean_x <- mean(data_fitness[[col]])
  sd_x <- sd(data_fitness[[col]])
  abs_dev <- abs(data_fitness[[col]] - mean_x)
  outliers <- data_fitness[[col]][abs_dev > 4 * sd_x]
  if (length(outliers) > 0) {
    outlier_values_4sd[[col]] <- data.frame(valor = outliers)
  }
}

#Identify  outliers - 3sd 

outlier_values_3sd <- list()

for (col in names(data_fitness)) {
  mean_x <- mean(data_fitness[[col]])
  sd_x <- sd(data_fitness[[col]])
  abs_dev <- abs(data_fitness[[col]] - mean_x)
  outliers <- data_fitness[[col]][abs_dev > 3 * sd_x]
  if (length(outliers) > 0) {
    outlier_values_4sd[[col]] <- data.frame(valor = outliers)
  }
}
# Betas calculaion: indicador fitness + gmv --------------------------------------------------------
# Model 1: Hangrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables_peak")
m1$TBV <- as.numeric(m1$TBV)
Covariable1 = "screen_age" # covariate
Covariable2 = "screen_gender" # covariate
Covariable3 = "screen_years_edu" # covariate
Covariable4 = "dxa_bmi"
Pos = c(10)    ### Independent indicator position (muscular strength indicator)
Pos2 = c(25:33)    ### Outcome variable position (eigenvalues)
Resultado.m1 = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~  unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4), m1) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1,Resultado)
  }
}

colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "ftness_gmv_m1.xlsx", rowNames = TRUE)

# Model 5: Squats (coordinate deleted for not complying with the gray matter limit in the xjview inspection -521227)----------------------------------------------
m5 <- read_excel("database_2.xlsx" , sheet = "m5_variables")
m5 <- m5 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" # Covariable
Covariable2 = "screen_gender" # Covariable
Covariable3 = "screen_years_edu"
Covariable4 = "dxa_bmi"

Pos = c(17)   ### Independent indicator position (muscular strength indicator)
Pos2 = c(25:27)  ### Outcome variable position (eigenvalues)
Resultado.m5 = data.frame(NULL)       

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m5[, j]) ~  unlist(m5[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4), m5) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m5)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m5[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m5 = rbind(Resultado.m5,Resultado)
  }
}

colnames(Resultado.m5) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m5, Resultado.m5$p.value < 0.05)
write.xlsx(Resultado.m5, "fitness_gmv_m5.xlsx", rowNames = TRUE)

# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables")
m9 <- m9 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Covariable4 = "dxa_bmi"

Pos = c(23) ### Independent indicator position (muscular strength indicator)
Pos2 = c(25:28)   ### Outcome variable position (eigenvalues)
Resultado.m9 = data.frame(NULL)         

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m9[, j]) ~  unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4), m9) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m9)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m9[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m9 = rbind(Resultado.m9,Resultado)
  }
}

colnames(Resultado.m9) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m9, Resultado.m9$p.value < 0.05)
write.xlsx(Resultado.m9, "fitness_gmv_m9.xlsx", rowNames = TRUE)

# Model 11: Elbow extension (Finally eliminated for not complying with the amount of gray matter in the xjview inspection). -----------------------------------------------
m11 <- read_excel("database_2.xlsx" , sheet = "m11_variables")
m11 <- m11 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" # Covariable
Covariable2 = "screen_gender" # Covariable
Covariable3 = "screen_years_edu"
Covariable4 = "dxa_bmi"

Pos = c(24)  ### Independent indicator position (muscular strength indicator)
Pos2 = c(25)   ### Outcome variable position (eigenvalues)
Resultado.m11 = data.frame(NULL)        

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m11[, j]) ~  unlist(m11[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4), m11) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m11)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m11[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m11 = rbind(Resultado.m11,Resultado)
  }
}

colnames(Resultado.m11) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m11, Resultado.m11$p.value < 0.05)
write.xlsx(Resultado.m11, "fitness_gmv_m11.xlsx", rowNames = TRUE)

# Model 2km-----------------------------------------------
twokm <- read_excel("database_2.xlsx" , sheet = "twokm_variables")
twokm <- twokm %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" # Covariable
Covariable2 = "screen_gender" # Covariable
Covariable3 = "screen_years_edu"
Covariable4 = "dxa_bmi"

Pos = c(8)   ### Independent indicator position (muscular strength indicator)
Pos2 = c(26:27) ### Outcome variable position (eigenvalues)
Resultado.twokm = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(twokm[, j]) ~  unlist(twokm[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4), twokm) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(twokm)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(twokm[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.twokm = rbind(Resultado.twokm,Resultado)
  }
}

colnames(Resultado.twokm) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.twokm, Resultado.twokm$p.value < 0.05)
write.xlsx(Resultado.twokm, "fitness_gmv_twokm.xlsx", rowNames = TRUE)


# Regression analysis:fitness associated regions + executive function (cluster) -----------------------------------------------------
# Model 1: Handgrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables")
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age"))
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25:33) ### Independent indicator position (muscular strength indicator)
Pos2 = c(35)     ### Outcome variable position (eigenvalues)
Resultado.m1 = data.frame(NULL)            

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~  unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m1) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1,Resultado)
  }
}

# Without covariates
for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~  unlist(m1[, i]), m1) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1,Resultado)
  }
}

colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "fitness_gmv_ef_m1.xlsx", rowNames = TRUE)

# Model 5: Squats (coordinate deleted for not complying with the gray matter limit in the xjview inspection -521227)-----------------------------------------------
m5 <- read_excel("database_2.xlsx" , sheet = "m5_variables")
m5 <- m5 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25:27)  ### Independent indicator position (muscular strength indicator)
Pos2 = c(29)   ### Outcome variable position (eigenvalues)
Resultado.m5 = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m5[, j]) ~  unlist(m5[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m5) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m5)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m5[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m5 = rbind(Resultado.m5,Resultado)
  }
}

colnames(Resultado.m5) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m5, Resultado.m5$p.value < 0.05)
write.xlsx(Resultado.m5, "fitness_gmv_ef_m5.xlsx", rowNames = TRUE)

# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables")
m9 <- m9 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25:28)    ### Independent indicator position (muscular strength indicator)
Pos2 = c(30)    ### Outcome variable position (eigenvalues)
Resultado.m9 = data.frame(NULL)             

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m9[, j]) ~  unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m9) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m9)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m9[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m9 = rbind(Resultado.m9,Resultado)
  }
}

colnames(Resultado.m9) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m9, Resultado.m9$p.value < 0.05)
write.xlsx(Resultado.m9, "fitness_gmv_ef_m9.xlsx", rowNames = TRUE)

# Model 11: Elbow extension (Finally eliminated for not complying with the amount of gray matter in the xjview inspection) -----------------------------------------------
m11 <- read_excel("database_2.xlsx" , sheet = "m11_variables")
m11 <- m11 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25)  ### Independent indicator position (muscular strength indicator)
Pos2 = c(27) ### Outcome variable position (eigenvalues)
Resultado.m11 = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m11[, j]) ~  unlist(m11[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m11) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m11)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m11[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m11 = rbind(Resultado.m11,Resultado)
  }
}

colnames(Resultado.m11) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m11, Resultado.m11$p.value < 0.05)
write.xlsx(Resultado.m11, "fitness_gmv_ef_m11.xlsx", rowNames = TRUE)

# Model 2km-----------------------------------------------
twokm <- read_excel("database_2.xlsx" , sheet = "twokm_variables")
twokm <- twokm %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(26:27)   ### Independent indicator position (muscular strength indicator)
Pos2 = c(28) ### Outcome variable position (eigenvalues)
Resultado.twokm = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(twokm[, j]) ~  unlist(twokm[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), twokm) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(twokm)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(twokm[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.twokm = rbind(Resultado.twokm,Resultado)
  }
}

colnames(Resultado.twokm) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.twokm, Resultado.twokm$p.value < 0.05)
write.xlsx(Resultado.twokm, "fitness_gmv_ef_twokm.xlsx", rowNames = TRUE)

coeficientes <- coef(Equation)

# Regression analysis:fitness associated regions + executive function (cluster)  -----------------------------------------------------
# Model 1: Hangrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables_sin289(ef)")

m1 <- subset(m1, record_id != 289)
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age")) 
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(25:33)    ### Independent indicator position (muscular strength indicator)
Pos2 = c(35)  ### Outcome variable position (eigenvalues)
Resultado.m1 = data.frame(NULL)            

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~  unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m1) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1,Resultado)
  }
}

colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "fitness_gmv_ef_m1.xlsx", rowNames = TRUE)

# Model 5: Squats (coordinate deleted for not complying with the gray matter limit in the xjview inspection -521227)-----------------------------------------------
m5 <- read_excel("database_2.xlsx" , sheet = "m5_variables_sin289(ef)")
m5 <- subset(m5, record_id != 289)
m5 <- m5 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(25:27)    ### Independent indicator position (muscular strength indicator)
Pos2 = c(29) ### Outcome variable position (eigenvalues)
Resultado.m5 = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m5[, j]) ~  unlist(m5[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m5) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m5)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m5[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m5 = rbind(Resultado.m5,Resultado)
  }
}

colnames(Resultado.m5) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m5, Resultado.m5$p.value < 0.05)
write.xlsx(Resultado.m5, "fitness_gmv_ef_m5.xlsx", rowNames = TRUE)

# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables_sin289(ef)")
m9 <- subset(m9, record_id != 289)
m9 <- m9 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25:28) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(30)  ### Outcome variable position (eigenvalues)
Resultado.m9 = data.frame(NULL)  

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m9[, j]) ~  unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m9) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m9)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m9[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m9 = rbind(Resultado.m9,Resultado)
  }
}

colnames(Resultado.m9) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m9, Resultado.m9$p.value < 0.05)
write.xlsx(Resultado.m9, "fitness_gmv_ef_m9.xlsx", rowNames = TRUE)

# Model 11: Elbow extension  (Finally eliminated for not complying with the amount of gray matter in the xjview inspection).-----------------------------------------------
m11 <- read_excel("database_2.xlsx" , sheet = "m11_variables_sin289(ef)")
m11 <- subset(m11, record_id != 289)
m11 <- m11 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(25)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(27)  ### Outcome variable position (eigenvalues) 
Resultado.m11 = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m11[, j]) ~  unlist(m11[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m11) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m11)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m11[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m11 = rbind(Resultado.m11,Resultado)
  }
}

colnames(Resultado.m11) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m11, Resultado.m11$p.value < 0.05)
write.xlsx(Resultado.m11, "fitness_gmv_ef_m11.xlsx", rowNames = TRUE)

# Model 2km-----------------------------------------------
twokm <- read_excel("database_2.xlsx" , sheet = "twokm_variables_sin289(ef)")
twokm <- subset(twokm, record_id != 289)
twokm <- twokm %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age"
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(26:27) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(28)  ### Outcome variable position (eigenvalues) 
Resultado.twokm = data.frame(NULL)  

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(twokm[, j]) ~  unlist(twokm[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), twokm) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(twokm)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(twokm[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.twokm = rbind(Resultado.twokm,Resultado)
  }
}

colnames(Resultado.twokm) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.twokm, Resultado.twokm$p.value < 0.05)
write.xlsx(Resultado.twokm, "fitness_gmv_ef_twokm.xlsx", rowNames = TRUE)


# Regression analysis: adjusting by handgrip in 2km - walking test and viceversa (cluster) -----------------------------------------------------
# Model 1: Hangrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables")
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age")) 
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Covariable4 = "pf_2km_walk_time"
Covariable5 = "dxa_bmi"
Pos = c(10)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(25:33)  ### Outcome variable position (eigenvalues) 
Resultado.m1 = data.frame(NULL) 

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~  unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4) + get(Covariable5), m1) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1,Resultado)
  }
}

colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "handgrip_gmv_controlx2km_m1.xlsx", rowNames = TRUE)

# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables")
m9 <- ungroup(m9)
m9$TBV <- as.numeric(m9$TBV)
m9$screen_gender <- as.numeric(m9$screen_gender)
m9$screen_age <- as.numeric(m9$screen_age)
m9$screen_years_edu <- as.numeric(m9$screen_years_edu)
m9$pf_2km_walk_time <- as.numeric(m9$pf_2km_walk_time)
m9$dxa_bmi <- as.numeric(m9$dxa_bmi)
m9$Peak_iso_Low <- as.numeric(m9$Peak_iso_Low)
m9$Path <- NULL
which(colnames(m9)==c("screen_age"))
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Covariable4 = "pf_2km_walk_time"
Covariable5 = "dxa_bmi"
Pos = c(22)  ### Independent indicator position (muscular strength indicator) 
Pos2 = c(24:26)   ### Outcome variable position (eigenvalues) 
Resultado.m9 = data.frame(NULL) 

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m9[, j]) ~  unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4) + get(Covariable5), m9) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m9)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m9[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m9 = rbind(Resultado.m9,Resultado)
  }
}

colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "kneeextension_gmv_controlx2km_m1.xlsx", rowNames = TRUE)

# Model 5: Squats-----------------------------------------------
m5 <- read_excel("database_2.xlsx" , sheet = "m5_variables")
m5 <- ungroup(m5)
m5$TBV <- as.numeric(m5$TBV)
m5$screen_gender <- as.numeric(m5$screen_gender)
m5$screen_age <- as.numeric(m5$screen_age)
m5$screen_years_edu <- as.numeric(m5$screen_years_edu)
m5$pf_2km_walk_time <- as.numeric(m5$pf_2km_walk_time)
m5$dxa_bmi <- as.numeric(m5$dxa_bmi)
m5$pf_sft_chair_score<- as.numeric(m5$pf_sft_chair_score)
m5$Path <- NULL
which(colnames(m5)==c("screen_age"))
Covariable1 = "screen_age"
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Covariable4 = "pf_2km_walk_time"
Covariable5 = "dxa_bmi"
Pos = c(16)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(24:26) ### Outcome variable position (eigenvalues) 
Resultado.m5 = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m5[, j]) ~  unlist(m5[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4) + get(Covariable5), m5) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m5)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m5[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m5 = rbind(Resultado.m5,Resultado)
  }
}


colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "squat_gmv_controlx2km_m5.xlsx", rowNames = TRUE)

# Model 2km-----------------------------------------------
twokm <- read_excel("database_2.xlsx" , sheet = "twokm_variables")
twokm <- twokm %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Covariable4 = "handgrip_score_mean"
Covariable5 = "dxa_bmi"
Pos = c(8)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(27)   ### Outcome variable position (eigenvalues) 
Resultado.twokm = data.frame(NULL)  

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(twokm[, j]) ~  unlist(twokm[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4) + get(Covariable5), twokm) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(twokm)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(twokm[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.twokm = rbind(Resultado.twokm,Resultado)
  }
}

colnames(Resultado.twokm) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.twokm, Resultado.twokm$p.value < 0.05)
write.xlsx(Resultado.twokm, "2km_gmv_controlxhandgrip_twokm.xlsx", rowNames = TRUE)

coeficientes <- coef(Equation)

# Bonferroni correction for multiple comparison --------------------

resultados <- bind_rows(Resultado.m1 , Resultado.m5 , Resultado.m9 , Resultado.m11 , Resultado.twokm)
correccion <- p.adjust(resultados$p.value, method = "bonferroni")
print(correccion)


# Kolmogorov - smirnov - Normality test  ---------------------------

data <- read_excel("database_2.xlsx" , sheet = "variables")

data[, c(1,2,3, 12, 14:17,19:23, 26, 27)] <- NULL #seleccionar variables para aplicar el test
data <- data %>% mutate_all(as.numeric)

variables <- names(data)[sapply(data, is.numeric)]

results <- list()

# Aplicar el test KS, iterando en cada variable
for (variable in variables) {
  results[[variable]] <- ks.test(data[[variable]], "pnorm", mean(data[[variable]]), sd(data[[variable]]))
}


# Q-Q graphs - normality visual inspection ------------------------
data[, c(2:10, 12:31)] <- NULL
data <- data %>% mutate_all(as.numeric)
handgrip <- data$handgrip_score_mean
qqnorm(handgrip)
qqline(handgrip, col = "blue")
title("Gráfico Q-Q")


# Interaction between covariates in the regression model -----------------------------------------------------
# Model 1: Handgrip Strength -----------------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables")
m1$TBV <- as.numeric(m1$TBV)

Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(10)  ### Independent indicator position (muscular strength indicator)  
Pos2 = c(25:33)  ### Outcome variable position (eigenvalues)   
Resultado.m1 = data.frame(NULL)     

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~ unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + unlist(m1[, i]) * get(Covariable2), m1)
    #Equation = lm(unlist(m1[, j]) ~ unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + unlist(m1[, i]) * get(Covariable1), m1)
    #Equation = lm(unlist(m1[, j]) ~ unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + unlist(m1[, i]) * get(Covariable3), m1)
    
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2], tidy(Equation)[2,2:5], t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1, Resultado)
  }
}

summary(Equation)

# Model 5: Squats (coordinate deleted for not complying with the gray matter limit in the xjview inspection -521227)-----------------------------------------------
m5 <- read_excel("database_2.xlsx" , sheet = "m5_variables")
m5 <- m5 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(17) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(25:27)  ### Outcome variable position (eigenvalues)  
Resultado.m5 = data.frame(NULL)        

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m5[, j]) ~ unlist(m5[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + unlist(m5[, i]) * get(Covariable3), m5)
    Resultado = as.data.frame(cbind(names(m5)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m5[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m5 = rbind(Resultado.m5,Resultado)
  }
}

summary(Equation)

colnames(Resultado.m5) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m5, Resultado.m5$p.value < 0.05)
write.xlsx(Resultado.m5, "fitness_gmv_m5.xlsx", rowNames = TRUE)
summary(Equation)

# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables")
m9 <- m9 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age"
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(23)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(25:28) ### Outcome variable position (eigenvalues) 
Resultado.m9 = data.frame(NULL)       

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m9[, j]) ~ unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + unlist(m9[, i]) * get(Covariable3), m9)
    Resultado = as.data.frame(cbind(names(m9)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m9[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m9 = rbind(Resultado.m9,Resultado)
  }
}
summary(Equation)
colnames(Resultado.m9) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m9, Resultado.m9$p.value < 0.05)
write.xlsx(Resultado.m9, "fitness_gmv_ef_m9.xlsx", rowNames = TRUE)


# Model 11: Elbow extension  (Finally eliminated for not complying with the amount of gray matter in the xjview inspection). -----------------------------------------------
m11 <- read_excel("database_2.xlsx" , sheet = "m11_variables")
m11 <- m11 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(24)   ### Independent indicator position (muscular strength indicator)  
Pos2 = c(25)    ### Outcome variable position (eigenvalues) 
Resultado.m11 = data.frame(NULL)            

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m11[, j]) ~ unlist(m11[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + unlist(m11[, i]) * get(Covariable1), m11)
    Resultado = as.data.frame(cbind(names(m11)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m11[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m11 = rbind(Resultado.m11,Resultado)
  }
}

summary(Equation)

colnames(Resultado.m11) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m11, Resultado.m11$p.value < 0.05)
write.xlsx(Resultado.m11, "fitness_gmv_ef_m11.xlsx", rowNames = TRUE)

# Model 2km-----------------------------------------------
twokm <- read_excel("database_2.xlsx" , sheet = "twokm_variables")
twokm <- twokm %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(8)   ### Independent indicator position (muscular strength indicator)  
Pos2 = c(26:27)  ### Outcome variable position (eigenvalues) 
Resultado.twokm = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(twokm[, j]) ~ unlist(twokm[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + unlist(twokm[, i]) * get(Covariable3), twokm)
    Resultado = as.data.frame(cbind(names(twokm)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(twokm[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.twokm = rbind(Resultado.twokm,Resultado)
  }
}
summary(Equation)
colnames(Resultado.twokm) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.twokm, Resultado.twokm$p.value < 0.05)
write.xlsx(Resultado.twokm, "fitness_gmv_ef_twokm.xlsx", rowNames = TRUE)

coeficientes <- coef(Equation)


# Regression models segmenting the sample by age (cluster) -----------------------------------------------------
# Model 1: Handgrip Strength -----------------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables")
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age")) 

mediana_screen_age <- median(m1$screen_age)
m1$age_cat <- ifelse(m1$screen_age <= mediana_screen_age, 0, 1)

m1_younger <- subset(m1, age_cat == 0)
m1_older <- subset(m1, age_cat == 1)

Covariable1 = "screen_gender"
Covariable2 = "screen_years_edu"
Pos = c(10)  ### Independent indicator position (muscular strength indicator)  
Pos2 = c(25:33) ### Outcome variable position (eigenvalues) 

# Regression analysis for the younger sample of older adults 
Resultado.m1_younger = data.frame(NULL)       

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1_younger[, j]) ~ unlist(m1_younger[, i]) + get(Covariable1) + get(Covariable2), m1_younger)
    Resultado = as.data.frame(cbind(names(m1_younger)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2], tidy(Equation)[2,2:5], t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1_younger[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1_younger = rbind(Resultado.m1_younger, Resultado)
  }
}

summary(Equation)

# Regression analysis for the older sample of older adults 

Resultado.m1_older = data.frame(NULL)             

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1_older[, j]) ~ unlist(m1_older[, i]) + get(Covariable1) + get(Covariable2), m1_older)
    Resultado = as.data.frame(cbind(names(m1_older)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2], tidy(Equation)[2,2:5], t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1_older[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1_older = rbind(Resultado.m1_older, Resultado)
  }
}

summary(Equation)

# Model 5: Squats (coordinate deleted for not complying with the gray matter limit in the xjview inspection -521227)-----------------------------------------------
m5 <- read_excel("database_2.xlsx" , sheet = "m5_variables")
m5 <- m5 %>%
  mutate_if(is.character, as.numeric)

mediana_screen_age <- median(m5$screen_age)
m5$age_cat <- ifelse(m5$screen_age <= mediana_screen_age, 0, 1)

m5_younger <- subset(m5, age_cat == 0)
m5_older <- subset(m5, age_cat == 1)

# Regression analysis for the younger sample of older adults 
Covariable1 = "screen_gender" # Covariable
Covariable2 = "screen_years_edu" # Covariable
Pos = c(17)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(25:27)    ### Outcome variable position (eigenvalues) 
Resultado.m5_younger = data.frame(NULL)              # Crea el objeto que almacena los resultados

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m5_younger[, j]) ~ unlist(m5_younger[, i]) + get(Covariable1) + get(Covariable2) , m5_younger)
    Resultado = as.data.frame(cbind(names(m5_younger)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m5_younger[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m5_younger = rbind(Resultado.m5_younger,Resultado)
  }
}

summary(Equation)

colnames(Resultado.m5_younger) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m5_younger, Resultado.m5_younger$p.value < 0.05)
write.xlsx(Resultado.m5_younger, "fitness_gmv_m5_younger.xlsx", rowNames = TRUE)

# Regression analysis for the older sample of older adults 
Covariable1 = "screen_gender" # Covariable
Covariable2 = "screen_years_edu" # Covariable
Pos = c(17)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(25:27)    ### Outcome variable position (eigenvalues) 
Resultado.m5_older = data.frame(NULL)              # Crea el objeto que almacena los resultados

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m5_older[, j]) ~ unlist(m5_older[, i]) + get(Covariable1) + get(Covariable2) , m5_older)
    Resultado = as.data.frame(cbind(names(m5_older)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m5_older[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m5_older = rbind(Resultado.m5_older,Resultado)
  }
}

summary(Equation)

colnames(Resultado.m5_older) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m5_older, Resultado.m5_older$p.value < 0.05)
write.xlsx(Resultado.m5_older, "fitness_gmv_m5_older.xlsx", rowNames = TRUE)

# Model 11: Elbow extension (Finally eliminated for not complying with the amount of gray matter in the xjview inspection)-----------------------------------------------
m11 <- read_excel("database_2.xlsx" , sheet = "m11_variables")
m11 <- m11 %>%
  mutate_if(is.character, as.numeric)

m11_men <- subset(m11, screen_gender == 0)
m11_women <- subset(m11, screen_gender == 1)

Covariable1 = "screen_age"
Covariable2 = "screen_years_edu"
Pos = c(24) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(25) ### Outcome variable position (eigenvalues) 

# Regression analysis for the sample of older adult males 
Resultado.m11_men = data.frame(NULL)              # Crea el objeto que almacena los resultados

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m11_men[, j]) ~ unlist(m11_men[, i]) + get(Covariable1) + get(Covariable2) , m11_men)
    Resultado = as.data.frame(cbind(names(m11_men)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m11_men[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m11_men = rbind(Resultado.m11_men,Resultado)
  }
}

summary(Equation)

colnames(Resultado.m11_men) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m11_men, Resultado.m11_men$p.value < 0.05)
write.xlsx(Resultado.m11_men, "fitness_gmv_m11_men.xlsx", rowNames = TRUE)

# Regression analysis for the sample of women of older adults 
Resultado.m11_women = data.frame(NULL)              # Crea el objeto que almacena los resultados

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m11_women[, j]) ~ unlist(m11_women[, i]) + get(Covariable1) + get(Covariable2) , m11_women)
    Resultado = as.data.frame(cbind(names(m11_women)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m11_women[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m11_women = rbind(Resultado.m11_women,Resultado)
  }
}

summary(Equation)
colnames(Resultado.m11_women) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m11_women, Resultado.m11_women$p.value < 0.05)
write.xlsx(Resultado.m11_women, "fitness_gmv_m11_women.xlsx", rowNames = TRUE)

# Exploratory analyses ----------------------------------------------------
# Regression analysis:fitness associated regions + TMT (cluster) -----------------------------------------------------
# Model 1: Hangrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables_sin289(ef)")
m1 <- subset(m1, record_id != 289)
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age")) 
Covariable1 = "screen_age"
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(25:33) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(36) ### Outcome variable position (eigenvalues) 
Resultado.m1 = data.frame(NULL)           

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~  unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m1) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1,Resultado)
  }
}

colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "fitness_gmv_tmt_m1.xlsx", rowNames = TRUE)
# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables_sin289(ef)")
m9 <- subset(m9, record_id != 289)
m9 <- m9 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25:28)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(31)   ### Outcome variable position (eigenvalues)  
Resultado.m9 = data.frame(NULL) 

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m9[, j]) ~  unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m9) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m9)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m9[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m9 = rbind(Resultado.m9,Resultado)
  }
}

colnames(Resultado.m9) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m9, Resultado.m9$p.value < 0.05)
write.xlsx(Resultado.m9, "fitness_gmv_tmt_m9.xlsx", rowNames = TRUE)

# Regression analysis:fitness associated regions + DCCS (cluster)-----------------------------------------------------
# Model 1: Hangrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables_sin289(ef)")
m1 <- subset(m1, record_id != 289)
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age")) 
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(25:33)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(37) ### Outcome variable position (eigenvalues) 
Resultado.m1 = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~  unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m1) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1,Resultado)
  }
}

colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "fitness_gmv_dccs_m1.xlsx", rowNames = TRUE)

# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables_sin289(ef)")
m9 <- subset(m9, record_id != 289)
m9 <- m9 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25:28)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(32)  ### Outcome variable position (eigenvalues) 
Resultado.m9 = data.frame(NULL) 

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m9[, j]) ~  unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m9) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m9)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m9[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m9 = rbind(Resultado.m9,Resultado)
  }
}

colnames(Resultado.m9) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m9, Resultado.m9$p.value < 0.05)
write.xlsx(Resultado.m9, "fitness_gmv_dccs_m9.xlsx", rowNames = TRUE)

# Regression analysis:fitness associated regions + DSST (cluster)-----------------------------------------------------
# Model 1: Hangrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables_sin289(ef)")
m1 <- subset(m1, record_id != 289)
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age"))
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(25:33)  ### Independent indicator position (muscular strength indicator) 
Pos2 = c(38)  ### Outcome variable position (eigenvalues) 
Resultado.m1 = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~  unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m1) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1,Resultado)
  }
}

colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "fitness_gmv_dsst_m1.xlsx", rowNames = TRUE)

# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables_sin289(ef)")
m9 <- subset(m9, record_id != 289)
m9 <- m9 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age"
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25:28) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(33) ### Outcome variable position (eigenvalues) 
Resultado.m9 = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m9[, j]) ~  unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m9) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m9)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m9[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m9 = rbind(Resultado.m9,Resultado)
  }
}

colnames(Resultado.m9) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m9, Resultado.m9$p.value < 0.05)
write.xlsx(Resultado.m9, "fitness_gmv_dsst_m9.xlsx", rowNames = TRUE)

# Regression analysis:fitness associated regions + SWM (cluster) -----------------------------------------------------
# Model 1: Hangrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables_sin289(ef)")
m1 <- subset(m1, record_id != 289)
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age")) 
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(25:33) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(39) ### Outcome variable position (eigenvalues) 
Resultado.m1 = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~  unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m1) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1,Resultado)
  }
}


colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "fitness_gmv_swm_m1.xlsx", rowNames = TRUE)

# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables_sin289(ef)")
m9 <- subset(m9, record_id != 289)
m9 <- m9 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25:28) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(34) ### Outcome variable position (eigenvalues) 
Resultado.m9 = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m9[, j]) ~  unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m9) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m9)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m9[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m9 = rbind(Resultado.m9,Resultado)
  }
}

colnames(Resultado.m9) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m9, Resultado.m9$p.value < 0.05)
write.xlsx(Resultado.m9, "fitness_gmv_swm_m9.xlsx", rowNames = TRUE)

# Regression analysis:fitness associated regions + Executive Function indicators (cluster) (regions with negative beta)  -----------------------------------------------------
# Model 1: Hangrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables_sin289(ef)")
m1 <- subset(m1, record_id != 289)
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age")) 
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(30) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(35:39) ### Outcome variable position (eigenvalues) 
Resultado.m1 = data.frame(NULL) 

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~  unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m1) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1,Resultado)
  }
}

# Without covariates
for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~  unlist(m1[, i]), m1) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1,Resultado)
  }
}

colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "fitness_gmv_ef_m1.xlsx", rowNames = TRUE)


# Model 5: Squats (coordinate deleted for not complying with the gray matter limit in the xjview inspection -521227) -----------------------------------------------
m5 <- read_excel("database_2.xlsx" , sheet = "m5_variables")
m5 <- m5 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(30:34) ### Outcome variable position (eigenvalues) 
Resultado.m5 = data.frame(NULL) 

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m5[, j]) ~  unlist(m5[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m5) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m5)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m5[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m5 = rbind(Resultado.m5,Resultado)
  }
}

colnames(Resultado.m5) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m5, Resultado.m5$p.value < 0.05)
write.xlsx(Resultado.m5, "fitness_gmv_ef_m5.xlsx", rowNames = TRUE)


# Model 2km-----------------------------------------------
twokm <- read_excel("database_2.xlsx" , sheet = "twokm_variables")
twokm <- twokm %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(27) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(28:32) ### Outcome variable position (eigenvalues) 
Resultado.twokm = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(twokm[, j]) ~  unlist(twokm[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), twokm) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(twokm)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(twokm[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.twokm = rbind(Resultado.twokm,Resultado)
  }
}

colnames(Resultado.twokm) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.twokm, Resultado.twokm$p.value < 0.05)
write.xlsx(Resultado.twokm, "fitness_gmv_ef_twokm.xlsx", rowNames = TRUE)




# Betas calculaion: indicador fitness + gmv --------------------------------------------------------
# Model 1: Hangrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables_peak")
m1$TBV <- as.numeric(m1$TBV)
Covariable1 = "screen_age" # covariate
Covariable2 = "screen_gender" # covariate
Covariable3 = "screen_years_edu" # covariate
Covariable4 = "dxa_bmi"
Pos = c(10)    ### Independent indicator position (muscular strength indicator)
Pos2 = c(25:33)    ### Outcome variable position (eigenvalues)
Resultado.m1 = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~  unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4), m1) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1,Resultado)
  }
}

colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "ftness_gmv_m1.xlsx", rowNames = TRUE)

# Model 5: Squats (coordinate deleted for not complying with the gray matter limit in the xjview inspection -521227)----------------------------------------------
m5 <- read_excel("database_2.xlsx" , sheet = "m5_variables")
m5 <- m5 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" # Covariable
Covariable2 = "screen_gender" # Covariable
Covariable3 = "screen_years_edu"
Covariable4 = "dxa_bmi"

Pos = c(17)   ### Independent indicator position (muscular strength indicator)
Pos2 = c(25:27)  ### Outcome variable position (eigenvalues)
Resultado.m5 = data.frame(NULL)       

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m5[, j]) ~  unlist(m5[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4), m5) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m5)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m5[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m5 = rbind(Resultado.m5,Resultado)
  }
}

colnames(Resultado.m5) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m5, Resultado.m5$p.value < 0.05)
write.xlsx(Resultado.m5, "fitness_gmv_m5.xlsx", rowNames = TRUE)

# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables")
m9 <- m9 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Covariable4 = "dxa_bmi"

Pos = c(23) ### Independent indicator position (muscular strength indicator)
Pos2 = c(25:28)   ### Outcome variable position (eigenvalues)
Resultado.m9 = data.frame(NULL)         

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m9[, j]) ~  unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4), m9) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m9)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m9[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m9 = rbind(Resultado.m9,Resultado)
  }
}

colnames(Resultado.m9) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m9, Resultado.m9$p.value < 0.05)
write.xlsx(Resultado.m9, "fitness_gmv_m9.xlsx", rowNames = TRUE)

# Model 11: Elbow extension (Finally eliminated for not complying with the amount of gray matter in the xjview inspection). -----------------------------------------------
m11 <- read_excel("database_2.xlsx" , sheet = "m11_variables")
m11 <- m11 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" # Covariable
Covariable2 = "screen_gender" # Covariable
Covariable3 = "screen_years_edu"
Covariable4 = "dxa_bmi"

Pos = c(24)  ### Independent indicator position (muscular strength indicator)
Pos2 = c(25)   ### Outcome variable position (eigenvalues)
Resultado.m11 = data.frame(NULL)        

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m11[, j]) ~  unlist(m11[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4), m11) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m11)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m11[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m11 = rbind(Resultado.m11,Resultado)
  }
}

colnames(Resultado.m11) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m11, Resultado.m11$p.value < 0.05)
write.xlsx(Resultado.m11, "fitness_gmv_m11.xlsx", rowNames = TRUE)

# Model 2km-----------------------------------------------
twokm <- read_excel("database_2.xlsx" , sheet = "twokm_variables")
twokm <- twokm %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" # Covariable
Covariable2 = "screen_gender" # Covariable
Covariable3 = "screen_years_edu"
Covariable4 = "dxa_bmi"

Pos = c(8)   ### Independent indicator position (muscular strength indicator)
Pos2 = c(26:27) ### Outcome variable position (eigenvalues)
Resultado.twokm = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(twokm[, j]) ~  unlist(twokm[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4), twokm) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(twokm)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(twokm[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.twokm = rbind(Resultado.twokm,Resultado)
  }
}

colnames(Resultado.twokm) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.twokm, Resultado.twokm$p.value < 0.05)
write.xlsx(Resultado.twokm, "fitness_gmv_twokm.xlsx", rowNames = TRUE)


# Regression analysis:fitness associated regions + executive function (cluster) -----------------------------------------------------
# Model 1: Handgrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables_peakcoord")
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age"))
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25:33) ### Independent indicator position (muscular strength indicator)
Pos2 = c(35)     ### Outcome variable position (eigenvalues)
Resultado.m1 = data.frame(NULL)            

for (j in Pos2) {
  for (i in Pos) {
    # Corre el modelo
    Equation <- lm(unlist(m1[, j]) ~ unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m1)
    
    # Extrae el coeficiente beta estandarizado
    beta_value <- lm.beta(Equation)$standardized.coefficients[2]  # Cambia el índice si no coincide
    
    # Extrae los resultados de tidy() y confint()
    Resultado <- as.data.frame(cbind(
      names(m1)[j],                            # Nombre de la variable dependiente
      tidy(Equation)[2, "term"],               # Nombre de la variable independiente
      beta_value,                              # Coeficiente beta estandarizado
      tidy(Equation)[2, c("estimate", "std.error", "statistic", "p.value")],  # Otros resultados del modelo
      t(as.data.frame(confint(Equation)[2, ])) # Intervalo de confianza
    ))
    
    # Añade el término de la variable independiente y el tamaño de la muestra
    Resultado$term <- names(m1[i])
    Resultado$n <- nrow(Equation[["model"]])
    
    # Redondea los valores numéricos
    Resultado <- Resultado %>% mutate_if(is.numeric, round, digits = 3)
    
    # Agrega el resultado al objeto final
    Resultado.m1 <- rbind(Resultado.m1, Resultado)
  }
}


# Model 5: Squats (coordinate deleted for not complying with the gray matter limit in the xjview inspection -521227)-----------------------------------------------
m5 <- read_excel("database_2.xlsx" , sheet = "m5_variables_peakcoord")
m5 <- m5 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25:26)  ### Independent indicator position (muscular strength indicator)
Pos2 = c(28)   ### Outcome variable position (eigenvalues)
Resultado.m5 = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    # Corre el modelo
    Equation <- lm(unlist(m5[, j]) ~ unlist(m5[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m5)
    
    # Extrae el coeficiente beta estandarizado
    beta_value <- lm.beta(Equation)$standardized.coefficients[2]  # Cambia el índice si no coincide
    
    # Extrae los resultados de tidy() y confint()
    Resultado <- as.data.frame(cbind(
      names(m5)[j],                            # Nombre de la variable dependiente
      tidy(Equation)[2, "term"],               # Nombre de la variable independiente
      beta_value,                              # Coeficiente beta estandarizado
      tidy(Equation)[2, c("estimate", "std.error", "statistic", "p.value")],  # Otros resultados del modelo
      t(as.data.frame(confint(Equation)[2, ])) # Intervalo de confianza
    ))
    
    # Añade el término de la variable independiente y el tamaño de la muestra
    Resultado$term <- names(m5[i])
    Resultado$n <- nrow(Equation[["model"]])
    
    # Redondea los valores numéricos
    Resultado <- Resultado %>% mutate_if(is.numeric, round, digits = 3)
    
    # Agrega el resultado al objeto final
    Resultado.m5 <- rbind(Resultado.m5, Resultado)
  }
}

# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables_peakcoord")
m9 <- m9 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25:27)    ### Independent indicator position (muscular strength indicator)
Pos2 = c(29)    ### Outcome variable position (eigenvalues)
Resultado.m9 = data.frame(NULL)             

for (j in Pos2) {
  for (i in Pos) {
    # Corre el modelo
    Equation <- lm(unlist(m9[, j]) ~ unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m9)
    
    # Extrae el coeficiente beta estandarizado
    beta_value <- lm.beta(Equation)$standardized.coefficients[2]  # Cambia el índice si no coincide
    
    # Extrae los resultados de tidy() y confint()
    Resultado <- as.data.frame(cbind(
      names(m9)[j],                            # Nombre de la variable dependiente
      tidy(Equation)[2, "term"],               # Nombre de la variable independiente
      beta_value,                              # Coeficiente beta estandarizado
      tidy(Equation)[2, c("estimate", "std.error", "statistic", "p.value")],  # Otros resultados del modelo
      t(as.data.frame(confint(Equation)[2, ])) # Intervalo de confianza
    ))
    
    # Añade el término de la variable independiente y el tamaño de la muestra
    Resultado$term <- names(m9[i])
    Resultado$n <- nrow(Equation[["model"]])
    
    # Redondea los valores numéricos
    Resultado <- Resultado %>% mutate_if(is.numeric, round, digits = 3)
    
    # Agrega el resultado al objeto final
    Resultado.m9 <- rbind(Resultado.m9, Resultado)
  }
}

# Model 2km-----------------------------------------------
twokm <- read_excel("database_2.xlsx" , sheet = "twokm_variables_peakcoord")
twokm <- twokm %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(26)   ### Independent indicator position (muscular strength indicator)
Pos2 = c(27) ### Outcome variable position (eigenvalues)
Resultado.twokm = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    # Corre el modelo
    Equation <- lm(unlist(twokm[, j]) ~ unlist(twokm[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), twokm)
    
    # Extrae el coeficiente beta estandarizado
    beta_value <- lm.beta(Equation)$standardized.coefficients[2]  # Cambia el índice si no coincide
    
    # Extrae los resultados de tidy() y confint()
    Resultado <- as.data.frame(cbind(
      names(twokm)[j],                            # Nombre de la variable dependiente
      tidy(Equation)[2, "term"],               # Nombre de la variable independiente
      beta_value,                              # Coeficiente beta estandarizado
      tidy(Equation)[2, c("estimate", "std.error", "statistic", "p.value")],  # Otros resultados del modelo
      t(as.data.frame(confint(Equation)[2, ])) # Intervalo de confianza
    ))
    
    # Añade el término de la variable independiente y el tamaño de la muestra
    Resultado$term <- names(twokm[i])
    Resultado$n <- nrow(Equation[["model"]])
    
    # Redondea los valores numéricos
    Resultado <- Resultado %>% mutate_if(is.numeric, round, digits = 3)
    
    # Agrega el resultado al objeto final
    Resultado.twokm <- rbind(Resultado.twokm, Resultado)
  }
}


# Regression analysis:fitness associated regions + executive function (cluster)  -----------------------------------------------------
# Model 1: Hangrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables_sin289(ef)")

m1 <- subset(m1, record_id != 289)
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age")) 
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(25:33)    ### Independent indicator position (muscular strength indicator)
Pos2 = c(35)  ### Outcome variable position (eigenvalues)
Resultado.m1 = data.frame(NULL)            

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~  unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m1) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1,Resultado)
  }
}

colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "fitness_gmv_ef_m1.xlsx", rowNames = TRUE)

# Model 5: Squats (coordinate deleted for not complying with the gray matter limit in the xjview inspection -521227)-----------------------------------------------
m5 <- read_excel("database_2.xlsx" , sheet = "m5_variables_sin289(ef)")
m5 <- subset(m5, record_id != 289)
m5 <- m5 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(25:27)    ### Independent indicator position (muscular strength indicator)
Pos2 = c(29) ### Outcome variable position (eigenvalues)
Resultado.m5 = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m5[, j]) ~  unlist(m5[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m5) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m5)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m5[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m5 = rbind(Resultado.m5,Resultado)
  }
}

colnames(Resultado.m5) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m5, Resultado.m5$p.value < 0.05)
write.xlsx(Resultado.m5, "fitness_gmv_ef_m5.xlsx", rowNames = TRUE)

# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables_sin289(ef)")
m9 <- subset(m9, record_id != 289)
m9 <- m9 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25:28) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(30)  ### Outcome variable position (eigenvalues)
Resultado.m9 = data.frame(NULL)  

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m9[, j]) ~  unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m9) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m9)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m9[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m9 = rbind(Resultado.m9,Resultado)
  }
}

colnames(Resultado.m9) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m9, Resultado.m9$p.value < 0.05)
write.xlsx(Resultado.m9, "fitness_gmv_ef_m9.xlsx", rowNames = TRUE)

# Model 11: Elbow extension  (Finally eliminated for not complying with the amount of gray matter in the xjview inspection).-----------------------------------------------
m11 <- read_excel("database_2.xlsx" , sheet = "m11_variables_sin289(ef)")
m11 <- subset(m11, record_id != 289)
m11 <- m11 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(25)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(27)  ### Outcome variable position (eigenvalues) 
Resultado.m11 = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m11[, j]) ~  unlist(m11[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m11) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m11)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m11[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m11 = rbind(Resultado.m11,Resultado)
  }
}

colnames(Resultado.m11) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m11, Resultado.m11$p.value < 0.05)
write.xlsx(Resultado.m11, "fitness_gmv_ef_m11.xlsx", rowNames = TRUE)

# Model 2km-----------------------------------------------
twokm <- read_excel("database_2.xlsx" , sheet = "twokm_variables_sin289(ef)")
twokm <- subset(twokm, record_id != 289)
twokm <- twokm %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age"
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(26:27) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(28)  ### Outcome variable position (eigenvalues) 
Resultado.twokm = data.frame(NULL)  

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(twokm[, j]) ~  unlist(twokm[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), twokm) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(twokm)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(twokm[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.twokm = rbind(Resultado.twokm,Resultado)
  }
}

colnames(Resultado.twokm) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.twokm, Resultado.twokm$p.value < 0.05)
write.xlsx(Resultado.twokm, "fitness_gmv_ef_twokm.xlsx", rowNames = TRUE)


# Regression analysis: adjusting by handgrip in 2km - walking test and viceversa (cluster) -----------------------------------------------------
# Model 1: Hangrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables")
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age")) 
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Covariable4 = "pf_2km_walk_time"
Covariable5 = "dxa_bmi"
Pos = c(10)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(25:33)  ### Outcome variable position (eigenvalues) 
Resultado.m1 = data.frame(NULL) 

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~  unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4) + get(Covariable5), m1) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1,Resultado)
  }
}

colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "handgrip_gmv_controlx2km_m1.xlsx", rowNames = TRUE)

# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables")
m9 <- ungroup(m9)
m9$TBV <- as.numeric(m9$TBV)
m9$screen_gender <- as.numeric(m9$screen_gender)
m9$screen_age <- as.numeric(m9$screen_age)
m9$screen_years_edu <- as.numeric(m9$screen_years_edu)
m9$pf_2km_walk_time <- as.numeric(m9$pf_2km_walk_time)
m9$dxa_bmi <- as.numeric(m9$dxa_bmi)
m9$Peak_iso_Low <- as.numeric(m9$Peak_iso_Low)
m9$Path <- NULL
which(colnames(m9)==c("screen_age"))
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Covariable4 = "pf_2km_walk_time"
Covariable5 = "dxa_bmi"
Pos = c(22)  ### Independent indicator position (muscular strength indicator) 
Pos2 = c(24:26)   ### Outcome variable position (eigenvalues) 
Resultado.m9 = data.frame(NULL) 

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m9[, j]) ~  unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4) + get(Covariable5), m9) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m9)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m9[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m9 = rbind(Resultado.m9,Resultado)
  }
}

colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "kneeextension_gmv_controlx2km_m1.xlsx", rowNames = TRUE)

# Model 5: Squats-----------------------------------------------
m5 <- read_excel("database_2.xlsx" , sheet = "m5_variables")
m5 <- ungroup(m5)
m5$TBV <- as.numeric(m5$TBV)
m5$screen_gender <- as.numeric(m5$screen_gender)
m5$screen_age <- as.numeric(m5$screen_age)
m5$screen_years_edu <- as.numeric(m5$screen_years_edu)
m5$pf_2km_walk_time <- as.numeric(m5$pf_2km_walk_time)
m5$dxa_bmi <- as.numeric(m5$dxa_bmi)
m5$pf_sft_chair_score<- as.numeric(m5$pf_sft_chair_score)
m5$Path <- NULL
which(colnames(m5)==c("screen_age"))
Covariable1 = "screen_age"
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Covariable4 = "pf_2km_walk_time"
Covariable5 = "dxa_bmi"
Pos = c(16)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(24:26) ### Outcome variable position (eigenvalues) 
Resultado.m5 = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m5[, j]) ~  unlist(m5[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4) + get(Covariable5), m5) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m5)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m5[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m5 = rbind(Resultado.m5,Resultado)
  }
}


colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "squat_gmv_controlx2km_m5.xlsx", rowNames = TRUE)

# Model 2km-----------------------------------------------
twokm <- read_excel("database_2.xlsx" , sheet = "twokm_variables")
twokm <- twokm %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Covariable4 = "handgrip_score_mean"
Covariable5 = "dxa_bmi"
Pos = c(8)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(27)   ### Outcome variable position (eigenvalues) 
Resultado.twokm = data.frame(NULL)  

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(twokm[, j]) ~  unlist(twokm[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + get(Covariable4) + get(Covariable5), twokm) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(twokm)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(twokm[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.twokm = rbind(Resultado.twokm,Resultado)
  }
}

colnames(Resultado.twokm) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.twokm, Resultado.twokm$p.value < 0.05)
write.xlsx(Resultado.twokm, "2km_gmv_controlxhandgrip_twokm.xlsx", rowNames = TRUE)

coeficientes <- coef(Equation)

# Bonferroni correction for multiple comparison --------------------

resultados <- bind_rows(Resultado.m1 , Resultado.m5 , Resultado.m9 , Resultado.m11 , Resultado.twokm)
correccion <- p.adjust(resultados$p.value, method = "bonferroni")
print(correccion)


# Kolmogorov - smirnov - Normality test  ---------------------------

data <- read_excel("database_2.xlsx" , sheet = "variables")

data[, c(1,2,3, 12, 14:17,19:23, 26, 27)] <- NULL #seleccionar variables para aplicar el test
data <- data %>% mutate_all(as.numeric)

variables <- names(data)[sapply(data, is.numeric)]

results <- list()

# Aplicar el test KS, iterando en cada variable
for (variable in variables) {
  results[[variable]] <- ks.test(data[[variable]], "pnorm", mean(data[[variable]]), sd(data[[variable]]))
}


# Q-Q graphs - normality visual inspection ------------------------
data[, c(2:10, 12:31)] <- NULL
data <- data %>% mutate_all(as.numeric)
handgrip <- data$handgrip_score_mean
qqnorm(handgrip)
qqline(handgrip, col = "blue")
title("Gráfico Q-Q")


# Interaction between covariates in the regression model -----------------------------------------------------
# Model 1: Handgrip Strength -----------------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables")
m1$TBV <- as.numeric(m1$TBV)

Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(10)  ### Independent indicator position (muscular strength indicator)  
Pos2 = c(25:33)  ### Outcome variable position (eigenvalues)   
Resultado.m1 = data.frame(NULL)     

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~ unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + unlist(m1[, i]) * get(Covariable2), m1)
    #Equation = lm(unlist(m1[, j]) ~ unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + unlist(m1[, i]) * get(Covariable1), m1)
    #Equation = lm(unlist(m1[, j]) ~ unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + unlist(m1[, i]) * get(Covariable3), m1)
    
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2], tidy(Equation)[2,2:5], t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1, Resultado)
  }
}

summary(Equation)

# Model 5: Squats (coordinate deleted for not complying with the gray matter limit in the xjview inspection -521227)-----------------------------------------------
m5 <- read_excel("database_2.xlsx" , sheet = "m5_variables")
m5 <- m5 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(17) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(25:27)  ### Outcome variable position (eigenvalues)  
Resultado.m5 = data.frame(NULL)        

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m5[, j]) ~ unlist(m5[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + unlist(m5[, i]) * get(Covariable3), m5)
    Resultado = as.data.frame(cbind(names(m5)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m5[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m5 = rbind(Resultado.m5,Resultado)
  }
}

summary(Equation)

colnames(Resultado.m5) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m5, Resultado.m5$p.value < 0.05)
write.xlsx(Resultado.m5, "fitness_gmv_m5.xlsx", rowNames = TRUE)
summary(Equation)

# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables")
m9 <- m9 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age"
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(23)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(25:28) ### Outcome variable position (eigenvalues) 
Resultado.m9 = data.frame(NULL)       

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m9[, j]) ~ unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + unlist(m9[, i]) * get(Covariable3), m9)
    Resultado = as.data.frame(cbind(names(m9)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m9[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m9 = rbind(Resultado.m9,Resultado)
  }
}
summary(Equation)
colnames(Resultado.m9) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m9, Resultado.m9$p.value < 0.05)
write.xlsx(Resultado.m9, "fitness_gmv_ef_m9.xlsx", rowNames = TRUE)


# Model 11: Elbow extension  (Finally eliminated for not complying with the amount of gray matter in the xjview inspection). -----------------------------------------------
m11 <- read_excel("database_2.xlsx" , sheet = "m11_variables")
m11 <- m11 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(24)   ### Independent indicator position (muscular strength indicator)  
Pos2 = c(25)    ### Outcome variable position (eigenvalues) 
Resultado.m11 = data.frame(NULL)            

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m11[, j]) ~ unlist(m11[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + unlist(m11[, i]) * get(Covariable1), m11)
    Resultado = as.data.frame(cbind(names(m11)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m11[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m11 = rbind(Resultado.m11,Resultado)
  }
}

summary(Equation)

colnames(Resultado.m11) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m11, Resultado.m11$p.value < 0.05)
write.xlsx(Resultado.m11, "fitness_gmv_ef_m11.xlsx", rowNames = TRUE)

# Model 2km-----------------------------------------------
twokm <- read_excel("database_2.xlsx" , sheet = "twokm_variables")
twokm <- twokm %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(8)   ### Independent indicator position (muscular strength indicator)  
Pos2 = c(26:27)  ### Outcome variable position (eigenvalues) 
Resultado.twokm = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(twokm[, j]) ~ unlist(twokm[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3) + unlist(twokm[, i]) * get(Covariable3), twokm)
    Resultado = as.data.frame(cbind(names(twokm)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(twokm[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.twokm = rbind(Resultado.twokm,Resultado)
  }
}
summary(Equation)
colnames(Resultado.twokm) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.twokm, Resultado.twokm$p.value < 0.05)
write.xlsx(Resultado.twokm, "fitness_gmv_ef_twokm.xlsx", rowNames = TRUE)

coeficientes <- coef(Equation)


# Regression models segmenting the sample by age (cluster) -----------------------------------------------------
# Model 1: Handgrip Strength -----------------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables")
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age")) 

mediana_screen_age <- median(m1$screen_age)
m1$age_cat <- ifelse(m1$screen_age <= mediana_screen_age, 0, 1)

m1_younger <- subset(m1, age_cat == 0)
m1_older <- subset(m1, age_cat == 1)

Covariable1 = "screen_gender"
Covariable2 = "screen_years_edu"
Pos = c(10)  ### Independent indicator position (muscular strength indicator)  
Pos2 = c(25:33) ### Outcome variable position (eigenvalues) 

# Regression analysis for the younger sample of older adults 
Resultado.m1_younger = data.frame(NULL)       

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1_younger[, j]) ~ unlist(m1_younger[, i]) + get(Covariable1) + get(Covariable2), m1_younger)
    Resultado = as.data.frame(cbind(names(m1_younger)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2], tidy(Equation)[2,2:5], t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1_younger[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1_younger = rbind(Resultado.m1_younger, Resultado)
  }
}

summary(Equation)

# Regression analysis for the older sample of older adults 

Resultado.m1_older = data.frame(NULL)             

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1_older[, j]) ~ unlist(m1_older[, i]) + get(Covariable1) + get(Covariable2), m1_older)
    Resultado = as.data.frame(cbind(names(m1_older)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2], tidy(Equation)[2,2:5], t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1_older[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1_older = rbind(Resultado.m1_older, Resultado)
  }
}

summary(Equation)

# Model 5: Squats (coordinate deleted for not complying with the gray matter limit in the xjview inspection -521227)-----------------------------------------------
m5 <- read_excel("database_2.xlsx" , sheet = "m5_variables")
m5 <- m5 %>%
  mutate_if(is.character, as.numeric)

mediana_screen_age <- median(m5$screen_age)
m5$age_cat <- ifelse(m5$screen_age <= mediana_screen_age, 0, 1)

m5_younger <- subset(m5, age_cat == 0)
m5_older <- subset(m5, age_cat == 1)

# Regression analysis for the younger sample of older adults 
Covariable1 = "screen_gender" # Covariable
Covariable2 = "screen_years_edu" # Covariable
Pos = c(17)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(25:27)    ### Outcome variable position (eigenvalues) 
Resultado.m5_younger = data.frame(NULL)              # Crea el objeto que almacena los resultados

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m5_younger[, j]) ~ unlist(m5_younger[, i]) + get(Covariable1) + get(Covariable2) , m5_younger)
    Resultado = as.data.frame(cbind(names(m5_younger)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m5_younger[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m5_younger = rbind(Resultado.m5_younger,Resultado)
  }
}

summary(Equation)

colnames(Resultado.m5_younger) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m5_younger, Resultado.m5_younger$p.value < 0.05)
write.xlsx(Resultado.m5_younger, "fitness_gmv_m5_younger.xlsx", rowNames = TRUE)

# Regression analysis for the older sample of older adults 
Covariable1 = "screen_gender" # Covariable
Covariable2 = "screen_years_edu" # Covariable
Pos = c(17)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(25:27)    ### Outcome variable position (eigenvalues) 
Resultado.m5_older = data.frame(NULL)              # Crea el objeto que almacena los resultados

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m5_older[, j]) ~ unlist(m5_older[, i]) + get(Covariable1) + get(Covariable2) , m5_older)
    Resultado = as.data.frame(cbind(names(m5_older)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m5_older[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m5_older = rbind(Resultado.m5_older,Resultado)
  }
}

summary(Equation)

colnames(Resultado.m5_older) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m5_older, Resultado.m5_older$p.value < 0.05)
write.xlsx(Resultado.m5_older, "fitness_gmv_m5_older.xlsx", rowNames = TRUE)

# Model 11: Elbow extension (Finally eliminated for not complying with the amount of gray matter in the xjview inspection)-----------------------------------------------
m11 <- read_excel("database_2.xlsx" , sheet = "m11_variables")
m11 <- m11 %>%
  mutate_if(is.character, as.numeric)

m11_men <- subset(m11, screen_gender == 0)
m11_women <- subset(m11, screen_gender == 1)

Covariable1 = "screen_age"
Covariable2 = "screen_years_edu"
Pos = c(24) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(25) ### Outcome variable position (eigenvalues) 

# Regression analysis for the sample of older adult males 
Resultado.m11_men = data.frame(NULL)              # Crea el objeto que almacena los resultados

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m11_men[, j]) ~ unlist(m11_men[, i]) + get(Covariable1) + get(Covariable2) , m11_men)
    Resultado = as.data.frame(cbind(names(m11_men)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m11_men[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m11_men = rbind(Resultado.m11_men,Resultado)
  }
}

summary(Equation)

colnames(Resultado.m11_men) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m11_men, Resultado.m11_men$p.value < 0.05)
write.xlsx(Resultado.m11_men, "fitness_gmv_m11_men.xlsx", rowNames = TRUE)

# Regression analysis for the sample of women of older adults 
Resultado.m11_women = data.frame(NULL)              # Crea el objeto que almacena los resultados

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m11_women[, j]) ~ unlist(m11_women[, i]) + get(Covariable1) + get(Covariable2) , m11_women)
    Resultado = as.data.frame(cbind(names(m11_women)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m11_women[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m11_women = rbind(Resultado.m11_women,Resultado)
  }
}

summary(Equation)
colnames(Resultado.m11_women) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m11_women, Resultado.m11_women$p.value < 0.05)
write.xlsx(Resultado.m11_women, "fitness_gmv_m11_women.xlsx", rowNames = TRUE)

# Exploratory analyses ----------------------------------------------------
# Regression analysis:fitness associated regions + TMT (cluster) -----------------------------------------------------
# Model 1: Hangrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables_peakcoord")
m1 <- subset(m1, record_id != 289)
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age")) 
Covariable1 = "screen_age"
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(25:33) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(36) ### Outcome variable position (eigenvalues) 
Resultado.m1 = data.frame(NULL)           

for (j in Pos2) {
  for (i in Pos) {
    # Corre el modelo
    Equation <- lm(unlist(m1[, j]) ~ unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m1)
    
    # Extrae el coeficiente beta estandarizado
    beta_value <- lm.beta(Equation)$standardized.coefficients[2]  # Cambia el índice si no coincide
    
    # Extrae los resultados de tidy() y confint()
    Resultado <- as.data.frame(cbind(
      names(m1)[j],                            # Nombre de la variable dependiente
      tidy(Equation)[2, "term"],               # Nombre de la variable independiente
      beta_value,                              # Coeficiente beta estandarizado
      tidy(Equation)[2, c("estimate", "std.error", "statistic", "p.value")],  # Otros resultados del modelo
      t(as.data.frame(confint(Equation)[2, ])) # Intervalo de confianza
    ))
    
    # Añade el término de la variable independiente y el tamaño de la muestra
    Resultado$term <- names(m1[i])
    Resultado$n <- nrow(Equation[["model"]])
    
    # Redondea los valores numéricos
    Resultado <- Resultado %>% mutate_if(is.numeric, round, digits = 3)
    
    # Agrega el resultado al objeto final
    Resultado.m1 <- rbind(Resultado.m1, Resultado)
  }
}



for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~  unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m1) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1,Resultado)
  }
}

colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "fitness_gmv_tmt_m1.xlsx", rowNames = TRUE)
# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables_peakcoord")
m9 <- subset(m9, record_id != 289)
m9 <- m9 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25:27)    ### Independent indicator position (muscular strength indicator)
Pos2 = c(30)    ### Outcome variable position (eigenvalues)
Resultado.m9 = data.frame(NULL)             

for (j in Pos2) {
  for (i in Pos) {
    # Corre el modelo
    Equation <- lm(unlist(m9[, j]) ~ unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m9)
    
    # Extrae el coeficiente beta estandarizado
    beta_value <- lm.beta(Equation)$standardized.coefficients[2]  # Cambia el índice si no coincide
    
    # Extrae los resultados de tidy() y confint()
    Resultado <- as.data.frame(cbind(
      names(m9)[j],                            # Nombre de la variable dependiente
      tidy(Equation)[2, "term"],               # Nombre de la variable independiente
      beta_value,                              # Coeficiente beta estandarizado
      tidy(Equation)[2, c("estimate", "std.error", "statistic", "p.value")],  # Otros resultados del modelo
      t(as.data.frame(confint(Equation)[2, ])) # Intervalo de confianza
    ))
    
    # Añade el término de la variable independiente y el tamaño de la muestra
    Resultado$term <- names(m9[i])
    Resultado$n <- nrow(Equation[["model"]])
    
    # Redondea los valores numéricos
    Resultado <- Resultado %>% mutate_if(is.numeric, round, digits = 3)
    
    # Agrega el resultado al objeto final
    Resultado.m9 <- rbind(Resultado.m9, Resultado)
  }
}


# Regression analysis:fitness associated regions + DCCS (cluster)-----------------------------------------------------
# Model 1: Hangrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables_peakcoord")
m1 <- subset(m1, record_id != 289)
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age")) 
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(25:33) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(37) ### Outcome variable position (eigenvalues) 
Resultado.m1 = data.frame(NULL)           

for (j in Pos2) {
  for (i in Pos) {
    # Corre el modelo
    Equation <- lm(unlist(m1[, j]) ~ unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m1)
    
    # Extrae el coeficiente beta estandarizado
    beta_value <- lm.beta(Equation)$standardized.coefficients[2]  # Cambia el índice si no coincide
    
    # Extrae los resultados de tidy() y confint()
    Resultado <- as.data.frame(cbind(
      names(m1)[j],                            # Nombre de la variable dependiente
      tidy(Equation)[2, "term"],               # Nombre de la variable independiente
      beta_value,                              # Coeficiente beta estandarizado
      tidy(Equation)[2, c("estimate", "std.error", "statistic", "p.value")],  # Otros resultados del modelo
      t(as.data.frame(confint(Equation)[2, ])) # Intervalo de confianza
    ))
    
    # Añade el término de la variable independiente y el tamaño de la muestra
    Resultado$term <- names(m1[i])
    Resultado$n <- nrow(Equation[["model"]])
    
    # Redondea los valores numéricos
    Resultado <- Resultado %>% mutate_if(is.numeric, round, digits = 3)
    
    # Agrega el resultado al objeto final
    Resultado.m1 <- rbind(Resultado.m1, Resultado)
  }
}


# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables_peakcoord")
m9 <- subset(m9, record_id != 289)
m9 <- m9 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25:27)    ### Independent indicator position (muscular strength indicator)
Pos2 = c(31)    ### Outcome variable position (eigenvalues)
Resultado.m9 = data.frame(NULL)             

for (j in Pos2) {
  for (i in Pos) {
    # Corre el modelo
    Equation <- lm(unlist(m9[, j]) ~ unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m9)
    
    # Extrae el coeficiente beta estandarizado
    beta_value <- lm.beta(Equation)$standardized.coefficients[2]  # Cambia el índice si no coincide
    
    # Extrae los resultados de tidy() y confint()
    Resultado <- as.data.frame(cbind(
      names(m9)[j],                            # Nombre de la variable dependiente
      tidy(Equation)[2, "term"],               # Nombre de la variable independiente
      beta_value,                              # Coeficiente beta estandarizado
      tidy(Equation)[2, c("estimate", "std.error", "statistic", "p.value")],  # Otros resultados del modelo
      t(as.data.frame(confint(Equation)[2, ])) # Intervalo de confianza
    ))
    
    # Añade el término de la variable independiente y el tamaño de la muestra
    Resultado$term <- names(m9[i])
    Resultado$n <- nrow(Equation[["model"]])
    
    # Redondea los valores numéricos
    Resultado <- Resultado %>% mutate_if(is.numeric, round, digits = 3)
    
    # Agrega el resultado al objeto final
    Resultado.m9 <- rbind(Resultado.m9, Resultado)
  }
}


# Regression analysis:fitness associated regions + DSST (cluster)-----------------------------------------------------
# Model 1: Hangrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables_peakcoord")
m1 <- subset(m1, record_id != 289)
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age"))
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(25:33) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(38) ### Outcome variable position (eigenvalues) 
Resultado.m1 = data.frame(NULL)           

for (j in Pos2) {
  for (i in Pos) {
    # Corre el modelo
    Equation <- lm(unlist(m1[, j]) ~ unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m1)
    
    # Extrae el coeficiente beta estandarizado
    beta_value <- lm.beta(Equation)$standardized.coefficients[2]  # Cambia el índice si no coincide
    
    # Extrae los resultados de tidy() y confint()
    Resultado <- as.data.frame(cbind(
      names(m1)[j],                            # Nombre de la variable dependiente
      tidy(Equation)[2, "term"],               # Nombre de la variable independiente
      beta_value,                              # Coeficiente beta estandarizado
      tidy(Equation)[2, c("estimate", "std.error", "statistic", "p.value")],  # Otros resultados del modelo
      t(as.data.frame(confint(Equation)[2, ])) # Intervalo de confianza
    ))
    
    # Añade el término de la variable independiente y el tamaño de la muestra
    Resultado$term <- names(m1[i])
    Resultado$n <- nrow(Equation[["model"]])
    
    # Redondea los valores numéricos
    Resultado <- Resultado %>% mutate_if(is.numeric, round, digits = 3)
    
    # Agrega el resultado al objeto final
    Resultado.m1 <- rbind(Resultado.m1, Resultado)
  }
}

# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables_peakcoord")
m9 <- subset(m9, record_id != 289)
m9 <- m9 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age"
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25:27)    ### Independent indicator position (muscular strength indicator)
Pos2 = c(32)    ### Outcome variable position (eigenvalues)
Resultado.m9 = data.frame(NULL)             

for (j in Pos2) {
  for (i in Pos) {
    # Corre el modelo
    Equation <- lm(unlist(m9[, j]) ~ unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m9)
    
    # Extrae el coeficiente beta estandarizado
    beta_value <- lm.beta(Equation)$standardized.coefficients[2]  # Cambia el índice si no coincide
    
    # Extrae los resultados de tidy() y confint()
    Resultado <- as.data.frame(cbind(
      names(m9)[j],                            # Nombre de la variable dependiente
      tidy(Equation)[2, "term"],               # Nombre de la variable independiente
      beta_value,                              # Coeficiente beta estandarizado
      tidy(Equation)[2, c("estimate", "std.error", "statistic", "p.value")],  # Otros resultados del modelo
      t(as.data.frame(confint(Equation)[2, ])) # Intervalo de confianza
    ))
    
    # Añade el término de la variable independiente y el tamaño de la muestra
    Resultado$term <- names(m9[i])
    Resultado$n <- nrow(Equation[["model"]])
    
    # Redondea los valores numéricos
    Resultado <- Resultado %>% mutate_if(is.numeric, round, digits = 3)
    
    # Agrega el resultado al objeto final
    Resultado.m9 <- rbind(Resultado.m9, Resultado)
  }
}

# Regression analysis:fitness associated regions + SWM (cluster) -----------------------------------------------------
# Model 1: Hangrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables_peakcoord")
m1 <- subset(m1, record_id != 289)
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age"))
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(25:33) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(39) ### Outcome variable position (eigenvalues) 
Resultado.m1 = data.frame(NULL)           

for (j in Pos2) {
  for (i in Pos) {
    # Corre el modelo
    Equation <- lm(unlist(m1[, j]) ~ unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m1)
    
    # Extrae el coeficiente beta estandarizado
    beta_value <- lm.beta(Equation)$standardized.coefficients[2]  # Cambia el índice si no coincide
    
    # Extrae los resultados de tidy() y confint()
    Resultado <- as.data.frame(cbind(
      names(m1)[j],                            # Nombre de la variable dependiente
      tidy(Equation)[2, "term"],               # Nombre de la variable independiente
      beta_value,                              # Coeficiente beta estandarizado
      tidy(Equation)[2, c("estimate", "std.error", "statistic", "p.value")],  # Otros resultados del modelo
      t(as.data.frame(confint(Equation)[2, ])) # Intervalo de confianza
    ))
    
    # Añade el término de la variable independiente y el tamaño de la muestra
    Resultado$term <- names(m1[i])
    Resultado$n <- nrow(Equation[["model"]])
    
    # Redondea los valores numéricos
    Resultado <- Resultado %>% mutate_if(is.numeric, round, digits = 3)
    
    # Agrega el resultado al objeto final
    Resultado.m1 <- rbind(Resultado.m1, Resultado)
  }
}

# Model 9: Knee extension-----------------------------------------------
m9 <- read_excel("database_2.xlsx" , sheet = "m9_variables_peakcoord")
m9 <- subset(m9, record_id != 289)
m9 <- m9 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age"
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25:27)    ### Independent indicator position (muscular strength indicator)
Pos2 = c(33)    ### Outcome variable position (eigenvalues)
Resultado.m9 = data.frame(NULL)             

for (j in Pos2) {
  for (i in Pos) {
    # Corre el modelo
    Equation <- lm(unlist(m9[, j]) ~ unlist(m9[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m9)
    
    # Extrae el coeficiente beta estandarizado
    beta_value <- lm.beta(Equation)$standardized.coefficients[2]  # Cambia el índice si no coincide
    
    # Extrae los resultados de tidy() y confint()
    Resultado <- as.data.frame(cbind(
      names(m9)[j],                            # Nombre de la variable dependiente
      tidy(Equation)[2, "term"],               # Nombre de la variable independiente
      beta_value,                              # Coeficiente beta estandarizado
      tidy(Equation)[2, c("estimate", "std.error", "statistic", "p.value")],  # Otros resultados del modelo
      t(as.data.frame(confint(Equation)[2, ])) # Intervalo de confianza
    ))
    
    # Añade el término de la variable independiente y el tamaño de la muestra
    Resultado$term <- names(m9[i])
    Resultado$n <- nrow(Equation[["model"]])
    
    # Redondea los valores numéricos
    Resultado <- Resultado %>% mutate_if(is.numeric, round, digits = 3)
    
    # Agrega el resultado al objeto final
    Resultado.m9 <- rbind(Resultado.m9, Resultado)
  }
}

# Regression analysis:fitness associated regions + Executive Function indicators (cluster) (regions with negative beta)  -----------------------------------------------------
# Model 1: Hangrip Strength -----------------------------------------------
m1 <- read_excel("database_2.xlsx" , sheet = "m1_variables_sin289(ef)")
m1 <- subset(m1, record_id != 289)
m1$TBV <- as.numeric(m1$TBV)
which(colnames(m1)==c("screen_age")) 
Covariable1 = "screen_age" 
Covariable2 = "screen_gender"
Covariable3 = "screen_years_edu"
Pos = c(30) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(35:39) ### Outcome variable position (eigenvalues) 
Resultado.m1 = data.frame(NULL) 

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~  unlist(m1[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m1) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1,Resultado)
  }
}

# Without covariates
for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m1[, j]) ~  unlist(m1[, i]), m1) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m1)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m1[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m1 = rbind(Resultado.m1,Resultado)
  }
}

colnames(Resultado.m1) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m1, Resultado.m1$p.value < 0.05)
write.xlsx(Resultado.m1, "fitness_gmv_ef_m1.xlsx", rowNames = TRUE)


# Model 5: Squats (coordinate deleted for not complying with the gray matter limit in the xjview inspection -521227) -----------------------------------------------
m5 <- read_excel("database_2.xlsx" , sheet = "m5_variables")
m5 <- m5 %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(25)   ### Independent indicator position (muscular strength indicator) 
Pos2 = c(30:34) ### Outcome variable position (eigenvalues) 
Resultado.m5 = data.frame(NULL) 

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(m5[, j]) ~  unlist(m5[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), m5) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(m5)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(m5[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.m5 = rbind(Resultado.m5,Resultado)
  }
}

colnames(Resultado.m5) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.m5, Resultado.m5$p.value < 0.05)
write.xlsx(Resultado.m5, "fitness_gmv_ef_m5.xlsx", rowNames = TRUE)


# Model 2km-----------------------------------------------
twokm <- read_excel("database_2.xlsx" , sheet = "twokm_variables")
twokm <- twokm %>%
  mutate_if(is.character, as.numeric)
Covariable1 = "screen_age" 
Covariable2 = "screen_gender" 
Covariable3 = "screen_years_edu"
Pos = c(27) ### Independent indicator position (muscular strength indicator) 
Pos2 = c(28:32) ### Outcome variable position (eigenvalues) 
Resultado.twokm = data.frame(NULL)

for (j in Pos2) {
  for (i in Pos) {
    Equation = lm(unlist(twokm[, j]) ~  unlist(twokm[, i]) + get(Covariable1) + get(Covariable2) + get(Covariable3), twokm) #Agregar o quitar covariables según corresponda
    Resultado = as.data.frame(cbind(names(twokm)[j], tidy(Equation)[2,1], lm.beta(Equation)[[13]][2],tidy(Equation)[2,2:5],t(as.data.frame(confint(Equation)[2,]))))
    Resultado$term = names(twokm[i])
    Resultado$n = nrow(Equation[["model"]])
    Resultado = Resultado %>% mutate_if(is.numeric, round, digits = 3)
    Resultado.twokm = rbind(Resultado.twokm,Resultado)
  }
}

colnames(Resultado.twokm) = c("Outcome", "Independent_V", "Std_estimate", "Estimate", "Std.error", "t.Value", "p.value", "L.CI", "U.CI", "n")
sig = subset(Resultado.twokm, Resultado.twokm$p.value < 0.05)
write.xlsx(Resultado.twokm, "fitness_gmv_ef_twokm.xlsx", rowNames = TRUE)



