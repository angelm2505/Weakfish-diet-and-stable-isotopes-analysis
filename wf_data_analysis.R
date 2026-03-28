
#https://derekogle.com/fishR/2021-05-25-fitPlot-replacement

#setwd("C:/Users/reyes/OneDrive/Documents/Grad school (maryland)/Research/Data")
setwd("~/Grad school (maryland)/Research/Data")

library(mgcv)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(vegan)
library(ggplot2)
library(FSA)
library(car) 
library(multcomp) 
library(lattice) 
library(ggplot2)
library(emmeans)
library(dietr)
library(janitor)
library(ggpubr)
library(rstatix)
library(broom)
library(coin)
library(exactRankTests)
library(stats)
library(multcomp)
library(nortest)
library(MASS)
library(matrixStats)
library(gridExtra)
library(mgcv)
library(stringr)








#Environmental factors
env <- read_excel("amrd_weakfish_data.xlsx", sheet=5)
env <- head(env, 52)

env <- env %>%
  mutate(date = substr(date, 1, 3))

env$DO.mg.L <- as.numeric(env$DO.mg.L)

env <- env [2:8]

env <- env %>%
  mutate(
    survey = case_when(
      survey == "DNR" & location == "MCB" ~ "DNR_MCB",
      survey == "DNR" & location == "PAX" ~ "DNR_PAX",
      survey == "CBL"                     ~ "CBL",
      TRUE                                ~ survey
    )
  )



#Boxplots by month
env_Aug <- env %>%
  filter(date == "Aug")
  
env_Jul <- env %>%
  filter(date == "Jul")

env_Sep <- env %>%
  filter(date == "Sep")


#Medians

median_Jul <- env_Jul %>%
  group_by(location, survey) %>%
  summarise(
    temp.C  = median(temp.C, na.rm = TRUE),
    sal.ppt = median(sal.ppt, na.rm = TRUE),
    DO.mg.L = median(DO.mg.L, na.rm = TRUE),
    pH.eq.L = median(pH.eq.L, na.rm = TRUE)
  )

median_Jul 

median_Aug <- env_Aug %>%
  group_by(location, survey) %>%
  summarise(
    temp.C  = median(temp.C, na.rm = TRUE),
    sal.ppt = median(sal.ppt, na.rm = TRUE),
    DO.mg.L = median(DO.mg.L, na.rm = TRUE),
    pH.eq.L = median(pH.eq.L, na.rm = TRUE)
  )

median_Aug 

median_Sep <- env_Sep %>%
  group_by(location, survey) %>%
  summarise(
    temp.C  = median(temp.C, na.rm = TRUE),
    sal.ppt = median(sal.ppt, na.rm = TRUE),
    DO.mg.L = median(DO.mg.L, na.rm = TRUE),
    pH.eq.L = median(pH.eq.L, na.rm = TRUE)
  )

median_Sep


#Means
#Jul_means <- env_Jul %>%
#  summarise(
#    temp_mean = mean(temp.C, na.rm = TRUE),
#    sal_mean  = mean(sal.ppt, na.rm = TRUE),
#    DO_mean   = mean(DO.mg.L, na.rm = TRUE),
#   pH_mean   = mean(pH.eq.L, na.rm = TRUE)
#  )

#Aug_means <- env_Aug %>%
#  summarise(
#  temp_mean = mean(temp.C, na.rm = TRUE),
#    sal_mean  = mean(sal.ppt, na.rm = TRUE),
#    DO_mean   = mean(DO.mg.L, na.rm = TRUE),
#    pH_mean   = mean(pH.eq.L, na.rm = TRUE)
#  )

#Sep_means <- env_Sep %>%
#  summarise(
#    temp_mean = mean(temp.C, na.rm = TRUE),
#    sal_mean  = mean(sal.ppt, na.rm = TRUE),
#    DO_mean   = mean(DO.mg.L, na.rm = TRUE),
#    pH_mean   = mean(pH.eq.L, na.rm = TRUE)
#  )



#July
temp.j <- ggplot() +
  geom_boxplot(
    data = env_Jul,
    aes(x = survey, y = temp.C, fill = survey),
    width = 0.5,
    outlier.shape = NA
  ) +
  scale_x_discrete(
    limits = c("DNR_PAX", "DNR_MCB", "CBL"),
    drop = FALSE
  ) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c(
    "DNR_PAX" = "cyan",
    "DNR_MCB" = "red",
    "CBL"     = "blue"
  )) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.tag = element_text(size = 30, face = "bold")
  )

sal.j <- ggplot() +
geom_boxplot(
    data = env_Jul,
    aes(x = survey, y = sal.ppt, fill = survey),
    width = 0.5,
    outlier.shape = NA
) +
  scale_x_discrete(
    limits = c("DNR_PAX", "DNR_MCB", "CBL"),
    drop = FALSE
  ) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c(
    "DNR_PAX" = "cyan",
    "DNR_MCB" = "red",
    "CBL"     = "blue"
  )) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.tag = element_text(size = 30, face = "bold")
  )

do.j <- ggplot() +
geom_boxplot(
    data = env_Jul,
    aes(x = survey, y = DO.mg.L, fill = survey),
    width = 0.5,
    outlier.shape = NA
) +
  scale_x_discrete(
    limits = c("DNR_PAX", "DNR_MCB", "CBL"),
    drop = FALSE
  ) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c(
    "DNR_PAX" = "cyan",
    "DNR_MCB" = "red",
    "CBL"     = "blue"
  )) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.tag = element_text(size = 30, face = "bold")
  )

ph.j <- ggplot() +
geom_boxplot(
    data = env_Jul,
    aes(x = survey, y = pH.eq.L, fill = survey),
    width = 0.5,
    outlier.shape = NA
) +
  scale_x_discrete(
    limits = c("DNR_PAX", "DNR_MCB", "CBL"),
    drop = FALSE
  ) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c(
    "DNR_PAX" = "cyan",
    "DNR_MCB" = "red",
    "CBL"     = "blue"
  )) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.tag = element_text(size = 30, face = "bold")
  )






#August
temp.a <- ggplot() +
  geom_boxplot(
    data = env_Aug,
    aes(x = survey, y = temp.C, fill = survey),
    width = 0.5,
    outlier.shape = NA
  ) +
  scale_x_discrete(
    limits = c("DNR_PAX", "DNR_MCB", "CBL"),
    drop = FALSE
  ) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c(
    "DNR_PAX" = "cyan",
    "DNR_MCB" = "red",
    "CBL"     = "blue"
  )) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.tag = element_text(size = 30, face = "bold")
  )

sal.a <- ggplot() +
  geom_boxplot(
    data = env_Aug,
    aes(x = survey, y = sal.ppt, fill = survey),
    width = 0.5,
    outlier.shape = NA
  ) +
  scale_x_discrete(
    limits = c("DNR_PAX", "DNR_MCB", "CBL"),
    drop = FALSE
  ) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c(
    "DNR_PAX" = "cyan",
    "DNR_MCB" = "red",
    "CBL"     = "blue"
  )) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.tag = element_text(size = 30, face = "bold")
  )

do.a <- ggplot() +
  geom_boxplot(
    data = env_Aug,
    aes(x = survey, y = DO.mg.L, fill = survey),
    width = 0.5,
    outlier.shape = NA
  ) +
  scale_x_discrete(
    limits = c("DNR_PAX", "DNR_MCB", "CBL"),
    drop = FALSE
  ) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c(
    "DNR_PAX" = "cyan",
    "DNR_MCB" = "red",
    "CBL"     = "blue"
  )) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.tag = element_text(size = 30, face = "bold")
  )

ph.a <- ggplot() +
  geom_boxplot(
    data = env_Aug,
    aes(x = survey, y = pH.eq.L, fill = survey),
    width = 0.5,
    outlier.shape = NA
  ) +
  scale_x_discrete(
    limits = c("DNR_PAX", "DNR_MCB", "CBL"),
    drop = FALSE
  ) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c(
    "DNR_PAX" = "cyan",
    "DNR_MCB" = "red",
    "CBL"     = "blue"
  )) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.tag = element_text(size = 30, face = "bold")
  )







#September
temp.s <- ggplot() +
  geom_boxplot(
    data = env_Sep,
    aes(x = survey, y = temp.C, fill = survey),
    width = 0.5,
    outlier.shape = NA
  ) +
  scale_x_discrete(
    limits = c("DNR_PAX", "DNR_MCB", "CBL"),
    labels = c(
      "DNR_PAX" = "PAX DNR",
      "DNR_MCB" = "MCBs DNR",
      "CBL"     = "PAX CBL"
    ),
    drop = FALSE
  ) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c(
    "DNR_PAX" = "cyan",
    "DNR_MCB" = "red",
    "CBL"     = "blue"
  )) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20),
    plot.tag = element_text(size = 30, face = "bold"))
  

sal.s <- ggplot() +
  geom_boxplot(
    data = env_Sep,
    aes(x = survey, y = sal.ppt, fill = survey),
    width = 0.5,
    outlier.shape = NA
  ) +
  scale_x_discrete(
    limits = c("DNR_PAX", "DNR_MCB", "CBL"),
    labels = c(
      "DNR_PAX" = "PAX DNR",
      "DNR_MCB" = "MCBs DNR",
      "CBL"     = "PAX CBL"
    ),
    drop = FALSE
  ) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c(
    "DNR_PAX" = "cyan",
    "DNR_MCB" = "red",
    "CBL"     = "blue"
  )) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20),
    plot.tag = element_text(size = 30, face = "bold")
  )

do.s <- ggplot() +
  geom_boxplot(
    data = env_Sep,
    aes(x = survey, y = DO.mg.L, fill = survey),
    width = 0.5,
    outlier.shape = NA
  ) +
  scale_x_discrete(
    limits = c("DNR_PAX", "DNR_MCB", "CBL"),
    labels = c(
      "DNR_PAX" = "PAX DNR",
      "DNR_MCB" = "MCBs DNR",
      "CBL"     = "PAX CBL"
    ),
    drop = FALSE
  ) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c(
    "DNR_PAX" = "cyan",
    "DNR_MCB" = "red",
    "CBL"     = "blue"
  )) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20),
    plot.tag = element_text(size = 30, face = "bold"))

ph.s <- ggplot() +
  geom_boxplot(
    data = env_Sep,
    aes(x = survey, y = pH.eq.L, fill = survey),
    width = 0.5,
    outlier.shape = NA
  ) +
  scale_x_discrete(
    limits = c("DNR_PAX", "DNR_MCB", "CBL"),
    labels = c(
      "DNR_PAX" = "PAX DNR",
      "DNR_MCB" = "MCBs DNR",
      "CBL"     = "PAX CBL"
    ),
    drop = FALSE
  ) +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c(
    "DNR_PAX" = "cyan",
    "DNR_MCB" = "red",
    "CBL"     = "blue"
  )) +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20),
    plot.tag = element_text(size = 30, face = "bold")
  )


library(patchwork)

temp_multiplot <- temp.j / temp.a / temp.s
do_multiplot <- do.j / do.a / do.s
ph_multiplot <- ph.j / ph.a / ph.s
sal_multiplot <- sal.j / sal.a / sal.s

temp_multiplot 
sal_multiplot
do_multiplot 
ph_multiplot 



#Save plots
base_path <- "C:/Users/Angel/Documents/Grad school (maryland)/Research/Data/Isotope/"

ggsave(paste0(base_path, "Fig_Temp.tiff"), temp_multiplot,
       device="tiff", dpi=600, width=8, height=12, units="in", compression="lzw")

ggsave(paste0(base_path, "Fig_DO.tiff"), do_multiplot,
       device="tiff", dpi=600, width=8, height=12, units="in", compression="lzw")

ggsave(paste0(base_path, "Fig_pH.tiff"), ph_multiplot,
       device="tiff", dpi=600, width=8, height=12, units="in", compression="lzw")

ggsave(paste0(base_path, "Fig_Salinity.tiff"), sal_multiplot,
       device="tiff", dpi=600, width=8, height=12, units="in", compression="lzw")





# Calculate the mean and standard error for MCB
env_mcb <- env %>%
  filter(location == "MCB")

env_mcb_mean_se <- env_mcb %>%
  group_by(date) %>%
  summarise(mean_temp = mean(temp.C, na.rm = TRUE),
            mean_salinity = mean(sal.ppt, na.rm = TRUE),
            mean_DO = mean(`DO.mg.L`, na.rm = TRUE),
            mean_pH = mean(`pH.eq.L`, na.rm = TRUE),
            se_temp = sd(temp.C, na.rm = TRUE) / sqrt(n()),
            se_salinity = sd(sal.ppt, na.rm = TRUE) / sqrt(n()),
            se_DO = sd(`DO.mg.L`, na.rm = TRUE) / sqrt(n()),
            se_pH = sd(`pH.eq.L`, na.rm = TRUE) / sqrt(n()))



# Calculate the mean and standard error for PAX DNR
env_pax_dnr <- env %>%
  filter(location == "PAX" & survey == "DNR")


env_pax_dnr_se <- env_pax_dnr %>%
  group_by(date) %>%
  summarise(mean_temp = mean(temp.C, na.rm = TRUE),
            mean_salinity = mean(sal.ppt, na.rm = TRUE),
            mean_DO = mean(`DO.mg.L`, na.rm = TRUE),
            mean_pH = mean(`pH.eq.L`, na.rm = TRUE),
            se_temp = sd(temp.C, na.rm = TRUE) / sqrt(n()),
            se_salinity = sd(sal.ppt, na.rm = TRUE) / sqrt(n()),
            se_DO = sd(`DO.mg.L`, na.rm = TRUE) / sqrt(n()),
            se_pH = sd(`pH.eq.L`, na.rm = TRUE) / sqrt(n()))



# Calculate the mean and standard error for PAX CBL
env_pax_cbl <- env %>%
  filter(location == "PAX" & survey == "CBL")

env_paxcbl_mean_se <- env_pax_cbl %>%
  group_by(date) %>%
  summarise(mean_temp = mean(temp.C, na.rm = TRUE),
            mean_salinity = mean(sal.ppt, na.rm = TRUE),
            mean_DO = mean(`DO.mg.L`, na.rm = TRUE),
            mean_pH = mean(`pH.eq.L`, na.rm = TRUE),
            se_temp = sd(temp.C, na.rm = TRUE) / sqrt(n()),
            se_salinity = sd(sal.ppt, na.rm = TRUE) / sqrt(n()),
            se_DO = sd(`DO.mg.L`, na.rm = TRUE) / sqrt(n()),
            se_pH = sd(`pH.eq.L`, na.rm = TRUE) / sqrt(n()))









setwd("~/Grad school (maryland)/Research/Data")

#FIXING DIFFERENCES IN LENGTH DISTRIBUTIONS
lw <- read_excel("amrd_weakfish_data.xlsx", sheet=1)
sc <- read_excel("amrd_weakfish_data.xlsx", sheet=2)

lw$TL_mm <- as.numeric(lw$TL_mm)
lw$TL_cm <- lw$TL_mm / 10

lw <- lw[!is.na(lw$TL_cm), ] 

totalmc <- nrow(subset (lw, location == 'MCB'))
totalpx <- nrow(subset (lw, location == 'PAX'))

# Filter rows where all prey columns are less than 1 (zero stomach contents)
zero_stomach_contents <- sc[(sc$mysidacea < 1 & sc$polychaete < 1 
                             & sc$unidentified_fish < 1 & sc$amphipods < 1 
                             & sc$horseshoe_crabs < 1 & sc$shrimp_larvae < 1
                             & sc$caligidae < 1 & sc$isopoda < 1
                             & sc$copepoda < 1 & sc$shrimp_not_larvae < 1 &
                               sc$parasitic_copepoda < 1), ]

# Extract the fish_id of rows with zero stomach contents
zero_stomach_fish_ids <- zero_stomach_contents$fish_id

lw <- lw[!(lw$fish_id %in% zero_stomach_fish_ids), ]

#TRIM DATA
lw <- lw %>%
  filter(TL_cm > 4.8 & TL_cm < 13.2)

totalmc <- nrow(subset (lw, location == 'MCB'))
totalpx <- nrow(subset (lw, location == 'PAX'))

#Perform KS test for the whole dataset trimmed
ks.test(lw$TL_cm[lw$location == "MCB"],
        lw$TL_cm[lw$location == "PAX"])

ggplot(lw, aes(x = location, y = TL_cm, fill = location)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Location", y = "Total Length (cm)")





#RELATIVE CONDITION (Kn)
#For this analysis, only consider fish above 4.7 or below 12.9 cm

# Add a new column 'Month' based on values in the 'date' column
lw$Month <- ifelse(grepl("Jul", lw$date), "July", 
                   ifelse(grepl("Aug", lw$date), "August", 
                          ifelse(grepl("Sep", lw$date), "September", NA)))

totalmc <- nrow(subset (lw, location == 'MCB'))
totalpx <- nrow(subset (lw, location == 'PAX'))

#write.csv(lw, "C:/Users/reyes/OneDrive/Documents/Grad school (maryland)/Research/Data/lw_dataset.csv", row.names = FALSE)




#LAST WORK 10/22/24, continue here



# New a and b parameters from excel 
#bMCBs = 3.123
#aMCBs = 0.006596606

#bPAX = 3.2518
#aPAX = 0.004670449
a = 0.005727363
b = 3.1725  
  
lw$logTL_cm <- log(lw$TL_cm)

#Calculate mean, min and max for each location
summary_stats <- lw %>%
  group_by(location) %>%
  summarise(
    min_TL_cm = min(TL_cm, na.rm = TRUE),
    mean_TL_cm = mean(TL_cm, na.rm = TRUE),
    max_TL_cm = max(TL_cm, na.rm = TRUE)
  )

#FISH LENGTH FREQUENCIES 

# Replace any occurrence of "Pax" with "CBL" in the survey column
lw <- lw %>%
  mutate(survey = str_replace(survey, "Pax.*", "CBL"))

ggplot(lw, aes(x = TL_cm, fill = interaction(location, survey))) +
  geom_histogram(binwidth = 1, color = "black", position = "dodge") +
  labs(x = "TL (cm)", y = "Frequency", title = "") +
  scale_fill_manual(values = c(
    "PAX.DNR" = "blue",
    "PAX.CBL" = "cyan",  # Shiny blue color
    "MCB.DNR" = "red"
  )) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 30),         # Increase axis text size
    axis.title = element_text(size = 34),        # Increase axis title size
    legend.title = element_blank(),              # Remove legend title
    legend.text = element_text(size = 10),       # Make legend text small
    plot.title = element_text(size = 38, hjust = 0.5), # Increase plot title size
    panel.grid.major.x = element_blank(),        # Remove major vertical grid lines
    panel.grid.minor.x = element_blank(),        # Remove minor vertical grid lines
    panel.grid.minor.y = element_blank(),        # Remove minor horizontal grid lines on Y-axis
    panel.grid.major.y = element_line(color = "gray70", size = 0.5)  # Darker Y-axis major grid lines
  )

#PREPARE DATA TO CALCULATE KN
lw <- lw %>% select(-c(2, 4:6))


# Calculating predicted weight at length using location-specific a and b values
lw$pred.W <- a*lw$TL_cm^b



#CALCULATING RELATIVE CONDITION
lw$Kn = lw$wt_g/lw$pred.W


# Step 1: Calculate the average Kn for location
Kn_summary <- lw %>%
  group_by(location) %>%
  summarise(avg_Kn = mean(Kn, na.rm = TRUE), .groups = 'drop')

# Step 2: Join the summary with the original dataset to add avg_Kn column
lw <- lw %>%
  left_join(Kn_summary, by = c("location"))

lw$LogKn <- log(lw$Kn)

# Step 4: Create the plot with vertical trendlines and add avg_Kn annotations
ggplot(data = lw, mapping = aes(x = logTL_cm, y = LogKn, color = location)) +
  geom_point() +
  geom_smooth(method = "glm", se = TRUE) +
  labs(x = "Log (TL)", y = "Log (Kn)", title = "") +
  scale_color_manual(values = c("PAX" = "blue", "MCB" = "red")) +  # Custom colors for locations
  theme_minimal() +
  theme(
    axis.text = element_text(size = 30),  # Increase text size for both axes
    axis.title = element_text(size = 34),  # Increase title size
    axis.text.x = element_text(),  # Rotate x-axis labels
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.title = element_text(size = 34),  # Increase legend title size
    legend.text = element_text(size = 28),   # Increase legend text size
    plot.title = element_text(size = 38, face = "bold", hjust = 0.5),
    axis.line = element_line(size = 1, color = "black"),  # Add axis lines
    axis.ticks = element_line(size = 1, color = "black"),  # Add axis ticks
    axis.ticks.length = unit(-0.2, "cm")  # Make ticks go inside the plot
  )



#GLM Model
#glm_Kn2 <- glm(LogKn ~ location + logTL_cm + location:logTL_cm, 
#             data = lw, family = gaussian)

# Display the summary of the model
#summary(glm_Kn2)

glm_Kn3 <- glm(LogKn ~ location + logTL_cm, 
               data = lw, family = gaussian)

# Display the summary of the model
summary(glm_Kn3)
#anova(glm_Kn3)

#Normality
#shapiro.test(glm_Kn2$residuals)
shapiro.test(glm_Kn3$residuals)

par(mfrow = c(2, 2)) 
#plot(glm_Kn)
plot(glm_Kn3)

#Kruskal test
kruskal.test(LogKn ~ location, data = lw)

# Fit a GAM model with smooth terms for logTL_cm
#gam_model <- gam(Kn ~ location + logTL_cm + location:logTL_cm, data = lw)

# Display summary
#summary(gam_model)

#par(mfrow = c(2, 2)) 
#gam.check(gam_model)

# Fit a GAM model with smooth terms for logTL_cm
gam_model2 <- gam(LogKn ~ location + s(logTL_cm), data = lw)

# Display summary
summary(gam_model2)

#par(mfrow = c(2, 2)) 
#gam.check(gam_model2)



#I TRIED GLM, KRUSKAL, GAM, and sqrt
#it all comes back as not statistically significant

#GLMs are more robust to violations of the assumptions of ordinary 
#least squares regression, 
#particularly when the response variable does not meet the assumption 
#of normality or homoscedasticity.



















#FULLNESS INDEX
#DO STOMACH CONTENT WEIGHT/WEIGHT OF THE FISH WITOUTH CONTENT
#Condition analysis
lw <- read_excel("amrd_weakfish_data.xlsx", sheet=1)
sc <- read_excel("amrd_weakfish_data.xlsx", sheet=2)

lw$TL_mm <- as.numeric(lw$TL_mm)
lw$TL_cm <- lw$TL_mm / 10

lw <- lw[!is.na(lw$TL_cm), ] 

totalmcb <- nrow(subset (lw, location == 'MCB'))
totalpax <- nrow(subset (lw, location == 'PAX'))

# Filter rows where all prey columns are less than 1 (zero stomach contents)
zero_stomach_contents <- sc[(sc$mysidacea < 1 & sc$polychaete < 1 
                             & sc$unidentified_fish < 1 & sc$amphipods < 1 
                             & sc$horseshoe_crabs < 1 & sc$shrimp_larvae < 1
                             & sc$caligidae < 1 & sc$isopoda < 1
                             & sc$copepoda < 1 & sc$shrimp_not_larvae < 1 &
                               sc$parasitic_copepoda < 1), ]

# Extract the fish_id of rows with zero stomach contents
zero_stomach_fish_ids <- zero_stomach_contents$fish_id

lw <- lw[!(lw$fish_id %in% zero_stomach_fish_ids), ]

#TRIM DATA
lw <- lw %>%
  filter(TL_cm > 4.8 & TL_cm < 13.2)

totalmcb <- nrow(subset (lw, location == 'MCB'))
totalpax <- nrow(subset (lw, location == 'PAX'))



#Do a new column with fish weight - stomach content weight
# Step 1: Calculate the rest in the sc sheet
sc$content_wt_g <- sc$full_wt_g - sc$empty_wt_g

sc <- sc[complete.cases(sc$content_wt_g), ]

# Step 2: Merge the calculated column with the lw sheet by fish_id
lw_sc <- merge(lw, sc[, c("fish_id", "content_wt_g")], by = "fish_id", all.x = TRUE)
lw_sc$fish_wt_g <- lw_sc$wt_g - lw_sc$content_wt_g
lw_sc <- lw_sc[complete.cases(lw_sc$fish_wt_g), ]
lw <- lw_sc
lw <- lw[, !(names(lw) %in% c("wt_g", "content_wt_g"))]
lw$wt_g <- lw$fish_wt_g
lw <- lw[, !(names(lw) %in% c("fish_wt_g"))]

#Calculate Fullness Index
FI <- lw_sc
FI$FI <- FI$content_wt_g/FI$fish_wt_g

totalmcb <- nrow(subset (FI, location == 'MCB'))
totalpax <- nrow(subset (FI, location == 'PAX'))

#Perform KS test for the whole dataset trimmed
ks.test(FI$TL_cm[lw$location == "MCB"],
        FI$TL_cm[lw$location == "PAX"])

#transforming fish weight for normal distribution
FI$log_wt_g <- log(FI$fish_wt_g)

#Transform fullness index to sqrt
FI$log_FI <- log(FI$FI)

FI$logTL_cm <- log(lw$TL_cm)
#Show the confidence intervals to see if they intersect at the tips
#plot log wt
ggplot(FI, aes(x = logTL_cm, y = log_FI, col = location)) +
  geom_point() +
  geom_smooth(method = "glm", se = TRUE) +
  xlab("Log (TL)") +
  ylab("Log (SFI)") +
  ggtitle("") +
  scale_color_manual(values = c("PAX" = "blue", "MCB" = "red")) +  # Custom colors
  theme_minimal() +
  theme(
    axis.text = element_text(size = 30),          # Increase text size for both axes
    axis.title = element_text(size = 34),         # Increase title size
    panel.grid.major = element_blank(),           # Remove major grid lines
    panel.grid.minor = element_blank(),           # Remove minor grid lines
    axis.line = element_line(size = 1, color = "black"),  # Add axis lines
    axis.ticks = element_line(size = 1, color = "black"),  # Add axis ticks
    axis.ticks.length = unit(-0.2, "cm"),         # Make ticks go inside the plot
    legend.title = element_text(size = 34),       # Increase legend title size
    legend.text = element_text(size = 28),        # Increase legend text size
    plot.title = element_text(size = 38, face = "bold", hjust = 0.5) # Center plot title
  ) +
  scale_x_continuous(limits = c(1.5, 2.65)) +  # Set x-axis limits
  scale_y_continuous(limits = c(-7.4, -1.5))    # Set y-axis limits

FI$TL_mm <- as.numeric(FI$TL_mm)
FI$TL_cm <- FI$TL_mm/10
FI$logTL_cm <- log(FI$TL_cm)

#LOG FI GLM
glm_int_FI <- glm(formula = log_FI ~ location + logTL_cm+ location:logTL_cm, data = FI)
summary(glm_int_FI)

#Normality
shapiro.test(residuals(glm_int_FI))
#THIS MODEL LOOKS GOOD

par(mfrow = c(2, 2)) 
plot(glm_int_FI)



#FI
#glm_int_FI2 <- glm(formula = FI ~ location +  TL_cm+ location:TL_cm, data = FI)
#summary(glm_int_FI2)

#Normality
#shapiro.test(residuals(glm_int_FI2))
#THIS ONE SUCKS ASS

#par(mfrow = c(2, 2)) 
#plot(glm_int_FI2)








#CREATE TROPHIC DIVERSITY CURVES
setwd("C:/Users/reyes/OneDrive/Documents/Grad school (maryland)/Research/Data")


#RUN FROM SCRATCH
#Excluding empty stomachs
lw <- read_excel("amrd_weakfish_data.xlsx", sheet=1)
sc <- read_excel("amrd_weakfish_data.xlsx", sheet=2)

lw$season <- ifelse(grepl("Jul|Aug", lw$date), "Summer", 
                    ifelse(grepl("Sep", lw$date), "Fall", NA))

#MM to CM
lw$TL_mm <- as.numeric(lw$TL_mm)
lw$TL_cm <- lw$TL_mm / 10
lw <- lw[!is.na(lw$TL_cm), ] 

totalmc <- nrow(subset (lw, location == 'MCB'))
totalpx <- nrow(subset (lw, location == 'PAX'))

# Filter rows where all prey columns are less 
#than 1 (zero stomach contents)
zero_stomach_contents <- sc[(sc$mysidacea < 1 & sc$polychaete < 1 
                             & sc$unidentified_fish < 1 & sc$amphipods < 1 
                             & sc$horseshoe_crabs < 1 & sc$shrimp_larvae < 1
                             & sc$caligidae < 1 & sc$isopoda < 1
                             & sc$copepoda < 1 & sc$shrimp_not_larvae < 1 &
                               sc$parasitic_copepoda < 1), ]

# Extract the fish_id of rows with zero stomach contents
zero_stomach_fish_ids <- zero_stomach_contents$fish_id

#Remove fish with 0 content in lw
lw <- lw[!(lw$fish_id %in% zero_stomach_fish_ids), ]

#TRIM DATA
lw <- lw %>%
  filter(TL_cm > 4.8 & TL_cm < 13.2)

lw <- lw[, -c( 2,5,6,7,9)]

TpDv <- merge(sc, lw, by = "fish_id")

totalmc <- nrow(subset (TpDv, location.x == 'MCB'))
totalpx <- nrow(subset (TpDv, location.x == 'PAX'))

TpDv <- TpDv[, -c( 3, 4,16)]

colnames(TpDv)[colnames(TpDv) == "location.x"] <- "location"

# Combine and rename columns based on the specified conditions
TpDv <- TpDv %>%
  mutate(
    polychaeta = polychaete,
    amphipoda = amphipods,
    other_crustaceans = horseshoe_crabs + shrimp_not_larvae + isopoda,
    parasites = caligidae + parasitic_copepoda,
    unidentified_fish = unidentified_fish,
    shrimp_larvae = shrimp_larvae
  ) %>%
  select(-polychaete, -amphipods, -horseshoe_crabs, -shrimp_not_larvae,
         -isopoda, -caligidae, -parasitic_copepoda)

# Define the desired order for the first few columns
fixed_columns <- c("fish_id", "location", "season")

# Get the remaining columns, sorted alphabetically
remaining_columns <- setdiff(names(TpDv), fixed_columns)
remaining_columns <- sort(remaining_columns)

# Combine the fixed columns with the sorted remaining columns
new_order <- c(fixed_columns, remaining_columns)

# Reorder the dataframe columns
TpDv <- TpDv %>%
  select(all_of(new_order))

TpDv <- TpDv[, -c( 6)]

# Filter and remove columns for PAX
PAX <- filter(TpDv, location == "PAX") %>% select(-1:-3)
PAX_Summer <- filter(TpDv, location == "PAX" & season == "Summer") %>% select(-1:-3)
PAX_Fall <- filter(TpDv, location == "PAX" & season == "Fall") %>% select(-1:-3)

# Filter and remove columns for MCB
MCB <- filter(TpDv, location == "MCB") %>% select(-1:-3)
MCB_Summer <- filter(TpDv, location == "MCB" & season == "Summer") %>% select(-1:-3)
MCB_Fall <- filter(TpDv, location == "MCB" & season == "Fall") %>% select(-1:-3)

# Define the base directory path
base_dir <- "C:/Users/reyes/OneDrive/Documents/Grad school (maryland)/Research/Data/TDC.new/"

# Write CSV files for PAX dataframes
write.csv(PAX, paste0(base_dir, "PAX.csv"), row.names = FALSE)
write.csv(PAX_Summer, paste0(base_dir, "PAX_Summer.csv"), row.names = FALSE)
write.csv(PAX_Fall, paste0(base_dir, "PAX_Fall.csv"), row.names = FALSE)

# Write CSV files for MCB dataframes
write.csv(MCB, paste0(base_dir, "MCB.csv"), row.names = FALSE)
write.csv(MCB_Summer, paste0(base_dir, "MCB_Summer.csv"), row.names = FALSE)
write.csv(MCB_Fall, paste0(base_dir, "MCB_Fall.csv"), row.names = FALSE)







setwd("C:/Users/reyes/OneDrive/Documents/Grad school (maryland)/Research/Data/TDC.new/")


#MCB
MCB <- read.csv("MCB.csv")
nprey=ncol(MCB) #number of prey
nsamp= nrow(MCB) #number of stomachs

#CALCULATING THE CUMULATIVE DIVERSITY

# Initialize out1 as an empty list
out1 <- vector("list", length = 100)

for (i in 1:100) {
  # Randomly sample rows from MCB dataframe
  randdat <- MCB[sample(nrow(MCB), replace = FALSE), ]
  
  # Calculate cumulative sum across columns
  csum <- cumsum(randdat)
  
  # Calculate diversity index (Shannon index)
  div <- diversity(csum, index = 'shannon')
  
  # Store div in out1 at index i
  out1[[i]] <- div
}
 # out1[[i]]=cbind(div) #Creating list of div data
  #END OF LOOP
  
  out2=data.frame(do.call(rbind, out1)) #Creating a very long column of data from out1, the diversity of each stomach
  out2=t(out2) #tranpose data
  out2 <- as.data.frame(out2) #treat it as data frame
  out2$stomnum <- 1:nrow(out2) #Add a stomach number for each diversity index
  out2 <- out2[, c("stomnum", names(out2)[-ncol(out2)])]
  
  
  
  out2$xbar <- rowMeans(out2[, -1], na.rm = TRUE)
   #xbar = mean, get a mean diversity index for each stomach that 
  #was randomized a 100 times (e.g., sum of all stomachs 1/100)
  
  out2$sd <- apply(out2[, -c(1, which(names(out2) == "xbar"))], 1, sd, na.rm = TRUE)
  ##Calculate sd of diversity index for each stomach that was randomized a 100 times
  
  MCB_TpDv <- out2[, c("stomnum", "xbar", "sd", setdiff(names(out2), c("stomnum", "xbar", "sd")))]
  
  # Reset row names to NULL to remove them as columns
  row.names(MCB_TpDv) <- NULL
  
  base_dir <- "C:/Users/reyes/OneDrive/Documents/Grad school (maryland)/Research/Data/TDC.new/"
  write.csv(MCB_TpDv, paste0(base_dir, "MCB_TpDv.csv"))

  MCBcurve <- ggplot(MCB_TpDv, aes(x = stomnum, y = xbar)) +
    geom_line(size = 1.2) +  # Increase line thickness
    geom_ribbon(aes(ymin = xbar - sd, ymax = xbar + sd), alpha = 0.2) +
    theme_bw(base_size = 20) +  # Increase base font size
    labs(x = 'Number of stomachs', y = 'Shannon H') +  # Axis labels
    theme(axis.title = element_text(size = 22),  # Increase axis label size
          plot.title = element_text(size = 24),  # Increase plot title size
          plot.subtitle = element_text(size = 18),  # Increase plot subtitle size
          plot.caption = element_text(size = 16),  # Increase plot caption size
          plot.margin = margin(20, 20, 20, 20)) +  # Adjust plot margins
    annotate("text", x = Inf, y = -Inf, label = "<0.0001", hjust = 1.5, vjust = -3, size = 10) 


#PAX
  PAX <- read.csv("PAX.csv")
  nprey=ncol(PAX) #number of prey
  nsamp= nrow(PAX) #number of stomachs
  
  #CALCULATING THE CUMULATIVE DIVERSITY
  
  # Initialize out1 as an empty list
  out1 <- vector("list", length = 100)
  
  for (i in 1:100) {
    # Randomly sample rows from MCB dataframe
    randdat <- PAX[sample(nrow(PAX), replace = FALSE), ]
    
    # Calculate cumulative sum across columns
    csum <- cumsum(randdat)
    
    # Calculate diversity index (Shannon index)
    div <- diversity(csum, index = 'shannon')
    
    # Store div in out1 at index i
    out1[[i]] <- div
  }
  # out1[[i]]=cbind(div) #Creating list of div data
  #END OF LOOP
  
  out2=data.frame(do.call(rbind, out1)) #Creating a very long column of data from out1, the diversity of each stomach
  out2=t(out2) #tranpose data
  out2 <- as.data.frame(out2) #treat it as data frame
  out2$stomnum <- 1:nrow(out2) #Add a stomach number for each diversity index
  out2 <- out2[, c("stomnum", names(out2)[-ncol(out2)])]
  
  
  
  out2$xbar <- rowMeans(out2[, -1], na.rm = TRUE)
  #xbar = mean, get a mean diversity index for each stomach that 
  #was randomized a 100 times (e.g., sum of all stomachs 1/100)
  
  out2$sd <- apply(out2[, -c(1, which(names(out2) == "xbar"))], 1, sd, na.rm = TRUE)
  ##Calculate sd of diversity index for each stomach that was randomized a 100 times
  
  PAX_TpDv <- out2[, c("stomnum", "xbar", "sd", setdiff(names(out2), c("stomnum", "xbar", "sd")))]
  
  # Reset row names to NULL to remove them as columns
  row.names(PAX_TpDv) <- NULL
  
  PAX_TpDv <- PAX_TpDv[1:3]
  
  base_dir <- "C:/Users/reyes/OneDrive/Documents/Grad school (maryland)/Research/Data/TDC.new/"
  write.csv(PAX_TpDv, paste0(base_dir, "PAX_TpDv.csv"))
  
  PAXcurve <- ggplot(PAX_TpDv, aes(x = stomnum, y = xbar)) +
    geom_line(size = 1.2) +  # Increase line thickness
    geom_ribbon(aes(ymin = xbar - sd, ymax = xbar + sd), alpha = 0.2) +
    theme_bw(base_size = 20) +  # Increase base font size
    labs(x = 'Number of stomachs', y = 'Shannon H') +  # Axis labels
    theme(axis.title = element_text(size = 22),  # Increase axis label size
          plot.title = element_text(size = 24),  # Increase plot title size
          plot.subtitle = element_text(size = 18),  # Increase plot subtitle size
          plot.caption = element_text(size = 16),  # Increase plot caption size
          plot.margin = margin(20, 20, 20, 20)) +  # Adjust plot margins
    annotate("text", x = Inf, y = -Inf, label = "<", hjust = 1.5, vjust = -3, size = 10) 

  
  
  
  
  
  
#MCB SUMMER
  MCB.s <- read.csv("MCB_Summer.csv")
  nprey=ncol(MCB.s) #number of prey
  nsamp= nrow(MCB.s) #number of stomachs
  
  
  # Initialize out1 as an empty list
  out1 <- vector("list", length = 100)
  
  for (i in 1:100) {
    # Randomly sample rows from MCB dataframe
    randdat <- MCB.s[sample(nrow(MCB.s), replace = FALSE), ]
    
    # Calculate cumulative sum across columns
    csum <- cumsum(randdat)
    
    # Calculate diversity index (Shannon index)
    div <- diversity(csum, index = 'shannon')
    
    # Store div in out1 at index i
    out1[[i]] <- div
  }
  # out1[[i]]=cbind(div) #Creating list of div data
  #END OF LOOP
  
  out2=data.frame(do.call(rbind, out1)) #Creating a very long column of data from out1, the diversity of each stomach
  out2=t(out2) #tranpose data
  out2 <- as.data.frame(out2) #treat it as data frame
  out2$stomnum <- 1:nrow(out2) #Add a stomach number for each diversity index
  out2 <- out2[, c("stomnum", names(out2)[-ncol(out2)])]
  
  out2$xbar <- rowMeans(out2[, -1], na.rm = TRUE)
  #xbar = mean, get a mean diversity index for each stomach that 
  #was randomized a 100 times (e.g., sum of all stomachs 1/100)
  
  out2$sd <- apply(out2[, -c(1, which(names(out2) == "xbar"))], 1, sd, na.rm = TRUE)
  ##Calculate sd of diversity index for each stomach that was randomized a 100 times
  
  MCB.s_TpDv <- out2[, c("stomnum", "xbar", "sd", setdiff(names(out2), c("stomnum", "xbar", "sd")))]
  
  # Reset row names to NULL to remove them as columns
  row.names(MCB.s_TpDv) <- NULL
  
  MCB.s_TpDv <- MCB.s_TpDv[1:3]
  
  base_dir <- "C:/Users/reyes/OneDrive/Documents/Grad school (maryland)/Research/Data/TDC.new/"
  write.csv(MCB.s_TpDv, paste0(base_dir, "MCB.s_TpDv.csv"))
  
#MCB.s.curve <- 
  ggplot(MCB.s_TpDv, aes(x = stomnum, y = xbar)) +
    geom_line(size = 1.2) +  # Increase line thickness
    geom_ribbon(aes(ymin = xbar - sd, ymax = xbar + sd), alpha = 0.2) +
    theme_bw(base_size = 20) +  # Increase base font size
    labs(x = 'Number of stomachs', y = 'Shannon H') +  # Axis labels
    theme(axis.title = element_text(size = 22),  # Increase axis label size
          plot.title = element_text(size = 24),  # Increase plot title size
          plot.subtitle = element_text(size = 18),  # Increase plot subtitle size
          plot.caption = element_text(size = 16),  # Increase plot caption size
          plot.margin = margin(20, 20, 20, 20)) +  # Adjust plot margins
    annotate("text", x = Inf, y = -Inf, label = "<", hjust = 1.5, vjust = -3, size = 10) 


  
  
  
  
#MCB FALL
  
  MCB.f <- read.csv("MCB_Fall.csv")
  nprey=ncol(MCB.f) #number of prey
  nsamp= nrow(MCB.f) #number of stomachs
  
  # Initialize out1 as an empty list
  out1 <- vector("list", length = 100)
  
  for (i in 1:100) {
    # Randomly sample rows from MCB dataframe
    randdat <- MCB.f[sample(nrow(MCB.f), replace = FALSE), ]
    
    # Calculate cumulative sum across columns
    csum <- cumsum(randdat)
    
    # Calculate diversity index (Shannon index)
    div <- diversity(csum, index = 'shannon')
    
    # Store div in out1 at index i
    out1[[i]] <- div
  }
  # out1[[i]]=cbind(div) #Creating list of div data
  #END OF LOOP
  
  out2=data.frame(do.call(rbind, out1)) #Creating a very long column of data from out1, the diversity of each stomach
  out2=t(out2) #tranpose data
  out2 <- as.data.frame(out2) #treat it as data frame
  out2$stomnum <- 1:nrow(out2) #Add a stomach number for each diversity index
  out2 <- out2[, c("stomnum", names(out2)[-ncol(out2)])]
  
  out2$xbar <- rowMeans(out2[, -1], na.rm = TRUE)
  #xbar = mean, get a mean diversity index for each stomach that 
  #was randomized a 100 times (e.g., sum of all stomachs 1/100)
  
  out2$sd <- apply(out2[, -c(1, which(names(out2) == "xbar"))], 1, sd, na.rm = TRUE)
  ##Calculate sd of diversity index for each stomach that was randomized a 100 times
  
  MCB.f_TpDv <- out2[, c("stomnum", "xbar", "sd", setdiff(names(out2), c("stomnum", "xbar", "sd")))]
  
  # Reset row names to NULL to remove them as columns
  row.names(MCB.f_TpDv) <- NULL
  
  MCB.f_TpDv <- MCB.f_TpDv[1:3]
  
  base_dir <- "C:/Users/reyes/OneDrive/Documents/Grad school (maryland)/Research/Data/TDC.new/"
  write.csv(MCB.f_TpDv, paste0(base_dir, "MCB.f_TpDv.csv"))
  
  #MCB.f.curve <- 
  ggplot(MCB.f_TpDv, aes(x = stomnum, y = xbar)) +
    geom_line(size = 1.2) +  # Increase line thickness
    geom_ribbon(aes(ymin = xbar - sd, ymax = xbar + sd), alpha = 0.2) +
    theme_bw(base_size = 20) +  # Increase base font size
    labs(x = 'Number of stomachs', y = 'Shannon H') +  # Axis labels
    theme(axis.title = element_text(size = 22),  # Increase axis label size
          plot.title = element_text(size = 24),  # Increase plot title size
          plot.subtitle = element_text(size = 18),  # Increase plot subtitle size
          plot.caption = element_text(size = 16),  # Increase plot caption size
          plot.margin = margin(20, 20, 20, 20)) +  # Adjust plot margins
    annotate("text", x = Inf, y = -Inf, label = "<", hjust = 1.5, vjust = -3, size = 10) 

  
  
  
  
  
  
  
#PAX SUMMER
  
  PAX.s <- read.csv("PAX_Summer.csv")
  nprey=ncol(PAX.s) #number of prey
  nsamp= nrow(PAX.s) #number of stomachs
  
  #CALCULATING THE CUMULATIVE DIVERSITY
  
  # Initialize out1 as an empty list
  out1 <- vector("list", length = 100)
  
  for (i in 1:100) {
    # Randomly sample rows from MCB dataframe
    randdat <- PAX.s[sample(nrow(PAX.s), replace = FALSE), ]
    
    # Calculate cumulative sum across columns
    csum <- cumsum(randdat)
    
    # Calculate diversity index (Shannon index)
    div <- diversity(csum, index = 'shannon')
    
    # Store div in out1 at index i
    out1[[i]] <- div
  }
  # out1[[i]]=cbind(div) #Creating list of div data
  #END OF LOOP
  
  out2=data.frame(do.call(rbind, out1)) #Creating a very long column of data from out1, the diversity of each stomach
  out2=t(out2) #tranpose data
  out2 <- as.data.frame(out2) #treat it as data frame
  out2$stomnum <- 1:nrow(out2) #Add a stomach number for each diversity index
  out2 <- out2[, c("stomnum", names(out2)[-ncol(out2)])]
  
  
  
  out2$xbar <- rowMeans(out2[, -1], na.rm = TRUE)
  #xbar = mean, get a mean diversity index for each stomach that 
  #was randomized a 100 times (e.g., sum of all stomachs 1/100)
  
  out2$sd <- apply(out2[, -c(1, which(names(out2) == "xbar"))], 1, sd, na.rm = TRUE)
  ##Calculate sd of diversity index for each stomach that was randomized a 100 times
  
  PAX.s_TpDv <- out2[, c("stomnum", "xbar", "sd", setdiff(names(out2), c("stomnum", "xbar", "sd")))]
  
  # Reset row names to NULL to remove them as columns
  row.names(PAX.s_TpDv) <- NULL
  
  PAX.s_TpDv <- PAX.s_TpDv[1:3]
  
  base_dir <- "C:/Users/reyes/OneDrive/Documents/Grad school (maryland)/Research/Data/TDC.new/"
  write.csv(PAX.s_TpDv, paste0(base_dir, "PAX.s_TpDv.csv"))
  
  #PAX.s.curve <- 
    ggplot(PAX.s_TpDv, aes(x = stomnum, y = xbar)) +
    geom_line(size = 1.2) +  # Increase line thickness
    geom_ribbon(aes(ymin = xbar - sd, ymax = xbar + sd), alpha = 0.2) +
    theme_bw(base_size = 20) +  # Increase base font size
    labs(x = 'Number of stomachs', y = 'Shannon H') +  # Axis labels
    theme(axis.title = element_text(size = 22),  # Increase axis label size
          plot.title = element_text(size = 24),  # Increase plot title size
          plot.subtitle = element_text(size = 18),  # Increase plot subtitle size
          plot.caption = element_text(size = 16),  # Increase plot caption size
          plot.margin = margin(20, 20, 20, 20)) +  # Adjust plot margins
    annotate("text", x = Inf, y = -Inf, label = "<", hjust = 1.5, vjust = -3, size = 10) 


  
    
    
    
    
  
#PAX FALL
    PAX.f <- read.csv("PAX_Fall.csv")
    nprey=ncol(PAX.f) #number of prey
    nsamp= nrow(PAX.f) #number of stomachs
    
    #CALCULATING THE CUMULATIVE DIVERSITY
    
    # Initialize out1 as an empty list
    out1 <- vector("list", length = 100)
    
    for (i in 1:100) {
      # Randomly sample rows from MCB dataframe
      randdat <- PAX.f[sample(nrow(PAX.f), replace = FALSE), ]
      
      # Calculate cumulative sum across columns
      csum <- cumsum(randdat)
      
      # Calculate diversity index (Shannon index)
      div <- diversity(csum, index = 'shannon')
      
      # Store div in out1 at index i
      out1[[i]] <- div
    }
    # out1[[i]]=cbind(div) #Creating list of div data
    #END OF LOOP
    
    out2=data.frame(do.call(rbind, out1)) #Creating a very long column of data from out1, the diversity of each stomach
    out2=t(out2) #tranpose data
    out2 <- as.data.frame(out2) #treat it as data frame
    out2$stomnum <- 1:nrow(out2) #Add a stomach number for each diversity index
    out2 <- out2[, c("stomnum", names(out2)[-ncol(out2)])]
    
    
    
    out2$xbar <- rowMeans(out2[, -1], na.rm = TRUE)
    #xbar = mean, get a mean diversity index for each stomach that 
    #was randomized a 100 times (e.g., sum of all stomachs 1/100)
    
    out2$sd <- apply(out2[, -c(1, which(names(out2) == "xbar"))], 1, sd, na.rm = TRUE)
    ##Calculate sd of diversity index for each stomach that was randomized a 100 times
    
    PAX.f_TpDv <- out2[, c("stomnum", "xbar", "sd", setdiff(names(out2), c("stomnum", "xbar", "sd")))]
    
    # Reset row names to NULL to remove them as columns
    row.names(PAX.f_TpDv) <- NULL
    
    PAX.f_TpDv <- PAX.f_TpDv[1:3]
    
    base_dir <- "C:/Users/reyes/OneDrive/Documents/Grad school (maryland)/Research/Data/TDC.new/"
    write.csv(PAX.f_TpDv, paste0(base_dir, "PAX.f_TpDv.csv"))
    
    #PAX.f.curve <- 
    ggplot(PAX.f_TpDv, aes(x = stomnum, y = xbar)) +
      geom_line(size = 1.2) +  # Increase line thickness
      geom_ribbon(aes(ymin = xbar - sd, ymax = xbar + sd), alpha = 0.2) +
      theme_bw(base_size = 20) +  # Increase base font size
      labs(x = 'Number of stomachs', y = 'Shannon H') +  # Axis labels
      theme(axis.title = element_text(size = 22),  # Increase axis label size
            plot.title = element_text(size = 24),  # Increase plot title size
            plot.subtitle = element_text(size = 18),  # Increase plot subtitle size
            plot.caption = element_text(size = 16),  # Increase plot caption size
            plot.margin = margin(20, 20, 20, 20)) +  # Adjust plot margins
      annotate("text", x = Inf, y = -Inf, label = "<", hjust = 1.5, vjust = -3, size = 10) 

  
    
    
setwd("C:/Users/reyes/OneDrive/Documents/Grad school (maryland)/Research/Data/TDC.new/")
    
#CREATING TROPHIC DIVERSITY CURVES FOR THESIS
    PAXtdc=read.csv("PAX_TpDv.csv")
    PAX.s.tdc=read.csv("PAX.s_TpDv.csv")
    PAX.f.tdc=read.csv("PAX.f_TpDv.csv")
    MCBtdc=read.csv("MCB_TpDv.csv")
    MCB.s.tdc=read.csv("MCB.s_TpDv.csv")
    MCB.f.tdc=read.csv("MCB.f_TpDv.csv")
  
  
    MCBcurve <- ggplot(MCBtdc, aes(x = stomnum, y = xbar)) +
      geom_line(size = 1.2) +  # Increase line thickness
      geom_ribbon(aes(ymin = xbar - sd, ymax = xbar + sd), alpha = 0.2) +
      theme_bw(base_size = 20) +  # Increase base font size
      labs(x = NULL, y = NULL) +  # Remove axis labels
      theme(axis.title = element_text(size = 22),  # Increase axis label size
            plot.title = element_text(size = 24),  # Increase plot title size
            plot.subtitle = element_text(size = 18),  # Increase plot subtitle size
            plot.caption = element_text(size = 16),  # Increase plot caption size
            plot.margin = margin(20, 20, 20, 20)) +  # Adjust plot margins
      annotate("text", x = Inf, y = -Inf, label = "<0.0001", hjust = 1.5, vjust = -3, size = 10) 
    
    MCB.s.curve <- ggplot(MCB.s.tdc, aes(x = stomnum, y = xbar)) +
      geom_line(size = 1.2) +  # Increase line thickness
      geom_ribbon(aes(ymin = xbar - sd, ymax = xbar + sd), alpha = 0.2) +
      theme_bw(base_size = 20) +  # Increase base font size
      labs(x = NULL, y = NULL) +  # Remove axis labels
      theme(axis.title = element_text(size = 22),  # Increase axis label size
            plot.title = element_text(size = 24),  # Increase plot title size
            plot.subtitle = element_text(size = 18),  # Increase plot subtitle size
            plot.caption = element_text(size = 16),  # Increase plot caption size
            plot.margin = margin(20, 20, 20, 20)) +  # Adjust plot margins
      annotate("text", x = Inf, y = -Inf, label = "<0.001", hjust = 1.5, vjust = -3, size = 10) 
    
    MCB.f.curve <- ggplot(MCB.f.tdc, aes(x = stomnum, y = xbar)) +
      geom_line(size = 1.2) +  # Increase line thickness
      geom_ribbon(aes(ymin = xbar - sd, ymax = xbar + sd), alpha = 0.2) +
      theme_bw(base_size = 20) +  # Increase base font size
      labs(x = NULL, y = NULL) +  # Remove axis labels
      theme(axis.title = element_text(size = 22),  # Increase axis label size
            plot.title = element_text(size = 24),  # Increase plot title size
            plot.subtitle = element_text(size = 18),  # Increase plot subtitle size
            plot.caption = element_text(size = 16),  # Increase plot caption size
            plot.margin = margin(20, 20, 20, 20)) +  # Adjust plot margins
      annotate("text", x = Inf, y = -Inf, label = "0.071", hjust = 1.5, vjust = -1, size = 10) 
    
    PAXcurve <- ggplot(PAXtdc, aes(x = stomnum, y = xbar)) +
      geom_line(size = 1.2) +  # Increase line thickness
      geom_ribbon(aes(ymin = xbar - sd, ymax = xbar + sd), alpha = 0.2) +
      theme_bw(base_size = 20) +  # Increase base font size
      labs(x = NULL, y = NULL) +  # Remove axis labels
      theme(axis.title = element_text(size = 22),  # Increase axis label size
            plot.title = element_text(size = 24),  # Increase plot title size
            plot.subtitle = element_text(size = 18),  # Increase plot subtitle size
            plot.caption = element_text(size = 16),  # Increase plot caption size
            plot.margin = margin(20, 20, 20, 20)) +  # Adjust plot margins
      annotate("text", x = Inf, y = -Inf, label = "<0.001", hjust = 1.5, vjust = -2, size = 10) 
    
    PAX.s.curve <- ggplot(PAX.s.tdc, aes(x = stomnum, y = xbar)) +
      geom_line(size = 1.2) +  # Increase line thickness
      geom_ribbon(aes(ymin = xbar - sd, ymax = xbar + sd), alpha = 0.2) +
      theme_bw(base_size = 20) +  # Increase base font size
      labs(x = NULL, y = NULL) +  # Remove axis labels
      theme(axis.title = element_text(size = 22),  # Increase axis label size
            plot.title = element_text(size = 24),  # Increase plot title size
            plot.subtitle = element_text(size = 18),  # Increase plot subtitle size
            plot.caption = element_text(size = 16),  # Increase plot caption size
            plot.margin = margin(20, 20, 20, 20)) +  # Adjust plot margins
      annotate("text", x = Inf, y = -Inf, label = "<0.001", hjust = 1.5, vjust = -2, size = 10) 
    
    PAX.f.curve <- ggplot(PAX.f.tdc, aes(x = stomnum, y = xbar)) +
      geom_line(size = 1.2) +  # Increase line thickness
      geom_ribbon(aes(ymin = xbar - sd, ymax = xbar + sd), alpha = 0.2) +
      theme_bw(base_size = 20) +  # Increase base font size
      labs(x = NULL, y = NULL) +  # Remove axis labels
      theme(axis.title = element_text(size = 22),  # Increase axis label size
            plot.title = element_text(size = 24),  # Increase plot title size
            plot.subtitle = element_text(size = 18),  # Increase plot subtitle size
            plot.caption = element_text(size = 16),  # Increase plot caption size
            plot.margin = margin(20, 20, 20, 20)) +  # Adjust plot margins
      annotate("text", x = Inf, y = -Inf, label = "<0.005", hjust = 1.5, vjust = -2, size = 10) 
  
  
TDCs <- grid.arrange(MCBcurve, PAXcurve, MCB.s.curve, PAX.s.curve, MCB.f.curve, PAX.f.curve, nrow = 3)
  
base_dir <- "C:/Users/reyes/OneDrive/Documents/Grad school (maryland)/Research/Data/TDC.new/"  
 
ggsave(filename = file.path(base_dir, "PAX.vs.MCB.TDCs.jpg"), plot = TDCs, width = 20, height = 15, units = "in", dpi = 600)    
    
    
    
    
    
    
    
    
#RUN FROM SCRATCH
#Excluding empty stomachs
lw <- read_excel("amrd_weakfish_data.xlsx", sheet=1)
sc <- read_excel("amrd_weakfish_data.xlsx", sheet=2)

lw$TL_mm <- as.numeric(lw$TL_mm)
lw$TL_cm <- lw$TL_mm / 10

lw <- lw[!is.na(lw$TL_cm), ] 

totalmc <- nrow(subset (lw, location == 'MCB'))
totalpx <- nrow(subset (lw, location == 'PAX'))

# Filter rows where all prey columns are less than 1 (zero stomach contents)
zero_stomach_contents <- sc[(sc$mysidacea < 1 & sc$polychaete < 1 
                             & sc$unidentified_fish < 1 & sc$amphipods < 1 
                             & sc$horseshoe_crabs < 1 & sc$shrimp_larvae < 1
                             & sc$caligidae < 1 & sc$isopoda < 1
                             & sc$copepoda < 1 & sc$shrimp_not_larvae < 1 &
                               sc$parasitic_copepoda < 1), ]

# Extract the fish_id of rows with zero stomach contents
zero_stomach_fish_ids <- zero_stomach_contents$fish_id

lw <- lw[!(lw$fish_id %in% zero_stomach_fish_ids), ]

#TRIM DATA
lw <- lw %>%
  filter(TL_cm > 4.8 & TL_cm < 13.2)

totalmc <- nrow(subset (lw, location == 'MCB'))
totalpx <- nrow(subset (lw, location == 'PAX'))


lw <- lw[, c(1, 3,8)]

# Join lw into sc by fish_id
sc1 <- merge(lw, sc, by = "fish_id")

colnames(sc1)[colnames(sc1) == "location.x"] <- "location"

totalmc <- nrow(subset (sc1, location == 'MCB'))
totalpx <- nrow(subset (sc1, location == 'PAX'))


ks.test(sc1$TL_cm[lw$location == "MCB"],
        sc1$TL_cm[lw$location == "PAX"])
    
    
    
    
    
    
    
    
    
  
#DIET ANALYSIS
setwd("~/Grad school (maryland)/Research/Data")


#RUN FROM SCRATCH
#Excluding empty stomachs
lw <- read_excel("amrd_weakfish_data.xlsx", sheet=1)
sc <- read_excel("amrd_weakfish_data.xlsx", sheet=2)

lw$TL_mm <- as.numeric(lw$TL_mm)
lw$TL_cm <- lw$TL_mm / 10

lw <- lw[!is.na(lw$TL_cm), ] 

totalmc <- nrow(subset (lw, location == 'MCB'))
totalpx <- nrow(subset (lw, location == 'PAX'))

# Filter rows where all prey columns are less than 1 (zero stomach contents)
zero_stomach_contents <- sc[(sc$mysidacea < 1 & sc$polychaete < 1 
                             & sc$unidentified_fish < 1 & sc$amphipods < 1 
                             & sc$horseshoe_crabs < 1 & sc$shrimp_larvae < 1
                             & sc$caligidae < 1 & sc$isopoda < 1
                             & sc$copepoda < 1 & sc$shrimp_not_larvae < 1 &
                               sc$parasitic_copepoda < 1), ]

# Extract the fish_id of rows with zero stomach contents
zero_stomach_fish_ids <- zero_stomach_contents$fish_id

lw <- lw[!(lw$fish_id %in% zero_stomach_fish_ids), ]

#TRIM DATA
lw <- lw %>%
  filter(TL_cm > 4.8 & TL_cm < 13.2)

totalmc <- nrow(subset (lw, location == 'MCB'))
totalpx <- nrow(subset (lw, location == 'PAX'))


lw <- lw[, c(1, 3)]
  
# Join lw into sc by fish_id
sc1 <- merge(lw, sc, by = "fish_id")

colnames(sc1)[colnames(sc1) == "location.x"] <- "location"

totalmc <- nrow(subset (sc1, location == 'MCB'))
totalpx <- nrow(subset (sc1, location == 'PAX'))


#ks.test(FI$TL_cm[lw$location == "MCB"],
#        FI$TL_cm[lw$location == "PAX"])
#Preparing data for %F and %N  

sc2 <- sc1[, -c( 3:5)]
#sc3 <- gather(sc2, key = "Fish_id", value = "Prey", -location)

sc3 <- pivot_longer(sc2, cols = -c(fish_id, location), names_to = "prey", values_to = "amount")

#sc3 <- sc3[!(sc3$prey %in% c(".", ".")), ]

sc3$Location <- sc3$location

#Join horseshoe crbas, shrimp not larvae and isopods (Uncommon prey, can name it other crustaceans)
#Join caligidae and parasitic copeods (name as Parasites)
sc3$prey <- dplyr::case_when(
  sc3$prey == "polychaete" ~ "Polychaeta",
  sc3$prey == "amphipods" ~ "Amphipoda",
  sc3$prey %in% c("horseshoe_crabs", "shrimp_not_larvae", "isopoda") ~ "other crustaceans",
  sc3$prey %in% c("caligidae", "parasitic_copepoda") ~ "parasites",
  sc3$prey == "unidentified_fish" ~ "unidentified fish",
  sc3$prey == "shrimp_larvae" ~ "shrimp larvae",
  sc3$prey == "mysidacea" ~ "Mysidacea",
  sc3$prey == "copepoda" ~ "Copepoda",
  TRUE ~ sc3$prey)  # Leave unchanged if no condition matches                                                                                                                                                         +                                                sc3$prey))))))


# % NUMBER

sc4 <- sc3 %>%
  group_by(prey, Location) %>%
  summarise(
    Tamount = sum(amount, na.rm = TRUE),
    n = ifelse(Location == "MCB", 110, 115),
    sd_amount = sd(amount, na.rm = TRUE),
    se_amount = sd_amount / sqrt(n),
    lower_ci = Tamount - qt(0.975, df = n - 1) * se_amount,
    upper_ci = Tamount + qt(0.975, df = n - 1) * se_amount,
    .groups = "drop"
  ) %>%
  distinct() %>%
  group_by(Location) %>%                # Group by Location for summing
  mutate(total = sum(Tamount))


sc41 <- sc4[, -c(4:6)] 


sc4 <- sc41 %>%
  dplyr::group_by(Location) %>%
  mutate(
    total = sum(Tamount),                      # Calculate total Tamount by Location
    perc.n = (Tamount / total) * 100,          # Calculate percentage for Tamount
    upper_ci_perc = (upper_ci / total) * 100,  # Calculate percentage for upper CI
    lower_ci_perc = (lower_ci / total) * 100   # Calculate percentage for lower CI
  )


ggplot(sc4, aes(x = prey, y = perc.n, fill = Location)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.9) +  # Set bar width
  geom_errorbar(
    aes(
      ymin = lower_ci_perc,  # Lower confidence interval
      ymax = upper_ci_perc   # Upper confidence interval
    ),
    position = position_dodge(width = 0.9),  # Align error bars with bars
    width = 0.9,  # Match the width of the bars
    color = "red"  # Set error bars to red
  ) +
  labs(x = "Prey", y = "Number (%)", title = "") +
  scale_fill_manual(values = c("MCB" = "black", "PAX" = "gray")) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 30),  # Increase text size for both axes
    axis.title = element_text(size = 34),  # Increase title size
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  # Rotate x-axis labels
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.grid.major.y = element_line(color = "gray", linetype = "solid"),
    legend.title = element_text(size = 34),  # Increase legend title size
    legend.text = element_text(size = 28),   # Increase legend text size
    plot.title = element_text(size = 38, face = "bold", hjust = 0.5)  # Center and bold title
  )

         
# % OCCURRENCE

#Work on sc3 again

sc3$occurrence <- ifelse(sc3$amount > 0, 1, 0)

sc5 <- sc3 %>%
  group_by(prey, Location) %>%
  summarise(
    prey.occurrence = sum(occurrence, na.rm = TRUE),  # Sum of occurrences
    n = ifelse(Location == "MCB", 110, 115),         # Number of weakfish per location
    sd_occurrence = sd(occurrence, na.rm = TRUE),    # Standard deviation
    se_occurrence = sd_occurrence / sqrt(n),         # Standard error
    lower_ci = prey.occurrence - qt(0.975, df = n - 1) * se_occurrence, # Lower CI
    upper_ci = prey.occurrence + qt(0.975, df = n - 1) * se_occurrence, # Upper CI
    .groups = "drop"
  ) %>%
  distinct() 

sc5 <- sc5 %>%
  dplyr::group_by(Location) %>%
  mutate(
    perc.o = (prey.occurrence / n) * 100,  # Calculate % occurrence
    lower_ci_perc.o = (lower_ci / n) * 100,  # Lower CI for % occurrence
    upper_ci_perc.o = (upper_ci / n) * 100   # Upper CI for % occurrence
  )

sc6 <- sc5[, -c(3:8)] 


ggplot(sc6, aes(x = prey, y = perc.o, fill = Location)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.9) +  # Bar plot with adjusted width
  geom_errorbar(aes(ymin = lower_ci_perc.o, ymax = upper_ci_perc.o),
                position = position_dodge(width = 0.9), width = 0.9, color = "red") +  # Error bars styled red
  labs(x = "Prey", y = "Occurrence (%)", title = "") +
  scale_fill_manual(values = c("MCB" = "black", "PAX" = "gray")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 30),  # Increase text size for both axes
        axis.title = element_text(size = 34),  # Increase title size
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  # Rotate x-axis labels
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.grid.major.y = element_line(color = "gray", linetype = "solid"),
        legend.title = element_text(size = 34),  # Increase legend title size
        legend.text = element_text(size = 28),   # Increase legend text size
        plot.title = element_text(size = 38, face = "bold", hjust = 0.5))  # Increase title size, make it bold, and center it



















#STABLE ISOTOPE ANALYSIS

setwd("C:/Users/reyes/OneDrive/Documents/Grad school (maryland)/Research/Data")

#RUN FROM SCRATCH
#Excluding empty stomachs
lw <- read_excel("amrd_weakfish_data.xlsx", sheet=1)
sc <- read_excel("amrd_weakfish_data.xlsx", sheet=2)
SI <- read_excel("amrd_weakfish_data.xlsx", sheet=3)
siPax <- read_excel("amrd_PAX_iso.xlsx")

lw$TL_mm <- as.numeric(lw$TL_mm)
lw$TL_cm <- lw$TL_mm / 10

lw <- lw[!is.na(lw$TL_cm), ] 

totalmc <- nrow(subset (lw, location == 'MCB'))
totalpx <- nrow(subset (lw, location == 'PAX'))

# Filter rows where all prey columns are less than 1 (zero stomach contents)
zero_stomach_contents <- sc[(sc$mysidacea < 1 & sc$polychaete < 1 
                             & sc$unidentified_fish < 1 & sc$amphipods < 1 
                             & sc$horseshoe_crabs < 1 & sc$shrimp_larvae < 1
                             & sc$caligidae < 1 & sc$isopoda < 1
                             & sc$copepoda < 1 & sc$shrimp_not_larvae < 1 &
                               sc$parasitic_copepoda < 1), ]

# Extract the fish_id of rows with zero stomach contents
zero_stomach_fish_ids <- zero_stomach_contents$fish_id

lw <- lw[!(lw$fish_id %in% zero_stomach_fish_ids), ]

totalmc <- nrow(subset (lw, location == 'MCB'))
totalpx <- nrow(subset (lw, location == 'PAX'))

SI <- SI [, -c( 2, 4:6)]

colnames(SI)[3] <- "Carbon"
colnames(SI)[4] <- "Nitrogen.Per"

#Merge
SIA <- merge(SI, lw[, c("fish_id", "TL_cm")], by = "fish_id", all.x = TRUE)

SIA <- SIA[!is.na(SIA$TL_cm), ]

#TRIM DATA
SIA <- SIA %>%
  filter(TL_cm > 4.8 & TL_cm < 13.2)

totalmc <- nrow(subset (SIA, location == 'MCB'))
totalpx <- nrow(subset (SIA, location == 'PAX'))

ks.test(SIA$TL_cm[SIA$location == "MCB"],
        SIA$TL_cm[SIA$location == "PAX"])

#Plot C vs N
ggplot(SIA, aes(x = Carbon, y = Nitrogen.Per, col = location)) +
  geom_point() +
  xlab("Carbon") +
  ylab("Nitrogen") +
  ggtitle("Carbon vs Nitrogen between locations")

#4 outliers (2 from each location). Remove?
SIA <- SIA %>%
  filter(
    !(location == "PAX" & Nitrogen.Per < 15),  # Remove PAX values where Nitrogen_Per < 15
    !(location == "MCB" & Nitrogen.Per > 15)  # Remove MCBs values where Nitrogen_Per > 15
  )

totalmc <- nrow(subset (SIA, location == 'MCB'))
totalpx <- nrow(subset (SIA, location == 'PAX'))

ks.test(SIA$TL_cm[SIA$location == "MCB"],
        SIA$TL_cm[SIA$location == "PAX"])


SIA <- SIA %>%
  group_by(location) %>%
  mutate(
    mean_Nitrogen_Per = mean(Nitrogen.Per, na.rm = TRUE),
    min_Nitrogen_Per = min(Nitrogen.Per, na.rm = TRUE),
    max_Nitrogen_Per = max(Nitrogen.Per, na.rm = TRUE),
    mean_carbon = mean(Carbon, na.rm = TRUE),
    min_carbon = min(Carbon, na.rm = TRUE),
    max_carbon = max(Carbon, na.rm = TRUE)
  ) 

#Add column for category
SIA <- SIA %>%
 mutate(Cat = case_when(
    location == "PAX" ~ "WF_PAX",
   location == "MCB" ~ "WF_MCB",
    TRUE ~ "Other"  # Handle other cases, if any
  ))

summary_si <- siPax %>%
  summarise(
    mean_d13C = mean(d13C, na.rm = TRUE),
    min_d13C = min(d13C, na.rm = TRUE),
    max_d13C = max(d13C, na.rm = TRUE),
    mean_d15N = mean(d15N, na.rm = TRUE),
    min_d15N = min(d15N, na.rm = TRUE),
    max_d15N = max(d15N, na.rm = TRUE)
  )

#Standard error
summary_se <- SIA %>%
  group_by(location) %>%
  summarise(
    mean_Carbon = mean(Carbon, na.rm = TRUE),
    se_Carbon = sd(Carbon, na.rm = TRUE) / sqrt(n()),
    mean_Nitrogen = mean(Nitrogen.Per, na.rm = TRUE),
    se_Nitrogen = sd(Nitrogen.Per, na.rm = TRUE) / sqrt(n())
  )

#ADDING SPOM to the MATH
new_rows <- data.frame(
  fish_id = c("SPOM", "SPOM"),
  location = c("MCB", "PAX"),
  Carbon = c(-23.55, -25.75),
  Nitrogen.Per = c(5.7, 10.91),
  TL_cm = c(0,0),
  mean_Nitrogen_Per = c(5.7, 10.91),
  min_Nitrogen_Per = c(5.7, 10.91),
  max_Nitrogen_Per = c(5.7, 10.91),
  mean_carbon = c(-23.55, -25.75),
  min_carbon = c(-26.80, -27.17),
  max_carbon = c(-20.30, -24.20),
  Cat = c("SPOM_MCB", "SPOM_PAX"))

#SIA <- SIA [, -c(5)]

# Convert TL_cm in SIA to numeric (if appropriate)
SIA$TL_cm <- as.numeric(as.character(SIA$TL_cm))

# Convert TL_cm in new_rows to numeric (if appropriate)
new_rows$TL_cm <- as.numeric(as.character(new_rows$TL_cm))

SIA <- rbind(SIA, new_rows)

#See all categories and values
SIA_unique <- SIA %>%
  distinct(location, .keep_all = TRUE)

#Plot mean C vs mean N
ggplot(SIA, aes(x = mean_carbon, y = mean_Nitrogen_Per, shape = location, color = Cat)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = min_Nitrogen_Per, ymax = max_Nitrogen_Per), width = 0.2) +  # Add y-axis error bars
  geom_errorbarh(aes(xmin = min_carbon, xmax = max_carbon), height = 0.7) +  # Add x-axis error bars
  xlab("δ13C") +
  ylab("δ15N") +
  ggtitle("") +
  scale_x_continuous(limits = c(-27.5, -18), breaks = seq(-28, -16, by = 2)) +  # Set x-axis limits and breaks
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) +  # Set y-axis limits and breaks
  theme_minimal() +
  theme(
    axis.text = element_text(size = 40),  # Adjust axis text size
    axis.title = element_text(size = 40),  # Adjust axis title size
    legend.title = element_text(size = 40),  # Adjust legend title size
    legend.text = element_text(size = 40),   # Adjust legend text size
    plot.title = element_text(size = 38, face = "bold", hjust = 0.5),  # Adjust title size, make it bold, and center it
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add plot borders
    axis.ticks.length = unit(0.1, "cm"),  # Set axis ticks length
    axis.ticks = element_line(size = 1)  # Set thickness of axis http://127.0.0.1:45211/graphics/plot_zoom_png?width=1487&height=903ticks
  )


# PLOT MEAN C VS MEAN N FOR DRAFT
ggplot(SIA, aes(x = mean_carbon, y = mean_Nitrogen_Per, shape = location, color = Cat)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = min_Nitrogen_Per, ymax = max_Nitrogen_Per), width = 0.2) +  # Add y-axis error bars
  geom_errorbarh(aes(xmin = min_carbon, xmax = max_carbon), height = 0.7) +  # Add x-axis error bars
 # geom_errorbar(data = SIA %>% filter(Cat %in% c("WF_PAX", "WF_MCB")), 
      #          aes(ymin = min_Nitrogen_Per, ymax = max_Nitrogen_Per), width = 0.2) +  # Add y-axis error bars excluding "SPOM"
 # geom_errorbarh(data = SIA %>% filter(Cat %in% c("WF_PAX", "WF_MCB")), 
       #          aes(xmin = min_carbon, xmax = max_carbon), height = 0.8) +  # Add x-axis error bars excluding "SPOM"
  xlab("δ13C") +
  ylab("δ15N") +
  ggtitle("") +
  scale_x_continuous(limits = c(-27.5, -18), breaks = seq(-28, -16, by = 2)) +  # Set x-axis limits and breaks
  scale_y_continuous(limits = c(0, 19), breaks = seq(0, 20, by = 5)) +  # Set y-axis limits and breaks
  scale_color_manual(values = c("WF_MCB" = "red", "WF_PAX" = "blue", "SPOM_PAX" = "purple", "SPOM_MCB" = "green")) +  # Define colors
  theme_minimal() +
  theme(
    axis.text = element_text(size = 35),  # Adjust axis text size
    axis.title = element_text(size = 35),  # Adjust axis title size
    legend.position = "none",  # Remove legend
    plot.title = element_text(size = 38, face = "bold", hjust = 0.5),  # Adjust title size, make it bold, and center it
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add plot borders
    axis.ticks.length = unit(0.1, "cm"),  # Set axis ticks length
    axis.ticks = element_line(size = 1)  # Set thickness of axis ticks
  )
  

SIA_test <- SIA %>% 
  filter(fish_id != "SPOM")




# IS D15N INCREASING WITH SIZE?
ggplot(SIA_test, aes(x = TL_cm, y = Nitrogen.Per, shape = location, color = location)) +
  geom_point(size = 7) +
  xlab("TL (cm)") +
  ylab("δ15N") +
  ggtitle("") +
  scale_color_manual(values = c("MCB" = "red", "PAX" = "blue")) +  # Set MCB to red and PAX to blue
  theme_minimal() +
  theme(
    axis.text = element_text(size = 35),  # Adjust axis text size
    axis.title = element_text(size = 35),  # Adjust axis title size
    legend.position = "none",  # Remove legend
    plot.title = element_text(size = 38, face = "bold", hjust = 0.5),  # Adjust title size, make it bold, and center it
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add plot borders
    axis.ticks.length = unit(0.1, "cm"),  # Set axis ticks length
    axis.ticks = element_line(size = 1)  # Set thickness of axis ticks
  )

# IS D13C GETTING DEPLETED WITH SIZE?
ggplot(SIA_test, aes(x = TL_cm, y = Carbon, shape = location, color = location)) +
  geom_point(size = 7) +
  xlab("TL (cm)") +
  ylab("δ13C") +
  ggtitle("") +
  scale_color_manual(values = c("MCB" = "red", "PAX" = "blue")) +  # Set MCB to red and PAX to blue
  theme_minimal() +
  theme(
    axis.text = element_text(size = 35),  # Adjust axis text size
    axis.title = element_text(size = 35),  # Adjust axis title size
    legend.position = "none",  # Remove legend
    plot.title = element_text(size = 38, face = "bold", hjust = 0.5),  # Adjust title size, make it bold, and center it
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add plot borders
    axis.ticks.length = unit(0.1, "cm"),  # Set axis ticks length
    axis.ticks = element_line(size = 1)  # Set thickness of axis ticks
  )

SIA_test2 <- SIA_test[2:5]

base_dir1 <- "C:/Users/reyes/OneDrive/Documents/Grad school (maryland)/Research/Data/"

#SUBSET FOR PAX
SIA_PAX <- SIA_test2 %>%
  filter(location == "PAX")

write.csv(SIA_PAX, paste0(base_dir1, "SIA_PAX.csv"), row.names = FALSE)

# Subset the SIA_test dataset for location MCB
SIA_MCB <- SIA_test2 %>%
  filter(location == "MCB")

write.csv(SIA_MCB, paste0(base_dir1, "SIA_MCB.csv"), row.names = FALSE)







setwd("C:/Users/reyes/OneDrive/Documents/Grad school (maryland)/Research/Data/")

# Spearman's correlation test for PAX

SIA_PAX <- read.csv("SIA_PAX.csv")

cor.test(SIA_PAX$TL_cm, SIA_PAX$Nitrogen.Per, method = "spearman")

cor.test(SIA_PAX$TL_cm, SIA_PAX$Carbon, method = "spearman")

PAXreg <- lm(Nitrogen.Per ~ TL_cm, data = SIA_PAX)
summary (PAXreg)


#Pearson just in case
#cor.test(SIA_PAX$TL_cm, SIA_PAX$Nitrogen.Per, method = "pearson")

#cor.test(SIA_PAX$TL_cm, SIA_PAX$Carbon, method = "pearson")


# Spearman's correlation test foor MCBs
SIA_MCB <- read.csv("SIA_MCB.csv")

cor.test(SIA_MCB$TL_cm, SIA_MCB$Nitrogen.Per, method = "spearman")

cor.test(SIA_MCB$TL_cm, SIA_MCB$Carbon, method = "spearman")

MCBreg <- lm(Nitrogen.Per ~ TL_cm, data = SIA_MCB)
summary(MCBreg)


#Pearson just in case
#cor.test(SIA_MCB$TL_cm, SIA_MCB$Nitrogen.Per, method = "pearson")

#cor.test(SIA_MCB$TL_cm, SIA_MCB$Carbon, method = "pearson")











#CALCULATE Trophic Level

#COASTAL BAYS
#For Carbon -26.8 to -20.3 (SPOM), for Nitrogen 5.7 +- 0.2. 

#Edge and Chigbu 2021

#PATUXENT RIVER
#For Carbon is 2.69 +- 0.54 (SPOM, BOM) and for Nitrogen is 3.55 +- 1.19 (Zoop)

#Quillen, K., Santos, N., Testa, J.M. and Woodland, R.J., 2022. Coastal hypoxia
#reduces trophic resource coupling and alters niche characteristics of an ecologically 
#dominant omnivore. Food Webs, 33, p.e00252.

#CALCULATING TROPHIC LEVEL
SIA$TL <- 1

SIA <- SIA %>%
  mutate(TL = case_when(
    Cat == "SPOM_MCB" ~ 1,
    Cat == "SPOM_PAX" ~ 1.5,
    location == "MCB" ~ 1 + ((Nitrogen.Per/5.7) / 3.4),
    location == "PAX" ~ 1.5 + ((Nitrogen.Per/2.69) / 3.4),
    TRUE ~ TL  # Keep TL unchanged for other locations and Cats
  ))


# Calculate mean, min and max for TL and Carbon by location and Cat
SIA_summ2 <- SIA %>%
  group_by(location, Cat) %>%
  summarise(mean_TL = mean(TL, na.rm = TRUE),      # Calculate mean TL
            min_TL = min(TL, na.rm = TRUE),        # Calculate min TL
            max_TL = max(TL, na.rm = TRUE),        # Calculate max TL
            mean_Carbon = mean(Carbon, na.rm = TRUE),  # Calculate mean Carbon
            min_Carbon = min(Carbon, na.rm = TRUE),    # Calculate min Carbon
            max_Carbon = max(Carbon, na.rm = TRUE)     # Calculate max Carbon
  )

# Modify min_Carbon and max_Carbon to 0 if Cat is SPOM
SIA_summ2 <- SIA_summ2 %>%
  mutate(min_Carbon = ifelse(grepl("SPOM", Cat), 0, min_Carbon),
         max_Carbon = ifelse(grepl("SPOM", Cat), 0, max_Carbon))

SIA_summ2$min_Carbon <- as.numeric(SIA_summ2$min_Carbon)
SIA_summ2$max_Carbon <- as.numeric(SIA_summ2$max_Carbon)

# Plot mean, min and max Carbon vs TL by location

ggplot(SIA_summ2, aes(x = mean_Carbon, y = mean_TL, shape = location, color = Cat)) +
  geom_point(size = 3) +
  geom_errorbar(data = SIA_summ2 %>% filter(Cat %in% c("WF_PAX", "WF_MCB")),
                aes(ymin = min_TL, ymax = max_TL), width = 0.2) +  # Add y-axis error bars using min and max TL for WF_PAX and WF_MCB
  geom_errorbarh(data = SIA_summ2 %>% filter(Cat %in% c("WF_PAX", "WF_MCB")),
                 aes(xmin = min_Carbon, xmax = max_Carbon), height = 0.2) +  # Add x-axis error bars using min and max Carbon for WF_PAX and WF_MCB
  xlab("13C") +
  ylab("TL") +
  ggtitle("") +
  scale_x_continuous(limits = c(-27, -18), breaks = seq(-28, -18, by = 2)) +  # Set x-axis limits and breaks
  scale_y_continuous(limits = c(0.5, 4), breaks = seq(0, 4, by = 1)) +  # Set y-axis limits and breaks
  theme_minimal() +
  theme(
    axis.text = element_text(size = 20),  # Make axis text smaller
    axis.title = element_text(size = 20),  # Enlarge axis titles
    legend.title = element_text(size = 34),  # Enlarge legend title size
    legend.text = element_text(size = 28),   # Enlarge legend text size
    plot.title = element_text(size = 38, face = "bold", hjust = 0.5),  # Enlarge title size, make it bold, and center it
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add plot borders
    axis.ticks.length = unit(0.1, "cm"),  # Set axis ticks length
    axis.ticks = element_line(size = 1)  # Set thickness of axis ticks
  )









































































































































#Size distribution by month

# Add a new column 'Month' based on values in the 'date' column
lw2$Month <- ifelse(grepl("Jul", lw2$date), "July", 
                    ifelse(grepl("Aug", lw2$date), "August", 
                           ifelse(grepl("Sep", lw2$date), "September", NA)))

# Ensure the 'Month' column is ordered as July, August, September
lw2$Month <- factor(lw2$Month, levels = c("July", "August", "September"))

#Testing if size distributions are different for each month
ggplot(lw2, aes(x = location, y = TL_cm, fill = location)) +
  geom_boxplot() +
  facet_wrap(~ Month) +  # Separate by month
  labs(x = "Location", y = "Total Length (cm)", title = "Fish Size Distribution by Location and Month") +
  theme_minimal()



#TRIM FOR MONTH AND AREA, GET SAMPLE SIZE FOR EACH ONE
lw3 <- lw2 %>%
  filter(Month != "July")

#Testing if size distributions are different for each month
ggplot(lw3, aes(x = location, y = TL_cm, fill = location)) +
  geom_boxplot() +
  facet_wrap(~ Month) +  # Separate by month
  labs(x = "Location", y = "Total Length (cm)", title = "Fish Size Distribution by Location and Month") +
  theme_minimal()

aug <- lw3 %>%
  filter(Month == "August") %>%
  filter(TL_cm > 7.4 & TL_cm < 12)

ggplot(aug, aes(x = location, y = TL_cm, fill = location)) +
  geom_boxplot() +
  labs(x = "Location", y = "Total Length (cm)", title = "Fish Size Distribution by Location and Month") +
  theme_minimal()

sep <- lw3 %>%
  filter(Month == "September") %>%
  filter(TL_cm > 8.1)

#TEST FOR SIZE DISTR. FOR AREA/MONTH
ks_test_aug <- ks.test(aug$TL_cm[aug$location == "MCB"],
                       aug$TL_cm[aug$location == "PAX"]) 
ks_test_aug


#TEST FOR SIZE DISTR. FOR AREA/MONTH
ks_test_sep <- ks.test(sep$TL_cm[sep$location == "MCB"],
                       sep$TL_cm[sep$location == "PAX"])
ks_test_sep



totalmc3 <- nrow(subset (aug, location == 'MCB')) # N = 27
totalmc4 <- nrow(subset (sep, location == 'MCB')) # N = 12

totalpax3 <- nrow(subset (aug, location == 'PAX')) # N = 24
totalpax4 <- nrow(subset (sep, location == 'PAX')) # N = 42

#NOT ENOUGH DATA TO ANALYZE BY AREA AND MONTH




















#SKIP FOR NOW
#Weight, do it for mysids and maybe crustaceans. 

W1 <- sc[, c(1, 2, 5, 8, 12)]
forW <- FI[, c(1, 6)]

W <- merge(W1, forW, by = "fish_id")
W <- W[, -c(1)]

#Amount sum by location
sum_by_location <- aggregate(. ~ location, data = W, sum)

#Mysids
mysid = 303.6*1e-6

mysid_wt <- with(sum_by_location, ifelse(location == 'MCB', mysidacea*mysid, mysidacea))
mysid_wt2 <- with(sum_by_location, ifelse(location == 'PAX', mysidacea*mysid, mysidacea))

sum_by_location <- sum_by_location %>%
  group_by(location) %>%
  mutate(
    perc.w.mysid = ifelse(row_number() == 1, (mysid_wt / prey_wt) * 100, NA),
    perc.w.mysid = ifelse(row_number() == 2, (mysid_wt2 / prey_wt) * 100, perc.w.mysid)
  ) %>%
  ungroup()

#Copepods
copepod = 10*1e-6

cope_wt <- with(sum_by_location, ifelse(location == 'MCB', copepoda*copepod, copepoda))
cope_wt2 <- with(sum_by_location, ifelse(location == 'PAX', copepoda*copepod, copepoda))

sum_by_location <- sum_by_location %>%
  group_by(location) %>%
  mutate(
    perc.w.cope = ifelse(row_number() == 1, (cope_wt / prey_wt) * 100, NA),
    perc.w.cope = ifelse(row_number() == 2, (cope_wt2 / prey_wt) * 100, perc.w.cope)
  ) %>%
  ungroup()

#Amphipods
amphipod = 102.2*1e-6

amph_wt <- with(sum_by_location, ifelse(location == 'MCB', amphipods*amphipod, amphipods))
amph_wt2 <- with(sum_by_location, ifelse(location == 'PAX', amphipods*amphipod, amphipods))

sum_by_location <- sum_by_location %>%
  group_by(location) %>%
  mutate(
    perc.w.amph = ifelse(row_number() == 1, (amph_wt / prey_wt) * 100, NA),
    perc.w.amph = ifelse(row_number() == 2, (amph_wt2 / prey_wt) * 100, perc.w.amph)
  ) %>%
  ungroup()


#FOR OTHER PREY DO LOGISTIC REGRESSION
#RUN FROM SCRATCH

setwd("C:/Users/reyes/OneDrive/Documents/Grad school (maryland)/Research/Data")
sc <- read_excel("amrd_weakfish_data.xlsx", sheet=2)

lw <- read_excel("amrd_weakfish_data.xlsx", sheet=1)
lw1 <- lw[, c( 1, 6)]

#Merging fish length with stomach content
lgr <- merge(sc, lw1, by = "fish_id")
lgr <- lgr[, -c( 3, 4)]

#Putting all prey in 1 column and changing names and values accordingly
lgr1 <- lgr%>%
  pivot_longer(cols = -c(fish_id, location, TL_mm),
               names_to = "Species", values_to = "prescence.abscence") %>%
  mutate(prescence.abscence = ifelse(prescence.abscence > 0, 1, 0))

# Split the data by species
subsetted_data <- split(lgr1, lgr1$Species)
lgr1$TL_mm <- as.numeric(lgr1$TL_mm)

# Create separate variables for each subsetted data frame
species_names <- names(subsetted_data)
for (i in seq_along(subsetted_data)) {
  species <- species_names[i]
  assign(paste0("lgr_", species), subsetted_data[[i]])
}

#Fitting GLMs for each prey

model_mysid <- glm(prescence.abscence ~ TL_mm, data = lgr_mysidacea, family = binomial)
model_amphi <- glm(prescence.abscence ~ TL_mm, data = lgr_amphipods, family = binomial)
model_cali <- glm(prescence.abscence ~ TL_mm, data = lgr_caligidae, family = binomial)
model_cope <- glm(prescence.abscence ~ TL_mm, data = lgr_copepoda, family = binomial)
model_parasi <- glm(prescence.abscence ~ TL_mm, data = lgr_parasitic_copepoda, family = binomial)
model_shri <- glm(prescence.abscence ~ TL_mm, data = lgr_shrimp_larvae, family = binomial)
model_fish <- glm(prescence.abscence ~ TL_mm, data = lgr_unidentified_fish, family = binomial)
model_poly <- glm(prescence.abscence ~ TL_mm, data = lgr_polychaete, family = binomial)

# Create a data frame for plotting each prey
plot_data_amphi <- data.frame(TL_mm = lgr_amphipods$TL_mm, predicted_prob = predict(model_amphi, type = "response"))
plot_data_mysid <- data.frame(TL_mm = lgr_mysidacea$TL_mm, predicted_prob = predict(model_mysid, type = "response"))
plot_data_fish <- data.frame(TL_mm = lgr_unidentified_fish$TL_mm, predicted_prob = predict(model_fish, type = "response"))
plot_data_poly <- data.frame(TL_mm = lgr_polychaete$TL_mm, predicted_prob = predict(model_poly, type = "response"))
plot_data_cope <- data.frame(TL_mm = lgr_copepoda$TL_mm, predicted_prob = predict(model_cope, type = "response"))
plot_data_shri <- data.frame(TL_mm = lgr_shrimp_larvae$TL_mm, predicted_prob = predict(model_shri, type = "response"))
plot_data_cali <- data.frame(TL_mm = lgr_caligidae$TL_mm, predicted_prob = predict(model_cali, type = "response"))
plot_data_parasi <- data.frame(TL_mm = lgr_parasitic_copepoda$TL_mm, predicted_prob = predict(model_parasi, type = "response"))

#LOGISTIC REGRESSION PLOTS

#Start to look when they are getting more into piscivory in my data
#DO IT SEPARATELY FOR EACH LOCATION WHEN I GOT THE FULL DATASET

# Logistic regression plot for unidentified fish
ggplot(lgr_unidentified_fish, aes(x = TL_mm, y = prescence.abscence, col = location)) +
  geom_point() +
  geom_line(data = plot_data_fish, aes(y = predicted_prob), color = "blue") +
  ylab("Probability of Presence") +
  xlab("TL_mm") +
  ggtitle("Logistic Regression for Unidentified Fish") +
  theme_minimal()

# Logistic regression plot for copepoda
ggplot(lgr_copepoda, aes(x = TL_mm, y = prescence.abscence, col = location)) +
  geom_point() +
  geom_line(data = plot_data_cope, aes(y = predicted_prob), color = "blue") +
  ylab("Probability of Presence") +
  xlab("TL_mm") +
  ggtitle("Logistic Regression for Copepoda") +
  theme_minimal()

# Logistic regression plot for amphipods
ggplot(lgr_amphipods, aes(x = TL_mm, y = prescence.abscence)) +
  geom_point() +
  geom_line(data = plot_data_amphi, aes(y = predicted_prob), color = "blue") +
  ylab("Probability of Presence") +
  xlab("TL_mm") +
  ggtitle("Logistic Regression for Amphipods") +
  theme_minimal()

# Logistic regression plot for mysids
ggplot(lgr_mysidacea, aes(x = TL_mm, y = prescence.abscence, col = location)) +
  geom_point() +
  geom_line(data = plot_data_mysid, aes(y = predicted_prob), color = "blue") +
  ylab("Probability of Presence") +
  xlab("TL_mm") +
  ggtitle("Logistic Regression for Mysids") +
  theme_minimal()

# Logistic regression plot for polychaete
ggplot(lgr_polychaete, aes(x = TL_mm, y = prescence.abscence, col= location)) +
  geom_point() +
  geom_line(data = plot_data_poly, aes(y = predicted_prob), color = "blue") +
  ylab("Probability of Presence") +
  xlab("TL_mm") +
  ggtitle("Logistic Regression for Polychaete") +
  theme_minimal()


# Logistic regression plot for shrimp larvae
ggplot(lgr_shrimp_larvae, aes(x = TL_mm, y = prescence.abscence)) +
  geom_point() +
  geom_line(data = plot_data_shri, aes(y = predicted_prob), color = "blue") +
  ylab("Probability of Presence") +
  xlab("TL_mm") +
  ggtitle("Logistic Regression for Shrimp Larvae") +
  theme_minimal()
