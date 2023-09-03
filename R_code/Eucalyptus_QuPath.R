
# R code for data analysis and visualization of quantitative wood anatomy data generated in QuPath for Eucalyptus grandis
 # Paper title: "An open-source machine-learning approach for obtaining high quality quantitative wood anatomy data in E. grandis and P. radiata."
 # Journal: Applications in Plant Sciences
 # Author: Rafael Keret
  
# IMPORTING DATA

# (1) Load packages

library(tidyverse)

# (2) Import QuPath raw (.csv) data file and indicate comma (",") delimiter to separate the data into columns

Auto_data <- read.table("./Data/Appendix_S2.csv", sep = ",", skip = 3, header = T)
Manual_data <- read.table("./Data/Appendix_S3.csv", sep = ",", skip = 3, header = T)

Auto_data$Class <- gsub(": Positive", "", Auto_data$Class)
Manual_data$Class <- gsub(": Positive", "", Manual_data$Class)

# (3) Calculating overall mean and standard deviation for cell area (CA), lumen area (LA) and lumen to cell area ratio (LC) 

Grouped_auto <- Auto_data %>% 
  group_by(Class) %>%
  summarise(CA_Mean_Auto = mean(Cell..Area), 
            LA_Mean_Auto = mean(Nucleus..Area), 
            LC_Mean_Auto = mean(Nucleus.Cell.area.ratio), 
            CA_SD_Auto = sd(Cell..Area), 
            LA_SD_Auto = sd(Nucleus..Area), 
            LC_SD_Auto = sd(Nucleus.Cell.area.ratio))

Grouped_manual <- Manual_data %>% 
  group_by(Class) %>%
  summarise(CA_Mean_Manual = mean(Cell..Area), 
            LA_Mean_Manual = mean(Nucleus..Area), 
            LC_Mean_Manual = mean(Nucleus.Cell.area.ratio), 
            CA_SD_Manual = sd(Cell..Area), 
            LA_SD_Manual = sd(Nucleus..Area), 
            LC_SD_Manual = sd(Nucleus.Cell.area.ratio))

# HISTOGRAM

# (1) Omit manually curated detections (manual curations do not have ": Positive" in the "Name")

Curated_data <- Manual_data[grep(": Positive", Manual_data$Name),]

# (2) Generating a counts matrix for cell types

Counts_curated <- Curated_data %>% 
  group_by(Class) %>%
  count() %>% 
  ungroup()

Error_data <- anti_join(Auto_data, Curated_data, by = c("Cell..Area", "Nucleus..Area", "Nucleus.Cell.area.ratio"))

Counts_error <- Error_data %>% 
  group_by(Class) %>%
  count() %>% 
  ungroup()

# (3) Rename the curated cell types for graphing

Counts_error$Class <- gsub("Vessels", "Curated vessels", Counts_error$Class)
Counts_error$Class <- gsub("Fibers", "Curated fibers", Counts_error$Class)
Counts_error$Class <- gsub("Parenchyma", "Curated parenchyma", Counts_error$Class)
Counts_error$Class <- gsub("Misdetection", "Curated misdetection", Counts_error$Class)

Counts_combined <- rbind(Counts_curated, Counts_error) 

Counts_combined <- Counts_combined %>% 
  mutate(Counts_combined, prop = n/sum(n)) %>% 
  mutate(perc = scales::percent(prop))

# (4) Specify margin sizes for figures

par(mar = c(0, 0, 0, 0)) 

# (5) Drawing histogram of absolute cell counts

ggplot(Counts_combined, aes(x = factor(Class, 
                                       levels = c("Fibers", "Parenchyma", "Vessels", "Misdetection", 
                                                  "Curated earlywood fiber", "Curated fibers", "Curated parenchyma", 
                                                  "Curated vessels", "Curated misdetection")), y = n))  + 
  geom_col() + coord_flip() + ylim(0, 8850) + ylab("Cell count") + xlab ("Detection type") + 
  geom_text(aes(label = n), vjust = -0.1, hjust = -0.3) + 
  geom_text(aes(label = perc), vjust = 1.1, hjust = -0.1) + theme_classic() +
  theme(axis.text.x = element_text(colour = "maroon", size = 10, 
                                   angle = 0, margin = margin(t = 3))) +
  theme(axis.text.y = element_text(colour = "maroon", size = 10, 
                                   angle = 0, margin = margin(r = 3))) + 
  theme(axis.title.x = element_text(size = 11, margin = margin(t = 4))) + 
  theme(axis.title.y = element_text(size = 11, margin = margin(r = 5))) + 
  theme(text = element_text(family = "Aerial", size = 12)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"))

# DATAFRAME BUILDING

# (1) Removing misdetections from the data frames

Auto_data <- subset(Auto_data, Class %in% c("Fibers", "Parenchyma", "Vessels"))
Manual_data <- subset(Manual_data, Class %in% c("Fibers", "Parenchyma", "Vessels"))

# (2) Averaging individual microslide/sample data for automatic and manually curated data sets

Auto_mean <- Auto_data %>% 
  group_by(Image, Class) %>%
  summarise(CA_Mean_Auto = mean(Cell..Area), 
            LA_Mean_Auto = mean(Nucleus..Area), 
            LC_Mean_Auto = mean(Nucleus.Cell.area.ratio)) 

Manual_mean <- Manual_data %>% 
  group_by(Image, Class) %>%
  summarise(CA_Mean_Manual = mean(Cell..Area), 
            LA_Mean_Manual = mean(Nucleus..Area), 
            LC_Mean_Manual = mean(Nucleus.Cell.area.ratio)) 

# (3) Appending data frames and calculating differences between the bivariate pairs

QuPath_data <- bind_cols(Auto_mean[, 1:5], Manual_mean[, 3:5])

QuPath_data$CA_diff <- QuPath_data$CA_Mean_Auto - QuPath_data$CA_Mean_Manual
QuPath_data$LA_diff <- QuPath_data$LA_Mean_Auto - QuPath_data$LA_Mean_Manual
QuPath_data$LC_diff <- QuPath_data$LC_Mean_Auto - QuPath_data$LC_Mean_Manual

# NORMALITY BETWEEN PAIRED DIFFERENCES

# (1) Shapiro wilks test (CA)

shapiro.test(QuPath_data$CA_diff[QuPath_data$Class == "Fibers"])
shapiro.test(QuPath_data$CA_diff[QuPath_data$Class == "Parenchyma"])
shapiro.test(QuPath_data$CA_diff[QuPath_data$Class == "Vessels"])

# (2) Shapiro wilks test (LA)

shapiro.test(QuPath_data$LA_diff[QuPath_data$Class == "Fibers"])
shapiro.test(QuPath_data$LA_diff[QuPath_data$Class == "Parenchyma"])
shapiro.test(QuPath_data$LA_diff[QuPath_data$Class == "Vessels"])

# (3) Shapiro wilks test (LC)

shapiro.test(QuPath_data$LC_diff[QuPath_data$Class == "Fibers"])
shapiro.test(QuPath_data$LC_diff[QuPath_data$Class == "Parenchyma"])
shapiro.test(QuPath_data$LC_diff[QuPath_data$Class == "Vessels"])

# MULTIVARIATE NORMALITY

# (1) Load packages

library("MVN")

# (2) MV normality for cell area (CA)

mvn(data = QuPath_data [QuPath_data$Class == "Fibers", c(3,6)], mvnTest = "royston")

mvn(data = QuPath_data [QuPath_data$Class == "Parenchyma", c(3,6)], mvnTest = "royston")

mvn(data = QuPath_data [QuPath_data$Class == "Vessels", c(3,6)], mvnTest = "royston")

# (3) MV normality for lumen area (LA)

mvn(data = QuPath_data [QuPath_data$Class == "Fibers", c(4,7)], mvnTest = "royston")

mvn(data = QuPath_data [QuPath_data$Class == "Parenchyma", c(4,7)], mvnTest = "royston")

mvn(data = QuPath_data [QuPath_data$Class == "Vessels", c(4,7)], mvnTest = "royston")

# (4) MV normality for lumen to cell area ratio (LC)

mvn(data = QuPath_data [QuPath_data$Class == "Fibers", c(5,8)], mvnTest = "royston")

mvn(data = QuPath_data [QuPath_data$Class == "Parenchyma", c(5,8)], mvnTest = "royston")

mvn(data = QuPath_data [QuPath_data$Class == "Vessels", c(5,8)], mvnTest = "royston")

# TESTING FOR HOMOSCEDASTICITY OF RESIDUALS (Breusch-Pagan Test)
 # First need to make linear models of the data then test for homoscedasticity

# (1) Load packages

library(lmtest)

# (2) Cell area

LM_F_CA <- lm(CA_Mean_Auto ~ CA_Mean_Manual, data = subset(QuPath_data, Class == "Fibers")) 
LM_P_CA <- lm(CA_Mean_Auto ~ CA_Mean_Manual, data = subset(QuPath_data, Class == "Parenchyma"))
LM_V_CA <- lm(CA_Mean_Auto ~ CA_Mean_Manual, data = subset(QuPath_data, Class == "Vessels")) 

bptest(LM_F_CA)
bptest(LM_P_CA)
bptest(LM_V_CA)

# (3) Lumen area

LM_F_LA <- lm(LA_Mean_Auto ~ LA_Mean_Manual, data = subset(QuPath_data, Class == "Fibers")) 
LM_P_LA <- lm(LA_Mean_Auto ~ LA_Mean_Manual, data = subset(QuPath_data, Class == "Parenchyma"))
LM_V_LA <- lm(LA_Mean_Auto ~ LA_Mean_Manual, data = subset(QuPath_data, Class == "Vessels"))

bptest(LM_F_LA)
bptest(LM_P_LA)
bptest(LM_V_LA)

# (4) Lumen to cell area Ratio

LM_F_LC <- lm(LC_Mean_Auto ~ LC_Mean_Manual, data = subset(QuPath_data, Class == "Fibers")) 
LM_P_LC <- lm(LC_Mean_Auto ~ LC_Mean_Manual, data = subset(QuPath_data, Class == "Parenchyma"))
LM_V_LC <- lm(LC_Mean_Auto ~ LC_Mean_Manual, data = subset(QuPath_data, Class == "Vessels"))

bptest(LM_F_LC)
bptest(LM_P_LC)
bptest(LM_V_LC)

# LIN's CONCORDANCE CORRELATION COEFFICIENT

# (1) Load packages

library(DescTools)

# (2) Cell area

CCC(QuPath_data$CA_Mean_Auto [QuPath_data$Class == "Fibers"], 
    QuPath_data$CA_Mean_Manual [QuPath_data$Class == "Fibers"], 
    ci = "z-transform", conf.level = 0.95, na.rm = FALSE)

CCC(QuPath_data$CA_Mean_Auto [QuPath_data$Class == "Parenchyma"], 
    QuPath_data$CA_Mean_Manual [QuPath_data$Class == "Parenchyma"], 
    ci = "z-transform", conf.level = 0.95, na.rm = FALSE)

CCC(QuPath_data$CA_Mean_Auto [QuPath_data$Class == "Vessels"], 
    QuPath_data$CA_Mean_Manual [QuPath_data$Class == "Vessels"], 
    ci = "z-transform", conf.level = 0.95, na.rm = FALSE)

# (3) Lumen area

CCC(QuPath_data$LA_Mean_Auto [QuPath_data$Class == "Fibers"], 
    QuPath_data$LA_Mean_Manual [QuPath_data$Class == "Fibers"], 
    ci = "z-transform", conf.level = 0.95, na.rm = FALSE)

CCC(QuPath_data$LA_Mean_Auto [QuPath_data$Class == "Parenchyma"], 
    QuPath_data$LA_Mean_Manual [QuPath_data$Class == "Parenchyma"], 
    ci = "z-transform", conf.level = 0.95, na.rm = FALSE)

CCC(QuPath_data$LA_Mean_Auto [QuPath_data$Class == "Vessels"], 
    QuPath_data$LA_Mean_Manual [QuPath_data$Class == "Vessels"], 
    ci = "z-transform", conf.level = 0.95, na.rm = FALSE)

# (4) Lumen to cell area Ratio

CCC(QuPath_data$LC_Mean_Auto [QuPath_data$Class == "Fibers"], 
    QuPath_data$LC_Mean_Manual [QuPath_data$Class == "Fibers"], 
    ci = "z-transform", conf.level = 0.95, na.rm = FALSE)

CCC(QuPath_data$LC_Mean_Auto [QuPath_data$Class == "Parenchyma"], 
    QuPath_data$LC_Mean_Manual [QuPath_data$Class == "Parenchyma"], 
    ci = "z-transform", conf.level = 0.95, na.rm = FALSE)

CCC(QuPath_data$LC_Mean_Auto [QuPath_data$Class == "Vessels"], 
    QuPath_data$LC_Mean_Manual [QuPath_data$Class == "Vessels"], 
    ci = "z-transform", conf.level = 0.95, na.rm = FALSE)

# BLAND-ALTMAN PLOT TESTING AGREEMENT (nonparametric)

# (1) Load packages

library("MKinfer")

# (2) BA plot cell area

BAP_CA_F <- 
baplot(x = QuPath_data$CA_Mean_Auto [QuPath_data$Class == "Fibers"], 
       y = QuPath_data$CA_Mean_Manual [QuPath_data$Class == "Fibers"], 
       type = "nonparametric",
       xlab = "Mean CA"~"("*mu*m^2*")",
       ylab = Delta ~ "CA"~"("*mu*m^2*")",
       title = "Fibers", 
       ci.type = "approximate",
       color.low = "red", 
       color.upp = "red", 
       shape = 18, 
       ci.diff = TRUE,
       ci.loa = FALSE, 
       alpha = 10) +
  annotate(geom = "label", x = 131, y = 1.11742, label = "UL = 1.117", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 131, y = 0.19167, label = "MD = 0.192", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 131, y = -1.15680, label = "LL = -1.157", color = "black", 
           size = 2.5) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  expand_limits(x = 134.5)

BAP_CA_P <-
baplot(x = QuPath_data$CA_Mean_Auto [QuPath_data$Class == "Parenchyma"], 
       y = QuPath_data$CA_Mean_Manual [QuPath_data$Class == "Parenchyma"], 
       type = "nonparametric",
       xlab = "Mean CA"~"("*mu*m^2*")",
       ylab = Delta ~ "CA"~"("*mu*m^2*")",
       title = "Parenchyma", 
       ci.type = "approximate", 
       color.low = "red", 
       color.upp = "red", 
       shape = 18, 
       ci.diff = TRUE,
       ci.loa = FALSE, 
       alpha = 10) +
  annotate(geom = "label", x = 263, y = 10, label = "UL = 8.487", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 263, y = -9, label = "MD = -7.169", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 263, y = -81.55414, label = "LL = -81.554", color = "black", 
           size = 2.5) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  expand_limits(x = 280)

BAP_CA_V <-
baplot(x = QuPath_data$CA_Mean_Auto [QuPath_data$Class == "Vessels"], 
       y = QuPath_data$CA_Mean_Manual [QuPath_data$Class == "Vessels"], 
       type = "nonparametric",
       xlab = "Mean CA"~"("*mu*m^2*")",
       ylab = Delta ~ "CA"~"("*mu*m^2*")",
       title = "Vessels", 
       ci.type = "approximate", 
       color.low = "red", 
       color.upp = "red", 
       shape = 18, 
       ci.diff = TRUE,
       ci.loa = FALSE, 
       alpha = 10) +
  annotate(geom = "label", x = 3350, y = 0.0, label = "UL & MD = 0", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 3350, y = -1.225797, label = "LL = -1.226", color = "black", 
           size = 2.5) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  expand_limits(x = 3530, y = 0.1)

# (3) BA plot lumen area

BAP_LA_F <- 
baplot(x = QuPath_data$LA_Mean_Auto [QuPath_data$Class == "Fibers"], 
       y = QuPath_data$LA_Mean_Manual [QuPath_data$Class == "Fibers"], 
       type = "nonparametric",
       xlab = "Mean LA"~"("*mu*m^2*")",
       ylab = Delta ~ "LA"~"("*mu*m^2*")",
       title = "Fibers", 
       ci.type = "approximate", 
       color.low = "red", 
       color.upp = "red", 
       shape = 18, 
       ci.diff = TRUE,
       ci.loa = FALSE, 
       alpha = 10) +
  annotate(geom = "label", x = 75, y = 0.68224, label = "UL = 0.682", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 75, y = 0.04492, label = "MD = 0.045", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 75, y = -0.96928, label = "LL = -0.969", color = "black", 
           size = 2.5) +
  theme(plot.title = element_text(hjust = 0.5)) +  
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  expand_limits(x = 77.8)

BAP_LA_P <-
baplot(x = QuPath_data$LA_Mean_Auto [QuPath_data$Class == "Parenchyma"], 
       y = QuPath_data$LA_Mean_Manual [QuPath_data$Class == "Parenchyma"], 
       type = "nonparametric",
       xlab = "Mean LA"~"("*mu*m^2*")",
       ylab = Delta ~ "LA"~"("*mu*m^2*")",
       title = "Parenchyma", 
       ci.type = "approximate", 
       color.low = "red", 
       color.upp = "red", 
       shape = 18, 
       ci.diff = TRUE,
       ci.loa = FALSE, 
       alpha = 10) +
  annotate(geom = "label", x = 165, y = 6, label = "UL = 4.778", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 165, y = -6, label = "MD = -4.834", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 165, y = -51.00057, label = "LL = -51.001", color = "black", 
           size = 2.5) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  expand_limits(x = 176.5, y = 8)

BAP_LA_V <-
baplot(x = QuPath_data$LA_Mean_Auto [QuPath_data$Class == "Vessels"], 
       y = QuPath_data$LA_Mean_Manual [QuPath_data$Class == "Vessels"], 
       type = "nonparametric",
       xlab = "Mean LA"~"("*mu*m^2*")",
       ylab = Delta ~ "LA"~"("*mu*m^2*")",
       title = "Vessels", 
       ci.type = "approximate", 
       color.low = "red", 
       color.upp = "red", 
       shape = 18, 
       ci.diff = TRUE,
       ci.loa = FALSE, 
       alpha = 10) +
  annotate(geom = "label", x = 3010, y = 0.0, label = "UL & MD = 0", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 3010, y = -13.26059, label = "LL = -13.261", color = "black", 
           size = 2.5) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  expand_limits(x = 3180, y = 1)

# (4) BA plot LC ratio

BAP_LC_F <-
baplot(x = QuPath_data$LC_Mean_Auto [QuPath_data$Class == "Fibers"], 
       y = QuPath_data$LC_Mean_Manual [QuPath_data$Class == "Fibers"], 
       type = "nonparametric",
       xlab = "Mean LC",
       ylab = Delta ~ "LC",
       title = "Fibers", 
       ci.type = "approximate", 
       color.low = "red", 
       color.upp = "red", 
       shape = 18, 
       ci.diff = TRUE,
       ci.loa = FALSE, 
       alpha = 10) +
  annotate(geom = "label", x = 0.537, y = 0.00291, label = "UL = 0.003", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 0.537, y = -0.00033, label = "MD = -0.0003", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 0.537, y = -0.00384, label = "LL = -0.004", color = "black", 
           size = 2.5) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  expand_limits(x = 0.55)

BAP_LC_P <-
baplot(x = QuPath_data$LC_Mean_Auto [QuPath_data$Class == "Parenchyma"], 
       y = QuPath_data$LC_Mean_Manual [QuPath_data$Class == "Parenchyma"], 
       type = "nonparametric",
       xlab = "Mean LC",
       ylab = Delta ~ "LC",
       title = "Parenchyma", 
       ci.type = "approximate", 
       color.low = "red", 
       color.upp = "red", 
       shape = 18, 
       ci.diff = TRUE,
       ci.loa = FALSE, 
       alpha = 10) +
  annotate(geom = "label", x = 0.570, y = 0.00092, label = "UL = 0.001", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 0.570, y = -0.01384, label = "MD = -0.014", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 0.570, y = -0.06, label = "LL = -0.061", color = "black", 
           size = 2.5) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  expand_limits(x = 0.584, y = 0.005)

BAP_LC_V <-
baplot(x = QuPath_data$LC_Mean_Auto [QuPath_data$Class == "Vessels"], 
       y = QuPath_data$LC_Mean_Manual [QuPath_data$Class == "Vessels"], 
       type = "nonparametric",
       xlab = "Mean LC",
       ylab = Delta ~ "LC",
       title = "Vessels", 
       ci.type = "approximate", 
       color.low = "red", 
       color.upp = "red", 
       shape = 18, 
       ci.diff = TRUE,
       ci.loa = FALSE, 
       alpha = 10) +
  annotate(geom = "label", x = 0.9105, y = 0.0, label = "UL & MD = 0", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 0.9105, y = -0.00621, label = "LL = -0.006", color = "black", 
           size = 2.5) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  expand_limits(x = 0.918, y = 0.0005)

# GRID ARRANGEMENT OF PLOTS

# (1) Load packages

library("cowplot")

# (2) Plot BA plots in grid

plot_grid(BAP_CA_F, BAP_CA_P, BAP_CA_V, 
          BAP_LA_F, BAP_LA_P, BAP_LA_V,
          BAP_LC_F, BAP_LC_P, BAP_LC_V,
          labels = c("A", "B", "C", 
                     "D", "E", "F",
                     "G", "H", "I"), 
          label_size = 12, 
          label_y = 0.95, 
          ncol = 3)