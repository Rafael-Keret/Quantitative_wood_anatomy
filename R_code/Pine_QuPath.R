
# R code for data analysis and visualization of quantitative wood anatomy data generated in QuPath for Pinus radiata
 # Paper title: "An open-source machine-learning approach for obtaining high-quality quantitative wood anatomy data from E. grandis and P. radiata xylem"
 # Author: Rafael Keret

# IMPORTING DATA

# (1) Load packages

library(tidyverse)

# (2) Import QuPath raw (.csv) data file and indicate comma (",") delimiter to separate the data into columns

Auto_data <- read.table("./Data/Table_S3.csv", sep = ",", skip = 2, header = T)
Manual_data <- read.table("./Data/Table_S4.csv", sep = ",", skip = 2, header = T)

Auto_data$Class <- gsub(": Positive", "", Auto_data$Class)
Manual_data$Class <- gsub(": Positive", "", Manual_data$Class)

# (3) Calculating overall mean and standard deviation for cell area (CA), lumen area (LA) and lumen to cell area ratio (LC)

Grouped_auto <- Auto_data %>% 
  group_by(Class) %>%
  summarise(CA_Mean_Auto = mean(Cell..Area), 
            LA_Mean_Auto = mean(Nucleus..Area), 
            LC_Mean_Auto = mean(Nucleus.Cell.area.ratio), 
            CA_SD_Auto = sd(Cell..Area)/sqrt(18), 
            LA_SD_Auto = sd(Nucleus..Area)/sqrt(18), 
            LC_SD_Auto = sd(Nucleus.Cell.area.ratio)/sqrt(18))

Grouped_manual <- Manual_data %>% 
  group_by(Class) %>%
  summarise(CA_Mean_Manual = mean(Cell..Area), 
            LA_Mean_Manual = mean(Nucleus..Area), 
            LC_Mean_Manual = mean(Nucleus.Cell.area.ratio), 
            CA_SE_Manual = sd(Cell..Area)/sqrt(18), 
            LA_SE_Manual = sd(Nucleus..Area)/sqrt(18), 
            LC_SE_Manual = sd(Nucleus.Cell.area.ratio)/sqrt(18))

# HISTOGRAM

# (1) Omit manually curated detections (manual curations do not have ": Positive" in the "Name")

Curated_data <- Manual_data[grep(": Positive", Manual_data$Name),]

# (2) Generating a counts matrix for cell types

Counts_curated <- Curated_data %>% 
  group_by(Class) %>%
  count() %>% 
  ungroup()

Error_data <- anti_join(Auto_data, Curated_data, by = c("Cell..Area", 
                                                        "Nucleus..Area", 
                                                        "Cell..Eccentricity"))

Counts_error <- Error_data %>% 
  group_by(Class) %>%
  count() %>% 
  ungroup()

# (3) Rename the curated cell types for graphing

Counts_error$Class <- gsub("Earlywood_tracheids", "Curated earlywood tracheids", Counts_error$Class)
Counts_error$Class <- gsub("Latewood_tracheids", "Curated latewood tracheids", Counts_error$Class)
Counts_error$Class <- gsub("Misdetection", "Curated misdetection", Counts_error$Class)

Counts_combined <- rbind(Counts_curated, Counts_error)
Counts_combined$Class <- gsub("Earlywood_tracheids", "Earlywood tracheids", Counts_combined$Class)
Counts_combined$Class <- gsub("Latewood_tracheids", "Latewood tracheids", Counts_combined$Class)
Counts_combined$Class <- gsub("Misdetection", "Misdetection", Counts_combined$Class)

Counts_combined <- Counts_combined %>% 
  mutate(Counts_combined, prop = n/sum(n)) %>% 
  mutate(perc = scales::percent(prop))

# (4) Specify margin sizes for figures

par(mar = c(0, 0, 0, 0)) 

# (5) Drawing histogram of absolute cell counts

ggplot(Counts_combined, aes(x = factor(Class, 
                                       levels = c("Earlywood tracheids", "Latewood tracheids",
                                                  "Misdetection", "Curated earlywood tracheids",
                                                  "Curated latewood tracheids", 
                                                  "Curated misdetection")), y = n)) + 
  geom_col() + coord_flip() + ylim(0, 3550) + ylab("Cell count") + xlab ("Detection type") + 
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

Auto_data <- subset(Auto_data, Class %in% c("Earlywood_tracheids", "Latewood_tracheids"))
Manual_data <- subset(Manual_data, Class %in% c("Earlywood_tracheids", "Latewood_tracheids"))

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

shapiro.test(QuPath_data$CA_diff[QuPath_data$Class == "Latewood_tracheids"])
shapiro.test(QuPath_data$CA_diff[QuPath_data$Class == "Earlywood_tracheids"])

# (2) Shapiro wilks test (LA)

shapiro.test(QuPath_data$LA_diff[QuPath_data$Class == "Latewood_tracheids"])
shapiro.test(QuPath_data$LA_diff[QuPath_data$Class == "Earlywood_tracheids"])

# (3) Shapiro wilks test (LC)

shapiro.test(QuPath_data$LC_diff[QuPath_data$Class == "Latewood_tracheids"])
shapiro.test(QuPath_data$LC_diff[QuPath_data$Class == "Earlywood_tracheids"])

# MULTIVARIATE NORMALITY

library("MVN")

# (1) MV normality for cell area (CA)

mvn(data = QuPath_data [QuPath_data$Class == "Latewood_tracheids", c(3,6)], mvnTest = "royston")

mvn(data = QuPath_data [QuPath_data$Class == "Earlywood_tracheids", c(3,6)], mvnTest = "royston")

# (2) MV normality for lumen area (LA)

mvn(data = QuPath_data [QuPath_data$Class == "Latewood_tracheids", c(4,7)], mvnTest = "royston")

mvn(data = QuPath_data [QuPath_data$Class == "Earlywood_tracheids", c(4,7)], mvnTest = "royston")

# (3) MV normality for lumen to cell area ratio (LC)

mvn(data = QuPath_data [QuPath_data$Class == "Latewood_tracheids", c(5,8)], mvnTest = "royston")

mvn(data = QuPath_data [QuPath_data$Class == "Earlywood_tracheids", c(5,8)], mvnTest = "royston")

# TESTING FOR HOMOSCEDASTICITY OF THE RESIDUALS (Breusch-Pagan Test)
 # First need to make linear models of the data then test for homoscedasticity

# (1) Load packages

library(lmtest)

# (2) Cell area

LM_E_CA <- lm(CA_Mean_Auto ~ CA_Mean_Manual, data = subset(QuPath_data, Class == "Earlywood_tracheids")) 
LM_L_CA <- lm(CA_Mean_Auto ~ CA_Mean_Manual, data = subset(QuPath_data, Class == "Latewood_tracheids"))

bptest(LM_E_CA)
bptest(LM_L_CA)

# (3) Lumen area

LM_E_LA <- lm(LA_Mean_Auto ~ LA_Mean_Manual, data = subset(QuPath_data, Class == "Earlywood_tracheids")) 
LM_L_LA <- lm(LA_Mean_Auto ~ LA_Mean_Manual, data = subset(QuPath_data, Class == "Latewood_tracheids"))

bptest(LM_E_LA)
bptest(LM_L_LA)

# (4) Lumen to cell area ratio

LM_E_LC <- lm(LC_Mean_Auto ~ LC_Mean_Manual, data = subset(QuPath_data, Class == "Earlywood_tracheids")) 
LM_L_LC <- lm(LC_Mean_Auto ~ LC_Mean_Manual, data = subset(QuPath_data, Class == "Latewood_tracheids"))

bptest(LM_E_LC)
bptest(LM_L_LC)

# LIN's CONCORDANCE CORRELATION COEFFICIENT

# (1) Load packages

library(DescTools)

# (2) Cell area

CCC(QuPath_data$CA_Mean_Auto [QuPath_data$Class == "Earlywood_tracheids"], 
    QuPath_data$CA_Mean_Manual [QuPath_data$Class == "Earlywood_tracheids"], 
    ci = "z-transform", conf.level = 0.95, na.rm = FALSE)

CCC(QuPath_data$CA_Mean_Auto [QuPath_data$Class == "Latewood_tracheids"], 
    QuPath_data$CA_Mean_Manual [QuPath_data$Class == "Latewood_tracheids"], 
    ci = "z-transform", conf.level = 0.95, na.rm = FALSE)

# (3) Lumen area

CCC(QuPath_data$LA_Mean_Auto [QuPath_data$Class == "Earlywood_tracheids"], 
    QuPath_data$LA_Mean_Manual [QuPath_data$Class == "Earlywood_tracheids"], 
    ci = "z-transform", conf.level = 0.95, na.rm = FALSE)

CCC(QuPath_data$LA_Mean_Auto [QuPath_data$Class == "Latewood_tracheids"], 
    QuPath_data$LA_Mean_Manual [QuPath_data$Class == "Latewood_tracheids"], 
    ci = "z-transform", conf.level = 0.95, na.rm = FALSE)

# (4) Lumen to cell area ratio

CCC(QuPath_data$LC_Mean_Auto [QuPath_data$Class == "Earlywood_tracheids"], 
    QuPath_data$LC_Mean_Manual [QuPath_data$Class == "Earlywood_tracheids"], 
    ci = "z-transform", conf.level = 0.95, na.rm = FALSE)

CCC(QuPath_data$LC_Mean_Auto [QuPath_data$Class == "Latewood_tracheids"], 
    QuPath_data$LC_Mean_Manual [QuPath_data$Class == "Latewood_tracheids"], 
    ci = "z-transform", conf.level = 0.95, na.rm = FALSE)

# BLAND-ALTMAN PLOTS TESTING AGREEMENT (nonparametric)

# (1) Load package

library("MKinfer")

# (2) BA plot cell area

BAP_CA_E <- 
  baplot(x = QuPath_data$CA_Mean_Auto [QuPath_data$Class == "Earlywood_tracheids"], 
         y = QuPath_data$CA_Mean_Manual [QuPath_data$Class == "Earlywood_tracheids"], 
         type = "nonparametric",
         xlab = "Mean CA"~"("*mu*m^2*")",
         ylab = Delta ~ "CA"~"("*mu*m^2*")",
         title = "Earlywood tracheids", 
         ci.type = "approximate",
         color.low = "red", 
         color.upp = "red", 
         shape = 18, 
         ci.diff = TRUE,
         ci.loa = FALSE, 
         alpha = 10) +
  annotate(geom = "label", x = 1877, y = 70.73305, label = "UL = 70.733", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 1877, y = 0.78386, label = "MD = 0.784", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 1877, y = -46, label = "LL = -48.686", color = "black", 
           size = 2.5) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  expand_limits(x = 1974)

BAP_CA_L <-
  baplot(x = QuPath_data$CA_Mean_Auto [QuPath_data$Class == "Latewood_tracheids"], 
         y = QuPath_data$CA_Mean_Manual [QuPath_data$Class == "Latewood_tracheids"], 
         type = "nonparametric",
         xlab = "Mean CA"~"("*mu*m^2*")",
         ylab = Delta ~ "CA"~"("*mu*m^2*")",
         title = "Latewood tracheids", 
         ci.type = "approximate", 
         color.low = "red", 
         color.upp = "red", 
         shape = 18, 
         ci.diff = TRUE,
         ci.loa = FALSE, 
         alpha = 10) +
  annotate(geom = "label", x = 995, y = 29.78536, label = "UL = 29.785", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 995, y = 3.18595, label = "MD = 3.186", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 995, y = -72.9, label = "LL = -73.335", color = "black", 
           size = 2.5) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  expand_limits(x = 1040, y = 35)

# (3) BA plot lumen area

BAP_LA_E <- 
  baplot(x = QuPath_data$LA_Mean_Auto [QuPath_data$Class == "Earlywood_tracheids"], 
         y = QuPath_data$LA_Mean_Manual [QuPath_data$Class == "Earlywood_tracheids"], 
         type = "nonparametric",
         xlab = "Mean LA"~"("*mu*m^2*")",
         ylab = Delta ~ "LA"~"("*mu*m^2*")",
         title = "Earlywood tracheids", 
         ci.type = "approximate", 
         color.low = "red", 
         color.upp = "red", 
         shape = 18, 
         ci.diff = TRUE,
         ci.loa = FALSE, 
         alpha = 10) +
  annotate(geom = "label", x = 1345, y = 68.19469, label = "UL = 68.195", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 1345, y = -0.91617, label = "MD = -0.916", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 1345, y = -35.5, label = "LL = -38.913", color = "black", 
           size = 2.5) +
  theme(plot.title = element_text(hjust = 0.5)) +  
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  expand_limits(x = 1420)

BAP_LA_L <-
  baplot(x = QuPath_data$LA_Mean_Auto [QuPath_data$Class == "Latewood_tracheids"], 
         y = QuPath_data$LA_Mean_Manual [QuPath_data$Class == "Latewood_tracheids"], 
         type = "nonparametric",
         xlab = "Mean LA"~"("*mu*m^2*")",
         ylab = Delta ~ "LA"~"("*mu*m^2*")",
         title = "Latewood tracheids", 
         ci.type = "approximate", 
         color.low = "red", 
         color.upp = "red", 
         shape = 18, 
         ci.diff = TRUE,
         ci.loa = FALSE, 
         alpha = 10) +
  annotate(geom = "label", x = 439, y = 22.96625, label = "UL = 22.966", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 439, y = -0.02784, label = "MD = -0.028", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 439, y = -44.91943, label = "LL = -44.919", color = "black", 
           size = 2.5) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  expand_limits(x = 462, y = 30)

# (4) BA plot LC ratio

BAP_LC_E <-
  baplot(x = QuPath_data$LC_Mean_Auto [QuPath_data$Class == "Earlywood_tracheids"], 
         y = QuPath_data$LC_Mean_Manual [QuPath_data$Class == "Earlywood_tracheids"], 
         type = "nonparametric",
         xlab = "Mean LC",
         ylab = Delta ~ "LC",
         title = "Earlywood tracheids", 
         ci.type = "approximate", 
         color.low = "red", 
         color.upp = "red", 
         shape = 18, 
         ci.diff = TRUE,
         ci.loa = FALSE, 
         alpha = 10) +
  annotate(geom = "label", x = 0.740, y = 0.02298, label = "UL = 0.023", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 0.740, y = 0.00060, label = "MD = 0.0006", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 0.740, y = -0.008, label = "LL = -0.009", color = "black", 
           size = 2.5) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  expand_limits(x = 0.755)

BAP_LC_L <-
  baplot(x = QuPath_data$LC_Mean_Auto [QuPath_data$Class == "Latewood_tracheids"], 
         y = QuPath_data$LC_Mean_Manual [QuPath_data$Class == "Latewood_tracheids"], 
         type = "nonparametric",
         xlab = "Mean LC",
         ylab = Delta ~ "LC",
         title = "Latewood tracheids", 
         ci.type = "approximate", 
         color.low = "red", 
         color.upp = "red", 
         shape = 18, 
         ci.diff = TRUE,
         ci.loa = FALSE, 
         alpha = 10) +
  annotate(geom = "label", x = 0.513, y = 0.01321, label = "UL = 0.013", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 0.513, y = -0.00104, label = "MD = -0.001", color = "black", 
           size = 2.5) +
  annotate(geom = "label", x = 0.513, y = -0.01878, label = "LL = -0.019", color = "black", 
           size = 2.5) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  expand_limits(x = 0.535)

# GRID ARRANGEMENT OF PLOTS

# (1) Load packages

library("cowplot")

# (2) Plot BA plots in grid

plot_grid(BAP_CA_E,BAP_CA_L, 
          BAP_LA_E, BAP_LA_L, 
          BAP_LC_E, BAP_LC_L,  
          labels = c("A", "B", 
                     "C", "D", 
                     "E", "F"), 
          label_size = 12, 
          label_y = 0.95, 
          ncol = 2) 
