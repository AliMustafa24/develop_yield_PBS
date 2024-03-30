## developing yield curve graph
## by Ali 2024

library(ggplot2)
library(cowplot)#tuk combine plot
library(readxl) # karena csv valuesnya berubah - kita pake excel nya - package ini lebih mudah
library(openxlsx) # tuk print dalam bentu xlsx 
library(dplyr)
library(zoo) # tuk bikin interpolasi diantara data yg ilang


rm(list=ls())

curve_IBPA<-"C:/Users/andy.mustafa/OneDrive - Kemenkeu/2. Seksi Analisis Harga SBSN/#Kajian_analisis_seksiAH/2024/inverted_PBS2Y_2024"

#input raw data 

setwd(curve_IBPA)

# Read the specified sheet into a data frame
sheet_name <- "copas_ke_R" # pilih sheet yg akan diimpor
MainData <- read_excel("YieldCurve_PBS_inputR.xlsx", sheet = sheet_name) #impor excel dari salah satu sheet

tanggal <-MainData[1,"Date"]
tanggal<- format(tanggal$Date,"%Y-%m-%d") #tuk ekstrak posisi tanggal IBPA
#tanggal_IBPA<-print(tanggal$Date)

# Replace NA values with 0 in a specific column
#MainData$Yield_Series_22_03_2024[is.na(MainData$Yield_Series_22_03_2024)] <- 0

# Interpolate missing values
MainData$Yield_Curve_IBPA_22_03_2024 <- na.approx(MainData$Yield_Curve_IBPA_22_03_2024) #sesuaikan nama kolomnya
MainData$Yield_Curve_IBPA_02_01_2024 <- na.approx(MainData$Yield_Curve_IBPA_02_01_2024) #sesuaikan nama kolomnya


# Replace 0 values with NA in Yield_Series_22_03_2024
MainData$Yield_Series_22_03_2024[MainData$Yield_Series_22_03_2024 == 0] <- NA #sesuaikan nama kolomnya

# Filter the dataset to include only rows where Series_Name is not empty
labeled_data <- MainData[MainData$Series_Name != "", ]


# Plot
yield_curve_IBPA <- ggplot(MainData) + 
  geom_line(aes(x = TENOR, y = `Yield_Curve_IBPA_22_03_2024`, color = "Yield_Curve_IBPA_22_03_2024"), linetype = "dashed") + #sesuaikan nama kolomnya
  geom_line(aes(x = TENOR, y = `Yield_Curve_IBPA_02_01_2024`, color = "Yield_Curve_IBPA_02_01_2024")) + #sesuaikan nama kolomnya
  geom_point(aes(x = TENOR, y = Yield_Series_22_03_2024, color = "Yield_Series_22_03_2024")) +  #sesuaikan nama kolomnya
  geom_text(data = labeled_data, aes(x = TENOR, y = Yield_Series_22_03_2024, label = Series_Name), vjust = -0.5) + #sesuaikan nama kolomnya
  labs(x = "TENOR(YEAR)", y = "Yield") + # Add color legend
  scale_color_manual(name = paste0("a.o.",tanggal), values = c("Yield_Curve_IBPA_22_03_2024" = "blue", "Yield_Curve_IBPA_02_01_2024" = "red", "Yield_Series_22_03_2024" = "green")) + # Assign colors to legend 
  scale_x_continuous(breaks = seq(0, 30, by = 1), labels = seq(0, 30, by = 1)) + # Adjust the interval unit for the x-axis  
  theme_minimal()

# Save as PDF
ggsave(paste0("yield_curve",tanggal,".pdf"), yield_curve_IBPA, width = 10, height = 6)


# Save as PNG
#ggsave(paste0("yield_curve",tanggal,".pdf"), yield_curve_IBPA, width = 10, height = 6, units = "in", dpi = 300)

# Save as JPEG
#ggsave(paste0("yield_curve",tanggal,".pdf"), yield_curve_IBPA, width = 10, height = 6, units = "in", dpi = 300)



###########
# Create line plot for Yield_Curve_IBPA_22_03_2024 and Yield_Curve_IBPA_02_01_2024
#line_plot <- ggplot(MainData) +
#  geom_line(aes(x = TENOR, y = Yield_Curve_IBPA_22_03_2024), color = "blue") +
#  geom_line(aes(x = TENOR, y = Yield_Curve_IBPA_02_01_2024), color = "red") +
#  labs(x = "TENOR", y = "Yield") +
#  theme_minimal()

# Create scatter plot for Yield_Series_22_03_2024, ignoring NA values
#scatter_plot <- ggplot(MainData, aes(x = TENOR, y = Yield_Series_22_03_2024)) +
#  geom_point(data = na.omit(MainData), color = "green") +  # Ignore NA values
#  labs(x = NULL, y = NULL) +
#  theme_void()

# Add scatter plot to the line plot
#combined_plot <- line_plot + annotation_custom(ggplotGrob(scatter_plot), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)

# Print the combined plot
#print(combined_plot)







#data <- data.frame(
#  TENOR = c(0.08, 1, 1.5, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
#  `Yield_Curve_IBPA_22_03_2024` = c(5.91, 6.11, 6.18, 6.26, 6.36, 6.43, 6.49, 6.54, 6.58, 6.62, 6.65, 6.69, 6.72, 6.75),
#  `Yield_Curve_IBPA_2_01_2024` = c(6.34, 6.36, 6.38, 6.39, 6.42, 6.45, 6.47, 6.50, 6.53, 6.56, 6.59, 6.62, 6.64, 6.67),
#  Additional_Column = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
#)

# Plot
#ggplot(data) +
#  geom_line(aes(x = TENOR, y = `Yield_Curve_IBPA_22_03_2024`), color = "blue") +
#  geom_line(aes(x = TENOR, y = `Yield_Curve_IBPA_2_01_2024`), color = "red") +
#  geom_point(aes(x = TENOR, y = Additional_Column), color = "green") +
#  labs(x = "TENOR(YEAR)", y = "Yield") +
#  theme_minimal()


# Filter out NA and 0 values
#filtered_data <- MainData %>%
#  filter(!is.na(Yield_Series_22_03_2024),
#         `Yield_Series_22_03_2024` != 0)#,
#`Yield_Curve_IBPA_02_01_2024` != 0,
#!is.na(Yield_Series_22_03_2024))
