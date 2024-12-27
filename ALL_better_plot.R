library(readxl) ; library(writexl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(Publish)


df=data.frame(
  year=rep(c("2010","2011"),each=4),
  treatment=rep(c("Impact","Control")),
  type=rep(c("Phylum1","Phylum2"),each=2),
  total=sample(1:100,8))



df %>% 
  group_by(year, treatment) %>% 
  mutate(cum_tot = cumsum(total)) %>% 
  ggplot(aes(treatment, cum_tot, fill =year)) + 
  geom_col(data = . %>% filter( type=="Phylum1"), position = position_dodge(width = 0.9), alpha = 1) +
  geom_col(data = . %>% filter( type=="Phylum2"), position = position_dodge(width = 0.9), alpha = 0.4) +
  geom_tile(aes(y=NA_integer_, alpha = factor(type))) + 
  scale_alpha_manual(values = c(1,0.4))


####ACTUAL CODE STARTS HERE####

plot_data_long = read_xlsx("ALL mortality stacked.xlsx")

plot_data_long %>% 
  group_by(`phase nutshell`, flag) %>% 
  mutate(cum_tot = cumsum(count)) %>% 
  ggplot(aes(factor(`phase nutshell`,levels = c("INDUCTION", "CONSOLIDATION", "MAINTENANCE","RELAPSE","POST BMT","COMPLETED")), 
             cum_tot, fill =factor(flag, levels = c("ICU Mortality","90 Days Mortality", "1 Year Mortality")))) + 
  geom_col(data = . %>% filter( Status == "Dead Patients"), position = position_dodge(width = 0.9), alpha = 1) +
  geom_col(data = . %>% filter( Status == "Alive Patients"), position = position_dodge(width = 0.9), alpha = 0.6) +
  geom_tile(aes(y=NA_integer_, alpha = factor(Status))) + 
  geom_text(data = plot_data_long %>% filter(Status == "Alive Patients"), 
            aes(label = paste0(round(percentage, 2), "%"), 
                y = count+1.35), # Top of Dead Patients stack
            position = position_dodge(width = 0.9), size = 3, color = "black") +
  geom_text(data = plot_data_long %>% filter(Status == "Dead Patients" ), 
            aes(label = paste0(round(percentage, 2), "%"), 
                y = count-0.5), # Top of Dead Patients stack
            position = position_dodge(width = 0.9), width=0.5, size = 3, color = "white")+
  scale_alpha_manual(
    values = c("Alive Patients" = 0.6, "Dead Patients" = 1),
    name = "Patient Status"
  ) +
  scale_fill_manual(
    values = c("ICU Mortality" = "#1f77b4", "90 Days Mortality" = "#ff7f0e", "1 Year Mortality" = "#2ca02c"),
    name = "Mortality"
  ) +
  scale_y_continuous(breaks = seq(0,80,10),limits = c(0,80))+
  theme_minimal(base_size = 12)+
  labs(
    x = "Phase",
    y = "Total Patients",
    title = "Total Number of Patients per Phase in ALL Cohort",
  )+
  guides(
    fill = guide_legend(override.aes = list(alpha = 1)),
    alpha = guide_legend(override.aes = list(fill = "#808080"))
  )+
  theme(
    legend.position = c(0.8, 0.7), # Adjust position inside the plot area
    legend.background = element_rect(fill = "white", color = "black", size = 0.1),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
  

####AML MORTALITY STACKED BAR PLOT####
plot_data_long = read_xlsx("AML_stacked_mortality.xlsx")

plot_data_long %>% 
  group_by(`phase nutshell`, flag) %>% 
  mutate(cum_tot = cumsum(count)) %>% 
  ggplot(aes(factor(`phase nutshell`,levels = c("INDUCTION", "CONSOLIDATION")), 
             cum_tot, fill =factor(flag, levels = c("ICU Mortality","90 Days Mortality", "1 Year Mortality")))) + 
  geom_col(data = . %>% filter( Status == "Dead Patients"), position = position_dodge(width = 0.9), alpha = 1) +
  geom_col(data = . %>% filter( Status == "Alive Patients"), position = position_dodge(width = 0.9), alpha = 0.6) +
  geom_tile(aes(y=NA_integer_, alpha = factor(Status))) + 
  geom_text(data = plot_data_long %>% filter(Status == "Alive Patients"), 
            aes(label = paste0(round(percentage, 2), "%"), 
                y = count+1.35), # Top of Dead Patients stack
            position = position_dodge(width = 0.9), size = 3, color = "black") +
  geom_text(data = plot_data_long %>% filter(Status == "Dead Patients" ), 
            aes(label = paste0(round(percentage, 2), "%"), 
                y = count-0.5), # Top of Dead Patients stack
            position = position_dodge(width = 0.9), width=0.5, size = 3, color = "white")+
  scale_alpha_manual(
    values = c("Alive Patients" = 0.6, "Dead Patients" = 1),
    name = "Patient Status"
  ) +
  scale_fill_manual(
    values = c("ICU Mortality" = "#1f77b4", "90 Days Mortality" = "#ff7f0e", "1 Year Mortality" = "#2ca02c"),
    name = "Mortality"
  ) +
  scale_y_continuous(breaks = seq(0,50,10),limits = c(0,50))+
  theme_minimal(base_size = 12)+
  labs(
    x = "Phase",
    y = "Total Patients",
    title = "Total Number of Patients per Phase in AML Cohort",
  )+
  guides(
    fill = guide_legend(override.aes = list(alpha = 1)),
    alpha = guide_legend(override.aes = list(fill = "#808080"))
  )+
  theme(
    legend.position = c(0.8, 0.8), # Adjust position inside the plot area
    legend.background = element_rect(fill = "white", color = "black", size = 0.1),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )+
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
