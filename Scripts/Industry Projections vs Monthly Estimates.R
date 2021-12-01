library(RSocrata)     # Download or Upload 'Socrata' Data Sets
library(tidyverse)    # Collection of R packages designed for data science
library(scales)       # Additional graphics functions
library(gridExtra)    # Functions for arranging multiple plots on a page
library(Cairo)        # Create high-quality vector (PDF, PostScript and SVG) and bitmap output
library(zoo)          # Moving averages     
library(lubridate)    # Makes it easier to work with dates and times.
library(ggpubr)       # Functions for creating and customizing 'ggplot2'- based publication ready plots. 

proj <- read.socrata(
  "https://data.edd.ca.gov/resource/sp6i-jezb.json?$where=area_type = 'Metropolitan Area'") %>%
  filter(series_code == "10000000" | series_code == "20000000" | series_code == "30000000" |
           series_code == "40000000" | series_code == "50000000" | series_code == "55000000" |
           series_code == "60000000" | series_code == "65000000" | series_code == "70000000" |
           series_code == "80000000" | series_code == "90000000") %>%
  mutate(
    projected_year_employment_estimate = as.numeric(projected_year_employment_estimate)) %>%
  select(-area_type, -industry_title, -period, -base_year_employment_estimate, -numeric_change, -percentage_change) %>%
  rename(msa = area_name) 

ces <- read.socrata(
  "https://data.edd.ca.gov/resource/r4zm-kdcg.json?$where=seasonally_adjusted = 'N' AND year >= '2021'") %>%
  filter(
    area_type == "Metropolitan Area" &
      (series_code == "10000000" | series_code == "20000000" | series_code == "30000000" |
         series_code == "40000000" | series_code == "50000000" | series_code == "55000000" |
         series_code == "60000000" | series_code == "65000000" | series_code == "70000000" |
         series_code == "80000000" | series_code == "90000000")) %>%
  select(msa = area_name, month = date, series_code, industry_title, estimated_employment = current_employment) %>%
  mutate(
    estimated_employment = as.numeric(estimated_employment),
    month = as.Date(month),) %>%
  filter(month == max(month))

current_month <- paste(
  months(as.Date(first(ces$month)))," ",
  year(first(ces$month)),sep="")

msa_list <- read.csv("./Data/msa_list.csv")

df <- ces %>%
  inner_join(msa_list, by = c("msa" = "msa_ces")) %>%
  inner_join(proj, by = c("msa_projections" = "msa","series_code" = "series_code")) %>%
  group_by(msa) %>%
  pivot_longer(
    cols=c("estimated_employment", "projected_year_employment_estimate"), 
    names_to="variable",
    values_to="employment") %>%
  select(-id, -msa_projections) 

d <- paste0("./Output/Current Estimates vs Projeted/", format(as.Date(first(ces$month)), "%y-%m")," ",month.abb[month(first(ces$month))])

dir.create(d, showWarnings = FALSE, recursive = TRUE)


lapply(unique(df$msa), function(x) {
  
  g <- df %>%
    filter(msa == x) %>%
    group_by(msa) %>%
    arrange(desc(employment)) %>%
    mutate(order = row_number()) %>%
    mutate(industry_title = fct_reorder(industry_title, employment)) %>%
    ggplot(aes(
      x = industry_title, 
      y = employment, 
      fill = variable)) +
    geom_col(alpha = 0.9, position = "dodge") +
    geom_text(
      aes(
        label = comma(employment),
        hjust = 1.1,
        vjust = 0.5,
        colour = ifelse(employment >= quantile(employment, prob=c(0.15)), "white", "black")),
      position = position_dodge(width = .9),
      show.legend = FALSE) +
    coord_flip() +
    scale_color_manual(
      values = c("white" = "white", "black" = "black"))+
    scale_fill_manual(
      labels = c(paste(current_month," Estimated Employment",sep=""), "2028 Projected Employment"),
      values = c(
        "estimated_employment" = "#005f7c",
        "projected_year_employment_estimate" = "#c67f07"))+
    theme(text = element_text(colour = "#000000", size=14),
          title = element_text(color = "#00587C"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.background = element_rect(fill = NA),
          plot.background = element_rect(fill = "#FFFFFF"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.position = "top",
          plot.title = element_text(
            hjust = 0.5,
            color = "#00587C", 
            size = 18,
            face="bold"),
          plot.subtitle = element_text(
            hjust = 0.5,
            color = "#00587C", 
            size = 16,
            face="bold"),
          axis.title=element_text(size=12,face="bold"),
          plot.margin=unit(c(0,0,0,0.5),"in")) +
    labs(
      title = x,
      subtitle = paste("Current Employment Estimates vs. 2028 Projected Employment",sep="")) 
  

  file_name <- paste0(d,"/",current_month," ",x,".png")
  
  ggsave(g, filename = file_name, dpi = 300, type = 'cairo',
         width = 13, height = 8.5, units = 'in')
  
  print(file_name)
})

