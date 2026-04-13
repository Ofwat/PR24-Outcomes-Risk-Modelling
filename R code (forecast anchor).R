#This code calculates monte carlo simulation outputs for all PR24 PCs based on truncated normal distributions
#The anchor point is set to PR24 PCLs by default, this can be changed in the code before 'Truncation point' calculations
#To get simulation outputs for the ODI Risk Monte Carlo Simulation models, this code is run once for each year of PR24. This example code is for the year 2025-26.

## Stage 1: Prep R to run code

#Clear environment
rm(list = ls())
# Clear console
cat("\014")

# Install packages

packs_list <- c("ggplot2", "openxlsx", "reshape2","tidyr", 
                
                "stringr", "jtools", "readxl", "ggrepel", 
                
                'rlang', 'dplyr', 'truncnorm', 'tidyverse')

check.packages <- function(pkg){
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg)) 
    
    install.packages(new.pkg, dependencies = TRUE)
  
  sapply(pkg, require, character.only = TRUE)
  
}

check.packages(packs_list)

#Set input to file path of folder with data files labelled 'PCL data_2025-26.csv' & 'Performance data.csv'
#Replace 'PCL data_2025-26.csv' with file of the relevant year below when defining PCL_data
#Replace any \ with / in the file path
#All outputs are saved in folder defined as out below

input = " [Copy file path to input folder into here] "
out = " [Copy file path to output folder here] "
setwd(out)

#Set seed

set.seed(123)
options(scipen = 999)
sample_size <- 1000


## Stage 2: Calculating distribution statistics


#Read in input data
PCL_data <- read.csv(file.path(input,"PCL data_2025-26.csv"))
Performance <- read.csv(file.path(input,"Performance data V1.csv"))

#Set standardisation factor

#As a performance range calibration adjustment, we use company-specific standard deviations for Water-only companies for Discharge Permit Compliance. This is due to the large variation in performance ranges across water-only companies.

Performance$SD_Sample <- ifelse((Performance$PC=="DPC")&(Performance$Type=="WoC"),"Company-specific", Performance$SD_Sample)

#standard deviation calcuations
Performance_sd <- Performance %>% 
  
  #Sector-wide standard deviation
  group_by(PC,Type) %>% 
  mutate(sd_sector = sd(Performance,na.rm = T)) %>% 
  ungroup() %>% 
  
  #Industry-wide standard deviation
  group_by(PC) %>%
  mutate(sd_industry = sd(Performance,na.rm = T)) %>%
  ungroup() %>% 
  
  #Company-specifc standard deviation
  group_by(PC,Company) %>% 
  mutate(sd_company = sd(Performance,na.rm = T)) %>% 
  ungroup() %>% 
  
  #add nested if else to select sd type
  select(Company,PC,sd_sector,sd_industry, sd_company,Type, SD_Sample) %>% 
  unique() %>% 
  mutate(sd = ifelse(SD_Sample=="Industry-wide", sd_industry, 
                     ifelse(SD_Sample=="Company-specific", sd_company, sd_sector))) %>%
  
  
  #drop additional columns
  select(Company,PC,Type,sd)

#Company-specific standard deviation adjustments based on performance range calibrations against historical outturn performance.

Performance_sd$sd <- ifelse((Performance_sd$PC=="DPC")&(Performance_sd$Type=="WaSC"), 1.2*Performance_sd$sd , Performance_sd$sd)

Performance_sd$sd <- ifelse(Performance_sd$PC=="POL", 0.2*Performance_sd$sd , Performance_sd$sd)
Performance_sd$sd <- ifelse((Performance_sd$PC=="POL")&(Performance_sd$Company=="SRN"), 1.1*5*(Performance_sd$sd) , Performance_sd$sd)
Performance_sd$sd <- ifelse((Performance_sd$PC=="POL")&(Performance_sd$Company=="SWB"), 1.1*5*(Performance_sd$sd) , Performance_sd$sd)
Performance_sd$sd <- ifelse((Performance_sd$PC=="POL")&(Performance_sd$Company=="HDD"), 1.5*5*(Performance_sd$sd) , Performance_sd$sd)

Performance_sd$sd <- ifelse((Performance_sd$PC=="ESF")&(Performance_sd$Company=="YKY"), 0.6*Performance_sd$sd , Performance_sd$sd)

Performance_sd$sd <- ifelse(Performance_sd$PC=="ISF", 0.5*Performance_sd$sd , Performance_sd$sd)
Performance_sd$sd <- ifelse((Performance_sd$PC=="ISF")&(Performance_sd$Company=="TMS"), 0.9*2*(Performance_sd$sd) , Performance_sd$sd)
Performance_sd$sd <- ifelse((Performance_sd$PC=="ISF")&(Performance_sd$Company=="UUW"), 0.9*2*(Performance_sd$sd) , Performance_sd$sd)
Performance_sd$sd <- ifelse((Performance_sd$PC=="ISF")&(Performance_sd$Company=="YKY"), 0.9*2*(Performance_sd$sd) , Performance_sd$sd)

#Merge data frames
Company_data <- merge(Performance_sd,PCL_data,by=c("Company","PC"))


#SET ANCHOR POINT HERE (Forecast or PCL)

Company_data$Anchor <- Company_data$PCL

#standardisation factor
Company_data$s_factor <- ifelse(Company_data$Discrepancy=="Levels",
                                Company_data$sd,
                                Company_data$Anchor*Company_data$sd)

###Truncation point

#Company specific truncation points based on performance range calibrations against historical outturn performance

Company_data$Natural_limit <- ifelse((Company_data$PC=="ISF")&(Company_data$Company=="SWB"),1.5, Company_data$Natural_limit)

Company_data$Natural_limit <- ifelse((Company_data$PC=="POL")&(Company_data$Company=="SWB"),10, Company_data$Natural_limit)
Company_data$Natural_limit <- ifelse((Company_data$PC=="POL")&(Company_data$Company=="SRN"),8, Company_data$Natural_limit)
Company_data$Natural_limit <- ifelse((Company_data$PC=="POL")&(Company_data$Company=="UUW"),10, Company_data$Natural_limit)

#Out performance truncation
#Calculate performance difference to PCL
Company_data$t_point <- NA

#for PCs where improvement is downward
Company_data$t_point <- ifelse(Company_data$Improvement_direction=="Down",
                               Company_data$Anchor - Company_data$Natural_limit,
                               Company_data$t_point)

#for PCs where improvement is upward
Company_data$t_point <- ifelse(Company_data$Improvement_direction=="Up",
                               Company_data$Natural_limit - Company_data$Anchor,
                               Company_data$t_point)

Company_data$t_point_standardised <- ifelse(Company_data$Apply.Truncation=="Yes",
                                            Company_data$t_point / Company_data$s_factor,
                                            Inf)

#Under performance truncation (BIO Only)
#BIO under performance truncated at natural limit of 0

Company_data$under_t_point <- NA
Company_data$under_t_point <- ifelse((Company_data$PC=="BIO"),0 - Company_data$Anchor, Company_data$under_t_point)


Company_data$under_t_point_standardised <- -Inf
Company_data$under_t_point_standardised <- ifelse((Company_data$PC=="BIO"),Company_data$under_t_point / Company_data$s_factor,Company_data$under_t_point_standardised )


###Run Monte Carlo

#a is left truncation point (underperformance)
#b is right truncation point (outperformance)
##Create function to run Monte Carlo z-statistic outputs for all PCs for each company
###
MC_creation <- function(Data) {
  MC_output <- c()
  for (i in 1:nrow(Data)) {
    
    # identify truncation point and characteristics for ith entry in company data
    trunc_point <- Data$t_point_standardised[i]
    under_trunc_point <- Data$under_t_point_standardised[i]
    comp_pc_name <- paste(Data$Company[i],Data$PC[i],sep=" ")
    
    #Monte Carlo truncated normal distribution
    MC_out <- rtruncnorm(n = sample_size, mean = 0, sd = 1, a = under_trunc_point, b = trunc_point)
    
    #combine individual PC columns
    MC_output <- cbind(MC_output,MC_out)
    MC_output <- as.data.frame(MC_output)
    names(MC_output)[i] <- comp_pc_name
  }
  return(MC_output)
}


company_select <- c("ANH","WSH","HDD","NES","SVE","SWB","SRN","TMS","UUW",
                    "WSX","YKY","AFW","BRL","SSC","PRT","SEW","SES")

company_order <- c("ANH","WSH","HDD","NES","NES (NNE)","NES (ESK)","SVE","SWB","SRN","TMS","UUW",
                   "WSX","YKY","AFW","BRL","SSC","SSC (SST)","SSC (CAM)","PRT","SEW","SES")

MC_company1 <- MC_creation(Company_data[Company_data$Company %in% c(company_select),])


###Create SD output for Excel conversion of z-stats
sd_values <- Performance_sd %>%
  pivot_wider(names_from = PC, values_from = sd)

sd_values <- sd_values %>%
  mutate(RWQ = NA)

sd_values <- sd_values %>%
  select(Company, ISF, ESF, BWQ, WQC, CRI, WSI, MRP, UNO, SCO, POL, SPL, DPC, SOF, LEA, PCC, NHH, RWQ, BIO, OGW, OGWW)

sd_values <- sd_values %>%
  mutate(Company = factor(Company, levels = company_order)) %>%
  arrange(Company) 


##Export dataframe
write.csv(MC_company1,'Monte Carlo output 2025-26 (Performance data V1).csv')
write.csv(sd_values, 'standard_dev_2025-26 (Performance data V1).csv')
