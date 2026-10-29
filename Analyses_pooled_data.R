# POOLED EFFECTS STANDARD ERRORS FOR COUNTRY AND RESPONDENT ####
## Libraries ####


pacman::p_load(
  tidyverse, haven, broom, cregg, survey, scales, marginaleffects,
  ggforce, ggdist, patchwork, ggplot2, dplyr, forcats, tictoc, emmeans
)


## Global variables ####

output_wd = "path_repository_with_dataset_produced_thorugh_script_for_pooled_analyses"

# Load dataset
dataset_rep = "repository_where_you_store_RDS_files/"
gdrive_code = "yourmain_Googledrive_address_if_you_use_it/"
dataset = readRDS("~/data.RDS")

#FOR ROBUSTNESS CHECKS
# dataset = dataset |>
#   filter(cpd_task_number == 1)

formula_nominal = cpd_chosen ~  cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
  cpd_consc + cpd_ope +
  cpd_diet + cpd_animal + cpd_holiday# + cpd_country

formula_match = cpd_chosen ~  cpd_match_gender + cpd_match_age + 
  cpd_match_educ + cpd_match_regionfeel +
  cpd_match_consc + cpd_match_ope +
  cpd_match_diet + cpd_match_animal + cpd_match_holiday


z_alpha=1.96 #95% significance
z_alpha_99=2.575 #99% significance
z_alpha_90=1.645 #90% significance

data_nat <- dataset |>
  filter(cpd_exparm2 == "natural")

design_nat <- svydesign(
  ids = ~ cpd_country + respid,  # Clustered standard errors by country & respondent
  data = data_nat  # Use the dataset
)

model_svy_match_nat <- svyglm(
  formula_match,
  design = design_nat
)

#1.2. Define Survey Design (without weights) for Manipulated M Arm (M=1)

data_match <- dataset |>
  filter(cpd_exparm2 == "ideology_match") 


design_match <- svydesign(
  ids = ~ cpd_country + respid,  # Clustered standard errors by country & respondent
  data = data_match  # Use the dataset
)

model_svy_match_match <- svyglm(
  formula_match,
  design = design_match
)


#1.3. Define Survey Design (without weights) for Manipulated M Arm (M=2)


data_mismatch <- dataset |>
  filter(cpd_exparm2 == "ideology_mismatch")


design_mismatch <- svydesign(
  ids = ~ cpd_country + respid,  # Clustered standard errors by country & respondent
  data = data_mismatch  # Use the dataset
)


model_svy_match_mismatch <- svyglm(
  formula_match,
  design = design_mismatch
)




## ATES ####

# 1. Define Survey Design (without weights)

# 2. Fit the OLS Model
model_nominal_ATEs <- svyglm(
  formula_nominal,
  design = design_nat
)

model_match_ATEs <- svyglm(
  formula_match,
  design = design_nat
)

# 3. Generate a weight variable that is simply the proportion of each category
# Define the conjoint variables
conjoint_vars_nominal <- c("cpd_gender", "cpd_age", "cpd_educ", "cpd_regionfeel",
                           "cpd_consc", "cpd_ope", 
                           "cpd_diet", "cpd_animal", "cpd_holiday"#, "cpd_country"
)

conjoint_vars_match <- c("cpd_match_gender", "cpd_match_age", "cpd_match_educ", "cpd_match_regionfeel", 
                         "cpd_match_consc", "cpd_match_ope",
                         "cpd_match_diet", "cpd_match_animal", "cpd_match_holiday"#, "cpd_country"
)


# Compute frequency weights (proportion of each category in the dataset)
data_nat_nominal <- data_nat %>%
  group_by(across(all_of(conjoint_vars_nominal))) %>%  # Group by all conjoint variables
  mutate(cells = n() / nrow(dataset)) %>%  # Compute proportion
  ungroup()

data_nat_match <- data_nat %>%
  group_by(across(all_of(conjoint_vars_match))) %>%  # Group by all conjoint variables
  mutate(cells = n() / nrow(dataset)) %>%  # Compute proportion
  ungroup()



#### MM #####
##### Nominal ATEs####
# Compute marginal means using `avg_predictions()`

tictoc::tic() 
marginal_means_list <- lapply(conjoint_vars_nominal, function(var) {
  avg_predictions(model_nominal_ATEs, by=var#, newdata = data_nat_nominal, wt ="wts"
  ) %>%
    rename(value = !!sym(var)) %>%  # Rename the grouping variable to "value"
    mutate(term = var)  # Add a column specifying the variable
})

tictoc::toc() #121.628 sec elapsed

ATEs_mock = dataset |>
  filter(cpd_exparm2 == "natural") |>
  cj(formula_nominal,
     id = ~respid,
     estimate = "mm")

ATEs_mock$lower99 = NA
ATEs_mock$upper99 = NA
ATEs_mock$lower90 = NA
ATEs_mock$upper90 = NA

ATEs = ATEs_mock

mm= marginal_means_list

mm <- bind_rows(mm)

for(i in unique(mm$value))
{
  #ATEs[ATEs$level == i, ]$estimate = mm[mm$value == i, ]$estimate
  ATEs[ATEs$level == i, ]$std.error = mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$lower = mm[mm$value == i, ]$conf.low
  ATEs[ATEs$level == i, ]$upper = mm[mm$value == i, ]$conf.high
  
  ATEs[ATEs$level == i, ]$lower90 = mm[mm$value == i, ]$estimate - z_alpha_90*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper90 = mm[mm$value == i, ]$estimate + z_alpha_90*mm[mm$value == i, ]$std.error
  
  ATEs[ATEs$level == i, ]$lower99 = mm[mm$value == i, ]$estimate - z_alpha_99*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper99 = mm[mm$value == i, ]$estimate + z_alpha_99*mm[mm$value == i, ]$std.error
}

dir <- file.path(output_wd) 
if(!dir.exists(dir)) dir.create(dir)

saveRDS(ATEs, file = paste0(output_wd,"mm_clustSE_ATEs_nominal.rds"))

##### Match ATEs ####

tictoc::tic() 
marginal_means_list <- lapply(conjoint_vars_match, function(var) {
  avg_predictions(model_match_ATEs, 
                  by = var
  ) %>%
    rename(value = !!sym(var)) %>%
    mutate(term = var)
})

tictoc::toc() 


ATEs_mock = dataset |>
  filter(cpd_exparm2 == "natural") |>
  cj(formula_match,
     id = ~respid,
     estimate = "mm")

ATEs_mock$lower99 = NA
ATEs_mock$upper99 = NA
ATEs_mock$lower90 = NA
ATEs_mock$upper90 = NA

ATEs = ATEs_mock

mm= marginal_means_list

mm <- bind_rows(mm)


for(i in unique(mm$value))
{
  ATEs[ATEs$level == i, ]$std.error = mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$lower = mm[mm$value == i, ]$conf.low
  ATEs[ATEs$level == i, ]$upper = mm[mm$value == i, ]$conf.high
  
  ATEs[ATEs$level == i, ]$lower90 = mm[mm$value == i, ]$estimate - z_alpha_90*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper90 = mm[mm$value == i, ]$estimate + z_alpha_90*mm[mm$value == i, ]$std.error
  
  ATEs[ATEs$level == i, ]$lower99 = mm[mm$value == i, ]$estimate - z_alpha_99*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper99 = mm[mm$value == i, ]$estimate + z_alpha_99*mm[mm$value == i, ]$std.error
}


saveRDS(ATEs, file = paste0(output_wd,"mm_clustSE_ATEs_match.rds"))


#### AMCES ####
####Match ATEs####


ATEs_mock <- dataset |>
  filter(cpd_exparm2 == "natural") |>
  cj(formula_match,
     id = ~respid,
     estimate = "amce")

ATEs_mock$lower99 = NA
ATEs_mock$upper99 = NA
ATEs_mock$lower90 = NA
ATEs_mock$upper90 = NA

ATEs = ATEs_mock

acmes = as.data.frame(summary(model_svy_match_nat)$coefficients)[-1, ]

for(i in 1:length(conjoint_vars_match))
{
  ATEs$estimate[2*i] = acmes$Estimate[i]
  ATEs$std.error[2*i] = acmes$`Std. Error`[i]
  ATEs$lower[2*i] = ATEs$estimate[2*i] - z_alpha*ATEs$std.error[2*i]
  ATEs$upper[2*i] = ATEs$estimate[2*i] + z_alpha*ATEs$std.error[2*i]
  
  ATEs$lower90[2*i] = acmes$Estimate[i] - z_alpha_90*acmes$`Std. Error`[i]
  ATEs$upper90[2*i] = acmes$Estimate[i] + z_alpha_90*acmes$`Std. Error`[i]
  
  ATEs$lower99[2*i] = acmes$Estimate[i] - z_alpha_99*acmes$`Std. Error`[i]
  ATEs$upper99[2*i] = acmes$Estimate[i] + z_alpha_99*acmes$`Std. Error`[i]
}

saveRDS(ATEs, file = paste0(output_wd,"amce_clustSE_ATEs_match.rds"))



## ACDEs ####
### MM ####
#### Match ####

# 1. Define Survey Design (without weights)
design_ACDEs_match <- svydesign(
  ids = ~ cpd_country + respid,  # Clustered standard errors by country & respondent
  data = data_match  # Use the dataset
)

design_ACDEs_mismatch <- svydesign(
  ids = ~ cpd_country + respid,  # Clustered standard errors by country & respondent
  data = data_mismatch  # Use the dataset
)

# 2. Fit the OLS Model
model_ACDEs_match <- svyglm(
  formula_match,
  design = design_ACDEs_match
)

model_ACDEs_mismatch <- svyglm(
  formula_match,
  design = design_ACDEs_mismatch
)


tictoc::tic() 

marginal_means_list <- lapply(conjoint_vars_match, function(var) {
  avg_predictions(model_ACDEs_match,
                  by = var
  ) %>%
    rename(value = !!sym(var)) %>% 
    mutate(term = var)  
})

tictoc::toc() 


ACDEs_mock = dataset |>
  filter(cpd_exparm2 == "ideology_match") |>
  cj(formula_match,
     id = ~respid,
     estimate = "mm")

ACDEs_mock$lower99 = NA
ACDEs_mock$upper99 = NA
ACDEs_mock$lower90 = NA
ACDEs_mock$upper90 = NA

ACDEs = ACDEs_mock

mm= marginal_means_list

mm <- bind_rows(mm)


for(i in unique(mm$value))
{
  ACDEs[ACDEs$level == i, ]$std.error = mm[mm$value == i, ]$std.error
  ACDEs[ACDEs$level == i, ]$lower = mm[mm$value == i, ]$conf.low
  ACDEs[ACDEs$level == i, ]$upper = mm[mm$value == i, ]$conf.high
  
  ACDEs[ACDEs$level == i, ]$lower90 = mm[mm$value == i, ]$estimate - z_alpha_90*mm[mm$value == i, ]$std.error
  ACDEs[ACDEs$level == i, ]$upper90 = mm[mm$value == i, ]$estimate + z_alpha_90*mm[mm$value == i, ]$std.error
  
  ACDEs[ACDEs$level == i, ]$lower99 = mm[mm$value == i, ]$estimate - z_alpha_99*mm[mm$value == i, ]$std.error
  ACDEs[ACDEs$level == i, ]$upper99 = mm[mm$value == i, ]$estimate + z_alpha_99*mm[mm$value == i, ]$std.error
}


saveRDS(ACDEs, file = paste0(output_wd,"mm_clustSE_ACDEs_match.rds"))


#### mismatch ####

tictoc::tic() 

marginal_means_list <- lapply(conjoint_vars_match, function(var) {
  avg_predictions(model_ACDEs_mismatch, 
                  by = var
  ) %>%
    rename(value = !!sym(var)) %>%
    mutate(term = var)
})

tictoc::toc() 


ACDEs_mock = dataset |>
  filter(cpd_exparm2 == "ideology_mismatch") |>
  cj(formula_match,
     id = ~respid,
     estimate = "mm")

ACDEs_mock$lower99 = NA
ACDEs_mock$upper99 = NA
ACDEs_mock$lower90 = NA
ACDEs_mock$upper90 = NA


ACDEs = ACDEs_mock

mm= marginal_means_list

mm <- bind_rows(mm)


for(i in unique(mm$value))
{
  ACDEs[ACDEs$level == i, ]$std.error = mm[mm$value == i, ]$std.error
  ACDEs[ACDEs$level == i, ]$lower = mm[mm$value == i, ]$conf.low
  ACDEs[ACDEs$level == i, ]$upper = mm[mm$value == i, ]$conf.high
  
  ACDEs[ACDEs$level == i, ]$lower90 = mm[mm$value == i, ]$estimate - z_alpha_90*mm[mm$value == i, ]$std.error
  ACDEs[ACDEs$level == i, ]$upper90 = mm[mm$value == i, ]$estimate + z_alpha_90*mm[mm$value == i, ]$std.error
  
  ACDEs[ACDEs$level == i, ]$lower99 = mm[mm$value == i, ]$estimate - z_alpha_99*mm[mm$value == i, ]$std.error
  ACDEs[ACDEs$level == i, ]$upper99 = mm[mm$value == i, ]$estimate + z_alpha_99*mm[mm$value == i, ]$std.error
}


saveRDS(ACDEs, file = paste0(output_wd,"mm_clustSE_ACDEs_mismatch.rds"))


###  AMCES ####
#### Match ####

ACDEs_mock <- dataset |>
  filter(cpd_exparm2 == "ideology_match") |>
  cj(formula_match,
     id = ~respid,
     estimate = "amce")

ACDEs_mock$lower99 = NA
ACDEs_mock$upper99 = NA
ACDEs_mock$lower90 = NA
ACDEs_mock$upper90 = NA

ACDEs = ACDEs_mock

acmes = as.data.frame(summary(model_svy_match_match)$coefficients)[-1, ]

for(i in 1:length(conjoint_vars_match))
{
  ACDEs$estimate[2*i] = acmes$Estimate[i] 
  ACDEs$std.error[2*i] = acmes$`Std. Error`[i]
  ACDEs$lower[2*i] = ACDEs$estimate[2*i] - z_alpha*ACDEs$std.error[2*i]
  ACDEs$upper[2*i] = ACDEs$estimate[2*i] + z_alpha*ACDEs$std.error[2*i]
  
  ACDEs$lower90[2*i] = ACDEs$estimate[2*i] - z_alpha_90*ACDEs$std.error[2*i]
  ACDEs$upper90[2*i] = ACDEs$estimate[2*i] + z_alpha_90*ACDEs$std.error[2*i]
  
  ACDEs$lower99[2*i] = ACDEs$estimate[2*i] - z_alpha_99*ACDEs$std.error[2*i]
  ACDEs$upper99[2*i] = ACDEs$estimate[2*i] + z_alpha_99*ACDEs$std.error[2*i]
}

saveRDS(ACDEs, file = paste0(output_wd,"amce_clustSE_ACDEs_match.rds"))


#### Mismatch ####

ACDEs_mock <- dataset |>
  filter(cpd_exparm2 == "ideology_mismatch") |>
  cj(formula_match,
     id = ~respid,
     estimate = "amce")

ACDEs_mock$lower99 = NA
ACDEs_mock$upper99 = NA
ACDEs_mock$lower90 = NA
ACDEs_mock$upper90 = NA

ACDEs = ACDEs_mock

acmes = as.data.frame(summary(model_svy_match_mismatch)$coefficients)[-1, ]

for(i in 1:length(conjoint_vars_match))
{
  ACDEs$estimate[2*i] = acmes$Estimate[i] 
  ACDEs$std.error[2*i] = acmes$`Std. Error`[i]
  ACDEs$lower[2*i] = ACDEs$estimate[2*i] - z_alpha*ACDEs$std.error[2*i]
  ACDEs$upper[2*i] = ACDEs$estimate[2*i] + z_alpha*ACDEs$std.error[2*i]
  
  ACDEs$lower90[2*i] = ACDEs$estimate[2*i] - z_alpha_90*ACDEs$std.error[2*i]
  ACDEs$upper90[2*i] = ACDEs$estimate[2*i] + z_alpha_90*ACDEs$std.error[2*i]
  
  ACDEs$lower99[2*i] = ACDEs$estimate[2*i] - z_alpha_99*acmes$`Std. Error`[i]
  ACDEs$upper99[2*i] = ACDEs$estimate[2*i] + z_alpha_99*acmes$`Std. Error`[i]
}

saveRDS(ACDEs, file = paste0(output_wd,"amce_clustSE_ACDEs_mismatch.rds"))


## EEs ####

### MM ####

tictoc::tic() 

marginal_means_list_natural <- lapply(conjoint_vars_match, function(var) {
  avg_predictions(model_svy_match_nat,
                  by = var
  ) %>%
    rename(value = !!sym(var)) %>%
    mutate(term = var)  
})


marginal_means_list_match <- lapply(conjoint_vars_match, function(var) {
  avg_predictions(model_svy_match_match,
                  by = var
  ) %>%
    rename(value = !!sym(var)) %>%
    mutate(term = var)
})


marginal_means_list_mismatch <- lapply(conjoint_vars_match, function(var) {
  avg_predictions(model_svy_match_mismatch,
                  by = var
  ) %>%
    rename(value = !!sym(var)) %>%
    mutate(term = var)
})

tictoc::toc()

#Now I have the MM for natural mediation arm, manìpulated mediation arm
#with ideology match and with ideology mismatch
# To compute the EEs, I now need to make the differences between these MMs

# I create a mock dataset to replicate
EEs_mock <- dataset |>
  filter(cpd_exparm2 == "natural" | cpd_exparm2 == "ideology_match") |>
  cj(formula_match,
     id = ~respid,
     estimate = "mm_diff",
     by = ~cpd_exparm)

EEs_mock$lower99 = NA
EEs_mock$upper99 = NA
EEs_mock$lower90 = NA
EEs_mock$upper90 = NA

EE_match = EEs_mock

EE_mismatch = EEs_mock

z_alpha=1.96

for(i in 1:length(conjoint_vars_match))
{
  mm_nat_temp = as.data.frame(marginal_means_list_natural[[i]])
  mm_match_temp = as.data.frame(marginal_means_list_match[[i]])
  mm_mismatch_temp = as.data.frame(marginal_means_list_mismatch[[i]])
  
  indxs = c(2*i-1,2*i)
  
  EE_match$estimate[indxs] = mm_nat_temp$estimate - mm_match_temp$estimate
  EE_match$std.error[indxs] = sqrt(mm_nat_temp$std.error^2 + mm_match_temp$std.error^2)
  
  EE_match$lower[indxs] = EE_match$estimate[indxs] - z_alpha*EE_match$std.error[indxs]
  EE_match$upper[indxs] = EE_match$estimate[indxs] + z_alpha*EE_match$std.error[indxs]
  
  EE_match$lower90[indxs] = EE_match$estimate[indxs] - z_alpha_90*EE_match$std.error[indxs]
  EE_match$upper90[indxs] = EE_match$estimate[indxs] + z_alpha_90*EE_match$std.error[indxs]
  
  EE_match$lower99[indxs] = EE_match$estimate[indxs] - z_alpha_99*EE_match$std.error[indxs]
  EE_match$upper99[indxs] = EE_match$estimate[indxs] + z_alpha_99*EE_match$std.error[indxs]
  
  EE_mismatch$estimate[indxs] = mm_nat_temp$estimate - mm_mismatch_temp$estimate
  EE_mismatch$std.error[indxs] = sqrt(mm_nat_temp$std.error^2 + mm_mismatch_temp$std.error^2)
  
  EE_mismatch$lower[indxs] = EE_mismatch$estimate[indxs] - z_alpha*EE_mismatch$std.error[indxs]
  EE_mismatch$upper[indxs] = EE_mismatch$estimate[indxs] + z_alpha*EE_mismatch$std.error[indxs]
  
  EE_mismatch$lower90[indxs] = EE_mismatch$estimate[indxs] - z_alpha_90*EE_mismatch$std.error[indxs]
  EE_mismatch$upper90[indxs] = EE_mismatch$estimate[indxs] + z_alpha_90*EE_mismatch$std.error[indxs]
  
  EE_mismatch$lower99[indxs] = EE_mismatch$estimate[indxs] - z_alpha_99*EE_mismatch$std.error[indxs]
  EE_mismatch$upper99[indxs] = EE_mismatch$estimate[indxs] + z_alpha_99*EE_mismatch$std.error[indxs]
}


saveRDS(EE_match, file = paste0(output_wd,"mm_clustSE_EEs_match.rds"))
saveRDS(EE_mismatch, file = paste0(output_wd,"mm_clustSE_EEs_mismatch.rds"))


### AMCES ####

#### EEs

EEs_mock <- dataset |>
  filter(cpd_exparm2 == "natural" | cpd_exparm2 == "ideology_match") |>
  cj(formula_match,
     id = ~respid,
     estimate = "amce_diff",
     by = ~cpd_exparm)

#reorder the row of the df in the conventional order for me
EEs_mock$feature = factor(EEs_mock$feature, levels = conjoint_vars_match)
EEs_mock = EEs_mock[order(EEs_mock$feature), ]

EEs_mock$lower99 = NA
EEs_mock$upper99 = NA
EEs_mock$lower90 = NA
EEs_mock$upper90 = NA

#creating the two dataset to refill
EE_match = EEs_mock
EE_mismatch = EEs_mock

#significance  0.95

amce_nat_temp = as.data.frame(summary(model_svy_match_nat)$coefficients)[-1, ] #no intercept row
amce_match_temp = as.data.frame(summary(model_svy_match_match)$coefficients)[-1, ]
amce_mismatch_temp = as.data.frame(summary(model_svy_match_mismatch)$coefficients)[-1, ]

for(i in 1:length(conjoint_vars_match))
{
  EE_match$estimate[i] = amce_nat_temp$Estimate[i] - amce_match_temp$Estimate[i]
  EE_match$std.error[i] = sqrt(amce_nat_temp$`Std. Error`[i]^2 + amce_match_temp$`Std. Error`[i]^2)
  EE_match$lower[i] = EE_match$estimate[i] - z_alpha*EE_match$std.error[i]
  EE_match$upper[i] = EE_match$estimate[i] + z_alpha*EE_match$std.error[i]
  
  EE_match$lower90[i] = EE_match$estimate[i] - z_alpha_90*EE_match$std.error[i]
  EE_match$upper90[i] = EE_match$estimate[i] + z_alpha_90*EE_match$std.error[i]
  
  EE_match$lower99[i] = EE_match$estimate[i] - z_alpha_99*EE_match$std.error[i]
  EE_match$upper99[i] = EE_match$estimate[i] + z_alpha_99*EE_match$std.error[i]
  
  EE_mismatch$estimate[i] = amce_nat_temp$Estimate[i] - amce_mismatch_temp$Estimate[i]
  EE_mismatch$std.error[i] = sqrt(amce_nat_temp$`Std. Error`[i]^2 + amce_mismatch_temp$`Std. Error`[i]^2)
  EE_mismatch$lower[i] = EE_mismatch$estimate[i] - z_alpha*EE_mismatch$std.error[i]
  EE_mismatch$upper[i] = EE_mismatch$estimate[i] + z_alpha*EE_mismatch$std.error[i]
  
  EE_mismatch$lower90[i] = EE_mismatch$estimate[i] - z_alpha_90*EE_mismatch$std.error[i]
  EE_mismatch$upper90[i] = EE_mismatch$estimate[i] + z_alpha_90*EE_mismatch$std.error[i]
  
  EE_mismatch$lower99[i] = EE_mismatch$estimate[i] - z_alpha_99*EE_mismatch$std.error[i]
  EE_mismatch$upper99[i] = EE_mismatch$estimate[i] + z_alpha_99*EE_mismatch$std.error[i]
}

#save them
saveRDS(EE_match, file = paste0(output_wd,"amce_clustSE_EEs_match.rds"))
saveRDS(EE_mismatch, file = paste0(output_wd,"amce_clustSE_EEs_mismatch.rds"))


