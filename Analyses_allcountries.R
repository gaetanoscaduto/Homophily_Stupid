# Analyses_allcountries.R ####
#this script is for the analyses related to the parallel conjoint design,
# considering all countries at the same time. For the pooled estimates,
# refer to "Analyses_Pooled.R"

# IMPORTANT: FOR THIS TO WORK, YOU NEED TO RUN POOL ANALYSES FIRST
# AND SAVE DATASETS CONSISTENTLY

## Libraries ####

# pacman::p_load(
#   cregg, dplyr, ggpubr, cowplot, 
#   MASS, cjoint, corrplot, dplyr, 
#   forcats, ggplot2, gt, gtools, 
#   gtsummary, margins, openxlsx, 
#   patchwork, rio, scales, texreg, tools, 
#   lme4, ggeffects, wesanderson, writexl
# )

## Functions ####


set_categories_and_levels_bycountry = function(effects, 
                                               type=c("match","nominal"), 
                                               nominal_attributes=nominal_attributes){
  #Here I define a function to set categories and levels in a neat and presentable 
  #fashion in the mm dataset resulting from the cj function. The
  #functio
  
  type=match.arg(type)
  
  effects$category="Sociodemographics"
  
  effects$category=ifelse(grepl("ope",effects$feature) | grepl("consc",effects$feature), 
                          "Psychological",
                          ifelse(grepl("diet",effects$feature) | grepl("animal",effects$feature) | grepl("holiday",effects$feature),
                                 "Lifestyle",
                                 ifelse(grepl("ideo", effects$feature), 
                                        "Political",
                                        "Sociodemographics")))
  
  if(type=="match")
  {
    effects$variable = as.character(effects$level)
    
    for(i in 1:nrow(effects))
    {
      effects$variable[i] = toTitleCase(paste(strsplit(as.character(effects$level[i]), "_")[[1]], collapse=" "))
    }
    
    
    effects$level=factor(effects$variable, levels = unique(effects$variable))
    
  }
  if(type=="nominal")
  {
    effects$feature = factor(nominal_attributes, levels = unique(nominal_attributes))
    effects$level=factor(levels_vector, levels = levels_vector)
  }
  
  effects <- effects |>
    mutate(across(everything(),
                  \(x) {
                    if (is.factor(x)) x <- as.character(x)
                    if (is.character(x)) gsub("\\bAnimal\\b", 
                                              "Pet", x) 
                    else x
                  }))
  
  return(effects)
}

draw_plot_effects_bycountry = function(effects, 
                                       type=c("match",
                                              "nominal"), #"match" or "nominal" 
                                       categories=c("Sociodemographics", 
                                                    "Psychological",
                                                    "Lifestyle"), #vector of thee categories 
                                       #("sociodemo", "psycho", "lifestyle")
                                       estimator=c("mm",
                                                   "amce",
                                                   "mm_differences",
                                                   "amce_differences"), #either amce, mm, or mm_differences
                                       y_labels=y_labels_plots,
                                       leftlim=999, #the left limit of the plot
                                       rightlim=999,#the right limit of the plot
                                       x_intercept=999, #the vertical line to signal the difference from the insignificance
                                       want_regionfeel = F # if you do not want regionfeel to be plotted in the nominal plot
){
  ##Function to draw plots for the effects
  
  
  estimator=match.arg(estimator)
  type=match.arg(type)
  
  v = list()
  
  
  
  if(leftlim==999) # if leftlim has default value (unspecified), then we set the limits conservatively
    #with [-1; 1] for amces and [0, 1] for mm
  {
    
    leftlim=ifelse(estimator!="mm", -1, 0)
    rightlim=1
  }
  if(x_intercept==999)
  {
    intercept = ifelse(estimator!="mm", 0, 0.5)
  }
  
  if(estimator =="mm")
  {
    xlabel="Marginal Means"
  }
  else
  {
    xlabel="Average Marginal Component Effects"
  }
  
  
  #relative positions of the graphs in the plot
  
  increment = 1/6
  y_CZ=2*increment
  y_FR = increment
  y_POOL=0
  y_IT=-1*increment
  y_SW=-2*increment
  
  size_countries=0.8
  size_pool=0.8
  size_countries_point = 1.5
  size_pool_point = 2
  alpha_countries = 0.5
  
  fatten_countries = 1
  fatten_pool = 1
  
  for(category in categories)
  {
    
    these_labels = y_labels[[type]][[category]]
    
    #if I do not want to plot regionfeel
    
    if((type == "nominal") && (want_regionfeel == F) && (category == "Sociodemographics"))
    {
      effects = effects[effects$feature != "Regionfeel", ]
      effects$level=factor(effects$level, levels = levels_vector2)
      
      these_labels = c("Woman", "Man",
                       "Under 35", "Between 35 and 59","Over 60",
                       "Degree","No degree")
    }
    
    data_IT=effects[effects$category == category & effects$cpd_country == "IT", ]
    data_FR=effects[effects$category == category & effects$cpd_country == "FR", ]
    data_SW=effects[effects$category == category & effects$cpd_country == "SW", ]
    data_CZ=effects[effects$category == category & effects$cpd_country == "CZ", ]
    data_POOL = effects[effects$category == category & effects$cpd_country == "POOL", ]
    
    # #I don't want the pool to drop regionfeel
    
    if(category == "Sociodemographics")
    {
      data_POOL = data_POOL[data_POOL$feature != "Regionfeel", ]
    }
    

    p = ggplot()+
      geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_point(data=data_IT,
                 aes(x=estimate, y=level, col = "IT", shape = "IT"),
                 size = size_countries_point,
                 position = position_nudge(y = y_IT),
                 show.legend = T)+
      geom_point(data=data_FR,
                 aes(x=estimate, y=level, col = "FR", shape = "FR"),
                 size = size_countries_point,
                 position = position_nudge(y = y_FR),
                 show.legend = T)+
      geom_point(data=data_SW,
                 aes(x=estimate, y=level, col = "SW", shape = "SW"),
                 size = size_countries_point,
                 position = position_nudge(y = y_SW),
                 show.legend = T)+
      geom_point(data=data_CZ,
                 aes(x=estimate, y=level, col = "CZ", shape = "CZ"),
                 size = size_countries_point,
                 position = position_nudge(y = y_CZ),
                 show.legend = T)+
      geom_point(data=data_POOL,
                 aes(x=estimate, y=level, col = "POOL", shape = "POOL"),
                 size = size_pool_point,
                 position = position_nudge(y = y_POOL),
                 show.legend = T)+
      geom_linerange(data=data_IT,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "IT", shape = "IT"),
                     alpha = alpha_countries/2,
                     size = 3/4*size_countries,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_IT),
                     show.legend = T)+
      geom_linerange(data=data_IT,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "IT", shape = "IT"),
                     alpha = alpha_countries,
                     size = size_countries,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_IT),
                     show.legend = T)+
      geom_linerange(data=data_IT,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "IT", shape = "IT"),
                     alpha = alpha_countries*2,
                     size = 5/4*size_countries,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_IT),
                     show.legend = T)+
      geom_linerange(data=data_FR,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "FR", shape = "FR"),
                     alpha = alpha_countries/2,
                     size = 3/4*size_countries,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_FR),
                     show.legend = T)+
      geom_linerange(data=data_FR,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "FR", shape = "FR"),
                     alpha = alpha_countries,
                     size = size_countries,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_FR),
                     show.legend = T)+
      geom_linerange(data=data_FR,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "FR", shape = "FR"),
                     alpha = alpha_countries*2,
                     size = 5/4*size_countries,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_FR),
                     show.legend = T)+
      geom_linerange(data=data_SW,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "SW", shape = "SW"),
                     alpha = alpha_countries/2,
                     size = 3/4*size_countries,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_SW),
                     show.legend = T)+
      geom_linerange(data=data_SW,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "SW", shape = "SW"),
                     alpha = alpha_countries,
                     size = size_countries,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_SW),
                     show.legend = T)+
      geom_linerange(data=data_SW,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "SW", shape = "SW"),
                     alpha = alpha_countries*2,
                     size = 5/4*size_countries,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_SW),
                     show.legend = T)+
      geom_linerange(data=data_CZ,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "CZ", shape = "CZ"),
                     alpha = alpha_countries/2,
                     size = 3/4*size_countries,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_CZ),
                     show.legend = T)+
      geom_linerange(data=data_CZ,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "CZ", shape = "CZ"),
                     alpha = alpha_countries,
                     size = size_countries,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_CZ),
                     show.legend = T)+
      geom_linerange(data=data_CZ,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "CZ", shape = "CZ"),
                     alpha = alpha_countries*2,
                     size = 5/4*size_countries,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_CZ),
                     show.legend = T)+
      geom_linerange(data=data_POOL,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "POOL", shape = "POOL"),
                     alpha = alpha_countries/2,
                     size = size_pool/2,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_POOL),
                     show.legend = T)+
      geom_linerange(data=data_POOL,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "POOL", shape = "POOL"),
                     alpha = alpha_countries,
                     size = size_pool,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_POOL),
                     show.legend = T)+
      geom_linerange(data=data_POOL,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "POOL", shape = "POOL"),
                     alpha = alpha_countries*2,
                     size = size_pool*2,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_POOL),
                     show.legend = T)+
      geom_text(data=data_POOL,
                aes(x = estimate, y = level, 
                    label = scales::percent(estimate, accuracy = 0.1)),  
                hjust = -3.5, 
                size = 3,  
                color = "black") +
      ylab(category)+
      xlab(xlabel)+
      scale_x_continuous(limits = c(leftlim, rightlim), 
                         breaks = round(seq(leftlim, rightlim,
                                            length.out = 9), 
                                        digits=3),
                         labels = label_percent(accuracy = 0.1) 
      )+
      scale_y_discrete(limits = rev(these_labels))+
      scale_color_manual(
        values = c("CZ" = wesanderson::wes_palettes$Darjeeling1[3],
                   "FR" = wesanderson::wes_palettes$Darjeeling1[2],
                   "IT" = wesanderson::wes_palettes$Darjeeling1[1],
                   "SW" = wesanderson::wes_palettes$Darjeeling1[4],
                   "POOL" = 'black'
        ),
        name = "Country",
        limits = c("CZ", "FR", "IT", "SW", "POOL"
        )
      ) +
      scale_shape_manual(
        values = c("IT" = 19, 
                   "FR" = 17, 
                   "SW" = 15, 
                   "CZ" = 18,
                   "POOL" = 16
        ),
        name = "Country",
        limits = c("CZ", "FR", "IT", "SW", "POOL"
        )
      ) +
      theme_minimal(base_size = 10) +  
      theme(
        legend.position = "right",  
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        plot.background = element_rect(fill = "white", color = NA)
      )
    
    
    
    v[[category]] = p
    
  }
  
  return(v)
}

full_analysis_bycountry = function(data,
                                   formula, #the conjoint formula
                                   effect=c("ATEs", "ACDEs", "EEs"), #the three possible effects to compute
                                   type=c("match", "nominal"), #whether we are considering the nominal attributes or the recoding match vs mismatch with the respondent
                                   estimator=c("mm","amce"), #marginal means and amces
                                   arm=c("natural", "ideology_match", "ideology_mismatch"), #natural mediation arm, or manipulated mediation arm with ideological match, 
                                   #or manipulated mediation arm with ideological mismatch
                                   subdir,
                                   leftlim=999,
                                   rightlim=999#the subdirectory where the plots will be saved
){
  
  # 
  ###### This function performs the whole analysis, draws the graphs and saves
  #them in the appropriate repositories. 
  #It calls the other functions previously defined plus the functions in cjregg and
  #patchwork
  
  #Notice that the function behaves slightly differently for EEs, that need to be
  #treated a bit differntly due to the complications
  
  effect=match.arg(effect)
  type=match.arg(type)
  estimator=match.arg(estimator)
  arm=match.arg(arm)
  
  
  
  if(effect!= "EEs")
  {
    ### The following does not take into account the clusteredness of the
    #data per country. To compute those clustered per country, see script
    #Analyses_pooled_data.R

    
    arm1 = ifelse(arm =="natural", type,
                  ifelse(arm =="ideology_match", "match", "mismatch")
    )
    
    
    effects_pooled= readRDS(paste0(
      gdrive_code,
      input_pooled_data,
      estimator,
      "_clustSE_",
      effect,
      "_",
      arm1,
      ".rds")
    )
    
    effects_pooled$cpd_country = "POOL"
    effects_pooled$BY = "POOL"
    
    effects_bycountry <- data |>
      filter(cpd_exparm2 == arm) |>
      cj(formula, 
         id = ~respid, 
         alpha = 0.05,
         by= ~cpd_country,
         estimate = estimator)
    
    effects_bycountry_90 <- data |>
      filter(cpd_exparm2 == arm) |>
      cj(formula, 
         id = ~respid, 
         alpha = 0.1,
         by= ~cpd_country,
         estimate = estimator)
    
    ci_90 = effects_bycountry_90[c("lower", "upper")]
    
    
    effects_bycountry_99 <- data |>
      filter(cpd_exparm2 == arm) |>
      cj(formula, 
         id = ~respid, 
         alpha = 0.01,
         by= ~cpd_country,
         estimate = estimator)
    
    ci_99 = effects_bycountry_99[c("lower", "upper")]
    
    effects_bycountry[c("lower90", "upper90")] = ci_90
    effects_bycountry[c("lower99", "upper99")] = ci_99
    
    
    
  }
  if(effect== "EEs")
  {
    
    ### The following does not take into account the clusteredness of the
    #data per country. To compute those clustered per country, see script
    #Analyses_pooled_data.R
    
    
  
    
    arm1 = ifelse(arm =="natural", type,
                  ifelse(arm =="ideology_match", "match", "mismatch")
    )
    
    effects_pooled= readRDS(paste0(
      gdrive_code,
      input_pooled_data,
      estimator,
      "_clustSE_",
      effect,
      "_",
      arm1,
      ".rds")
    )
    
    effects_pooled$cpd_country = "POOL"
    
    effects_bycountry =data.frame()
    
    for(context in c("IT", "FR", "SW", "CZ"))
    {
      temp_effects_bycountry <- data |>
        filter((cpd_exparm2 == "natural" | cpd_exparm2 == arm) & cpd_country == context) |>
        cj(formula,
           id = ~respid,
           alpha = 0.05,
           estimate = paste0(estimator, "_differences"),
           by = ~cpd_exparm)
      
      
      temp_effects_bycountry_90 <- data |>
        filter((cpd_exparm2 == "natural" | cpd_exparm2 == arm) & cpd_country == context) |>
        cj(formula,
           id = ~respid,
           alpha = 0.1,
           estimate = paste0(estimator, "_differences"),
           by = ~cpd_exparm)
      
      ci_90 = temp_effects_bycountry_90[c("lower", "upper")]
      
      temp_effects_bycountry_99 <- data |>
        filter((cpd_exparm2 == "natural" | cpd_exparm2 == arm) & cpd_country == context) |>
        cj(formula,
           id = ~respid,
           alpha = 0.01,
           estimate = paste0(estimator, "_differences"),
           by = ~cpd_exparm)
      
      ci_99 = temp_effects_bycountry_99[c("lower", "upper")]
      
      temp_effects_bycountry[c("lower90", "upper90")] = ci_90
      temp_effects_bycountry[c("lower99", "upper99")] = ci_99
      
      temp_effects_bycountry$cpd_country = context
      
      effects_bycountry=rbind(effects_bycountry, temp_effects_bycountry)
      
    }
    
  }
  
  # 
  effects = rbind(effects_bycountry, effects_pooled)
  
  
  effects=set_categories_and_levels_bycountry(effects,
                                              type,
                                              nominal_attributes=nominal_attributes)
  
  
  
  v = draw_plot_effects_bycountry(effects, 
                                  type = type, 
                                  categories=categories, 
                                  estimator=estimator, 
                                  y_labels=y_labels_plots,
                                  leftlim = leftlim,
                                  rightlim= rightlim)
  
  
  for(category in categories[1:3])
  {
    v[[category]]=v[[category]]#+patchwork::plot_annotation(title = paste(effect, "of the Parallel Design Conjoint Experiment, ", type),
    #                           caption= paste0(toupper(estimator), "s of the", arm, " mediation arm"))
    
    ggsave(paste0(output_wd,"estimations/", subdir, category, "_bycountry.png"), 
           v[[category]],
           dpi = 600,
           height = 4, 
           width = 8, create.dir = T)
    
    saveRDS(v[[category]], file = paste0(output_wd,"estimations/", subdir, category, "_bycountry.rds"))
    
  }
  
  p = (v[["Sociodemographics"]]+xlab("")+theme(
    legend.position = "none",  
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    plot.background = element_rect(fill = "white", color = NA)  
  ))/(v[["Psychological"]]+xlab("")+theme(
    legend.position = "none",  
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    plot.background = element_rect(fill = "white", color = NA)
  )
  )/v[["Lifestyle"]]
  
  if(type == "nominal")
  {
    p = p+plot_layout(heights = c(7,6,9))
  }
  
  ggsave(paste0(output_wd,"estimations/", subdir, "total", "_bycountry.png"), 
         p, 
         dpi = 600,
         height = 12, 
         width = 10, create.dir = T)
  
  saveRDS(effects, paste0(output_wd,"estimations/", subdir, "total", "_bycountry.rds"))
  
  effects$CI = paste0("(", round(effects$lower, digits = 3), "; ", round(effects$upper, digits = 3), ")")
  effects$CI_90 = paste0("(", round(effects$lower90, digits = 3), "; ", round(effects$upper90, digits = 3), ")")
  effects$CI_99 = paste0("(", round(effects$lower99, digits = 3), "; ", round(effects$upper99, digits = 3), ")")
  
  effects$estimate = round(effects$estimate, digits = 3)
  effects$std.error = round(effects$std.error, digits = 3)
  
  
  effects |> 
    select(BY, feature, level, estimate, std.error, CI, CI_90, CI_99) |>
    rename(
      Country = BY,
      Attribute = feature,
      `Attribute level` = level,
      Estimate = estimate,
      `Std. Error` = std.error,
      `95% Confidence Interval` = CI,
      `90% Confidence Interval` = CI_90,
      `99% Confidence Interval` = CI_99
    ) |>
    mutate(across(where(is.numeric), ~ signif(., digits = 3))) |>
    write_xlsx(paste0(output_wd,"estimations/", subdir, "total", "_bycountry.xlsx"))
  
}




draw_compared_effects_bycountry = function(ates, #the dataset with the ates
                                           #already disentangled by country+pooled
                                           acdes, #the dataset with the acdes
                                           #already disentangled by country+pooled
                                           ees, #the dataset with the ees
                                           #already disentangled by country+pooled
                                           type=c("match", "nominal"), #"match" or "nominal" 
                                           categories=c("Sociodemographics", "Psychological", "Lifestyle", "Political"), #vector of thee categories 
                                           #("sociodemo", "psycho", "lifestyle")
                                           estimator=c("mm", "amce", "mm_differences", "amce_differences"), #either amce, mm, or mm_differences
                                           y_labels=y_labels_plots,
                                           leftlim=999, #the left limit of the plot
                                           rightlim=999,#the right limit of the plot
                                           x_intercept=999 #the vertical line to signal the difference from the insignificance
){
  
  
  ### Function to draw lots comparing the effects of ATEs, ACDEs, and EEs.
  # It funtion similarly to draw_plot_effects_bycountry
  # in the single country script it doesnt exist since there it's only a variation 
  #of the draw_plot_effects function with the argument for_comparison = T. 
  #Here it is a function in its own right
  
  
  estimator=match.arg(estimator)
  type=match.arg(type)
  
  intercept = ifelse(estimator=="mm", 0.5, 0)
  
  
  ates_list = list()
  acdes_list = list()
  ees_list = list()
  
  #relative positions in the graphs
  increment = 1/6
  y_CZ=2*increment
  y_FR = increment
  y_POOL=0
  y_IT=-1*increment
  y_SW=-2*increment
  
  size_countries=0.8
  size_pool=0.8
  size_countries_point = 1.5
  size_pool_point = 2
  alpha_countries = 0.5
  
  fatten_countries = 1
  fatten_pool = 1
  
  if(estimator =="mm")
  {
    xlabel="Marginal Means"
  }
  else
  {
    xlabel="Average Marginal Component Effects"
  }
  
  
  
  #draw the ATEs plots
  for(category in categories[1:3])
  {
    
    data_IT=ates[ates$category == category & ates$cpd_country == "IT", ]
    data_FR=ates[ates$category == category & ates$cpd_country == "FR", ]
    data_CZ=ates[ates$category == category & ates$cpd_country == "CZ", ]
    data_SW=ates[ates$category == category & ates$cpd_country == "SW", ]
    data_POOL=ates[ates$category == category & ates$cpd_country == "POOL", ]
    
    these_labels = y_labels[[type]][[category]]
    

    
    ates_plot = ggplot()+
      geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_point(data=data_IT,
                 aes(x=estimate, y=level, col = "IT", shape = "IT"),
                 size = size_countries_point,
                 position = position_nudge(y = y_IT),
                 show.legend = T)+
      geom_point(data=data_FR,
                 aes(x=estimate, y=level, col = "FR", shape = "FR"),
                 size = size_countries_point,
                 position = position_nudge(y = y_FR),
                 show.legend = T)+
      geom_point(data=data_SW,
                 aes(x=estimate, y=level, col = "SW", shape = "SW"),
                 size = size_countries_point,
                 position = position_nudge(y = y_SW),
                 show.legend = T)+
      geom_point(data=data_CZ,
                 aes(x=estimate, y=level, col = "CZ", shape = "CZ"),
                 size = size_countries_point,
                 position = position_nudge(y = y_CZ),
                 show.legend = T)+
      geom_point(data=data_POOL,
                 aes(x=estimate, y=level, col = "POOL", shape = "POOL"),
                 size = size_pool_point,
                 position = position_nudge(y = y_POOL),
                 show.legend = T)+
      geom_linerange(data=data_IT,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "IT", shape = "IT"),
                     alpha = alpha_countries/2,
                     size = 3/4*size_countries,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_IT),
                     show.legend = T)+
      geom_linerange(data=data_IT,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "IT", shape = "IT"),
                     alpha = alpha_countries,
                     size = size_countries,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_IT),
                     show.legend = T)+
      geom_linerange(data=data_IT,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "IT", shape = "IT"),
                     alpha = alpha_countries*2,
                     size = 5/4*size_countries,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_IT),
                     show.legend = T)+
      geom_linerange(data=data_FR,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "FR", shape = "FR"),
                     alpha = alpha_countries/2,
                     size = 3/4*size_countries,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_FR),
                     show.legend = T)+
      geom_linerange(data=data_FR,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "FR", shape = "FR"),
                     alpha = alpha_countries,
                     size = size_countries,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_FR),
                     show.legend = T)+
      geom_linerange(data=data_FR,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "FR", shape = "FR"),
                     alpha = alpha_countries*2,
                     size = 5/4*size_countries,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_FR),
                     show.legend = T)+
      geom_linerange(data=data_SW,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "SW", shape = "SW"),
                     alpha = alpha_countries/2,
                     size = 3/4*size_countries,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_SW),
                     show.legend = T)+
      geom_linerange(data=data_SW,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "SW", shape = "SW"),
                     alpha = alpha_countries,
                     size = size_countries,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_SW),
                     show.legend = T)+
      geom_linerange(data=data_SW,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "SW", shape = "SW"),
                     alpha = alpha_countries*2,
                     size = 5/4*size_countries,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_SW),
                     show.legend = T)+
      geom_linerange(data=data_CZ,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "CZ", shape = "CZ"),
                     alpha = alpha_countries/2,
                     size = 3/4*size_countries,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_CZ),
                     show.legend = T)+
      geom_linerange(data=data_CZ,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "CZ", shape = "CZ"),
                     alpha = alpha_countries,
                     size = size_countries,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_CZ),
                     show.legend = T)+
      geom_linerange(data=data_CZ,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "CZ", shape = "CZ"),
                     alpha = alpha_countries*2,
                     size = 5/4*size_countries,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_CZ),
                     show.legend = T)+
      geom_linerange(data=data_POOL,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "POOL", shape = "POOL"),
                     alpha = alpha_countries/2,
                     size = size_pool/2,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_POOL),
                     show.legend = T)+
      geom_linerange(data=data_POOL,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "POOL", shape = "POOL"),
                     alpha = alpha_countries,
                     size = size_pool,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_POOL),
                     show.legend = T)+
      geom_linerange(data=data_POOL,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "POOL", shape = "POOL"),
                     alpha = alpha_countries*2,
                     size = size_pool*2,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_POOL),
                     show.legend = T)+
      geom_text(data=data_POOL,
                aes(x = estimate, y = level, 
                    label = scales::percent(estimate, accuracy = 0.1)),  
                hjust = -3, 
                size = 3,  
                color = "black") +
      ylab("")+
      xlab(xlabel)+
      scale_x_continuous(limits = c(leftlim, rightlim), 
                         breaks = round(seq(leftlim, rightlim,
                                            length.out = 9), 
                                        digits=3),
                         labels = label_percent(accuracy = 0.1) 
      )+    
      scale_y_discrete(limits = rev(these_labels))+
      scale_color_manual(
        values = c("CZ" = wesanderson::wes_palettes$Darjeeling1[3],
                   "FR" = wesanderson::wes_palettes$Darjeeling1[2],
                   "IT" = wesanderson::wes_palettes$Darjeeling1[1],
                   "SW" = wesanderson::wes_palettes$Darjeeling1[4],
                   "POOL" = 'black'
        ),
        name = "Country",
        limits = c("CZ", "FR", "IT", "SW", "POOL"
        )
      ) +
      scale_shape_manual(
        values = c("IT" = 19, 
                   "FR" = 17, 
                   "SW" = 15, 
                   "CZ" = 18,
                   "POOL" = 16
        ),
        name = "Country",
        limits = c("CZ", "FR", "IT", "SW", "POOL"
        )
      ) +
      theme_minimal(base_size = 10) +  
      theme(
        legend.position = "none",  
        axis.text.y = element_text(size = 10,
                                   hjust = 0.5, vjust=0.5),
        axis.title.y = element_text(size = 12),
        plot.background = element_rect(fill = "white", color = NA)  
      )
    
    
    
    ates_list[[category]] = ates_plot
    
  }
  
  
  #draw the acdes plots
  for(category in categories[1:3])
  {
    data_IT=acdes[acdes$category == category & acdes$cpd_country == "IT", ]
    data_FR=acdes[acdes$category == category & acdes$cpd_country == "FR", ]
    data_CZ=acdes[acdes$category == category & acdes$cpd_country == "CZ", ]
    data_SW=acdes[acdes$category == category & acdes$cpd_country == "SW", ]
    data_POOL=acdes[acdes$category == category & acdes$cpd_country == "POOL", ]
    
    #I don't want to plot the reference category here
    if(estimator=="amce")
    {
      these_labels = y_labels[[type]][[category]][!grepl("Mismatch",y_labels[[type]][[category]])] 
    }
    else
    {
      these_labels = y_labels[[type]][[category]]
    }
    
    
    acdes_plot = ggplot()+
      geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_point(data=data_IT,
                 aes(x=estimate, y=level, col = "IT", shape = "IT"),
                 size = size_countries_point,
                 position = position_nudge(y = y_IT),
                 show.legend = T)+
      geom_point(data=data_FR,
                 aes(x=estimate, y=level, col = "FR", shape = "FR"),
                 size = size_countries_point,
                 position = position_nudge(y = y_FR),
                 show.legend = T)+
      geom_point(data=data_SW,
                 aes(x=estimate, y=level, col = "SW", shape = "SW"),
                 size = size_countries_point,
                 position = position_nudge(y = y_SW),
                 show.legend = T)+
      geom_point(data=data_CZ,
                 aes(x=estimate, y=level, col = "CZ", shape = "CZ"),
                 size = size_countries_point,
                 position = position_nudge(y = y_CZ),
                 show.legend = T)+
      geom_point(data=data_POOL,
                 aes(x=estimate, y=level, col = "POOL", shape = "POOL"),
                 size = size_pool_point,
                 position = position_nudge(y = y_POOL),
                 show.legend = T)+
      geom_linerange(data=data_IT,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "IT", shape = "IT"),
                     alpha = alpha_countries/2,
                     size = 3/4*size_countries,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_IT),
                     show.legend = T)+
      geom_linerange(data=data_IT,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "IT", shape = "IT"),
                     alpha = alpha_countries,
                     size = size_countries,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_IT),
                     show.legend = T)+
      geom_linerange(data=data_IT,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "IT", shape = "IT"),
                     alpha = alpha_countries*2,
                     size = 5/4*size_countries,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_IT),
                     show.legend = T)+
      geom_linerange(data=data_FR,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "FR", shape = "FR"),
                     alpha = alpha_countries/2,
                     size = 3/4*size_countries,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_FR),
                     show.legend = T)+
      geom_linerange(data=data_FR,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "FR", shape = "FR"),
                     alpha = alpha_countries,
                     size = size_countries,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_FR),
                     show.legend = T)+
      geom_linerange(data=data_FR,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "FR", shape = "FR"),
                     alpha = alpha_countries*2,
                     size = 5/4*size_countries,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_FR),
                     show.legend = T)+
      geom_linerange(data=data_SW,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "SW", shape = "SW"),
                     alpha = alpha_countries/2,
                     size = 3/4*size_countries,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_SW),
                     show.legend = T)+
      geom_linerange(data=data_SW,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "SW", shape = "SW"),
                     alpha = alpha_countries,
                     size = size_countries,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_SW),
                     show.legend = T)+
      geom_linerange(data=data_SW,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "SW", shape = "SW"),
                     alpha = alpha_countries*2,
                     size = 5/4*size_countries,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_SW),
                     show.legend = T)+
      geom_linerange(data=data_CZ,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "CZ", shape = "CZ"),
                     alpha = alpha_countries/2,
                     size = 3/4*size_countries,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_CZ),
                     show.legend = T)+
      geom_linerange(data=data_CZ,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "CZ", shape = "CZ"),
                     alpha = alpha_countries,
                     size = size_countries,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_CZ),
                     show.legend = T)+
      geom_linerange(data=data_CZ,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "CZ", shape = "CZ"),
                     alpha = alpha_countries*2,
                     size = 5/4*size_countries,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_CZ),
                     show.legend = T)+
      geom_linerange(data=data_POOL,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "POOL", shape = "POOL"),
                     alpha = alpha_countries/2,
                     size = size_pool/2,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_POOL),
                     show.legend = T)+
      geom_linerange(data=data_POOL,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "POOL", shape = "POOL"),
                     alpha = alpha_countries,
                     size = size_pool,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_POOL),
                     show.legend = T)+
      geom_linerange(data=data_POOL,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "POOL", shape = "POOL"),
                     alpha = alpha_countries*2,
                     size = size_pool*2,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_POOL),
                     show.legend = T)+
      geom_text(data=data_POOL,
                aes(x = estimate, y = level, 
                    label = scales::percent(estimate, accuracy = 0.1)),  
                hjust = -3.5, 
                size = 3,  
                color = "black") + 
      ylab("")+
      xlab(xlabel)+
      scale_x_continuous(limits = c(leftlim, rightlim), 
                         breaks = round(seq(leftlim, rightlim,
                                            length.out = 9), 
                                        digits=3),
                         labels = label_percent(accuracy = 0.1)  
      )+ 
      scale_y_discrete(limits = rev(these_labels))+
      scale_color_manual(
        values = c("CZ" = wesanderson::wes_palettes$Darjeeling1[3],
                   "FR" = wesanderson::wes_palettes$Darjeeling1[2],
                   "IT" = wesanderson::wes_palettes$Darjeeling1[1],
                   "SW" = wesanderson::wes_palettes$Darjeeling1[4],
                   "POOL" = 'black'
        ),
        name = "Country",
        limits = c("CZ", "FR", "IT", "SW", "POOL"
        )
      ) +
      scale_shape_manual(
        values = c("IT" = 19, 
                   "FR" = 17, 
                   "SW" = 15, 
                   "CZ" = 18,
                   "POOL" = 16
        ),
        name = "Country",
        limits = c("CZ", "FR", "IT", "SW", "POOL"
        )
      ) +
      theme_minimal(base_size = 10) +  
      theme(
        legend.position = "none",  
        axis.text.y = element_text(size = 10,
                                   hjust = 0.5, vjust=0.5),
        axis.title.y = element_text(size = 12),
        plot.background = element_rect(fill = "white", color = NA)
      )
    
    
    
    acdes_list[[category]] = acdes_plot
    
  }
  
  
  if(estimator =="mm")
  {
    xlabel="Marginal Means"
  }
  else
  {
    xlabel="Average Marginal Component Effects"
  }
  
  #draw the ees plots
  for(category in categories[1:3])
  {
    data_IT=ees[ees$category == category & ees$cpd_country == "IT", ]
    data_FR=ees[ees$category == category & ees$cpd_country == "FR", ]
    data_CZ=ees[ees$category == category & ees$cpd_country == "CZ", ]
    data_SW=ees[ees$category == category & ees$cpd_country == "SW", ]
    data_POOL=ees[ees$category == category & ees$cpd_country == "POOL", ]
    
    if(estimator=="amce")
    {
      these_labels = y_labels[[type]][[category]][!grepl("Mismatch",y_labels[[type]][[category]])] 
    }
    else
    {
      these_labels = y_labels[[type]][[category]]
    }
    
    
    ees_plot = ggplot()+
      geom_vline(aes(xintercept=intercept), col="black", alpha=1/4)+
      geom_point(data=data_IT,
                 aes(x=estimate, y=level, col = "IT", shape = "IT"),
                 size = size_countries_point,
                 position = position_nudge(y = y_IT),
                 show.legend = T)+
      geom_point(data=data_FR,
                 aes(x=estimate, y=level, col = "FR", shape = "FR"),
                 size = size_countries_point,
                 position = position_nudge(y = y_FR),
                 show.legend = T)+
      geom_point(data=data_SW,
                 aes(x=estimate, y=level, col = "SW", shape = "SW"),
                 size = size_countries_point,
                 position = position_nudge(y = y_SW),
                 show.legend = T)+
      geom_point(data=data_CZ,
                 aes(x=estimate, y=level, col = "CZ", shape = "CZ"),
                 size = size_countries_point,
                 position = position_nudge(y = y_CZ),
                 show.legend = T)+
      geom_point(data=data_POOL,
                 aes(x=estimate, y=level, col = "POOL", shape = "POOL"),
                 size = size_pool_point,
                 position = position_nudge(y = y_POOL),
                 show.legend = T)+
      geom_linerange(data=data_IT,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "IT", shape = "IT"),
                     alpha = alpha_countries/2,
                     size = 3/4*size_countries,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_IT),
                     show.legend = T)+
      geom_linerange(data=data_IT,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "IT", shape = "IT"),
                     alpha = alpha_countries,
                     size = size_countries,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_IT),
                     show.legend = T)+
      geom_linerange(data=data_IT,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "IT", shape = "IT"),
                     alpha = alpha_countries*2,
                     size = 5/4*size_countries,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_IT),
                     show.legend = T)+
      geom_linerange(data=data_FR,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "FR", shape = "FR"),
                     alpha = alpha_countries/2,
                     size = 3/4*size_countries,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_FR),
                     show.legend = T)+
      geom_linerange(data=data_FR,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "FR", shape = "FR"),
                     alpha = alpha_countries,
                     size = size_countries,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_FR),
                     show.legend = T)+
      geom_linerange(data=data_FR,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "FR", shape = "FR"),
                     alpha = alpha_countries*2,
                     size = 5/4*size_countries,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_FR),
                     show.legend = T)+
      geom_linerange(data=data_SW,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "SW", shape = "SW"),
                     alpha = alpha_countries/2,
                     size = 3/4*size_countries,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_SW),
                     show.legend = T)+
      geom_linerange(data=data_SW,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "SW", shape = "SW"),
                     alpha = alpha_countries,
                     size = size_countries,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_SW),
                     show.legend = T)+
      geom_linerange(data=data_SW,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "SW", shape = "SW"),
                     alpha = alpha_countries*2,
                     size = 5/4*size_countries,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_SW),
                     show.legend = T)+
      geom_linerange(data=data_CZ,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "CZ", shape = "CZ"),
                     alpha = alpha_countries/2,
                     size = 3/4*size_countries,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_CZ),
                     show.legend = T)+
      geom_linerange(data=data_CZ,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "CZ", shape = "CZ"),
                     alpha = alpha_countries,
                     size = size_countries,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_CZ),
                     show.legend = T)+
      geom_linerange(data=data_CZ,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "CZ", shape = "CZ"),
                     alpha = alpha_countries*2,
                     size = 5/4*size_countries,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_CZ),
                     show.legend = T)+
      geom_linerange(data=data_POOL,
                     aes(x=estimate, xmin=lower99, xmax=upper99, 
                         y=level, col = "POOL", shape = "POOL"),
                     alpha = alpha_countries/2,
                     size = size_pool/2,
                     fatten=fatten_countries/2,
                     position = position_nudge(y = y_POOL),
                     show.legend = T)+
      geom_linerange(data=data_POOL,
                     aes(x=estimate, xmin=lower, xmax=upper, 
                         y=level, col = "POOL", shape = "POOL"),
                     alpha = alpha_countries,
                     size = size_pool,
                     fatten=fatten_countries,
                     position = position_nudge(y = y_POOL),
                     show.legend = T)+
      geom_linerange(data=data_POOL,
                     aes(x=estimate, xmin=lower90, xmax=upper90, 
                         y=level, col = "POOL", shape = "POOL"),
                     alpha = alpha_countries*2,
                     size = size_pool*2,
                     fatten=fatten_countries*2,
                     position = position_nudge(y = y_POOL),
                     show.legend = T)+
      geom_text(data=data_POOL,
                aes(x = estimate, y = level, 
                    label = scales::percent(estimate, accuracy = 0.1)),  
                hjust = -3.5, 
                size = 3,
                color = "black") + 
      ylab("")+
      xlab(xlabel)+
      scale_x_continuous(limits = c(leftlim, rightlim), 
                         breaks = round(seq(leftlim, rightlim,
                                            length.out = 9), 
                                        digits=3),
                         labels = label_percent(accuracy = 0.1) 
      )+ 
      scale_y_discrete(limits = rev(these_labels))+
      scale_color_manual(
        values = c("CZ" = wesanderson::wes_palettes$Darjeeling1[3],
                   "FR" = wesanderson::wes_palettes$Darjeeling1[2],
                   "IT" = wesanderson::wes_palettes$Darjeeling1[1],
                   "SW" = wesanderson::wes_palettes$Darjeeling1[4],
                   "POOL" = 'black'
        ),
        name = "Country",
        limits = c("CZ", "FR", "IT", "SW", "POOL"
        )
      ) +
      scale_shape_manual(
        values = c("IT" = 19, 
                   "FR" = 17, 
                   "SW" = 15, 
                   "CZ" = 18,
                   "POOL" = 16
        ),
        name = "Country",
        limits = c("CZ", "FR", "IT", "SW", "POOL"
        )
      ) +
      theme_minimal(base_size = 10) +  # Adjust font size for better readability
      theme(
        legend.position = "right",  # You can change this to "top", "bottom", etc.
        axis.text.y = element_text(size = 10, #angle = 90,
                                   hjust = 0.5, vjust=0.5),
        axis.title.y = element_text(size = 12),
        plot.background = element_rect(fill = "white", color = NA)  # White background
      )
    
    
    ees_list[[category]] = ees_plot
    
  }
  
  
  
  #Now I make a list containing for each kind of effect the (three) plots
  #related to that effect (the different attribute category)
  #This list is the argument that then gets returned to the compare_effects_bycountry
  #function 
  
  full_plot_list = list(ates_plots = ates_list,
                        acdes_plots=acdes_list,
                        ees_plots=ees_list
  )
  
  return(full_plot_list)
} 


full_match_effects_bycountry = function(data, 
                                        formula, 
                                        exparm){
  #function to draw and save the plots related to the effect of the number of matches
  # (regardless of the actual attribute displayed) into the probaility of 
  #selecting someone as their conversation partners (with and without politics,
  #depending on the experimental arm selected)
  
  # 
  

  full_df = data.frame()
  for(context_loc in c("IT", "FR","SW","CZ",# "POOL"
  )
  )
  {
    if(context_loc != "POOL")
    {
      #filter the data by country and experimental arm 
      filtered_data = data |>
        filter(cpd_country == context_loc & cpd_exparm == exparm)
      
    }
    else
    {
      #filter the data by experimental arm (I want the pooled data)
      filtered_data = data |>
        filter(cpd_exparm == exparm)
    }
    
    #make sure respid is a factor
    filtered_data$respid = as.factor(filtered_data$respid)
    
    #I fit a multilevel model (random effects with respID)
    
    model =  glmer(cpd_chosen ~ cpd_n_matches +
                     cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
                     cpd_consc + cpd_ope +
                     cpd_diet + cpd_animal + cpd_holiday + 
                     (1 | respid),  # Random intercept for each respondent
                   data = filtered_data,
                   family = binomial)
    
    #using the library ggeffects I make the prediction for the different
    #values of cpd_n_matches when the other variables are in the reference category
    predictions = as.data.frame(ggpredict(model, terms = "cpd_n_matches"))
    
    # Convert predictions object to a data frame
    effect_df <- data.frame(
      x = predictions$x,  
      fit = predictions$predicted,   
      lower = predictions$conf.low, 
      upper = predictions$conf.high 
    )
    
    #I add the country variable because then I row bind all the datasets created
    #by the loop toghether
    effect_df$cpd_country = context_loc
    
    #I bind the datasets
    full_df=rbind(full_df, effect_df)
    
  }
  
  # Create caterpillar plot
  p = ggplot() +
    geom_pointrange(data = full_df[full_df$cpd_country == "IT", ], 
                    aes(x = x, y = fit, ymin = lower, ymax = upper, 
                        col="IT", shape="IT"),
                    position = position_nudge(x = -1/5)) +
    geom_pointrange(data = full_df[full_df$cpd_country == "FR", ], 
                    aes(x = x, y = fit, ymin = lower, ymax = upper, 
                        col="FR", shape="FR"),
                    position = position_nudge(x = -1/10)) + 
    geom_pointrange(data = full_df[full_df$cpd_country == "SW", ], 
                    aes(x = x, y = fit, ymin = lower, ymax = upper, 
                        col="SW", shape="SW"),
                    position = position_nudge(x = 0)) + 
    geom_pointrange(data = full_df[full_df$cpd_country == "CZ", ], 
                    aes(x = x, y = fit, ymin = lower, ymax = upper, 
                        col="CZ", shape="CZ"),
                    position = position_nudge(x = 1/10)) + 
    geom_pointrange(data = full_df[full_df$cpd_country == "POOL", ], 
                    aes(x = x, y = fit, ymin = lower, ymax = upper, 
                        col="POOL", shape="POOL"),
                    position = position_nudge(x = 1/5)) + 
    labs(
      x = "Number of attribute matches",
      y = "Marginal effect on the probability of choosing the profile",
      title = ""
    )+
    scale_x_continuous(seq(1, 1+lengths(gregexpr("\\+", as.character(formula)[3])),by=1))+
    scale_color_manual(
      values = c("IT" = wesanderson::wes_palettes$Darjeeling1[1],
                 "FR" = wesanderson::wes_palettes$Darjeeling1[2],
                 "SW" = wesanderson::wes_palettes$Darjeeling1[3],
                 "CZ" = wesanderson::wes_palettes$Darjeeling1[4],
                 "POOL" = 'black'),
      name = "Country",
      limits = c("IT", "FR", "SW", "CZ", "POOL")
    ) +
    scale_shape_manual(
      values = c("IT" = 19, 
                 "FR" = 17, 
                 "SW" = 15, 
                 "CZ" = 18, 
                 "POOL" = 1),
      name = "Country",
      limits = c("IT", "FR", "SW", "CZ", "POOL")
    ) +
    theme(
      legend.position = "right", 
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(size = 12)
    ) 
  
  #saving the plots
  
  ggsave(paste0(output_wd,"estimations/", 
                subdir,"bycountry_", exparm, ".png"), 
         p, 
         dpi = 600,
         height = 10, 
         width = 10, create.dir = T)
  
  saveRDS(p, file = paste0(output_wd,"estimations/", 
                           subdir,"bycountry_", exparm, ".rds"))
  
  
  
}

compare_effects_bycountry = function(data,
                                     formula, 
                                     type=c("match", "nominal"), #whether we are considering 
                                     #the nominal attributes or the recoding match vs mismatch with the respondent
                                     estimator=c("mm","amce"), #marginal means and amces
                                     arm=c("ideology_match", "ideology_mismatch"), #manipulated mediation arm with ideological match, 
                                     #or manipulated mediation arm with ideological mismatch
                                     subdir,
                                     leftlim=999,
                                     rightlim=999,
                                     x_intercept=999#the subdirectory where the plots will be saved
){
  
  
  ###### This function ends up drawing the graphs with the three effects compared like
  #Acharya et al
  
  type=match.arg(type)
  estimator=match.arg(estimator)
  arm=match.arg(arm)
  
  arm1 = ifelse(arm =="natural", type,
                ifelse(arm =="ideology_match", "match", "mismatch")
  )
  
  
  ### Compute the ATEs
  ates <- data |>
    filter(cpd_exparm2 == "natural") |>
    cj(formula, 
       id = ~respid,
       estimate = estimator,
       by= ~cpd_country)
  
  ates_90 <- data |>
    filter(cpd_exparm2 == "natural") |>
    cj(formula, 
       id = ~respid,
       alpha = 0.1,
       estimate = estimator,
       by= ~cpd_country)
  
  ci_90 = ates_90[c("lower", "upper")]
  
  
  ates_99 <- data |>
    filter(cpd_exparm2 == "natural") |>
    cj(formula, 
       id = ~respid,
       alpha = 0.01,
       estimate = estimator,
       by= ~cpd_country)
  
  ci_99 = ates_99[c("lower", "upper")]
  
  ates[c("lower90", "upper90")] = ci_90
  ates[c("lower99", "upper99")] = ci_99
  
  
  ates_pooled= readRDS(paste0(
    gdrive_code,
    input_pooled_data,
    estimator,
    "_clustSE_",
    "ATEs",
    "_",
    type,
    ".rds"))
  
  ates_pooled$cpd_country = "POOL"
  ates_pooled$BY = "POOL"
  
  ates=rbind(ates, ates_pooled)
  
  ###Compute the ACDEs
  acdes <- data |>
    filter(cpd_exparm2 == arm) |>
    cj(formula, 
       id = ~respid,
       estimate = estimator,
       by = ~cpd_country)
  
  acdes_90 <- data |>
    filter(cpd_exparm2 == arm) |>
    cj(formula, 
       id = ~respid,
       alpha = 0.1,
       estimate = estimator,
       by= ~cpd_country)
  
  ci_90 = acdes_90[c("lower", "upper")]
  
  
  acdes_99 <- data |>
    filter(cpd_exparm2 == arm) |>
    cj(formula, 
       id = ~respid,
       alpha = 0.01,
       estimate = estimator,
       by= ~cpd_country)
  
  ci_99 = acdes_99[c("lower", "upper")]
  
  acdes[c("lower90", "upper90")] = ci_90
  acdes[c("lower99", "upper99")] = ci_99
  
  
  acdes_pooled= readRDS(paste0(
    gdrive_code,
    input_pooled_data,
    estimator,
    "_clustSE_",
    "ACDEs",
    "_",
    arm1,
    ".rds"))
  
  
  acdes_pooled$cpd_country = "POOL"
  acdes_pooled$BY = "POOL"
  
  acdes=rbind(acdes, acdes_pooled)
  
  if(estimator == "amce")
  {
    #avoid to plot the reference category
    #acdes[is.na(acdes$std.error), ]$estimate = NA
    acdes = acdes[!is.na(acdes$std.error), ]
  }
  
  
  ### Compute the EEs
  #### 
  
  ees = data.frame()
  
  for(context in c("IT","FR","SW","CZ"))
  {
    ees_country = data |>
      filter(cpd_country == context & (cpd_exparm2 == "natural" | cpd_exparm2 == arm)) |>
      cj(formula,
         id = ~respid,
         estimate = paste0(estimator, "_differences"),
         by = ~cpd_exparm)
    
    ees_country_90 <- data |>
      filter(cpd_country == context & (cpd_exparm2 == "natural" | cpd_exparm2 == arm)) |>
      cj(formula, 
         id = ~respid,
         alpha = 0.1,
         estimate = paste0(estimator, "_differences"),
         by = ~cpd_exparm)
    
    ci_90 = ees_country_90[c("lower", "upper")]
    
    
    ees_country_99 <- data |>
      filter(cpd_country == context & (cpd_exparm2 == "natural" | cpd_exparm2 == arm)) |>
      cj(formula, 
         id = ~respid,
         alpha = 0.01,
         estimate = paste0(estimator, "_differences"),
         by = ~cpd_exparm)
    
    ci_99 = ees_country_99[c("lower", "upper")]
    
    ees_country[c("lower90", "upper90")] = ci_90
    ees_country[c("lower99", "upper99")] = ci_99
    
    ees_country$cpd_country = context
    
    ees=rbind(ees, ees_country)
  }
  

  ees_pooled= readRDS(paste0(
    gdrive_code,
    input_pooled_data,
    estimator,
    "_clustSE_",
    "EEs",
    "_",
    arm1,
    ".rds"))
  
  ees_pooled$cpd_country = "POOL"
  
  
  
  ees = rbind(ees, ees_pooled)
  
  if(estimator == "amce")
  {
    #avoid to plot the reference category
    #acdes[is.na(acdes$std.error), ]$estimate = NA
    ees = ees[!is.na(ees$std.error), ]
  }
  
  
  
  ##Set the categories and levels for the three datasets
  
  ates = set_categories_and_levels_bycountry(ates,
                                             type,
                                             nominal_attributes=nominal_attributes)
  
  acdes = set_categories_and_levels_bycountry(acdes,
                                              type,
                                              nominal_attributes=nominal_attributes)
  
  ees = set_categories_and_levels_bycountry(ees,
                                            type,
                                            nominal_attributes=nominal_attributes)
  
  #Call draw effects with the for_comparison argument ==T, which means that it will return
  #the vector separately, not the already assembled immage
  
  
  x_intercept = ifelse(estimator!="mm_differences", 0, 0.5)
  
  plots = draw_compared_effects_bycountry(ates,
                                          acdes,
                                          ees,
                                          type = type,
                                          categories=categories,
                                          estimator=estimator,
                                          y_labels=y_labels_plots,
                                          leftlim=leftlim,
                                          rightlim=rightlim,
                                          x_intercept = x_intercept
  )
  pates=plots$ates_plots
  
  pacdes=plots$acdes_plots
  pees=plots$ees_plots
  #Now I assemble three plots (for each category) so that they are easy to compare
  
  
  p_socio = (pates[["Sociodemographics"]]+labs(title = "Total Effects (ATEs)"))/(pacdes[["Sociodemographics"]]+labs(title = "Direct Effects (ACDEs)"))/(pees[["Sociodemographics"]]+labs(title = "Eliminated Effects (EEs)"))
  
  p_psycho = (pates[["Psychological"]]+labs(title = "Total Effects (ATEs)"))/(pacdes[["Psychological"]]+labs(title = "Direct Effects (ACDEs)"))/(pees[["Psychological"]]+labs(title = "Eliminated Effects (EEs)"))
  
  p_lifestyle = (pates[["Lifestyle"]]+labs(title = "Total Effects (ATEs)"))/(pacdes[["Lifestyle"]]+labs(title = "Direct Effects (ACDEs)"))/(pees[["Lifestyle"]]+labs(title = "Eliminated Effects (EEs)"))
  
  
  
  
  #I save the three plots
  
  
  ggsave(paste0(output_wd,"estimations/", subdir,"socio_bycountry.png"), 
         p_socio,
         dpi = 600,
         height = 10, 
         width = 10, create.dir = T)
  
  ates$CI = paste0("(", round(ates$lower, digits = 3), "; ", round(ates$upper, digits = 3), ")")
  ates$CI_90 = paste0("(", round(ates$lower90, digits = 3), "; ", round(ates$upper90, digits = 3), ")")
  ates$CI_99 = paste0("(", round(ates$lower99, digits = 3), "; ", round(ates$upper99, digits = 3), ")")
  
  ates$estimate = round(ates$estimate, digits = 3)
  ates$std.error = round(ates$std.error, digits = 3)
  
  ates |> 
    select(cpd_country, feature, level, estimate, std.error, CI, CI_90, CI_99) |>
    filter(!grepl("Mismatch", level)) |>
    rename(
      Country = cpd_country,
      Attribute = feature,
      `Attribute level` = level,
      Estimate = estimate,
      `Std. Error` = std.error,
      `95% Confidence Interval` = CI,
      `90% Confidence Interval` = CI_90,
      `99% Confidence Interval` = CI_99
    ) |>
    mutate(across(where(is.numeric), ~ signif(., digits = 3))) |>
    write_xlsx(paste0(output_wd,"estimations/", subdir,"ates.xlsx"))
  
  ggsave(paste0(output_wd,"estimations/", subdir,"psycho_bycountry.png"), 
         p_psycho, 
         dpi = 600,
         height = 10, 
         width = 10)
  
  acdes$CI = paste0("(", round(acdes$lower, digits = 3), "; ", round(acdes$upper, digits = 3), ")")
  acdes$CI_90 = paste0("(", round(acdes$lower90, digits = 3), "; ", round(acdes$upper90, digits = 3), ")")
  acdes$CI_99 = paste0("(", round(acdes$lower99, digits = 3), "; ", round(acdes$upper99, digits = 3), ")")
  
  acdes$estimate = round(acdes$estimate, digits = 3)
  acdes$std.error = round(acdes$std.error, digits = 3)
  
  acdes |> 
    select(cpd_country, feature, level, estimate, std.error, CI, CI_90, CI_99) |>
    filter(!grepl("Mismatch", level)) |>
    rename(
      Country = cpd_country,
      Attribute = feature,
      `Attribute level` = level,
      Estimate = estimate,
      `Std. Error` = std.error,
      `95% Confidence Interval` = CI,
      `90% Confidence Interval` = CI_90,
      `99% Confidence Interval` = CI_99
    ) |>
    mutate(across(where(is.numeric), ~ signif(., digits = 3))) |>
    write_xlsx(paste0(output_wd,"estimations/", subdir,"acdes.xlsx"))  
  
  ggsave(paste0(output_wd,"estimations/", subdir,"lifestyle_bycountry.png"), 
         p_lifestyle, 
         dpi = 600,
         height = 10, 
         width = 10, create.dir = T)
  
  ees$CI = paste0("(", round(ees$lower, digits = 3), "; ", round(ees$upper, digits = 3), ")")
  ees$CI_90 = paste0("(", round(ees$lower90, digits = 3), "; ", round(ees$upper90, digits = 3), ")")
  ees$CI_99 = paste0("(", round(ees$lower99, digits = 3), "; ", round(ees$upper99, digits = 3), ")")
  
  ees$estimate = round(ees$estimate, digits = 3)
  ees$std.error = round(ees$std.error, digits = 3)
  
  ees |> 
    select(cpd_country, feature, level, estimate, std.error, CI, CI_90, CI_99) |>
    filter(!grepl("Mismatch", level)) |>
    rename(
      Country = cpd_country,
      Attribute = feature,
      `Attribute level` = level,
      Estimate = estimate,
      `Std. Error` = std.error,
      `95% Confidence Interval` = CI,
      `90% Confidence Interval` = CI_90,
      `99% Confidence Interval` = CI_99
    ) |>
    mutate(across(where(is.numeric), ~ signif(., digits = 3))) |>
    write_xlsx(paste0(output_wd,"estimations/", subdir,"ees.xlsx"))    
  
  return(plots)
  
}


## Defining global variables ####



#Our categories of apolitical traits
categories= c("Sociodemographics", "Psychological", "Lifestyle", "Political")

#Our levels regarding match and mismatches (for labeling)
y_labels_match = list(Sociodemographics=c("Gender Mismatch", "Gender Match",
                                          "Age Mismatch", "Age Match",
                                          "Educ Mismatch", "Educ Match",
                                          "Regionfeel Mismatch", "Regionfeel Match"),
                      Psychological = c("Consc Mismatch", "Consc Match", 
                                        "Ope Mismatch", "Ope Match"),
                      Lifestyle =c("Diet Mismatch", "Diet Match",
                                   "Pet Mismatch", "Pet Match",
                                   "Holiday Mismatch", "Holiday Match"
                      ),
                      Political = c("Ideology Mismatch",
                                    "Ideology Match"))


nominal_attributes= c("Gender", "Gender",
                      "Age", "Age","Age",
                      "Education","Education",
                      "Regionfeel","Regionfeel","Regionfeel",
                      "Regionfeel",
                      "Regionfeel","Regionfeel","Regionfeel","Regionfeel",
                      "Regionfeel","Regionfeel","Regionfeel",
                      "Conscientiousness","Conscientiousness","Conscientiousness",
                      "Openness","Openness","Openness",
                      "Diet","Diet","Diet",
                      "Pet","Pet","Pet",
                      "Holiday","Holiday","Holiday")

levels_vector= c("Woman", "Man",
                 "Under 35", "Between 35 and 59","Over 60",
                 "Degree","No degree",
                 "Cechia (CZ)","Center (IT)", "Götaland (SW)",
                 "Moravia (CZ)", "No Paris (FR)", "North (IT)",
                 "Norrland (SW)", "Paris (FR)", "Prague (CZ)",
                 "South (IT)", "Svealand (SW)",
                 "High Consc.","Med. Consc.","Low Consc.",
                 "High Ope.","Med. Ope.","Low Ope.",
                 "Omnivore","Vegetarian","Vegan",
                 "Cat","Dog","No pet",
                 "City","Outdoor","Relax")

levels_vector2= c("Woman", "Man",
                  "Under 35", "Between 35 and 59","Over 60",
                  "Degree","No degree",
                  "High Consc.","Med. Consc.","Low Consc.",
                  "High Ope.","Med. Ope.","Low Ope.",
                  "Omnivore","Vegetarian","Vegan",
                  "Cat","Dog","No pet",
                  "City","Outdoor","Relax")

y_labels_nominal = list(Sociodemographics = c("Woman", "Man",
                                              "Under 35", "Between 35 and 59","Over 60",
                                              "Degree","No degree",
                                              "Cechia (CZ)","Center (IT)", "Gotland (SW)",
                                              "Moravia (CZ)", "No Paris (FR)", "North (IT)",
                                              "Norrland (SW)", "Paris (FR)", "Prague (CZ)",
                                              "South (IT)", "Svealand (SW)"),
                        Psychological = c("High Consc.","Med. Consc.","Low Consc.",
                                          "High Ope.","Med. Ope.","Low Ope."),
                        Lifestyle = c("Omnivore","Vegetarian","Vegan",
                                      "Cat","Dog","No pet",
                                      "City","Outdoor","Relax"),
                        Political = c("Right-wing",
                                      "Left-wing",
                                      "Center",
                                      "Not collocated")
)

y_labels_ideology = list(Political = c("Center",
                                       "Right-wing",
                                       "Left-wing",
                                       "Not collocated",
                                       "Ideology Mismatch",
                                       "Ideology Match")
)

y_labels_plots=list(match=y_labels_match, 
                    nominal=y_labels_nominal)

#Nominal attributes (here called nominal_attributes)


formula_match = cpd_chosen ~  cpd_match_gender + cpd_match_age + 
  cpd_match_educ + cpd_match_regionfeel +
  cpd_match_consc + cpd_match_ope +
  cpd_match_diet + cpd_match_animal + cpd_match_holiday

formula_nominal = cpd_chosen ~  cpd_gender + cpd_age + cpd_educ + cpd_regionfeel +
  cpd_consc + cpd_ope +
  cpd_diet + cpd_animal + cpd_holiday

dataset_rep = "repository_where_you_store_RDS_files/"
gdrive_code = "yourmain_Googledrive_address_if_you_use_it/"

Clean = T #if you are working on the clean dataset
output_wd = paste0(gdrive_code, "repository_where_save_outputs", "Clean_", clean, "/")


if(!dir.exists(file.path(output_wd))) 
{
  dir.create(file.path(output_wd))
}

data = readRDS("~/data.RDS")



input_pooled_data = "path_repository_with_dataset_produced_thorugh_script_for_pooled_analyses/"

# For Robustness Checks
# data = data |>
#   filter(cpd_task_number == 1)

## ATEs (MATCH/MISMATCH) ####

# Estimation: The marginal mean associated with S_i^k=1 for respondents 
#in the natural mediation arm

# Interpretation: The total effect that observing similarity in attribute k 
#has on the willingness to engage in political conversation when no political 
#information is given to the respondent, whether through political inferences or not.

subdir = "ATEs/match/MMs/"


full_analysis_bycountry(data, 
                        formula_match,
                        "ATEs",
                        "match",
                        "mm",
                        "natural",
                        leftlim=0.35,
                        rightlim=0.65,
                        subdir)


### Same as before, but with AMCes (for appendix)
subdir = "ATEs/match/AMCEs/"

full_analysis_bycountry(data, 
                        formula_match,
                        "ATEs",
                        "match",
                        "amce",
                        "natural",
                        leftlim=-0.2,
                        rightlim=0.2,
                        subdir)



############ ATEs (nominal value)

subdir = "ATEs/nominal/MMs/"


full_analysis_bycountry(data, 
                        formula_nominal,
                        "ATEs",
                        "nominal",
                        "mm",
                        "natural",
                        leftlim=0.35,
                        rightlim=0.65,
                        subdir)



## ADCEs (MATCH/MISMATCH) #####

#ESTIMATION
# The marginal mean associated with S_i^k=1 for respondents 
# in the maniulated mediation arm with ideological similarity condition

# The marginal mean associated with S_i^k=1 for respondents in the 
#maniulated mediation arm with ideological dissimilarity condition

# INTERPRETATION
#The effects that similarity in each attribute k has on the willingness to
#engage in political conversations that is due neither to mediation nor to 
#interaction with political inferences.

### ACDEs for ideological match with MM ####

subdir = "ACDEs/match/MMs/"


full_analysis_bycountry(data, 
                        formula_match,
                        "ACDEs",
                        "match",
                        "mm",
                        "ideology_match",
                        leftlim=0.4,
                        rightlim=0.6,
                        subdir)



### ACDEs for ideological mismatch with MM ####

subdir = "ACDEs/mismatch/MMs/"


full_analysis_bycountry(data, 
                        formula_match,
                        "ACDEs",
                        "match",
                        "mm",
                        "ideology_mismatch",
                        leftlim=0.4,
                        rightlim=0.6,
                        subdir)

### ACDEs for ideological match with AMCE ####

subdir = "ACDEs/match/AMCEs/"

full_analysis_bycountry(data, 
                        formula_match,
                        "ACDEs",
                        "match",
                        "amce",
                        "ideology_match",
                        leftlim=-0.1,
                        rightlim=0.1,
                        subdir)

### ACDEs for ideological mismatch with AMCE ####

subdir = "ACDEs/mismatch/AMCEs/"

full_analysis_bycountry(data, 
                        formula_match,
                        "ACDEs",
                        "match",
                        "amce",
                        "ideology_mismatch",
                        leftlim=-0.1,
                        rightlim=0.1,
                        subdir)



## Eliminated Effects ####

#ESTIMATION
# Point estimation: the difference between ATE and ACDE. 
#for standard errors see Acharya et al. (2018) Or Lòpez-Ortega 2023

#INTERPRETATION
# The portion of the ATE explained by political inferences, either through 
#mediation or through interaction between S_i^k and the specific ideology inferred.


##### ELIMINATED EFFECTS WITH MM FOR IDEOLOGICAL MATCH

subdir = "EEs/match/MMs/"

full_analysis_bycountry(data, 
                        formula_match,
                        "EEs",
                        "match",
                        "mm",
                        "ideology_match",
                        leftlim=-0.2,
                        rightlim=0.2,
                        subdir)


##### ELIMINATED EFFECTS WITH MM FOR IDEOLOGICAL MISMATCH

subdir = "EEs/mismatch/MMs/"

full_analysis_bycountry(data, 
                        formula_match,
                        "EEs",
                        "match",
                        "mm",
                        "ideology_mismatch",
                        leftlim=-0.2,
                        rightlim=0.2,
                        subdir)


##### ELIMINATED EFFECTS WITH AMCE FOR IDEOLOGICAL MATCH

subdir = "EEs/match/AMCEs/"

full_analysis_bycountry(data, 
                        formula_match,
                        "EEs",
                        "match",
                        "amce",
                        "ideology_match",
                        leftlim=-0.2,
                        rightlim=0.2,
                        subdir)

##### ELIMINATED EFFECTS WITH AMCE FOR IDEOLOGICAL MISMATCH

subdir = "EEs/mismatch/AMCEs/"

full_analysis_bycountry(data, 
                        formula_match,
                        "EEs",
                        "match",
                        "amce",
                        "ideology_mismatch",
                        leftlim=-0.2,
                        rightlim=0.2,
                        subdir)



### Now I want to have the three effects close to each other. How do I do that?

## Compare effects ####

subdir="CompareEffects/Ideology_match/"


plots_match = compare_effects_bycountry(data,
                                        formula_match,
                                        type="match", #whether we are considering the nominal attributes or the recoding match vs mismatch with the respondent
                                        estimator="amce", #marginal means and amces
                                        arm="ideology_match", #manipulated mediation arm with ideological match, 
                                        #or manipulated mediation arm with ideological mismatch
                                        subdir,#the subdirectory where the plots will be saved
                                        leftlim=-0.25,
                                        rightlim=0.25#,
                                        #x_intercept=0.5
)
