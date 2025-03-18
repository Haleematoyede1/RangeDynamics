#initial models on which the final models are based on

library(purrr)

models <- list(
  model1 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDVI +meanNDWI, data = reportRate, family = binomial),
  model2 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDVI +meanNDWI + meanMaxTemp , data = reportRate, family = binomial),
  model3 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDVI +meanNDWI + meanMinTemp , data = reportRate, family = binomial),
  model4 <- glm(cbind(detections, failures) ~ meanPrecp +meanNDWI + mean_slope , data = reportRate, family = binomial),
  model5 <- glm(cbind(detections, failures) ~ meanMinTemp + meanMaxTemp + meanNDVI , data = reportRate, family = binomial),
  model6 <- glm(cbind(detections, failures) ~ meanMinTemp + meanMaxTemp +meanPrecp+meanNDWI + cropland_prop , data = reportRate, family = binomial),
  model7 <- glm(cbind(detections, failures) ~ meanMinTemp + meanMaxTemp +meanPrecp+meanNDWI + grassland_prop , data = reportRate, family = binomial),
  model8 <- glm(cbind(detections, failures) ~ meanMinTemp + meanMaxTemp +meanPrecp+meanNDWI + urban_prop , data = reportRate, family = binomial),
  model9 <- glm(cbind(detections, failures) ~ meanMinTemp + meanMaxTemp +meanPrecp+meanNDWI + forest_prop , data = reportRate, family = binomial),
  model10 <- glm(cbind(detections, failures) ~ meanMinTemp + meanPrecp+meanNDWI + wetland_prop , data = reportRate, family = binomial),
  model11 <- glm(cbind(detections, failures) ~ meanMinTemp + meanMaxTemp , data = reportRate, family = binomial),
  model12 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDVI +meanNDWI +meanMaxTemp+ meanMinTemp , data = reportRate, family = binomial),
  model13 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDVI +meanNDWI +meanMaxTemp+ meanMinTemp+cropland_prop, data = reportRate, family = binomial),
  model14 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDVI +meanNDWI +meanMaxTemp+ meanMinTemp+grassland_prop, data = reportRate, family = binomial),
  model15 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDVI +meanNDWI +meanMaxTemp+ meanMinTemp+urban_prop, data = reportRate, family = binomial),
  model16 <- glm(cbind(detections, failures) ~ meanPrecp + meanNDVI +meanNDWI +meanMaxTemp+ meanMinTemp+forest_prop, data = reportRate, family = binomial)
)

# Create a summary table
model_summary <- map_dfr(models, ~{
  data.frame(
    Model = deparse(substitute(.x)),  # Model name
    AIC = AIC(.x),                   # AIC value
    Deviance = .x$deviance,           # Model deviance
    Null_Deviance = .x$null.deviance, # Null deviance
    df_residual = .x$df.residual      # Residual degrees of freedom
  )
}, .id = "Model")

print(model_summary)
model_summary <- model_summary %>%
  arrange(AIC)
print(model_summary)

#checking the inneraction terms to see better models
# Create a list of models
models <- list(
  model_precp_ndvi = glm(cbind(detections, failures) ~ meanPrecp * meanNDVI, 
                         data = reportRate, family = binomial),
  
  model_precp_urban = glm(cbind(detections, failures) ~ meanPrecp * urban_prop, 
                          data = reportRate, family = binomial),
  
  model_temp_precp = glm(cbind(detections, failures) ~ meanMinTemp * meanPrecp + meanMaxTemp * meanPrecp, 
                         data = reportRate, family = binomial),
  
  model_temp_ndvi = glm(cbind(detections, failures) ~ meanMaxTemp * meanNDVI + meanMinTemp * meanNDVI, 
                        data = reportRate, family = binomial),
  
  model_slope_precp = glm(cbind(detections, failures) ~ mean_slope * meanPrecp, 
                          data = reportRate, family = binomial),
  
  model_ndvi_urban = glm(cbind(detections, failures) ~ meanNDVI * urban_prop, 
                         data = reportRate, family = binomial),
  
  model_ndvi_grassland = glm(cbind(detections, failures) ~ meanNDVI * grassland_prop, 
                             data = reportRate, family = binomial),
  
  model_ndvi_forest = glm(cbind(detections, failures) ~ meanNDVI * forest_prop, 
                          data = reportRate, family = binomial),
  
  model_ndwi_precp = glm(cbind(detections, failures) ~ meanNDWI * meanPrecp, 
                         data = reportRate, family = binomial),
  
  model_temp_urban = glm(cbind(detections, failures) ~ meanMinTemp * urban_prop, 
                         data = reportRate, family = binomial)
)

# Create a summary table of AIC and deviance
model_summary <- data.frame(
  Model = names(models),
  AIC = sapply(models, AIC),
  Deviance = sapply(models, function(x) x$deviance),
  Null_Deviance = sapply(models, function(x) x$null.deviance),
  df_residual = sapply(models, function(x) x$df.residual)
)

# Print the table
print(model_summary)

# Optionally, you can order by AIC to see the best models:
model_summary <- model_summary[order(model_summary$AIC),]
print(model_summary)


library(MuMIn)
model_avg <- model.avg(models)
summary(model_avg)
