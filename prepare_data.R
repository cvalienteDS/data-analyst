
prepare_data <- function(df){
  
  
  
  categories = c("UNRATED", "G" , "NC-17", "PG", "PG-13", "R")
  aux <-  !(df$rating %in% categories)
  df$rating[aux] <- "UNRATED"
  
  df$rating <- as.factor(df$rating)
  
  df$genre <- as.factor(df$genre)
  df$genre <- fct_lump_prop(df$genre, prop = 0.03)
  df$company <- as.factor(df$company)
  df$company <- fct_lump_prop(df$company, prop = 0.01)
  df$country <- as.factor(df$country)
  df$country <- fct_lump_prop(df$country, prop = 0.02)
  
  # imputacion de valores nulos (ceros en  budget)
  nrow(df[df$budget == 0,])
  # replace zeros with NA
  df[df$budget == 0,"budget"] <- NA
  
  df_vis <- df[, c("budget", "genre", "gross", "rating", "year", "runtime", "score", "votes", "country")]
  ## observacion de nulos
  # # check if missing at random
  # loginfo('check if missing at random')
  # df_vis$miss <- ifelse(is.na(df_vis$budget), 1, 0) # ponemos 1 cuando hay missing data e intentamos ver si hay correlacion
  # fit_data <- dplyr::select(df_vis,-c("budget"))
  # browser()
  # summary(glm(miss ~ year + genre + gross, data=fit_data, family=binomial))
  # fit_data %>% select_if(is.numeric) %>% cor()
  
  # library(Amelia)
  # amelia_output <- Amelia::amelia(df_vis, noms=c("genre", "rating"))
  # missingness_map <- Amelia::missmap(amelia_output,  col=c('navyblue','yellow'),
  #                   legend=TRUE, sortVars=TRUE,
  #                   x.cex=.7,
  #                   gap.xaxis=3, main=c("Missing data pattern"))
  
  
  suppressMessages(library(missForest))
  # # probamos primero a ver que tal lo hace en un subset que podamos comprobar
  # df_temp <- na.omit(df[, c("budget", "genre", "gross", "rating", "year", "runtime","score", "votes", "country")])
  # df.mis_temp <- prodNA(df_temp, noNA = 0.2) # no parece coger solo una columna, asi que hay que hacer workaround
  # df.mis <- df_temp
  # df.mis$budget <- df.mis_temp$budget
  # 
  # 
  # df.imp <- missForest(df.mis)
  # 
  # # imputation error
  # df.imp$OOBerror
  # #comparing actual data accuracy
  # iris.err <- mixError(df.imp$ximp, df.mis, df_temp)
  # iris.err
  # metric <- MLmetrics::MAPE(y_pred =df.imp$ximp$budget, y_true = df_temp$budget)
  # loginfo(sprintf("imputation metric error %s", metric))
  
  
  
  # # intentar con Hmisc
  # library(Hmisc)
  # library(MLmetrics)
  # numImputations = 5
  # impute_arg <- aregImpute(~ budget+ genre+ gross+ rating+ year, data = df.mis, n.impute = numImputations)
  # 
  # MLmetrics::MAPE(y_pred = impute_arg$imputed$budget[, numImputations], y_true = df_temp$budget[is.na(df.mis$budget)])
  
  # ahora procedemos sobre el df
  loginfo("Imputing zeros in budget based on Random Forest")
  df.imp <- missForest(df_vis, verbose = F)
  df$budget <- df.imp$ximp$budget
  
  
  df$grossPerBudget <- df$gross/df$budget
  df$netGross <- df$gross - df$budget
  
  return(df)
}