########################################################################################################################
#----------------------------------------------------------------------------------------------------------------------#
#|                                                                                                                    |#
#|                  Multiple Financial Time Series Analysis | Benchmarking Tool for ARMA-GARCH Models                 |#
#|                                                                                                                    |#
#----------------------------------------------------------------------------------------------------------------------#
########################################################################################################################

#-------------------------------------------------------------------------------#
# If you cannot install packages, run Rstudio as administrator and then write:  #
#.libPaths()                                                                    #
#                                                                               #
#             Use your "Program Files/R/" path and you are ready!               #
#                                                                               #
#                                e.g. mine is:                                  #
#                                                                               #
#.libPaths("C:/Program Files/R/R-4.0.3/library")                                #
#-------------------------------------------------------------------------------#

install.packages("PerformanceAnalytics")
install.packages("tseries")
install.packages("quantmod")
install.packages("urca")
install.packages("xts")
install.packages("forecast")
install.packages("rcompanion")
install.packages("knitr")
install.packages("rugarch")
install.packages("svDialogs")

#-----------------------------------------------------------------------------#
#   At this point, all you need to do is run each line, using CTRL + Enter.   #
#                                                                             #
#  Literally, just put the cursor in line 40 and start pressing CTRL+ENTER!!  #
#                                                                             #
#---------------------# ALL STEPS ARE PROVIDED IN ORDER #---------------------#
#-----------------------------------------------------------------------------#
#                                                                             #
#-----------------------------------------------------------------------------#

library(PerformanceAnalytics)
library(tseries)
library(quantmod)
library(urca)
library(xts)
library(forecast)
library(rcompanion)
library(knitr)
library(rugarch)
library(svDialogs)



{
  options(warn=2)
  is_png_error <- try(png(), silent = TRUE)
  {
    if(class(is_png_error) == "try-error"){
      dlg_message("It seems that your Anti-Virus program blocks RStudio for several reasons, such as saving .png images or .txt files. Before proceeding, please consider disabling it.")
    }
    else
    {
      dlg_message("You can continue running the script. No errors should occur.")
    }
  }
  dlg_message("Reminder: In any case you encounter errors with the script, please feel free to contact me at: www.koufopoulosf.com/")
}




####################################################################################################
############################################## START ###############################################
####################################################################################################



{
  
  
  
  
  
  
  #########################################################################################################################
  #---------------------------------------------------------------------------------------------------------------------###
  #-----------------------------------------------------# START #-------------------------------------------------------###
  #---------------------------------------------------------------------------------------------------------------------###
  #----------------------------------------------# MANUAL CONFIGURATION #-----------------------------------------------###
  #---------------------------------------------------------------------------------------------------------------------###
  
  
  basic_configuration <- 0
  while(basic_configuration!=1){
    cat("##############################################\n")
    cat("### PLEASE FOLLOW THE NEXT STEPS CAREFULLY ###\n")
    cat("##############################################\n\n")
    total_num_of_tickers <- c()
    total_num_of_tickers <- as.numeric(readline(cat("Choose number of Tickers: ")))
    Tickers <- c()
    choose_Tickers_Name <- c()
    count_valid_tickers <- 0
    count_valid_names <- 0
    for(i in 1:total_num_of_tickers){
      Tickers[i] <- readline(cat("Enter Ticker's", i,"Symbol (e.g. ^GSPC):"))
      choose_Tickers_Name[i] <-  readline(cat("Give a name to that Ticker (e.g. S&P500):"))
    }
    options(warn=1)
    dlg_message("I'm just making sure you gave valid Tickers..")
    for(i in seq_along(Tickers)){
      is_symbol_error <- try(getSymbols(Tickers[i]),silent = TRUE)
      {
        if(class(is_symbol_error) == "try-error"){
          dlg_message(c("Ticker", Tickers[i],", does not exist. Please fill the form again!"))
        }
        else if(trimws(Tickers[i])=="") {
          dlg_message("Ticker is not valid! Please fill the form again!")
        } else {
          dlg_message(c("Ticker", Tickers[i],", is valid!"))
          count_valid_tickers <- count_valid_tickers + 1
        }
      }
    }
    options(warn=2)
    if(count_valid_tickers == length(Tickers)){
      for(i in seq_along(choose_Tickers_Name)) {
        if(trimws(choose_Tickers_Name[i])==""){
          dlg_message(c("You didn't give a name to Ticker ", Tickers[i],". It's important!! Please fill the form again!"))
        } else {
          dlg_message(c("Great!! You named Ticker ", Tickers[i]," to ", choose_Tickers_Name[i]," successfully!"))
          count_valid_names <- count_valid_names + 1
          if(count_valid_names == length(choose_Tickers_Name)){
            basic_configuration <- 1
            dlg_message("Perfect!! Basic configuration was successful!!!")
          }
        }
      }
    }
  }
  
  options(warn=0, error=NULL)
  
  
  from_date <- as.Date(readline(cat("Choose start date (YYYY-MM-DD):")))
  to_date <- as.Date(readline(cat("Choose end date (YYYY-MM-DD):")))
  choose_periodicity <- dlg_list(title="Available periodicities ",c("daily", "weekly", "monthly"))$res
  predict_type <- dlg_list(title="Available Values To Predict ",c("Open Prices", "High Prices", "Low Prices","Close Prices","Volume","Adjusted Close Prices"))$res
  switch(predict_type,
         "Open Prices" = predict_column <- 1,
         "High Prices" = predict_column <- 2,
         "Low Prices" = predict_column <- 3,
         "Close Prices" = predict_column <- 4,
         "Volume" = predict_column <- 5,
         "Adjusted Close Prices" = predict_column <- 6)
  is_width_valid <- 0
  is_height_valid <- 0
  while(is_width_valid!=1){
    image_width <- as.numeric(readline(cat("Choose width of images in pixels (e.g. 700):")))
    if(image_width<100 || image_width>2000){
      dlg_message("Invalid image width! Please choose a valid number!!")
    } else{ is_width_valid <- 1 }
  }
  while(is_height_valid!=1){
    image_height <- as.numeric(readline(cat("Choose height of images in pixels (e.g. 400):")))
    if(image_height<100 || image_height>2000){
      dlg_message("Invalid image height! Please choose a valid number!!")
    } else{ is_height_valid <- 1 }
  }
  language <- dlg_list(title="Choose Language for Graphs ",c("English", "Greek"))$res
  switch(language,
         'English' = Sys.setlocale(category = "LC_ALL", locale = "english"),
         'Greek' = Sys.setlocale(category = "LC_ALL", locale = "greek"))
  new_folder_name <- c()
  is_folder_valid <- 0
  new_folder_name <- readline(cat("I forgot to mention, I'll create a folder on your desktop to store the results.\nHow should I name it?"))
  if(trimws(new_folder_name)==""){
    cat("Don't resist, you've fallen into a while loop :P\n")
    while(is_folder_valid!=1){
      new_folder_name <- readline(cat("Please give a name for the folder:"))
      if(trimws(new_folder_name)!=""){is_folder_valid <- 1}
    }
  }
  
  perform_arma_garch_benchmark <- dlg_list(title="Do you wish to perform ARMA-GARCH Benchmarking? ",c("Yes!!!", "Naahh, it's boring.."))$res
  
  if(perform_arma_garch_benchmark == "Yes!!!"){
    choose_arma_min_order <- readline(cat("Choose minimum ARMA(p,q) order for the ARMA model:\nEnter two numbers separated by space(s) and then ENTER.\n\n"))
    choose_arma_min_order <- as.integer(unlist(strsplit(choose_arma_min_order," ")))
    choose_arma_min_order <- na.omit(choose_arma_min_order)
    if(length(choose_arma_min_order)!=2){
      is_arma_min_order_valid <- 0
      while(is_arma_min_order_valid!=1){
        choose_arma_min_order <- readline(cat("Choose minimum ARMA(p,q) order for the ARMA model:\nEnter two numbers separated by space(s) and then ENTER.\n\n"))
        choose_arma_min_order <- as.integer(unlist(strsplit(choose_arma_min_order," ")))
        choose_arma_min_order <- na.omit(choose_arma_min_order)
        if(length(choose_arma_min_order)==2) { is_arma_min_order_valid <- 1 }
      }
    }
    choose_arma_max_order <- readline(cat("Choose maximum ARMA(p,q) order for the ARMA model:\nEnter two numbers separated by space(s) and then ENTER.\n\n"))
    choose_arma_max_order <- as.integer(unlist(strsplit(choose_arma_max_order," ")))
    choose_arma_max_order <- na.omit(choose_arma_max_order)
    if(length(choose_arma_max_order)!=2){
      is_arma_max_order_valid <- 0
      while(is_arma_max_order_valid!=1){
        choose_arma_max_order <- readline(cat("Choose maximum ARMA(p,q) order for the ARMA model:\nEnter two numbers separated by space(s) and then ENTER.\n\n"))
        choose_arma_max_order <- as.integer(unlist(strsplit(choose_arma_max_order," ")))
        choose_arma_max_order <- na.omit(choose_arma_max_order)
        if(length(choose_arma_max_order)==2) { is_arma_max_order_valid <- 1}
      }
    }
    if(choose_arma_max_order[1]<choose_arma_min_order[1] || choose_arma_max_order[2]<choose_arma_min_order[2]){
      while(choose_arma_max_order[1]<choose_arma_min_order[1] || choose_arma_max_order[2]<choose_arma_min_order[2]){
        choose_arma_max_order <- readline(cat("Maximum ARMA(p,q) order should be equal or greater than minimum ARMA(p,q) order!\nPlease enter again maximum ARMA(p,q) order:"))
        choose_arma_max_order <- as.integer(unlist(strsplit(choose_arma_max_order," ")))
        choose_arma_max_order <- na.omit(choose_arma_max_order)
        if(length(choose_arma_max_order)!=2){
          is_arma_max_order_valid <- 0
          while(is_arma_max_order_valid!=1){
            choose_arma_max_order <- readline(cat("Maximum ARMA(p,q) order should be equal or greater than minimum ARMA(p,q) order!\nPlease enter again maximum ARMA(p,q) order:"))
            choose_arma_max_order <- as.integer(unlist(strsplit(choose_arma_max_order," ")))
            choose_arma_max_order <- na.omit(choose_arma_max_order)
            if(length(choose_arma_min_order)==2) { is_arma_max_order_valid <- 1}
          }
        }
      }
    }
    choose_model <- dlg_list(title="~ ~ ~ Choose GARCH Model ~ ~ ~\n",c("sGARCH", "iGARCH", "gjrGARCH", "eGARCH", "apARCH", "fGARCH", "csGARCH"))$res
    choose_submodel <- c()
    if(choose_model == "fGARCH"){
      choose_submodel <- dlg_list(title="### Choose fGARCH Submodel ###\n",c("GARCH", "TGARCH", "GJRGARCH", "AVGARCH", "NGARCH", "NAGARCH", "APARCH", "ALLGARCH"))$res
    }
    choose_garch_order <- readline(cat("Choose GARCH(p,q) order for the GARCH model:\nEnter two numbers separated by space(s) and then ENTER.\n\n"))
    choose_garch_order <- as.integer(unlist(strsplit(choose_garch_order," ")))
    choose_garch_order <- na.omit(choose_garch_order)
    if(length(choose_garch_order)!=2){
      is_garch_order_valid <- 0
      while(is_garch_order_valid!=1){
        choose_garch_order <- readline(cat("Choose GARCH(p,q) order for the GARCH model:\nEnter two numbers separated by space(s) and then ENTER.\n\n"))
        choose_garch_order <- as.integer(unlist(strsplit(choose_arma_max_order," ")))
        choose_garch_order <- na.omit(choose_arma_max_order)
        if(length(choose_garch_order)==2) { is_garch_order_valid <- 1}
      }
    }
    choose_distributions <- dlg_list(title="\n\n#### Choose Single or Multiple Distributions for Benchmarking ####\n",c("Normal Distribution", "Student-t Distribution","Generalized Error Distribution", "Skew Normal Distribution", "Skew Student-t Distribution", "Skew Generalized Error Distribution", "Normal-Inverse Gaussian Distribution", "Johnson's SU-Distribution", "Generalized Hyperbolic Distribution"), multiple=TRUE)$res
    for(i in 1:length(choose_distributions)){
      switch(choose_distributions[i],
             "Normal Distribution" = choose_distributions[i] <- "norm",
             "Student-t Distribution" = choose_distributions[i] <- "std",
             "Generalized Error Distribution" = choose_distributions[i] <- "ged",
             "Skew Normal Distribution" = choose_distributions[i] <- "snorm",
             "Skew Student-t Distribution" = choose_distributions[i] <- "sstd",
             "Skew Generalized Error Distribution" = choose_distributions[i] <- "sged",
             "Normal-Inverse Gaussian Distribution" = choose_distributions[i] <- "nig",
             "Johnson's SU-Distribution" = choose_distributions[i] <- "jsu",
             "Generalized Hyperbolic Distribution" = choose_distributions[i] <- "ghyp")
    }
    choose_crit <- dlg_list(title="\n\n#### Choose Single or Multiple Information Criteria for Benchmarking ####\n", c("Akaike Information Criterion", "Bayesian Information Criterion", "Shibata Information Criterion", "Hannan-Quinn Information Criterion"), multiple = TRUE)$res
    for(i in 1:length(choose_crit)){
      switch(choose_crit[i],
             "Akaike Information Criterion" = choose_crit[i] <- 1,
             "Bayesian Information Criterion" = choose_crit[i] <- 2,
             "Shibata Information Criterion" = choose_crit[i] <- 3,
             "Hannan-Quinn Information Criterion" = choose_crit[i] <-4)
    }
    choose_crit <- as.numeric(choose_crit)
    dlg_message("Perfect!! Now press OK and let me do my calculations!")
  }
  
  #---------------------------------------------------------------------------------------------------------------------###
  #----------------------------------------------# MANUAL CONFIGURATION #-----------------------------------------------###
  #---------------------------------------------------------------------------------------------------------------------###
  #------------------------------------------------------# END #--------------------------------------------------------###
  #---------------------------------------------------------------------------------------------------------------------###
  #########################################################################################################################
  #########################################################################################################################
  #########################################################################################################################
  
  
  
  
  
  ##########################################################################################################################
  ##########################################################################################################################
  ###--------------------------------------------------------------------------------------------------------------------###
  ###---------------------------------------------# RESULTS ENVIRONMENT #------------------------------------------------###
  ###--------------------------------------------------------------------------------------------------------------------###
  ###-------------------------------------------# DATASET - REQUIRED INFO #----------------------------------------------###
  ###--------------------------------------------------------------------------------------------------------------------###
  ##########################################################################################################################
  ##########################################################################################################################
  
  
  dir.create(file.path(Sys.getenv("USERPROFILE"), "Desktop", new_folder_name, fsep="\\"))
  folder_path <- c()
  for(i in seq_along(choose_Tickers_Name)) {
    dir.create(file.path(Sys.getenv("USERPROFILE"),paste0("Desktop\\",new_folder_name, "\\", choose_Tickers_Name[i],"_", choose_periodicity),fsep="\\"))
    folder_path[i] <- file.path(Sys.getenv("USERPROFILE"),paste0("Desktop\\", new_folder_name, "\\", choose_Tickers_Name[i], "_", choose_periodicity, "\\\\"),fsep="\\")
  }
  
  
  
  
  #----------------------------------------------------------------------------------------------------#
  #-----------------------------------------# Create Dataset #-----------------------------------------#
  #----------------------------------------------------------------------------------------------------#
  Stocks <- vector("list", length(Tickers))
  
  for(i in seq_along(Tickers)){
    Stocks[[i]] <- getSymbols(Tickers[i], src = "yahoo", from = from_date, to = to_date, periodicity = choose_periodicity, auto.assign = FALSE)
    Stocks[[i]] <- Stocks[[i]][,predict_column]
    Stocks[[i]] <- na.omit(Stocks[[i]])
  }
  
  
  
  #----------------------------------------------------------------------------------------------------#
  #--------------------------# Data Description and Transformation Category #--------------------------#
  #----------------------------------------------------------------------------------------------------#
  
  #----------------------#
  # Plot the Time Series #
  #----------------------#
  for(i in seq_along(choose_Tickers_Name)){
    png(paste0(folder_path[[i]],choose_Tickers_Name[i],"_", choose_periodicity,"_prices.png"), width=image_width, height=image_height)
    print(plot(Stocks[[i]], col="blue", lwd=2, main= paste(choose_Tickers_Name[i], "-", predict_type, "- Frequency:", choose_periodicity)))
    dev.off()
  }
  
  
  #--------------------------------------------------------#
  # ACF displays high AutoCorrelation with previous values #
  #--------------------------------------------------------#
  for(i in seq_along(choose_Tickers_Name)){
    png(paste0(folder_path[[i]],choose_Tickers_Name[i],"_", choose_periodicity,"_ACF_prices.png"), width=image_width, height=image_height)
    acf(Stocks[[i]], lag=10*log(length(Stocks[[i]])), main = (paste(choose_Tickers_Name[i], "- ACF of", predict_type)))
    dev.off()
  }
  
  
  #-----------#
  # ADF Tests #
  #-----------#
  for(i in seq_along(choose_Tickers_Name)){
    sink(paste0(folder_path[i],choose_Tickers_Name[i],"_", choose_periodicity,"_stationarity_tests_prices.txt"))
    cat(paste("###| ADF Tests on",predict_type,"|###\n\n\n\n"))
    print(summary(ur.df(Stocks[[i]], type = "trend")))
    cat("\n\n\n\n\n\n\n")
    print(summary(ur.df(Stocks[[i]], type = "drift")))
    cat("\n\n\n\n\n\n\n")
    print(summary(ur.df(Stocks[[i]], type = "none")))
    sink()
  }
  
  
  #------------------------------------------------------------------#
  # Time Series is not stationary, so we will proceed to log returns #
  #--------------------------------------------------------------------------------------------------------------------#
  # We remove NA values of the object to avoid problems that some R functions have when missing values are encountered #
  #--------------------------------------------------------------------------------------------------------------------#
  Stocks_returns <- c()
  for(i in seq_along(choose_Tickers_Name)){
    Stocks_returns[[i]] <- log(Stocks[[i]]) - log(lag(Stocks[[i]],1))
    Stocks_returns[[i]] <- na.omit(Stocks_returns[[i]])
  }
  
  
  
  #-------------------------------------#
  # Log returns appear to be stationary #
  #-------------------------------------#
  for(i in seq_along(choose_Tickers_Name)){
    png(paste0(folder_path[[i]],choose_Tickers_Name[i],"_", choose_periodicity,"_returns.png"), width=image_width, height=image_height)
    print(plot(Stocks_returns[[i]], col="blue", lwd=1, main= paste(choose_Tickers_Name[i], "- Returns of", predict_type)))
    dev.off()
  }
  
  
  
  
  #-------------------------------------------------#
  # ACF of Log Returns display few AutoCorrelations #
  #-------------------------------------------------#
  for(i in seq_along(choose_Tickers_Name)){
    png(paste0(folder_path[[i]],choose_Tickers_Name[i],"_", choose_periodicity,"_ACF_returns.png"), width=image_width, height=image_height)
    acf(Stocks_returns[[i]], lag=10*log(length(Stocks_returns[[i]])), main = (paste(choose_Tickers_Name[i], "- ACF on returns of", predict_type)))
    dev.off()
  }
  
  
  #--------------------------#
  # ADF tests of log returns #
  #--------------------------#
  for(i in seq_along(choose_Tickers_Name)){
    sink(paste0(folder_path[i],choose_Tickers_Name[i],"_", choose_periodicity,"_stationarity_tests_returns.txt"))
    cat(paste("###| ADF Tests on returns of",predict_type,"|###\n\n\n\n"))
    print(summary(ur.df(Stocks_returns[[i]], type = "trend")))
    cat("\n\n\n\n\n\n\n")
    print(summary(ur.df(Stocks_returns[[i]], type = "drift")))
    cat("\n\n\n\n\n\n\n")
    print(summary(ur.df(Stocks_returns[[i]], type = "none")))
    sink()
  }
  
  
  #----------------------------------#
  # Tables of Descriptive Statistics #
  #----------------------------------#
  for(i in seq_along(choose_Tickers_Name)){
    sink(paste0(folder_path[i],choose_Tickers_Name[i],"_", choose_periodicity,"_descriptive_statistics.txt"))
    cat(paste("###|", choose_Tickers_Name[i], "- Descriptive Statistics of",predict_type,"on",choose_periodicity,"basis, ranging from",from_date,"to",to_date,"|###\n\n"))
    print(table.Stats(Stocks[[i]]))
    cat("\n\n\n")
    cat(paste("###|", choose_Tickers_Name[i], "- Descriptive Statistics on returns of",predict_type,"on",choose_periodicity,"basis, ranging from",from_date,"to",to_date,"|###\n\n"))
    print(table.Stats(Stocks_returns[[i]]))
    sink()
  }
  
  
  
  
  
  #-----------------------------------------------------------------------------------------#
  #--------------------------# Normality of Log Returns Category #--------------------------# 
  #-----------------------------------------------------------------------------------------#
  
  #-----------#
  # Histogram #
  #-----------#
  for(i in seq_along(choose_Tickers_Name)){
    png(paste0(folder_path[[i]],choose_Tickers_Name[i],"_", choose_periodicity,"_histogram_returns.png"), width=image_width, height=image_height)
    hist(Stocks_returns[[i]], col="cornflowerblue", xlab="Logarithmic Returns", main= paste(choose_Tickers_Name[i], "- Histogram of Logarithmic Returns"))
    dev.off()
  }
  
  
  
  #----------------------#
  # Normal Density Graph #
  #----------------------#
  for(i in seq_along(choose_Tickers_Name)){
    png(paste0(folder_path[[i]],choose_Tickers_Name[i],"_", choose_periodicity,"_norm_density_returns.png"), width=image_width, height=image_height)
    plotNormalDensity(Stocks_returns[[i]], col2 ="blue",col3="red", main= paste(choose_Tickers_Name[i], "- Normal Density Graph of Log Returns"))
    dev.off()
  }
  
  
  
  
  #-----------------#
  # Normal Q-Q Plot #
  #-----------------#
  for(i in seq_along(choose_Tickers_Name)){
    png(paste0(folder_path[[i]],choose_Tickers_Name[i],"_", choose_periodicity,"_qqnorm.png"), width=image_width, height=image_height)
    qqnorm(Stocks_returns[[i]], col="blue")
    qqline(Stocks_returns[[i]],col="red", lwd=2)
    dev.off()
  }
  
  
  
  
  #-------------------------------------------------#
  # Shapiro-Wilk and Jarque-Bera Test for Normality #
  #-------------------------------------------------#
  for(i in seq_along(choose_Tickers_Name)){
    sink(paste0(folder_path[i],choose_Tickers_Name[i],"_", choose_periodicity,"_normality_tests.txt"))
    cat(paste("###|", choose_Tickers_Name[i], "- Normality Tests on returns of",predict_type,"on",choose_periodicity,"basis, ranging from",from_date,"to",to_date,"|###\n\n"))
    print(shapiro.test(as.vector(Stocks_returns[[i]])))
    cat("\n\n\n")
    print(jarque.bera.test(Stocks_returns[[i]]))
    sink()
  }
  
  
  
  #----------------------------------------------------#
  # Squared Log Returns and ACF of Squared Log Returns #
  #----------------------------------------------------#
  Stocks_returns_sq <- c()
  for(i in seq_along(choose_Tickers_Name)){
    Stocks_returns_sq[[i]] <- Stocks_returns[[i]]^2
  }
  for(i in seq_along(choose_Tickers_Name)){
    png(paste0(folder_path[[i]],choose_Tickers_Name[i],"_", choose_periodicity,"_returns_sq.png"), width=image_width, height=image_height)
    print(plot(Stocks_returns_sq[[i]], col="blue", lwd=1, main= paste(choose_Tickers_Name[i], "- Squared Log Returns")))
    dev.off()
  }
  
  for(i in seq_along(choose_Tickers_Name)){
    png(paste0(folder_path[[i]],choose_Tickers_Name[i],"_", choose_periodicity,"_ACF_returns_sq.png"), width=image_width, height=image_height)
    acf(Stocks_returns_sq[[i]], lag=10*log(length(Stocks_returns_sq[[i]])), main= paste(choose_Tickers_Name[i], "- ACF of Squared Log Returns"))
    dev.off()
  }
  
  
  #------------------------------------------------------#
  # Absolute Log Returns and ACF of Absolute Log Returns #
  #------------------------------------------------------#
  Stocks_returns_abs <- c()
  for(i in seq_along(choose_Tickers_Name)){
    Stocks_returns_abs[[i]] <- abs(Stocks_returns[[i]])
  }
  for(i in seq_along(choose_Tickers_Name)){
    png(paste0(folder_path[[i]],choose_Tickers_Name[i],"_", choose_periodicity,"_returns_abs.png"), width=image_width, height=image_height)
    print(plot(Stocks_returns_abs[[i]], col="blue", lwd=1, main= paste(choose_Tickers_Name[i], "- Absolute Log Returns")))
    dev.off()
  }
  
  for(i in seq_along(choose_Tickers_Name)){
    png(paste0(folder_path[[i]],choose_Tickers_Name[i],"_", choose_periodicity,"_ACF_returns_abs.png"), width=image_width, height=image_height)
    acf(Stocks_returns_abs[[i]], lag=10*log(length(Stocks_returns_abs[[i]])), main= paste(choose_Tickers_Name[i], "- ACF of Absolute Log Returns"))
    dev.off()
  }
  
  
  
  
  
  
  
  
  #########################################################################################################################
  #########################################################################################################################
  ######-------------------------------------------------------------------------------------------------------------######
  ######--------------------------------------------# BENCHMARKING  #------------------------------------------------######
  ######-------------------------------------------------------------------------------------------------------------######
  #########################################################################################################################
  #########################################################################################################################
  
  
  matsplitter <- function(M, r, c){
    simplify2array(lapply(
      split(M, interaction((row(M)-1)%/%r+1,(col(M)-1)%/%c+1)),
      function(x) {dim(x) <- c(r,c); x;}
    ))
  }
  
  
  
  
  
  arma_garch_benchmark <- function(dataset, named_tickers, data_periodicity, path, arma_min_order, arma_max_order, garch_model, garch_submodel, garch_order, distributions, info_crit){
    
    
    available_criterion <- c("Akaike", "Bayesian", "Shibata", "Hannan-Quinn")
    
    all_distributions <-  c("norm", "std", "ged", "snorm", "sstd", "sged", "nig", "jsu", "ghyp")
    
    available_distributions <- c("Normal Distribution", "Student-t Distribution","Generalized Error Distribution", "Skew Normal Distribution", "Skew Student-t Distribution", "Skew Generalized Error Distribution", "Normal-Inverse Gaussian Distribution", "Johnson's SU-Distribution", "Generalized Hyperbolic Distribution")
    
    my_distributions <- c()
    for(i in seq_along(distributions)){
      for(j in seq_along(all_distributions)){
        if((distributions[i] == all_distributions[j]) == TRUE) {
          my_distributions[i] <- available_distributions[j]
        }
      }
    }
    
    
    min_p <- arma_min_order[1]
    min_q <- arma_min_order[2]
    
    max_p <- arma_max_order[1]
    max_q <- arma_max_order[2]
    
    garch_p <- garch_order[1]
    garch_q <- garch_order[2]
    
    for(stock_table in seq_along(dataset)){
      
      comb1 <- c()
      comb2 <- c()
      for(p in min_p:max_p){
        for(q in min_q:max_q){
          comb1 <- append(comb1,p)
          comb2 <- append(comb2,q)
        }
      }
      
      master_table  <- matrix(NA, (length(comb1)), 4)
      add_table <- matrix(NA, (length(comb1)), 4)
      k <- 1
      for(dist in seq_along(distributions)){
        for (p in min_p:max_p){
          for (q in min_q:max_q){
            garch.spec <- ugarchspec(variance.model=list(model=garch_model, garchOrder=c(garch_p,garch_q), submodel = garch_submodel), mean.model=list(armaOrder=c(p,q)), distribution.model = distributions[dist])
            garch.fit <- ugarchfit(spec=garch.spec, data=dataset[[stock_table]], solver = "hybrid")
            for(crit in seq_along(info_crit)){
              master_table[k, info_crit[crit]] <-  infocriteria(garch.fit)[info_crit[crit]]
            }
            k <- k + 1
          }
        }
        master_table <- rbind(master_table,add_table)
      }
      
      master_table <- master_table[1:(length(comb1)*length(distributions)),]
      
      
      for(crit_col in seq_along(info_crit)) {
        
        current.ic <- c()
        final.ic <- Inf
        order_row <- c()
        
        for(i in 1:(length(comb1)*length(distributions))) {
          
          if(length(distributions)==1&&length(comb1)==1){
            current.ic <- master_table[info_crit[crit_col]]
          } else{
            current.ic <- master_table[i,info_crit[crit_col]]
          }
          
          if(current.ic < final.ic) {
            final.ic <- current.ic
            order_row <- i
          }
        }
        
        
        counter <- 1
        
        batch <- length(comb1)
        
        for(i in seq_along(distributions)) {
          if((order_row/batch)>1){
            counter <- counter + 1
            batch <- batch + length(comb1)
          }
        }
        
        final.order_row <- length(comb1) - (length(comb1)*counter - order_row)
        
        final.order <- c(comb1[final.order_row],comb2[final.order_row])
        
        dist_best <- distributions[counter]
        
        
        sink(paste0(path[stock_table],named_tickers[stock_table],"_", data_periodicity,"_",garch_model,"_benchmarking_results.txt"), append = TRUE)
        cat("*******************************************************************************************************************************************\n")
        cat("*******************************************************************************************************************************************\n")
        cat("                                                    ####  ",available_criterion[info_crit[crit_col]], "Information Criterion  ###\n")
        cat("*******************************************************************************************************************************************\n")
        cat("*******************************************************************************************************************************************\n\n")
        cat(paste0("Based on ",available_criterion[info_crit[crit_col]]," Information Criterion,\n\nthe best model is ARMA(",final.order[1],",",final.order[2],") - ", garch_model,"(",garch_p,",",garch_q,")", " in ", my_distributions[counter],", with value ", final.ic,"\n\n\n"))
        garch.spec <- ugarchspec(variance.model=list(model= garch_model, garchOrder=c(garch_p,garch_q), submodel=garch_submodel), mean.model=list(armaOrder=c(final.order[1],final.order[2])), distribution.model = dist_best)
        garch.fit <- ugarchfit(spec=garch.spec, data=dataset[[stock_table]], solver = "hybrid")
        print(garch.fit)
        cat("\n\n\n\n")
        cat("###########################################################################################################################################\n")
        cat("###########################################################################################################################################\n")
        cat("###########################################################################################################################################\n\n\n\n\n\n\n\n\n")
        sink()
      }
      
      
      if(length(comb1)!=1){
        
        junior_table <- master_table[,apply(master_table,2,function(master_table) !any(is.na(master_table)))]
        
        if(length(info_crit)==1){
          junior_table <- t(t(junior_table))
          dist_tables <- matsplitter(junior_table, length(comb1), length(info_crit))
        }
        
        if(length(info_crit)!=1){
          dist_tables <- matsplitter(junior_table, length(comb1), length(info_crit))
        }
        
        rnames <- c()
        
        for(k in seq_along(comb1)){
          rnames[k] <- c(paste0("ARMA(",comb1[k],",", comb2[k],")-", garch_model,"(",garch_p,",",garch_q,")"))
        }
        
        rownames(dist_tables) <- c(rnames)
        
        info_crit_sort <- sort(info_crit)
        
        sink(paste0(path[stock_table],named_tickers[stock_table],"_", data_periodicity,"_",garch_model,"_benchmarking.txt"), append = TRUE)
        for(dist in seq_along(my_distributions)){
          cat("\n\n\n\n")
          cat(paste("             ### ", my_distributions[dist]," ###\n"))
          cat("---------------------------------------------------------------------------")
          print(kable(x=(dist_tables[,,dist]), "rst",digits=15, col.names = c(paste(available_criterion[info_crit_sort],"IC"))))
          cat("###########################################################################\n")
          cat("###########################################################################\n")
          cat("###########################################################################\n\n\n\n\n")
        }
        sink()
        for(dist in seq_along(my_distributions)){
          sink(paste0(path[stock_table],named_tickers[stock_table],"_", data_periodicity,"_",garch_model,"_benchmarking_LaTeX.txt"), append = TRUE)
          print(kable(x=(dist_tables[,,dist]), format="latex",digits=15, col.names = c(paste(available_criterion[info_crit_sort],"IC"))))
          sink()
        }
      }
      
      
      if(length(comb1)==1) {
        
        if(length(distributions)!=1){
          junior_table <- master_table[,apply(master_table,2,function(master_table) !any(is.na(master_table)))]
          dist_tables <- t(t(junior_table))
        }
        
        if(length(distributions)==1){
          dist_tables <- master_table
          i <- 4
          while(any(is.na(dist_tables))==TRUE){
            if(any(is.na(dist_tables[i]))==TRUE){
              dist_tables <- dist_tables[-i]
            }
            i <- i - 1
          }
          dist_tables <- matrix(dist_tables)
          dist_tables <- t(dist_tables)
        }
        
        info_crit_sort <- sort(info_crit)
        colnames(dist_tables) <- c(paste(available_criterion[info_crit_sort],"IC"))
        rownames(dist_tables) <- c(my_distributions)
        
        sink(paste0(path[stock_table],named_tickers[stock_table],"_", data_periodicity,"_",garch_model,"_benchmarking.txt"), append = TRUE)
        cat("\n\n\n\n")
        cat(paste0("              ### ARMA(",comb1[1],",", comb2[1],")-", garch_model,"(",garch_p,",",garch_q,")"," ###\n"))
        cat("---------------------------------------------------------------------------\n")
        print(kable(x=(dist_tables), "rst",digits=15))
        cat("###########################################################################\n")
        cat("###########################################################################\n")
        cat("###########################################################################\n\n\n\n\n")
        sink()
        sink(paste0(path[stock_table],named_tickers[stock_table],"_", data_periodicity,"_",garch_model,"_benchmarking_LaTeX.txt"), append = TRUE)
        print(kable(x=(dist_tables), format="latex",digits=15))
        sink()
      }
    }
  }
  
  
  
  
  
  {
    if((perform_arma_garch_benchmark == "Yes!!!") == TRUE) {
      
      dataset <- Stocks_returns
      named_tickers <- choose_Tickers_Name
      data_periodicity <- choose_periodicity
      path <- folder_path
      arma_min_order <- choose_arma_min_order
      arma_max_order <- choose_arma_max_order
      garch_model <- choose_model
      garch_submodel <- choose_submodel
      garch_order <- choose_garch_order
      distributions <- choose_distributions
      info_crit <- choose_crit
      
      time_benchmark_started <- Sys.time()
      arma_garch_benchmark(dataset, named_tickers, data_periodicity, path, arma_min_order, arma_max_order, garch_model, garch_submodel, garch_order, distributions, info_crit)
      time_benchmark_ended <- Sys.time()
      benchmark_running_time <- difftime(time_benchmark_ended, time_benchmark_started, units='secs')
      benchmark_running_time <- round(benchmark_running_time,2)
      dlg_message("I tried to be as fast as possible.... and I think I made it!!")
      dlg_message(c("Total running time of benchmark was", benchmark_running_time,"seconds."))
    } else {
      dlg_message("Aaallrighhtt!! Your graphs are ready!!")
    }
  }
  
  
  dlg_message("Thanks for your time!! I stored your results on Desktop :)")
  
}



####################################################################################################
############################################### END ################################################
####################################################################################################
