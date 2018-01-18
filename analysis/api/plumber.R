#* @apiTitle Predict Bank Data
#* @apiDescription Scoring flat files

library(dplyr)
library(forcats)
library(randomForest)
library(DBI)

strct <- readRDS("training_data_str.RDS")
logistic <- readRDS("model_logistic.RDS")
ranger <- readRDS("model_rf.RDS")

p <- function(inputline, s=strct){

  v <- unlist(strsplit(inputline, ","))
  v[4] <- ifelse(v[4] %in% c("basic.4y", "basic.6y", "illiterate"), "less.than.5.years", v[4])
  
  data.frame(
    age = as.numeric(v[1]),
    job = factor(v[2], levels(s[[2]])),
    marital = factor(v[3], levels(s[[3]])),
    education = factor(v[4], levels(s[[4]])),
    housing_loan = factor(v[6], levels(s[[5]])),
    personal_loan = factor(v[7], levels(s[[6]])),
    contact = factor(v[8], levels(s[[7]])),
    month = factor(v[9], levels(s[[8]])),
    day_of_week = factor(v[10], levels(s[[9]])),
    prior_outcome = factor(v[11], levels(s[[10]])),
    cons_price_idx = as.numeric(v[12]),
    cons_conf_idx = as.numeric(v[13]),
    euribor3m = as.numeric(v[14]),
    term_depost = factor(v[15], levels(s[[14]])),
    total_contacts = as.numeric(v[16])
  )
}

#* @param inputline
#* @serializer unboxedJSON
#* @get /logistic
function(inputline){
  predict(logistic, p(inputline), type = "response")
}

#* @param inputline
#* @serializer unboxedJSON
#* @get /ranger
function(inputline){
  predict(randomForest, p(inputline))$prediction
}

# inputline<-"56,housemaid,married,basic.4y,no,no,no,telephone,may,mon,nonexistent,93.994,-36.4,4.857,no,1"
# inputline<-"59,retired,married,professional.course,unknown,yes,yes,cellular,jul,mon,nonexistent,93.918,-42.7,4.96,yes,4"

# rsconnect::deployAPI("../04_predict_api", server = 'colorado.rstudio.com', account = "nathan")

