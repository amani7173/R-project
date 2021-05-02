#install.packages("pdftools")
library(tidyverse)
library(pdftools)
library(stringr)

raw_emails <- pdf_text("/Users/mz195/Downloads/Transactions.pdf")
date_pattern <- "\\d{4}[-/.]\\d{2}[-/.]\\d{2}"
time_pattern <- "\\s\\d{2}[-:.]\\d{2}"
amount_pattern <- "[:digit:]+(\\.[:digit:]+|\\s)"
currency_pattern <- "\\(\\s[:upper:]+\\s\\)"

# Extract the transaction reference from the email
Get_transaction_id <- function(x){
  x <- str_replace_all(x, "TRANSACTION REF", "")
  x <- str_replace_all(x, "رﻗﻢ اﻟﻌﻤﻠﯿﺔ", "")
  x <- gsub(" ", "", x, fixed = TRUE)
  return(x)
}

# Extract the transaction type from the email
Get_transaction_type <- function(x){
  if(grepl("DEBIT", x, fixed = TRUE)){
    return("DEBIT")
  } else {
    return("CREDIT")
  }
}

# Extract the merchant from the email
Get_merchant <- function(x){
  x <- str_replace_all(x, "Merchant name", "")
  x <- str_replace_all(x, "‫اﺳﻢ اﻟﺘﺎﺟﺮ‬", "")
  x <- trimws(x)
  return(x)
}

# Extract the country from the email
Get_country <- function(x){
  x <- str_replace_all(x, "Country", "")
  x <- str_replace_all(x, "اﻟﺪوﻟﺔ‬", "")
  x <- gsub(" ", "", x, fixed = TRUE)
  x <- gsub("\\-.*","",x)
  return(x)
}

# Columns:
#   ID
#   Date
#   Time
#   Type - Credit, Debit
#   Amount
#   Currency
#   Merchant/Category
#   country

col_names <- c("ID", "Date", "Time", "Type", "Amount", "Currency", "Merchant", "Country")
result <- c()

for(email in raw_emails){
  # Only valuable lines to process 
  current_transaction <- rep(NA,8)
  lines <- strsplit(email, "\n")[[1]][6:15]
  for(line in lines){
    
    # Get the transaction reference
    if(grepl("REF", line)){
      transaction_id <- Get_transaction_id(line)
      current_transaction[1] <- transaction_id
    }

    # Get the date & time
    if(grepl("DATE", line)){
      date_str <- str_extract(line, pattern = date_pattern)
      time_str <- str_extract(line, pattern = time_pattern)
      current_transaction[2] <- date_str
      current_transaction[3] <- time_str
    }

    # Get the transaction type
    if(grepl("A/C", line)){
      transaction_type <- Get_transaction_type(line)
      current_transaction[4] <- transaction_type
    }

    # Get the amount and currency
    if(grepl("THE SUM", line) || grepl("Amount", line)){
      amount <- str_extract(line, pattern = amount_pattern)
      currency <- str_extract(line, pattern = currency_pattern)
      current_transaction[5] <- amount
      current_transaction[6] <- currency
    }

    # Get the merchant
    if(grepl("Merchant", line)){
      merchant <- Get_merchant(line)
      current_transaction[7] <- merchant
    }

    # Get the country of purchase
    if(grepl("Country", line)){
      country <- Get_country(line)
      current_transaction[8] <- country
    }
  }
  result <- rbind(result, current_transaction)
}

df <- data.frame(result)
colnames(df) <- col_names
rownames(df) <- NULL
dim(df)
View(df)