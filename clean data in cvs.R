installed.packages("tidyverse")
#install.packages("dplyr")
library(tidyverse)
Cooked_Transactions

# To delete $ 
df<- data.frame(Cooked_Transactions) %>%
  mutate (Amount1= gsub("[$]", "", df$Amount)) %>% #remove $
  mutate(datee=gsub("(.{10})", "\\1 ", df$Date)) #add space between date and time in new column
view(df)
# to delet Amount column
df<- subset(df, select = -c(Amount))

view(df2)


# remove date column
df <- subset(df, select = -c(Date))
view(df)

#separate date column into time and date
df<-  separate(df, col = datee, into = c("Date","Time"), sep = " ")
View(df)




   