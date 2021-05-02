library(tidyverse)
library(gridExtra)


id <- "1Ypts-KxfXmwYH9sq_8e9JVXer0jkxUtlanyqkNeFXe4"
df <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
#View(df)


# Change type to Date
df$Date <- as.Date(df$Date, format = "%d/%m/%Y")


# Barchart: annual credit & debit details 
chart1<- df %>%
  mutate(Year = format(Date, "%Y"),month = format(Date, "%m")) %>%
  group_by(Year,month, Type) %>%
  summarise(Total = sum(Amount)) %>%
  ggplot(aes(x=Year, y=Total, fill=Type))+
  geom_bar(stat="identity", position = 'dodge')


# Barchart: monthly credit & debit details  
chart2<-df %>%
  mutate(Month = format(Date, "%Y-%m")) %>%
  group_by(Month, Type) %>%
  summarise(Total = sum(Amount)) %>%
  ggplot(aes(x=Month, y=Total, fill=Type)) +
  theme(legend.position="none")+
  scale_x_reverse(Month)+
  geom_bar(stat="identity")




# Balance line_chart
df1 <- df %>%
  mutate(Month = format(Date, "%Y-%m")) %>%
  group_by(Month, Type) %>%
  summarise(total = sum(Amount)) 

# creating a new column to calculate the monthly balance amount after subtracting  debit from credit
df2 <- df1[df1$Type == "CREDIT",]
df3 <- df1[df1$Type == "DEBIT",]
df4 <- df2$total - df3$total
df5 <- cbind(df2, df4)
colnames(df5)[4] <- "Balance"
#View(df5)

chart3 <- df5 %>%
  ggplot(aes(x=Month, y=Balance, group=1,color="#1b98e0")) +
  geom_line()+
  geom_point()+
  theme(legend.position="none")


#to show the number of purchases per category
df <- ggplot(data = df)+
  geom_bar(mapping = aes(x=Category))+
theme(legend.position="none")
chart4 <- df+coord_cartesian()
chart4  


grid.arrange(chart1,chart2,chart3, ncol=1,nrow=3)