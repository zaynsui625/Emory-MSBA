setwd("C:\\Users\\henry\\Documents\\MSBAprogram\\MachineLearning1\\Project Data")


Labs <- scan(file="ProjectTestData.csv",what="xx",sep=",",nlines=1)
Data <- matrix(
            scan(file="ProjectTestData.csv",what="xx",sep=",",skip=1),
            ncol=length(Labs),byrow=T)
colnames(Data) <- Labs

library(dplyr)

Data_df <- as.data.frame(Data)

Data_1 <- as.data.frame(Data)

#This line was used to create data for testing our models
#Data_1 <- sample_frac(Data_df, .01)

Data_1 <- as.matrix(Data_1)


# How many unique values in each variable?

apply(Data_1,2,FUN=function(x){length(unique(x))})


remove(Data)
remove(Data_df)

Data_1_df <- as.data.frame(Data_1)



#Data examination and Preparation for each column
#Observe the data to see distribution of variables
#Assign any occurrences that do not appear in the data at least 1% of the time as "other" 

#id

tmp_id <- sort(as.numeric(table(Data_1[,'id'])),decreasing=T)
plot(tmp_id,ylab="Count")
title("Pareto of the number of observations
      for each category of id")

table(Data_1_df$id)



#click

tmp_click <- sort(as.numeric(table(Data_1[,'click'])),decreasing=T)
plot(tmp_click,ylab="Count")
title("Pareto of the number of observations
      for each category of click")

table(Data_1_df$click)

#hour, remake hour into 4 new columns, drop year and month, keep hour, and keep day of month, add day of week

Data_1_df <- tidyr::separate(
    data = Data_1_df,
    col = hour,
    sep = c(2, 4, 6),
    into = c("y", "m", "day", "hour"),
    remove = FALSE
  ) 

Data_1_df$day <- as.numeric(Data_1_df$day)
class(Data_1_df$day)
Data_1_df$hour <- as.numeric(Data_1_df$hour)
class(Data_1_df$hour)

Data_1_df <- Data_1_df %>% 
  subset(select = -c(y, m)) %>% 
  mutate(weekday= (day+1)%%7)


Data_1 <- as.matrix(Data_1_df)

#hour

tmp_hour <- sort(as.numeric(table(Data_1[,'hour'])),decreasing=T)
plot(tmp_hour,ylab="Count")
title("Pareto of the number of observations
      for each category of hour")

table(Data_1_df$hour)

#day

tmp_day <- sort(as.numeric(table(Data_1[,'day'])),decreasing=T)
plot(tmp_day, ylab="Count")
title("Pareto of the number of observations
      for each category of day")

table(Data_1_df$day)

#weekday

tmp_weekday <- sort(as.numeric(table(Data_1[,'weekday'])),decreasing=T)
plot(tmp_weekday, ylab="Count")
title("Pareto of the number of observations
      for each category of weekday")

table(Data_1_df$weekday)

#C1

Data_1_df <- Data_1_df %>% 
  group_by(C1) %>% 
  mutate(C1_count = n()) %>% 
  mutate(C1_prop = C1_count/nrow(Data_1_df)) %>% 
  mutate(C1 = ifelse(C1_prop >= .01, C1, "other")) %>% 
  subset(select = -c(C1_count, C1_prop))

tmp_C1 <- sort(as.numeric(table(Data_1[,'C1'])),decreasing=T)
plot(tmp_C1,ylab="Count")
title("Pareto of the number of observations
      for each category of C1")

table(Data_1_df$C1)

#banner_pos

tmp_banner_pos <- sort(as.numeric(table(Data_1[,'banner_pos'])),decreasing=T)
plot(tmp_banner_pos,ylab="Count")
title("Pareto of the number of observations
      for each category of banner_pos")

table(Data_1_df$banner_pos)

#site_id, take any values that appear more than 1% of the time and assign the rest to "other"

Data_1_df <- Data_1_df %>% 
  group_by(site_id) %>% 
  mutate(site_id_count = n()) %>% 
  mutate(site_id_prop = site_id_count/nrow(Data_1_df)) %>% 
  mutate(site_id = ifelse(site_id_prop >= .01, site_id, "other")) %>% 
  subset(select = -c(site_id_count, site_id_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_site_id <- sort(as.numeric(table(Data_1[,'site_id'])),decreasing=T)
plot(tmp_site_id,ylab="Count")
title("Pareto of the number of observations
      for each category of site_id")

table(Data_1_df$site_id)


#site_domain

Data_1_df <- Data_1_df %>% 
  group_by(site_domain) %>% 
  mutate(site_domain_count = n()) %>% 
  mutate(site_domain_prop = site_domain_count/nrow(Data_1_df)) %>% 
  mutate(site_domain = ifelse(site_domain_prop >= .01, site_domain, "other")) %>% 
  subset(select = -c(site_domain_count, site_domain_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_site_domain <- sort(as.numeric(table(Data_1[,'site_domain'])),decreasing=T)
plot(tmp_site_domain,ylab="Count")
title("Pareto of the number of observations
      for each category of site_domain")

table(Data_1_df$site_domain)

#site_category

Data_1_df <- Data_1_df %>% 
  group_by(site_category) %>% 
  mutate(site_category_count = n()) %>% 
  mutate(site_category_prop = site_category_count/nrow(Data_1_df)) %>% 
  mutate(site_category = ifelse(site_category_prop >= .01, site_category, "other")) %>% 
  subset(select = -c(site_category_count, site_category_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_site_category <- sort(as.numeric(table(Data_1[,'site_category'])),decreasing=T)
plot(tmp_site_category,ylab="Count")
title("Pareto of the number of observations
      for each category of site_category")

table(Data_1_df$site_category)

#app_id

Data_1_df <- Data_1_df %>% 
  group_by(app_id) %>% 
  mutate(app_id_count = n()) %>% 
  mutate(app_id_prop = app_id_count/nrow(Data_1_df)) %>% 
  mutate(app_id = ifelse(app_id_prop >= .01, app_id, "other")) %>% 
  subset(select = -c(app_id_count, app_id_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_app_id <- sort(as.numeric(table(Data_1[,'app_id'])),decreasing=T)
plot(tmp_app_id,ylab="Count")
title("Pareto of the number of observations
      for each category of app_id")

table(Data_1_df$app_id)

#app_domain

Data_1_df <- Data_1_df %>% 
  group_by(app_domain) %>% 
  mutate(app_domain_count = n()) %>% 
  mutate(app_domain_prop = app_domain_count/nrow(Data_1_df)) %>% 
  mutate(app_domain = ifelse(app_domain_prop >= .01, app_domain, "other")) %>% 
  subset(select = -c(app_domain_count, app_domain_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_app_domain <- sort(as.numeric(table(Data_1[,'app_domain'])),decreasing=T)
plot(tmp_app_domain,ylab="Count")
title("Pareto of the number of observations
      for each category of app_domain")

table(Data_1_df$app_domain)

#app_category

Data_1_df <- Data_1_df %>% 
  group_by(app_category) %>% 
  mutate(app_category_count = n()) %>% 
  mutate(app_category_prop = app_category_count/nrow(Data_1_df)) %>% 
  mutate(app_category = ifelse(app_category_prop >= .01, app_category, "other")) %>% 
  subset(select = -c(app_category_count, app_category_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_app_category <- sort(as.numeric(table(Data_1[,'app_category'])),decreasing=T)
plot(tmp_app_category,ylab="Count")
title("Pareto of the number of observations
      for each category of app_category")

table(Data_1_df$app_category)

#device_id

Data_1_df <- Data_1_df %>% 
  group_by(device_id) %>% 
  mutate(device_id_count = n()) %>% 
  mutate(device_id_prop = device_id_count/nrow(Data_1_df)) %>% 
  mutate(device_id = ifelse(device_id_prop >= .01, device_id, "other")) %>% 
  subset(select = -c(device_id_count, device_id_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_device_id <- sort(as.numeric(table(Data_1[,'device_id'])),decreasing=T)
plot(tmp_device_id,ylab="Count")
title("Pareto of the number of observations
      for each category of device_id")

table(Data_1_df$device_id)

#device_ip

Data_1_df <- Data_1_df %>% 
  group_by(device_ip) %>% 
  mutate(device_ip_count = n()) %>% 
  mutate(device_ip_prop = device_ip_count/nrow(Data_1_df)) %>% 
  mutate(device_ip = ifelse(device_ip_prop >= .01, device_ip, "other")) %>% 
  subset(select = -c(device_ip_count, device_ip_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_device_ip <- sort(as.numeric(table(Data_1[,'device_ip'])),decreasing=T)
plot(tmp_device_ip,ylab="Count")
title("Pareto of the number of observations
      for each category of device_ip")

table(Data_1_df$device_ip)

Data_1_df <- Data_1_df %>% 
  subset(select = -c(device_ip))

#device_model

Data_1_df <- Data_1_df %>% 
  group_by(device_model) %>% 
  mutate(device_model_count = n()) %>% 
  mutate(device_model_prop = device_model_count/nrow(Data_1_df)) %>% 
  mutate(device_model = ifelse(device_model_prop >= .01, device_model, "other")) %>% 
  subset(select = -c(device_model_count, device_model_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_device_model <- sort(as.numeric(table(Data_1[,'device_model'])),decreasing=T)
plot(tmp_device_model,ylab="Count")
title("Pareto of the number of observations
      for each category of device_model")

table(Data_1_df$device_model)

#device_type

Data_1_df <- Data_1_df %>% 
  group_by(device_type) %>% 
  mutate(device_type_count = n()) %>% 
  mutate(device_type_prop = device_type_count/nrow(Data_1_df)) %>% 
  mutate(device_type = ifelse(device_type_prop >= .01, device_type, "other")) %>% 
  subset(select = -c(device_type_count, device_type_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_type_model <- sort(as.numeric(table(Data_1[,'device_type'])),decreasing=T)
plot(tmp_type_model,ylab="Count")
title("Pareto of the number of observations
      for each category of device_type")

table(Data_1_df$device_type)

#device_conn_type

Data_1_df <- Data_1_df %>% 
  group_by(device_conn_type) %>% 
  mutate(device_conn_type_count = n()) %>% 
  mutate(device_conn_type_prop = device_conn_type_count/nrow(Data_1_df)) %>% 
  mutate(device_conn_type = ifelse(device_conn_type_prop >= .01, device_conn_type, "other")) %>% 
  subset(select = -c(device_conn_type_count, device_conn_type_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_conn_type_model <- sort(as.numeric(table(Data_1[,'device_conn_type'])),decreasing=T)
plot(tmp_conn_type_model,ylab="Count")
title("Pareto of the number of observations
      for each category of device_conn_type")

table(Data_1_df$device_conn_type)

#C14

Data_1_df <- Data_1_df %>% 
  group_by(C14) %>% 
  mutate(C14_count = n()) %>% 
  mutate(C14_prop = C14_count/nrow(Data_1_df)) %>% 
  mutate(C14 = ifelse(C14_prop >= .01, C14, "other")) %>% 
  subset(select = -c(C14_count, C14_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_C14 <- sort(as.numeric(table(Data_1[,'C14'])),decreasing=T)
plot(tmp_C14,ylab="Count")
title("Pareto of the number of observations
      for each category of C14")

table(Data_1_df$C14)

#C15

Data_1_df <- Data_1_df %>% 
  group_by(C15) %>% 
  mutate(C15_count = n()) %>% 
  mutate(C15_prop = C15_count/nrow(Data_1_df)) %>% 
  mutate(C15 = ifelse(C15_prop >= .01, C15, "other")) %>% 
  subset(select = -c(C15_count, C15_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_C15 <- sort(as.numeric(table(Data_1[,'C15'])),decreasing=T)
plot(tmp_C15,ylab="Count")
title("Pareto of the number of observations
      for each category of C15")

table(Data_1_df$C15)

#C16

Data_1_df <- Data_1_df %>% 
  group_by(C16) %>% 
  mutate(C16_count = n()) %>% 
  mutate(C16_prop = C16_count/nrow(Data_1_df)) %>% 
  mutate(C16 = ifelse(C16_prop >= .01, C16, "other")) %>% 
  subset(select = -c(C16_count, C16_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_C16 <- sort(as.numeric(table(Data_1[,'C16'])),decreasing=T)
plot(tmp_C16,ylab="Count")
title("Pareto of the number of observations
      for each category of C16")

table(Data_1_df$C16)

#C17

Data_1_df <- Data_1_df %>% 
  group_by(C17) %>% 
  mutate(C17_count = n()) %>% 
  mutate(C17_prop = C17_count/nrow(Data_1_df)) %>% 
  mutate(C17 = ifelse(C17_prop >= .01, C17, "other")) %>% 
  subset(select = -c(C17_count, C17_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_C17 <- sort(as.numeric(table(Data_1[,'C17'])),decreasing=T)
plot(tmp_C17,ylab="Count")
title("Pareto of the number of observations
      for each category of C17")

table(Data_1_df$C17)

#C18

Data_1_df <- Data_1_df %>% 
  group_by(C18) %>% 
  mutate(C18_count = n()) %>% 
  mutate(C18_prop = C18_count/nrow(Data_1_df)) %>% 
  mutate(C18 = ifelse(C18_prop >= .01, C18, "other")) %>% 
  subset(select = -c(C18_count, C18_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_C18 <- sort(as.numeric(table(Data_1[,'C18'])),decreasing=T)
plot(tmp_C18,ylab="Count")
title("Pareto of the number of observations
      for each category of C18")

table(Data_1_df$C18)

#C19

Data_1_df <- Data_1_df %>% 
  group_by(C19) %>% 
  mutate(C19_count = n()) %>% 
  mutate(C19_prop = C19_count/nrow(Data_1_df)) %>% 
  mutate(C19 = ifelse(C19_prop >= .01, C19, "other")) %>% 
  subset(select = -c(C19_count, C19_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_C19 <- sort(as.numeric(table(Data_1[,'C19'])),decreasing=T)
plot(tmp_C19,ylab="Count")
title("Pareto of the number of observations
      for each category of C19")

table(Data_1_df$C19)

#C20

Data_1_df <- Data_1_df %>% 
  group_by(C20) %>% 
  mutate(C20_count = n()) %>% 
  mutate(C20_prop = C20_count/nrow(Data_1_df)) %>% 
  mutate(C20 = ifelse(C20_prop >= .01, C20, "other")) %>% 
  subset(select = -c(C20_count, C20_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_C20 <- sort(as.numeric(table(Data_1[,'C20'])),decreasing=T)
plot(tmp_C20,ylab="Count")
title("Pareto of the number of observations
      for each category of C20")

table(Data_1_df$C20)

#C21

Data_1_df <- Data_1_df %>% 
  group_by(C21) %>% 
  mutate(C21_count = n()) %>% 
  mutate(C21_prop = C21_count/nrow(Data_1_df)) %>% 
  mutate(C21 = ifelse(C21_prop >= .01, C21, "other")) %>% 
  subset(select = -c(C21_count, C21_prop))

Data_1 <- as.matrix(Data_1_df)

tmp_C21 <- sort(as.numeric(table(Data_1[,'C21'])),decreasing=T)
plot(tmp_C21,ylab="Count")
title("Pareto of the number of observations
      for each category of C21")

table(Data_1_df$C21)


#write new csv

write.csv(Data_1_df, "ProjectManipulatedTestData.csv", row.names = TRUE, col.names = TRUE)
