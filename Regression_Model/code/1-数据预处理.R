# 导入必要的库
library(ggplot2)
library(tidyverse)
library(geosphere)
library(gridExtra)

# 导入数据
df <- read.csv("../deliverytime.csv", stringsAsFactors = FALSE)

# 删除不需要的字段
df <- df %>%
  select(
    -ID,
    -Delivery_person_ID
  )

# 规范命名
df <- df %>%
  rename(
    Time_taken = `Time_taken.min.`
  )

# 查看字段名
names(df)

# 检查缺失值
missing_values <- colSums(is.na(df))

# 根据经纬度计算距离
df <- df %>%
  mutate(
    Distance_km = distHaversine(
      cbind(Restaurant_longitude, Restaurant_latitude),
      cbind(Delivery_location_longitude, Delivery_location_latitude)
    ) / 1000
  )

df <- df %>%
  select(
    -Restaurant_latitude,
    -Restaurant_longitude,
    -Delivery_location_latitude,
    -Delivery_location_longitude
  )

total_n <- nrow(df_sample)

# 评分小于 2 的数量与占比
low_n <- sum(df_sample$Delivery_person_Ratings < 2)
low_prop <- low_n / total_n

# 评分大于 5 的数量与占比
high_n <- sum(df_sample$Delivery_person_Ratings > 5)
high_prop <- high_n / total_n

# 配送速度异常值检测
df$Speed = df$Distance_km / df$Time_taken

df <- df %>%
  filter(
    Speed >= 0.1,
    Speed <= 0.2
  )

p_age <- ggplot(df, aes(y = Delivery_person_Age)) +
    geom_boxplot() +
    labs(title = "Boxplot: Delivery_person_Age") +
    theme_minimal()

p_rating <- ggplot(df, aes(y = Delivery_person_Ratings)) +
    geom_boxplot() +
    labs(title = "Boxplot: Delivery_person_Ratings") +
    theme_minimal()

p_distance <- ggplot(df, aes(y = Distance_km)) +
    geom_boxplot() +
    labs(title = "Boxplot: Distance_km") +
    theme_minimal()

p_time <- ggplot(df, aes(y = Time_taken)) +
    geom_boxplot() +
    labs(title = "Boxplot: Time_taken") +
    theme_minimal()

p_speed <- ggplot(df, aes(y = Speed)) +
    geom_boxplot() +
    labs(title = "Boxplot: Speed") +
    theme_minimal()

grid.arrange(p_age, p_rating, p_distance, p_time, p_speed, ncol = 2)

p_order <- ggplot(df, aes(x = Type_of_order)) +
    geom_bar() +
    labs(title = "Frequency: Type_of_order", x = "Order Type", y = "Count") +
    theme_minimal()

p_vehicle <- ggplot(df, aes(x = Type_of_vehicle)) +
    geom_bar() +
    labs(title = "Frequency: Type_of_vehicle", x = "Vehicle Type", y = "Count") +
    theme_minimal()

grid.arrange(p_order, p_vehicle, ncol = 2)
