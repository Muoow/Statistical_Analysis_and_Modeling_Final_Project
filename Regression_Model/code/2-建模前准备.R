# 抽样
set.seed(2352985)
df_sample <- df %>% sample_n(1000)

p1 <- ggplot(df_sample, aes(x = Delivery_person_Age, y = Time_taken)) +
  geom_point(color = "blue", alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Scatter plot: Age vs Time Taken", x = "Delivery Person Age", y = "Time Taken (min)") +
  theme_minimal()

p2 <- ggplot(df_sample, aes(x = Delivery_person_Ratings, y = Time_taken)) +
  geom_point(color = "green", alpha = 0.5) +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Scatter plot: Ratings vs Time Taken", x = "Delivery Person Ratings", y = "Time Taken (min)") +
  theme_minimal()

p3 <- ggplot(df_sample, aes(x = Distance_km, y = Time_taken)) +
  geom_point(color = "orange", alpha = 0.5) +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Scatter plot: Distance vs Time Taken", x = "Distance (km)", y = "Time Taken (min)") +
  theme_minimal()

grid.arrange(p1, p2, p3, ncol = 2)

variables <- c("Delivery_person_Age", "Delivery_person_Ratings", 
               "Distance_km", "Time_taken")
plots <- list()

for(var in variables) {
  skew_val <- round(moments::skewness(df[[var]], na.rm = TRUE), 3)
  kurt_val <- round(moments::kurtosis(df[[var]], na.rm = TRUE) - 3, 3)
  
  p <- ggplot(df_sample, aes_string(x = var)) +
    geom_density(fill = "lightblue", alpha = 0.7, color = "blue") +
    geom_vline(xintercept = mean(df[[var]], na.rm = TRUE), 
               color = "red", linetype = "dashed", linewidth = 1) +
    labs(title = paste("Distribution of", var),
         subtitle = paste("Skewness:", skew_val, "| Kurtosis:", kurt_val),
         x = var, y = "Density") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(color = "darkgray"))
  
  plots[[var]] <- p
}

grid.arrange(grobs = plots, ncol = 2)

# 计算相关系数矩阵
cor_matrix <- cor(df_sample %>% select(Delivery_person_Age, Delivery_person_Ratings, Distance_km, Time_taken), method = "spearman")

# 将相关矩阵转换格式
library(reshape2)
cor_matrix_melted <- melt(cor_matrix)

# 绘制相关系数热力图
ggplot(cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +  
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limits = c(-1, 1)) +  
  labs(title = "Correlation Matrix Heatmap", x = "Variables", y = "Variables", fill = "Correlation") +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

df_sample$Type_of_order  <- factor(df_sample$Type_of_order)
df_sample$Type_of_vehicle  <- factor(df_sample$Type_of_vehicle)

p4 <- ggplot(df_sample, aes(x = Type_of_order, y = Time_taken)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +  
  labs(title = "Boxplot: Order Type vs Time Taken", x = "Order Type", y = "Time Taken (min)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# 执行单因素方差分析（ANOVA）
anova_result <- aov(Time_taken ~ Type_of_order, data = df)
# 输出结果
summary(anova_result)

p5 <- ggplot(df_sample, aes(x = Type_of_vehicle, y = Time_taken)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +  
  labs(title = "Boxplot: Vehicle Type vs Time Taken", x = "Vehicle Type", y = "Time Taken (min)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# 执行单因素方差分析（ANOVA）
anova_result <- aov(Time_taken ~ Type_of_vehicle, data = df)
# 输出结果
summary(anova_result)

# 加载car包计算VIF
library(car)
# 构建初步回归模型
model <- lm(Time_taken ~ Delivery_person_Age + Delivery_person_Ratings + Distance_km + Type_of_vehicle,
            data = df_sample) 

# 计算VIF
vif(model)
