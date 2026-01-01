model <- lm(Time_taken ~ Delivery_person_Age + Delivery_person_Ratings + Distance_km + Type_of_vehicle, 
                         data = df_sample)
summary(model)

model <- lm(log(Time_taken + 1) ~ Delivery_person_Age + Delivery_person_Ratings + Distance_km + Type_of_vehicle, 
                         data = df_sample)
summary(model)

model_residuals <- residuals(model)
model_fitted <- fitted(model)

# 残差 vs 拟合值图
ggplot(data.frame(Fitted = model_fitted, Residuals = model_residuals),
       aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()

# Q-Q 图
qqnorm(model_residuals)
qqline(model_residuals, col = "red")

set.seed(123)  
res_sample <- sample(model_residuals, 100)

shapiro.test(res_sample)
