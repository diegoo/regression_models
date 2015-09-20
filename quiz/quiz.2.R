# 1) y 2)

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

fit <- lm(y ~ x)
> summary(fit)

#             Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   0.1885     0.2061   0.914    0.391  
# x             0.7224     0.3107   2.325    0.053 .
# F-statistic: 5.408 on 1 and 7 DF,  p-value: 0.05296

# 3)

data(mtcars)
y <- mtcars$mpg
x <- mtcars$wt
fit_mtcars <- lm(y ~ x)
predict(fit_mtcars, newdata = data.frame(x = mean(x)), interval = "confidence")

#        fit      lwr      upr
# 1 20.09062 18.99098 21.19027

# 4)

# Weight (lb/1000) 

# 5)

predict(fit_mtcars, newdata = data.frame(x = 3), interval = "prediction")

#  fit      lwr      upr
# 1 21.25171 14.92987 27.57355

# 6)

halfx <- x / 2
halfx_fit <- lm(y ~ halfx)

# -10.68894
# "the expected change in mpg per 1 short ton increase in weight" -> esto es la pendiente del modelo (el cambio en y por cada cambio en x)
b1 <- coef(halfx_fit)[2]

# desvío standard para la pendiente
summary(halfx_fit)$coefficients[2,2]
# 1.118202

mean <- summary(halfx_fit)$coefficients[2, 1]      
std_error <- summary(halfx_fit)$coefficients[2, 2] 
df <- halfx_fit$df

mean + c(-1,1) * qt(0.975, df=df) * std_error
[1] -12.97262  -8.40527

# 7)

summary(lm(y ~ x))$coefficients[2, 1]

# [1] -5.344472
x_meters <- x / 100
x_meters_fit <- lm(y ~ x_meters)
summary(x_meters_fit)$coefficients[2, 1]

# [1] -534.4472

# 9)

