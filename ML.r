view(mtcars)
mtcars$mpg
mtcars$hp
nrow(mtcars)
ncol(mtcars)
str(mtcars)


cor(mtcars$mpg , mtcars$hp, method =  "pearson")
cor.test(mtcars$hp, mtcars$mpg)
boxplot(mtcars$mpg)
fivenum(mtcars$mpg)
scatter.smooth(mtcars$mpg,mtcars$hp)
help("boxplot")

plot(mtcars$hp,mtcars$mpg, pch = 1)

## Correlation Metric
cor(mtcars[ ,c("mpg","hp","wt")])

## Correlation Metric in dplyr
mtcars %>%
  select("mpg","wt","hp") %>%
  cor()

## Linear Regression
lmFit <- lm(mpg ~ hp, data = mtcars)
lm(mtcars$mpg ~ mtcars$hp)

summary(lmFit)

## Predict
lmFit$coefficients

lmFit$coefficients[[1]]+lmFit$coefficients[[2]]*200

## Predict many item
new_car <- data.frame(hp = c(250,220,400,410,450))
lmFit$coefficients[[1]]+lmFit$coefficients[[2]]*new_car

predict(lmFit, newdata = new_car)

## add predicted data column to new_car 
new_car$mpg_prdic <- predict(lmFit, newdata = new_car)
new_car

# at hp 450  mpg_prdict = -0.603 it not makesence because linear model has not 
# know value of hp 450 before. The model has a max value of hp at 335  
# if you put predict data that had not have in the hp dataset before it will
# have oppertunity to predict non-sense value 
# so don't put predict value over 10% 0f data you have 
# how to fix it  :  get sample data more 

summary(mtcars$hp)

## Root mean squared error
## Multiple linear Regression
# mpg = f(hp, wt, am)
# mpg = b0 + b1(hp) + b2(wt) + b3(am)

lmFit_2 <- lm(mpg ~ hp + wt + am, data = mtcars)
cof <- coef(lmFit_2)

# แทนค่าในโมเดล
cof[[1]] + cof[[2]]*200 + cof[[3]]*3.5 + cof[[4]]*1


## Build full model  & คำนวณ  RMSE
lm(mpg ~ . - gear , data = mtcars)  # - gear คือลบคอลัมน์ gear ออก ไม่เอามาคำนวณ

lmFit_full <- lm(mpg ~ ., data = mtcars)# ให้มัน predict ตัวมันเองยังไม่ต้องใส่ new_data

#Train RMSE
mtcars$predicted <- predict(lmFit_full)
mt_residual <- mtcars$mpg - mtcars$predicted
mt_mean_residual <- mt_residual^2
mt_sse <- sum(mt_mean_residual)
mt_RMSE <- sqrt(mean(mt_mean_residual))  # ยิ่งต่ำยิ่งดี 
      # โดยเฉลี่ยโมเดลของเราคำนวณค่า mpg ผิดไปประมาณ 2.14 point ตลอดทั้งเส้น regr

## split data
set.seed(42)
sample(1:10, 3)
n <- nrow(mtcars)
id <- sample(1:n, n * 0.8)

train_data <- mtcars[id, ]
test_data <- mtcars[-id, ]

# Run line 82-88 together

model1 <- lm(mpg ~ hp + wt , data = train_data)
model2 <- lm(mpg ~ hp + wt , data = test_data)

train_data$predicted_train <- predict(model1)
RMSE_train <- sqrt(mean((train_data$mpg - train_data$predicted_train)^2))

## Test data model
p_test <- predict(model1 , newdata = test_data)
error_test <- test_data$mpg - p_test
RMSE_test <- sqrt(mean(error_test^2))


cat("RMSE_TRAIN : ",RMSE_train, "\n")
cat("RMSE_TEST : ",RMSE_test)


cat("RMSE_TRAIN : ",RMSE_train, "\nRMSE_TEST : ",RMSE_test)


