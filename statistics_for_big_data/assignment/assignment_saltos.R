library("readxl")
library("dplyr")


data_path <- "C:/Users/thomas/ownCloud/data science/Statistics for Big Data/2nd assignment/project 2 stat for big data.xls"
data <- read_excel(data_path)
print(data)


data$SUBSCRIBED = as.factor(data$SUBSCRIBED)
data$job = as.factor(data$job)
data$marital = as.factor(data$marital)
data$education = as.factor(data$education)
data$month = as.factor(data$month)
summary(data)

model_1 = glm(formula = SUBSCRIBED ~ job + marital + education + previous + month + cons.price.idx + cons.conf.idx + euribor3m + nr.employed, data = data, family = binomial)
summary(model_1)$coefficients[1:36]

# divide and recombine 10 splits
set.seed(10)
rows <- sample(nrow(data))
data_shuffle <- data[rows, ]
n <- 10
dfchunk <- split(data_shuffle, factor(sort(rank(row.names(data_shuffle))%%n)))
summary(dfchunk$`0`)

cu <- 0
models = list()
for (chunk in dfchunk){
model_2 = glm(formula = SUBSCRIBED ~ job + marital 
+ education + previous + month + cons.price.idx 
+ cons.conf.idx + euribor3m + nr.employed, data = chunk, family = binomial)
print(cu)
cu <- cu+1
models[[cu]] <- summary(model_2)$coefficients[1:36]
}

part_coeff = Reduce("+", models) / length(models)

# 20  splits
n <- 20
dfchunk <- split(data_shuffle, factor(sort(rank(row.names(data_shuffle))%%n)))
summary(dfchunk$`0`)

cu <- 0
models = list()
for (chunk in dfchunk){
model_3 = glm(formula = SUBSCRIBED ~ job + marital 
+ education + previous + month + cons.price.idx 
+ cons.conf.idx + euribor3m + nr.employed, data = chunk, family = binomial)
print(cu)
cu <- cu+1
models[[cu]] <- summary(model_3)$coefficients[1:36]
}

full_coeff = Reduce("+", models) / length(models)
part_coeff
full_coeff
