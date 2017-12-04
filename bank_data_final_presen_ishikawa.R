# ライブラリの読み込み
library(ggplot2)
library(car,lib.loc="C:\\Users\\takao\\OneDrive\\R_lib")

# データの読み込み
bank_data <- read.csv("homework_data/bank_marketing_train.csv")

# ロジスティック回帰の下準備
# 目的変数を0/1にする
bank_data$y <- ifelse(bank_data$y == "yes", 1, 0)

# pdaysをカテゴリに変換 ##0 - 7, 8~998, 999の3カわテゴリに分ける
#step1) 関数を作る
pdays.categorize <- function(x){
  if (x == 999){ 
    return("no")
  } else {
    return("yes")
  } 
} 
#step2) sapply関数でリストを作り、fieldを追加する
bank_data$contact_within_one_year <- sapply(bank_data$pdays, pdays.categorize)
bank_data$contact_within_one_year <- as.factor(bank_data$contact_within_one_year)

# age_groupに分ける
## age_groupの閾値を決める
student_data <- bank_data[bank_data$job == "student", ]
retired_data <- bank_data[bank_data$job == "retired", ]

## 学生を含む若者グループの上限値を決める
summary(student_data$age)
summary(student_data[student_data$y == 1, ]["age"]) 
## 上記から student&yの人たちの第三四分位が28歳なので、これをage group1の上限とする

## Retireした人たちを含むグループの下限値を決める
summary(retired_data$age)
summary(retired_data[retired_data$y == 1, ]["age"])
## 上記から、retired$yの人たちの第一四分位が57歳なので、これをage group3の加減とする
## 上記のグループの属さない群を入れて、3グループ作る

## 関数を作る
age_categorize <- function(x){
  if (x <= 28){ 
    return("young_group")
  } else if (x >= 57) {
    return("old_group")
  } else {
    return("mid-age_group")
  } 
} 

## age_categoryのFieldを作成
bank_data$age_category <- sapply(bank_data$age, age_categorize)
bank_data$age_category <- as.factor(bank_data$age_category)
summary(bank_data)

# 文字系の列をダミー変数にする
# カテゴリ変数
bank_data$job <- as.factor(bank_data$job)
bank_data$marital <- as.factor(bank_data$marital)
bank_data$contact <- as.factor(bank_data$contact)
bank_data$month <- as.factor(bank_data$month)
bank_data$day_of_week <- as.factor(bank_data$day_of_week)
bank_data$poutcome <- as.factor(bank_data$poutcome)
bank_data$loan <- as.factor(bank_data$loan)
bank_data$default <- as.factor(bank_data$default)
bank_data$housing <- as.factor(bank_data$housing)

# Train dataとTest dataへの分割
set.seed(1234)
train_idx <- sample(1:dim(bank_data)[1], size = dim(bank_data)[1] * 0.7)
train <- bank_data[train_idx, ]
test <- bank_data[-train_idx, ]
head(bank_data)

#ロジスティック回帰を実行
# mymodel <- glm(y~.-duration-age-pdays,data = train, family = "binomial")
# exp(mymodel$coefficients)
# step(mymodel)

#ステップ関数で最適化されたモデルを作成
# mymodel2step <- glm(formula = y ~ job + marital + education + default + housing + 
#                       loan + contact + month + day_of_week + campaign + previous + 
#                       poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
#                       euribor3m + contact_within_one_year + age_category, family = "binomial", 
#                     data = train)

#回帰係数の解読のために、オッズ比に変換
# exp(mymodel2step$coefficients)
# sprintf("%f", exp(mymodel2step$coefficients))

#残差を正規分布に近づけるべく、carを使って、VIFを確認する
# vif(mymodel2step)

# Loanとhousingが完全なマルチコになっているので、housingを削除
# mymodel2.1step <- glm(formula = y ~ job + marital + education + default + loan + 
#                         contact + month + day_of_week + campaign + previous + 
#                         poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
#                         euribor3m + contact_within_one_year + age_category, family = "binomial", 
#                       data = train)

#回帰係数の解読のために、オッズ比に変換
# exp(mymodel2.1step$coefficients)
# sprintf("%f", exp(mymodel2.1step$coefficients))

#残差を正規分布に近づけるべく、carを使って、VIFを確認する
# vif(mymodel2.1step)

#Monthを除外
# mymodel3 <- glm(formula = y ~ job + marital + education + default + loan + 
#                   contact + day_of_week + campaign + previous + 
#                   poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
#                   euribor3m + contact_within_one_year + age_category, family = "binomial", 
#                 data = train)
# vif(mymodel3)

#Emp_varianceを除外F
# mymodel4 <- glm(formula = y ~ job + marital + education + default + loan + 
#                   contact + day_of_week + campaign + previous + 
#                   poutcome + cons.price.idx + cons.conf.idx + 
#                   euribor3m + contact_within_one_year + age_category, family = "binomial", 
#                 data = train)
# vif(mymodel4)

#preivousを除外
# mymodel5 <- glm(formula = y ~ job + marital + education + default + loan + 
#                   contact + day_of_week + campaign + 
#                   poutcome + cons.price.idx + cons.conf.idx + 
#                   euribor3m + contact_within_one_year + age_category, family = "binomial", 
#                 data = train)
# vif(mymodel5)

#poutcomeを除外
# mymodel6 <- glm(formula = y ~ job + marital + education + default + loan + 
#                   contact + day_of_week + campaign + 
#                   cons.price.idx + cons.conf.idx + 
#                   euribor3m + contact_within_one_year + age_category, family = "binomial", 
#                 data = train)
# vif(mymodel6)

#Euribr3mを除外
# mymodel7 <- glm(formula = y ~ job + marital + education + default + loan + 
#                   contact + day_of_week + campaign + 
#                   cons.price.idx + cons.conf.idx + 
#                   contact_within_one_year + age_category, family = "binomial", 
#                 data = train)
# vif(mymodel7)

#Day_of_weekは使いづらいので、除外 →このモデリングを採択★ （AIC=13170)
mymodel8 <- glm(formula = y ~ job + marital + education + default + loan + 
                  contact + campaign + 
                  cons.price.idx + cons.conf.idx + 
                  contact_within_one_year + age_category, family = "binomial", 
                data = train)
exp(mymodel8$coefficients)
vif(mymodel8)
summary(mymodel8)

#Testデータを使って確率を予測する
y_pred <- predict(mymodel8, newdata = test, type = "response")
# y_pred

#閾値を決めて、フラグを作る
# hist(y_pred)
ypred_flag<-ifelse(y_pred>0.2063689, 1, 0)

#Confusion Matrixを作る
conf_mat <- table(test$y, ypred_flag)
conf_mat

#正解率(accuracy) ★92.32983%@th=0.2
accuracy <- (conf_mat[1] + conf_mat[4]) / (conf_mat[1] + conf_mat[2] + conf_mat[3] + conf_mat[4])
accuracy

#適合率(precision) ★36.05948%@th=0.2
precision <- conf_mat[4] / (conf_mat[3] + conf_mat[4])
precision

#再現率(recall) ★12.46787%@th=0.2
recall <- conf_mat[4] / (conf_mat[2] + conf_mat[4])
recall

#最後にROIの最大化をする
## 1) ROIを計算する関数を作る
ROI_calc <- function(call_threshold){
  f_ypred_flag <- ifelse(y_pred>call_threshold, 1, 0)
  f_conf_mat <- table(test$y, f_ypred_flag)
  ROI <- f_conf_mat[4] * 2000 - (f_conf_mat[3] + f_conf_mat[4]) * 500
  return(ROI)
}

## 2) Optimize関数を使い、ROIを最大化する → ★0.2063689, ★63000.なった
optimise(ROI_calc, interval = c(0, 0.5), maximum = TRUE)

## これからやることリスト：
# アタックリストを生成する関数、
## 1) 新しいデータのデータ加工
## 2) それをあらかじめ閾値の入ったモデルに入れて、predictionをする
## 3) Predictionをした結果(1/0のベクトル)を返す

# exp(mymodel8$coefficients)
# mymodel8 <- glm(formula = y ~ job + marital + education + default + loan + 
#                   contact + campaign + 
#                   cons.price.idx + cons.conf.idx + 
#                   contact_within_one_year + age_category, family = "binomial", 
#                 data = train)

#####################################################################################
#### データの予測のためのテンプレート ### 
# データ読み込み
real_data <- read.csv("homework_data/bank_marketing_test.csv")   #ここに新しいデータを入れる！！！

## as.factorへの置き換えとField作成
real_data$job <- as.factor(real_data$job)
real_data$marital <- as.factor(real_data$marital)
real_data$education <- as.factor(real_data$education)
real_data$default <- as.factor(real_data$default)
real_data$loan <- as.factor(real_data$loan)
real_data$age_category <- sapply(real_data$age, age_categorize)
real_data$age_category <- as.factor(real_data$age_category)
real_data$contact_within_one_year <- sapply(real_data$pdays, pdays.categorize)
real_data$contact_within_one_year <- as.factor(real_data$contact_within_one_year)

# 読み込んだデータを1/0のベクトルに変換する関数作成
target_list_conv <- function(x){
  th <- 0.2063689
  ifelse (x >= th, 1, 0)
}

target_list_gen <- function(x){
  y_pred <- predict(mymodel8, newdata = x, type = "response")
  y_flag <- sapply(y_pred, target_list_conv)
  return (y_flag)
} 

target_list_gen(real_data) -> flag_data
flag_data

###############################################################################
#### テストデータの収益計算をする ####
real_data$y <- ifelse(real_data$y == "yes", 1, 0)

#Confusion Matrixを作る
conf_mat2 <- table(real_data$y, flag_data)
conf_mat2

#正解率(accuracy) 
accuracy2 <- (conf_mat2[1] + conf_mat2[4]) / (conf_mat2[1] + conf_mat2[2] + conf_mat2[3] + conf_mat2[4])
accuracy2

#適合率(precision) 
precision2 <- conf_mat2[4] / (conf_mat2[3] + conf_mat2[4])
precision2

#再現率(recall) 
recall2 <- conf_mat2[4] / (conf_mat2[2] + conf_mat2[4])
recall2

conf_mat2[4] * 2000 - (conf_mat2[3] + conf_mat2[4]) * 500

########################################################################

