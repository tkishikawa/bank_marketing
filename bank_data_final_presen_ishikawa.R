# ���C�u�����̓ǂݍ���
library(ggplot2)
library(car,lib.loc="C:\\Users\\takao\\OneDrive\\R_lib")

# �f�[�^�̓ǂݍ���
bank_data <- read.csv("homework_data/bank_marketing_train.csv")

# ���W�X�e�B�b�N��A�̉�����
# �ړI�ϐ���0/1�ɂ���
bank_data$y <- ifelse(bank_data$y == "yes", 1, 0)

# pdays���J�e�S���ɕϊ� ##0 - 7, 8~998, 999��3�J��e�S���ɕ�����
#step1) �֐������
pdays.categorize <- function(x){
  if (x == 999){ 
    return("no")
  } else {
    return("yes")
  } 
} 
#step2) sapply�֐��Ń��X�g�����Afield��ǉ�����
bank_data$contact_within_one_year <- sapply(bank_data$pdays, pdays.categorize)
bank_data$contact_within_one_year <- as.factor(bank_data$contact_within_one_year)

# age_group�ɕ�����
## age_group��臒l�����߂�
student_data <- bank_data[bank_data$job == "student", ]
retired_data <- bank_data[bank_data$job == "retired", ]

## �w�����܂ގ�҃O���[�v�̏���l�����߂�
summary(student_data$age)
summary(student_data[student_data$y == 1, ]["age"]) 
## ��L���� student&y�̐l�����̑�O�l���ʂ�28�΂Ȃ̂ŁA�����age group1�̏���Ƃ���

## Retire�����l�������܂ރO���[�v�̉����l�����߂�
summary(retired_data$age)
summary(retired_data[retired_data$y == 1, ]["age"])
## ��L����Aretired$y�̐l�����̑��l���ʂ�57�΂Ȃ̂ŁA�����age group3�̉����Ƃ���
## ��L�̃O���[�v�̑����Ȃ��Q�����āA3�O���[�v���

## �֐������
age_categorize <- function(x){
  if (x <= 28){ 
    return("young_group")
  } else if (x >= 57) {
    return("old_group")
  } else {
    return("mid-age_group")
  } 
} 

## age_category��Field���쐬
bank_data$age_category <- sapply(bank_data$age, age_categorize)
bank_data$age_category <- as.factor(bank_data$age_category)
summary(bank_data)

# �����n�̗���_�~�[�ϐ��ɂ���
# �J�e�S���ϐ�
bank_data$job <- as.factor(bank_data$job)
bank_data$marital <- as.factor(bank_data$marital)
bank_data$contact <- as.factor(bank_data$contact)
bank_data$month <- as.factor(bank_data$month)
bank_data$day_of_week <- as.factor(bank_data$day_of_week)
bank_data$poutcome <- as.factor(bank_data$poutcome)
bank_data$loan <- as.factor(bank_data$loan)
bank_data$default <- as.factor(bank_data$default)
bank_data$housing <- as.factor(bank_data$housing)

# Train data��Test data�ւ̕���
set.seed(1234)
train_idx <- sample(1:dim(bank_data)[1], size = dim(bank_data)[1] * 0.7)
train <- bank_data[train_idx, ]
test <- bank_data[-train_idx, ]
head(bank_data)

#���W�X�e�B�b�N��A�����s
# mymodel <- glm(y~.-duration-age-pdays,data = train, family = "binomial")
# exp(mymodel$coefficients)
# step(mymodel)

#�X�e�b�v�֐��ōœK�����ꂽ���f�����쐬
# mymodel2step <- glm(formula = y ~ job + marital + education + default + housing + 
#                       loan + contact + month + day_of_week + campaign + previous + 
#                       poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
#                       euribor3m + contact_within_one_year + age_category, family = "binomial", 
#                     data = train)

#��A�W���̉�ǂ̂��߂ɁA�I�b�Y��ɕϊ�
# exp(mymodel2step$coefficients)
# sprintf("%f", exp(mymodel2step$coefficients))

#�c���𐳋K���z�ɋ߂Â���ׂ��Acar���g���āAVIF���m�F����
# vif(mymodel2step)

# Loan��housing�����S�ȃ}���`�R�ɂȂ��Ă���̂ŁAhousing���폜
# mymodel2.1step <- glm(formula = y ~ job + marital + education + default + loan + 
#                         contact + month + day_of_week + campaign + previous + 
#                         poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
#                         euribor3m + contact_within_one_year + age_category, family = "binomial", 
#                       data = train)

#��A�W���̉�ǂ̂��߂ɁA�I�b�Y��ɕϊ�
# exp(mymodel2.1step$coefficients)
# sprintf("%f", exp(mymodel2.1step$coefficients))

#�c���𐳋K���z�ɋ߂Â���ׂ��Acar���g���āAVIF���m�F����
# vif(mymodel2.1step)

#Month�����O
# mymodel3 <- glm(formula = y ~ job + marital + education + default + loan + 
#                   contact + day_of_week + campaign + previous + 
#                   poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + 
#                   euribor3m + contact_within_one_year + age_category, family = "binomial", 
#                 data = train)
# vif(mymodel3)

#Emp_variance�����OF
# mymodel4 <- glm(formula = y ~ job + marital + education + default + loan + 
#                   contact + day_of_week + campaign + previous + 
#                   poutcome + cons.price.idx + cons.conf.idx + 
#                   euribor3m + contact_within_one_year + age_category, family = "binomial", 
#                 data = train)
# vif(mymodel4)

#preivous�����O
# mymodel5 <- glm(formula = y ~ job + marital + education + default + loan + 
#                   contact + day_of_week + campaign + 
#                   poutcome + cons.price.idx + cons.conf.idx + 
#                   euribor3m + contact_within_one_year + age_category, family = "binomial", 
#                 data = train)
# vif(mymodel5)

#poutcome�����O
# mymodel6 <- glm(formula = y ~ job + marital + education + default + loan + 
#                   contact + day_of_week + campaign + 
#                   cons.price.idx + cons.conf.idx + 
#                   euribor3m + contact_within_one_year + age_category, family = "binomial", 
#                 data = train)
# vif(mymodel6)

#Euribr3m�����O
# mymodel7 <- glm(formula = y ~ job + marital + education + default + loan + 
#                   contact + day_of_week + campaign + 
#                   cons.price.idx + cons.conf.idx + 
#                   contact_within_one_year + age_category, family = "binomial", 
#                 data = train)
# vif(mymodel7)

#Day_of_week�͎g���Â炢�̂ŁA���O �����̃��f�����O���̑��� �iAIC=13170)
mymodel8 <- glm(formula = y ~ job + marital + education + default + loan + 
                  contact + campaign + 
                  cons.price.idx + cons.conf.idx + 
                  contact_within_one_year + age_category, family = "binomial", 
                data = train)
exp(mymodel8$coefficients)
vif(mymodel8)
summary(mymodel8)

#Test�f�[�^���g���Ċm����\������
y_pred <- predict(mymodel8, newdata = test, type = "response")
# y_pred

#臒l�����߂āA�t���O�����
# hist(y_pred)
ypred_flag<-ifelse(y_pred>0.2063689, 1, 0)

#Confusion Matrix�����
conf_mat <- table(test$y, ypred_flag)
conf_mat

#����(accuracy) ��92.32983%@th=0.2
accuracy <- (conf_mat[1] + conf_mat[4]) / (conf_mat[1] + conf_mat[2] + conf_mat[3] + conf_mat[4])
accuracy

#�K����(precision) ��36.05948%@th=0.2
precision <- conf_mat[4] / (conf_mat[3] + conf_mat[4])
precision

#�Č���(recall) ��12.46787%@th=0.2
recall <- conf_mat[4] / (conf_mat[2] + conf_mat[4])
recall

#�Ō��ROI�̍ő剻������
## 1) ROI���v�Z����֐������
ROI_calc <- function(call_threshold){
  f_ypred_flag <- ifelse(y_pred>call_threshold, 1, 0)
  f_conf_mat <- table(test$y, f_ypred_flag)
  ROI <- f_conf_mat[4] * 2000 - (f_conf_mat[3] + f_conf_mat[4]) * 500
  return(ROI)
}

## 2) Optimize�֐����g���AROI���ő剻���� �� ��0.2063689, ��63000.�Ȃ���
optimise(ROI_calc, interval = c(0, 0.5), maximum = TRUE)

## ���ꂩ���邱�ƃ��X�g�F
# �A�^�b�N���X�g�𐶐�����֐��A
## 1) �V�����f�[�^�̃f�[�^���H
## 2) ��������炩����臒l�̓��������f���ɓ���āAprediction������
## 3) Prediction����������(1/0�̃x�N�g��)��Ԃ�

# exp(mymodel8$coefficients)
# mymodel8 <- glm(formula = y ~ job + marital + education + default + loan + 
#                   contact + campaign + 
#                   cons.price.idx + cons.conf.idx + 
#                   contact_within_one_year + age_category, family = "binomial", 
#                 data = train)

#####################################################################################
#### �f�[�^�̗\���̂��߂̃e���v���[�g ### 
# �f�[�^�ǂݍ���
real_data <- read.csv("homework_data/bank_marketing_test.csv")   #�����ɐV�����f�[�^������I�I�I

## as.factor�ւ̒u��������Field�쐬
real_data$job <- as.factor(real_data$job)
real_data$marital <- as.factor(real_data$marital)
real_data$education <- as.factor(real_data$education)
real_data$default <- as.factor(real_data$default)
real_data$loan <- as.factor(real_data$loan)
real_data$age_category <- sapply(real_data$age, age_categorize)
real_data$age_category <- as.factor(real_data$age_category)
real_data$contact_within_one_year <- sapply(real_data$pdays, pdays.categorize)
real_data$contact_within_one_year <- as.factor(real_data$contact_within_one_year)

# �ǂݍ��񂾃f�[�^��1/0�̃x�N�g���ɕϊ�����֐��쐬
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
#### �e�X�g�f�[�^�̎��v�v�Z������ ####
real_data$y <- ifelse(real_data$y == "yes", 1, 0)

#Confusion Matrix�����
conf_mat2 <- table(real_data$y, flag_data)
conf_mat2

#����(accuracy) 
accuracy2 <- (conf_mat2[1] + conf_mat2[4]) / (conf_mat2[1] + conf_mat2[2] + conf_mat2[3] + conf_mat2[4])
accuracy2

#�K����(precision) 
precision2 <- conf_mat2[4] / (conf_mat2[3] + conf_mat2[4])
precision2

#�Č���(recall) 
recall2 <- conf_mat2[4] / (conf_mat2[2] + conf_mat2[4])
recall2

conf_mat2[4] * 2000 - (conf_mat2[3] + conf_mat2[4]) * 500

########################################################################
