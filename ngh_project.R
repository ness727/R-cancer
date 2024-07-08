cancer <- read.csv("lung_cancer.csv", header = TRUE)
head(cancer)

cancer.num <- cancer[3:26]
head(cancer.num)

# 스피어만 상관 분석
install.packages("psych")
library(psych)
cancer.cor <- corr.test(cancer.num,use = 'complete',
          method = 'spearman', adjust = 'none')
cancer.cor$r[, "Level_Num"]

# 상관 분석 결과 보기
# 그래프 크기 조절
par(mar = c(2, 2, 2, 2))
install.packages("corrplot")
library(corrplot)
corrplot(cancer.cor$r, method="circle", type="lower")

# 다중 선형 회귀 분석
cancer.lm <- lm(Level_Num ~ Age + Gender + Air.Pollution + Alcohol.use + Dust.Allergy 
         + OccuPational.Hazards + Genetic.Risk + chronic.Lung.Disease + Balanced.Diet + Obesity 
         + Smoking + Passive.Smoker +  Chest.Pain + Coughing.of.Blood  
         + Fatigue + Weight.Loss + Shortness.of.Breath + Wheezing + Swallowing.Difficulty 
         + Clubbing.of.Finger.Nails + Frequent.Cold + Dry.Cough + Snoring, data = cancer.num)
cancer.lm
summary(cancer.lm)


# 통계적으로 무의미하다고 판단되는 독립 변수들을 자동으로 제외함
# AIC(Akaike Information Criterion)를 기준으로 최적의 모델을 찾음
# AIC는 낮을수록 더 적절한 적합도를 제공하고, 불필요한 데이터가 없다는 것을 의미
cancer.lm.step <- step(cancer.lm)
summary(cancer.lm.step)
# step()을 호출한 이후 AIC 값이 -147.73 -> -152.57 으로 감소한 것을 확인




# 로지스틱 회귀 모델
cancer.glm <- glm(Level_Num ~ Age + Gender + Air.Pollution + Alcohol.use + Dust.Allergy 
                + OccuPational.Hazards + Genetic.Risk + chronic.Lung.Disease + Balanced.Diet + Obesity 
                + Smoking + Passive.Smoker +  Chest.Pain + Coughing.of.Blood  
                + Fatigue + Weight.Loss + Shortness.of.Breath + Wheezing + Swallowing.Difficulty 
                + Clubbing.of.Finger.Nails + Frequent.Cold + Dry.Cough + Snoring, data = cancer.num)
summary(cancer.glm)

# 통계적으로 무의미하다고 판단되는 독립 변수들을 자동으로 제외함
# AIC(Akaike Information Criterion)를 기준으로 최적의 모델을 찾음
cancer.glm.step <- step(cancer.glm)
summary(cancer.glm.step)

# 임의의 데이터 생성
new.data <- data.frame(rbind(c(40, 1, 7, 2,
                               2, 3, 3,
                               7, 8, 6, 2,
                               2, 7, 2,
                               3, 2,
                               2, 5, 6)))
names(new.data) <- c('Age', 'Gender', 'Air.Pollution', 'Alcohol.use', 
                     'OccuPational.Hazards', 'Genetic.Risk', 'Balanced.Diet', 
                     'Obesity', 'Smoking', 'Passive.Smoker', 'Chest.Pain', 
                     'Coughing.of.Blood', 'Fatigue', 'Weight.Loss',  
                     'Shortness.of.Breath','Swallowing.Difficulty', 
                     'Clubbing.of.Finger.Nails','Dry.Cough', 'Snoring')

# 폐암 위험도 예측
result <- predict(cancer.glm.step, new.data)
result
round(result)



# 다른 분석 코드입니다.

# 군집분석
# 표준화
# cancer.num.scale <- scale(cancer.num)
# summary(cancer.num.scale)

# cancer.kmeans <- kmeans(cancer.num.scale, centers = 3)
# cancer.kmeans$centers
# 
# library(cluster)
# clusplot(cancer.num, cancer.kmeans$cluster, color=TRUE, shade=TRUE, labels=1, lines=0)


# 계층적 군집화
# 덴드로그램
# cancer.hc = hclust(dist(mtcars))
# cancer.dend <- as.dendrogram(cancer.hc)
# plot(cancer.dend, type = "triangle")

# 색 입힌 버전
# 3개의 군집으로 구분
# library(cluster)
# install.packages("factoextra")
# library(factoextra)
# 
# cancer.hk <- hkmeans(cancer.num, 3)
# fviz_dend(cancer.hk, cex = 0.6, palette = "jco", 
#           rect = TRUE, rect_border = "jco", rect_fill = TRUE)


# 상관분석
# cancer.cor <- cor(cancer.num)
# cancer.cor
# summary(cancer.cor)

# install.packages("corrplot")
# library(corrplot)
# corrplot(cancer.cor, method="circle", type="lower", order="hclust")
