# -----------------------------------------------------------
# Monte Carlo Simulation and Resampling Methods for Social Science
# Adopted from: Carsey, T. M. & Harden, J. J.
# Source: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
# -----------------------------------------------------------

# 패키지 준비: 없으면 설치 후 로드 (mvtnorm은 다변량 정규분포 샘플링에 사용)
if (!require(mvtnorm)) {
  install.packages("mvtnorm")
  library(mvtnorm)
}

# 책의 두 가지 예제
# (a.) 생략변수 편의(omitted variable bias)
# (b.) 확률적 설명변수를 포함한 회귀(무관계 변수 다수 포함 시의 문제)

# -----------------------------------------------------------
# 용어 정리
# -----------------------------------------------------------

# 몬테카를로 시뮬레이션과 DGP(자료 생성 과정)
# - 가정한 DGP로부터 여러 번 표본을 반복 추출해(재현 가능한 가상의 세계)
#   추정량의 성질(편향, 분산 등)과 방법의 성능(예: 평균제곱오차)을 평가
# - 현실에서는 진짜 DGP를 알 수 없으므로, 관측표본이 만들어지는
#   과정에 대한 가정 하에서 모의실험을 수행

# -----------------------------------------------------------
# (a) Omitted Variable (Chapter 5.3.4)
# -----------------------------------------------------------

set.seed(37943)   # 재현 가능성을 위해 시드 고정

reps <- 1000      # 반복 횟수(시뮬레이션 회수)
b0 <- .2          # 진짜 절편 (true intercept)
b1 <- .5          # X1의 진짜 기울기
b2 <- .75         # X2의 진짜 기울기
n  <- 1000        # 각 반복에서의 표본 크기

# X1과 X2(설명변수) 사이의 상관수준들
cor.level <- c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, .99)

# 각 상관수준 j, 반복 i에서 추정된 b1(=X1 계수)을 저장할 행렬
par.est.ov <- matrix(NA, nrow = reps, ncol = length(cor.level))

# 몬테카를로 반복 루프
for (j in 1:length(cor.level)) {      # 상관수준(r)을 바꿔가며
  for (i in 1:reps) {                 # 각 r에서 reps번 반복
    
    # X1, X2의 공분산(=상관) 구조 설정 (분산=1, 공분산=r)
    X.corr <- matrix(c(1, cor.level[j], cor.level[j], 1), nrow = 2)
    X <- rmvnorm(n, mean = c(0, 0), sigma = X.corr)   # 다변량 정규에서 샘플 추출
    X1 <- X[, 1]  # 첫 번째 열을 X1로
    X2 <- X[, 2]  # 두 번째 열을 X2로
    
    # 진짜 DGP: Y = b0 + b1*X1 + b2*X2 + N(0,1) 오차
    Y <- b0 + b1 * X1 + b2 * X2 + rnorm(n, 0, 1)
    
    # 고의로 X2를 누락한 단순회귀(=생략변수 편의 유도)
    model <- lm(Y ~ X1)
    
    # 이때 추정된 X1 계수(편향 가능)를 저장
    par.est.ov[i, j] <- model$coef[2]
  }
}

# 각 반복에서의 추정치 행렬 확인 (열: 상관수준별, 행: 반복별)
par.est.ov

# -----------------------------------------------------------
# 추정치 요약: 상관수준별 평균 추정치 확인
# -----------------------------------------------------------
# 진짜 b1 = 0.5 이지만, X2를 누락했으므로 r≠0일 때 평균이 0.5에서 벗어나는지 확인
mean(par.est.ov[, 1])  # r = 0  (X1-X2 독립이면 편의가 0에 가까워야 함)
mean(par.est.ov[, 2])  # r = 0.1
mean(par.est.ov[, 3])  # r = 0.2
mean(par.est.ov[, 4])  # r = 0.3
mean(par.est.ov[, 5])  # r = 0.4
mean(par.est.ov[, 6])  # r = 0.5
mean(par.est.ov[, 7])  # r = 0.6
mean(par.est.ov[, 8])  # r = 0.7
mean(par.est.ov[, 9])  # r = 0.8
mean(par.est.ov[, 10]) # r = 0.9
mean(par.est.ov[, 11]) # r = 0.99 (거의 완전상관, 편의가 극대화)

# -----------------------------------------------------------
# 시뮬레이션 결과 시각화
# -----------------------------------------------------------
# 밀도곡선으로 r에 따른 b1 추정치 분포 비교
plot(density(par.est.ov[, 1]), xlim = c(0, 1.5), ylim = c(0, 12))  # r=0
lines(density(par.est.ov[, 3]), col = "gray")                      # r=0.2
lines(density(par.est.ov[, 6]), col = "orange")                    # r=0.5
lines(density(par.est.ov[, 11]), col = "red")                      # r=0.99
abline(v = b1, col = "black")                                      # 진짜 b1=0.5 표시
legend(0, 12,
       legend = c("r=0", "r=0.2", "r=0.5", "r=0.99"),
       col = c("black", "gray", "orange", "red"),
       pch = 1)

# ggplot 버전 (겹쳐 그리기 + 범례/미관)
library(ggplot2)
# ggplot 입력용 데이터프레임 구성: r별 추정치 벡터를 하나로 묶기
df_plot <- data.frame(
  b1_est = c(par.est.ov[, 1], par.est.ov[, 3], par.est.ov[, 6], par.est.ov[, 11]),
  r = factor(rep(c("r = 0", "r = 0.2", "r = 0.5", "r = 0.99"), 
                 each = nrow(par.est.ov)))
)

# ggplot 밀도곡선과 기준선(진짜 b1) 표시
ggplot(df_plot, aes(x = b1_est, fill = r, color = r)) +
  geom_density(alpha = 0.3, size = 1) +
  geom_vline(xintercept = b1, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("black", "gray60", "orange", "red")) +
  scale_color_manual(values = c("black", "gray60", "orange", "red")) +
  labs(title = "Effect of Omitted Variable Bias under Different Correlations",
       x = expression(hat(beta)[1] ~ "Estimate"),  # \hat{β}_1 표기
       y = "Density",
       fill = "Correlation (r)",
       color = "Correlation (r)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

# -----------------------------------------------------------
# 교차검증(Cross-validation, Chapter 9.3)
# -----------------------------------------------------------

# CV의 취지:
# - 모델이 ‘새로운 관측치’를 얼마나 잘 예측하는가로 적합도 평가
# - 하나의 데이터셋을 학습/검증 세트로 나누어 과적합을 방지
# - 표본의 ‘신호’(체계적 관계)와 ‘잡음’(우연)을 구분하는 데 도움

set.seed(843749)  # 재현성 확보

# 변수 간 상관 0인 20×20 공분산행렬(단위분산) 생성
rand.vcv <- matrix(0, nrow = 20, ncol = 20)
diag(rand.vcv) <- 1

# 상관이 전혀 없는 20개 변수에서 1000개 관측치 생성 (첫 열을 y로 사용)
rand.data <- as.data.frame(
  rmvnorm(1000, mean = rep(0, times = 20), sigma = rand.vcv)
)

# 열 이름 부여: y(종속변수) + 19개의 설명변수
colnames(rand.data) <- c(
  "y", "x1", "x2", "x3", "x4", "x5", "x6", "x7",
  "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",
  "x16", "x17", "x18", "x19"
)

# 전부 무관계(r=0)임에도 불구하고, 유의한 변수가 나올 수 있음을 시연
# (유의수준 5%에서 거짓양성은 기대상 약 5% 발생)
rand.model <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 +
                   x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19,
                 data = rand.data)
summary(rand.model)
# 해설:
# - 진짜로는 y와 모든 x가 무관계지만, 우연히 p<0.05인 계수가 나올 수 있음
# - 이는 표본 노이즈가 ‘신호처럼’ 보이는 전형적 사례 → 과적합 위험
# - 교차검증은 이런 우연 신호를 걸러내는 한 방법(검증셋 성능으로 확인)

# 실제 CV 구현(홀드아웃, K-겹, 반복 CV 등)은 다음 학습에서 다룸
# - 개략 절차: 데이터를 train/test로 분할 → train에서 적합 → test에서 예측오차 평가
# - 유의미한 계수라도 검증셋 성능이 낮다면, 신호가 아닌 잡음일 가능성