# practical_statistics_1

### PRACTICAL STATISTICS FOR DATA SCIENTISTS 
### 1장.탐색적 데이터 분석 
### 기초 통계 및 시각화 일부 


## 패키지 설치 
.libPaths("C:/myprojectr/library")
install.packages(c("matrixStats","ggplot2","hexbin")) 

library("matrixStats") #가중중간값과 가중중간값 활용하기 위한 패키지 
library(ggplot2) #ggplot
library(hexbin) #stat_binhex()
library(corrplot)
library(ascii)
library(descr) # 범주형 변수 vs 범주형 변수 crosstable 만들 때 필요 




## 위치 추정 
# state 데이터 : 인구에 따른 살인 비율의 위치 추정 

state = read.csv(file="C:/datas/state.csv")
state
summary(state)


# 평균이 절사평균보다 크고, 절사평균은 중간값보다 크다
# 절사평균 : 가장 큰 5개 주의 인구와 가장 작은 5개의 주의 인구를 제외 
# 가중 평균 및 가중 중간값 :  library("matrixStats")
state = read.csv(file="C:/datas/state.csv")
mean(state[["Population"]])
mean(state[["Population"]], trim=0.1)
median(state[["Population"]])
weighted.mean(state[["Murder.Rate"]], w=state[["Population"]]) # 가중평균

weightedMedian(state[["Murder.Rate"]], w=state[["Population"]]) # 가중중간값 
sd(state[["Population"]])
IQR(state[["Population"]])
mad(state[["Population"]])

## 백분위수와 상자그림 
quantile(state[["Murder.Rate"]], p=c(.05,.25,.5,.75,.95))
boxplot(state[["Population"]]/100000, ylab="Population (millions) ")

## 도수분포표와 히스토그램 
## 도수분포표 : 변수의 범위를 동일한 크기의 구간으로 나눈 다음, 각 구간마다 몇 개의 변수 값이 존재하는지를 보여주기 위해 사용됨 
breaks = seq(from=min(state[["Population"]]), to=max(state[["Population"]]), length=11)
breaks             
pop_freq = cut(state[["Population"]], breaks = breaks, right=TRUE, include.lowest = TRUE)
pop_freq
table(pop_freq)
hist(state[["Population"]], breaks=breaks)

#cut 함수
n=300 
x=round(runif(n,1,100))
breaks = seq(0,100,by=10)
cut(x,breaks=breaks,label=breaks[-length(breaks)],right=T)
hist(x,breaks=breaks,plot=F)

# 밀도추정
hist(state[["Murder.Rate"]], freq=FALSE)
lines(density(state[["Murder.Rate"]]), lwd=3, col="blue")
barplot(as.matrix(dfw)/6, cex.axis=.5)


## 상관행렬
sp500_px = read.csv(file="C:/datas/sp500_px.csv")
sp500_sym = read.csv(file="C:/datas/sp500_sym.csv")
sp500_sym

etfs = sp500_px[row.names(sp500_px)>"2012-07-01", sp500_sym[sp500_sym$sector=="etf", 'symbol']]
etfs
corrplot(cor(etfs), method="ellipse")

telecom = sp500_px[,sp500_sym[sp500_sym$sector=="telecommunications_services",'symbol']]
telecom

telecom = telecom[row.names(telecom) > "2012-07-01",]
telecom_cor = cor(telecom)
telecom_cor
ascii(telecom_cor, digits=c(3,3,3,3,3), align=c("1","r","r","r","r","r"),caption="Correlation between telecommunications", include.colnames = T, include.rownames = T)

## 산점도
plot(telecom$T, telecom$VZ, xlab="T", ylab="VZ")

### 두 개 이상의 변수 탐색하기
## 육각형 구간과 등고선 
kc_tax = read.csv(file="C:/datas/kc_tax.csv")
kc_tax

kc_tax0 = subset(kc_tax, TaxAssessedValue < 750000 & SqFtTotLiving>100 & SqFtTotLiving<3500)
nrow(kc_tax0)
ggplot(kc_tax0, aes(x=SqFtTotLiving, y=TaxAssessedValue)) + stat_binhex(colour="white") + theme_bw() + scale_fill_gradient(low="white",high="purple") + labs(x="Finished Sqaure Feet", y="Tax Assessed Value")

## 등고선 
ggplot(kc_tax0, aes(SqFtTotLiving,TaxAssessedValue)) + theme_bw() + geom_point(alpha=0.1) + geom_density2d(colour= "white") + labs(x="Finished Square Feet", y="Tax Assessed Value")

ggplot(kc_tax0, aes(SqFtTotLiving, TaxAssessedValue)) +
  theme_bw() + 
  geom_point(alpha=0.1) + 
  geom_density2d(colour="red") + 
  labs(x="Finished Square Feet", y="Tax Assessed Value")


## 범주형 변수 vs 범주형 변수 : library(descr)
lc_loans = read.csv("C:/datas/lc_loans.csv")
x_tab = CrossTable(lc_loans$grade, lc_loans$status, prop.c = FALSE, prop.chisq = FALSE, prop.t = FALSE)
x_tab

## 범주형 변수 vs 수치형변수
airline_stats = read.csv("C:/datas/airline_stats.csv")
boxplot(pct_carrier_delay ~ airline, data=airline_stats, ylim=c(0,50))

## 바이올린 도표 
ggplot(data=airline_stats, aes(airline, pct_carrier_delay)) + ylim(0,50) + geom_violin() + labs(x="", y="Daily % of Delayed Flights")


## 다변수 시각화
ggplot(subset(kc_tax0, ZipCode %in% c(98188, 98105, 98108, 98126)),
       aes(x=SqFtTotLiving, y=TaxAssessedValue)) + 
  stat_binhex(colour="white") + 
  theme_bw() + 
  scale_fill_gradient( low="gray95", high="blue") +
  labs(x="Finished Square Feet", y="Tax Assessed Value") +
  facet_wrap("ZipCode")



## 정리
# 육각형 구간이나 등고선 도표는 데이터의 방대한 양에 압도당하지 않으면서, 한 번에 두 수치형 변수를 시각적으로 거모하기 위해 유용한 도구
# 분할표는 두 범주형 변수의 도수를 화인하기 위한 표준방법
# 상자그림과 바이올린 도표는 범주형 변수와 수치형 변수 간의 관계를 도식화하기 위한 도구 


