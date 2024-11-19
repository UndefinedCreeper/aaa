##----------------------------Cr铬污染全球分布图-------------------------

##生物聚集因子（BCF）
#BCF = 植物可食用部分的重金属含量 / 相应土壤中的重金属含量。
#BCF > 1 表示生菜对Cr具有富集作用



##1.首先评估生菜中的Cr含量是否达到最低摄入标准
#DI=CxIRxEF xED/BWxAT
#  =13.5mg/kg * 0.268kg/天 *365天/年 * 70年  /  73kg *25550天
#  =0.0495616 mg/kg.天  （远高于0.005）
# DI 是通过摄入粮食作物每天摄入的重金属，单位为 mg/kg/天。
# C  是粮食作物可食用部分的重金属浓度 （mg/kg） 
# IR 是粮食作物的摄入率（kg/天）。
# EF 是暴露频率（即 365 天/年），
# ED 是暴露持续时间，即 70 年
# BW 是平均体重，即 73 kg ，
# AT 是平均暴露时间段，即 25,550 天（70年）.


##2.计算生菜中Cr的转换效率
#计算生菜的均值MCr=Cr1+Cr2+...+Crn/n
# Cr转换效率=Cr(AMF)-Cr(NAMF)/Cr(NAMF) *100%
# Cr转换效率=13.5-11/11 *100% = 0.227272

##3.将转换效率用于风险评估
#Cr 对人类健康构成的癌症风险 （CR） 评估如下：
# CR=DI*SF  (SF 是口服致癌剂量, Cr 的斜率因子 （0.5 mg/kg/天）)
# CR = 0.0495616 mg/kg.天  * 0.5 mg/kg.天 = 0.0247808


DI <- (13.5 * 0.268 * 365 * 70) / (73 * 25550)  # C * IR * EF * ED / (BW * AT)
cat("DI =", DI, "mg/kg/day")

CR <- DI*0.5
cat("CR =", CR, "mg/kg/day")

##将风险评估系数转化为全球生菜消费数据（暂时用生产数据进行代替）
data <- read.csv("lettuce data.csv")
data$Lettuce.tonnes.per.hectare <- data$Lettuce.tonnes.per.hectare * CR
data <- data[-109, ] #109行为Puerto rico 数值为3.01于其他国家相差太大，


##绘制世界地图

library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggspatial)


# 获取世界国家边界的简单要素数据 (sf object)
world <- ne_countries(scale = "medium", returnclass = "sf")

# 绘制带国家边界的世界地图
ggplot(data = world) +
  geom_sf() +
  theme_minimal() +
  labs(title = "World Map with Country Borders")


head(data)
# 合并数据框，使用国家名称进行匹配
world_data <- merge(world, data, by.x = "name", by.y = "Entity", all.x = TRUE)

# 绘制世界地图，根据 'value' 列填充颜色
ggplot(data = world_data) +
  geom_sf(aes(fill = Lettuce.tonnes.per.hectare)) +
  scale_fill_viridis_c(option = "plasma", na.value = "lightgray" ,limits=c(0,1.5)) + # 使用 viridis 颜色梯度
  #scale_fill_gradient2(low="gray70", high ="blue",limits=c(1,3.1))+
  theme_minimal() +
  labs(title = "World Map with Custom Data", fill = "Cr Cancer risk")
  




##-------------------------绘制土壤中Cr暴露风险全球分布地图------------------------------------

##1、计算土壤中重金属的污染系数（CF） 
#Cf=Cn/Bv (Cf土壤暴露风险，Cn土壤中Cr的含量，Bv土壤中Cr的平均含量背景值)
# Cf=Cn/Bv = 

##2、计算皮肤接触土壤的每日摄入量 (CDI) 
# CDIder = Csoil  * ( SA xAF xABS xEF xED ) / ( BW x AT)  * 10^-6
#        
# Csoil 是土壤中微量元素的含量，即：土壤中Cr含量mg/kg
# SA 是接触污染物的皮肤面积，5700 cm2，
# AF 是土壤的附着系数，0.07 mg/cm2/天
# ABS是皮肤对重金属的吸附系数（无单位）0.001 ，
# EF 是接触重金属的频率，365 天/年，
# ED 是接触每种重金属的持续时间，24 年，
# BW 是体重，70 kg，
# AT 是接触重金属的平均时间，8760 day。
# CSFderm 基于 Cr 的值为 2.00E + 01 mg/kg/天

##3、计算土壤中Cr的皮肤接触暴露致癌风险
# CRderm = CDIder * CSFderm 



# CDIder = Csoil * 10_6 * SA xAF xABS xEF xED / BWxAT
CDIder <- (115) * (5700 * 0.07 * 0.001 * 365 * 24) / (70 * 8760)  *10^-6
cat("CDIder =", CDIder, "mg/kg/day")


