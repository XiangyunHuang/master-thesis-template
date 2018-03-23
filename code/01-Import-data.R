library(ggplot2)
# 全局主题设置

# 导入数据
library(readxl)

cumcm2011A_location <- read_xls(path = 'data/cumcm2011A附件_数据.xls',sheet = '附件1',
                                range = 'B4:E322',col_names = FALSE)
colnames(cumcm2011A_location) <- c('x','y','elevation','area')
cumcm2011A_location$area <- as.factor(cumcm2011A_location$area)
# 因子变量名称替换
area_labels = c('生活区','工业区','山区','交通区','公园绿地区')
# 地形加采样点分布图 气泡图 大小表示海拔 颜色表示区域
ggplot(cumcm2011A_location, aes(x = x, y = y, size = elevation, color = area )) +
  geom_point(shape = 19, alpha = 0.5) +
  scale_color_brewer(palette = "Set1", labels = area_labels) +
  labs(x = '横坐标', y = '纵坐标', size = '海拔\n米', color = '功能区')


cumcm2011A_data <- read_xls(path = 'data/cumcm2011A附件_数据.xls', sheet = '附件2',
                            range = 'B4:I322', col_names = FALSE)
colnames(cumcm2011A_data) <- c('As', 'Cd', 'Cr', 'Cu', 'Hg', 'Ni', 'Pb', 'Zn')
ggplot(cumcm2011A_location, aes(x = x, y = y, color = area )) +
  geom_point(aes(size = cumcm2011A_data$As ), shape = 19, alpha = 0.5 ) +
  scale_color_brewer(palette = "Set1", labels = area_labels ) +
  labs(x = '横坐标', y = '纵坐标', size = '砷浓度\n微克/克', color = '功能区')




## 各重金属浓度背景值
cumcm2011A_refer <- read_xls(path = 'data/cumcm2011A附件_数据.xls',sheet = '附件3',
                            range = 'B4:C11',col_names = FALSE)


pairs(cumcm2011A_location)
