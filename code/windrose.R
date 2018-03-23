# windrose 风玫瑰图
# http://blog.csdn.net/Bone_ACE/article/details/47624987

# 首先加载ggplot2扩展包
library(ggplot2)
set.seed(2018)
# 随机生成100次风向，并汇集到16个区间内
direction <- cut_interval(runif(100, 0, 360), n = 16)
# 随机生成100次风速，并划分成4种强度
mag <- cut_interval(rgamma(100, 15), 4) 
dat <- data.frame(direction = direction, mag = mag)
# 将风向映射到X轴，频数映射到Y轴，风速大小映射到填充色，生成条形图后再转为极坐标形式即可
p <- ggplot(dat, aes(x = direction, y = ..count.., fill = mag))
p + geom_bar(colour = "white") + coord_polar() + 
  theme_minimal() +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  labs(x = "", y = "",fill = "Magnitude")
p + geom_bar(position = "stack") + coord_polar() + theme_minimal() +
  theme(axis.ticks = element_line(colour = "white"))

library(ggplot2)
ggplot(data = diamonds, mapping = aes(x = cut, fill = cut)) + 
  geom_bar(width = 1) + coord_polar(theta = "x") + theme_minimal() +
  theme(legend.position = "none",axis.ticks = element_blank(), axis.text.y = element_blank()) +
  labs(x = "", y = "")