
# 下载代码到本地
# curl -O https://gist.githubusercontent.com/crsh/c906e93c260488e7363ea606243057c2/raw/f6f0fce38bc274446d205854ea35cfa03b9b1f7a/plot_dependencies.R

# 安装依赖
Pkgs <- c("citr", "miniCRAN", "ggnetwork", "intergraph")
install.packages(Pkgs)
source("https://gist.githubusercontent.com/crsh/c906e93c260488e7363ea606243057c2/raw/f6f0fce38bc274446d205854ea35cfa03b9b1f7a/plot_dependencies.R")

# 克隆 repo 
# git clone https://github.com/tidyverse/ggplot2.git

# 转至仓库根目录
setwd("~/Desktop/report4thesis/packages/ggplot2")

# 绘图保存
pdf(file = 'ggdepnet.pdf',width = 8,height = 6)
plot_dependencies()
dev.off()

# 代码来自 https://github.com/crsh/citr