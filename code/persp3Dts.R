# code from https://stackoverflow.com/questions/6432996/making-a-3d-surface-from-time-series-data-in-r

set.seed(3)
dat <- data.frame(Dates = rep(seq(Sys.Date(), Sys.Date() + 9, by = 1), 
                              each = 24),
                  Times = rep(0:23, times = 10),
                  Value = rep(c(0:12,11:1), times = 10) + rnorm(240))


new.dates <- with(dat, sort(unique(Dates)))
new.times <- with(dat, sort(unique(Times)))
new.values <- with(dat, matrix(Value, nrow = 10, ncol = 24, byrow = TRUE))

persp(new.dates, new.times, new.values, ticktype = "detailed", r = 10, 
      theta = 35, scale = FALSE)

# ideas from http://www.quantmod.com/examples/chartSeries3d/


library(git2r)
# get my github active data
## Open an existing repository
repo <- repository('~/Developers')

config(repo, user.name="Xiangyun Huang", user.email="xiangyunfaith@outlook.com")



# 日历热图
# devtools::install_github("rCarto/yach")
# 基于 base R
library(yach)
data(wiki)
calendarHeat(dates = wiki$date, values = wiki$views,colors = terrain.colors(5),
             title = "Number of views of the North Korea page on 'en.wikipedia.org' in 2017")

# get my figure
# https://github.com/users/XiangyunHuang/contributions
# http://patientslikeme.github.io/react-calendar-heatmap/

# 抓到原始网页

# curl -s "https://github.com/users/XiangyunHuang/contributions"

# 清洗出数据
# 两个字段 svg 
data-date="2017-03-25"
data-count="0"














