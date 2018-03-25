[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/XiangyunHuang/master-thesis-template?branch=master&svg=true)](https://ci.appveyor.com/project/XiangyunHuang/master-thesis-template)
[![Build Status](https://api.travis-ci.org/XiangyunHuang/master-thesis-template.svg?branch=master)](https://travis-ci.org/XiangyunHuang/master-thesis-template)

---

# 中国矿业大学（北京）硕士学位论文模板

- 这是一个用 **bookdown** 写论文模板的例子。在使用它之前，建议您至少泛读一遍 [**bookdown** 官方文档](https://bookdown.org/yihui/bookdown)。
- 论文模板源文件放在本仓库，基于 [netlify](https://www.netlify.com/) 部署，可以 [在线预览](https://cumtb-thesis-template.netlify.com/)，模板的PDF输出文件[见此](https://cumtb-thesis-template.netlify.com/master-thesis-template.pdf)
- 使用之前请仔细阅读[模板说明](https://cumtb-thesis-template.netlify.com/)

# 本地编译流程

## 先启动 Docker 

```
docker run --name rocker -d -p 8787:8787 -e ROOT=TRUE \
> -e USER=xiangyun -e PASSWORD=cloud rocker/geospatial
docker start rocker
```

## 克隆本 repo

```
git clone --depth=50 --branch=master https://github.com/XiangyunHuang/master-thesis-template.git master-thesis-template
git submodule update --init --recursive
```

## 配置 Git

```bash
git config --global user.email "邮箱"
git config --global user.name "用户名"
touch .git-credentials
echo "https://username:password@github.com" >> .git-credentials
git config --global credential.helper store
sudo apt update && sudo apt install zsh
sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
```

## 编译文档需要的 R 包

```r
install.packages(c("ggExtra","showtext"))
```

## 编译 PDF 文档需要的 TeX 包

```bash
tlmgr install ctex xecjk zhnumber fandol filehook lm-math unicode-math
```
