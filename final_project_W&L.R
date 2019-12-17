library(quantmod)
library(zoo)
library(xts)
library(tidyverse)
library(readxl)
library(scales)
library(gridExtra)
library(ggplot2)

####1
getSymbols("AAPL",src="yahoo",from="2019-01-01",to="2019-12-12")
chartSeries(AAPL,theme = "white",name = "苹果",up.col = 'red',dn.col = 'green')

####2
# 读取20191105数据
dat <- read_excel(file.choose())
# 修改列名称
colnames(dat) <- 
  c("Stkcd", "Stknm", "SMA50", "Industry", "Return", "Close", "Mktvl")
# 新增列，表示当日涨跌
dat <- dat %>%
  mutate(BullBear = ifelse(Return > 0, "Advancing", 
                           ifelse(Return == 0, "Zero", "Declining")),
dat.home.1 <- dat %>%
  group_by(BullBear) %>%
  summarise(Stocks = n()) %>%
  mutate(Stocks_ratio = Stocks / 300,
         HS300 = 1,
         ys = c(0.1, 0.9, 0.5),
         yl = paste(round(Stocks_ratio * 100, 1), "%", sep = ""))

dat.home.1$yl[3] <- ""
# 设定当日涨跌变量的因子水平
dat.home.1$BullBear <- 
  factor(dat.home.1$BullBear, 
         levels = c("Declining", "Zero", "Advancing"),
         ordered = T)
# 显示数据
dat.home.1  
# 绘图
ggplot(dat.home.1, aes(x = HS300, y = Stocks_ratio, fill = BullBear)) +
  geom_bar(stat = "identity", width = 0.5) + # width设置宽度
  scale_fill_manual(values = c("#de7e7e", "grey", "#83ca83")) + # 设置颜色
  # scale_fill_manual(values = c(muted("red", c = 500), "grey", muted("green", c = 500))) + 
  annotate("text", x = 1.5, y = 0.15, # 上方文字显示
           label = dat.home.1$BullBear[1], 
           size = 6) +
  annotate("text", x = 1.5, y = 0.05, 
           label = dat.home.1$Stocks[1], 
           size = 6, 
           colour = "#00B060") +
  annotate("text", x = 1.5, y = 0.85, 
           label = dat.home.1$BullBear[2], 
           size = 6) +
  annotate("text", x = 1.5, y = 0.95, 
           label = dat.home.1$Stocks[2], 
           size = 6, 
           colour = "#FF4500") +
  geom_text(y = dat.home.1$ys, # 百分比数值显示
            label = dat.home.1$yl, 
            size = 9,
            colour = "white") +
  xlim(0, 2) +
  coord_flip() + # 坐标轴翻转
  theme(panel.background = element_blank(), # 主题设置
        axis.title =  element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        legend.position = "none")

######3
# 计算行业股票总市值
ind.mktvl <- dat %>%
  group_by(Industry) %>%
  summarise(sum.mktvl = sum(Mktvl))

# 计算绘图所需数据
dat.2 <- dat %>% 
  left_join(ind.mktvl, by = "Industry") %>%
  mutate(Mktvl_ratio = Mktvl / sum.mktvl, 
         Label = ifelse(Mktvl_ratio > 0.09, Stknm, "")) %>% 
  arrange(Industry, desc(Return)) %>%
  plyr::ddply("Industry", transform, Label_y = cumsum(Mktvl_ratio) - 0.5*Mktvl_ratio)

# 绘图
ggplot(dat.2, aes(x = Industry, y = Mktvl_ratio, fill = Return)) +
  geom_bar(stat = "identity", width = 0.75) + 
  scale_fill_gradient(low = "yellow", high = "red") + # 设置颜色
  geom_text(aes(y = Label_y, label = Label), size = 2.9, colour = "white") + # 显示股票名称
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() + 
  labs(title = "1 DAY PERFORMANCE SPECTRUM") + # 标题文本
  theme(panel.background = element_blank(),  # 主题设置
        axis.title =  element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(), 
        legend.position = "none",
        plot.title = element_text(hjust = -0.4))

###4

# 为了解决日期作为横坐标会出现休市日的情况，需要以下的变量辅助
row_len <- nrow(SHDX2017)
breaks <- seq(1, row_len, 10)
labels <- SHDX2017$date[breaks]

# 作K线图
p1 <- SHDX2017 %>%
  arrange(date) %>%
  mutate(ma5 = SMA(close, n = 5, align ="right"),
         ma10 = SMA(close, n = 10, align = "right"),
         date_axis = row_number()) %>%
  ggplot(aes(x = date_axis)) +
  geom_boxplot(aes(lower = pmin(close, open),
                   middle = close,
                   upper = pmax(close, open),
                   ymin = low,
                   ymax = high,
                   group = date_axis,
                   fill = open > close),
               stat = "identity",
               show.legend = FALSE) +
  geom_line(aes(y = ma5), color = "blue3") +
  geom_line(aes(y = ma10), color = "red") +
  scale_x_continuous(breaks = breaks,
                     labels = NULL,
                     expand = c(0, 0)) +
  theme(axis.ticks.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(margin = margin(l = 8)))

# 作成交量图
p2 <- SHDX2017 %>%
  arrange(date) %>%
  mutate(vol_ma5 = SMA(volume, n = 5, align ="right"),
         vol_ma10 = SMA(volume, n = 10, align = "right"),
         date_axis = row_number()) %>%
  ggplot(aes(x = date_axis, y = volume)) +
  geom_bar(stat = "identity",
           aes(fill = open > close),
           show.legend = FALSE) +
  geom_line(aes(y = vol_ma5), color = "blue3") +
  geom_line(aes(y = vol_ma10), color = "red") +
  scale_x_continuous(breaks = breaks,
                     labels = format(labels, "%m-%d"),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0.5)) +
  theme(axis.title = element_blank())

# 组合
grid.arrange(p1, p2, nrow = 2, heights = 2:1)

###5
data.return <- read.csv("return.csv",header = TRUE,sep = ',')

p <- ggplot(data.return, aes(PE,return))+geom_point(color = "grey")
p <- p + labs(x="PE",y="RETURN")
p <- p + facet_wrap(~location) + scale_y_discrete(name = "RETURN",breaks=c(-10,-5,0,5,10))
p <- p + ggtitle("不同地区上市公司股价涨幅和PE关系") + theme(plot.title = element_text(hjust = 0.5))
p