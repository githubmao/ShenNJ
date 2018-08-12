#-----------------------Code Description---------------------------------------#
# Notes:
# ver1.0, data: 20180811, by MaoYan
#
# Description:
# ShenNJ客车通过性分析数据分析，真实道路。
#------------------------------------------------------------------------------#


library(data.table)
library(ggplot2)


# 导入数据----
setwd(dir = "E:/R/ShenNJ/Data/realDesign")  # 设置工作目录


# drivingTraj
# df.drivingtrajR12V20VRTdown
tmp.df <- read.table(file = "drivingTraj_R12_V20_VRT-1.58.txt",
                     header = TRUE,
                     sep = "",
                     stringsAsFactors = FALSE,
                     skip = 1,
                     col.names = c("posX", "posY"))

kXDesignRowNo <- which(tmp.df$posX == "X_Design")
kXTargetRowNo <- which(tmp.df$posX == "X_Target")

tmp.df1 <- tmp.df[1:(kXDesignRowNo - 1),]
tmp.df1$TrajTyp <- "DrivingTraj"

tmp.df2 <- tmp.df[(kXTargetRowNo + 1):length(tmp.df$posX),]
tmp.df2$TrajTyp <- "TargetTraj"

df.drivingtrajR12V20VRTdown <- rbind(tmp.df1, tmp.df2)


# df.drivingtrajR12V20VRTup
tmp.df <- read.table(file = "drivingTraj_R12_V20_VRT1.58.txt",
                     header = TRUE,
                     sep = "",
                     stringsAsFactors = FALSE,
                     skip = 1,
                     col.names = c("posX", "posY"))

kXDesignRowNo <- which(tmp.df$posX == "X_Design")
kXTargetRowNo <- which(tmp.df$posX == "X_Target")

tmp.df1 <- tmp.df[1:(kXDesignRowNo - 1),]
tmp.df1$TrajTyp <- "DrivingTraj"

tmp.df2 <- tmp.df[(kXTargetRowNo + 1):length(tmp.df$posX),]
tmp.df2$TrajTyp <- "TargetTraj"

df.drivingtrajR12V20VRTup <- rbind(tmp.df1, tmp.df2)


df.drivingtrajR12V20VRTdown$posX <- as.numeric(df.drivingtrajR12V20VRTdown$posX)
df.drivingtrajR12V20VRTdown$posY <- as.numeric(df.drivingtrajR12V20VRTdown$posY)

df.drivingtrajR12V20VRTup$posX <- as.numeric(df.drivingtrajR12V20VRTup$posX)
df.drivingtrajR12V20VRTup$posY <- as.numeric(df.drivingtrajR12V20VRTup$posY)


# latForCoeff
tmp.df <- read.table("latForCoeff_R12_V20_VRT-1.58.txt",
                     header = TRUE,
                     sep = "",
                     stringsAsFactors = FALSE,
                     skip = 1,
                     col.names = c("Station", "LatFor"))

which(tmp.df$Station == "Station")

df.latforcoeffR12V20down <- rbind(data.frame(tmp.df[1:840,],
                                             LatForTyp = "LatForA1"),
                                  data.frame(tmp.df[842:1681,],
                                             LatForTyp = "LatForA2"),
                                  data.frame(tmp.df[1683:2522,],
                                             LatForTyp = "LatForAll"))

tmp.df <- read.table("latForCoeff_R12_V20_VRT1.58.txt",
                     header = TRUE,
                     sep = "",
                     stringsAsFactors = FALSE,
                     skip = 1,
                     col.names = c("Station", "LatFor"))

which(tmp.df$Station == "Station")

df.latforcoeffR12V20up <- rbind(data.frame(tmp.df[1:951,],
                                           LatForTyp = "LatForA1"),
                                data.frame(tmp.df[953:1903,],
                                           LatForTyp = "LatForA2"),
                                data.frame(tmp.df[1905:2855,],
                                           LatForTyp = "LatForAll"))

df.latforcoeffR12V20down$Station <- as.numeric(df.latforcoeffR12V20down$Station)
df.latforcoeffR12V20down$LatFor <- as.numeric(df.latforcoeffR12V20down$LatFor)
df.latforcoeffR12V20up$Station <- as.numeric(df.latforcoeffR12V20up$Station)
df.latforcoeffR12V20up$LatFor <- as.numeric(df.latforcoeffR12V20up$LatFor)

# longSpeed
df.longspeedR12V20VRTdown <- fread("longSpeed_R12_V20_VRT-1.58.txt",
                                   header = TRUE,
                                   sep = "auto",
                                   stringsAsFactors = FALSE,
                                   data.table = FALSE,
                                   skip = "Station",
                                   col.names = c("Station", "Speed"))

df.longspeedR12V20VRTup <- fread("longSpeed_R12_V20_VRT1.58.txt",
                                 header = TRUE,
                                 sep = "auto",
                                 stringsAsFactors = FALSE,
                                 data.table = FALSE,
                                 skip = "Station",
                                 col.names = c("Station", "Speed"))


# trackOff
df.trackOffR12V20VRTdown <- fread("trackOff_R12_V20_VRT-1.58.txt",
                                  header = TRUE,
                                  sep = "auto",
                                  stringsAsFactors = FALSE,
                                  data.table = FALSE,
                                  skip = "Station",
                                  col.names = c("Station", "TrackOff"))

df.trackOffR12V20VRTup <- fread("trackOff_R12_V20_VRT1.58.txt",
                                header = TRUE,
                                sep = "auto",
                                stringsAsFactors = FALSE,
                                data.table = FALSE,
                                skip = "Station",
                                col.names = c("Station", "TrackOff"))


# 1 RAD, 12m, Down, Right----
# 1.1 drivingTraj & longSpeed----
plot.drivingtrajR12V20VRTdown <- ggplot(data = df.drivingtrajR12V20VRTdown,
                                        aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(-12, 0)) +
  scale_y_continuous(limits = c(-12, 12)) +
  theme(legend.position = c(0.8, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR12V20VRTdown

plot.longspeedR12V20VRTdown <- ggplot(data = df.longspeedR12V20VRTdown,
                                      aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 38, 5)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR12V20VRTdown

grid.arrange(plot.drivingtrajR12V20VRTdown, plot.longspeedR12V20VRTdown,
             ncol=2, nrow=1)


# 1.2 trackoff----
plot.trackoffR12V20VRTdown <- ggplot(data = df.trackOffR12V20VRTdown,
                                     aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 38, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR12V20VRTdown

max(df.trackOffR12V20VRTdown$TrackOff)


# 1.3 latforcoeff----
plot.latforcoeffR12V20VRTdown <- ggplot(data = df.latforcoeffR12V20down,
                                        aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 38, 5)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR12V20VRTdown


# 2 RAD, 12m, Up, Left----
# 2.1 drivingTraj & longSpeed----
plot.drivingtrajR12V20VRTup <- ggplot(data = df.drivingtrajR12V20VRTup,
                                      aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(-15, 0)) +
  scale_y_continuous(limits = c(-15, 15)) +
  theme(legend.position = c(0.8, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR12V20VRTup

plot.longspeedR12V20VRTup <- ggplot(data = df.longspeedR12V20VRTup,
                                    aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 38, 5)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR12V20VRTup

grid.arrange(plot.drivingtrajR12V20VRTup, plot.longspeedR12V20VRTup,
             ncol=2, nrow=1)


# 2.2 trackoff----
plot.trackoffR12V20VRTup <- ggplot(data = df.trackOffR12V20VRTup,
                                   aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 38, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR12V20VRTup

max(df.trackOffR12V20VRTup$TrackOff)


# 2.3 latforcoeff----
plot.latforcoeffR12V20VRTup <- ggplot(data = df.latforcoeffR12V20up,
                                      aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 38, 5)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR12V20VRTup




