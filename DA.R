#-----------------------Code Description---------------------------------------#
# Notes:
# ver1.0, data: 20180810, by MaoYan
#
# Description:
# ShenNJ客车通过性分析数据分析。
#------------------------------------------------------------------------------#

# 调用DataInput.R----
source(file = "E:/R/ShenNJ/DataInput.R", encoding = "utf-8")

df.drivingtraj$posX <- as.numeric(df.drivingtraj$posX)
df.drivingtraj$posY <- as.numeric(df.drivingtraj$posY)

df.longspeed$Station <- as.numeric(df.longspeed$Station)
df.longspeed$Speed <- as.numeric(df.longspeed$Speed)

df.latforcoeff$Station <- as.numeric(df.latforcoeff$Station)
df.latforcoeff$LatFor <- as.numeric(df.latforcoeff$LatFor)

df.longspeed$Station <- as.numeric(df.longspeed$Station)
df.longspeed$Speed <- as.numeric(df.longspeed$Speed)


# 1 RAD, 12m----
# 1.1 RAD, 12m, V15----
# drivingTraj & longSpeed----
df.drivingtrajR12V15 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R12" &
                                 df.drivingtraj$drivingSpeed == "V15")

plot.drivingtrajR12V15 <- ggplot(data = df.drivingtrajR12V15,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(-12, 0)) +
  scale_y_continuous(limits = c(-12, 12)) +
  theme(legend.position = c(0.8, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR12V15


df.longspeedR12V15 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R12" &
                               df.longspeed$drivingSpeed == "V15")

plot.longspeedR12V15 <- ggplot(data = df.longspeedR12V15,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 38, 5)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR12V15

grid.arrange(plot.drivingtrajR12V15, plot.longspeedR12V15, ncol=2, nrow=1)


# trackoff----
df.trackoffR12V15 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R12" &
                              df.trackoff$drivingSpeed == "V15")

plot.trackoffR12V15 <- ggplot(data = df.trackoffR12V15,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 38, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR12V15


# latforcoeff----
df.latforcoeffR12V15 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R12" &
                               df.latforcoeff$drivingSpeed == "V15")

plot.latforcoeffR12V15 <- ggplot(data = df.latforcoeffR12V15,
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

plot.latforcoeffR12V15


# 1.2 RAD, 12m, V20----
# drivingTraj & longSpeed----
df.drivingtrajR12V20 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R12" &
                                 df.drivingtraj$drivingSpeed == "V20")

plot.drivingtrajR12V20 <- ggplot(data = df.drivingtrajR12V20,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(-12, 0)) +
  scale_y_continuous(limits = c(-12, 12)) +
  theme(legend.position = c(0.8, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR12V20


df.longspeedR12V20 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R12" &
                               df.longspeed$drivingSpeed == "V20")

plot.longspeedR12V20 <- ggplot(data = df.longspeedR12V20,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 38, 5)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR12V20

grid.arrange(plot.drivingtrajR12V20, plot.longspeedR12V20, ncol=2, nrow=1)


# trackoff----
df.trackoffR12V20 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R12" &
                              df.trackoff$drivingSpeed == "V20")

plot.trackoffR12V20 <- ggplot(data = df.trackoffR12V20,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 38, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR12V20


# latforcoeff----
df.latforcoeffR12V20 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R12" &
                                 df.latforcoeff$drivingSpeed == "V20")

plot.latforcoeffR12V20 <- ggplot(data = df.latforcoeffR12V20,
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

plot.latforcoeffR12V20


# 1.3 RAD, 12m, V30----
# drivingTraj & longSpeed----
df.drivingtrajR12V30 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R12" &
                                 df.drivingtraj$drivingSpeed == "V30")

plot.drivingtrajR12V30 <- ggplot(data = df.drivingtrajR12V30,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(-12, 0)) +
  scale_y_continuous(limits = c(-12, 12)) +
  theme(legend.position = c(0.8, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR12V30


df.longspeedR12V30 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R12" &
                               df.longspeed$drivingSpeed == "V30")

plot.longspeedR12V30 <- ggplot(data = df.longspeedR12V30,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 38, 5)) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR12V30

grid.arrange(plot.drivingtrajR12V30, plot.longspeedR12V30, ncol=2, nrow=1)


# trackoff----
df.trackoffR12V30 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R12" &
                              df.trackoff$drivingSpeed == "V30")

plot.trackoffR12V30 <- ggplot(data = df.trackoffR12V30,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 38, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR12V30


# latforcoeff----
df.latforcoeffR12V30 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R12" &
                                 df.latforcoeff$drivingSpeed == "V30")

plot.latforcoeffR12V30 <- ggplot(data = df.latforcoeffR12V30,
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

plot.latforcoeffR12V30


# 1.4 RAD, 12m, 小结----
# drivingTraj----
df.drivingtrajR12 <- subset(x = df.drivingtraj,
                            df.drivingtraj$curveRad == "R12" &
                              df.drivingtraj$TrajTyp == "DrivingTraj" &
                              df.drivingtraj$drivingSpeed != "V40")

plot.drivingtrajR12 <- ggplot(data = df.drivingtrajR12,
                              aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(drivingSpeed)), size = 1) +
  scale_x_continuous(limits = c(-12, 0)) +
  scale_y_continuous(limits = c(-12, 12)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR12

# trackoff----
df.trackoffR12 <- subset(x = df.trackoff,
                         df.trackoff$curveRad == "R12" &
                           df.trackoff$drivingSpeed != "V40")

plot.trackoffR12 <- ggplot(data = df.trackoffR12,
                           aes(x = Station, y = TrackOff)) +
  geom_line(aes(colour = factor(drivingSpeed)), size = 1) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 38, 5)) +
  scale_y_continuous(limits = c(-5, 35), breaks = seq(0, 35, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR12


# latforcoeff----
df.latforcoeffR12 <- subset(x = df.latforcoeff,
                            df.latforcoeff$curveRad == "R12" &
                              df.latforcoeff$LatForTyp == "LatForAll" &
                              df.latforcoeff$drivingSpeed != "V40")

plot.latforcoeffR12 <- ggplot(data = df.latforcoeffR12,
                              aes(x = Station, y = LatFor)) +
  geom_line(aes(colour = factor(drivingSpeed)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1,
             linetype = "dashed") +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 38, 5)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR12


# 2 RAD, 15m----
# 2.1 RAD, 15m, V15----
# drivingTraj & longSpeed----
df.drivingtrajR15V15 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R15" &
                                 df.drivingtraj$drivingSpeed == "V15")

plot.drivingtrajR15V15 <- ggplot(data = df.drivingtrajR15V15,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(-15, 0)) +
  scale_y_continuous(limits = c(-15, 15)) +
  theme(legend.position = c(0.8, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR15V15


df.longspeedR15V15 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R15" &
                               df.longspeed$drivingSpeed == "V15")

plot.longspeedR15V15 <- ggplot(data = df.longspeedR15V15,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 48, 5)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR15V15

grid.arrange(plot.drivingtrajR15V15, plot.longspeedR15V15, ncol=2, nrow=1)


# trackoff----
df.trackoffR15V15 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R15" &
                              df.trackoff$drivingSpeed == "V15")

plot.trackoffR15V15 <- ggplot(data = df.trackoffR15V15,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 48, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR15V15


# latforcoeff----
df.latforcoeffR15V15 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R15" &
                                 df.latforcoeff$drivingSpeed == "V15")

plot.latforcoeffR15V15 <- ggplot(data = df.latforcoeffR15V15,
                                 aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 48, 5)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR15V15


# 2.2 RAD, 15m, V20----
# drivingTraj & longSpeed----
df.drivingtrajR15V20 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R15" &
                                 df.drivingtraj$drivingSpeed == "V20")

plot.drivingtrajR15V20 <- ggplot(data = df.drivingtrajR15V20,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(-15, 0)) +
  scale_y_continuous(limits = c(-15, 15)) +
  theme(legend.position = c(0.8, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR15V20


df.longspeedR15V20 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R15" &
                               df.longspeed$drivingSpeed == "V20")

plot.longspeedR15V20 <- ggplot(data = df.longspeedR15V20,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 48, 5)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR15V20

grid.arrange(plot.drivingtrajR15V20, plot.longspeedR15V20, ncol=2, nrow=1)


# trackoff----
df.trackoffR15V20 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R15" &
                              df.trackoff$drivingSpeed == "V20")

plot.trackoffR15V20 <- ggplot(data = df.trackoffR15V20,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 48, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR15V20


# latforcoeff----
df.latforcoeffR15V20 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R15" &
                                 df.latforcoeff$drivingSpeed == "V20")

plot.latforcoeffR15V20 <- ggplot(data = df.latforcoeffR15V20,
                                 aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 48, 5)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR15V20


# 2.3 RAD, 15m, V30----
# drivingTraj & longSpeed----
df.drivingtrajR15V30 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R15" &
                                 df.drivingtraj$drivingSpeed == "V30")

plot.drivingtrajR15V30 <- ggplot(data = df.drivingtrajR15V30,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(-15, 0)) +
  scale_y_continuous(limits = c(-15, 15)) +
  theme(legend.position = c(0.8, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR15V30


df.longspeedR15V30 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R15" &
                               df.longspeed$drivingSpeed == "V30")

plot.longspeedR15V30 <- ggplot(data = df.longspeedR15V30,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 48, 5)) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR15V30

grid.arrange(plot.drivingtrajR15V30, plot.longspeedR15V30, ncol=2, nrow=1)


# trackoff----
df.trackoffR15V30 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R15" &
                              df.trackoff$drivingSpeed == "V30")

plot.trackoffR15V30 <- ggplot(data = df.trackoffR15V30,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 48, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR15V30


# latforcoeff----
df.latforcoeffR15V30 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R15" &
                                 df.latforcoeff$drivingSpeed == "V30")

plot.latforcoeffR15V30 <- ggplot(data = df.latforcoeffR15V30,
                                 aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 48, 5)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR15V30


# 2.4 RAD, 15m, 小结----
# drivingTraj----
df.drivingtrajR15 <- subset(x = df.drivingtraj,
                            df.drivingtraj$curveRad == "R15" &
                              df.drivingtraj$TrajTyp == "DrivingTraj" &
                              df.drivingtraj$drivingSpeed != "V40")

plot.drivingtrajR15 <- ggplot(data = df.drivingtrajR15,
                              aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(drivingSpeed)), size = 1) +
  scale_x_continuous(limits = c(-15, 0)) +
  scale_y_continuous(limits = c(-15, 15)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR15


# trackoff----
df.trackoffR15 <- subset(x = df.trackoff,
                         df.trackoff$curveRad == "R15" &
                           df.trackoff$drivingSpeed != "V40")

plot.trackoffR15 <- ggplot(data = df.trackoffR15,
                           aes(x = Station, y = TrackOff)) +
  geom_line(aes(colour = factor(drivingSpeed)), size = 1) +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 48, 5)) +
  scale_y_continuous(limits = c(-5, 30), breaks = seq(0, 30, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR15


# latforcoeff----
df.latforcoeffR15 <- subset(x = df.latforcoeff,
                            df.latforcoeff$curveRad == "R15" &
                              df.latforcoeff$LatForTyp == "LatForAll" &
                              df.latforcoeff$drivingSpeed != "V40")

plot.latforcoeffR15 <- ggplot(data = df.latforcoeffR15,
                              aes(x = Station, y = LatFor)) +
  geom_line(aes(colour = factor(drivingSpeed)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1,
             linetype = "dashed") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 48, 5)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR15


# 3 RAD, 30m----
# 3.1 RAD, 30m, V15----
# drivingTraj & longSpeed----
df.drivingtrajR30V15 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R30" &
                                 df.drivingtraj$drivingSpeed == "V15")

plot.drivingtrajR30V15 <- ggplot(data = df.drivingtrajR30V15,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(-30, 0)) +
  scale_y_continuous(limits = c(-30, 30)) +
  theme(legend.position = c(0.8, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR30V15


df.longspeedR30V15 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R30" &
                               df.longspeed$drivingSpeed == "V15")

plot.longspeedR30V15 <- ggplot(data = df.longspeedR30V15,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 95), breaks = seq(0, 95, 10)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR30V15

grid.arrange(plot.drivingtrajR30V15, plot.longspeedR30V15, ncol=2, nrow=1)


# trackoff----
df.trackoffR30V15 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R30" &
                              df.trackoff$drivingSpeed == "V15")

plot.trackoffR30V15 <- ggplot(data = df.trackoffR30V15,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 95), breaks = seq(0, 95, 10)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR30V15


# latforcoeff----
df.latforcoeffR30V15 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R30" &
                                 df.latforcoeff$drivingSpeed == "V15")

plot.latforcoeffR30V15 <- ggplot(data = df.latforcoeffR30V15,
                                 aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 95), breaks = seq(0, 95, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR30V15


# 3.2 RAD, 30m, V20----
# drivingTraj & longSpeed----
df.drivingtrajR30V20 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R30" &
                                 df.drivingtraj$drivingSpeed == "V20")

plot.drivingtrajR30V20 <- ggplot(data = df.drivingtrajR30V20,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(-30, 0)) +
  scale_y_continuous(limits = c(-30, 30)) +
  theme(legend.position = c(0.8, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR30V20


df.longspeedR30V20 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R30" &
                               df.longspeed$drivingSpeed == "V20")

plot.longspeedR30V20 <- ggplot(data = df.longspeedR30V20,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 95), breaks = seq(0, 95, 10)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR30V20

grid.arrange(plot.drivingtrajR30V20, plot.longspeedR30V20, ncol=2, nrow=1)


# trackoff----
df.trackoffR30V20 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R30" &
                              df.trackoff$drivingSpeed == "V20")

plot.trackoffR30V20 <- ggplot(data = df.trackoffR30V20,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 95), breaks = seq(0, 95, 10)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR30V20


# latforcoeff----
df.latforcoeffR30V20 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R30" &
                                 df.latforcoeff$drivingSpeed == "V20")

plot.latforcoeffR30V20 <- ggplot(data = df.latforcoeffR30V20,
                                 aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 95), breaks = seq(0, 95, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR30V20


# 3.3 RAD, 30m, V30----
# drivingTraj & longSpeed----
df.drivingtrajR30V30 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R30" &
                                 df.drivingtraj$drivingSpeed == "V30")

plot.drivingtrajR30V30 <- ggplot(data = df.drivingtrajR30V30,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(-30, 0)) +
  scale_y_continuous(limits = c(-30, 30)) +
  theme(legend.position = c(0.8, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR30V30


df.longspeedR30V30 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R30" &
                               df.longspeed$drivingSpeed == "V30")

plot.longspeedR30V30 <- ggplot(data = df.longspeedR30V30,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 95), breaks = seq(0, 95, 10)) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR30V30

grid.arrange(plot.drivingtrajR30V30, plot.longspeedR30V30, ncol=2, nrow=1)


# trackoff----
df.trackoffR30V30 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R30" &
                              df.trackoff$drivingSpeed == "V30")

plot.trackoffR30V30 <- ggplot(data = df.trackoffR30V30,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 95), breaks = seq(0, 95, 10)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR30V30


# latforcoeff----
df.latforcoeffR30V30 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R30" &
                                 df.latforcoeff$drivingSpeed == "V30")

plot.latforcoeffR30V30 <- ggplot(data = df.latforcoeffR30V30,
                                 aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 95), breaks = seq(0, 95, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR30V30


# 3.4 RAD, 30m, V40----
# drivingTraj & longSpeed----
df.drivingtrajR30V40 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R30" &
                                 df.drivingtraj$drivingSpeed == "V40")

plot.drivingtrajR30V40 <- ggplot(data = df.drivingtrajR30V40,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(-30, 0)) +
  scale_y_continuous(limits = c(-30, 30)) +
  theme(legend.position = c(0.8, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR30V40


df.longspeedR30V40 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R30" &
                               df.longspeed$drivingSpeed == "V40")

plot.longspeedR30V40 <- ggplot(data = df.longspeedR30V40,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 95), breaks = seq(0, 95, 10)) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR30V40

grid.arrange(plot.drivingtrajR30V40, plot.longspeedR30V40, ncol=2, nrow=1)


# trackoff----
df.trackoffR30V40 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R30" &
                              df.trackoff$drivingSpeed == "V40")

plot.trackoffR30V40 <- ggplot(data = df.trackoffR30V40,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 95), breaks = seq(0, 95, 10)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR30V40


# latforcoeff----
df.latforcoeffR30V40 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R30" &
                                 df.latforcoeff$drivingSpeed == "V40")

plot.latforcoeffR30V40 <- ggplot(data = df.latforcoeffR30V40,
                                 aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 95), breaks = seq(0, 95, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR30V40


# 3.5 RAD, 30m, 小结----
df.drivingtrajR30 <- subset(x = df.drivingtraj,
                            df.drivingtraj$curveRad == "R30" &
                              df.drivingtraj$TrajTyp == "DrivingTraj")

plot.drivingtrajR30 <- ggplot(data = df.drivingtrajR30,
                              aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(drivingSpeed)), size = 1) +
  scale_x_continuous(limits = c(-30, 0)) +
  scale_y_continuous(limits = c(-30, 30)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR30


# trackoff----
df.trackoffR30 <- subset(x = df.trackoff,
                         df.trackoff$curveRad == "R30")

plot.trackoffR30 <- ggplot(data = df.trackoffR30,
                           aes(x = Station, y = TrackOff)) +
  geom_line(aes(colour = factor(drivingSpeed)), size = 1) +
  scale_x_continuous(limits = c(0, 95), breaks = seq(0, 95, 10)) +
  scale_y_continuous(limits = c(-5, 30), breaks = seq(0, 30, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR30


# latforcoeff----
df.latforcoeffR30 <- subset(x = df.latforcoeff,
                            df.latforcoeff$curveRad == "R30" &
                              df.latforcoeff$LatForTyp == "LatForAll")

plot.latforcoeffR30 <- ggplot(data = df.latforcoeffR30,
                              aes(x = Station, y = LatFor)) +
  geom_line(aes(colour = factor(drivingSpeed)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1,
             linetype = "dashed") +
  scale_x_continuous(limits = c(0, 95), breaks = seq(0, 95, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR30


# 4 RAD, 40m----
# 4.1 RAD, 40m, V15----
# drivingTraj & longSpeed----
df.drivingtrajR40V15 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R40" &
                                 df.drivingtraj$drivingSpeed == "V15")

plot.drivingtrajR40V15 <- ggplot(data = df.drivingtrajR40V15,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(-40, 0)) +
  scale_y_continuous(limits = c(-40, 40)) +
  theme(legend.position = c(0.8, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR40V15


df.longspeedR40V15 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R40" &
                               df.longspeed$drivingSpeed == "V15")

plot.longspeedR40V15 <- ggplot(data = df.longspeedR40V15,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 125), breaks = seq(0, 125, 10)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR40V15

grid.arrange(plot.drivingtrajR40V15, plot.longspeedR40V15, ncol=2, nrow=1)


# trackoff----
df.trackoffR40V15 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R40" &
                              df.trackoff$drivingSpeed == "V15")

plot.trackoffR40V15 <- ggplot(data = df.trackoffR40V15,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 125), breaks = seq(0, 125, 10)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR40V15


# latforcoeff----
df.latforcoeffR40V15 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R40" &
                                 df.latforcoeff$drivingSpeed == "V15")

plot.latforcoeffR40V15 <- ggplot(data = df.latforcoeffR40V15,
                                 aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 125), breaks = seq(0, 125, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR40V15


# 4.2 RAD, 40m, V20----
# drivingTraj & longSpeed----
df.drivingtrajR40V20 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R40" &
                                 df.drivingtraj$drivingSpeed == "V20")

plot.drivingtrajR40V20 <- ggplot(data = df.drivingtrajR40V20,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(-40, 0)) +
  scale_y_continuous(limits = c(-40, 40)) +
  theme(legend.position = c(0.8, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR40V20


df.longspeedR40V20 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R40" &
                               df.longspeed$drivingSpeed == "V20")

plot.longspeedR40V20 <- ggplot(data = df.longspeedR40V20,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 125), breaks = seq(0, 125, 10)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR40V20

grid.arrange(plot.drivingtrajR40V20, plot.longspeedR40V20, ncol=2, nrow=1)


# trackoff----
df.trackoffR40V20 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R40" &
                              df.trackoff$drivingSpeed == "V20")

plot.trackoffR40V20 <- ggplot(data = df.trackoffR40V20,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 125), breaks = seq(0, 125, 10)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR40V20


# latforcoeff----
df.latforcoeffR40V20 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R40" &
                                 df.latforcoeff$drivingSpeed == "V20")

plot.latforcoeffR40V20 <- ggplot(data = df.latforcoeffR40V20,
                                 aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 125), breaks = seq(0, 125, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR40V20


# 4.3 RAD, 40m, V30----
# drivingTraj & longSpeed----
df.drivingtrajR40V30 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R40" &
                                 df.drivingtraj$drivingSpeed == "V30")

plot.drivingtrajR40V30 <- ggplot(data = df.drivingtrajR40V30,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(-40, 0)) +
  scale_y_continuous(limits = c(-40, 40)) +
  theme(legend.position = c(0.8, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR40V30


df.longspeedR40V30 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R40" &
                               df.longspeed$drivingSpeed == "V30")

plot.longspeedR40V30 <- ggplot(data = df.longspeedR40V30,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 125), breaks = seq(0, 125, 10)) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR40V30

grid.arrange(plot.drivingtrajR40V30, plot.longspeedR40V30, ncol=2, nrow=1)


# trackoff----
df.trackoffR40V30 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R40" &
                              df.trackoff$drivingSpeed == "V30")

plot.trackoffR40V30 <- ggplot(data = df.trackoffR40V30,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 125), breaks = seq(0, 125, 10)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR40V30


# latforcoeff----
df.latforcoeffR40V30 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R40" &
                                 df.latforcoeff$drivingSpeed == "V30")

plot.latforcoeffR40V30 <- ggplot(data = df.latforcoeffR40V30,
                                 aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 125), breaks = seq(0, 125, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR40V30


# 4.4 RAD, 40m, V40----
# drivingTraj & longSpeed----
df.drivingtrajR40V40 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R40" &
                                 df.drivingtraj$drivingSpeed == "V40")

plot.drivingtrajR40V40 <- ggplot(data = df.drivingtrajR40V40,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(-40, 0)) +
  scale_y_continuous(limits = c(-40, 40)) +
  theme(legend.position = c(0.8, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR40V40


df.longspeedR40V40 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R40" &
                               df.longspeed$drivingSpeed == "V40")

plot.longspeedR40V40 <- ggplot(data = df.longspeedR40V40,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 125), breaks = seq(0, 125, 10)) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR40V40

grid.arrange(plot.drivingtrajR40V40, plot.longspeedR40V40, ncol=2, nrow=1)


# trackoff----
df.trackoffR40V40 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R40" &
                              df.trackoff$drivingSpeed == "V40")

plot.trackoffR40V40 <- ggplot(data = df.trackoffR40V40,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 125), breaks = seq(0, 125, 10)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR40V40


# latforcoeff----
df.latforcoeffR40V40 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R40" &
                                 df.latforcoeff$drivingSpeed == "V40")

plot.latforcoeffR40V40 <- ggplot(data = df.latforcoeffR40V40,
                                 aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 125), breaks = seq(0, 125, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR40V40


# 4.5 RAD, 40m, 小结----
df.drivingtrajR40 <- subset(x = df.drivingtraj,
                            df.drivingtraj$curveRad == "R40" &
                              df.drivingtraj$TrajTyp == "DrivingTraj")

plot.drivingtrajR40 <- ggplot(data = df.drivingtrajR40,
                              aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(drivingSpeed)), size = 1) +
  scale_x_continuous(limits = c(-40, 0)) +
  scale_y_continuous(limits = c(-40, 40)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR40


# trackoff----
df.trackoffR40 <- subset(x = df.trackoff,
                         df.trackoff$curveRad == "R40")

plot.trackoffR40 <- ggplot(data = df.trackoffR40,
                           aes(x = Station, y = TrackOff)) +
  geom_line(aes(colour = factor(drivingSpeed)), size = 1) +
  scale_x_continuous(limits = c(0, 125), breaks = seq(0, 125, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR40


# latforcoeff----
df.latforcoeffR40 <- subset(x = df.latforcoeff,
                            df.latforcoeff$curveRad == "R40" &
                              df.latforcoeff$LatForTyp == "LatForAll")

plot.latforcoeffR40 <- ggplot(data = df.latforcoeffR40,
                              aes(x = Station, y = LatFor)) +
  geom_line(aes(colour = factor(drivingSpeed)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1,
             linetype = "dashed") +
  scale_x_continuous(limits = c(0, 125), breaks = seq(0, 125, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR40

