#Using traj to identify clusters of longitudinal trajectories
#KML K means clustering
library(dplyr)
library(tidyr)
library(ggplot2)
library(kml)
library(data.table)
library(readxl)
set.seed(1)


MCDI_data <- read_excel("~/Desktop/Projects/imaging/Native Language/6wk/spreadsheets/MCDI_02-04-18.xlsx", sheet = "short_MCDI")
MCDI <- as.data.table(MCDI_data)
word_COMP <- MCDI %>% select(3:5)
word_PROD <- MCDI %>% select(6:8)
word_RADV <- MCDI %>% select(11:13)
#MCDI <- cbsst[, .(ID, ave_face=`aveprage_face`, pface = `Percent face`, psal = `Psal`, face_sal = `Face over Sal`, sal_face = `Sal over Face`, SN_LRvsHR_OFC, SN_HRvsLR_SMA, sex, age, age_sq  = age*age, RX, StimType, pface_2=Percent_dyad, Mean_Fix_MS, sub_id = as.factor(ID))]
mcdi_clust <- clusterLongData(id=MCDI$ID, time= c(9,12,18), traj=as.matrix(word_COMP))
mcdi_clust_prod <- clusterLongData(id=MCDI$ID, time= c(9,12,18), traj=as.matrix(word_PROD))
mcdi_clust_radv <- clusterLongData(id=MCDI$ID, time= c(9,12,18), traj=as.matrix(word_RADV))
plot(mcdi_clust)
kml(mcdi_clust_radv)
plot(mcdi_clust_radv,2)

plot(mcdi_clust_prod)
kml(mcdi_clust_prod)
plot(mcdi_clust,2)

plot(mcdi_clust_radv)
kml(mcdi_clust_radv)
plot(mcdi_clust,2)

plotAllCriterion(mcdi_clust)
try(choice(mcdi_clust))

plotAllCriterion(mcdi_clust_prod)
try(choice(mcdi_clust_prod))

plotAllCriterion(mcdi_clust_radv)
try(choice(mcdi_clust_radv))

MCDI$radv_clust <- getClusters(mcdi_clust_radv,2)

summary(glm(RX~radv_clust, data=MCDI, family="binomial"))

#### MEMBERSHIP
traj <- gald()["word_COMP"]
center <- word_COMP[runif(2,1, nrow(word_COMP)),]
affectFuzzyIndiv(traj, center)


# Plot Clusters in ggplot
# We're going to use the 3 cluster solution, based on kml
# First, convert the data to a long format for ggplot, then plot
comp_traj <- cbind.data.frame(id = MCDI$ID, traj = getClusters(mcdi_clust_radv, 2), MCDI) %>% 
  gather(time,count,-traj, -ID) %>%
  group_by(ID) %>%
  mutate(age = c(9:18))

# Plot
# use 'stat_smooth' to fit a cubic polynomial trend to each trajectory
ggplot(MCDI_long) +
  stat_summary(aes(x = age, y = count, group = mcdi_clust_radv, color = mcdi_clust_radv), fun.y = "mean", geom = "line", linetype = 2) +
  stat_summary(aes(x = age, y = count, group = mcdi_clust_radv, color = mcdi_clust_radv), fun.y = "mean", geom = "point", size = 2) +
  stat_smooth(aes(x = age, y = count, group = traj, color = traj), method = "lm", formula = y ~ poly(x,3), se = FALSE) +
  geom_line(aes(x = age, y = count, group = ID), alpha = .1) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~traj, ncol = 2)



Mullen_data <- read_excel("~/Desktop/Projects/imaging/Native Language/6wk/spreadsheets/Mullen_02-04-18.xlsx", sheet = "SHORT")
Mullen_short <- as.data.table(Mullen_data)
rl_traj <- Mullen %>% select(3:5)
el_traj <- Mullen %>% select(7:9)
verbal_traj <- Mullen_short %>% select(11:13)
verbal_T_traj <- Mullen_short %>% select(15:17)
verbal_EvsR_T_traj <- Mullen_short %>% select(19:21)

#MCDI <- cbsst[, .(ID, ave_face=`aveprage_face`, pface = `Percent face`, psal = `Psal`, face_sal = `Face over Sal`, sal_face = `Sal over Face`, SN_LRvsHR_OFC, SN_HRvsLR_SMA, sex, age, age_sq  = age*age, RX, StimType, pface_2=Percent_dyad, Mean_Fix_MS, sub_id = as.factor(ID))]
rl_clust <- clusterLongData(id=Mullen$`Row Labels`, time= c(6,12,18), traj=as.matrix(rl_traj))
el_clust <- clusterLongData(id=Mullen$`Row Labels`, time= c(6,12,18), traj=as.matrix(el_traj))
verbal_EvsR_traj <- clusterLongData(id=Mullen_short$`Row Labels`, time= c(6,12,18), traj=as.matrix(verbal_EvsR_T_traj))
plot(verbal_EvsR_traj)
kml(verbal_EvsR_traj)

plotAllCriterion(verbal_EvsR_traj)
try(choice(verbal_EvsR_traj))

plot(el_clust)
kml(el_clust)

plotAllCriterion(el_clust)
try(choice(el_clust))

plot(verbal_clust_no36)
kml(verbal_clust_no36)

plotAllCriterion(verbal_clust_no36)
try(choice(verbal_clust_no36))

Mullen_short$`Verbal_TRAJ_A-good` <- getClusters(verbal_EvsR_traj,2)

summary(glm(RX~`Verbal_TRAJ_A-good`, data=Mullen_short, family="binomial"))

# Plot Clusters in ggplot
# We're going to use the 3 cluster solution, based on kml
# First, convert the data to a long format for ggplot, then plot
MCDI_data <- read_excel("~/Desktop/Projects/imaging/Native Language/6wk/spreadsheets/MCDI_02-04-18.xlsx", sheet = "long_MCDI")
conv_traj <- as.data.table(MCDI_data)
conv_traj <- na.omit(conv_traj)

Mullen_long <- read_excel("~/Desktop/Projects/imaging/Native Language/6wk/spreadsheets/Mullen_02-04-18.xlsx", sheet = "LONG")
traj_mull <- as.data.table(Mullen_long)
traj_mull <- na.omit(traj_mull)
# Plot
# use 'stat_smooth' to fit a cubic polynomial trend to each trajectory
traj_mull$rx <- as.character(traj_mull$Risk)
ggplot(traj_mull) +
  stat_summary(aes(x = age, y = Verbal_Norm, group = rx, color = rx), fun.y = "mean", geom = "point", size = 2) +
  stat_smooth(aes(x = age, y = Verbal_Norm, group = traj, color =traj), method = "lm", formula = y ~ poly(x,1), se = FALSE) +
  geom_line(aes(x = age, y = Verbal_Norm, group = PROJECTID), alpha = .1) +
  theme_minimal() +
  scale_x_continuous(name="Age, Months",breaks=seq(6,18,6))+
  scale_color_manual(values=c("dodgerblue2", "firebrick2", "darkorchid3", "palegreen4")) + 
  ylab("Verbal Developmental Quotient") + 
  ggsave("VDQ_traj.pdf",path=workingDirName,width=5,height=3,units="in")

#+
 # scale_color_manual(values=c("darkorchid3", "palegreen4"))
#+
 # facet_wrap(~traj, ncol = 2)
conv_traj$RX <- as.character(conv_traj$RX)
ggplot(conv_traj) +
  stat_summary(aes(x = age, y = Radv, group = RX, color = RX), fun.y = "mean", geom = "point", size = 2) +
  stat_smooth(aes(x = age, y = Radv, group = traj, color = traj), method = "lm", formula = y ~ poly(x,1), se = FALSE) +
  geom_line(aes(x = age, y = Radv, group = PROJECTID), alpha = .1) +
  theme_minimal() +
  scale_x_continuous(name="Age, Months",breaks=seq(9,18,3)) +
  ylab("Receptive Advantage") + 
  scale_color_manual(values=c("dodgerblue2", "firebrick2", "darkorchid3", "palegreen4")) +
    ggsave("MCDI_RADV_traj.pdf",path=workingDirName,width=5,height=3,units="in")

 # 
#stat_summary(aes(x = age, y = Radv, group = traj, color = traj), fun.y = "mean", geom = "line", linetype = 2) +
