library(dagitty)
library(ggdag)
library(viridis)
library(tidyverse)
library(AER)

###### TASK 1
# a)
G1 = dagitty('dag{
W [outcome, pos="2,0"]
D [exposure, pos="1,1"]
A [unobserved, pos="1, -1"]
G [pos="1, 3"]
O [pos="0, 0"]
G -> D
D -> O
D -> W
A -> O
A -> W            
O -> W}')

tidy_dag <- tidy_dagitty(G1) %>% 
  dplyr::mutate(VariableType = ifelse(name == "A", "Unobserved", 
                                      ifelse(name == "W", "Outcome", "Observed")))

tidy_dag %>% ggplot(aes(x = x, 
                        y = y,
                        xend = xend,
                        yend = yend)) +
  geom_dag_point(aes(colour = VariableType)) +
  scale_color_manual(values = c("darkblue", "darkred", "grey")) +
  geom_dag_edges() +
  geom_dag_text() +
  theme_dag()

# list all paths
paths(G1, 
      from = exposures(G1),
      to = outcomes(G1))

ggdag_paths(G1, shadow = TRUE) + theme_dag() + 
  theme(legend.position = "none", strip.text = element_blank()) +
  scale_color_manual(values = "darkblue") + 
  scale_fill_manual(values = "darkblue") + 
  ggraph::scale_edge_color_manual(values = "darkblue") +
  ggtitle("Open paths from discrimination to wages")

# Determine if it is possible to identify the causal effect of discrimination on 
# wages based on observed probability distribution of gender, discrimination, 
# occupation and wages.
adjustmentSets(G1)

ggdag_adjustment_set(tidy_dag, node_size = 14, shadow = TRUE) +
  scale_color_manual(values = c("darkblue", "darkred", "grey")) +
  theme_dag()

# b)
G2 = dagitty('dag{
W [outcome, pos="2,0"]
D [exposure, pos="1,1"]
A [unobserved, pos="1, -1"]
G [pos="1, 3"]
O [pos="0, 0"]
G -> D
G -> O
D -> O
D -> W
A -> O
A -> W            
O -> W}')

tidy_dag <- tidy_dagitty(G2) %>% 
  dplyr::mutate(VariableType = ifelse(name == "A", "Unobserved", 
                                      ifelse(name == "W", "Outcome", "Observed")))

tidy_dag %>% ggplot(aes(x = x, 
                        y = y,
                        xend = xend,
                        yend = yend)) +
  geom_dag_point(aes(colour = VariableType)) +
  scale_color_manual(values = c("darkblue", "darkred", "grey")) +
  geom_dag_edges() +
  geom_dag_text() +
  theme_dag()

# list all paths 
paths(G2, 
      from = exposures(G2),
      to = outcomes(G2))

ggdag_paths(G2, shadow = TRUE) + theme_dag() + 
  theme(legend.position = "none", strip.text = element_blank()) +
  scale_color_manual(values = "darkblue") + 
  scale_fill_manual(values = "darkblue") + 
  ggraph::scale_edge_color_manual(values = "darkblue") +
  ggtitle("Open paths from discrimination to wages")


# Determine if it is possible to identify the causal effect of discrimination on 
# wages based on observed probability distribution of gender, discrimination, 
# occupation and wages.
adjustmentSets(G2)

ggdag_adjustment_set(tidy_dag, node_size = 14, shadow = TRUE) +
  scale_color_manual(values = c("darkblue", "darkred", "grey")) +
  theme_dag()

# c)
G3 = dagitty('dag{
W [outcome, pos="2,0"]
D [exposure, pos="1,1"]
A [pos="1, -1"]
G [pos="1, 3"]
O [pos="0, 0"]
G -> D
G -> O
D -> O
D -> W
A -> O
A -> W            
O -> W}')

tidy_dag <- tidy_dagitty(G3) %>% 
  dplyr::mutate(VariableType = ifelse(name == "W", "Outcome", "Observed"))

tidy_dag %>% ggplot(aes(x = x, 
                        y = y,
                        xend = xend,
                        yend = yend)) +
  geom_dag_point(aes(colour = VariableType)) +
  scale_color_manual(values = c("darkblue", "darkred", "grey")) +
  geom_dag_edges() +
  geom_dag_text() +
  theme_dag()

# list all the paths
paths(G3, 
      from = exposures(G3),
      to = outcomes(G3))

ggdag_paths(G3, shadow = TRUE) + theme_dag() + 
  theme(legend.position = "none", strip.text = element_blank()) +
  scale_color_manual(values = "darkblue") + 
  scale_fill_manual(values = "darkblue") + 
  ggraph::scale_edge_color_manual(values = "darkblue") +
  ggtitle("Open paths from discrimination to wages")


adjustmentSets(G3, exposure = "D", outcome = "W", type="all")

ggdag_adjustment_set(tidy_dag, node_size = 14, effect="total", type="all",
                     shadow = TRUE) +
  scale_color_manual(values = c("darkblue", "darkred", "grey")) +
  theme_dag()

# Assume that discrimination variable is binary and propose an estimator for (total) average treatment
# effect of discrimination on wages
# Control for gender and ability 

set.seed(666)
gender <- rbinom(1000, 1, 0.5)
discrimination <- gender + rnorm(1000, 0, 0.2)
discrimination <- ifelse(discrimination >= 0.2, 1, 0)
summary(lm(discrimination ~ gender))

ability <- rnorm(1000)
occupation <- 1.5*gender + discrimination + 2*ability + rnorm(1000, 0, 0.2)
wages <- 3*ability + 2*discrimination + 6*occupation + rnorm(1000, 0, 0.2)

summary(lm(wages ~ discrimination + gender + ability))


###### TASK 2
############## PART 1 ##############

dat <- matrix(c(20,19,1,1,
                23,21,1,1,
                36,20,0,1,
                43,19,0,1,
                19,19,0,1,
                23,55,0,0,
                26,41,1,0,
                21,21,1,0,
                33,37,0,0,
                16,17,1,0),
              byrow=T, ncol=4)
colnames(dat) <- c("Y1","Y0","D","X")
dat <- as.data.frame(dat)

#a)
#ATE = E[Y(1)-Y(0)] 
ATE <- mean(dat$Y1) - mean(dat$Y0)
ATE

#b)
#ATT = E[Y(1)-Y(0) | D=1]
ATT <- mean(dat$Y1[dat$D==1]) - mean(dat$Y0[dat$D==1])
ATT

#c)
#ATU = E[Y(1)-Y(0) | D=0]
ATU <- mean(dat$Y1[dat$D==0]) - mean(dat$Y0[dat$D==0])
ATU

#d)
#ATT = E[Y(1)-Y(0) | D=1, X=1]
ATT1 <- mean(dat$Y1[dat$D==1 & dat$X == 1]) - mean(dat$Y0[dat$D==1 & dat$X == 1])
ATT1

#e)
#ATT = E[Y(1)-Y(0) | D=1, X=0]
ATT2 <- mean(dat$Y1[dat$D==1 & dat$X == 0]) - mean(dat$Y0[dat$D==1 & dat$X == 0])
ATT2

#f)
dat_intervention <- dat
dat_intervention$D <- ifelse(dat_intervention$Y1 >= dat_intervention$Y0, 1, 0)

# first select the true realized Y's
true_y <- ifelse(dat$D == 1, dat$Y1, dat$Y0)

# Y's after intervention
true_y1 <- ifelse(dat_intervention$D ==1, dat_intervention$Y1, dat_intervention$Y0)

mean(true_y) - mean(true_y1)

############## PART 2 ##############
#2a)
mean(dat$Y1[dat$D == 1]) - mean(dat$Y0[dat$D == 0])

#2b)
selection_bias <- mean(dat$Y0[dat$D == 1]) - mean(dat$Y0[dat$D == 0])
mean(dat$Y1[dat$D == 1]) - mean(dat$Y0[dat$D == 0]) == ATT + selection_bias

#2c)
prob_D0 <- length(dat$D[dat$D == 0])/length(dat$D)
left <- mean(dat$Y1[dat$D == 1]) - mean(dat$Y0[dat$D == 0])
right <- round((ATE + selection_bias + prob_D0*(ATT - ATU)),1)  # float imprecision
left == right

#2d)
mean(dat$Y0[dat$D == 1 & dat$X == 1]) - mean(dat$Y0[dat$D == 0 & dat$X == 1])
mean(dat$Y0[dat$D == 1 & dat$X == 0]) - mean(dat$Y0[dat$D == 0 & dat$X == 0])

############## PART 3 ##############
# Does compulsory school attendance affect schooling and earnings?
load("asciiqob.RData")

# Figure 1
x %>%
  group_by(year_of_birth, quarter_of_birth) %>%
  summarise(education = mean(education), .groups = "drop") %>%
  mutate(xcoord = year_of_birth + 0.25*(quarter_of_birth-1)) %>%
  ggplot(aes(x = xcoord, y = education)) +
  geom_line() +
  geom_point(size = 3) +
  geom_text(aes(label = quarter_of_birth), nudge_y = -0.04) +
  scale_x_continuous("Year of birth") +
  scale_y_continuous("Years of education") +
  theme_bw()

# two-period, two-sided moving average, MA(+2,-2)
# for the cohort of men born in year c and quarter j
# MA_cj = (E 2+ E-1 + E+1 + E+2) / 4
library(zoo)

x1 <- x %>%
  group_by(year_of_birth, quarter_of_birth) %>%
  summarise(education = mean(education), .groups = "drop")

x.ma <- rollmean(x1$education, 3, fill = NA, align = "right")

# The "detrended" education series is simply E - MA
# Figure 4
x %>%
  group_by(year_of_birth, quarter_of_birth) %>%
  summarise(education = mean(education), .groups = "drop") %>%
  mutate(xcoord = year_of_birth + 0.25*(quarter_of_birth-1), ma = x.ma) %>%
  ggplot(aes(x = xcoord, y = education - ma)) +
  geom_line(aes(y=0)) +
  labs(fill = "Quarter of birth") +
  geom_col(aes(fill = factor(quarter_of_birth))) +
  geom_text(aes(label = quarter_of_birth), nudge_y = 0.01,) +
  scale_x_continuous("Year of birth") +
  scale_y_continuous("Schooling differential", limits = c(-0.2, 0.2)) +
  theme_bw()

# Table 1
x$quarter_of_birth1 <- relevel(factor(x$quarter_of_birth), ref = 4)
mean(x$education)
summary(lm(education ~ factor(quarter_of_birth), x))

# Figure 5
x %>%
  group_by(year_of_birth, quarter_of_birth) %>%
  summarise(log_weekly_wage = mean(log_weekly_wage), .groups = "drop") %>%
  mutate(xcoord = year_of_birth + 0.25*(quarter_of_birth-1)) %>%
  ggplot(aes(x = xcoord, y = log_weekly_wage)) +
  geom_line() +
  geom_point(size = 3) +
  geom_text(aes(label = quarter_of_birth), nudge_y = 0.0015, nudge_x = 0.1) +
  scale_x_continuous("Year of birth") +
  scale_y_continuous("Log Weekly Earnings") +
  theme_bw()

summary(lm(log_weekly_wage ~ factor(quarter_of_birth), x))

# OLS and TSLS Estimates
iv <- ivreg(log_weekly_wage ~ education + year_of_birth | quarter_of_birth + year_of_birth*quarter_of_birth + year_of_birth, data = x)
summary(iv)

ols <- lm(log_weekly_wage ~ education + year_of_birth, data = x)
summary(ols)
