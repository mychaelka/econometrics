##### 1 Causal graphical methods #####
library(dagitty)
library(ggdag)

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

ggdag(G1) + theme_dag() #plotting the causal graph 

#### listing all paths ####
paths(G1, 
      from = exposures(G1),
      to = outcomes(G1))

# paths "D -> O -> W" and "D -> W" are open (d-connected) -- causal
# path "D -> O <- A -> W" is closed -- non-causal

# Determine if it is possible to identify the causal effect of discrimination on 
# wages based on observed probability distribution of gender, discrimination, 
# occupation and wages.

# There are no testable implications due to both direct and indirect effect of
# discrimination on wages.

# Discuss the pros and cons of adjusting for the occupation.

# After adjusting for occupation, we would be able to recover the direct effect
# of discrimination on wages, however, we would block the indirect path going 
# through occupation, so the resulting effect would be lower. Moreover, occupation
# can be considered a collider between discrimination and unobserved ability,
# so controlling for it opens the path between discrimination, ability and 
# wages and a spurious effect between D and W through A may emerge.


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

ggdag(G2) + theme_dag() #plotting the causal graph 

paths(G2, 
      from = exposures(G2),
      to = outcomes(G2))

# paths "D -> O -> W", "D <- G -> O -> W" and "D -> W" are open (d-connected) -- causal
# paths "D -> O <- A -> W" and "D <- G -> O <- A -> W"
# are closed -- non-causal


# Determine if it is possible to identify the causal effect of discrimination on 
# wages based on observed probability distribution of gender, discrimination, 
# occupation and wages.

# There are no testable implications due to both direct and indirect effect of
# discrimination on wages.

# 1. adjusting only for occupation
# same as before + new causal path emerges between gender and wages through
# ability and possibly the effect of gender on wages increases due to opening 
# the path between G and W through O and D.
# 2. adjusting only for gender
# Gender influences the indirect path between D and W through O as a confounder,
# to recover the original indirect path, it is necessary to adjust for gender.
# also changes the effect of D on W (interaction of gender discrimination and others)
# 3. adjusting for both occupation and gender
# adjusting for gender and occupation changes the effect of D on W, closes the
# indirect path between D and W through O and opens the path between D and W through
# collider O and unobserved A.
# it's depressing, but that's the situation


# c)
G3 = dagitty('dag{
W [outcome, pos="2,0"]
D [exposure, pos="1,1"]
A [observed, pos="1, -1"]
G [pos="1, 3"]
O [pos="0, 0"]
G -> D
G -> O
D -> O
D -> W
A -> O
A -> W            
O -> W}')

ggdag(G3) + theme_dag() #plotting the causal graph 

paths(G3, 
      from = exposures(G3),
      to = outcomes(G3))

# Determine if it is possible to identify the causal effect of discrimination on wages based on observed
# probability distribution of gender, discrimination, occupation and wages.
# SAME AS BEFORE

# 1. occupation only
# -||-
# 2. gender only
# -||-
# 3. ability only
# -||-
# 4. occupation and gender
# -||-
# 5. occupation and ability
# possibly changes effect of G on D and therefore the effect of D on W
# 6. gender and ability
# both direct and indirect effects stay, othres are closed
# 7. all three
# for each G we can find the true direct effect of D on W, indirect effect via O 
# is blocked. All other paths are closed.


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
"... it ain't much but it's honest work. :X"
