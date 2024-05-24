# Simulation 

# case 1) When the interaction is between X and W

rm(list=ls())
n <- 1000000
set.seed(5)

# 1) Simulate the source data
u1_s <- rnorm(n, 0, 1)  # Confounding between Z and Y

W_source <- rbinom(n, 1, 0.2)
X_source <- rbinom(n, 1, 0.5)     
Z_source <- rbinom(n, 1, 1/(1+exp(-(5*X_source + W_source + u1_s))))                             
Y_source <- 0.5*Z_source + 0.5*W_source + u1_s 

SimulatedData_source <- data.frame(X_source,Z_source,Y_source,W_source, u1_s)  

# 2) Simulate the target data for the diagram #3
set.seed(5)
u1_t <- rnorm(n, 0, 1)
#S <- rnorm(n,0,2)

#W_target <- rbinom(n, 1, 1/(1+exp(-S)))
W_target <- rbinom(n, 1, 0.8)
X_target <- rbinom(n, 1, 0.5)     
Z_target <- rbinom(n, 1, 1/(1+exp(-(5*X_target + W_target + u1_t + 10*W_target*X_target))))
Y_target <- 0.5*Z_target + 0.5*W_target + u1_t

SimulatedData_target <- data.frame(X_target,Z_target,Y_target,W_target, u1_t)

# 3) Calculate ATE for both populations
# 3-1) Source
#model_ref_source <- lm(Y_source ~ Z_source + W_source + u1_s, data = SimulatedData_source)
model_ref_source <- lm(Y_source ~ X_source, data = SimulatedData_source)
summary(model_ref_source)

library("ivreg")
model_ref_source_2SLS <- ivreg(Y_source ~ Z_source + W_source + u1_s | X_source + W_source + u1_s, data = SimulatedData_source)
summary(model_ref_source_2SLS)

# 3-2) Target
#model_ref_target <- lm(Y_target ~ Z_target + W_target + u1_t, data = SimulatedData_target)
model_ref_target <- lm(Y_target ~ X_target, data = SimulatedData_target)
summary(model_ref_target)

model_ref_target_2SLS <- ivreg(Y_target ~ Z_target + W_target + u1_t | X_target + W_target + u1_t, data = SimulatedData_target)
summary(model_ref_target_2SLS)

# 4) Transportability
# 5-1) P(y|do(x), w)
P_y_x0_w0 <- aggregate(Y_source ~ X_source + W_source, data=SimulatedData_source, mean)[1,3]
P_y_x1_w0 <- aggregate(Y_source ~ X_source + W_source, data=SimulatedData_source, mean)[2,3]
P_y_x0_w1 <- aggregate(Y_source ~ X_source + W_source, data=SimulatedData_source, mean)[3,3]
P_y_x1_w1 <- aggregate(Y_source ~ X_source + W_source, data=SimulatedData_source, mean)[4,3]

# 5-2) P*(W)
P_w0 <- table(W_target)[1]/n
P_w1 <- table(W_target)[2]/n

# 5-3) P*(y|do(X=0)) and P*(y|do(X=1))
P_y_x0 <- P_y_x0_w0*P_w0 + P_y_x0_w1*P_w1
P_y_x1 <- P_y_x1_w0*P_w0 + P_y_x1_w1*P_w1

# 5-4) ATE in target
P_y_x1-P_y_x0

# 6) W-specific effects
# 6-1) OLS
model_ref_target_Wspecific <- lm(Y_target ~ X_target + W_target + X_target*W_target, data = SimulatedData_target)
summary(model_ref_target_Wspecific)

# 6-2) TP
P_y_x1_w0-P_y_x0_w0
P_y_x1_w1-P_y_x0_w1


################################################################################3

# case 2) When the interaction is between X and U1

rm(list=ls())
n <- 1000000
set.seed(5)

# 1) Simulate the source data
u1_s <- rnorm(n, 0, 1)  # Confounding between Z and Y

W_source <- rbinom(n, 1, 0.2)
X_source <- rbinom(n, 1, 0.5)     
Z_source <- rbinom(n, 1, 1/(1+exp(-(5*X_source + W_source + u1_s))))                             
Y_source <- 0.5*Z_source + 0.5*W_source + u1_s 

SimulatedData_source <- data.frame(X_source,Z_source,Y_source,W_source, u1_s)  

# 2) Simulate the target data
set.seed(5)
u1_t <- rnorm(n, 0, 1)
#S <- rnorm(n,0,2)

#W_target <- rbinom(n, 1, 1/(1+exp(-S)))
W_target <- rbinom(n, 1, 0.8)
X_target <- rbinom(n, 1, 0.5)     
Z_target <- rbinom(n, 1, 1/(1+exp(-(5*X_target + W_target + u1_t + 10*u1_t*X_target))))
Y_target <- 0.5*Z_target + 0.5*W_target + u1_t

SimulatedData_target <- data.frame(X_target,Z_target,Y_target,W_target, u1_t)

# 3) Calculate ATE for both populations
# 3-1) Source
#model_ref_source <- lm(Y_source ~ Z_source + W_source + u1_s, data = SimulatedData_source)
model_ref_source <- lm(Y_source ~ X_source, data = SimulatedData_source)
summary(model_ref_source)

library("ivreg")
model_ref_source_2SLS <- ivreg(Y_source ~ Z_source + W_source + u1_s | X_source + W_source + u1_s, data = SimulatedData_source)
summary(model_ref_source_2SLS)

# 3-2) Target
#model_ref_target <- lm(Y_target ~ Z_target + W_target + u1_t, data = SimulatedData_target)
model_ref_target <- lm(Y_target ~ X_target, data = SimulatedData_target)
summary(model_ref_target)

model_ref_target_2SLS <- ivreg(Y_target ~ Z_target + W_target + u1_t | X_target + W_target + u1_t, data = SimulatedData_target)
summary(model_ref_target_2SLS)

# 4) Transportability
# 4-1) P(y|do(x), w)
P_y_x0_w0 <- aggregate(Y_source ~ X_source + W_source, data=SimulatedData_source, mean)[1,3]
P_y_x1_w0 <- aggregate(Y_source ~ X_source + W_source, data=SimulatedData_source, mean)[2,3]
P_y_x0_w1 <- aggregate(Y_source ~ X_source + W_source, data=SimulatedData_source, mean)[3,3]
P_y_x1_w1 <- aggregate(Y_source ~ X_source + W_source, data=SimulatedData_source, mean)[4,3]

# 4-2) P*(W)
P_w0 <- table(W_target)[1]/n
P_w1 <- table(W_target)[2]/n

# 4-3) P*(y|do(X=0)) and P*(y|do(X=1))
P_y_x0 <- P_y_x0_w0*P_w0 + P_y_x0_w1*P_w1
P_y_x1 <- P_y_x1_w0*P_w0 + P_y_x1_w1*P_w1

# 4-4) ATE in target
P_y_x1-P_y_x0

# 5) Create Proxy for U1
V_source <- rbinom(n, 1, 1/(1+exp(-5*u1_s)))
Z_source <- rbinom(n, 1, 1/(1+exp(-(5*X_source + W_source + V_source))))                             
Y_source <- 0.5*Z_source + 0.5*W_source + V_source 
SimulatedData_source <- data.frame(X_source,Z_source,Y_source,W_source, u1_s, V_source)
cor(V_source, u1_s, method = "spearman")

V_target <- rbinom(n, 1, 1/(1+exp(-u1_t)))
Z_target <- rbinom(n, 1, 1/(1+exp(-(5*X_target + W_target + V_target + 10*V_target*X_target))))
Y_target <- 0.5*Z_target + 0.5*W_target + V_target
SimulatedData_target <- data.frame(X_target,Z_target,Y_target,W_target, u1_t, V_target)

# 6) V-specific effects
# 6-1) OLS
model_ref_target_Vspecific <- lm(Y_target ~ X_target + V_target + X_target*V_target, data = SimulatedData_target)
summary(model_ref_target_Vspecific)

# 6-2) TP
# 6-2-1) P(Y|do(x), W, V)
P_Y_x0_w0_v0 <- aggregate(Y_source ~ X_source + W_source + V_source, data=SimulatedData_source, mean)[1,4]
P_Y_x1_w0_v0 <- aggregate(Y_source ~ X_source + W_source + V_source, data=SimulatedData_source, mean)[2,4]
P_Y_x0_w1_v0 <- aggregate(Y_source ~ X_source + W_source + V_source, data=SimulatedData_source, mean)[3,4]
P_Y_x1_w1_v0 <- aggregate(Y_source ~ X_source + W_source + V_source, data=SimulatedData_source, mean)[4,4]

P_Y_x0_w0_v1 <- aggregate(Y_source ~ X_source + W_source + V_source, data=SimulatedData_source, mean)[5,4]
P_Y_x1_w0_v1 <- aggregate(Y_source ~ X_source + W_source + V_source, data=SimulatedData_source, mean)[6,4]
P_Y_x0_w1_v1 <- aggregate(Y_source ~ X_source + W_source + V_source, data=SimulatedData_source, mean)[7,4]
P_Y_x1_w1_v1 <- aggregate(Y_source ~ X_source + W_source + V_source, data=SimulatedData_source, mean)[8,4]

# 6-2-2) P*(W|do(x))
P_w0_x0 <- table(W_target, X_target)[1,1]/table(X_target)[1]
P_w1_x0 <- table(W_target, X_target)[2,1]/table(X_target)[1]
P_w0_x1 <- table(W_target, X_target)[1,2]/table(X_target)[2]
P_w1_x1 <- table(W_target, X_target)[2,2]/table(X_target)[2]

# 6-2-3) V specific effects
P_Y_X0_v0 <- P_Y_x0_w0_v0*P_w0_x0 + P_Y_x0_w1_v0*P_w1_x0
P_Y_X1_v0 <- P_Y_x1_w0_v0*P_w0_x1 + P_Y_x1_w1_v0*P_w1_x1

P_Y_X0_V1 <- P_Y_x0_w0_v1*P_w0_x0 + P_Y_x0_w1_v1*P_w1_x0
P_Y_X1_V1 <- P_Y_x1_w0_v1*P_w0_x1 + P_Y_x1_w1_v1*P_w1_x1

P_Y_X1_v0-P_Y_X0_v0
P_Y_X1_V1-P_Y_X0_V1
