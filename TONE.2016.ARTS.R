# Load library
library(DJL)

# load dataset / parameters
d <- dataset.airplane.2017
n <- subset(d, select = 1)
x <- data.frame(FREW = rep(1, 28))
y <- subset(d, select = 3:7)
t <- subset(d, select = 2)

# Table 1. Dataset
table.1 <- data.frame(d[, 1:4], PFE = round(exp(1)^d$PFE, 3), d[, 6:7])
print(table.1[t <= 2007, ], row.names = F)

# Table 2. Local Roc of SOA airplanes at the frontier year of 2007
m1 <- roc.dea(x, y, t, 2007, "vrs", "o", "min")
soa.r <- !is.na(m1$roc_past)
soa.t <- !is.na(m1$roc_local)
table.2 <- data.frame(SOA.Airplane = n[soa.t, ], 
                      Local.RoC = m1$roc_local[soa.t,],
                      Dtd.airplanes = apply(m1$lambda_t[soa.r, soa.t], 
                                            2, 
                                            function(x) paste(n[soa.r,][which(x > 0)], collapse = ", ")))
print(table.2, row.names = F)

# Table 3. Four airplanes concepts in 2007
table.3 <- data.frame(Concept = 1:4, table.1[t > 2007, 3:7], Delivery.target = c(2010, 2010, 2013, 2015))
print(table.3, row.names = F)

# Table 4. Results summary
m2 <- target.arrival.dea(x, y, t, 2007, "vrs", "o", "min")
table.4 <- data.frame(Concept = n[t > 2007, 1], 
                      Reference.airplane = apply(m2$lambda_t[t > 2007, soa.t], 
                                                 1, 
                                                 function(x) paste(n[soa.t,][which(x > 0)], collapse = ", ")), 
                      Planned.EIS = table.3$Delivery.target, 
                      Estimated.EIS = round(m2$arrival_seg[25:28, ], 2),
                      Delayed.EIS = c(2012, 2014, 2014, 2017))
print(table.4, row.names = F)
