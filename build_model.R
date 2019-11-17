# Author: Sunny Singh
library(ordinal)

ICS45C_data <- read.csv('data/ICS45C-Full.csv')
ICS45C_data$Grade <- factor(ICS45C_data$Grade, c("F", "D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+"),
                           ordered = TRUE)
ICS45C_data <- ICS45C_data[complete.cases(ICS45C_data),]
ICS45C_data$MTPct <- ICS45C_data$MT / ICS45C_data$MidtermOutOf
ICS45C_data$FinalPct <- ICS45C_data$Final / ICS45C_data$FinalOutOf
ICS45C_data$TotalPct <- as.numeric(sub("%", "", ICS45C_data$TotalPct)) / 100
ICS45C_data$QtrLec <- paste(ICS45C_data$Quarter, ICS45C_data$LectureRef)

grade_model <- clmm(Grade ~ Proj0 + I(Proj1 + Proj2 + Proj3 + Proj4) + MTPct + (MTPct|QtrLec), data = ICS45C_data)

save(grade_model, file = "ICS45C_clmm.Rmodel", precheck = TRUE)
