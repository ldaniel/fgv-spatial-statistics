# analysing missing values and other strange conditions -----------------------

# looking for NAs
mytable_na_cols <- sapply(target@data, function(x) sum(is.na(x)))

# looking for empty values
mytable_empty_cols <-  sapply(target@data, function(x) table(as.character(x) == "" | as.character(x) == " ")["TRUE"])

# there are 33 cities in which the variables POP_RUR, POP_URB, POP_FEM, 
# POP_MAS, POP_TOT have zero in their values, however the column POP_94
# has value greater than zero. so, the strategy is to produce the value
# for the missing columns based on the average values for the rest of
# cities in Minas Gerais.

# converting columns from text to numeric
target$POP_RUR <- as.numeric(as.character(target$POP_RUR))
target$POP_URB <- as.numeric(as.character(target$POP_URB))
target$POP_FEM <- as.numeric(as.character(target$POP_FEM))
target$POP_MAS <- as.numeric(as.character(target$POP_MAS))

# getting rural population and female's mean ratio
target.dataframe <- as(target, "data.frame")
target.dataframe <- filter(target.dataframe, POP_RUR != 0)
poprur.ratio <- mean(target.dataframe$POP_RUR) / mean(target.dataframe$POP_URB)
pophm.ratio  <- mean(target.dataframe$POP_MAS) / mean(target.dataframe$POP_FEM)
popfem.ratio <- mean(target.dataframe$POP_FEM) / mean(target.dataframe$POP_TOT)

# adjusting rows with zero in POP_RUR, POP_URB, POP_FEM, POP_MAS and POP_TOT
target$POP_RUR <- ifelse(target$POP_RUR == 0, 
                         round(target$POP_94 * poprur.ratio, digits = 0),
                         round(target$POP_RUR, digits = 0))
target$POP_URB <- ifelse(target$POP_URB == 0, 
                         round(target$POP_94 - target$POP_RUR, digits = 0),
                         round(target$POP_URB, digits = 0))
target$POP_FEM <- ifelse(target$POP_FEM == 0, 
                         round(target$POP_94 * popfem.ratio, digits = 0),
                         round(target$POP_FEM, digits = 0))
target$POP_MAS <- ifelse(target$POP_MAS == 0, 
                         round(target$POP_94 - target$POP_FEM, digits = 0),
                         round(target$POP_MAS, digits = 0))
target$POP_TOT <- ifelse(target$POP_TOT == 0, 
                         round(target$POP_94, digits = 0),
                         round(target$POP_TOT, digits = 0))