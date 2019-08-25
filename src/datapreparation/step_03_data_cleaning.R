# analysing missing values and other strange conditions -----------------------

# looking for NAs
mytable_na_cols <- sapply(target@data, function(x) sum(is.na(x)))

# looking for empty values
mytable_empty_cols <-  sapply(target@data, function(x) table(as.character(x) == "" | as.character(x) == " ")["TRUE"])

# there is 33 cities in which the variables POP_RUR, POP_URB, POP_FEM, 
# POP_MAS, POP_TOT have zero in their values, however the column POP_94
# has value greater than zero. so, the strategy is to produce the value
# for the missing columns based on the average values for the rest of
# cities in Minas Gerais.

# TO-DO: tratar as colunas com valor zero

target$POP_RUR <- as.numeric(as.character(target@POP_RUR))
