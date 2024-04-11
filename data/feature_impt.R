library(dplyr)
library(tidyverse)


lines <- readLines("Downloads/data/impt.txt")
impt <- lapply(lines,function(line) strsplit(line,", ")[[1]])
lines2 <- readLines("Downloads/data/name.txt")
names<- lapply(lines2,function(line) strsplit(line,", ")[[1]])

name1<- names[[1]]
n1 <- impt[[1]]
n2 <- impt[[2]]
n3 <- impt[[3]]
n4 <- impt[[4]]
n5 <- impt[[5]]
n6 <- impt[[6]]
n7 <- impt[[7]]
n8 <- impt[[8]]

df <- data.frame(name1 = name1,
                 n1 = n1,
                 n2 = n2,
                 n3 = n3,
                 n4 = n4,
                 n5 = n5,
                 n6 = n6,
                 n7 = n7,
                 n8 = n8)

# Transpose the dataframe and gather the columns
transposed_df <- df %>%
  pivot_longer(cols = -name1, names_to = "category", values_to = "value") %>%
  mutate(category = sub("n", "", category),
         category = paste0("t+", category)) %>%
  pivot_wider(names_from = name1, values_from = value)

name3 <- sort(names(transposed_df))


"rinvbf"   + "rinvchi" ->"Private Investment"
"RCONS" + "rconhh" +  "rconsnp" -> "Consumption"
"CPI" + "P" -> "CPI"
"IPT"-> "Industrial Production Index"

"HSTARTS" + "rinvresid" -> "Housing"
"LFC" + "LFPART" + "POP"+" OPH"+"RUC" -> "Labour Market/Productivity"
"M1" -> "Monetary"
"WSD" +"OLI" +"PINTI" +"PINTPAID"+"PROPI"+"PTAX" ->"Personal Income"
"PTAX"+ "rinvbf" +"rinvchi" + "rinvresid" ->"Investment/Saving"
"RNX"  + "REX" + "RIMP" ->"Export/Import"
"ROUTPUT" -> "GDP"



convert_to_numeric <- function(x) {
  if (!identical(x, 'category')) {
    as.numeric(x)
  } else {
    x
  }
}

numeric_df <- transposed_df %>%
  mutate_at(vars(-category), convert_to_numeric)

# Mutate the columns to add them together or rename them based on the provided mapping
mutated_df <- numeric_df %>%
  mutate(`Private Investment` = rinvbf + rinvchi,
         `Consumption` = RCONS + rconhh + rconsnp,
         `CPI` = CPI + P,
         `Industrial Production Index` = IPT,
         `Housing` = HSTARTS + rinvresid,
         `Labour Market/Productivity` = LFC + LFPART + POP + OPH + RUC,
         `Monetary` = M1,
         `Personal Income` = WSD + OLI + PINTI + PINTPAID + PROPI + PTAX,
         `Investment/Saving` = PTAX + rinvbf + rinvchi + rinvresid +RATESAV,
         `Export/Import` = RNX + REX + RIMP,
         `Govt Expenditure` = RG +RGF +RGSL,
         `GDP` = ROUTPUT) %>%
  select(-rinvbf, -rinvchi, -RCONS, -rconhh, -rconsnp, -CPI, -P, -IPT, 
         -HSTARTS, -rinvresid, -LFC, -LFPART, -POP, -OPH, -RUC, -M1, 
         -WSD, -OLI, -PINTI, -PINTPAID, -PROPI, -PTAX, -RNX, -REX, -RIMP, -ROUTPUT,-RATESAV
         ,-RG,-RGF,-RGSL)


long_df <- tidyr::pivot_longer(mutated_df, cols = -category, names_to = "t", values_to = "value")

# Plot stacked bar chart
ggplot(long_df, aes(x = category, y = value, fill = t)) +
  geom_bar(stat = "identity") +
  labs(x = "Category", y = "Feature Importance", fill = "Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

