install.packages("ggpubr")
library(tidyverse)
library(gridExtra)
library(knitr)
library(plotly)
library(Hmisc)
library(ggpubr)

#load data
ne_commercial_landing <- read.csv("Data/NRS 568 Project - All States.csv", header = TRUE, stringsAsFactors = FALSE,
         sep = ",")

#format number
"formatColumns" <-
  function(data, digits)
  {
    "%,%" <- function(x,y)paste(x,y,sep="")
    nms <- names(data)
    nc <- ncol(data)
    nd <- length(digits)
    if(nc!=nd) 
      stop("Argument 'digits' must be vector of length " %,% 
             nc %,% ", the number of columns in 'data'.")
    out <- as.data.frame(sapply(1:nc, 
                                FUN=function(x, d, Y)
                                  format(Y[,x], digits=d[x]), Y=tbl, d=digits))
    if(!is.null(nms)) names(out) <- nms
    out
  }

ne_commercial_landing_clean <- ne_commercial_landing %>% 
  select(State, Year, Species, Metric.Tons, Pounds, X.) %>%
  filter(!State == "Total", !State == "")

#unique(ne_commercial_landing_clean$State)

#change column name
colnames(ne_commercial_landing_clean)[6] <- "Unit.Price"

#commercial landing in Connecticut (CT)
ct_landing <- ne_commercial_landing_clean %>%
  filter(!State == "RI", !State == "MA", !State == "ME")

#plot based on metric ton for each species
ct_landing_ton <- ct_landing %>%
  select(Year, Species, Metric.Tons)

ct_landing_ton$Year <- as.factor(ct_landing_ton$Year)  

ggplot(ct_landing_ton, aes(x = Year, y = Metric.Tons, fill = Species)) + 
  geom_bar(stat = "identity") + facet_wrap(~Species, scales = "free_y") +
  theme_linedraw() + theme(legend.position = "right") +
  labs(title = "CT commercial landing 2000 - 2016", 
       x = "Year", y = "in Metric Tons") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
  theme(axis.text.y = element_text(hjust = 1, size = 9))

#commercial landing in Rhode Island (RI)
ri_landing <- ne_commercial_landing_clean %>%
  filter(!State == "CT", !State == "MA", !State == "ME")

#plot based on metric ton for each species
ri_landing_ton <- ri_landing %>%
  select(Year, Species, Metric.Tons)

ri_landing_ton$Year <- as.factor(ri_landing_ton$Year)  

ggplot(ri_landing_ton, aes(x = Year, y = Metric.Tons, fill = Species)) + 
  geom_bar(stat = "identity") + facet_wrap(~Species, scales = "free_y") +
  theme_linedraw() + theme(legend.position = "right") +
  labs(title = "RI commercial landing 2000 - 2016", 
       x = "Year", y = "in Metric Tons") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
  theme(axis.text.y = element_text(hjust = 1, size = 9))

#commercial landing in Massachusetts (MA)
ma_landing <- ne_commercial_landing_clean %>%
  filter(!State == "CT", !State == "RI", !State == "ME")

#plot based on metric ton for each species
ma_landing_ton <- ma_landing %>%
  select(Year, Species, Metric.Tons)

ma_landing_ton$Year <- as.factor(ma_landing_ton$Year)  

ggplot(ma_landing_ton, aes(x = Year, y = Metric.Tons, fill = Species)) + 
  geom_bar(stat = "identity") + facet_wrap(~Species, scales = "free_y") +
  theme_linedraw() + theme(legend.position = "right") +
  labs(title = "MA commercial landing 2000 - 2016", 
       x = "Year", y = "in Metric Tons") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
  theme(axis.text.y = element_text(hjust = 1, size = 9))

#commercial landing in Maine (ME)
me_landing <- ne_commercial_landing_clean %>%
  filter(!State == "CT", !State == "RI", !State == "MA")

#plot based on metric ton for each species
me_landing_ton <- me_landing %>%
  select(Year, Species, Metric.Tons)

me_landing_ton$Year <- as.factor(me_landing_ton$Year)  

ggplot(me_landing_ton, aes(x = Year, y = Metric.Tons, fill = Species)) + 
  geom_bar(stat = "identity") + facet_wrap(~Species, scales = "free_y") +
  theme_linedraw() + theme(legend.position = "right") +
  labs(title = "ME commercial landing 2000 - 2016", 
       x = "Year", y = "in Metric Tons") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
  theme(axis.text.y = element_text(hjust = 1, size = 9))

#time series plot base on each species

# Cod, Atlantic
cod_atlantic <- ne_commercial_landing_clean %>%
  filter(!Species == "FLOUNDER, WINTER", !Species == "GOOSEFISH",
         !Species == "LOBSTER, AMERICAN", !Species == "MACKEREL, ATLANTIC",
         !Species == "MENHADEN", !Species == "SCALLOP, SEA", 
         !Species == "SEA BASS, BLACK", !Species == "SQUID, LONGFIN",
         !Species == "SQUID, NORTHERN SHORTFIN")

ggplot(cod_atlantic, aes(x = Year, y = Metric.Tons, fill = State)) + 
  geom_bar(stat = "identity") +
  theme_linedraw() + theme(legend.position = "right") +
  labs(title = "Comparison landing Between Each State for Atlantic Cod", 
       x = "Year", y = "in Metric Tons") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
  theme(axis.text.y = element_text(hjust = 1, size = 9))

#Flounder, Winter
flounder_winter <- ne_commercial_landing_clean %>%
  filter(!Species == "COD, ATLANTIC", !Species == "GOOSEFISH",
         !Species == "LOBSTER, AMERICAN", !Species == "MACKEREL, ATLANTIC",
         !Species == "MENHADEN", !Species == "SCALLOP, SEA", 
         !Species == "SEA BASS, BLACK", !Species == "SQUID, LONGFIN",
         !Species == "SQUID, NORTHERN SHORTFIN")

ggplot(flounder_winter, aes(x = Year, y = Metric.Tons, fill = State)) + 
  geom_bar(stat = "identity") +
  theme_linedraw() + theme(legend.position = "right") +
  labs(title = "Comparison landing Between Each State for Winter Flounder", 
       x = "Year", y = "in Metric Tons") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
  theme(axis.text.y = element_text(hjust = 1, size = 9))

#Goosefish
Goosefish <- ne_commercial_landing_clean %>%
  filter(!Species == "COD, ATLANTIC", !Species == "FLOUNDER, WINTER",
         !Species == "LOBSTER, AMERICAN", !Species == "MACKEREL, ATLANTIC",
         !Species == "MENHADEN", !Species == "SCALLOP, SEA", 
         !Species == "SEA BASS, BLACK", !Species == "SQUID, LONGFIN",
         !Species == "SQUID, NORTHERN SHORTFIN")

ggplot(Goosefish, aes(x = Year, y = Metric.Tons, fill = State)) + 
  geom_bar(stat = "identity") +
  theme_linedraw() + theme(legend.position = "right") +
  labs(title = "Comparison landing Between Each State for Goosefish", 
       x = "Year", y = "in Metric Tons") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
  theme(axis.text.y = element_text(hjust = 1, size = 9))

#LOBSTER, AMERICAN
lobster <- ne_commercial_landing_clean %>%
  filter(!Species == "COD, ATLANTIC", !Species == "FLOUNDER, WINTER",
         !Species == "GOOSEFISH", !Species == "MACKEREL, ATLANTIC",
         !Species == "MENHADEN", !Species == "SCALLOP, SEA", 
         !Species == "SEA BASS, BLACK", !Species == "SQUID, LONGFIN",
         !Species == "SQUID, NORTHERN SHORTFIN")

ggplot(lobster, aes(x = Year, y = Metric.Tons, fill = State)) + 
  geom_bar(stat = "identity") +
  theme_linedraw() + theme(legend.position = "right") +
  labs(title = "Comparison landing Between Each State for American Lobster", 
       x = "Year", y = "in Metric Tons") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
  theme(axis.text.y = element_text(hjust = 1, size = 9))

# MACKEREL, ATLANTIC
mackerel <- ne_commercial_landing_clean %>%
  filter(!Species == "COD, ATLANTIC", !Species == "FLOUNDER, WINTER",
         !Species == "GOOSEFISH", !Species == "LOBSTER, AMERICAN",
         !Species == "MENHADEN", !Species == "SCALLOP, SEA", 
         !Species == "SEA BASS, BLACK", !Species == "SQUID, LONGFIN",
         !Species == "SQUID, NORTHERN SHORTFIN")

ggplot(mackerel, aes(x = Year, y = Metric.Tons, fill = State)) + 
  geom_bar(stat = "identity") +
  theme_linedraw() + theme(legend.position = "right") +
  labs(title = "Comparison landing Between Each State for Atlantic Mackerel", 
       x = "Year", y = "in Metric Tons") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
  theme(axis.text.y = element_text(hjust = 1, size = 9))

# MENHADEN
menhaden <- ne_commercial_landing_clean %>%
  filter(!Species == "COD, ATLANTIC", !Species == "FLOUNDER, WINTER",
         !Species == "GOOSEFISH", !Species == "LOBSTER, AMERICAN",
         !Species == "MACKEREL, ATLANTIC", !Species == "SCALLOP, SEA", 
         !Species == "SEA BASS, BLACK", !Species == "SQUID, LONGFIN",
         !Species == "SQUID, NORTHERN SHORTFIN")

ggplot(menhaden, aes(x = Year, y = Metric.Tons, fill = State)) + 
  geom_bar(stat = "identity") +
  theme_linedraw() + theme(legend.position = "right") +
  labs(title = "Comparison landing Between Each State for Menhaden", 
       x = "Year", y = "in Metric Tons") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
  theme(axis.text.y = element_text(hjust = 1, size = 9))

# SCALLOP, SEA
scallop <- ne_commercial_landing_clean %>%
  filter(!Species == "COD, ATLANTIC", !Species == "FLOUNDER, WINTER",
         !Species == "GOOSEFISH", !Species == "LOBSTER, AMERICAN",
         !Species == "MACKEREL, ATLANTIC", !Species == "MENHADEN", 
         !Species == "SEA BASS, BLACK", !Species == "SQUID, LONGFIN",
         !Species == "SQUID, NORTHERN SHORTFIN")

ggplot(scallop, aes(x = Year, y = Metric.Tons, fill = State)) + 
  geom_bar(stat = "identity") +
  theme_linedraw() + theme(legend.position = "right") +
  labs(title = "Comparison landing Between Each State for Sea Scallop", 
       x = "Year", y = "in Metric Tons") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
  theme(axis.text.y = element_text(hjust = 1, size = 9))



# SEA BASS, BLACK
seabass <- ne_commercial_landing_clean %>%
  filter(!Species == "COD, ATLANTIC", !Species == "FLOUNDER, WINTER",
         !Species == "GOOSEFISH", !Species == "LOBSTER, AMERICAN",
         !Species == "MACKEREL, ATLANTIC", !Species == "MENHADEN", 
         !Species == "SCALLOP, SEA", !Species == "SQUID, LONGFIN",
         !Species == "SQUID, NORTHERN SHORTFIN")

ggplot(seabass, aes(x = Year, y = Metric.Tons, fill = State)) + 
  geom_bar(stat = "identity") +
  theme_linedraw() + theme(legend.position = "right") +
  labs(title = "Comparison landing Between Each State for Black Sea Bass", 
       x = "Year", y = "in Metric Tons") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
  theme(axis.text.y = element_text(hjust = 1, size = 9))

# SQUID, LONGFIN
squid_longfin <- ne_commercial_landing_clean %>%
  filter(!Species == "COD, ATLANTIC", !Species == "FLOUNDER, WINTER",
         !Species == "GOOSEFISH", !Species == "LOBSTER, AMERICAN",
         !Species == "MACKEREL, ATLANTIC", !Species == "MENHADEN", 
         !Species == "SCALLOP, SEA", !Species == "SEA BASS, BLACK",
         !Species == "SQUID, NORTHERN SHORTFIN")

ggplot(squid_longfin, aes(x = Year, y = Metric.Tons, fill = State)) + 
  geom_bar(stat = "identity") +
  theme_linedraw() + theme(legend.position = "right") +
  labs(title = "Comparison landing Between Each State for Longfin Squid", 
       x = "Year", y = "in Metric Tons") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
  theme(axis.text.y = element_text(hjust = 1, size = 9))


# SQUID, NORTHERN SHORTFIN
squid_shortfin <- ne_commercial_landing_clean %>%
  filter(!Species == "COD, ATLANTIC", !Species == "FLOUNDER, WINTER",
         !Species == "GOOSEFISH", !Species == "LOBSTER, AMERICAN",
         !Species == "MACKEREL, ATLANTIC", !Species == "MENHADEN", 
         !Species == "SCALLOP, SEA", !Species == "SEA BASS, BLACK",
         !Species == "SQUID, LONGFIN")

ggplot(squid_shortfin, aes(x = Year, y = Metric.Tons, fill = State)) + 
  geom_bar(stat = "identity") +
  theme_linedraw() + theme(legend.position = "right") +
  labs(title = "Comparison landing Between Each State for Northern Shortfin Squid", 
       x = "Year", y = "in Metric Tons") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
  theme(axis.text.y = element_text(hjust = 1, size = 9))

ggplot(ne_commercial_landing_clean, aes(x = Year, y = Metric.Tons)) + 
  geom_line(aes(color = Species)) + theme_classic() + 
  theme(legend.position = "right") +
  labs(title = "Comparison all States Base On Species", x = "Year", 
       y = "In Metric Tons (g)", color = "Species") +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + 
  theme(axis.text.x = element_text(size = 9)) +
  theme(axis.text.y = element_text(size = 9)) 

