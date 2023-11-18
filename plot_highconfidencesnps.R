# Comparing high-confidence SNPs in TAD1, TAD1_C184A, and HsAID

library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)

df <- read_excel('highconfidencesnps_temptrial.xlsx')


df$snp_id <- paste(df$ref, df$pos_rpob, df$alt, sep="_")
head(df)


# Counts of snp_id for TAD1, TAD1_C184A, and HsAID
ggplot(df, aes(x = snp_id)) +
  geom_bar(aes(fill=construct))+
  facet_wrap(~temp)


# Count constructs (idk why I did this)
df %>% 
  count(construct)


# Counts of snp_id for TAD1 only
tad1 <- df %>%
  filter(construct == "TAD1")
head(tad1)

ggplot(tad1, aes(x = snp_id)) +
  geom_bar()


#Counts of snp_id for TAD1_C184A only
c184a <- df %>%
  filter(construct == "TAD1_C184A")
head(c184a)

ggplot(c184a, aes(x = snp_id)) +
  geom_bar()


# Counts of snp_id for HsAID only
hsaid <- df %>%
  filter(construct == "HsAID")
head(hsaid)

ggplot(hsaid, aes(x = snp_id)) +
  geom_bar()


#Filter out conserved SNPs (1467, 1867, 1941)
df2 <- df %>%
  filter(pos_rpob != 1467 & pos_rpob != 1867 & pos_rpob != 1941)
head(df2)


# Counts of snp_id for filtered TAD1, TAD1_C184A, and HsAID
ggplot(df2, aes(x = snp_id)) +
  geom_bar(aes(fill=construct))+
  facet_wrap(~temp)


# Group by SNP type from filtered SNPs
df2$snp_type <- paste(df2$ref, df2$alt, sep="_")
head(df2)


# Counts of filtered snp_type, grouped by temperature
ggplot(df2, aes(x = snp_type)) +
  geom_bar(aes(fill=construct))+
  facet_wrap(~temp)


# Isolate HsAID and group by snp_type
hsaid_nonconserved <- hsaid %>%
  filter(pos_rpob != 1467 & pos_rpob != 1867 & pos_rpob != 1941)
head(hsaid_nonconserved)

# Isolate TAD1 and group by snp_type for 23C only
tad1_nonconserved <- tad1 %>%
  filter(pos_rpob != 1467 & pos_rpob != 1867 & pos_rpob != 1941 & temp == 23)
head(tad1_nonconserved)

# TAD1 frequency: CT and GA mutations / total mutations, at 23C

