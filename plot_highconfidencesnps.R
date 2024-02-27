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


# Count constructs
# df %>% 
#   count(construct)


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
#hsaid_nonconserved <- hsaid %>%
  #filter(pos_rpob != 1467 & pos_rpob != 1867 & pos_rpob != 1941)
#head(hsaid_nonconserved)

# Isolate TAD1 and group by snp_type for 23C only
#tad1_nonconserved <- tad1 %>%
  #filter(pos_rpob != 1467 & pos_rpob != 1867 & pos_rpob != 1941 & temp == 23)
#head(tad1_nonconserved)

# Is it a relevant (CT or GA) mutation?
df2$is_relevant_mutation <- ifelse(df2$snp_type == 'C_T' | df2$snp_type == 'G_A', TRUE, FALSE)
head(df2)

TAD1_mut_freq <- sum(df2$is_relevant_mutation == TRUE & df2$construct == 'TAD1')/sum(df2$construct == 'TAD1')
TAD1_C184A_mut_freq <- sum(df2$is_relevant_mutation == TRUE & df2$construct == 'TAD1_C184A')/sum(df2$construct == 'TAD1_C184A')

# Making data frame with the frequencies
df3 <- tibble(construct  = c("TAD1", "TAD1_C184A"),
                   mut_frequency = c(TAD1_mut_freq, TAD1_C184A_mut_freq)
)

# Plotting frequencies
ggplot(df3, aes(x = construct)) +
  geom_col(aes(y = mut_frequency)) +
  xlab("Construct") + ylab("Mutation Frequency")