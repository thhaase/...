library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(easystats)
library(ggdist)
library(gt) 
library(tools)
library(gplots)
setwd("~/Insync/thhaase.soz@gmail.com/GoogleDrive/_1_Projects/_BA_Thesis/3_Analyse_script")


# transform data ----
## load and filter ----
data <- read.csv("data/data.csv") %>% 
  select(-X) %>% 
  mutate(source = factor(source))

data <- data %>% 
  filter(language == "english") %>% # only english articles...
  filter(!is.na(abstract)) %>%      # ...with abstracts...
  filter(year < 2024)               # ...before 2024 (year is not over yet)

# oops... data only available after 1992...
plot_desc_abstracts_per_year <- data %>%
  group_by(year) %>%
  summarise(count = n(), .groups = "drop") %>% 
  ggplot(aes(x = year, y = count)) +
  geom_line(size = 1, 
            color = "red") +  
  geom_vline(xintercept = 1992, 
             linetype = "dashed", 
             color = "blue", size = 1) +  
  geom_text(aes(x = 1992, y = 1, 
                label = "1992", 
                vjust = 1, hjust = -0.2), 
            color = "blue",
            size = 4.5) +  
  labs(title = "Articles with Abstracts Per Year",
       x = "Year",
       y = "Number of Rows") +
  theme_minimal()
plot_desc_abstracts_per_year

ggsave("plots/desc_abstracts_per_year.png", bg="white", 
       width = 8, height = 5)



plot_desc_sources_per_time <-  data %>%
  group_by(source, year) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  group_by(year) %>%
  summarise(n_sources = n_distinct(source)) %>% 
ggplot(aes(x = year, y = n_sources)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Number of Distinct Sources per Year",
       subtitle = "How many journals does the dataset contain each year",
       x = "Year",
       y = "Number of Sources") +
  geom_vline(xintercept = 1992, 
             linetype = "dashed", 
             color = "blue", size = 1) +  
  geom_text(aes(x = 1992, y = 1, 
                label = "1992", 
                vjust = 1, hjust = -0.2), 
            color = "blue",
            size = 4.5) +  
  theme_minimal()

plot_desc_sources_per_time

ggsave("plots/desc_sources_per_time.png", bg="white", 
       width = 8, height = 5)


### filter > 1992 ----
data <- data %>% 
  filter(year>1992)


## create splits ----
interval <- (max(data$year) - min(data$year)) %/% 3

split1 <- min(data$year) + interval
split2 <- split1 + interval

data_s1 <- subset(data, year <= split1)
data_s2 <- subset(data, year > split1 & year <= split2)
data_s3 <- subset(data, year > split2)


### explore splits ----

plot_split_desc_sources_per_time <-  data %>%
  group_by(source, year) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  group_by(year) %>%
  summarise(n_sources = n_distinct(source)) %>% 
  ggplot(aes(x = year, y = n_sources)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Number of Distinct Sources per Year",
       subtitle = "Splits are marked by the blue marks",
       x = "Year",
       y = "Number of Sources") +
  geom_vline(xintercept = split1, 
             linetype = "dashed", 
             color = "blue", size = 1) +  
  geom_text(aes(x = split1, y = 1, 
                label = as.character(split1), 
                vjust = 1, hjust = -0.2), 
            color = "blue",
            size = 4.5) +  
  geom_vline(xintercept = split2, 
             linetype = "dashed", 
             color = "blue", size = 1) +  
  geom_text(aes(x = split2, y = 1, 
                label = as.character(split2), 
                vjust = 1, hjust = -0.2), 
            color = "blue",
            size = 4.5) +  
  theme_minimal()

plot_split_desc_sources_per_time

ggsave("plots/split_desc_sources_per_time.png", bg="white", 
       width = 8, height = 5)



# Descriptives ----

plot_desc_top_journals <- table(data$source) %>% 
  as.data.frame() %>%
  arrange(desc(Freq)) %>% 
  head() %>% 
  rename(Journals = Var1) %>%
  mutate(Journals = toTitleCase(tolower(as.character(Journals)))) %>% 
  gt() %>% 
  tab_header(md("**Top 10 Journals**"),md("*Journals with most available Abstracts*")) %>% 
  cols_label(
    Journals = md("**Journals**"),
    Freq = md("**Abstracts**")
  )
plot_desc_top_journals

gtsave(plot_desc_top_journals, "plots/desc_Top_10_Journals.png", expand = 10) 
gtsave(plot_desc_top_journals, "plots/desc_Top_10_Journals.tex")


top_sources <- data %>%
  group_by(source) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count)) %>%
  slice_head(n = 10) %>%
  mutate(is_top_source = TRUE) %>%  # Add a flag for top sources
  select(source, is_top_source)

# Data processing and plotting
plot_desc_rows_per_year_by_source <- data %>%
  group_by(year, source) %>%
  summarise(articles = n(), .groups = "drop") %>%
  left_join(top_sources, by = "source") %>%  # Join to include the top source information
  mutate(color_group = ifelse(is.na(is_top_source), "Other", "Top Source")) %>%
  ggplot(aes(x = year, y = articles, group = source, color = color_group)) +
  geom_line(size=1) +
  labs(title = "Article Count with Abstract Per Year By Source",
       x = "Year",
       y = "Article Count") +
  theme_minimal() +
  scale_color_manual(values = c("Top Source" = "red", "Other" = "grey"),
                     labels = c("Top Source" = "Top 10 Journals with most Articles")) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 10))
plot_desc_rows_per_year_by_source

ggsave("plots/desc_rows_per_year_by_source.png", bg="white", 
       width = 8, height = 5)



# Exploratory ----


# corpus
corp <- corpus(data, text_field = "abstract")
docnames(corp) <- paste0(str_extract(data$author, "^[^,]+")," ",data$year) 

# tokens
toks <- corp %>% 
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE,
         remove_url = TRUE,
         remove_separators = TRUE,
         split_hyphens = TRUE)
# dfm
dfm <- dfm(toks)
 

dfm <- dfm %>% 
  dfm_remove(pattern = stopwords("en")) %>%  
  dfm_remove(pattern = c("univ", "j", "b")) %>%   
  dfm_remove(pattern = c("c", "inc", "u"))

#  dfm_trim(max_termfreq = .95,termfreq_type = "quantile",verbose = T) %>% 
#  dfm_trim(min_termfreq = .07,termfreq_type = "quantile",verbose = T) 




## Keyness Analyse ----

keyness1 <- textstat_keyness(dfm, target = dfm$year <= split1)
textplot_keyness(keyness1, n = 10L)


keyness2 <- textstat_keyness(dfm, target = (dfm$year > split1) & (dfm$year <= split2))
textplot_keyness(keyness2, n = 10L)


keyness3 <- textstat_keyness(dfm, target = dfm$year > split2)
textplot_keyness(keyness3, n = 10L)






# 1. Sentiments ----
## Vader ----

vader_scores <- read.csv("data/vader.csv") %>% 
  select(-X)


vader_data <- data %>% 
  left_join(vader_scores, by=c("abstract" = "text"))

names(vader_data)

vader_data %>% 
  filter(is.na(pos)) %>% 
  nrow()
# 32 vaderscores couldnt be calculated

### Correlation ----

vader_freq <- vader_data %>%
  group_by(year) %>%
  summarise(pos = mean(pos, na.rm = TRUE), 
            neg = mean(neg, na.rm = TRUE))

vader_cor<- cor.test(vader_freq$year,
                     vader_freq$pos,
                     method="spearman",
                     alternative="greater")
### Trend ----

plot_vader <- vader_freq %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = pos), color = "red",  size = 1) +
  geom_line(aes(y = neg), color = "blue", size = 1) +
  geom_smooth(aes(y=pos),color="red",span=0.5) +
  geom_smooth(aes(y=neg),color="blue",span=0.5) +
  geom_smooth(aes(y=pos),color="darkred", method="lm", span=0.5, se = F) +
  geom_smooth(aes(y=neg),color="darkblue", method="lm", span=0.5, se = F) +
  labs(title = 'Vader Sentiment Simeseries',
       subtitle = paste0("Spearman rho of positive sentiments: ", 
                         round(vader_cor$estimate,2),
                         ", p-value =", vader_cor$p.value),
       x = "Year",
       y = "Sentiment Score",
       caption = "red = positive sentiment\nblue = negative sentiment") +
  theme_minimal() 
plot_vader

ggsave("plots/sentiment_vader.png", bg="white", 
       width = 8, height = 5)


## LSD2015 ----

dict_lsd2015 <- data_dictionary_LSD2015[1:2]


lsd_corp <- corpus(data,text_field = "abstract")

# instead of text1, text2,... assign meaningful docvars
docnames(lsd_corp) <- paste0(str_extract(data$author, "^[^,]+")," ",data$year) 

lsd_toks <- lsd_corp %>% 
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_separators = TRUE,
         ) %>% 
  tokens_lookup(dictionary = dict_lsd2015, nomatch = "_UNMATCHED") %>% 
  tokens_group(groups = year)

lsd_toks %>% dfm() %>% head()

lsd_dfm <- lsd_toks %>%
  dfm() %>%
  dfm_weight(scheme = "prop") 
  
lsd_dfm %>% head()


lsd_df <- lsd_dfm %>%
  convert(to = "data.frame") %>%
  mutate(year = as.numeric(doc_id)) %>%
  select(-doc_id) %>% 
  select(year,everything())

### Correlation ----

lsd_cor <- cor.test(lsd_df$year,
                    lsd_df$positive,
                    method="spearman",
                    alternative="greater")

### Trend ----

plot_LSD2015 <- ggplot(lsd_df, aes(x = year)) +
  geom_line(aes(y = positive), color = "red",  size = 1) +
  geom_line(aes(y = negative), color = "blue", size = 1) +
  geom_smooth(aes(y=positive),color="red",span=0.75) +
  geom_smooth(aes(y=negative),color="blue",span=0.75) +
  labs(title = 'LSD2015 Sentiments',
       subtitle = paste0("Spearman rho of positive sentiments: ", 
                         round(lsd_cor$estimate,2),
                         ", p-value =", lsd_cor$p.value),
       x = "Year",
       y = "Percent of Tokenmatches",
       caption = "red = positive\nblue = negative") +
  theme_minimal()
plot_LSD2015

ggsave("plots/sentiment_lsd2015.png", bg="white", 
       width = 8, height = 5)


## Vinkers ----

dict_vinker <- dictionary(list(
  positive = c("Amazing", 
               "Assuring", 
               "Astonishing", 
               "Bright", 
               "Creative", 
               "Encouraging", 
               "Enormous", 
               "Excellent", 
               "Favourable", 
               "Groundbreaking", 
               "Hopeful", 
               "Innovative", 
               "Inspiring",
               "Inventive",
               "Novel",
               "Phenomenal",
               "Prominent", 
               "Promising",
               "Reassuring",
               "Remarkable",
               "Robust",
               "Spectacular",
               "Supportive", 
               "Unique",
               "Unprecedented"),
  negative = c("Detrimental",
               "Disappointing",
               "Disconcerting",
               "Discouraging",
               "Disheartening", 
               "Disturbing",
               "Frustrating", 
               "Futile", 
               "Hopeless", 
               "Impossible",
               "Inadequate", 
               "Ineffective", 
               "Insignificant",
               "Insufficient", 
               "Irrelevant",
               "Mediocre", 
               "Pessimistic", 
               "Substandard", 
               "Unacceptable",
               "Unpromising", 
               "Unsatisfactory",
               "Unsatisfying", 
               "Useless", 
               "Weak",
               "Worrisome")
  )
  )


vinkers_corp <- corpus(data,text_field = "abstract")

# instead of text1, text2,... assign meaningful docvars
docnames(vinkers_corp) <- paste0(str_extract(data$author, "^[^,]+")," ",data$year) 

vinkers_toks <- vinkers_corp %>% 
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_separators = TRUE,
         ) 

vinkers_toks_dict <- vinkers_toks %>%  
  tokens_lookup(dictionary = dict_vinker,
                nomatch = "_UNMATCHED") %>% 
  tokens_group(groups = year)

vinkers_toks_dict %>% dfm() %>% head()

vinkers_dfm <- vinkers_toks_dict %>%
  dfm() %>%
  dfm_weight(scheme = "prop") 

vinkers_df <- vinkers_dfm %>%
  convert(to = "data.frame") %>%
  mutate(year = as.numeric(doc_id)) %>%
  select(-doc_id) %>% 
  select(year,everything())


### Correlation ----

vinkers_cor <- cor.test(vinkers_df$year,
                        vinkers_df$positive,
                        method="spearman",
                        alternative="greater")


### Trend ----

plot_vinkers <- ggplot(vinkers_df, aes(x = year)) +
  geom_line(aes(y = positive), color = "red",  size = 1) +
  geom_line(aes(y = negative), color = "blue", size = 1) +
  geom_smooth(aes(y=positive),color="red",span=0.75) +
  geom_smooth(aes(y=negative),color="blue",span=0.75) +
  labs(title = 'Vinkers Sentiments',
       subtitle = paste0("Spearman rho of positive sentiments: ", 
                         round(lsd_cor$estimate,2),
                         ", p-value =", lsd_cor$p.value),
       x = "Year",
       y = "Percent of Tokenmatches",
       caption = "red = positive\nblue = negative") +
  theme_minimal() 
plot_vinkers

ggsave("plots/sentiment_vinkers.png", bg="white", 
       width = 8, height = 5)



# 2. marginale Signifikanz ----


dict_marg_sign <- dictionary(
  list(
    marginal_significant_effects = c("marginally significant", 
                                     "p < .10", "p<.10", "p .10", 
                                     "marginal significant",
                                     "marginal significance",
                                     "trend towards significance",
                                     "trended towards significance",
                                     "trending towards significance",
                                     "approaching significance",
                                     "approached significance"
                                     )
    
    )
  )


marg_sign_corp <- corpus(data,text_field = "abstract")
docnames(marg_sign_corp) <- paste0(str_extract(data$author, "^[^,]+")," ",data$year) 

marg_sign_toks <- marg_sign_corp %>% 
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_separators = TRUE,
         ) 

marg_sign_toks_dict <- marg_sign_toks %>% 
  tokens_lookup(dictionary = dict_marg_sign,
                nomatch = "_UNMATCHED") %>% 
  tokens_group(groups = year)

marg_sign_toks_dict %>% dfm() %>% head()


marg_sign_dfm <- marg_sign_toks_dict %>%
  dfm() #%>%
#  dfm_weight(scheme = "prop") 

marg_sign_df <- marg_sign_dfm %>%
  convert(to = "data.frame") %>%
  mutate(year = as.numeric(doc_id)) %>%
  select(-doc_id) %>% 
  select(year,everything())


### Correlation ----

marg_sign_cor <- cor.test(marg_sign_df$year,
                        marg_sign_df$marginal_significant_effects,
                        method="spearman",
                        alternative="greater")

### Trend ----

plot_marg_sign <- ggplot(marg_sign_df, aes(x = year, y = marginal_significant_effects)) +
  geom_point(color = "purple",  size = 1) +
  geom_smooth(aes(y=marginal_significant_effects),method = "lm", color="purple",span=0.75) +
  labs(title = 'Mentions of marginal significant effects',
       subtitle = paste0("Spearman rho of marginal effects: ", 
                         round(marg_sign_cor$estimate,2),
                         ", p-value =", marg_sign_cor$p.value),
       x = "Year",
       y = "Absolute Tokenmatches") +
  theme_minimal() 
plot_marg_sign

ggsave("plots/marginal_significance.png", bg="white", 
       width = 8, height = 5)


#marg_sign_toks %>% 
#  kwic(pattern = dict_marg_sign)  %>% 
#  textplot_xray()



# 3. Textkomplexität ----

data$textcomplexity <- textstat_readability(data$abstract, measure = "meanWordSyllables")[,2]

## Correlation ----

textcomplexity_freq <- data %>%
  group_by(year) %>%
  summarise(textcomplexity = mean(textcomplexity, na.rm = TRUE))

textcomplexity_cor <- cor.test(textcomplexity_freq$year,
                               textcomplexity_freq$textcomplexity,
                               method="spearman",
                               alternative="greater")



## Trend ----

plot_textcomplexity <- textcomplexity_freq %>%
  ggplot(aes(x = year, y = textcomplexity)) + 
  geom_line() +
  geom_smooth() + 
  labs(title = 'Mean Word Syllables',
       subtitle = paste0("Spearman rho of positive sentiments: ", 
                         round(textcomplexity_cor$estimate,2),
                         ", p-value =", textcomplexity_cor$p.value),
       x = "Year", y = "Mean Text Complexity") +
  theme_minimal()
plot_textcomplexity

ggsave("plots/mean_word_syllables.png", bg="white", 
       width = 8, height = 5)


## hier commonwords analysieren ----


# t-tests ----



## sentiments ----
### vader ----

vader_data_s1 <- data_s1 %>% 
  left_join(vader_scores, by=c("abstract" = "text"))

vader_data_s3 <- data_s3 %>% 
  left_join(vader_scores, by=c("abstract" = "text"))

t_test_vader <-  t.test(x = vader_data_s3$pos,
                        y = vader_data_s1$pos,
                        alternative = "greater") %>%
  report()


### lsd2015 ----
#### report ----
lsd_split_1 <- subset(lsd_df, year <= split1)
lsd_split_3 <- subset(lsd_df, year > split2)

t_test_lsd2015 <-  t.test(x = lsd_split_3$pos,
                          y = lsd_split_1$pos,
                          alternative = "greater") %>%
  report()

#### plot ----
### plot ----
lsd_split_1$Condition <- 'Split1'
lsd_split_1$Condition_num <- 1
lsd_split_3$Condition <- 'Split3'
lsd_split_3$Condition_num <- 2

lsd_combined <- bind_rows(lsd_split_1, lsd_split_3) 

ggplot(data=lsd_combined, aes(x=Condition_num, y=positive, group=year)) +
  geom_jitter(color='darkred', width=.015, height=0, alpha=.8, size=2) +
  theme_ggdist() +
  theme(aspect.ratio = 1,
        axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        title = element_text(size=14)) +
  stat_slab(data=subset(lsd_combined, Condition == 'Split1'),
            aes(group=Condition, x=Condition_num-.15, y=positive), 
            position=position_dodge(), 
            scale=.5, 
            alpha=.3, 
            side='left') +
  stat_slab(data=subset(lsd_combined, Condition == 'Split3'),
            aes(group=Condition, x=Condition_num+.15, y=positive), 
            position=position_dodge(), 
            scale=.5, 
            alpha=.3, 
            side='right') +
  stat_pointinterval(data=subset(lsd_combined, Condition == 'Split1'), 
                     aes(group=Condition, x=Condition_num-.15, y=positive), 
                     color='darkred') +
  stat_pointinterval(data=subset(lsd_combined, Condition == 'Split3'), 
                     aes(group=Condition, x=Condition_num+.15, y=positive), 
                     color='darkred') +
  labs(title = 'Comparison of positive LSD2015 Sentiments per Split',
       subtitle = paste0(),
       x = "", y = "Mean Sentiment of Abstract") +
  scale_x_continuous(breaks=c(1, 2), labels=c('1993-2003', '2013-2023'))


ggsave("plots/ttest_vinkers.png", bg="white", 
       width = 7, height = 7)


### vinkers ----
#### report ----
vinkers_split_1 <- subset(vinkers_df, year <= split1)
vinkers_split_3 <- subset(vinkers_df, year > split2)

t_test_vinkers2015 <-  t.test(x = vinkers_split_3$pos,
                              y = vinkers_split_1$pos,
                              alternative = "greater") %>%
  report()

#### plot ----
vinkers_split_1$Condition <- 'Split1'
vinkers_split_1$Condition_num <- 1
vinkers_split_3$Condition <- 'Split3'
vinkers_split_3$Condition_num <- 2

vinkers_combined <- bind_rows(vinkers_split_1, vinkers_split_3) 

ggplot(data=vinkers_combined, aes(x=Condition_num, y=positive, group=year)) +
  geom_jitter(color='darkred', width=.015, height=0, alpha=.8, size=2) +
  theme_ggdist() +
  theme(aspect.ratio = 1,
        axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        title = element_text(size=14)) +
  stat_slab(data=subset(vinkers_combined, Condition == 'Split1'),
            aes(group=Condition, x=Condition_num-.15, y=positive), 
            position=position_dodge(), 
            scale=.5, 
            alpha=.3, 
            side='left') +
  stat_slab(data=subset(vinkers_combined, Condition == 'Split3'),
            aes(group=Condition, x=Condition_num+.15, y=positive), 
            position=position_dodge(), 
            scale=.5, 
            alpha=.3, 
            side='right') +
  stat_pointinterval(data=subset(vinkers_combined, Condition == 'Split1'), 
                     aes(group=Condition, x=Condition_num-.15, y=positive), 
                     color='darkred') +
  stat_pointinterval(data=subset(vinkers_combined, Condition == 'Split3'), 
                     aes(group=Condition, x=Condition_num+.15, y=positive), 
                     color='darkred') +
  labs(title = 'Comparison of positive Vinkers Sentiments per Split',
       subtitle = paste0(),
       x = "", y = "positive mean Sentiments of Abstract") +
  scale_x_continuous(breaks=c(1, 2), labels=c('1993-2003', '2013-2023'))


ggsave("plots/ttest_vinkers.png", bg="white", 
       width = 7, height = 7)




## marginale Signifikanz ----
### report ----
marg_sign_split_1 <- subset(marg_sign_df, year <= split1)
marg_sign_split_3 <- subset(marg_sign_df, year > split2)

t_test_marg_sign2015 <-  t.test(x = marg_sign_split_3$marginal_significant_effects,
                                y = marg_sign_split_1$marginal_significant_effects,
                                alternative = "greater") %>%
  report()


### plot ----
marg_sign_split_1$Condition <- 'Split1'
marg_sign_split_1$Condition_num <- 1
marg_sign_split_3$Condition <- 'Split3'
marg_sign_split_3$Condition_num <- 2

marg_sign_combined <- bind_rows(marg_sign_split_1, marg_sign_split_3) 

ggplot(data=marg_sign_combined, aes(x=Condition_num, y=marginal_significant_effects, group=year)) +
  geom_jitter(color='darkred', width=.015, height=0, alpha=.8, size=2) +
  theme_ggdist() +
  theme(aspect.ratio = 1,
        axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        title = element_text(size=14)) +
  stat_slab(data=subset(marg_sign_combined, Condition == 'Split1'),
            aes(group=Condition, x=Condition_num-.15, y=marginal_significant_effects), 
            position=position_dodge(), 
            scale=.5, 
            alpha=.3, 
            side='left') +
  stat_slab(data=subset(marg_sign_combined, Condition == 'Split3'),
            aes(group=Condition, x=Condition_num+.15, y=marginal_significant_effects), 
            position=position_dodge(), 
            scale=.5, 
            alpha=.3, 
            side='right') +
  stat_pointinterval(data=subset(marg_sign_combined, Condition == 'Split1'), 
                     aes(group=Condition, x=Condition_num-.15, y=marginal_significant_effects), 
                     color='darkred') +
  stat_pointinterval(data=subset(marg_sign_combined, Condition == 'Split3'), 
                     aes(group=Condition, x=Condition_num+.15, y=marginal_significant_effects), 
                     color='darkred') +
  labs(title = 'Comparison of Mean Word Syllables per Split',
       subtitle = paste0(),
       x = "", y = "Mean Word Syllables of Abstract") +
  scale_x_continuous(breaks=c(1, 2), labels=c('1993-2003', '2013-2023'))


ggsave("plots/ttest_marg_sign.png", bg="white", 
       width = 7, height = 7)


## textcomplexity ----
### report ----
textcomplexity_split_1 <- subset(textcomplexity_freq, year <= split1)
textcomplexity_split_3 <- subset(textcomplexity_freq, year > split2)

t_test_textcomplexity2015 <-  t.test(x = textcomplexity_split_3$textcomplexity,
                                y = textcomplexity_split_1$textcomplexity,
                                alternative = "greater") %>% 
  report() %>% 
  summary()

### plot ----
textcomplexity_split_1$Condition <- 'Split1'
textcomplexity_split_1$Condition_num <- 1
textcomplexity_split_3$Condition <- 'Split3'
textcomplexity_split_3$Condition_num <- 2

textcomplexity_combined <- bind_rows(textcomplexity_split_1, textcomplexity_split_3) 

ggplot(data=textcomplexity_combined, aes(x=Condition_num, y=textcomplexity, group=year)) +
  geom_jitter(color='darkred', width=.015, height=0, alpha=.8, size=2) +
  theme_ggdist() +
  theme(aspect.ratio = 1,
        axis.text = element_text(size=13),
        axis.title = element_text(size=14),
        title = element_text(size=14)) +
  stat_slab(data=subset(textcomplexity_combined, Condition == 'Split1'),
            aes(group=Condition, x=Condition_num-.15, y=textcomplexity), 
            position=position_dodge(), 
            scale=.5, 
            alpha=.3, 
            side='left') +
  stat_slab(data=subset(textcomplexity_combined, Condition == 'Split3'),
            aes(group=Condition, x=Condition_num+.15, y=textcomplexity), 
            position=position_dodge(), 
            scale=.5, 
            alpha=.3, 
            side='right') +
  stat_pointinterval(data=subset(textcomplexity_combined, Condition == 'Split1'), 
                     aes(group=Condition, x=Condition_num-.15, y=textcomplexity), 
                     color='darkred') +
  stat_pointinterval(data=subset(textcomplexity_combined, Condition == 'Split3'), 
                     aes(group=Condition, x=Condition_num+.15, y=textcomplexity), 
                     color='darkred') +
  labs(title = 'Comparison of Mean Word Syllables per Split',
       subtitle = paste0(),
       x = "", y = "Mean Word Syllables of Abstract") +
  scale_x_continuous(breaks=c(1, 2), labels=c('1993-2003', '2013-2023'))


ggsave("plots/ttest_syllables.png", bg="white", 
       width = 7, height = 7)
