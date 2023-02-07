library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(gridExtra)
library(grid)

twitter <- read_delim("~/Documents/coding projects/Twitter_LINKE/LINKE_data.csv",
                      delim = ";", locale = locale(decimal_mark = ".")
)

print(twitter)

ht_words <- list()
for (row in 1:nrow(twitter)) {
    if (!is.na(twitter$hashtags[row])) {
        ht_words <- c(ht_words, unlist(strsplit(twitter$hashtags[row], split = ",")))
    }
}
hashtag_df <- data.frame(hashtag = unlist(ht_words)) %>%
    group_by(hashtag) %>%
    summarise(count = n())

dev.new()
grid.table(hashtag_df %>%
    select(hashtag, count) %>%
    arrange(desc(count)) %>%
    filter(count > 2)
)

twitter$message <- sapply(strwrap(twitter$text, width = 50, simplify = FALSE), paste, collapse = "\n")
grid.newpage()
grid.table(twitter %>%
    filter(is.na(rt_source)) %>%
    mutate(interactions = (likes + retweets) / age) %>%
    select(interactions, hashtags, message, creat_time) %>%
    arrange(desc(interactions)) %>%
    slice(1:10)
)

grid.newpage()
grid.table(twitter["rt_source"] %>%
    filter(!is.na(rt_source)) %>%
    group_by(rt_source) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(relative = count / sum(count))
)

freq_ht <- table(ifelse(hashtag_df$count > 2, 2,
                 ifelse(hashtag_df$count > 1, 1, 0)))
pie(freq_ht,
    labels = c(paste("einmal benutzt\n", round(100 * as.numeric(freq_ht)[1] / nrow(hashtag_df), 1), "%"),
               paste("zweimal benutzt\n", round(100 * as.numeric(freq_ht)[2] / nrow(hashtag_df), 1), "%"),
               paste("mehrmals benutzt\n", round(100 * as.numeric(freq_ht)[3] / nrow(hashtag_df), 1), "%")),
    col = c("#8EE4E2", "#8EBBE4", "#8EE4B7"),
    main = paste("einmalig / mehrmals verwendete Hashtags, N = ", nrow(hashtag_df))
)

rt_src_stats <- twitter["rt_source"] %>%
    filter(!is.na(rt_source)) %>%
    group_by(rt_source) %>%
    summarise(count0 = n()) %>%
    arrange(desc(count0)) %>%
    mutate(rt_source_thr = ifelse(count0 / sum(count0) < 0.015, "Other", rt_source)) %>%
    group_by(rt_source_thr) %>%
    summarise(count = sum(count0)) %>%
    mutate(pct = round(count / sum(count) * 100, 1)) %>%
    arrange(desc(count))

#par(mar = c(10, 4, 3, 0.5)) #c(5, 4, 4, 2) + 0.1
rt_bar <- barplot(
    height = rt_src_stats$pct,
    names = rt_src_stats$rt_source_thr,
    ylim = c(0, 40),
    col = "#8EBBE4",
    main = paste("Quellen der Retweets, N = ", sum(rt_src_stats$count)),
    ylab = "Anzahl in Prozent",
    beside = TRUE,
    las = 2
)
text(rt_bar, rt_src_stats$pct + 2.0, paste(rt_src_stats$pct, "%", sep = ""))

twitter <- mutate(twitter,
                  tweet_kind = ifelse(is.na(rt_source), "Tweet", "Retweet")
)
week_order <- as.character(c(46:52, 1:tail(twitter$creat_week, n = 1)))
posted_tweets <- table(factor(twitter$tweet_kind),
                       factor(twitter$creat_week, levels = week_order)
)
print(posted_tweets)

barplot(posted_tweets,
        main = paste("Anzahl (Re)Tweets, N = ", nrow(twitter)),
        xlab = "Kalenderwoche, 2022/23",
        ylab = "Anzahl",
        ylim = c(0, 60),
        col = c("lightblue", "#bae4ba"),
        legend.text = rownames(posted_tweets),
        beside = TRUE
)
grid(nx = NA,
     ny = NULL,
     lty = 2, col = "#d1c7c7", lwd = 0.5
)

str_detect(twitter$message, "riefahl")