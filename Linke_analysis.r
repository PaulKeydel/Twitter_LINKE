library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(gridExtra)
library(grid)

#lower all alphanumerical characters
#replace umlauts
#remove all non-alphanumerical characters
transform_text_col <- function(txtcol) {
    return(str_replace_all(casefold(txtcol), c("ü" = "ue", "ä" = "ae", "ö" = "oe")) %>%
           str_replace_all("[^[:alnum:]]", "")
    )
}

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
ht_words <- transform_text_col(ht_words)
hashtag_df <- data.frame(hashtag = unlist(ht_words)) %>%
    group_by(hashtag) %>%
    summarise(count = n())

dev.new()
grid.table(hashtag_df %>%
    select(hashtag, count) %>%
    arrange(desc(count)) %>%
    filter(count > 2)
)

interactions_per_week <- twitter %>%
    filter(is.na(rt_source)) %>%
    group_by(creat_week) %>%
    summarise(day = max(creat_time), interactions = sum(likes) + sum(retweets), sumage = sum(age)) %>%
    mutate(impact = interactions / sumage) %>%
    arrange(day)

barplot(height = interactions_per_week$impact,
        names = interactions_per_week$creat_week,
        main = "Interaktionen auf Tweets per Stunde",
        xlab = "Kalenderwoche, 2022/23",
        ylab = "Interaktionen auf Tweets per Stunde",
        ylim = c(0, 3),
        col = "#8EBBE4"
)

if (TRUE) {
    #Retweets: How many accounts are related to the party?
    rt_csv <- read_delim("~/Documents/coding projects/Twitter_LINKE/single_rt_sources.csv")
    print(table(replace_na(rt_csv$is_from_party, 0)))
} else {
    #export single RT sources to new csv file for further analysis
    write.csv(twitter["rt_source"] %>%
        filter(!is.na(rt_source)) %>%
        group_by(rt_source) %>%
        summarise(count = n()) %>%
        arrange(desc(count)),
        "single_rt_sources.csv"
    )
}

freq_ht <- table(ifelse(hashtag_df$count > 2, "3", hashtag_df$count))
names(freq_ht) <- sapply(names(freq_ht), paste, "-mal", sep = "")
names(freq_ht)[3] <- ">2-mal"

par(mar = c(1, 4, 4, 2) + 0.1)
pie(freq_ht,
    labels = paste(round(100 * as.numeric(freq_ht) / nrow(hashtag_df), 1), "%"),
    col = c("#8EE4E2", "#8EBBE4", "#8EE4B7"),
    main = paste("Hashtag-Verwendung, N = ", nrow(hashtag_df))
)
legend(0.95, .75,
       names(freq_ht),
       fill = c("#8EE4E2", "#8EBBE4", "#8EE4B7"),
       title = "Nutzung"
)

rt_src_stats <- twitter["rt_source"] %>%
    filter(!is.na(rt_source)) %>%
    group_by(rt_source) %>%
    summarise(count0 = n()) %>%
    arrange(desc(count0)) %>%
    mutate(rt_source_thr = ifelse(count0 / sum(count0) < 0.01, "Other", rt_source)) %>%
    group_by(rt_source_thr) %>%
    summarise(count = sum(count0)) %>%
    mutate(pct = round(count / sum(count) * 100, 1)) %>%
    arrange(desc(count))

par(mar = c(10, 4, 3, 0.5))
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

par(mar = c(5, 4, 4, 2) + 0.1) #this are the default margins
barplot(posted_tweets,
        main = paste("Anzahl (Re)Tweets, N = ", nrow(twitter)),
        xlab = "Kalenderwoche, 2022/23",
        ylab = "Anzahl",
        ylim = c(0, 70),
        col = c("lightblue", "#bae4ba"),
        legend.text = rownames(posted_tweets),
        args.legend = list(x = "topleft", inset = c(0.05, 0)),
        beside = TRUE
)
grid(nx = NA,
     ny = NULL,
     lty = 2, col = "#d1c7c7", lwd = 0.3
)

text_entities <- list("co2",
                      "klima",
                      "luetzi",
                      "luetzerath",
                      "oeko",
                      "treibhaus",
                      "energiewende",
                      "sozial",
                      "rente",
                      "armut",
                      "wohnungslos",
                      "strom",
                      "obdachlosigkeit",
                      "waerme"
)
for (it in text_entities) {
    print(paste0(it, " : ", sum(str_detect(transform_text_col(twitter$text), it))))
}
