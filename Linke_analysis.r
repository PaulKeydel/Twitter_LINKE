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
sum(is.na(twitter$rt_source))
sum(!is.na(twitter$rt_source))

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
    stopifnot(nrow(rt_csv) == length(unique(na.omit(twitter$rt_source))))
    stopifnot(sum(rt_csv$count) == sum(!is.na(twitter$rt_source)))
    rt_csv$is_from_party <- replace_na(rt_csv$is_from_party, 0)
    print(table(rt_csv$is_from_party))
    print(rt_csv %>%
          group_by(is_from_party) %>%
          summarise(party_content = sum(count) / sum(rt_csv$count))
    )
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
    mutate(rt_source_thr = ifelse(count0 / sum(count0) < 0.01, "Nebensächliche Accounts", rt_source)) %>%
    group_by(rt_source_thr) %>%
    summarise(count = sum(count0)) %>%
    mutate(pct = round(count / sum(count) * 100, 1)) %>%
    arrange(count)

par(mar = c(3, 11.5, 4, 2))
rt_bar <- barplot(
    height = rt_src_stats$pct,
    names = rt_src_stats$rt_source_thr,
    xlim = c(0, 30),
    col = "#8EBBE4",
    main = paste("Verwendung der Retweet-Accounts, N = ", length(unique(na.omit(twitter$rt_source)))),
    axes = FALSE,
    beside = TRUE,
    horiz = TRUE,
    las = 2
)
text(rt_src_stats$pct + 1.5, rt_bar, paste(rt_src_stats$pct, "%", sep = ""))

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
                      "nahverkehr",
                      "energiewende",
                      "sozial",
                      "rente",
                      "armut",
                      "wohnungslos",
                      "schule",
                      "obdachlos",
                      "waerme"
)
for (it in text_entities) {
    print(paste0(it, " : ", sum(str_detect(transform_text_col(twitter$text), it))))
}

#interaction score Si
print(twitter$text[str_detect(transform_text_col(twitter$text), "miete") & is.na(twitter$rt_source)])
print(twitter$text[str_detect(transform_text_col(twitter$text), "vergesellschaft") & is.na(twitter$rt_source)])
twitter <- twitter %>% mutate(Si = (likes + retweets) / age)
print(paste("Durchschnittliche Interaktionen pro Std.: ",
            mean(twitter$Si[is.na(twitter$rt_source)])))
print(paste("Durchschnittliche Interaktionen pro Std. zum Thema Mieten: ",
            mean(twitter$Si[is.na(twitter$rt_source) & str_detect(transform_text_col(twitter$text), "miete")])))
print(paste("Durchschnittliche Interaktionen pro Std. zum Thema Vergesellschaftung: ",
            mean(twitter$Si[is.na(twitter$rt_source) & str_detect(transform_text_col(twitter$text), "vergesellschaft")])))
