import tweepy
import pandas as pd
from datetime import datetime
import Twitter_token

# function to print all the hashtags in a text
def extract_hashtags(text):
    hashtag_list = []
    for word in text.split():
        # checking the first character of every word
        if word[0] == '#':
            #replace last character if necessary
            if not(word[-1].isalpha()):
                word = word[:-1]
            hashtag_list.append(word[1:])
    return hashtag_list

#open CSV file
df = pd.read_csv('LINKE_data.csv', header=0, sep=';')

auth = tweepy.Client(Twitter_token.bearer_token)
api = tweepy.API(auth)

query = 'from:dielinkeberlin'

tweets = auth.search_recent_tweets(query=query, tweet_fields=['created_at','public_metrics'], max_results=100)

for tweet in reversed(tweets.data):
    created_at = str(tweet.created_at)
    creation_date = datetime.strptime(created_at[:created_at.find('+')], '%Y-%m-%d %H:%M:%S')
    if not(df['creat_time'].map(str).eq(created_at).any()):
        txt = str(tweet.text).replace(';', '')
        rt_src = ''
        if txt[:4] == 'RT @':
            rt_src = txt[3:txt.find(':')]
        df = df.append({'text': txt,
                        'creat_time': created_at,
                        'creat_week': creation_date.isocalendar().week,
                        'rt_source': rt_src,
                        'hashtags': ",".join(extract_hashtags(txt)),
                        'likes': tweet.public_metrics['like_count'],
                        'retweets': tweet.public_metrics['retweet_count'],
                        'age': (datetime.now() - creation_date).total_seconds() / 3600.0}, ignore_index=True)
        print("Tweet (" + created_at + ") hinzugefügt!")
    else:
        print("Tweet (" + created_at + ") bereits erfasst!")

df.to_csv('LINKE_data.csv', sep=';', index = False)

#print(df)