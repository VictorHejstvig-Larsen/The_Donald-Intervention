import json
import datetime
import requests
import random
import csv
import time
import os
from json.decoder import JSONDecodeError
from tqdm import tqdm
import csv
import pandas as pd


# Define start and end dates
start_date = 1548983190

end_date = 1598835990

mid_date = (start_date + end_date) // 2

timestamps = [start_date, mid_date, end_date]

#datetime.date(2020, 8, 31)

# Define subreddit to query

# Define API endpoint URL
api_url = "http://api.pushshift.io/reddit/search/comment"

def collect_subData(subm):
    author = subm['author']
    subreddit = subm['subreddit']
    body = subm['body']
    created = datetime.datetime.fromtimestamp(subm['created_utc'])
    score = subm['score']
    reddit_gold = subm['gilded']
    return (author, subreddit, body, str(created), score, reddit_gold)

#Problemet er at den ikke har kommentarer fra brugeren på timestampet så tager den bare random kommentarer
df = pd.read_csv('userlist.csv', header=None)
user_list = df[1].tolist()
print(user_list)

def update_subFile():
        upload_count = 0
        location = os.getcwd()
        print("Skriv filnavn husk .csv")
        filename = input()
        file_path = location + '/' + filename
        headers = ["Author", "Subreddit", "Body", "Publish Date", "Score", "Reddit_Gold"]

        with open(file_path, 'w', newline='', encoding='utf-8') as file:
            a = csv.writer(file, delimiter=',')
            a.writerow(headers)

            bar = tqdm(total=len(user_list))

            for user in user_list:

                timestamps = sorted(random.sample(range(start_date, end_date), 3))

                for timestamp in timestamps:

                    print(timestamp)

                    url = f"{api_url}?author={user}&after={timestamp}&size=300"
                    print(url)
                    response = requests.get(url)
                    if not response or response.text == '':
                        # Empty or invalid response, retry request
                        continue

                    data = json.loads(response.text, strict=False)
                    if 'data' not in data:
                        print(data)
                        break

                    subs = data['data']
                    if not subs:
                        break

                    for sub in subs:
                        subData = collect_subData(sub)
                        a.writerow(subData)
                        upload_count += 1


                bar.update(1)
                time.sleep(0.5)



        file.close()

update_subFile()


