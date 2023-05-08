import json
import datetime
import requests
import random
import csv
import time
import os
from json.decoder import JSONDecodeError
from tqdm import tqdm

# Define start and end dates
start_date = datetime.date(2019, 2, 1)
end_date = datetime.date(2020, 2, 29)

# Define subreddit to query
subreddit = "the_donald"

# Define API endpoint URL
api_url = "http://api.pushshift.io/reddit/search/comment"

def collect_subData(subm):
    author = subm['author']
    post_id = subm['id']
    body = subm['body']
    created = datetime.datetime.fromtimestamp(subm['created_utc'])
    score = subm['score']
    reddit_gold = subm['gilded']
    return (author, post_id, body, str(created), score, reddit_gold)

def update_subFile():
        upload_count = 0
        scrape_count = 0
        location = os.getcwd()
        print("Skriv filnavn husk .csv")
        filename = input()
        file_path = location + '/' + filename
        headers = ["Author", "Post_Id", "Body", "Publish Date", "Score", "Reddit_Gold"]

        with open(file_path, 'w', newline='', encoding='utf-8') as file:
            a = csv.writer(file, delimiter=',')
            a.writerow(headers)

            total = 20000000
            bar = tqdm(total=total)
            while upload_count < 20000000:
                rand_numb = random.randint(1,13)
                if rand_numb <= 11:
                    year=2019
                    month = random.randint(2,12)
                else:
                    year=2020
                    month = random.randint(1,2)
                day = random.randint(1, 28)
                hour = random.randint(0, 23)
                minute = random.randint(0, 59)
                second = random.randint(0, 59)
                timestamp = datetime.datetime(year, month, day, hour, minute, second)
                print(timestamp)
                epoch_time = int(timestamp.timestamp())

                url = f"{api_url}?subreddit={subreddit}&before={epoch_time}&size=500"

                response = requests.get(api_url)
                if not response or response.text == '':
                # Empty or invalid response, retry request
                    continue

                r = requests.get(url)
                data = json.loads(r.text, strict=False)


                if 'data' not in data:
                    print(data)
                    continue

                subs = data['data']
                for sub in subs:
                    subData = collect_subData(sub)
                    a.writerow(subData)
                    upload_count += 1
                    scrape_count += 1
                    bar.update(1)

                    if upload_count == 20000000:
                        break
        print(str(upload_count) + " rows uploadet til filen :-)")

        file.close()

update_subFile()

