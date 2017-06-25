import json
import requests
import time
from datetime import date, timedelta

base = 'http://api.nytimes.com/svc/search/v2/articlesearch.json?%s'
def get_articles(n):
    articles = []
    today = date.today()
    day = 0

    while True:
        td = today - timedelta(days=day)
        day += 1
        start_date = end_date = str(td).replace('-', '')
        params = { 'facet_field': 'day_of_week', 'begin_date': start_date, 'end_date': end_date, 'api-key': '________________________________' }
        res = requests.get(base, params).json()['response']['docs']
        for article in res:
            articles.append([article['headline']['main'] + article['snippet'].strip('....') + article['lead_paragraph'], article['web_url']])
            if len(articles) == n: break
        if len(articles) == n: break
        time.sleep(.9) #bc the usage limit of the api is 1 call/second
    return articles
def get_articles_news(n, src):
    arts = []
    base = 'https://newsapi.org/v1/articles'
    params = {'apiKey': '________________________________', 'source': src}

    while True:
        tmp_res = requests.get(base, params).json()['articles']
        for i in tmp_res:
            arts.append(['', i['url']])
            tmp_str = ''
            if 'description' in i:
                tmp_str += str(i['description'])
                # arts.append([i['description'] + ' ' + i['title'], i['url']])
            if 'title' in i:
                tmp_str += str(i['title'])
            arts[len(arts)-1][0] = tmp_str

            if len(arts) == n: break
        if len(arts) == n: break
    return arts
