import json
import requests
import time
import math
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
def get_articles_guardian(n=50):
    base = 'https://content.guardianapis.com/search'
    arts = []
    i = 0
    while True:
        i+=1
        params = {'api-key': '____________________________________', 'page-size': 50, 'page': i}
        ret = requests.get(base, params).json()['response']['results']
        for art in ret:
            tmp_title = ''
            for ltr in art['webTitle']:
                if ord(ltr) <= 128:
                    tmp_title += ltr
                else:
                    tmp_title += ' '
            arts.append([tmp_title, art['webUrl']])
            if len(arts) == n: return arts
        print(str(len(arts)) + ' added so far')
    return arts

def w2f(js_o, fname):
    with open(fname, 'w') as f:
        json.dump(js_o, f)
def rff(fname):
    with open(fname, 'r') as f:
        ret = json.load(f)
    return ret
