import json
import requests

def get_articles_guardian(n=50, verbose=True):
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
            if tmp_title[len(tmp_title)-1] == ' ':
                arts.append([tmp_title[:-1], art['webUrl']])
            else:
                arts.append([tmp_title, art['webUrl']])
            if len(arts) == n: return arts
        if verbose: print(str(len(arts)) + ' articles grabbed so far')
    return arts

def w2f(js_o, fname):
    with open(fname, 'w') as f:
        json.dump(js_o, f)
def rff(fname):
    with open(fname, 'r') as f:
        ret = json.load(f)
    return ret
