import meme
import os

m = meme.Meme()

_all = m.reference['data']['memes']
c = 0
for i in _all:
# maybe change meme.py code to jsut use the filenames would make more sense lol
    os.popen('wget ' + i['url'] + ' --no-check-certificate -O resources/img/' + str(i['id']) + '.jpg')
    os.wait()
    c+=1
    print(str(c) + '/' + str(len(_all)))
