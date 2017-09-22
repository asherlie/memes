import meme
import articles
import sys
import os

m = meme.Meme()

arts = articles.get_articles_guardian(int(sys.argv[1]))
articles.w2f(arts, '.tmp_art_file')
os.popen('./meme .tmp_art_file .tmp_meme_file')
os.wait()

memes = m.memes_from_f('.tmp_meme_file')
os.popen('rm .tmp_art_file .tmp_meme_file')
os.wait()
final_memes = []
for i in range(len(memes)):
    tmp_meme = m.gen_meme_from_m(memes[i], arts[i][0]).json()
    if 'data' in tmp_meme:
        final_memes.append(tmp_meme['data']['url'])
        print(str(i) + '/' + str(len(memes)))
for i in final_memes:
    print(i)
