import meme
import articles
import sys
import os

m = meme.Meme()

def create_memes(n, stop_at_ml=False):
    # def normalize(inp):
        # if len(inp) != 
    arts = articles.get_articles_guardian(n)
    articles.w2f(arts, '.tmp_art_file')
    os.popen('./meme .tmp_art_file .tmp_meme_file')
    os.wait()
    memes = m.memes_from_f('.tmp_meme_file')
    os.popen('rm .tmp_art_file .tmp_meme_file')
    os.wait()
    if stop_at_ml:
        return [m.choose_meme_from_m(memes[x], arts[x][0]) for x in range(len(memes))]
    else:
        final_memes = []
        for i in range(len(memes)):
            tmp_meme = m.gen_meme_from_m(m.choose_meme_from_m(memes[i], arts[i][0])).json()
            if 'data' in tmp_meme:
                final_memes.append(tmp_meme['data']['url'])
                print(str(i) + '/' + str(len(memes)))
        return final_memes
if __name__ == '__main__':
    memes = create_memes(int(sys.argv[1]))
    for i in memes:
        print(i)
