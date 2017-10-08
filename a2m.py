import meme
import articles
import sys
import os

m = meme.Meme()

def create_memes(n, stop_at_ml=False, filter_failure=False):
    # def normalize(inp):
        # if len(inp) != 
    arts = articles.get_articles_guardian(n)
    articles.w2f(arts, '.tmp_art_file')
    os.popen('./meme .tmp_art_file .tmp_meme_file')
    os.wait()
    memes = m.memes_from_f('.tmp_meme_file')
    m_a_tuples = []
    for i in range(len(arts)):
        m_a_tuples.append((memes[i], arts[i]))
    if filter_failure:
        m_a_tuples = [x for x in m_a_tuples if x[0] != [(['bad luck brian'], ('tried to make a meme from this article', 'failed'))]]
    os.popen('rm .tmp_art_file .tmp_meme_file')
    os.wait()
    if stop_at_ml:
        return [m.choose_meme_from_m(m_a_tuples[x][0], m_a_tuples[x][1][0]) for x in range(len(m_a_tuples))]
    else:
        final_memes = []
        for i in range(len(m_a_tuples)):
            tmp_meme = m.gen_meme_from_m(m.choose_meme_from_m(m_a_tuples[i][0], m_a_tuples[i][1][0])).json()
            if 'data' in tmp_meme:
                final_memes.append(tmp_meme['data']['url'])
                print(str(i) + '/' + str(len(m_a_tuples)))
        return final_memes
if __name__ == '__main__':
    memes = create_memes(int(sys.argv[1]), 'stop' in sys.argv, 'filter' in sys.argv)
    for i in memes:
        print(i)
