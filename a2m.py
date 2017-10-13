import meme
import articles
import sys
import os

m = meme.Meme()

def create_memes(n, stop_at_ml=False, filter_failure=False, verbose=True, print_steps=False, art=None, r_len=False, test_mode=False):
    if art != None:
        if print_steps: print('using user supplied articles')
        arts = articles.rff(art)
    else: arts = articles.get_articles_guardian(n, verbose)
    if print_steps: print('all ' + str(len(arts)) + ' articles in place')
    if art == None:
        articles.w2f(arts, '.tmp_art_file')
        os.popen('./meme .tmp_art_file .tmp_meme_file')
        os.wait()
    else:
        os.popen('./meme ' + art + ' .tmp_meme_file')
        os.wait()
    if print_steps: print('meme scaffolding placed')
    memes = m.memes_from_f('.tmp_meme_file')
    m_a_tuples = []
    for i in range(len(arts)):
        m_a_tuples.append((memes[i], arts[i]))
    if filter_failure:
        m_a_tuples = [x for x in m_a_tuples if x[0] != [(['bad luck brian'], ('tried to make a meme from this article', 'failed'))]]
    if art == None: os.popen('rm .tmp_art_file')
    os.popen('rm .tmp_meme_file')
    os.wait()
    if stop_at_ml:
        if print_steps: print('stopping before final step')
        # return [m.choose_meme_from_m(m_a_tuples[x][0], m_a_tuples[x][1][0]) for x in range(len(m_a_tuples))]
        if not test_mode:
            ret = []
            for i in range(len(m_a_tuples)):
                ret.append(m.choose_meme_from_m(m_a_tuples[i][0], m_a_tuples[i][1][0]))
                if print_steps: print(str(i) + '/' + str(len(m_a_tuples)))
        else: ret = [x[0] for x in m_a_tuples]
        if r_len: return (ret, len(arts))
        return ret
    else:
        final_memes = []
        for i in range(len(m_a_tuples)):
            tmp_meme = m.gen_meme_from_m(m.choose_meme_from_m(m_a_tuples[i][0], m_a_tuples[i][1][0])).json()
            if 'data' in tmp_meme:
                final_memes.append(tmp_meme['data']['url'])
                print(str(i) + '/' + str(len(m_a_tuples)))
        return final_memes

def test_coverage(n, v=True, ps=False, a=None):
    mm = create_memes(n, True, True, verbose=v, print_steps=ps, art=a, r_len=True, test_mode=True)
    return len(mm[0])/mm[1]

if __name__ == '__main__':
    user_art = None
    if 'uart' in sys.argv:
        # so when argv[1] is not numeric we don't cast to int 
        n_inp = -1
        user_art = sys.argv[1]
    else:
        n_inp = int(sys.argv[1])
    if 'test' in sys.argv:
        ret = test_coverage(n_inp, 'silent' not in sys.argv, 'print' in sys.argv, user_art)
        print(str(ret*100) + '% coverage')
    else:
        memes = create_memes(n_inp, 'stop' in sys.argv, 'filter' in sys.argv, 'silent' not in sys.argv, 'print' in sys.argv, art=user_art, r_len=False, test_mode=False)
        for i in memes:
            print(i)
