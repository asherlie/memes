import os
# import articles

# the article data for this is prepared in write_pats_from_art in meme.hs
def prep_data_chatter(fname):
    ret = []
    with open(fname, 'r') as f:
        ff = f.readlines()
    for i in ff:
        tmp = i.strip('\n').split(' ')
        tmp_el = []
        for i in tmp:
            tmp_el.append(i.split('/'))
        ret.append(tmp_el)
    return ret
            
def find_p_chatter(prepped, consec_words=4):
    ret = {}
    for article in prepped:
        for i in range(len(article)-consec_words):
            #this loop is just to construct tmp vars
            tmp_type = ''
            tmp_str  = ''
            for wrd in range(consec_words):
                tmp_type += (article[i + wrd][1] + ' ')
                tmp_str  += (article[i + wrd][0] + ' ')

            if tmp_type not in ret:
                ret[tmp_type] = [1, [tmp_str]]
            else:
                ret[tmp_type][0] += 1
                ret[tmp_type][1].append(tmp_str)
                # ret[tmp_type][1] = tmp_str
    return ret

def pp_chat(pats, min_occ=1, rev=False):
    pat_id_sorted = sorted(pats, key=lambda k: pats[k][0], reverse=rev) # sort by num of occurences
    for i in pat_id_sorted:
        if pats[i][0] >= min_occ:
            print(i + ' : ' + str(pats[i][0]) + ' : ' + pats[i][1][0])

def ppl(lst):
    strn = ''
    for i in range(len(lst)):
        if i != 0:
            strn += ', '
        strn += lst[i]
    return strn
def pp(pats, mini=1, allow_unknowns=True):
    for i in pats:
        if pats[i][0] >= mini:
            temp_str = (i + ' : ' + str(pats[i][0]) + ' : ' + ppl(pats[i][1]))
            if allow_unknowns:
                print(temp_str)
            else:
                if temp_str.find('Unknown') == -1:
                    print(temp_str)
def st_quot(strng):
    return strng[1:-1]

def tag(st):
    b = os.popen('./meme_pos "' + st + '"').read().strip('\n').strip('[').strip(']')[0:-1]
    ret = b.split('"')
    rr={}
    for i in range(0, len(ret), 2):
        rr[ret[i].strip(' ')] = ret[i+1].strip(' ')
    return rr

def m_pos(art):
    num = len(art)
    c = 0
    articles = []
    for i in art:
        articles.append(os.popen('./meme_pos "' + i[0] + '"').read().encode('ascii', 'ignore').decode().strip('\n').strip('[').strip(']').strip(',').split('"')[0:-1])
        c+=1
        print('done tagging ' + str(c) + '/' + str(num))
    return articles
def find_p(num, unique=True):
    # m = meme.Meme()
    # a = articles.get_articles(num)
    print('articles scraped succesfully')
    
    lst = {}
    # for art_pos in m_pos(a):
    for art_pos in num:
        for i in range(0, len(art_pos)-6, 2):
            lst[art_pos[i] + art_pos[i+2] + art_pos[i+4] + art_pos[i+6]] = [0, []]
        for i in range(0, len(art_pos)-6, 2):
            tmp_type = (art_pos[i] + art_pos[i+2] + art_pos[i+4] + art_pos[i+6])
            tmp_sent = (art_pos[i+1] + ' ' + art_pos[i+3] + ' ' + art_pos[i+5] + ' ' + art_pos[i+7])
            if unique:
                if tmp_sent not in lst[tmp_type]:
                    lst[tmp_type][0] += 1
                    lst[tmp_type][1].append(tmp_sent)
                    
            else:
                lst[tmp_type][0] += 1
                lst[tmp_type][1].append(tmp_sent)

    return lst
            # word = wrd.strip(',').strip(' ') #shouldnt be here lol - want to have separate loop to convert before i go into the pattern finding loop
