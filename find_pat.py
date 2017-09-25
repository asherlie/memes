import os
import json

# the article data for this is prepared in write_pats_from_art in meme.hs
def generate_pattern_list(fname, consec_words=4, min_occ=1):
    return create_pat_chat(pp_chat(find_p_chatter(prep_data_chatter(fname), consec_words), min_occ))

def _enumerate(spl):
    # spl = strn.split(' ')
    occ = {x: 0 for x in spl}
    for i in range(len(spl)):
        if spl[i] in spl[i+1:]:
            occ[spl[i]] += 1
    for i in range(len(spl)-1, -1, -1): #iterate backwards
        bef = spl[i]
        spl[i] += ('_' + str(occ[spl[i]]))
        occ[bef] -= 1
    return spl
def prep_data_chatter(fname, line_f=True):
    def remove_tr(strn):
        if strn == '' or strn[len(strn)-1] != ' ':
            return strn
        return remove_tr(strn[:-1])
    # def _enumerate(strn):
    def _enumerate(spl):
        # spl = strn.split(' ')
        occ = {x: 0 for x in spl}
        for i in range(len(spl)):
            if spl[i] in spl[i+1:]:
                occ[spl[i]] += 1
        for i in range(len(spl)-1, -1, -1): #iterate backwards
            bef = spl[i]
            spl[i] += ('_' + str(occ[spl[i]]))
            occ[bef] -= 1
        return spl
    ret = []
    if line_f:
        with open(fname, 'r') as f:
            ff = f.readlines()
        for i in ff:
            tmp = i.strip('\n').split(' ')
            tmp_el = []
            # tmp_el = set()
            # tm_s = []
            for i in tmp:
                # tm_s = i.split('/')
                # if len(tm_s) != 2:
                    # print(tm_s)
                # tmp_el.append([remove_tr(tm_s[0]), tm_s[1]])
                # tmp_el.append(remove_tr(i).split('/'))
                tmp_el.append(i.split('/'))
                # tmp_el.add(i.split('/'))
            ret.append(tmp_el)
    else:
        with open(fname, 'r') as f:
            ret = json.load(f)
    fin = []
    c=0
    oc=0
    # iii = 0
    for article in ret:
        # iii += 1
        # print('on article ' + str(iii))
        tmp_art = []
        # for sd in article:
            # print(sd)
        for word in _enumerate([x[1] for x in article[:-1]]):
            tmp_art.append([ret[oc][c][0], word])
            fin.append(tmp_art)
            c+=1
        oc+=1
        c = 0
    return ret
    # return fin # enumeration done in prep step invalidates patterns
    # could always just return [fin, ret] and pp with fin, find pats with ret
            
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
            # else:
            elif tmp_str not in ret[tmp_type][1]: # to avoid duplicates
                ret[tmp_type][0] += 1
                ret[tmp_type][1].append(tmp_str)
                # ret[tmp_type][1] = tmp_str
    updated = {}
    tmp_str = ''
    for i in ret:
        for wrd in _enumerate(i.split(' ')):
            tmp_str += wrd + ' '
        updated[tmp_str[:-4]] = ret[i] # terrible workaround for the ' _0' that would show up
        # don't plan on fixing tho
        tmp_str = ''
    # return ret
    return updated

def pp_chat(pats, min_occ=1, rev=False, must_include = [''], prnt=True):
    relevant = []
    relev = {}
    def has_items(strn, items):
        for i in items:
            if strn.find(i) == -1: return False
        return True
    pat_id_sorted = sorted(pats, key=lambda k: pats[k][0], reverse=rev) # sort by num of occurences
    c=0
    for i in pat_id_sorted:
        if pats[i][0] >= min_occ and has_items(i, must_include):
            if prnt: print(str(c) + ' : ' + i + ' : ' + str(pats[i][0]) + ' : ' + pats[i][1][0])
            # relevant.append(pats[i][1])
            # relevant.append({i: 0})
            # relev[i] = 0
            relev[i] = pats[i]
            # relev
            c+=1
    # for i in relevant:
        # relev[i[]
    # return [c, relevant, pats] #pats is also returned so this can act as a filter for create_pat_chat
    # return [c, relev, pats] #pats is also returned so this can act as a filter for create_pat_chat
    return [c, relev]

def create_pat_chat(pp_ch):
    def add_num(strn):
        ret = ''
        spl = strn.split(' ')
        for i in range(len(spl)):
            ret += ('(' + str(i) + ' : ' + spl[i] + '), ')
        return ret
    # def add_num_dups(strn):
        # return  strn + 'dup'
    #trying to add nums to original find_pat or prep_chat
    patterns = []
    inp = ''
    c = 0
    # for pat in pp_ch[2]: #this is unfiltered. bad.
    for pat in pp_ch[1]:
        # pat = add_num_dups(pat)
        c+=1
        # print('now on pattern number ' + str(c) + '/' + str(len(pp_ch[2])))
        print('now on pattern number ' + str(c) + '/' + str(len(pp_ch[1])) + ' with ' + str(len(pp_ch[1][pat][1])) + ' elements')
        # why does the above print number of elements to be min_occ?
        if inp == 'q': next
        good = []
        # for occurence in pp_ch[2][pat][1]: # [2].. ? actually doesnt make a diff
        for occurence in pp_ch[1][pat][1]:
            print(add_num(occurence))
            inp = input('enter order - div by "|" ')
            # for some reason q and st do the same thing
            if inp == 'q': break # stop asking about the current pattern
            if inp == 's':
                inp = ''
                next 
            if inp == 'st': # stop the current pattern selection here and count entries
                inp = ''
                break
            good.append(inp)
        checker = {x: 0 for x in good}
        # for i in {checker[x]: 0 for x in good}:
        # print(good)
        # for i in checker:
        for i in good:
            checker[i] += 1
        # print(checker)
        wip = ''
        for i in checker:
            if checker[i] >= 2:
                # print('ayy ' + str(i) + ' appeared ' + str(checker[i]))
                # for cha in checker[i]:
                for cha in i:
                    # won't work for pat lengths over 9
                    if cha == '|': wip += '| '
                    else: wip += (pat.split(' ')[int(cha)] + ' ')
        if wip != '': patterns.append(pat + '-> ' + wip)
    #add an example with each element of patterns
    return patterns
       #i have 'NNP VBZ NN' and 21|0
       # pos.split(' ')

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
