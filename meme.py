import requests
import random
import os
import sys

class Meme:

        base = "https://api.imgflip.com/"
        reference = requests.get(base + "get_memes").json()
        ref = {}
        for i in reference['data']['memes']:
                ref[i['name']] = i['id'] 

        def to_cam(self, wrd):
                new_str = ''
                for i in wrd.split(' '):
                        for ltr in range(len(i)):
                                if ltr == 0: new_str += (' ' + i[ltr].upper())
                                else: new_str += i[ltr]
                return new_str[1:len(new_str)]
        def meme_to_id(self, meme_name):
                if self.to_cam(meme_name) in self.ref: return self.ref[self.to_cam(meme_name)]
                return "you fucked up"  


        def gen_meme(self, meme_id, top, bottom):
                parameters = {
                                'template_id': meme_id,
                                'username': 'ubhacking2016',
                                'password': 'ubhacking2016',
                                'text0': top, #top text for meme
                                'text1': bottom, #bottom text for meme
                                }
                response = requests.post(self.base + "caption_image", params=parameters)
                return response

        def parse(self, strng):
            ops = strng.split('&&')
            ret = []
            for i in range(len(ops)):
                tmp_meme = [[], ()]
                # [type, content]
                t_c = ops[i].split('^^')
                tmp_meme[0] = t_c[0].split('|')
                # print(t_c)
                tmp_meme[1] = tuple(t_c[1].split('#%'))
                ret.append(tuple(tmp_meme))
            return ret

        def parse_delims(self, strng):
                def real_split(st):
                        return True
                        return st.split('&&')[1] != '"]'
                if strng.find('&&') != -1 and real_split(strng): to_parse = strng.split('&&')[random.randrange(len(strng.split('&&'))-1)]
                else: to_parse = strng
                m_type = to_parse.split('^^')[0].split('|')
                top = []; bot = []
                top_text = ''; bot_text = ''
                if to_parse.find('@@') != -1:
                        for i in to_parse.split('^^')[1].split('@@'):
                                top.append(i.split('#%')[0])
                                bot.append(i.split('#%')[1])
                else:
                        top.append(to_parse.split('^^')[1].split('#%')[0])
                        bot.append(to_parse.split('^^')[1].split('#%')[1])
                # m_type is a [] bc first has pos sentiment, second neg
                # this returns ([pos_sent, neg_sent], ([pos_top, neg_top], [pos_bot, neg_bot]))
                # TODO: there will be cases where len(m_type) > 1 and len(top) == 1 or len(bot) == 1
                # handle these. if len of top or bot is 2 and so is m_type,
                # choose the corresponding top or bot. if either doenst have both, use the only one
                return (m_type, (top, bot))

        def select_top_bot(self, mm):
                m_type = mm[0]
                top = mm[1][0]
                bot = mm[1][1]
                if len(m_type) != 1:
                        sent = requests.post('http://text-processing.com/api/sentiment/', 'text=' + strng).json()
                        if sent['probability']['pos'] > sent['probability']['neg']: 
                            m_type = m_type[0]
                            top_text = top[0] #can just do this bc it defaults to [0] anyway
                            bot_text = bot[0] #and [0] is positive option
                        else: 
                            m_type = m_type[1]
                            if len(top) == 2:
                                top_text = top[1]
                                bot_text = bot[1]
                            else:
                                top_text = top[0]
                                bot_text = bot[0]
                else:
                    m_type = m_type[0]
                    top_text = top[0]
                    bot_text = bot[0]
                return (m_type, (top_text, bot_text))
            

        def memes_from_f(self, delim_file):
            with open(delim_file, 'r') as f:
                fi = f.read()
            f_memes = fi.split('\n')[:-1]
            memes = []
            for i in f_memes:
                memes.append(self.parse(i))
            return memes

                
        def spit(self, strng):
                a = os.popen('./meme "' + strng + '"').read().encode('ascii', 'ignore').decode().strip('&&[()]\n')
                if a.find('&&') != -1: to_parse = a.split('&&')[random.randrange(len(a.split('&&'))-1)]
                else: to_parse = a
                m_type = to_parse.split('^^')[0].split('|')

                top = []; bot = []
                top_text = ''; bot_text = ''
                if to_parse.find('@@') != -1:
                        for i in to_parse.split('^^')[1].split('@@'):
                                top.append(i.split('#%')[0])
                                bot.append(i.split('#%')[1])
                else:
                        top.append(to_parse.split('^^')[1].split('#%')[0])
                        bot.append(to_parse.split('^^')[1].split('#%')[1])

                if len(m_type) != 1:
                        sent = requests.post('http://text-processing.com/api/sentiment/', 'text=' + strng).json()
                        if sent['probability']['pos'] > sent['probability']['neg']: 
                            m_type = m_type[0]
                            top_text = top[0] #can just do this bc it defaults to [0] anyway
                            bot_text = bot[0] #and [0] is positive option
                        else: 
                            m_type = m_type[1]
                            if len(top) == 2:
                                top_text = top[1]
                                bot_text = bot[1]
                            else:
                                top_text = top[0]
                                bot_text = bot[0]
                else:
                    m_type = m_type[0]
                    top_text = top[0]
                    bot_text = bot[0]
                mem = self.gen_meme(self.meme_to_id(m_type), top_text, bot_text).json()
                if 'data' in mem:
                    return mem['data']['url']
                else: 
                    print(m_type)
                    return "incorrect meme template"
if __name__ == '__main__':
        print(Meme().spit(sys.argv[1]))
