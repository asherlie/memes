import requests
import random
import os
import sys

from textblob import TextBlob

class Meme:

        base = "https://api.imgflip.com/"
        reference = requests.get(base + "get_memes").json()
        ref = {x['name'].upper(): x['id'] for x in reference['data']['memes']}

        def meme_to_id(self, mn):
                up = mn.upper()
                if up in self.ref:
                        return self.ref[up]
                else:
                        return 'invalid meme name'
                
        def gen_meme(self, meme_id, top, bottom):
                parameters = {
                                'template_id': meme_id,
                                'username': '_____________',
                                'password': '_____________',
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

        def memes_from_f(self, delim_file):
            with open(delim_file, 'r', encoding='utf-8') as f:
                fi = f.read()
            f_memes = fi.split('\n')[:-1]
            memes = []
            for i in f_memes:
                memes.append(self.parse(i))
            return memes

        def choose_meme_from_m(self, mm, art):
            mtu = mm[random.randrange(0, len(mm))]
            m_type = mtu[0][0]
            if len(mtu[0]) == 2:
                if not self.sent_pos(art):
                    m_type = mtu[0][1]
            return (m_type, (mtu[1][0], mtu[1][1]))

        def gen_meme_from_m(self, clarified):
            return self.gen_meme(self.meme_to_id(clarified[0]), clarified[1][0], clarified[1][1])

        def sent_pos(self, strng):
            def clean_str(st):
                ret = ''
                for i in st:
                    if ord(i) <= 128:
                        ret += i
                    else: ret += ' '
                return ret
            return TextBlob(strng).sentiment.polarity > 0
