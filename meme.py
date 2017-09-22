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

        def meme_to_id(self, meme_name):
                def to_cam(wrd):
                        new_str = ''
                        for i in wrd.split(' '):
                                for ltr in range(len(i)):
                                        if ltr == 0: new_str += (' ' + i[ltr].upper())
                                        else: new_str += i[ltr]
                        return new_str[1:len(new_str)]
                if to_cam(meme_name) in self.ref: return self.ref[to_cam(meme_name)]
                return "you fucked up"  


        def gen_meme(self, meme_id, top, bottom):
                parameters = {
                                'template_id': meme_id,
                                'username': '____',
                                'password': '____',
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
            with open(delim_file, 'r') as f:
                fi = f.read()
            f_memes = fi.split('\n')[:-1]
            memes = []
            for i in f_memes:
                memes.append(self.parse(i))
            return memes

        def gen_meme_from_m(self, mm, art):
            mtu = mm[random.randrange(0, len(mm))]
            m_type = mtu[0][0]
            # type
            if len(mtu[0]) == 2:
                if self.sent_pos(art):
                    m_type = mtu[0][0]
                else:
                    m_type = mtu[0][1]
            return self.gen_meme(self.meme_to_id(m_type), mtu[1][0], mtu[1][1])

        def sent_pos(self, strng):
                sent = requests.post('http://text-processing.com/api/sentiment/', 'text=' + strng).json()
                return sent['probability']['pos'] > sent['probability']['neg']
