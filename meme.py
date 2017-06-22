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
                                'username': '____',
                                'password': '____',
                                'text0': top, #top text for meme
                                'text1': bottom, #bottom text for meme
                                }
                response = requests.post(self.base + "caption_image", params=parameters)
                return response
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
                else: return "incorrect meme template"
if __name__ == '__main__':
        print(Meme().spit(sys.argv[1]))
