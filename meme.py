import requests
import random
import os
import sys

class Meme:

        base = "https://api.imgflip.com/"

        def meme_to_id(self, meme_name):
                response = requests.get(self.base + "get_memes").json()
#               if response['success']  check if success is true
                for meme in response['data']['memes']:
                        if (meme['name'].lower()).find(meme_name.lower()) != -1:
                                return meme['id']
                return "you fucked up"  


        def gen_meme(self, meme_id, top, bottom):
                parameters = {
                                'template_id': meme_id,
                                'username': '____',
                                'password': '____',
                                'text0': top, #top text for meme
                                'text1': bottom, #bottom text for meme
#                               self.meme_to_id(meme    
                                }
                response = requests.post(self.base + "caption_image", params=parameters)
                return response
        def spit(self, strng):
                a = os.popen('./meme "' + strng + '"').read()
                if a.find('&&') != -1:
                        to_parse = a.split('&&')[random.randrange(len(a.split('&&'))-1)]
                        m_type = to_parse.split('^^')[0].split('|')
                        top_text = to_parse.split('^^')[1]
                        bot_text = to_parse.split('^^')[2]

                        if len(m_type) != 1:
                                sent = requests.post('http://text-processing.com/api/sentiment/', 'text=' + strng).json()
                                # print(sent)
                                if sent['probability']['pos'] > sent['probability']['neg']: 
                                    m_type = m_type[0]
                                else: 
                                    m_type = m_type[1]
                        else: m_type = m_type[0]

                        mem = self.gen_meme(self.meme_to_id(m_type), top_text, bot_text).json()
                        if 'data' in mem: print(mem['data']['url'])
                        else: print("nione")
                else: print("none")
if __name__ == '__main__':
        Meme().spit(sys.argv[1])
