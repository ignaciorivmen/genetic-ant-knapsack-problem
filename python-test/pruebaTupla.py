#!/usr/bin/python
# -*- coding: UTF-8 -*-
import string
import time
def doitSimple(listWords, textProcessed):
    result = 0
    for w in textProcessed:
        if (w[1]==listWords[0][1]):
            if (w[0].lower()==listWords[0][0]):#There is two if because lower function is really expensive 
                result+=1
    return result

def doitHard(listWords, textProcessed):
    result = 0
    lenListWords = len(listWords)
    nListWords = lenListWords-1
    lenTextProcessed = len(textProcessed)

    for i,w in enumerate(textProcessed):
        controler = True
#       HAY QUE CONTROLAR QUE SI ENTRA, EN TEXTPROCESSED HAYA SUFICIENTES STRING DESPUES
#       PARA COMPROBAR CON TODOS LOS STRINGS DE LISTWORDS. SINO, INDEXBOUND ERROR
        if (w[1]==listWords[0][1] and lenTextProcessed-i>=lenListWords):
            for x in range(0, nListWords):
                if (textProcessed[i+x][1]==listWords[x][1]):
                    if (textProcessed[i+x][0].lower()==listWords[x][0]):
                        controler = controler and True
                    else:
                        controler = controler and False
                else:
                    controler = controler and False
            if controler:
                result+=1
    return result

def wordSeparator(text):
    textProcessed = []
    firstC = 0
    lettersNumbers = string.ascii_letters + string.digits + "'"
    lettersSpace = string.ascii_letters + string.whitespace
    spaces = string.whitespace
    lenText = len(text)
    for i,c in enumerate(text):
        if c in spaces:
            textProcessed.append((text[firstC:i],i-firstC))
            textProcessed.append((text[i:i+1],'1'))
            firstC = i + 1
        elif c=="'" and i!=lenText-1 and i!=0 and (text[i-1] not in lettersSpace or text[i+1] not in lettersSpace):
                textProcessed.append((text[firstC:i], i-firstC))
                textProcessed.append((text[i:i+1],'1'))
                firstC = i + 1
        elif c not in lettersNumbers:
            textProcessed.append((text[firstC:i], i-firstC))
            textProcessed.append((text[i:i+1],'1'))
            firstC = i + 1
        elif i==lenText-1:
            textProcessed.append((text[firstC:i+1],i+1-firstC))
    return textProcessed

def to_unicode_or_bust(obj, encoding='utf-8'):
    if isinstance(obj, basestring):
        if not isinstance(obj, unicode):
            obj = unicode(obj, encoding)
    return obj

def CountOccurencesInText(word,text):
    """Number of occurences of word (case insensitive) in text"""
    #This does not pass the unittests:
    to_unicode_or_bust(word)
    word = word.lower()
    result = -1
    listWords = wordSeparator(word)
    if (len(listWords)==0 or len(word)>len(text)):
        print "There is no word or word is bigger than text!"
    else:
        to_unicode_or_bust(text)
        textProcessed = wordSeparator(text)
        if (len(listWords)==1):
            doitSimpleTime = time.time()
            result = doitSimple(listWords, textProcessed)
        else:
            result = doitHard(listWords, textProcessed)
    return result

def testCountOccurencesInText():
    """ Test the CountOccurencesInText function"""
    text="""Georges is my name and I like python. Oh ! your name is georges? And you like Python!
Yes is is true, I like PYTHON
and my name is GEORGES"""
    # test with a little text.
    assert( 3 == CountOccurencesInText("Georges",text) )
    assert( 3 == CountOccurencesInText("GEORGES",text) )
    assert( 3 == CountOccurencesInText("georges",text) )
    assert( 0 == CountOccurencesInText("george",text) )
    assert( 3 == CountOccurencesInText("python",text) )
    assert( 3 == CountOccurencesInText("PYTHON",text) )
    assert( 2 == CountOccurencesInText("I",text) )
    assert( 0 == CountOccurencesInText("n",text) )
    assert( 1 == CountOccurencesInText("true",text) )    
    # regard ' as text:
    assert ( 0 == CountOccurencesInText ( "maley", "John O'maley is my friend" ) )
    # Test it but with a BIG length file. (we once had a memory error with this...)
    text = """The quick brown fox jump over the lazy dog.The quick brown fox jump over the lazy dog.""" * 500
    text += """The quick brown fox jump over the lazy dog.The quick brown Georges jump over the lazy dog."""
    text += """esrf sqfdg sfdglkj sdflgh sdflgjdsqrgl """ * 4000
    text += """The quick brown fox jump over the lazy dog.The quick brown fox jump over the lazy python.""" 
    text += """The quick brown fox jump over the lazy dog.The quick brown fox jump over the lazy dog.""" * 500
    text += """The quick brown fox jump over the lazy dog.The quick brown Georges jump over the lazy dog."""
    text += """esrf sqfdg sfdglkj sdflgh sdflgjdsqrgl """ * 4000
    text += """The quick brown fox jump over the lazy dog.The quick brown fox jump over the lazy python.""" 
    text += """The quick brown fox jump over the lazy dog.The quick brown fox jump over the lazy dog.""" * 500
    text += """The quick brown fox jump over the lazy dog.The quick brown Georges jump over the lazy dog."""
    text += """esrf sqfdg sfdglkj sdflgh sdflgjdsqrgl """ * 4000
    text += """The quick brown fox jump over the lazy dog.The quick brown fox jump over the lazy python.""" 
    text += """The quick brown fox jump over the true lazy dog.The quick brown fox jump over the lazy dog."""
    text += """The quick brown fox jump over the lazy dog.The quick brown fox jump over the lazy dog.""" * 500
    text += """ I vsfgsdfg sfdg sdfg sdgh sgh I sfdgsdf"""
    text += """The quick brown fox jump over the lazy dog.The quick brown fox jump over the lazy dog.""" * 500
    assert( 3 == CountOccurencesInText("Georges",text) )
    assert( 3 == CountOccurencesInText("GEORGES",text) )
    assert( 3 == CountOccurencesInText("georges",text) )
    assert( 0 == CountOccurencesInText("george",text) )
    assert( 3 == CountOccurencesInText("python",text) )
    assert( 3 == CountOccurencesInText("PYTHON",text) )
    assert( 2 == CountOccurencesInText("I",text) )
    assert( 0 == CountOccurencesInText("n",text) )
    assert( 1 == CountOccurencesInText("true",text) )
    assert( 0 == CountOccurencesInText("reflexion mirror",
     "I am a senior citizen and I live in the Fun-Plex 'Reflexion Mirror' in Sopchoppy, Florida") )
    assert( 1 == CountOccurencesInText("'reflexion mirror'",
     "I am a senior citizen and I live in the Fun-Plex 'Reflexion Mirror' in Sopchoppy, Florida") )
    assert( 1 == CountOccurencesInText("reflexion mirror",
     "I am a senior citizen and I live in the Fun-Plex (Reflexion Mirror) in Sopchoppy, Florida") )
    assert( 1 == CountOccurencesInText("reflexion mirror",
     "Reflexion Mirror\" in Sopchoppy, Florida") )
    assert( 1 == CountOccurencesInText("reflexion mirror",
     u"I am a senior citizen and I live in the Fun-Plex «Reflexion Mirror» in Sopchoppy, Florida") )
    assert( 1 == CountOccurencesInText("reflexion mirror",
     u"I am a senior citizen and I live in the Fun-Plex \u201cReflexion Mirror\u201d in Sopchoppy, Florida") )
    assert( 1 == CountOccurencesInText("legitimate",
     u"who is approved by OILS is completely legitimate: their employees are of legal working age") )
    assert( 0 == CountOccurencesInText("legitimate their",
     u"who is approved by OILS is completely legitimate: their employees are of legal working age") )
    assert( 1 == CountOccurencesInText("get back to me",
     u"I hope you will consider this proposal, and get back to me as soon as possible") )
    assert( 1 == CountOccurencesInText("skin-care",
     u"enable Delavigne and its subsidiaries to create a skin-care monopoly") )
    assert( 1 == CountOccurencesInText("skin-care monopoly",
     u"enable Delavigne and its subsidiaries to create a skin-care monopoly") )
    assert( 0 == CountOccurencesInText("skin-care monopoly in the US",
     u"enable Delavigne and its subsidiaries to create a skin-care monopoly") )
    assert( 1 == CountOccurencesInText("get back to me",
     u"When you know:get back to me") )
    assert( 1 == CountOccurencesInText("don't be left" , """emergency alarm warning.
Don't be left unprotected. Order your SSSS3000 today!""" ) )
    assert( 1 == CountOccurencesInText("don" , """emergency alarm warning.
Don't be left unprotected. Order your don SSSS3000 today!""" ) )
    assert( 1 == CountOccurencesInText("take that as a 'yes'",
     "Do I have to take that as a 'yes'?") )
    assert( 1 == CountOccurencesInText("don't take that as a 'yes'",
     "I don't take that as a 'yes'?") )        
    assert( 1 == CountOccurencesInText("take that as a 'yes'",
     "I don't take that as a 'yes'?") )
    assert( 1 == CountOccurencesInText("don't",
     "I don't take that as a 'yes'?") )
    assert( 1 == CountOccurencesInText("attaching my c.v. to this e-mail",
     "I am attaching my c.v. to this e-mail." ))
    assert ( 1 == CountOccurencesInText ( "Linguist", "'''Linguist Specialist Found Dead on Laboratory Floor'''" ))
    assert ( 1 == CountOccurencesInText ( "Linguist Specialist", "'''Linguist Specialist Found Dead on Laboratory Floor'''" ))
    assert ( 1 == CountOccurencesInText ( "Laboratory Floor", "'''Linguist Specialist Found Dead on Laboratory Floor'''" ))
    assert ( 1 == CountOccurencesInText ( "Floor", "'''Linguist Specialist Found Dead on Laboratory Floor'''" ))
    assert ( 1 == CountOccurencesInText ( "Floor", "''Linguist Specialist Found Dead on Laboratory Floor''" ))        
    assert ( 1 == CountOccurencesInText ( "Floor", "__Linguist Specialist Found Dead on Laboratory Floor__" ))
    assert ( 1 == CountOccurencesInText ( "Floor", "'''''Linguist Specialist Found Dead on Laboratory Floor'''''" ))
    assert ( 1 == CountOccurencesInText ( "Linguist", "'''Linguist Specialist Found Dead on Laboratory Floor'''" ))
    assert ( 1 == CountOccurencesInText ( "Linguist", "''Linguist Specialist Found Dead on Laboratory Floor''" ))        
    assert ( 1 == CountOccurencesInText ( "Linguist", "__Linguist Specialist Found Dead on Laboratory Floor__" ))
    assert ( 1 == CountOccurencesInText ( "Linguist", "'''''Linguist Specialist Found Dead on Laboratory Floor'''''" ))
    assert ( 1 == CountOccurencesInText ( "Floor", """Look: ''Linguist Specialist Found Dead on Laboratory Floor'' is the headline today."""))

SampleTextForBench = """
A Suggestion Box Entry from Bob Carter

Dear Anonymous,

I'm not quite sure I understand the concept of this 'Anonymous' Suggestion Box. If no one reads what we write, then how will anything ever
change?

But in the spirit of good will, I've decided to offer my two cents, and hopefully Kevin won't steal it! (ha, ha). I would really like to
see more varieties of coffee in the coffee machine in the break room. 'Milk and sugar', 'black with sugar', 'extra sugar' and 'cream and su
gar' don't offer much diversity. Also, the selection of drinks seems heavily weighted in favor of 'sugar'. What if we don't want any suga
r?

But all this is beside the point because I quite like sugar, to be honest. In fact, that's my second suggestion: more sugar in the office.
Cakes, candy, insulin, aspartame... I'm not picky. I'll take it by mouth or inject it intravenously, if I have to.

Also, if someone could please fix the lock on the men's room stall, that would be helpful. Yesterday I was doing my business when Icarus ne
arly climbed into my lap.

So, have a great day!

Anonymously,
 Bob Carter
"""


def doit():
    """Run CountOccurencesInText on a few examples"""
    i = 0
    for x in xrange(400):
        i+= CountOccurencesInText("word" , SampleTextForBench)
        i+= CountOccurencesInText("sugar" , SampleTextForBench)
        i+= CountOccurencesInText("help" , SampleTextForBench)
        i+= CountOccurencesInText("heavily" , SampleTextForBench)
        i+= CountOccurencesInText("witfull" , SampleTextForBench)
        i+= CountOccurencesInText("dog" , SampleTextForBench)
        i+= CountOccurencesInText("almost" , SampleTextForBench)
        i+= CountOccurencesInText("insulin" , SampleTextForBench)
        i+= CountOccurencesInText("attaching" , SampleTextForBench)
        i+= CountOccurencesInText("asma" , SampleTextForBench)
        i+= CountOccurencesInText("neither" , SampleTextForBench)
        i+= CountOccurencesInText("won't" , SampleTextForBench)
        i+= CountOccurencesInText("green" , SampleTextForBench)
        i+= CountOccurencesInText("parabole" , SampleTextForBench)
    print i



#Start the tests     
if __name__ == '__main__':
    start_time = time.time()
    #I need to pass the test:
    try:
        testCountOccurencesInText()
    except:
        print "Error !"
        raise
    print "Tests passed"
    print time.time() - start_time
    #I need to be fast as well:
    import profile
    doit_time = time.time()
    profile.run('doit()')
    print "Doit time"
    print time.time() - doit_time
