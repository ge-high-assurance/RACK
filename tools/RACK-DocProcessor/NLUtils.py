from nltk.tokenize import word_tokenize

def word_tokensToString(tokens):
    tokensString = ""
    if len(tokens) > 0:
        #punctuation = [".", ",", "{", "}", "[", "]", ";", ":", "(", ")", "-", "\\", "/"]
        punctuation = [".", ",", ";", ":", "-", "\\", "/", "?"]
        brackets = [ "{", "}", "[", "]", ";", ":", "(", ")"]
        tokensString = tokens[0]
        i = 1
        while i < len(tokens):
            if tokens[i] in punctuation:
                tokensString += tokens[i]
            elif tokens[i] in brackets:
                tokensString += tokens[i]
            elif tokens[i-1] in brackets:
                tokensString += tokens[i]
            else:
                tokensString += " "+ tokens[i]
            i+=1

    return tokensString.lstrip()

def normalizeString(string):
    tokens = word_tokenize(string)
    return word_tokensToString(tokens)


if __name__ == '__main__':
    print("||"+normalizeString("1.2.3.0     Hello\n ? How is it going?  \n I am Fine.\n")+"||")
