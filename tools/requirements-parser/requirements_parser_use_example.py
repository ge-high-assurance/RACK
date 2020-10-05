import requirements_parser as parser

if __name__ == '__main__':

    sents = ['Input Thread shall initialize the park_count to 0 on powerup.',
             'Input Thread shall check for a incoming UDP message on port 62000. ']

    for sent in sents:
        dict_ = parser.parse_requriments(sent)

        if 'inputs' in dict_:
            inputs = dict_["inputs"]
            print(inputs)

        if 'outputs' in dict_:
            outputs = dict_["outputs"]
            print(outputs)

