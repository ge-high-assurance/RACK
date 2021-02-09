import requirements_parser

def print_extracted_info(list_, tag):
    for item in list_:
        print(tag + ': ' + item)

if __name__ == '__main__':

    sents = ['Input Thread shall initialize the park_count to 0 on powerup.',
             'Input Thread shall check for a incoming UDP message on port 62000. ']

    config_file_path = 'resources/config.ini'
    parser = requirements_parser.RequirementsParser(config_file_path)

    for sent in sents:
        extracted_entities = parser.get_entities(sent)
        subjects = ''
        actions = ''
        conditions = ''
        
        if 'subject' in extracted_entities:
            subjects = extracted_entities['subject']
        if 'action' in extracted_entities:
            actions = extracted_entities['action']
        if 'condition' in extracted_entities:
            conditions = extracted_entities['condition']

        print('Text: ' + sent)
        print_extracted_info(subjects, 'subject')
        print_extracted_info(actions, 'action')
        print_extracted_info(conditions, 'condition')
        print()

