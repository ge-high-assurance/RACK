import config
import logging
from util.logger import logger
import entity_extraction

class RequirementsParser:

    entity_extractor = None

    def __init__(self, config_file_path: str):
        config_obj = config.Config(config_file_path)
        self.entity_extractor = entity_extraction.EntityExtraction(config_obj)

    def get_entities(self, text: str):
        subject = []
        action = []
        condition = []

        # extract entities (subject, action, condition) from the text 
        # using the Named Entity Recognition model
        entity_dict = self.entity_extractor.extract(text)

        # loop through the extracted entities and split into subject, action, and condition
        if 'entities' in entity_dict:
            entities = entity_dict['entities']
        
            for entity in entities:
                entity_text = entity['text']
                entity_type = entity['type']
            
                if entity_type == 'Subject':
                    subject.append(entity_text)
                elif entity_type == 'Condition':
                    condition.append(entity_text)
                elif entity_type == 'Action':
                    action.append(entity_text)
        
        return {'subject': subject, 'action': action, 'condition': condition}