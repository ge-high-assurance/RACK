from flair.data import Sentence
from flair.models import SequenceTagger
import config

class EntityExtraction:

    model = None

    def __init__(self, config):
        self.model = SequenceTagger.load_from_file(config.model_file_path)

    def extract(self, text: str):
        sentence = Sentence(text)
        self.model.predict(sentence)
        entity_dict = sentence.to_dict(tag_type='ner')
        return entity_dict