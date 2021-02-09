import config
import logging
from util.logger import logger
import requests
import json
import csv

config = config.Config('resources/config.ini')
logger.setup_log()


def get_outgoing_edges(edges, words: list, word_index):
    """

    :param edges:
    :param words:
    :param word_index:
    :return:
    """
    allow = ['amod', 'compound']
    word_list = [word_index]
    for edge in edges:
        source = edge['source']
        if source == word_index:
            destination = edge['destination']
            relation = edge['relation']
            if relation in allow:
                # print(words[word_index], words[destination], relation)
                word_list.append(destination)
    word_list.sort()
    return word_list


def get_list_from_dictionary(key, dictionary):
    """
    Given a dictionary and a key whose value is a list, check if key exists in the dictionary
    Return the list if key exist, else return empty value.
    :param key: The name of the key whose value needs to be extracted
    :param dictionary: Dictionary from which tke value has to be extracted for the given key
    :return: value (list) associated with the key; empty list if key is absent
    """
    list_ = []
    if key in dictionary:
        list_ = dictionary[key]
    return list_


def get_automates_parse(text: str):
    """
    Get a NLP parse of the sentence from the AutoMATES Text Reading service.
    This method makes a REST call to the text reading service and returns the nlp parse
    :param text: The text/string to be parsed
    :return: List of sentence objects.
    """
    results_json = {}
    sentences = []
    payload = {'text': text}
    headers = {'Content-Type': 'application/json'}

    response = requests.request("POST", config.AutomatesServiceURL, headers=headers, data=json.dumps(payload))

    if response.status_code == 200:
        try:
            results_json = response.json()
        except:
            logging.exception("Failed to parse JSON results from Auotomates NLP parse service")

    else:
        error_line = "Service call to Auotomates NLP parser failed. Status Code: " + str(response.status_code) \
                     + " Message: " + response.text
        logging.error(error_line)

    if 'documents' in results_json:
        doc_obj = results_json['documents']
        for key in doc_obj.keys():
            if 'sentences' in doc_obj[key]:
                sentences = doc_obj[key]['sentences']

    return sentences


def get_system_name(words: list, chunks: list):
    """
    This method extracts the system name that governs the requirement.
    System name are all words in noun phrases preceding the first verb phrase
    :param words: List of words in a sentence
    :param chunks: List of chunk tags for each word in the sentence
    :return: the system name as string or empty if not found
    """

    system_name = ''
    for idx in range(0, len(chunks)):
        chunk_tag = chunks[idx]
        if chunk_tag != 'B-VP':
            system_name = system_name + ' ' + words[idx]
        else:
            break
    return system_name.strip()


def get_action_verb(words: list, tags: list, chunks: list):
    """
    Given a list of words, their part of speech (POS) tags and chunks,
    this method extracts the action verb in the requirement sentence.
    Action verb is defined as the first verb or noun in the first verb phrase in the sentence.
    :param words: List of words in a sentence
    :param tags: List of POS tags for each word in the sentence
    :param chunks: List of chunk tags for each word in the sentence
    :return: action_verb as string. Returns None if action_verb is not found
    """

    action_verb = None

    for i in range(0, len(chunks)):
        chunk = chunks[i]
        if chunk == 'B-VP' or chunk == 'I-VP':
            tag = tags[i]
            if tag == 'VB' or tag == 'NN':
                action_verb = words[i]
                break

    return action_verb


def get_dependency_graph_edges(sentence):
    """
    This mehtod extracts the edges object from the univeral-enhanced dependecy graph of the sentence
    :param sentence: Automates parse JSON object of the sentence
    :return: List of dependency graph edge objects
    """
    edges = []
    if 'graphs' in sentence:
        if 'universal-enhanced' in sentence['graphs']:
            # print(sentence['graphs']['universal-enhanced'], "\n")
            if 'edges' in sentence['graphs']['universal-enhanced']:
                edges = sentence['graphs']['universal-enhanced']['edges']
    return edges


def process_sentence(sentence):
    """
    Process a requirements sentence and extract system name, inputs, outputs
    :param sentence: Sentence/text to be processed
    :return: A dictionary with system name, list of inputs, list of outputs
    {"system_name" : "str", "inputs": ["str"], "outputs": ["str"]}
    """

    inputs = []
    outputs = []

    # Retrieve the words in the sentence
    words = get_list_from_dictionary('words', sentence)
    logging.info(words)

    # Retrieve part of speech tags for each word in the sentence
    tags = get_list_from_dictionary('tags', sentence)

    # Retrieve chunks/phrases in the sentence
    chunks = get_list_from_dictionary('chunks', sentence)

    # Extract system name from the requirement sentence
    system_name = get_system_name(words, chunks)

    # Extract requirement action verb
    action_verb = get_action_verb(words, tags, chunks)

    # Extract dependency graph edges
    edges = get_dependency_graph_edges(sentence)

    # Loop through the dependency graph edges to extract inputs and outputs
    for edge in edges:
        source = edge['source']
        destination = edge['destination']
        relation = edge['relation']

        if 'nmod_' in relation and words[source] == action_verb:
            # print("match:", words[source], words[destination], relation)
            word_list = get_outgoing_edges(edges, words, destination)
            str_ = ''
            for word_idx in word_list:
                str_ = str_ + ' ' + words[word_idx]
            # print("Input:", str_)
            inputs.append(str_.strip())

        elif 'dobj' in relation and words[source] == action_verb:
            # print("match:", words[source], words[destination], relation)
            word_list = get_outgoing_edges(edges, words, destination)
            str_ = ''
            for word_idx in word_list:
                str_ = str_ + ' ' + words[word_idx]
            # print("Output:", str_)
            outputs.append(str_.strip())

    return {"system_name": system_name, "inputs": inputs, "outputs": outputs}


# TODO: This is designed to deal with only sentence per requirement
def parse_requirements(text: str):
    """
    This method is the main API call to parse requirements.
    This method obtains the NLP parse of the text
    Loops through each sentence to extract system name, inputs and outputs.
    :param text: Sentence to be processed
    :return: A dictionary with system name, list of inputs, list of outputs
    """
    dict_ = {}
    sentences = get_automates_parse(text)
    for sentence in sentences:
        dict_ = process_sentence(sentence)
    return dict_
