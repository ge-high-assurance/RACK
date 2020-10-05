import configparser


class Config:
    def __init__(self, config_file_path):
        config = configparser.ConfigParser()
        config.read(config_file_path)
        self.ChunksServiceURL = config["DEFAULT"]["ChunksServiceURL"]
        self.ChunksSubsetServiceURL = config["DEFAULT"]["ChunksSubsetServiceURL"]
        self.AutomatesServiceURL = config["DEFAULT"]["AutomatesServiceURL"]