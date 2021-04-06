from colorama import Fore, Style
import logging
from typing import Dict

class CustomFormatter(logging.Formatter):
    """Add custom styles to our log"""

    FORMATS: Dict[int, str] = {
        logging.DEBUG: "[DEBUG]",
        logging.INFO: f"{Fore.CYAN}[INFO]{Style.RESET_ALL}",
        logging.WARNING: f"{Fore.YELLOW}[WARNING]{Style.RESET_ALL}",
        logging.ERROR: f"{Fore.RED}[ERROR]{Style.RESET_ALL}",
        logging.CRITICAL: f"{Style.BRIGHT}{Fore.RED}[CRITICAL]{Style.RESET_ALL}",
    }

    def format(self, record: logging.LogRecord) -> str:
        prefix = self.FORMATS.get(record.levelno)
        if not prefix:
            return "%(message)s"
        formatter = logging.Formatter(f"{prefix} %(message)s", "%H:%M:%S")
        return formatter.format(record)

stream_handler = logging.StreamHandler()
stream_handler.setFormatter(CustomFormatter())
