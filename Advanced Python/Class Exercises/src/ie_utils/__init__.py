"""
IE Titanic utils.
"""

__version__ = "0.1.0"  # semver.org

import sys
import pandas as pd


def tokenize(text, lower=False):
    if not text:
        raise ValueError("Cannot tokenize empty sentence")

    if lower is True:
        text = text.lower()

    return text.split()


def main():
    print(tokenize(sys.argv[1]))


if __name__ == "__main__":
    main()
