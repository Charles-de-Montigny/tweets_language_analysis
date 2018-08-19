# -*- coding: utf-8 -*-
from subprocess import call
import os

def main():
    """
    Calls R script from python.
    """
    call('Rscript src/data/twitter_scraper.R')


if __name__ == '__main__':
    main()
