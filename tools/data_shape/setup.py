#!/usr/bin/env python3

from setuptools import setup

setup(name='data_shape',
      version='0.1',
      description='RACK data shape',
      author='Eric Mertens',
      author_email='emertens@galois.com',
      packages=['data_shape'],
      scripts=[
            'bin/data_shape',
            'bin/collaboration'],
      install_requires=[
            'SPARQLWrapper',
            'graphviz'
      ],
      zip_safe=False)
