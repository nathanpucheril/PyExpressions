import os
from setuptools import setup

VERSION = 'v0.0.0.1dev'

def read(fname):
    return open(os.path.join(os.getcwd(), fname)).read()

setup(
    name = "PyExpressions",
    version = VERSION,
    author = "Alvin Wan, Nathan Pucheril",
    author_email = '',
    description = ("Mathematical expressions library"),
    license = "Apache",
    url = "http://github.com/alvinwan/PyExpressions",
    packages = ['pyexpressions'],
    install_requires = [],
    download_url = 'https://github.com/alvinwan/PyExpressions/archive/%s.zip' % VERSION,
    classifiers = [
        "Topic :: Utilities",
    ],
)
