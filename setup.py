import os
from setuptools import setup

VERSION = '0.0.1'

def read(fname):
    return open(os.path.join(os.getcwd(), fname)).read()

setup(
    name = "PyExpressions",
    version = VERSION,
    author = "Alvin Wan, Nathan Pucheril",
    description = ("Mathematical expressions library"),
    license = "Apache",
    url = "http://github.com/alvinwan/PyExpressions",
    packages = ['pyexpressions'],
    long_description = read('README.md'),
    install_requires = list(filter(bool, read('requirements.txt').split('\n'))),
    download_url = 'https://github.com/alvinwan/PyExpressions/archive/%s.zip' % VERSION,
    classifiers = [
        "Development Status :: 3 - Alpha",
        "Topic :: Utilities",
    ],
)
