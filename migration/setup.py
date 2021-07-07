from setuptools import setup, find_packages

setup(
    name="RACK migration tool suite",
    scripts=[
        "rack_crawl/rack_crawl",
        "rack_migrate/rack_migrate",
    ],
    packages=find_packages(),
)
