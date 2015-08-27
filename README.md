[![Build Status](https://travis-ci.org/EHRI/ehri-frontend.svg?branch=master)](https://travis-ci.org/EHRI/frontend)

Front-end for  the [EHRI REST](https://github.com/EHRI/ehri-rest) web service.

This app has a few dependencies in addition to the backend:

 - A MySQL database or compatible
 - Solr, running configurated as per the config in [EHRI Search Tools](https://github.com/EHRI/ehri-search-tools)
 - The Java-based command line tool for converting JSON streams between the database and Solr, also
   in the search tools repository
   
For development you also need:
   
 - NodeJS installed (for asset compilation)

See the [setup docs](docs/install.md) for instructions on setting up the dev environment using Docker.