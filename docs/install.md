# Setting up the development environment

These instructions assume you're using the Docker-ised [backend](https://github.com/EHRI/ehri-rest) and [search]
(https://github.com/EHRI/ehri-search-tools) components, running on, respectively, ports 7474 and 8983. To use
without Docker, see the individual instructions for building and running the services.

## Prerequisites

 - [Docker](https://www.docker.com/)
 - MySQL (or compatible)
 - [NodeJS](https://nodejs.org/) (for asset compilation)
 - [Typesafe Activator](https://www.typesafe.com/community/core-tools/activator-and-sbt)

### Install and set up the backend DB and search engine

Fetch and run the EHRI backend service:

    sudo docker run -p 0.0.0.0:7474:7474 -it ehri/ehri-rest

And the Solr-based search engine:

    sudo docker run -p 127.0.0.1:8983:8983 -it ehri/ehri-search-tools
    
## Setting up the development code:

Download the source from Github:

    cd ~/dev
    git clone https://github.com/EHRI/ehri-frontend.git
    cd ehri-frontent

Start the compilation process, which, on the first run, will download tons of dependencies:

    activator compile

### MySQL Setup

Install MySQL if you haven't already.

Now, **at the MySQL shell**, type the following commands (replacing the password with your password):

    CREATE USER 'docview'@'localhost' IDENTIFIED BY '<PASSWORD>';
    CREATE DATABASE docview;
    GRANT ALL PRIVILEGES ON docview.* TO 'docview'@'localhost';

**Note: the database settings you should here should be configured in the `conf/application.conf` file.**


We can now see if the app actually works:

    activator run

Now, visit `http://localhost:9000` in your browser. The app should show a screen saying it needs to apply a migration to the database. **Click the "Apply This Script Now" button.** Click the button, which will prompt the framework to create the database schema.

## Create an account

Go to `http://localhost:9000/login` and create a regular account, either with a password or OAuth2. Since this is a 
regular user account, it won't have admin privileges. The simplest way to gain those privileges is to add the user
to the `admin` group. To do that we need the user ID, which can be found one of two ways. Either:

 - log in to MySQL and run `SELECT id FROM users` (there should only be one row, corresponding to the newly created 
 account)
 - run `curl http://localhost:7474/ehri/userProfile/list`, which should show a JSON list of account profiles, which 
 again should only show one object. The id is the `id` field.
 
Then, using that user id, which we'll **assume** is `user000001`, add the profile to the `admin` group:

    curl -XPOST http://localhost:7474/ehri/group/admin/user000001

You should now have full privileges on the development DB.

