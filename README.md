# pred-serv

[![Build Status](https://travis-ci.org/ocramz/pred-serv.png)](https://travis-ci.org/ocramz/pred-serv)

# Dependencies

* `stack` (https://docs.haskellstack.org/en/stable/README/)
* `docker`


# Environment

* OSX : A `docker-machine` environment (i.e. OS and networking virtualization) should be running.

Write down the IP address produced by `docker-machine env`; this will be necessary for testing the project on localhost. This IP (let's call it `DOCKER_IP`) is also visible in the DOCKER_HOST env var:


    $ echo ${DOCKER_HOST}


# Building


* Build the development image, the project and the deployment image:


    $ make all


At this point `docker images` should list three new images:

* `alpine` containing an installation of Alpine Linux (if this was not already present) (5.5 MB)

* `ocramz/cmbnt-test-dev` : the development image with build tools etc. (1.1 GB)

* `pred-serv:1.0` : the image ready to be deployed (around 22 MB)


# Running


    $ `make docker-deploy-run`

will start the web server on port 3000

Now you can direct your web browser to ${DOCKER_IP}:3000 to verify the server is online and you can make queries on the endpoints:


    /model/v1/one-shot

    /model/v1/batch


* TODO
