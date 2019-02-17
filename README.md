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

First thing, make sure that `docker.enable` is set to `true` in the `stack.yaml` configuration file. This ensures that the project is built against in an Alpine Linux container, since the deployment image will be based on this Linux distribution as well.

Build the development image, the project and the deployment image:


    $ make all


At this point `docker images` should list three new images:

* `alpine` containing an installation of Alpine Linux (if this was not already present) (5.5 MB)

* `ocramz/cmbnt-test-dev` : the development image with build tools etc. (1.1 GB)

* `pred-serv:1.0` : the image ready to be deployed (around 22 MB)


# Running


    $ `make docker-deploy-run`

will start a HTTP webserver at `${DOCKER_IP}:3000`.

## REST endpoints

* Liveness : the `/liveness` endpoint replies with 200 OK if prediction server is online.

* The one-shot prediction endpoint is queried via GET query parameters; the `x` and `y` parameters are the query coordinates, e.g. : 

    /model/v1/one-shot/?x=<point_x>&y=<point_y>

* The batch endpoint is queried by passing the query points as a JSON object in the body of a POST request:

    /model/v1/batch/

Each query point in the batch is represented as a list of floating point numbers, for example:

    POST http://${DOCKER_IP}:3000/model/v1/batch
    {
      "batch": [[1.9747777403969031,0.1703482031671503],
            [0.2268872897216034,0.9602596319569988],
            [0.577768094821916,0.8049502627101064]]
    }

which will return

    {"prediction":[true,false,false]}


# Building the HTML documentation


    $ stack haddock

the path to the documentation index page can be found after the line `Updating Haddock index for local packages in` in the stack haddock log.