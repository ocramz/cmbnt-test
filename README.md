# pred-serv

A little prediction server

# Dependencies

* `stack` (https://docs.haskellstack.org/en/stable/README/)
* `docker`


# Environment

* OSX : A `docker-machine` environment (i.e. OS and networking virtualization) should be running.

Write down the IP address produced by `docker-machine env`; this will be necessary for testing the project on localhost. This IP (let's call it `DOCKER_IP`) is also visible in the DOCKER_HOST env var:


    $ echo ${DOCKER_HOST}


# Building

First thing, make sure that `docker.enable` is set to `true` in the `stack.yaml` configuration file. This ensures that the project is built in an Alpine Linux container, since the deployment image will be based on this Linux distribution as well.

Build the development image and the project, test the project and build the deployment image:


    $ make all


At this point `docker images` will list three new images:

* `alpine` containing an installation of Alpine Linux (if this was not already present) (5.5 MB)

* `ocramz/cmbnt-test-dev` : the development image with build tools etc. (1.1 GB)

* `pred-serv:1.0` : the prediction server image ready to be deployed (around 22 MB)


# Running

    $ docker run -p 3000:3000 -it pred-serv:1.0

will start a HTTP webserver at `${DOCKER_IP}:3000`. The server will not log anything to console, and can be stopped with Control-C (i.e. SIGINT).

## REST endpoints

* Liveness : the `/liveness` endpoint replies with 200 OK if the prediction server is online.

### v1

* The one-shot prediction endpoint is queried via GET query parameters; the `x` and `y` parameters are the query coordinates, e.g. : 

    `/model/v1/one-shot/?x=<point_x>&y=<point_y>`

* The batch endpoint is queried by passing the query points as a JSON object in the body of a POST request:

    `/model/v1/batch/`

Each query point in the batch is represented as a list of floating point numbers, for example:

    POST http://${DOCKER_IP}:3000/model/v1/batch/
    {
      "batch": [[1.9747777403969031,0.1703482031671503],
            [0.2268872897216034,0.9602596319569988],
            [0.577768094821916,0.8049502627101064]]
    }

which will return

    {"prediction":[true,false,false]}

NB: since the internal model is restricted to classifying points in 2D, lists that have more or less than 2 elements will cause a parse error.

### v2 API

The `v2` API lets the user reconfigure the classifier at runtime, i.e. by providing different training data and a classification method.

For now, the only two classification methods supported are FDA (Fisher linear discriminant analysis) and QDA (quadratic discriminant analysis).

Example usage of the training endpoint :

    POST http://${DOCKER_IP}:3000/model/v2/train/
    {
    "clcTrainingSet":[
        {"slabel":false,"sy":0.7145113953210204,"sx":-0.6214134912880266},
	{"slabel":true,"sy":-0.5067041565111601,"sx":1.0299942802199769},
	{"slabel":false,"sy":0.18514230070868073,"sx":0.12440603711836776},
	{"slabel":false,"sy":0.9504106504744793,"sx":0.43845269753671307},
	{"slabel":false,"sy":0.5200132456123451,"sx":0.806956117527472},
	{"slabel":false,"sy":0.24590087515509454,"sx":-1.593655827995092e-2},
	{"slabel":true,"sy":-0.4134296465409296,"sx":1.454319160697646},
	{"slabel":true,"sy":-0.39239141788819104,"sx":1.0157021298747575},
	{"slabel":true,"sy":0.3120120296852688,"sx":2.040571734630943},
	{"slabel":false,"sy":0.3247240243855006,"sx":3.696035731787589e-2}
	],
    "clcClassifier":"QDA"
    }

Afterwards, the server can be queried in batch or one-shot mode just like with the v1 API on the corresponding endpoints :

    model/v2/batch

    model/v2/one-shot


## Local testing

The server can also be built and tested as a regular Haskell application (i.e. without Docker):

    $ stack build

    $ stack exec -- pred-serv

This will spin up a webserver at `<localhost>:3000`.

The file path of the default training dataset can also be changed with a command line option, see the help screen :

    $ stack exec -- pred-serv -h

    Usage: pred-serv [-d|--dataset-path PATH]
    pred-serv - a little prediction server

    Available options:
      -d,--dataset-path PATH   Path of the default training
                               dataset (default: "data/samples.csv")
      -h,--help                Show this help text


# Building the HTML documentation


    $ stack haddock

the path to the documentation index page can be found after the line `Updating Haddock index for local packages in` in the stack haddock log.


# Project structure


    |-- LICENSE
    |-- Makefile
    |-- README.md
    |-- Setup.hs
    |-- analysis
    |   `-- plot_samples.R
    |-- app
    |   `-- Main.hs
    |-- data
    |   |-- model.csv
    |   `-- samples.csv
    |-- docker
    |   |-- deploy
    |   |   `-- Dockerfile
    |   `-- dev
    |       `-- Dockerfile
    |-- pred-serv.cabal
    |-- src
    |   |-- Lib
    |   |   |-- Math.hs
    |   |   `-- Types.hs
    |   `-- Lib.hs
    |-- stack.yaml
    `-- test
        |-- LibSpec.hs
        `-- Spec.hs

`src/` is the project source library. The server implementation is in `app/Main.hs`.

`test/` contains only unit tests for now (in `LibSpec.hs`).

`data/` contains the default model parameters and a small labeled dataset.

`docker/` contains the Dockerfiles for the development and deployment images. The statically-linked server binary meant to be deployed is copied in `docker/deploy`.

`stack.yaml` and `pred-serv.cabal` are project files, containing dependency and configuration information.