FROM alpine

EXPOSE 3000

COPY pred-serv pred-serv

COPY samples.csv data/samples.csv

ENTRYPOINT ["/pred-serv"]