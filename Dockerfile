FROM scratch

EXPOSE 8080

ADD dist/pred-serv pred-serv

ENTRYPOINT ["/pred-serv"]