FROM erlang

COPY src src/
COPY sys.config .
COPY rebar.config .

RUN mkdir /data

EXPOSE 8080

CMD ["/bin/bash", "-c", "rebar3 shell --sname main_server --setcookie test"]
# docker run --rm -v ${PWD}/data:/data -p 8080:8080 -it -h main_server --net mynetwork --name main_server main_server
# docker run --rm -p 8080:8080 -it -h main_server --net mynetwork --name main_server main_server