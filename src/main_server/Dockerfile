FROM erlang

COPY src src/
COPY sys.config .
COPY rebar.config .

RUN mkdir /data

EXPOSE 8080

CMD ["/bin/bash", "-c", "rebar3 shell --sname main_server --setcookie test"]
# docker run --rm -v ${PWD}/data:/data -p 8080:8080 -i -h main_server --net mynetwork --name main_server main_server
# docker run -it --rm -h node2 --net mynetwork erlang erl -sname node2 -setcookie test