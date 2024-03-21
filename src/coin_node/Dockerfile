FROM erlang

COPY src src/
COPY sys.config .
COPY rebar.config .

RUN mkdir /data

EXPOSE 8080

CMD ["/bin/bash", "-c", "rebar3 shell --sname node --setcookie test"]
# docker run --rm -v ${PWD}/data:/data -p 8081:8080 -i -h node1 --net mynetwork --name coin_node1 coin_node
# docker run -it --rm -h node2 --net mynetwork erlang erl -sname node2 -setcookie test