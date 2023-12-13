FROM erlang:26.1.2.0-alpine

RUN apk add git
RUN wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
COPY . .

EXPOSE 8080

ENTRYPOINT ["./rebar3"]
CMD ["shell"]