FROM haskell:latest


RUN mkdir -p /opt/server
WORKDIR /opt/server

RUN export PATH=~/.cabal/bin:$PATH
RUN cabal update
RUN cabal install happstack-server
RUN cabal install cmdargs

COPY . /opt/server
RUN ghc --make -threaded app/Server.hs -o app/Server -iapp

EXPOSE 80:8000

CMD ["app/Server"]
