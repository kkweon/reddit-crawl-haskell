FROM samdoshi/haskell-stack

ADD config /code/config
ADD src /code/src
ADD . /code/

WORKDIR /code
RUN stack setup && stack build
CMD stack exec crawl
