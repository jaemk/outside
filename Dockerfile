FROM clfoundation/sbcl:2.1.5-slim-buster

RUN apt update --yes && apt install --yes \
    build-essential \
    curl

WORKDIR /app
COPY ./src ./src
COPY ./deps.lisp ./deps.lisp
COPY ./Makefile ./Makefile
COPY ./outside.asd ./outside.asd

RUN make || cat build/build.log
CMD [ "build/bin/outside" ]

