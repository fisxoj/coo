FROM parentheticalenterprises/sbcl-quicklisp-base:2.0.0-2019-12-27

COPY . /quicklisp/local-projects/coo

RUN sbcl --non-interactive --eval "(ql:quickload :coo)"

WORKDIR /build

COPY ./ci/build-docs.lisp .

RUN chmod +x build-docs.lisp

RUN mkdir /work

WORKDIR /work

CMD /build/build-docs.lisp
