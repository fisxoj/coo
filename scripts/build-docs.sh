#!/usr/bin/env sh

LISP=sbcl

# Only run in top-level project dir
if [ ! -f coo.asd ]; then
    echo "This script must be run in the top-level project directory"
    exit 1
else
    echo "Building documentation for coo..."
    $LISP --non-interactive \
	  --eval "(ql:quickload :coo)" \
	  --eval "(coo:document-system :coo)"
    echo "Success!"
fi
