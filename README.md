# swedishverbs-haskell

This is an experimental project to create a REST-API with Haskell.

Stack is needed to compile and run application. See https://docs.haskellstack.org/

Commands to build and run:
* stack build
* stack exec swedishverbs-haskell-exe

Endpoints:
* GET: http://{hostname}:8080/verbs                               - get all verbs
* GET: http://{hostname}:8080/verb?infinitive={infinitive form}   - get verb by modern swedish infinitive form
* GET: http://{hostname}:8080/id/{id}                             - get verb by ID
