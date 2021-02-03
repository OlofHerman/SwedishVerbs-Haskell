# swedishverbs-haskell

This is an experimental project to create a REST-API written in Haskell with Servant and Persistent.

Stack is needed to compile and run application. See https://docs.haskellstack.org/

## Commands to build and run:
* `stack build`
* `stack exec swedishverbs-haskell-exe`

It is also possible to run in docker (if Stack isn't available):
* `docker build -t swedishverbs .`
* `docker run -p 8080:8080 swedishverbs`

### Endpoints:
* GET: http://{hostname}:8080/all                                  - get all verbs
* GET: http://{hostname}:8080/verbs?infinitive={infinitive form}   - get verbs by modern swedish infinitive form
* GET: http://{hostname}:8080/verb/{id}                             - get verb by ID
* DELETE: http://{hostname}:8080/verb/{id}                        - delete verb by ID
* POST: http://{hostname}:8080/verb (with JSON body)              - post new verb
* PUT: http://{hostname}:8080/verb/{id} (with JSON body)            - update a verb by ID

Documentation for the REST-API is available through Swagger-UI at:
http://{hostname}:8080/swagger-ui/

*Example JSON input:*
```
{
  "infinitive": "bryta",
  "present": "bryter",
  "past": "bröt",
  "supine": "brutit",
  "oldInfinitive": "brȳta",
  "oldPresent": "brȳter"
  "oldPast": "brø̄t",
  "oldPastPlural": "brutu",
  "oldPastParticiple": "brutin",
  "verbClass": "II"
}
```