
## Next steps

- [ ] render jacks
  - [ ] shape (chevron)
  - [ ] content (name, rate (amount, units))
  - [X] position
- [X] associate jacks with processes
- [X] draw jacks near processes

- [X] drag and drop from jack to jack
- [ ] render flows
  - [X] simple lines
  - [ ] complex orthogonal path
- [X] render containers
  - [X] shape (box?)
  - [ ] content (name, capacity)

- [X] drag and drop from jack to container


- [ ] reposition ports dynamically
- [ ] animation

- [ ] clean up
  - [X] split into files
  - [ ] rethink the Shape impl (e.g. should be able to grab width/height from a Process)
    - maybe all the visible elements and their records can live in a union type, and all the functions would have case statements

- [X] populate initial data from YAML

- [ ] math
  - [ ] calculate new values based on epoch

## Specs

### Connecting jacks and containers

Connecting two Jacks:
- the two Jacks must not be part of any other Flow
- create a new Container
- create two new Flows between the Container and the two Jacks

Connecting a Jack to a Container
- the Jack must not be part of another flow
- create a new Flow between the Container and the Jack