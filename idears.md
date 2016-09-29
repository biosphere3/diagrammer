
## Next steps

- [ ] render jacks
  - [ ] shape (chevron)
  - [ ] content (name, rate (amount, units))
  - [X] position
- [X] associate jacks with processes
- [X] draw jacks near processes

- [X] drag and drop from jack to jack
- [ ] render links
- [ ] render containers
  - [ ] shape (box?)
  - [ ] content (name, capacity)

- [ ] drag and drop from jack to container



## Specs

### Connecting jacks and containers

Connecting two Jacks:
- the two Jacks must not be part of any other Flow
- create a new Container
- create two new Flows between the Container and the two Jacks

Connecting a Jack to a Container
- the Jack must not be part of another flow
- create a new Flow between the Container and the Jack