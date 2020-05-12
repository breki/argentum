# Argentum to do

- move the common XML parsing code to a production module
- can we simplify the slot parsing code? 
- design the model from the GnuCash XML file
    - implement property testing that (de)serializes data to XML
- implement an XML reader for the model
- import command line parsing code from Demetron
- prepare a command to anonymize a GnuCash XML file
    - create an anonymized sample XML file and add it to the source control
- implement an integration test that reads that sample XML file
- define a report command that generates a balance graph
    - which JS library to use for visualization?
        - D3 perhaps (so we can learn D3)?