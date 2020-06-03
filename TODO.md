# Argentum to do

- parsing prices
    - move to production module
    - after price parsing is done, clean up code a little
- implement an XML reader for the model
    - account
    - transaction
    - template-transactions
    - schedxaction
    - budget
- design the model from the GnuCash XML file
    - implement property testing that (de)serializes data to XML
- import command line parsing code from Demetron
- prepare a command to anonymize a GnuCash XML file
    - create an anonymized sample XML file and add it to the source control
- implement an integration test that reads that sample XML file
- define a report command that generates a balance graph
    - which JS library to use for visualization?
        - D3 perhaps (so we can learn D3)?