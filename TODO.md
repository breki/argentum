# Argentum to do

- implement a balance-on-date function
    - finds all transactions <= date

- implement code to parse the whole XML file
    - implement non-recursive `parseElements`
- introduce `Parser` and `ParserTransformer`

- implement an XML reader for the model
    - template-transactions
    - schedxaction
    - budget
- design the model from the GnuCash XML file
    - implement property testing that (de)serializes data to XML
- import command line parsing code from Demetron
- prepare a command to anonymize a GnuCash XML file
    - create an anonymized sample XML file and add it to the source control
- define a report command that generates a balance graph
    - which JS library to use for visualization?
        - D3 perhaps (so we can learn D3)?