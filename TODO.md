# Argentum to do


- function for converting amount given the time
- `transactionNetBalance` must convert to base currency 

- handle this:
```
      <split:id type="guid">de5d8ddf5e9a489281976ea3fd3a61d4</split:id>
      <split:reconciled-state>n</split:reconciled-state>
      <split:value>5000/100</split:value>
      <split:quantity>3279/100</split:quantity>
      <split:account type="guid">a5f2092903ca4368be1b112b5b92329e</split:account> - PayPal EUR
```

- implement a balance-on-date function
    - implement a function that converts split value to EUR

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