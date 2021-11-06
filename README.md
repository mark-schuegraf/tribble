# tribble with Grammar Transformation Capability

The [tribble](https://github.com/havrikov/tribble) grammar-based fuzzer generates unexpected inputs whilst pursuing a grammar coverage goal.

This derivative project enriches the tribble fuzzer with a transformation mode that allows for language-preserving transformations of the input grammar. The transformed output grammars may then be used as the input grammar for tribble in generation mode.

See [havrikov/tribble](https://github.com/havrikov/tribble) for information on building tribble and running tribble in generation mode.

Also see [mark-schuegraf/fuzzing-with-grammar-variants-results](https://github.com/mark-schuegraf/fuzzing-with-grammar-variants-results) for experimental results demonstrating the effect of using transformed grammars in fuzzing.

## Supported Transformations

Primarily, the tool supports transformation into the following normal forms:

- Backus-Naur Form
- Chomsky Normal Form
- Extended Chomsky Normal Form
- Greibach Normal Form
- Extended Greibach Normal Form

Not listed above are the individual substeps needed to reach these forms. However, they can also be run individually.

In addition to these normal form transformations, several transformations based on Ralf Lämmel's framework for grammar adaptation also exist. These mostly target syntactic shorthands, like quantifiers, or perform crude operations, like inlining rules into their reference sites.

## Running tribble in Transformation Mode

As in standard tribble, executing `java -jar tribble.jar --help` will print out all available flags and options, including those pertaining to the transformation mode. 

As an example, let us transform a JSON grammar into Chomsky Normal Form :

```bash
java -jar tribble.jar transform-grammar --mode=chomsky-normal-formalizer --grammar-file=tribble-core/src/test/resources/json.tribble --output-grammar-file=./json-chomsky-normal-form.tribble
```

Note that transformations may impose restrictions on their input grammar. In this case, Chomsky Normal Form requires an input grammar stripped of syntactic sugar, i.e., that is in Backus-Naur Form. To this end, tribble may need to be run in the prerequisite transformation mode beforehand.

## Grammar Specification

Input grammars must be in [tribble format](https://github.com/havrikov/tribble/blob/3b9ae3e95375fd23c47cb3c018312bfaff57f9d7/README.md#grammars).

Output grammars are in tribble format by default, but may optionally be in binary format for reuse by tribble's generation component. The main advantage to using the binary format is that it saves time parsing the grammar and that any modifications to the automata of regexes are also persisted. To this end, the options `loading-strategy` and `storing-strategy` exist. 
