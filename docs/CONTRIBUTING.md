# Contributing to Lambda Soup

Lambda Soup is developed on [GitHub][repo]. All feedback is welcome – open a
GitHub issue, or send me an email at [antonbachin@yahoo.com][email].

## Some ideas

The library is at a very early stage in development, and several things need
improvement. Here are a few examples:

- It is likely that a lot of commonly-needed helpers are missing (see the
  [Convenience][convenience] section and others in the documentation). I would
  like to add whatever people need often. Some existing functions might benefit
  from having optional parameters.
- The `'a nodes` type may need to be generalized to a general lazy sequence
  `'a sequence`, and then replaced with `'a node sequence`. This may make the
  interface more flexible – but I wish OCaml had a standard lazy sequence type.
- The CSS parser should probably be rewritten. It is currently a hand-coded
  recursive descent parser. For the most part, it supports a superset of the CSS
  grammar. However, it does not support strange numbers such as in
  `:nth-child(-5n-12)`. Its behavior can be made more conformant and less
  surprising in a few corner cases. Perhaps it can also be made faster.
- The selector matcher is a naive first implementation. There may well be a lot
  of performance improvements to be made.
- Lambda Soup could extend CSS with parent, ancestor, previous sibling, and
  adjacent previous sibling combinators. These are the natural inverses of the
  standard combinators. There are also some potentially useful extensions in
  jQuery, such as `:has`, that Lambda Soup could borrow.
- Lambda Soup could also support XPath for selection. XPath does seem to be much
  less familiar to people, on average, however.
- Lambda Soup relies on [Ocamlnet's parser][nethtml] for HTML. This parser, like
  every other, has quirks, such as the way it handles attributes without a
  value and the fact that it does not resolve entities. Lambda Soup is also
  completely reliant on it for dealing properly with character encodings.

Of course, this is aside from any bugs that may be found :)

## Developing

Clone the repository locally. The library source is organized as follows:

- `src/`: main source, i.e. `src/soup.mli` and `src/soup.ml`.
- `test/`: unit tests in `test.ml`, a performance test, and a reverse dependency
  ("integration") test.
- `docs/`: files related to the `ocamldoc` documentation, such as extra HTML and
  the postprocessing script.

Tu run tests, run `make all-tests`. This requires the `ounit` package. This
takes some time, because it reinstalls Lambda Soup. If you want to run only the
unit and performance tests, run `make test`.

To generate docs, run `make docs`.

To install the library from your repo clone, run `make install`. This uses OPAM
to pin the repository and install a `lambdasoup` package. To undo this, run
`make uninstall`.

Make a branch off master, make your changes, rebase over master (if you have to)
when done, and submit a pull request :)

[repo]:        https://github.com/aantron/lambda-soup
[email]:       mailto:antonbachin@yahoo.com
[convenience]: http://aantron.github.io/lambda-soup#2_Convenience
[nethtml]:     http://ocamlnet.sourceforge.net/refman/Nethtml.html
