Hanagram
========

Haskell library for finding anagrams

Install
-------

Installs with a basic make interface you will just need to make sure that
you have ghc to compile it.

```bash
make
sudo make install
```

Usage
-----

To find anagrams you will need to supply the `hanagram` command with a list
of letters and atleast one dictionary file to use:

```bash
hanagram --letters=anagram /usr/share/dict/words
```

For a full list of options you can run `hanagram --help`
