# Cleanup Algorithm

Inspired by reading [Cleanup Algorithm](http://joneshf.github.io/programming/2015/10/04/Cleanup-Algorithm.html) I decided to have a little go at implementing it in Haskell.

## What's it do, eh?

Given this input:

```
[ { "foo": "2", "bar": "12", "baz": "6", "quux": true },
  { "foo": "10", "bar": "4", "baz": "17", "quux": false },
  { "foo": "5", "bar": null, "baz": null, "quux": false },
  { "foo": "8", "bar": "2", "baz": "12", "quux": false },
  { "foo": "-2", "bar": "2", "baz": "9", "quux": false },
  { "foo": "-8", "bar": "17", "baz": "8", "quux": false },
  { "foo": "9", "bar": "18", "baz": null, "quux": true },
  { "foo": "6", "bar": "14", "baz": null, "quux": false },
  { "foo": "6", "bar": null, "baz": null, "quux": true },
  { "foo": "-999", "bar": "18", "baz": "14", "quux": true },
  { "foo": "-999", "bar": "18", "baz": "9", "quux": true },
  { "foo": "-6", "bar": "12", "baz": "17", "quux": true },
  { "foo": "-9", "bar": "12", "baz": "12", "quux": true },
  { "foo": "-999", "bar": "20", "baz": "3", "quux": true },
  { "foo": "-6", "bar": "5", "baz": "6", "quux": true },
  { "foo": "5", "bar": "15", "baz": "1", "quux": true },
  { "foo": "-8", "bar": "0", "baz": "10", "quux": false },
  { "foo": "0", "bar": "9", "baz": "17", "quux": false }
]

```

It produces this output:

```
Scanned:
[(2,12,6),(10,4,17),(5,4,17),(8,2,12),(2,2,9),(8,17,8),(9,18,8),(6,14,8),(6,14,8),(0,18,14),(0,18,9),(6,12,17),(9,12,12),(0,20,3),(6,5,6),(5,15,1),(8,0,10),(0,9,17)]

Final output:
Counter {
    _foos = [2,10,5,8,2,8,9,6,6,0,0,6,9,0,6,5,8,0],
    _bars = [12,4,4,2,2,17,18,14,14,18,18,12,12,20,5,15,0,9],
    _bazs = [6,17,17,12,9,8,8,8,8,14,9,17,12,3,6,1,10,17]
  }
```
