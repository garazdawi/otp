I did a prototype in Erlang of some changes to the current term format. The changes are:

1. All sizes and identifiers (id, creation, serial) are changed to be [varint] encoded.
2. unsigned byte (SMALL_INT) we leave as is, but signed small (signed 60-bit) is encoded as [zigzag] [varint], and bignums are encoded as 64-bit [varint] chunks.
3. We move all "text" data (called blocks) to be after the term structure. That means that binary, string and atoms are appended (and de-duplicated) at the end. The blocks appear in the order in which they are present in the term structure. The "Size" field of the term is a [zigzag] [varint], where if it is positive, it is the size. If it is negative it is the index into the block structure.
4. We then optionally compress the text block using some compression algorithm (zstd, zlib, ...).

Using this format we get roughly a size decrease of:
* 20% without compressing the text block
* 40% if compressing text block with zstd/zlib (I also tested compressing text and control block, that added a few %)
* 60% if compressing text block with zstd and a dictionary

When using `term_to_binary(..., [compressed])` the compressed size is roughly 40%,
so it would seem that zip does a rather good job at compressing the current term format.

So I guess a conclusion here is that the current compressed terms does quite a good job. If we were to add zstd together with a dictionary it would probably be able to compress the data down to the 60% size of this prototype. Most likely the prototype will use less CPU to create the final data than compressing the zlib, and also when compressing it will use less CPU as it only looks at the text block which is smaller.

Something to keep in mind here as well is that building the zstd dict is really an off-line task as there is no way to do it by streaming data into the dictionary builder. That is, if we are to "automagically" build dictionaries for distribution entries we would need to keep references to quite a lot of data in memory until we have enough to create the dictionary. If that is acceptable is hard to know as it would depend on the system.

> The data used to compress is data taken from ~5 seconds of distribution traffic at an SGSN node

An example of how encoding works:

```erlang
{a, "bb", a, "bb"}
```

is encoded to

```
Hex: 84 0a 68 04 77 02 6b 04 77 03 6b 05 61 62 62
Dec: 132 10 104 4 119 2 107 4 119 3 107 5 97 98 98
```

where

- 132 - New ext format tag
- 10 - Size of control payload as [varint]
- 104 - A tuple!
- 4 - Size of tuple as [varint]
- 119 - An atom!
- 2 - Size of atom in [zigzag] [varint], i.e. 1
- 107 - A string!
- 4 - Size of string in [zigzag] [varint], i.e. 2
- 119 - An atom!
- 3 - Index of atom 'a' in [zigzag] [varint], i.e. -1
- 107 - A string!
- 5 - Index of string "bb" in [zigzag] [varint], i.e. -2
- 97 98 98 - The text payload "abb"

[varint]: https://en.wikipedia.org/wiki/Variable-length_quantity
[zigzag]: https://gist.github.com/mfuerstenau/ba870a29e16536fdbaba