+++
title = "What is Kolmogorov Complexity?"
description = "A brief note on Kolmogorov complexity."
date = "2014-09-29"
categories = [ "example", "configuration" ]
tags = [
    "example",
    "hugo",
    "toml"
]
markup: mmark
+++

The Basic Idea
-------------------------

I've been reading about Kolmogorov complexity, with the aim of understanding certain metamathematical results (including general incompleteness). I seem to be on the verge of understanding specifically Chaitin's reformulation of Godel's results using the Berry paradox (which, interestingly, was first published by Bertrand Russell but named after a librarian in Oxford named Berry who first articulated the problem to Russell). First, what is Kolmogorov (or Kolmogorov-Chaitin) complexity? Assuming a given string (or string rendering of a particular problem), the Kolmogorov complexity of that string is the size of the smallest program that is needed for generate that string. For example, let's assume that we have the following two strings of size

<!--more-->

The `toml` front matter used on this entry:

```
+++
title = "Another Hugo Post"
description = "Nothing special, but one post is boring."
date = "2014-09-02"
categories = [ "example", "configuration" ]
tags = [
    "example",
    "hugo",
    "toml"
]
+++
```

This flexibility also extends to your site's global configuration file. You're free to use any format you prefer::simply
name the file `config.yaml`, `config.toml` or `config.json`, and go on your merry way.

JSON Example
------------

How would this entry's front matter look in `json`? That's easy enough to demonstrate:

```
{
    "title": "Another Hugo Post",
    "description": "Nothing special, but one post is boring.",
    "date": "2014-09-02",
    "categories": [ "example", "configuration" ],
    "tags": [
        "example",
        "hugo",
        "toml"
    ],
}
```
