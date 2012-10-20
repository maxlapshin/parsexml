ParseXML
========


It is a very simple and limited DOM XML parser that can work only with valid, well-formed and "good" XML.

There are more than hundred ways to crush it down with a proper XML, but it was written for a "good" XML
to parse feeds and machine-generated content.

It is fast enough  and convenient. Really!

Usage:


```
{Tag, Attrs, Content} = parsexml:parse(Bin). 
```

Where Tag is binary name of root tag, Attrs is a {Key,Value} list of attrs and Content is
list of inner tags or Text which is binary.
