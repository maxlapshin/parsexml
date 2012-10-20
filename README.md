ParseXML
========


It is a very simple and limited XML parser that can work only with valid, well-formed and "good" XML.

But it is fast enough. Really!

Usage:


```
{Tag, Attrs, Content} = parsexml:parse(Bin). 
```

Where Tag is binary name of root tag, Attrs is a {Key,Value} list of attrs and Content is
list of inner tags or Text which is binary.
