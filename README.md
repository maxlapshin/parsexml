ParseXML
========


It is a very simple and limited DOM XML parser that can work only with valid, well-formed and "good" XML.

There are more than hundred ways to crush it down with a proper XML, but it was written for a "good" XML
to parse feeds and machine-generated content.

It is fast enough, convenient and has very low memory footprint due to binary usage. Really!

Usage:


```
{Tag, Attrs, Content} = parsexml:parse(Bin). 
```

Where Tag is binary name of root tag, Attrs is a {Key,Value} list of attrs and Content is
list of inner tags or Text which is binary.



Benchmarking
------------

Download some XML and run bench:

```
$ ./bench.erl m.xml 500
   xmerl:     8511ms     2845KB 1MB/s
parsexml:     1047ms       86KB 14MB/s
  erlsom:     3428ms     1759KB 4MB/s
$ wc -l m.xml 
      82 m.xml
$ du -hs m.xml 
 32K  m.xml
```

Here we can see that small 32K file is parsed 500 times on a high speed with low memory usage.
Memory usage is collected via process_info(Pid,memory)

Let's check on something bigger:

```
$ du -hs FIX50SP2.xml
512K  FIX50SP2.xml
$ wc -l FIX50SP2.xml
10540 FIX50SP2.xml
$ ./bench.erl FIX50SP2.xml 5
   xmerl:     2179ms    46622KB 1MB/s
parsexml:      701ms     7449KB 3MB/s
  erlsom:      854ms    18917KB 3MB/s
```

Here we can see, that erlsom runs on the same speed but with higher memory usage.

Lets now parse this file 100 times:

```
$ ./bench.erl FIX50SP2.xml 100
   xmerl:    46240ms    56653KB 1MB/s
parsexml:    15607ms     6501KB 3MB/s
  erlsom:    17838ms    15630KB 2MB/s
```

parsexml and erlsom take similar time, but erlsom is using more memory.

Now lets start parsing with spawn_opt([{fullsweep_after,5}]):

```
$ ./bench.erl m.xml 500
   xmerl:    13022ms     1535KB 1MB/s
parsexml:     1081ms      171KB 14MB/s
  erlsom:     5045ms     1087KB 3MB/s
$ ./bench.erl ../trader/apps/fix/spec/FIX50SP2.xml 100
   xmerl:    76785ms    29696KB 0MB/s
parsexml:    19656ms     7449KB 2MB/s
  erlsom:    23631ms    17165KB 2MB/s
```

Time is lowered to to frequent garbage collection, but memory footprint is again better for parsexml

