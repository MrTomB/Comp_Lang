#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"----------------------
 Thomas Burch (1314305)
 tburch1@ucsc.edu
 Bryce Paine (1232591)
 bpaine@ucsc.edu
 11/15/205
 CMPS112-ASG3
 ---------------------"

Object subclass: BitStream [
   |writeInd writeByte bitInd byte theStream |
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      theStream := fileStream.
      bitInd := 1.
      writeInd := 9.
      writeByte := 0.
   ]
   placeBit: theBit [
      writeInd := writeInd - 1.
      writeByte := writeByte bitAt: writeInd put: theBit.
      writeInd = 1 ifTrue: [
         theStream nextPutByte: writeByte.
         writeByte := 0.
         writeInd := 9.
      ].
   ]
   placeString: theString [
      theString do: [:index|
         (index asString = '0')
            ifTrue: [self placeBit: 0]
            ifFalse: [self placeBit: 1].
      ].
   ]
   atEnd [
      ^bitInd = 1 and: [theStream atEnd]
   ]
]

"----------------------------------
Mackey's catfile.st for FileStream
-----------------------------------"
FileStream extend [
    nl := Character nl.
    catFile [
        |count|
        count := 0.
        [self atEnd not] whileTrue: [
            count := count +1.
            count asString size to: 5 do: [:skip| stdout << ' '].
            stdout << count << ': ' << self nextLine << nl.
        ]
    ]
    parseFile [
      |freqtable|
      freqtable := ZeroArray new: 257.
      [self atEnd not] whileTrue: [
         |ordChar|
         ordChar := self next asInteger.
         freqtable incr: ordChar.
      ].
      freqtable incr: 256.
      ^ freqtable
    ]
]

"-------------------
Mackey's charfreq.st
--------------------"
Object subclass: ZeroArray [
    |theArray|

    ZeroArray class >> new [
        self shouldNotImplement.
    ]

    ZeroArray class >> new: size [
        |result|
        result := super new.
        result init: size.
        ^ result
    ]

    init: size [
        theArray := Array new: size.
    ]

    size [
        ^ theArray size.
    ]

    at: index [
        ^ theArray at: index + 1.
    ]

    at: index put: value [
        ^ theArray at: index + 1 put: value.
    ]

    incr: index [
        (self at: index) isNil
            ifTrue: [ self at: index put: 0].
        self at: index put: (self at: index) + 1.
    ]

    keysAndValuesDo: aBlock [
        (0 to: (self size) - 1) do: [:index |
            aBlock value: index value: (self at: index).
        ]
    ]
]

"----------------
Mackey's Tree.st
-----------------"
Object subclass: Leaf [
    |char count|
    char [ ^ char ]
    count [ ^ count ]
    isLeaf [ ^ true ]
    Leaf class >> new [
        self shouldNotImplement
    ]

    Leaf class >> new: aChar count: aCount [
        |result|
        result := super new.
        result setChar: aChar andCount: aCount.
        ^result
    ]

    setChar: aChar andCount: aCount [
        char := aChar.
        count := aCount.
    ]

    <= other [
        ^ (count < other count)
        | ((count = other count) & (char <= other char)) 
    ]

    printBase: aStream [
    |fixhl| "Fix highlighting in vim - variable never used."
        ^ aStream << self class << '(' << char << ',' << count
    ]
    printOn: aStream [
        (self printBase: aStream) << ')'.
    ]

    inorder: visitor prefix: string [
        visitor value: char value: string.
    ]
]

"----------------
Mackey's tree.st
-----------------"
Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
       |result|
       result := super new: aChar count: aCount.
       result setLeft: aLeft andRight: aRight.
       ^ result
   ]

   isLeaf [ ^ false ]

   setLeft: aLeft andRight: aRight [
       left := aLeft.
       right := aRight.
   ]
   right [ ^ right ]
   left [ ^ left ]

   printOn: aStream [
       (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
       left inorder: visitor prefix: string, '0'.
       right inorder: visitor prefix: string, '1'.
   ]
]

"declaring option variables = false"
nl := Character nl.
dOpt := false.
tOpt := false.
cOpt := false.
uOpt := false.
infile := ''.
outfile := stdout.

"requires more than 2 args"
(Smalltalk arguments size < 2) ifTrue:[
  stdout << 'Error: not enough arguments.' << nl.
  ObjectMemory quit: 1.
]

(1 to: Smalltalk arguments size)
do: [:index |
    args := (Smalltalk arguments at: index).
    (args includes: $-) ifTrue: [
        (args includes: $d) ifTrue: [dOpt := true.].
        (args includes: $t) ifTrue: [tOpt := true.].
        (args includes: $c) ifTrue: [cOpt := true.].
        (args includes: $u) ifTrue: [uOpt := true.].

"Can only compress or uncompress at a time"
        cOpt ifTrue: [
          uOpt ifTrue: [
            stdout << 'Error: Cannot have -c and -u opts both!' << nl.
            cOpt := false.
            uOpt := false.
          ].
        ].

"executed if args are files"
    ] ifFalse: [
        (infile isEmpty) ifTrue: [
            infile := args.
        ] ifFalse: [
            outfile :=  args.
        ]
    ]
].

"Debug plus, -t, -c, -u"
dOpt ifTrue: [
    'Debug mode is on' display.
    nl display.

    tOpt ifTrue: [
        'Prints The Tree' display.
        nl display.
    ]. 

    cOpt ifTrue: [
        'Compressing: ' display.
        infile display.
        (outfile isEmpty) ifFalse: [
            ' to file ' display.
            outfile display.
            nl display.
        ] ifTrue: [
            ' to stdout' display.
            nl display.
        ]
    ].

    uOpt ifTrue: [
        'Uncompressing: ' display.
        infile display.
        (outfile isEmpty) ifTrue: [
            ' to stdout' display.
            nl display.
        ] ifFalse: [
            ' to file ' display.
            outfile display.
            nl display.
        ]
    ].    
].

|readfile writefile|
readfile := FileStream open: infile 
                       mode: FileStream read.

(outfile isEmpty) ifTrue: [
  writefile := stdout.
  ]
  ifFalse: [                       
  writefile := FileStream open: outfile
                        mode: FileStream write.
  ]. 

"This is where the frequency table is created"
|frequencyTable|
uOpt ifFalse: [frequencyTable := readfile parseFile.].
dOpt ifTrue: [
    (0 to: frequencyTable size - 2)
    do: [:index |
        index display.
        ' ' display.
        item := frequencyTable at: index.
        item display.
        nl display.
    ].
].

"This is where the pq is created"
|pq|
pq := SortedCollection new.
uOpt ifFalse:[
   (0 to: frequencyTable size - 2)
   do: [:index |
      |sLeaf aChar aCount|
      aCount := frequencyTable at: index.
      aCount isInteger ifTrue: [
          aChar := index asCharacter.
          sLeaf := Leaf new: aChar  count: aCount.
          pq add: sLeaf.
      ].
   ].
].

dOpt ifTrue: [pq inspect.].

"makes tree from the priority queue"
uOpt ifFalse: [
   [pq size > 1] whileTrue: [
       |aTree tChar tCount last slast|
       last := pq removeFirst.
       slast := pq removeFirst.
       tChar := last char.
       tCount := last count + slast count.
       aTree := Tree new: tChar count: tCount left: last 
                                            right: slast.
       pq add: aTree.
   ].
].

dOpt ifTrue: [pq inspect.].

|finalTree encodeTable|
encodeTable := ZeroArray new: 257.
uOpt ifFalse: [
   finalTree := pq removeLast.
   finalTree inorder: [:char :string |
       |index|
       index := char asInteger.
       encodeTable at: index put: string.
   ] prefix: ''.
].

tOpt ifTrue: [
   (0 to: encodeTable size - 2)
   do: [:index |
       |char great32 less127 count string|
       great32 := index > 32.
       less127 := index < 127.
       count := frequencyTable at: index.
       string := encodeTable at: index.
       count isInteger ifTrue: [
          great32 & less127 ifTrue: [
             char := index asCharacter.
             stdout << char << ' ' << count << ' ' << string << nl.
          ] ifFalse: [
             char := index.
             stdout << index << ' ' << count << ' ' << string << nl.
          ].
       ].
   ]. 
   readfile close.
].

cOpt ifTrue: [
   |encodeString|
   aBitstream := BitStream new: writefile.
   readfile reset.
   aBitstream placeBit: 1.
   [readfile atEnd not] whileTrue: [
      |index|
      index := readfile next asInteger.
      encodeString := (encodeTable at: index).
      aBitstream placeString: encodeString.
   ].
   readfile close.
   writefile close.
].

uOpt ifTrue: [
   |encodeString|
   aBitstream := BitStream new: writefile.
   readfile reset.
   aBitstream placeBit: 1.
   [readfile atEnd not] whileTrue: [
      |index|
      index := readfile next asInteger.
      encodeString := (encodeTable at: index).
      aBitstream placeString: encodeString.
   ].
   readfile close.
   writefile close.
].
