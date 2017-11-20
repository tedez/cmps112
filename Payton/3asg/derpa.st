"#!/AFS/CATS.UCSC.EDU/COURSES/CMPS112-WM/USR/SMALLTALK/BIN/GST -F"

nl := Character nl.
FileStream extend [
  nl := Character nl.
   buildTable: table [
      [self atEnd not] whileTrue: [
        table incr: self next asInteger.
      ]
   ]
]

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

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

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]
   isLeaf [ ^true ]
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
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   dumpTree: outbits [
      outbits writeBit: 0.

      8 to: 1 by: -1 do: [:index |
         outbits writeBit: ((self char asInteger) bitAt: index)].

      char asInteger = 0 ifTrue: [
         outbits writeBit: 0.
      ].
      char asInteger = 256 ifTrue: [
         outbits writeBit: 1.
      ].
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

]

Leaf subclass: Tree [
   |left right|
   isLeaf [ ^false ]
   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]
   
   getRight [ ^right ]
   getLeft [ ^left ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   dumpTree: outbits [
      left dumpTree: outbits.
      right dumpTree: outbits.
      outbits writeBit: 1.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

]


|is_debug is_compress is_uncompress is_display 
file_name infile root outfile istream ostream encoding ptr| 
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"
"    FLAG VARIABLES   "
is_debug := 0.
is_compress := 0.
is_uncompress := 0.
is_display := 0.
file_name = ''.

(Smalltalk arguments size) > 3 ifTrue: [
    status := 127.
    stderr << 'Usage: ' << 'Too many arguments ' << status << nl.
    ObjectMemory quit: status.
].


(Smalltalk arguments size) < 2 ifTrue: [
    status := 127.
    stderr << 'Usage: ' << 'Too few arguments ' << status << nl.
    ObjectMemory quit: status.
].

"FREQUENCY TABLE"
freqtable := ZeroArray new: 257.
"ENCODING TABLE, HOLDS HUFFMAN ENCODING FOR EACH CHAR"
encoding := ZeroArray new: 257.
"PRIORITY QUEUE FOR BUILDING TREE"
priority := SortedCollection new.
"MANUALLY INSERT EOF"
freqtable at: 256 put: 1.

"PARSE COMMAND LINE ARGS"
(1 to: Smalltalk arguments size) do: [:index |
    arg := (Smalltalk arguments at: index).
    (arg includes: $-) ifTrue: [
        arg = '-d' ifTrue: [is_debug := 1].
        arg = '-c' ifTrue: [is_compress := 1].
        arg = '-u' ifTrue: [is_uncompress := 1].
        arg = '-t' ifTrue: [is_display := 1].
    ] ifFalse: [
        outfile := ''.
        |ofile|
        (Smalltalk arguments size) = 3 ifTrue: [
            outFile := (Smalltalk arguments at: 3).
            ofile := FileStream openL outFile mode: Filestream write.
        ].

        (Smalltalk arguments size) = 2 ifTrue: [ 
            ofile := stdout. 
        ].
    ]
].
"
Smalltalk arguments: '-d -t -c -u' do: [:opt :arg |
   (opt = $d) ifTrue: [ is_debug := 1 ].
   (opt = $t) ifTrue: [ is_compress := 1 ].
   (opt = $c) ifTrue: [ is_uncompress := 1 ].
   (opt = $u) ifTrue: [ is_display := 1 ].

  (arg ~= nil) ifTrue: [
     (argc = 0) ifTrue:[
          optD ifTrue:[
              stdout <<'input filename: ' << arg <<nl.].

              inputfile := FileStream open: arg
                        mode: FileStream read.
              readfile := FileStream open: arg
                        mode: FileStream read.
         ]

        ifFalse:[
           optD ifTrue:[
           stdout <<'output filename: ' << arg <<nl.].
            outputfile := FileStream open: arg
                       mode: FileStream write.].
  
     argc := argc + 1.
   ].
].

"
"DISPLAY ENCODING TABLE"
(is_display = 1) ifTrue: [
    infile_name := Smalltalk arguments at: Smalltalk arguments size.
    infile := FileStream open: infile_name mode: FileStream read.

    "BUILD FREQUENCY TABLE"
    infile buildTable: freqtable.

    "ADD ALL ELEMENTS FROM FREQUENCY TABLE INTO PQ"
    freqtable keysAndValuesDo: [:key :value |
       (value notNil and: [value > 0]) ifTrue: [
            |node|
            node := Leaf new: key asCharacter count: value.
            priority add: node.
       ]
    ].

    "BUILD HUFFMAN TREE"
    [priority size > 1] whileTrue: [
        |left right newtree|
        left := priority removeFirst.
        (priority size > 0) ifTrue: [
            right := priority removeFirst.
            (left notNil and: [right notNil]) ifTrue: [
              newtree := Tree new: left char 
              count: left count + right count left: left right: right.
              priority add: newtree.
            ]
        ]
    ].

    root := priority removeFirst.

    "INORDER DFS TO BUILD ENCODING TABLE"
    root inorder: [:char :string |
        encoding at: (char asInteger) put: string.
    ] prefix: ''.

    "DO A POSITIVELY STUPID AMOUNT OF CODE JUST TO FORMAT A TABLE"
    encoding keysAndValuesDo: 
    [:key :value |
       (value notNil and: [value > 0]) ifTrue: [
            count := freqtable at: key.
            numdigits := (count floorLog: 10) + 1.
            (key < 33 or: [key > 126]) ifTrue: [
                numdigits := numdigits + 1.
                (key = 256) ifTrue: [
                    stdout << 'EOF'.
                ]
                ifFalse: [
                    stdout << 'x'.
                    (key < 16) ifTrue: [
                        stdout << '0'.
                    ].


                    tmp := key asInteger.
                    tmp printOn: stdout base: 16.
                ].
            ]
            ifFalse: [
                stdout << ' ' << key asCharacter.

            ].

            0 to: (8 - numdigits) do: [:i |
                stdout << ' '.
            ].
            stdout << count << '  ' << value << nl.
        ]
    ].
]

"COMPRESS THE FILE"
(is_compress = 1) ifTrue: [
    outfile_name := Smalltalk arguments at: Smalltalk arguments size.
    infile_name := Smalltalk arguments at: 
                                    ((Smalltalk arguments size) - 1).
    infile := FileStream open: infile_name mode: FileStream read.
    outfile := FileStream open: outfile_name mode: FileStream write.

    "BUILD FREQUENCY TABLE"
    infile buildTable: freqtable.

    "ADD ALL ELEMENTS FROM FREQUENCY TABLE INTO PQ"
    freqtable keysAndValuesDo: [:key :value |
        (value notNil and: [value > 0]) ifTrue: [
            |node|
            node := Leaf new: key asCharacter count: value.
            priority add: node.
        ]
    ].

    "BUILD HUFFMAN TREE"
    [priority size > 1] whileTrue: [
        |left right newtree|
        left := priority removeFirst.
        (priority size > 0) ifTrue: [
            right := priority removeFirst.
            (left notNil and: [right notNil]) ifTrue: [
                newtree := Tree new: left char 
                count: left count + right count left: left right: right.
                priority add: newtree.
            ]
        ]
    ].

    root := priority removeFirst.
    (root isLeaf = false) ifTrue: [
        "INORDER DFS TO BUILD ENCODING TABLE"
        root inorder: [:char :string |
            encoding at: (char asInteger) put: string.
        ] prefix: ''.

        "OPEN A BITSTREAM FOR THE OUTFILE"
        ostream := OutBits new: outfile.

        "WRITE ENCODING TREE OUT TO FILE"
        root dumpTree: ostream.

        "DUMP LAST BIT TO INDICATE RETURN FROM ROOT FOR DECODE"
        ostream writeBit: 1.

        "WRITE OUT ENCODED VERSION OF EACH BYTE"
        infile position: 0.
        [infile atEnd not] whileTrue: [
            temp := encoding at: infile next asInteger.
            (1 to: temp size) do: [:i |
                ((temp at: i) asString = '0') 
                    ifTrue:  [ostream writeBit: 0.]
                    ifFalse: [ostream writeBit: 1.].
            ].
        ].

        "WRITE OUT EOF ENCODING"
        eof := encoding at: 256.
        (1 to: eof size) do: [:i |
            ((eof at: i) asString = '0') 
                ifTrue:  [ostream writeBit: 0.]
                ifFalse: [ostream writeBit: 1.].
        ].

        "SIMPLY FLUSHING BYTE SHOULD PAD OUT TO BYTE SIZE"
        ostream flushByte.

    ].

]

"UNCOMPRESS FILE"
(is_uncompress = 1) ifTrue: [
    |treeBuilt decoded byte node tree|

" ISN'T THIS THE FILE WE ARE SUPPOSED TO BE WRITING TO???? "
" I THINK WE NEED TO OPEN A FILE TO READ AT SIZE - 1 "
    infile_name := Smalltalk arguments at: Smalltalk arguments size.
    infile := FileStream open: infile_name mode: FileStream read.

    (infile atEnd) ifFalse: [

        treeBuilt := 0.
        decoded := 0.
        stack := OrderedCollection new.

        "OPEN A BITSTREAM ON THE INPUT FILE"
        istream := BitStream new: infile.

        "READ FIRST BIT, SHOULD BE A 0"
        byte := 0.
        bit := istream nextBit.

        "READ THE FIRST BYTE AND INSERT INTO STACK"
        8 to: 1 by: -1 do: [:i |
            bit := istream nextBit.
            byte := byte bitAt: i put: bit.
        ].

        "SPECIAL CASE, CHECK FOR EOF"
        (byte = 0) ifTrue: [
            bit := istream nextBit.
            (bit = 1) ifTrue: [
                byte := byte + 256.
            ].
        ].

        "ADD FIRST ELEMENT TO STACK"
        node := Leaf new: byte count: ''.
        stack addFirst: node.

        "LOOP UNTIL STACK IS EMPTY TO BUILD DECODING TREE"
        [treeBuilt = 0] whileTrue: [
            byte := 0.
            op := istream nextBit.

            (op = 0) ifTrue: [
                "READ THE BYTE"
                8 to: 1 by: -1 do: [:i |
                    bit := istream nextBit.
                    byte := byte bitAt: i put: bit.
                ].

                "SPECIAL CASE, CHECK FOR EOF"
                (byte = 0) ifTrue: [
                    bit := istream nextBit.
                    (bit = 1) ifTrue: [
                        byte := byte + 256.
                    ].
                ].

                "ADD ELEMENT TO STACK"
                node := Leaf new: byte count: ''.
                stack addFirst: node.
            ].

            (op = 1) ifTrue: [
                right := stack removeFirst.
                (stack size = 0) 
                ifTrue: [
                    treeBuilt := 1.
                    tree := right.
                ]
                ifFalse: [
                    left := stack removeFirst.
                    temp := Tree new: -1 
                        count: '' left: left right: right.
                    stack addFirst: temp.
                ].
            ].
        ].

        "READ REST OF THE BITS TO WRITE OUT ORIGINAL FILE"
        eof := 256.
        ptr := tree.
        (ptr isLeaf) ifFalse: [
            [decoded = 0] whileTrue: [
                bit := istream nextBit.
                "TRAVEL DOWN LEFT BRANCH"
                (bit = 0) ifTrue: [
                    node := ptr getLeft.
                    ptr := node.
                ].

                (bit = 1) ifTrue: [
                    node := ptr getRight.
                    ptr := node.
                ].

                (node isLeaf = true) ifTrue: [
                    c := node char.
                    (c asInteger = eof) ifTrue: [
                        decoded := 1.
                    ]
                    ifFalse: [
                        stdout nextPutByte: c.
                        ptr := tree.

                    ].
                ].
            ].
        ].
    ].
]
