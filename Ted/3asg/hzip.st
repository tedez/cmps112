#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

|is_debug is_compress is_uncompress is_display 
file_name inFile root outFile istream ostream 
encoding ptr oFile has_ofile| 

has_ofile := false.

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
         outbits writeBit: 0.].
      char asInteger = 256 ifTrue: [
         outbits writeBit: 1.].
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

"
=============================================
                  MAIN
=============================================
"
"CHECKING THE CORRECT NUMBER OF ARGUMENTS"
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

"parse command line args"
(1 to: Smalltalk arguments size) do: [:index |
        arg := (Smalltalk arguments at: index).
        ((arg includes: $-) and: [(arg size) = 2]) ifTrue: [
		arg = '-d' ifTrue: [is_debug := 1].
		arg = '-c' ifTrue: [is_compress := 1].
		arg = '-u' ifTrue: [is_uncompress := 1].
		arg = '-t' ifTrue: [is_display := 1].
	]. ((arg includes: $-) and: [(arg size) > 2]) ifTrue: [
	       status := 127.
	       stderr << 'Usage: "' << arg << '" is not a valid option'
		      << nl << 'Valid options ---> (-c -d -t -u)' << nl.
	       ObjectMemory quit: status.
	]
    ].

" CREATING I/O STREAMS "
outFile := ''.
(Smalltalk arguments size) = 3 ifTrue: [
   has_ofile := true.
   outFile := (Smalltalk arguments at: 3).
   oFile := FileStream open: outFile mode: FileStream write. ]. 

" DEFAULTS TO STDOUT IF NO OUTFILE SPECIFIED"
(Smalltalk arguments size) = 2 ifTrue: [ 
   oFile := stdout. 
].

inFile := ''.
inFile := (Smalltalk arguments at: 2).

|iFile|
iFile := FileStream open: inFile mode: FileStream read.
"iFile catFile."
iReset := [
    iFile close.
    iFile := FileStream open: inFile mode: FileStream read.
].


"frequency table"
freqtable := ZeroArray new: 257.
"encoding table, holds huffman encoding for each char"
encoding := ZeroArray new: 257.
"priority queue for building tree"
priority := SortedCollection new.
"manually insert EOF"
freqtable at: 256 put: 1.


"display encoding table"
(is_display = 1) ifTrue:
[
    
    "build frequency table"
    iFile buildTable: freqtable.

    "add all elements from frequency table into PQ"
    freqtable keysAndValuesDo: 
    [:key :value |
       (value notNil and: [value > 0]) ifTrue: 
       [
        |node|
        node := Leaf new: key asCharacter count: value.
        priority add: node.
       ]
    ].

    "build huffman tree"
    [priority size > 1] whileTrue: 
    [
        |left right newtree|
        left := priority removeFirst.
        (priority size > 0) ifTrue:
        [
            right := priority removeFirst.
            (left notNil and: [right notNil]) ifTrue: 
            [
              newtree := Tree new: left char 
              count: left count + right count left: left right: right.
              priority add: newtree.
            ]
        ]
    ].

    root := priority removeFirst.

    "inorder DFS to build encoding table"
    root inorder: [:char :string |
        encoding at: (char asInteger) put: string.
    ] prefix: ''.

    "do a positively stupid amount of code just to format a table"
    encoding keysAndValuesDo: 
    [:key :value |
       (value notNil and: [value > 0]) ifTrue: 
       [
            count := freqtable at: key.
            numdigits := (count floorLog: 10) + 1.
            (key < 33 or: [key > 126]) ifTrue:
            [
                numdigits := numdigits + 1.
                (key = 256) ifTrue:
                [
                    stdout << 'EOF'.
                ]
                ifFalse:
                [
                    stdout << 'x'.
                    (key < 16) ifTrue:
                    [
                        stdout << '0'.
                    ].


                    tmp := key asInteger.
                    tmp printOn: stdout base: 16.
                ].
            ]
            ifFalse:
            [
                stdout << ' ' << key asCharacter.

            ].

            0 to: (8 - numdigits) do:
            [:i |
                stdout << ' '.
            ].
            stdout << count << '  ' << value << nl.
        ]
    ].

]

"compress the file"
(is_compress = 1) ifTrue:
[
    "build frequency table"
    iFile buildTable: freqtable.

    "add all elements from frequency table into PQ"
    freqtable keysAndValuesDo: 
    [:key :value |
        (value notNil and: [value > 0]) ifTrue: 
        [
            |node|
            node := Leaf new: key asCharacter count: value.
            priority add: node.
        ]
    ].

    "build huffman tree"
    [priority size > 1] whileTrue: 
    [
        |left right newtree|
        left := priority removeFirst.
        (priority size > 0) ifTrue:
        [
            right := priority removeFirst.
            (left notNil and: [right notNil]) ifTrue: 
            [
              newtree := Tree new: left char 
              count: left count + right count left: left right: right.
              priority add: newtree.
            ]
        ]
    ].

    root := priority removeFirst.
    (root isLeaf = false) ifTrue: 
    [
        "inorder DFS to build encoding table"
        root inorder: [:char :string |
            encoding at: (char asInteger) put: string.
        ] prefix: ''.

        "open a bitstream for the outfile"
        ostream := OutBits new: oFile.

        "write encoding tree out to file"
        root dumpTree: ostream.

        "dump last bit to indicate return from root for decode"
        ostream writeBit: 1.

        "write out encoded version of each byte"
        iFile position: 0.
        [iFile atEnd not] whileTrue:
        [
            temp := encoding at: iFile next asInteger.
            (1 to: temp size) do: [:i |
                ((temp at: i) asString = '0') 
                    ifTrue:  [ostream writeBit: 0.]
                    ifFalse: [ostream writeBit: 1.].
            ].
        ].

        "write out EOF encoding"
        eof := encoding at: 256.
        (1 to: eof size) do: 
        [:i |
            ((eof at: i) asString = '0') 
                ifTrue:  [ostream writeBit: 0.]
                ifFalse: [ostream writeBit: 1.].
        ].

        "simply flushing byte should pad out to byte size"
        ostream flushByte.

    ].

]


"uncompress file"
(is_uncompress = 1) ifTrue:
[
    |treeBuilt decoded byte node tree ostream|

    " ADD CHECK FOR HAS_OFILE HERE "
    ostream := stdout.
    (has_ofile = true) ifTrue: [
        " Create reference to FileStream oFile "
        " This lets us keep our nextPutByte generic and
          bypass additional checks "
        ostream := oFile.
    ].

    (iFile atEnd) ifFalse:
    [

        treeBuilt := 0.
        decoded := 0.
        stack := OrderedCollection new.

        "open a bitstream on the input file"
        istream := BitStream new: iFile.

        "read first bit, should be a 0"
        byte := 0.
        bit := istream nextBit.

        "read the first byte and insert into stack"
        8 to: 1 by: -1 do: 
        [:i |
            bit := istream nextBit.
            byte := byte bitAt: i put: bit.
        ].

        "special case, check for EOF"
        (byte = 0) ifTrue: 
        [
            bit := istream nextBit.
            (bit = 1) ifTrue: 
            [
                byte := byte + 256.
            ].
        ].

        "add first element to stack"
        node := Leaf new: byte count: ''.
        stack addFirst: node.

        "loop until stack is empty to build decoding tree"
        [treeBuilt = 0] whileTrue:
        [
            byte := 0.
            op := istream nextBit.

            (op = 0) ifTrue:
            [
                "read the byte"
                8 to: 1 by: -1 do: 
                [:i |
                    bit := istream nextBit.
                    byte := byte bitAt: i put: bit.
                ].

                "special case, check for EOF"
                (byte = 0) ifTrue: 
                [
                    bit := istream nextBit.
                    (bit = 1) ifTrue: 
                    [
                        byte := byte + 256.
                    ].
                ].

                "add element to stack"
                node := Leaf new: byte count: ''.
                stack addFirst: node.
            ].

            (op = 1) ifTrue:
            [
                right := stack removeFirst.
                (stack size = 0) 
                ifTrue:
                [
                    treeBuilt := 1.
                    tree := right.
                ]
                ifFalse:
                [
                    left := stack removeFirst.
                    temp := Tree new: -1 
                        count: '' left: left right: right.
                    stack addFirst: temp.
                ].
            ].
        ].

        "read rest of the bits to write out original file"
        eof := 256.
        ptr := tree.
        (ptr isLeaf) ifFalse: 
        [
            [decoded = 0] whileTrue:
            [
                bit := istream nextBit.
                "travel down left branch"
                (bit = 0) ifTrue:
                [
                    node := ptr getLeft.
                    ptr := node.
                ].

                (bit = 1) ifTrue:
                [
                    node := ptr getRight.
                    ptr := node.
                ].

                (node isLeaf = true) ifTrue:
                [
                    c := node char.
                    (c asInteger = eof) ifTrue:
                    [
                        decoded := 1.
                    ]
                    ifFalse:
                    [
                        ostream nextPutByte: c.
                        ptr := tree.

                    ].
                ].
            ].
        ].
    ].

]

