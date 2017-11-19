" PUT MACKEY's GST LINK HERE "


nl := Character nl.

FileStream extend [
	nl := Character nl.
	buildTable: table [
		[self atEnd not] whileTrue: [
			" NEXT GIVES US OUR STRING VALUE EXCEPT A NEWLINE "
			" WHICH WE THEN CAST TO AN INT "
			table incr: self next asInteger.
		]
	]
]

Character extend [
	isPrint [
		^(Character space <= self) & (self <= $~)
	]
	visible [
		self isPrint ifTrue: [^ '$', self asString]
			     ifFalse: [^ self asInteger printStringRadix: 8]
	]
]

""
" OUTBITS: OPENS A FILE GIVEN AS THE FIRST ARG AND WRITES OUT BITS
		   TO EITHER STDOUT OR A FILE. "
		   "BITS -> STRING"
""
Object subclass: OutBits [
	|bitIndex currentByte myStream|
	OutBits class >> new [
		self shouldNotImplement.
	]
	OutBits class >> new: fileStream [
		|result|
		result := super new.
		result init: fileStream.
		^ result
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

""
" BITSTREAM:  TAKES IN A STRING FROM A STREAM AND STORES IT'S 
              BITS INTERNALLY IN NEXTBIT "
              "STRING -> BITS"
""

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

	" DISALLOWING CREATING A ZEROARRAY W/O SPECIFYING ARGS "
	ZeroArray class >> new [
		self shouldNotImplement.
	]

	ZeroArray class >> new: size [
		|result|
		result := super new. " NEEDED IN EVERY SUBCLASS CONSTRUCTOR. "
		result init: size.
		^ result " ^ OBJECTTORETURN "
	]

	init: size [
		theArray := Array new: size.
	]

	size [
		^theArray size.
	]

	at: index [
		^theArray at: index + 1.
	]

	at: index put: value [
		^theArray at: index + 1 put: value.
	]

	incr: index [
		(self at: index) isNil
			ifTrue: [self at: index put: 0].
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

	char [ ^char ]
	count [ ^count ]
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
		"
			&,|     - NONLAZY EVAL
			AND, OR - LAZY EVAL
		"
		" IF THE CHARACTER COUNTS ARE THE SAME,
		  USE THE ASCII CHAR VALUE AS THE TIE-BREAKER "
		| ((count = other count) & (char <= other char))
		|FixVimHighlighting|
	]

	printBase: aStream [
		^ aStream << self class << '(' << char << ',' << count
	]

	printOn: aStream [
		(self printBase: aStream) << ')'.
	]

	printTree: outbits [
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

	printTree: outbits [
		" SEND THE LEFT SIDE OF THE TREE TO OUTBITS "
		left printTree: outbits.
		" SEND THE RIGHT SIDE OF THE TREE TO OUTBITS "
		right printTree: outbits.
		outbits writeBit: 1.
	]

	inorder: visitor prefix: string [
		left inorder: visitor prefix: string, '0'.
		right inorder: visitor prefix: string, '1'.
	]

]



" ======== MAIN ======== "

|debug_f compress_f uncompress_f display_f f_name
in_file out_file root in_stream out_stream encoded_table root_node|

debug_f      := 0.
compress_f   := 0.
uncompress_f := 0.
display_f    := 0.
f_name       := ''.

freqtable := ZeroArray new: 257.
freqtable at: 256 put: 1.
encoded_table := ZeroArray new: 257.

" PRIORITY QUEUE FOR DECODING TREE. "
pq := SortedCollection new.

" START PARSING CMD LINE ARGS "
(1 to: Smalltalk arguments size) do:
	[:index | 
		arg := (Smalltalk arguments at: index).
		arg = '-d' ifTrue: [debug_f := 1].
		arg = '-c' ifTrue: [compress_f := 1].
		arg = '-u' ifTrue: [uncompress_f := 1].
		arg = '-t' ifTrue: [display_f := 1].
	].

" DISPLAY ENCODING TABLE "
(display_f = 1) ifTrue: [
	" TRY TO OPEN FILE "
	f_name := Smalltalk arguments at: Smalltalk arguments size.
	in_file := FileStream open: f_name mode: FileStream read.

	" BUILD THE FREQTABLE WITH THE FILE IN IN_FILE "
	in_file buildTable: freqtable.

	"(B) ADD ALL ELEMENTS IN FREQTABLE TO OUR PQ "
	" SINCE THE PQ IS A SORTEDCOLLECTION, IT'S ALREADY SORTED
	  IN ASCENDING ORDER! HOW NEAT IS THAT? "
	freqtable keysAndValuesDo: [:key :value |
		(value notNil and: [value > 0]) ifTrue: [
			|node|
			"STDOUT << 'VALUE=' << VALUE << NL."
			node := Leaf new: key asCharacter count: value.
			"NODE INORDER: [:CHAR :STRING |
				STDOUT << '[' << CHAR << ']=' << STRING << NL.
			] PREFIX: ''."
			pq add: node.
		]

	].

	"freqtable keysAndValuesDo: [:key :value |
		(value notNil and: [value > 0]) ifTrue: [
			stdout << 'key: ' << key asCharacter visible << ', val: ' << value << nl.
		]
	]."

	" print pq, for debug "
	"stdout << pq << nl."

	"(C) BUILDING THE ENCODE TREE OUT OF THE PQ"
	[pq size > 1] whileTrue: [
		|left right new_tree|
		left := pq removeFirst.

		(pq size > 0) ifTrue: [
			right := pq removeFirst.
			(left notNil and: [right notNil]) ifTrue: [
				new_tree := Tree new: left char
				count: left count + right count left: left right: right.
				pq add: new_tree.
				stdout << new_tree << nl << nl.
			]
		]
	].
	root_node := pq removeFirst.

	stdout << nl << nl << nl.

	stdout << root_node << nl.

	"(D) CONSTRUCT ENCODING STRING USING DFS "
	root_node inorder: [:char :string |
		"stdout << 'char: ' << char << ', str: ' << string << nl."
		encoded_table at: (char asInteger) put: string.
	] prefix: ''.

	"(E) FORMAT THE ENCODING TABLE "
	encoded_table keysAndValuesDo: [:key :value |
		(value notNil and: [value > 0]) ifTrue: [
			count := freqtable at: key.
			numDigits := (count floorLog: 10) + 1.
			(key < 33 or: [key > 126]) ifTrue: [
				numDigits := numDigits + 1.
				(key = 256) ifTrue: [
					stdout << 'EOF'.
				]
				ifFalse: [
					stdout << 'x'.
					(key < 16) ifTrue: [
						stdout << '0'.
					].

					temp := key asInteger.
					temp printOn: stdout base: 16.
				].
			]
			ifFalse: [
				stdout << ' ' << key asCharacter.
			].

			0 to: (8 - numDigits) do: [:i |
				stdout << ' '.
			].
			stdout << count << '  ' << value << nl.
		]
	].
]

" COMPRESS THE FILE "
(compress_f = 1) ifTrue: [
	out_file_name := Smalltalk arguments at: Smalltalk arguments size.
	in_file_name  := Smalltalk arguments at:
						((Smalltalk arguments size) - 1).
	in_file := FileStream open: in_file_name mode: FileStream read.
	out_file := FileStream open: out_file_name mode: FileStream write.

	" BUILD FREQTABLE FROM OUR INFILE "
	in_file buildTable: freqtable.

	freqtable keysAndValuesDo: [:key :value |
		(value notNil and: [value > 0]) ifTrue: [
			|node|
			node := Leaf new: key asCharacter count: value.
			pq add: node.	
		]
	].

	" BUILD ENCODE TREE"
	[pq size > 1] whileTrue: [
		|left right new_tree|
		left := pq removeFirst.
		(pq size > 0) ifTrue: [
			right := pq removeFirst.
			(left notNil and: [right notNil]) ifTrue: [
				new_tree := Tree new: left char
				count: left count + right count left: left right: right.
				pq add: new_tree.	
			]
		]
	].

	root_node := pq removeFirst.
	(root_node isLeaf = false) ifTrue: [
		"USE DFS TO BUILD ENCODING STRING "
		root_node inorder: [:char :string |
			encoded_table at: (char asInteger) put: string.
		] prefix: ''.

		" OPEN BITSTREAM FOR OUT_FILE "
		out_stream := OutBits new: out_file.

		" WRITE ENCODING TREE TO FILE "
		root_node printTree: out_stream.

		" WRITE LAST BIT TO SIGNAL 'EOF'"
		out_stream writeBit: 1.

		" WRITE OUT ENCODED BYTES "
		in_file position: 0.
		[in_file atEnd not] whileTrue: [
			temp := encoded_table at: in_file next asInteger.
			(1 to: temp size) do: [:i |
				((temp at: i) asString = '0')
					ifTrue: [out_stream writeBit: 0.]
					ifFalse: [out_stream writeBit: 1.].
			].
		].

		" WRITE EOF ENCODING "
		eof := encoded_table at: 256.
		(1 to: eof size) do: [:i |
			((eof at: i) asString = '0')
				ifTrue: [out_stream writeBit: 0.]
				ifFalse: [out_stream writeBit: 1.].
		].

		" PAD OUT TO BYTE SIZE "
		out_stream flushByte.					
	].

]

" UNCOMPRESS "
(uncompress_f = 1) ifTrue: [
	|built decoded byte node tree|
	in_file_name := Smalltalk arguments at: Smalltalk arguments size.
	in_file := FileStream open: in_file_name mode: FileStream read.

	(in_file atEnd) ifFalse: [
		built := 0.
		decoded := 0.
		stack := OrderedCollections new.

		" CREATE BITSTREAM ON INPUT FILE "
		in_stream := BitStream new: in_file.

		" READ FIRST BYTE POP ONTO STACK "
		8 to: 1 by: -1 do: [:i |
			bit := byte bitAt: i put: bit.
		].
		
		" EOF CHECK "
		(byte = 0) ifTrue: [
           bit := in_stream nextBit.
           (bit = 1) ifTrue: [
                byte := byte + 256
           ].
		].
	    
        " POP ELEMENT ON STACK "
        node := Leaf new: byte count: ''.
        stack addFirst: node.

        " BUILD DECODING TREE "

        [built = 0] whileTrue: [
            byte := 0.
            op := in_stream nextBit.

            " READ BYTE "
            (op = 0) ifTrue: [
                8 to: 1 by: -1 do: [:i |
                    bit := in_stream nextBit.
                    byte := byte bitAt: i put: bit.    
                ].

                " EOF CHECK "
                (byte = 0) ifTrue: [
                    bit := in_stream nextBit.
                    (bit = 1) ifTrue: [
                        byte := byte + 256.
                    ].
                ].

                " POP ELEMENT ON STACK "
                node := Leaf new: byte count: ''.
                stack addFirst: node.
            ].
            
            (op = 1) ifTrue: [
                r := stack removeFirst.
                (stack size = 0) ifTrue: [
                    built := 1.
                    tree := right.
                ]
                ifFalse: [
                    left := stack removeFirst.
                    temp := Tree new: -1 count: '' left: left right: right.
                    stack addFirst: temp.
                ].
            ].
        ].

        " REST OF BITS "
        eof := 256.
        ptr := tree.
        (ptr isLeaf) ifFalse: [
            [decode = 0] whileTrue: [
                bit := in_stream nextBit.

                " LEFT BRANCH TRAVERSAL "
                (bit = 0) ifTrue: [
                    node := ptr getLeft.
                    prt := node.
                ].

                (bit = 1) ifTrue: [
                    node := ptr getRight.
                    ptr := node.
                ].

                (node isLeaf = true) ifTrue: [
                    c := node char.
                    (c asInteger = eof) ifTrue: [
                        decoded := 1.
                    ] ifFalse: [
                        stdout << nextPutByte: c.
                        ptr := tree.
                    ].
                ].
            ].
        ].
    ]
]
