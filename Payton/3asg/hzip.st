#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

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
	|bitIndex byte myStream stringIndex writeIndex writeByte|
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

	putBit: aBit [
		writeIndex := writeIndex - 1.
		writeByte := writeByte bitAt: writeIndex put: aBit.
		writeIndex = 1 ifTrue: [
		myStream nextPutByte: writeByte.
			writeByte := 0.
			writeIndex := 9.
		].
	]

	putString: aString [
		aString do: [:index|
			(index asString = '0')
				ifTrue: [self putBit: 0]
				ifFalse: [self putBit: 1].
		].
	]

	padZeros [
		[writeIndex > 1 & writeIndex < 9] whileFalse: [
			self putBit: 0.
			writeIndex := writeIndex - 1.
		].
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
	]

	printBase: aStream [
		|FixVimHighlighting|
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

	right [ ^right ]
	left [ ^left ]

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
in_file out_file in_stream out_stream encoded_table root_node|

debug_f      := 0.
compress_f   := 0.
uncompress_f := 0.
display_f    := 0.
in_file      := ''.
out_file     := stdout.
f_name       := ''.

freqtable := ZeroArray new: 257.
freqtable at: 256 put: 1.
encoded_table := ZeroArray new: 257.

" PRIORITY QUEUE FOR DECODING TREE. "
pq := SortedCollection new.

" START PARSING CMD LINE ARGS "
(1 to: Smalltalk arguments size) do: [:index | 
	arg := (Smalltalk arguments at: index).
	(arg includes: $-) ifTrue: [
		(arg includes: $d) ifTrue: [debug_f := 1].
		(arg includes: $c) ifTrue: [compress_f := 1].
		(arg includes: $u) ifTrue: [uncompress_f := 1].
		(arg includes: $t) ifTrue: [display_f := 1].
	] ifFalse: [
		" If the arguments don't have a hyphen, then they
		  are assumed to be io files. "
	    (in_file isEmpty) ifTrue: [
	    	in_file := arg.
	    ] ifFalse: [
	    	out_file := arg.
	    ]
	]
].

|read_file write_file|
read_file := FileStream open: in_file mode: FileStream read.
write_file := FileStream open: out_file mode: FileStream write.

" Build freqtable only if we're not uncompressing. "
(uncompress_f = 0) ifTrue: [
	read_file buildTable: freqtable.
].

"(B) ADD ALL ELEMENTS IN FREQTABLE TO OUR PQ "
	" SINCE THE PQ IS A SORTEDCOLLECTION, IT'S ALREADY SORTED
	  IN ASCENDING ORDER! HOW NEAT IS THAT? "
(uncompress_f = 0) ifTrue: [
	(0 to: freqtable size - 1) do: [:index |
		|aLeaf aCount aChar|
		aCount := freqtable at: index.
		aCount isInteger ifTrue: [
			aChar := index asCharacter.
			aLeaf := Leaf new: aChar count: aCount.
			pq add: aLeaf.
		].
	].	
].

"(C) BUILDING THE ENCODE TREE OUT OF THE PQ"
(uncompress_f = 0) ifTrue: [
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
].

"(D) CONSTRUCT ENCODING STRING USING DFS "
(uncompress_f = 0) ifTrue: [
	root_node inorder: [:char :string |
		"stdout << 'char: ' << char << ', str: ' << string << nl."
		encoded_table at: (char asInteger) put: string.
	] prefix: ''.
].


" DISPLAY ENCODING TABLE (-t)"
(display_f = 1) ifTrue: [
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
	read_file close.
].

" COMPRESS THE FILE "
(compress_f = 1) ifTrue: [
	|encoded_string|
	aBitStream := BitStream new: write_file.

	read_file reset.

	root_node printTree: aBitStream.
	aBitStream putBit: 1.

	[read_file atEnd not] whileTrue: [
		|index|
		index := read_file next asInteger.
		encoded_string := (encoded_table at: index).
		aBitStream putString: encoded_string.
	].
	encoded_string := encoded_table at: 256.
	aBitStream putString: encoded_string.
	aBitStream padZeros.
	read_file close.
	write_file close.
].

" UNCOMPRESS "
(uncompress_f = 1) ifTrue: [
	"|built decoded byte node tree|"
	out_BitStream := BitStream new: read_file.
	in_BitStream  := BitStream new: write_file.

	stack := OrderedCollection new.
	built := 0.
	decoded := nil.

	[(out_BitStream atEnd not) & (built = 0)] whileTrue: [
		|aChar|

		aChar := out_BitStream nextBit.
		(aChar = 0) ifTrue: [
			rChar := 0.
			base := 1.

			(1 to: 8) do: [:i |
				|cBit|
				cBit := out_BitStream nextBit.
				(cBit = 1) ifTrue: [
					rChar := rChar + base.
				].
				base := base * 2.
			].

			rChar = 0 ifTrue: [
				|cBit|
				cBit := out_BitStream nextBit.
				(cBit = 1) ifTrue: [
					stack addFirst: (Leaf new: 256 asCharacter count: 1).
				] ifFalse: [
					stack addFirst: (Leaf new: 0 asCharacter count: 1).
				].
			] ifFalse: [
				stack addFirst: (Leaf new: rChar asCharacter count: 1).
			].
		] ifFalse: [
			(stack size > 0) ifTrue: [
				right := stack removeFirst.
				(stack size = 0) ifTrue: [
					decoded := right.
					built := 1.
				] ifFalse: [
					left := stack removeFirst.
					stack addFirst: (Tree new: (left char) count: 1 left: left right: right).
				].
			].
		].
    ].

    tempTree := decoded.
	eof := false.
	[(out_BitStream atEnd not) & (eof = false)] whileTrue: [
	  |enbit|

	  enbit := out_BitStream nextBit.

	  enbit = 0 ifTrue: [
	     tempTree  isLeaf ifTrue: [
	        tempTree char asInteger = 256 ifTrue: [eof = true.].
	     ] ifFalse: [
	       tempTree := tempTree left.
	       tempTree  isLeaf ifTrue: [
	          tempTree char asInteger = 256 ifTrue: [ eof = true.]
	             ifFalse: [
	                write_file << tempTree char.
	             ].
	           tempTree := decoded.
	        ].
	     ].
	  ] ifFalse: [
	     tempTree isLeaf ifTrue: [
	        tempTree char asInteger = 256 ifTrue: [eof := true.].
	     ] ifFalse: [
	        tempTree := tempTree right.
	        tempTree isLeaf ifTrue: [
	           tempTree char asInteger = 256 ifTrue: [ eof := true.]
	              ifFalse: [
	                 write_file << tempTree char.
	              ].
	           tempTree := decoded.
	        ].
	     ].
	  ].
	].

	read_file close.
	write_file close.
].
