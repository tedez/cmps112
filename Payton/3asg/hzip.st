" PUT MACKEY's GST LINK HERE "

nl := Character nl.

Character extend [
	isPrint [
		^ (Character space <= self) & (self <= $~)
	]
	visible [
		self isPrint ifTrue: [^ '$', self asString]
			     ifFalse: [^ self asInteger printStringRadix: 8]
	]
]

Object subclass: ZeroArray [
	|theArray|

	" Disallowing creating a ZeroArray w/o specifying args "
	ZeroArray class >> new [
		self shouldNotImplement.
	]

	ZeroArray class >> new: size [
		|result|
		result := super new. " Needed in every subclass constructor. "
		result init: size.
		^ result " ^ objectToReturn "
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
			ifTrue: [self at: index put: 0].
		self at: index put: (self at: index) + 1.
	]

	keysAndValuesDo: aBlock [
		(0 to: (self size) - 1) do: [:index |
			aBlock value: index value: (self at: index).
		]
	]
]

""
" OutBits: Opens a file given as the first arg and writes out bits
		   to either stdout or a file. "
		   "Bits -> String"
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
" BitStream:  Takes in a string from a stream and stores it's 
              bits internally in nextBit "
              "String -> Bits"
""
Object subclass: BitStream [
	|bitIndex byte myStream|
	BitStream class >> new: fileStream [
		|result|
		result := super new.
		result init: fileStream.
		^ result
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
		^ byte value bitAt: bitIndex
	]
	atEnd [
		^ bitIndex = 1 and: [myStream atEnd]
	]
]

Object subclass: Leaf [
	|char count|
	char [ ^ char ]
	count [ ^ count ]

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

	inorder: visitor prefix: string [
		visitor value: char value: string.
	]
]

Leaf subclass: Tree [
	|left right|

	Tree class >> new: aChar count: aCount [
		self shouldNotImplement
	]

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

	inorder: visitor prefix: string [
		left inorder: visitor prefix: string, '0'.
		right inorder: visitor prefix: string, '1'.
	]

	printTree: outbits [
		" Send the left side of the tree to outbits "
		left printTree: outbits.
		" Send the right side of the tree to outbits "
		right printTree: outbits.
		outbits writeBit: 1.
	]
]


FileStream extend [
	nl := Character nl.
	buildTable: table [
		[self atEnd not] whileTrue: [
			" next gives us our string value except a newline "
			" which we then cast to an int "
			table incr: self next asInteger.
		]
	]
]
" ======== MAIN ======== "

|debug_f compress_f uncompress_f display_f f_name
in_file out_file root in_stream out_stream h_encoded_table root_node|

debug_f      := 0.
compress_f   := 0.
uncompress_f := 0.
display_f    := 0.
f_name       := ''.

freqtable := ZeroArray new: 257.
freqtable at: 256 put: 1.

" Priority queue for decoding tree. "
pq := SortedCollection new.

" Start parsing cmd line args "
(1 to: Smalltalk arguments size) do:
	[:index | 
		arg := (Smalltalk arguments at: index).
		arg = '-d' ifTrue: [debug_f := 1].
		arg = '-c' ifTrue: [compress_f := 1].
		arg = '-u' ifTrue: [uncompress_f := 1].
		arg = '-t' ifTrue: [display_f := 1].
	].

(display_f = 1) ifTrue: [
	" Try to open file "
	f_name := Smalltalk arguments at: Smalltalk arguments size.
	in_file := FileStream open: f_name mode: FileStream read.

	" build the freqtable with the file in in_file "
	in_file buildTable: freqtable.

	"(b) Add all elements in freqtable to our PQ "
	freqtable keysAndValuesDo: [:key :value |
		(value notNil and: [value > 0]) ifTrue: [
			|node|
			node := Leaf new: key asCharacter count: value.
			pq add: node.
		]

	].

	" print pq, for debug "
	stdout << pq << nl.

	"(c) build huff tree "
	(pq size > 1) whileTrue: [
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

]
