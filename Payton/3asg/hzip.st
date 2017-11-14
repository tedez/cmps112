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

FileStream extend [
	nl := Character nl.
	buildTable: table [
		[self atEnd not] whileTrue: [
			table incr: self next asInteger.
		]
	]
]

" ======== MAIN ======== "

freqtable := ZeroArray new: 257.
freqtable at: 256 put: 1.

|debug_f compress_f uncompress_f display_f f_name
in_file out_file root in_stream out_stream encoding ptr|

debug_f      := 0.
compress_f   := 0.
uncompress_f := 0.
display_f    := 0.
f_name       := ''.

" Start parsing cmd line args "
(1 to: Smalltalk arguments size) do:
	[:index | 
		arg := (Smalltalk arguments at: index).
		arg = '-d' ifTrue: [debug_f := 1].
		arg = '-c' ifTrue: [compress_f := 1].
		arg = '-u' ifTrue: [uncompress_f := 1].
		arg = '-t' ifTrue: [display_f := 1].
	].

" Try to open file "
f_name := Smalltalk arguments at: Smalltalk arguments size.
in_file := FileStream open: f_name mode: FileStream read.

" build the freqtable with the file in in_file "
in_file buildTable: freqtable.

" Print out freqtable? "
freqtable keysAndValuesDo: [:key :value |
	(value notNil and: [value > 0]) ifTrue: [
		stdout << 'freq[' << key asCharacter visible << '] = ' << value << nl.
	]

].
