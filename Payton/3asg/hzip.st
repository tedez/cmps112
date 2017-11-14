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


