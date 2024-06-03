Object subclass: Location [
    | name description items paths |

    init: aName description: aDescription [
        name := aName.
        description := aDescription.
        items := Dictionary new.
        paths := Dictionary new.
    ]

    getName [
        ^ name
    ]

    getDescription [
        ^ description
    ]

    changeDescription: aDescription [
        description := aDescription.
    ]

    addPath: aDirStr destination: aLocation [
        paths at: aDirStr put: aLocation.
    ]

    getDestination: aDirStr [
        ^ paths at: aDirStr ifAbsent: [nil].
    ]

    addItem: aItemStr amount: anInt [
        items at: aItemStr put: anInt.
    ]

    removeItem: aItemStr amount: anInt [
        | itemAmount |
        itemAmount := items at: aItemStr ifAbsent: [^ false].
        (itemAmount < anInt) ifTrue: [^ false].
        items at: aItemStr put: (itemAmount - anInt).
        (items at: aItemStr) = 0 ifTrue: [items removeKey: aItemStr].
        ^ true
    ]
]