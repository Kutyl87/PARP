
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


    addPath: aDirStr destination: aLocation [
        paths at: aDirStr put: aLocation.
    ]

    getDestination: aDirStr [
        ^ paths at: aDirStr
    ]

    addItem: aItemStr amount: anInt [
        items at: aItemStr put: anInt.
    ]

    removeItem: aItemStr amount: anInt [
        | itemAmount |
        itemAmount := items at: aItemStr.
        [anInt >= itemAmount] ifTrue: [
            items at: aItemStr put: [itemAmount-anInt].
            [[items at: aItemStr] == 0] ifTrue: [
                items removeKey: aItemStr.
            ].

            ^ true
        ].

        ^ false
    ]

]

| entrance inFrontOfFirstTunnel inFrontOfSecondTunnel inFrontOfThirdTunnel |

entrance := Location new.
entrance 
    init: 'entrance'
    description: 'entrance to tunnel'.
entrance addPath: 'forward' destination: inFrontOfFirstTunnel.

inFrontOfFirstTunnel := Location new.
inFrontOfFirstTunnel
    init: 'in front of first tunnel'
    description: 'in front of first tunnel'.

inFrontOfSecondTunnel := Location new.
inFrontOfSecondTunnel
    init: 'in front of second tunnel'
    description: 'in front of second tunnel'.

inFrontOfThirdTunnel := Location new.
inFrontOfThirdTunnel
    init: 'in front of third tunnel'
    description: 'in front of third tunnel'.




