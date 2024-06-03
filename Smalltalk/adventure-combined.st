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
        itemAmount := items at: aItemStr.
        (itemAmount isNil or: [itemAmount < anInt]) ifTrue: [^ false].
        items at: aItemStr put: (itemAmount - anInt).
        (items at: aItemStr) = 0 ifTrue: [items removeKey: aItemStr].
        ^ true
    ]
]Object subclass: Game [
    | isGameOver inventory currentLocation energy maxEnergy dead restingPace riddle entrance inFrontOfFirstTunnel inFrontOfSecondTunnel inFrontOfThirdTunnel |

    Game class >> new [
        ^ super new init
    ]

    init [
        entrance := Location new init: 'entrance' description: 'entrance to tunnel'.
        inFrontOfFirstTunnel := Location new init: 'in front of first tunnel' description: 'in front of first tunnel'.
        inFrontOfSecondTunnel := Location new init: 'in front of second tunnel' description: 'in front of second tunnel'.
        inFrontOfThirdTunnel := Location new init: 'in front of third tunnel' description: 'in front of third tunnel'.

        entrance addPath: 'forward' destination: inFrontOfFirstTunnel.

        isGameOver := false.
        inventory := OrderedCollection new.
        currentLocation := entrance.
        energy := 100.
        maxEnergy := 100.
        dead := false.
        restingPace := 10.
        riddle := Array new: 2.
        riddle at: 1 put: 0; at: 2 put: 0.
        ^ self
    ]

    printLines: anArray [
        anArray do: [ :string | Transcript show: string; cr ].
    ]

    printIntroduction [
        self printLines: #('Welcome to the Adventure Game!').
    ]

    printInstructions [
        self printLines: #(
            'Available commands are:'
            ''
            'instructions  -- to see these instructions.'
            'go [direction]'
            'Available directions: forward, back, left, right'
            'take [object]'
            'inspect [object]'
            'craft [object]'
            'look'
            'inventory     -- to see items in your inventory'
            'rest          -- to regenerate energy'
            'quit          -- to end the game and quit.'
            'buy           -- to buy an item'
            ''
        ).
    ]

    readCommand [
        | cmdIn |
        Transcript show: '> '.
        cmdIn := FileStream stdin nextLine.
        ^ cmdIn substrings.
    ]

    getCurrentLocation [
        ^ currentLocation.
    ]

    go: dirStr [
        | destination |
        destination := currentLocation getDestination: dirStr.
        destination ifNotNil: [
            currentLocation := destination.
            self printLines: #('Went to the new location.' '').
            self printLines: {currentLocation getDescription. ''}.
        ] ifNil: [
            self printLines: #('You cannot go there!' '').
        ].
    ]

    look [
        self printLines: {currentLocation getDescription. ''}.
    ]

    run [
        | cmdArr cmdName isUnknown |

        self printIntroduction.
        self printInstructions.

        [isGameOver] whileFalse: [
            isUnknown := true.
            cmdArr := self readCommand.
            cmdName := cmdArr first.

            cmdName = 'instructions' ifTrue: [
                self printInstructions.
                isUnknown := false.
            ].

            cmdName = 'quit' ifTrue: [
                isGameOver := true.
                isUnknown := false.
            ].

            cmdName = 'go' ifTrue: [
                self go: (cmdArr at: 2).
                isUnknown := false.
            ].

            cmdName = 'look' ifTrue: [
                self look.
                isUnknown := false.
            ].

            isUnknown ifTrue: [
                self printLines: #('Unknown command.' '').
            ].
        ].
    ]
]

Game new run.
