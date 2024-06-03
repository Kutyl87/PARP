
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




"The germ of a text adventure game
Marcin Szlenk 2024"

"Game class

Responsibilities:
* print/read outputs/inputs
* handle game loop
* handle 'instructions' and 'halt' commands

Collaborators:
* ..."

Object subclass: Game [
    | isGameOver inventory currentLocation locations events energy maxEnergy dead restingPace riddle |

    introductionText := #(
        ''
    ).

    instructionsText := #(
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
 
    Game class >> new [
        | g |
        g := super new.
        g init.
        ^g
    ]

    init [
        isGameOver := false.
        inventory := OrderedCollection new.
        currentLocation := #{inFrontOfFirstTunnel}.
        energy := 100.
        maxEnergy := 100.
        dead := false.
        restingPace := 10.
        riddle := Array new: 2. 
        riddle at: 1 put: 0; at: 2 put: 0.
    ]

    "Print strings from array in separate lines."
    printLines: anArray [
        anArray do: [ :string | Transcript show: string; cr ]
    ]

    printIntroduction [
       self printLines: introductionText.
    ]

    printInstructions [
        self printLines: instructionsText.
    ]

    readCommand [
        | cmdIn |
        Transcript show: '> '.
        cmdIn := FileStream stdin nextLine.
        ^ cmdIn substrings
    ]

    getCurrentLocation [
        ^ currentLocation
    ]

    "Movement"

    go: dirStr [
        | destination |
        destination := currentLocation getDestination: dirStr.
        destination ifNotNil: [
            currentLocation := destination.
            self printLines: #('Went to ').
        ]
        ifNil: [
            self printLines: #('You cannot go there!' '').
        ].

    ]



    "Run the game."
    run [
        | cmdArr cmdName cmdTaill isUnknown |

        self printIntroduction.
        self printInstructions.

        "Game loop."
        [isGameOver] whileFalse: [

            isUnknown := true.
            cmdArr := self readCommand.
            cmdName := cmdArr first.
            cmdArr display.            

            cmdName = 'instructions' ifTrue: [
                self printInstructions.
                isUnknown := false
            ].

            cmdName = 'quit' ifTrue: [
                isGameOver := true.
                isUnknown := false
            ].

            cmdName = 'go' ifTrue: [
                self go: [cmdArr at: 2].
                isUnknown := false
            ].
                
            isUnknown ifTrue: [
                self printLines: #('Unknown command.' '')
            ]
        ]
    ]
]

Game new run.
