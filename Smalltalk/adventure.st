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
        currentLocation := Entrance.
        locations := Dictionary new.
        events := OrderedCollection new.
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
        Transcript show: '> '.
        ^ FileStream stdin nextLine
    ]

    "Run the game."
    run [
        | cmd isUnknown |

        self printIntroduction.
        self printInstructions.

        "Game loop."
        [isGameOver] whileFalse: [

            isUnknown := true.
            cmd := self readCommand.

            cmd = 'instructions' ifTrue: [
                self printInstructions.
                isUnknown := false
            ].

            cmd = 'quit' ifTrue: [
                isGameOver := true.
                isUnknown := false
            ].
                
            isUnknown ifTrue: [
                self printLines: #('Unknown command.' '')
            ]
        ]
    ]
]

Game new run.
