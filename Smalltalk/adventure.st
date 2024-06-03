Object subclass: Game [
    | isGameOver inventory currentLocation energy maxEnergy dead restingPace riddle entrance inFrontOfFirstTunnel inFrontOfSecondTunnel inFrontOfThirdTunnel hall firstTunnel dealerRoom alligatorRoom endOfFirstTunnel waterfall secondTunnel tunnelDiggers sideTunnel secondTunnel1 thirdTunnel synagogue items travellingCost holdingFlute|

    Game class >> new [
        ^ super new init
    ]

    init [
        entrance := Location new init: 'Entrance' description: [ :game |
            'You are in the entrance. You notice doors behind you, and you feel pain inside your head. There is a small note at the floor. Maybe you should inspect it?'
        ].
        inFrontOfFirstTunnel := Location new init: 'In front of first tunnel' description: [:game |
            'You are standing in front of the rightmost tunnel.'.
        ].
        inFrontOfSecondTunnel := Location new init: 'In front of second tunnel' description: [:game |
            'You are standing in front of the middle tunnel.'.
        ].
        inFrontOfThirdTunnel := Location new init: 'In front of third tunnel' description: [:game |
            'You are standing in front of the leftmost tunnel.'.
        ].
        hall := Location new init: 'Hall' description: [:game |
            'There is a hall with 3 tunnels. There must be a way to get out of there.'.
        ].
        firstTunnel := Location new init: 'First tunnel' description: [:game |
            'You are in the first tunnel. You can smell blood inside. It seems to be a bad way to escape.'.
        ].
        dealerRoom := Location new init: 'Dealer room' description: [:game |
            'You have entered a Jewish dealers space. He wants to sell you a magic flute, but he has not specified its purpose. Maybe it can be useful? He wants to help him, it will cost you 50 energy. (use buy(flute) to buy the flute)'.
        ].
        alligatorRoom := Location new init: 'Alligator room' description: [:game |
            'You have entered an Alligator space. There is a huge reptile at the back. Fight could be difficult and demanding. Would you try? (Type fight(alligator) to fight)'.
        ].
        endOfFirstTunnel := Location new init: 'End of first tunnel' description: [:game |
            'The sound of water crashing forcefully ahead fills the air. Advancing might prove to be unwise.'.
        ].
        waterfall := Location new init: 'Waterfall' description: [:game |
            'You have entered a waterfall. You can see a light at the end of the tunnel. You are carried away by the current of water.'.
        ].
        secondTunnel := Location new init: 'Second tunnel' description: [:game |
            'You are in the second tunnel. You can see a light at the end of a tunnel. Maybe this is a way to escape?'.
        ].
        tunnelDiggers := Location new init: 'Tunnel diggers' description: [:game |
            'You see a few people with mining tools. They tell you that they are trying to dig a tunnel to the surface but a herd of rats stands in their way. They point at a crudely built tunnel entrance to the left. Perhaps they will help you if you slay the rats?'.
        ].
        sideTunnel := Location new init: 'Side tunnel' description: [:game |
            'You are in a narrow, crudely built tunnel. Deep inside you can see the towering shadow of a giant rat. Greeting, traveler. What do you seek from the Great Rat King? You explain that the miners would like to pass. I shall grant your demand if you can answer my riddles. But if you fail, you shall perish at my hand. Do you take up the challenge? (Type yes or no)'.
        ].
        secondTunnel1 := Location new init: 'Second tunnel 1' description: [:game |
            'You reach the end of the tunnel. The light you saw comes from a tiny hole in the left wall of the tunnel.'.
        ].
        thirdTunnel := Location new init: 'Third tunnel' description: [:game |
            'Navigating the shadowy length of the third tunnel, you notice the air gradually fill with an unplaceable but comforting aroma, reminiscent of ancient texts and whispered secrets. The walls, subtly adorned with symbols that speak of community and stars aligning, lead you on with the promise of discovery. A faint, melodic hum, akin to distant singing, encourages each step forward, hinting at the existence of a sacred space ahead. As the passage unfolds into a serene chamber bathed in a soft, ethereal light, the ambiance suggests you are on the verge of entering a place of deep cultural significance, perhaps a sanctuary where history and spirituality converge in silent communion.'.
        ].
        synagogue := Location new init: 'Synagogue' description: [:game |
            'As you reach the end of the tunnel, the ambiance shifts dramatically. You find yourself in a spacious, time-honored synagogue. Sunlight streams through stained glass windows, casting a kaleidoscope of colors across the polished stone floor. The air is filled with a sense of peace and ancient wisdom. Rows of wooden pews lead your gaze to the ornate Ark at the far end, where the Torah scrolls are kept. You realize the journey through the tunnels was not just a test of survival, but a journey of discovery, leading you to this place of profound spiritual significance. Congratulations, you have found the hidden synagogue and completed your adventure!'.
        ].

        entrance addPath: 'forward' destination: hall.
        hall addPath: 'back' destination: entrance.
        hall addPath: 'right' destination: inFrontOfFirstTunnel.
        hall addPath: 'forward' destination: inFrontOfSecondTunnel.
        hall addPath: 'left' destination: inFrontOfThirdTunnel.
        
        inFrontOfFirstTunnel addPath: 'back' destination: hall.
        inFrontOfFirstTunnel addPath: 'forward' destination: firstTunnel.
        inFrontOfFirstTunnel addPath: 'left' destination: inFrontOfSecondTunnel.
        
        inFrontOfSecondTunnel addPath: 'back' destination: hall.
        inFrontOfSecondTunnel addPath: 'forward' destination: secondTunnel.
        inFrontOfSecondTunnel addPath: 'right' destination: inFrontOfFirstTunnel.
        inFrontOfSecondTunnel addPath: 'left' destination: inFrontOfThirdTunnel.
        
        inFrontOfThirdTunnel addPath: 'back' destination: hall.
        inFrontOfThirdTunnel addPath: 'forward' destination: thirdTunnel.
        inFrontOfThirdTunnel addPath: 'right' destination: inFrontOfSecondTunnel.
        
        firstTunnel addPath: 'back' destination: inFrontOfFirstTunnel.
        firstTunnel addPath: 'forward' destination: dealerRoom.
        
        dealerRoom addPath: 'back' destination: firstTunnel.
        dealerRoom addPath: 'forward' destination: alligatorRoom.
        
        alligatorRoom addPath: 'back' destination: dealerRoom.
        alligatorRoom addPath: 'forward' destination: endOfFirstTunnel.
        
        endOfFirstTunnel addPath: 'back' destination: alligatorRoom.
        endOfFirstTunnel addPath: 'forward' destination: waterfall.
        
        waterfall addPath: 'back' destination: endOfFirstTunnel.
        
        secondTunnel addPath: 'back' destination: inFrontOfSecondTunnel.
        secondTunnel addPath: 'forward' destination: tunnelDiggers.
        
        tunnelDiggers addPath: 'back' destination: secondTunnel.
        tunnelDiggers addPath: 'forward' destination: secondTunnel1.
        tunnelDiggers addPath: 'left' destination: sideTunnel.
        
        sideTunnel addPath: 'back' destination: tunnelDiggers.
        
        secondTunnel1 addPath: 'back' destination: tunnelDiggers.
        
        thirdTunnel addPath: 'back' destination: inFrontOfThirdTunnel.
        thirdTunnel addPath: 'forward' destination: synagogue.
        
        synagogue addPath: 'back' destination: thirdTunnel.
        entrance addItem: 'note' amount: 2.
        alligatorRoom addItem: 'Aligator' amount: 1.
        isGameOver := false.
        inventory := Dictionary new.
        currentLocation := entrance.
        energy := 100.
        maxEnergy := 100.
        dead := false.
        restingPace := 10.
        travellingCost:=10.
        riddle := Array new: 2.
        riddle at: 1 put: 0; at: 2 put: 0.
        items := Dictionary new.
        holdingFlute := false.
        items at: 'note' put: 'You read the note from a lost wanderer. It says: You are in a maze. You need to find a way out. There are 3 tunnels. The first one is very dangerous. The second one has a light at the end - thats the path you should take. The third one may hold a secret. Choose wisely.'.
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
            | energyCost |
            energyCost := travellingCost.  "Adjust this value as needed"
            energy > energyCost ifTrue: [
                energy := energy - energyCost.
                self printLines: {'Current energy: ', energy asString. ''}.
                currentLocation := destination.
                self printLines: #('Went to the new location.' '').
                self printLines: {currentLocation getDescription value: self. ''}.
            ] ifFalse: [
                Transcript show: 'You do not have enough energy to move.'; cr.
                self die.
            ].
        ] ifNil: [
            self printLines: #('You cannot go there!' '').
        ].
    ]

    take: aItemStr [
        | itemTaken itemAmount |
        itemTaken := currentLocation removeItem: aItemStr amount: 1.
        itemTaken ifTrue: [
            self printLines: #('Item taken.' '').
            itemAmount := inventory at: aItemStr ifAbsent: [^ inventory at: aItemStr put: 1.].
            inventory at: aItemStr put: itemAmount + 1.
        ] ifFalse: [
            self printLines: #('Item not present in location.' '').
        ]
    ]

    inspect: aItemStr [
        | itemAmount |
        itemAmount := inventory at: aItemStr ifAbsent: [^ self printLines: #('You are not holding this item.' '')].
        (itemAmount < 1) ifTrue: [
            ^ self printLines: #('You are not holding this item.' '')
        ].
        self printLines: { (items at: aItemStr). '' }.
    ]

    printInventory [
        self printLines: #('Inventory:').
        inventory keysAndValuesDo: [ :key :value |
            Transcript show: value printString, ' ', key.
        ].
        self printLines: #('').
    ]

    look [
        self printLines: {currentLocation getDescription value: self. ''}.
    ]
    die [
        dead := true.
        isGameOver := true.
        Transcript show: 'You died! Game Over.'; cr.
    ]
    fightAlligator [
        | E ELoss NewE |

        self getCurrentLocation = alligatorRoom ifTrue: [
            E := energy.
            ELoss := (10 + (1 to: 20) atRandom).  "Random energy loss between 40 and 50"
            NewE := E - ELoss.
            energy := NewE.
            self printLines: {'You have fought with an alligator. You have ', NewE asString, ' energy left.'}.
            (NewE <= 0) ifTrue: [
                self die.
            ] ifFalse: [
                self improveResting.
            ].
        ] ifFalse: [
            self printLines: #('You are not in the alligator room.' '').
        ].
    ]
    rest [
    | newEnergy |

    newEnergy := energy + restingPace.
    newEnergy <= maxEnergy ifTrue: [
        energy := newEnergy.
        Transcript
            show: 'You take a rest and feel better. Your energy is now ', newEnergy printString, '.';
            cr.
    ] ifFalse: [
        Transcript show: 'You are already fully rested.'; cr.
    ].
    ]
    improveResting [
    | RP NewRP |

    RP := restingPace.
    NewRP := RP + 10.
    restingPace := NewRP.
    self printLines: {'You have improved your resting pace. It is now ', NewRP asString, '.'}.
    ]
    buyFlute [
        | currentEnergy newEnergy newMaxEnergy |

        currentEnergy := energy.
        currentEnergy >= 50 ifFalse: [
            Transcript show: 'You don''t have enough energy.'; cr.
            ^ false
        ].
        
        newEnergy := currentEnergy - 50.
        energy := newEnergy.

        newMaxEnergy := maxEnergy - 30.
        maxEnergy := newMaxEnergy.
        inventory at: 'flute' put: 1.
        self printLines: #('Item taken.' '').
        holdingFlute := true.
        Transcript
            show: 'You have bought a magic flute. You have ', newEnergy printString, ' energy left.';
            cr.
        ^ true
    ]
    useFlute [
        (holdingFlute and: [ self getCurrentLocation = alligatorRoom ]) ifTrue: [
            Transcript
                show: 'You have used a magic flute. The alligator obeys you now. You can use him as a form of transport.';
                cr.
            travellingCost := 0.
            inventory at: 'Aligator' put: 1.
            ^ true
        ] ifFalse: [
            Transcript
                show: 'You cannot use the flute here.';
                cr.
            ^ false
        ].
    ]

    getEnergy [
    self printLines: {'Current energy: ', energy asString. ''}.
    ]

    ratKingAccept [

    ]

    ratKingReject [
        self printLines: #('Fare well, then.').
        self go: 'back'.
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

            cmdName = 'rest' ifTrue: [
                self rest.
                isUnknown := false.
            ].
            cmdName = 'getEnergy' ifTrue: [
                self getEnergy.
                isUnknown := false.
            ].
            cmdName = 'fightAligator' ifTrue: [
                self fightAlligator.
                isUnknown := false.
            ].
            cmdName = 'buyFlute' ifTrue: [
                self buyFlute.
                isUnknown := false.
            ].
            cmdName = 'useFlute' ifTrue: [
                self useFlute.
                isUnknown := false.
            ].
            cmdName = 'take' ifTrue: [
                self take: (cmdArr at: 2).
                isUnknown := false.
            ].

            cmdName = 'inspect' ifTrue: [
                self inspect: (cmdArr at: 2).
                isUnknown := false.
            ].

            cmdName = 'inventory' ifTrue: [
                self printInventory.
                isUnknown := false.
            ].

            cmdName = 'yes' ifTrue: [
                self ratKingAccept.
                isUnknown := false.
            ].

            (cmdName = 'no' and: [currentLocation == sideTunnel]) ifTrue: [
                self ratKingReject.
                isUnknown := false.
            ].

            isUnknown ifTrue: [
                self printLines: #('Unknown command.' '').
            ].
        ].
    ]
]

Game new run.
