REBOL []

fn: context [
    map: funct [ fn a-list ] [
        res: make block! 5
        foreach elem a-list [ append/only res do [fn elem] ]
        res
    ]
    range: funct [ start end ] [
        res: make block! 10
        step: either start < end [ 1 ] [ -1 ]
        for i start end step [ append res i ]
        res
    ]
    frequencies: funct [ a-list ] [
        res: make map! 5
        foreach elem a-list [
            either res/(elem)
            [ poke res elem res/(elem) + 1 ]
            [ append res reduce [ elem 1 ]]
        ]
        res
    ]
    val-in?: funct [ val map ] [
        foreach k map [
            if map/(k) = val [ return true ]
        ]
        return false
    ]
]

hands: make object! [
    straight: [ ♥/1  ♣/2  ♣/3  ♦/4  ♠/5 ]
    straight-flush: [ ♥/1  ♥/2  ♥/3  ♥/4  ♥/5 ]
    pair: [ ♥/2  ♣/2  ♣/3  ♦/4  ♠/5 ]
    two-pair: [ ♥/2  ♣/2  ♣/3  ♦/3  ♠/5 ]
]

read-hand: func [ hand-string ] [
    suits-table: [ #"H" ♥  #"C" ♣  #"D" ♦  #"S" ♠ ]
    ranks-table: "--23456789TJQKA"
    fn/map func [ c ] [
        to-path reduce [ 
            select suits-table c/2 
            offset? ranks-table find c/1 ranks-table ]
    ] parse hand-string " "
]

ranks: func [ hand ] [ fn/map func [ c ] [ probe second c] hand ]
suits: func [ hand ] [ fn/map func [ c ] [ probe first c ] hand ]

count-ranks: func [ hand ] [ fn/frequencies ranks hand ]
count-suits: func [ hand ] [ fn/frequencies suits hand ]


has-flush: func [ hand ] [
    1 = length? group-by-suit hand
]

has-straight: func [ hand ] [
    rs: sort ranks hand
    rs = fn/range rs/1 (rs/1 + (length? rs) - 1)
]

has-straight-flush: func [ hand ] [
    all [ has-straight hand has-flush hand ]
]

has-group-of: func [ size hand ] [
    fs: count-ranks hand
    fn/val-in? size fs
]

has-pair: func [ hand ] [ has-group-of 2 hand ]
has-three: func [ hand ] [ has-group-of 3 hand ]
has-four: func [ hand ] [ has-group-of 4 hand ]
has-two-pair: func [ hand ] [
    fs: fn/frequencies values-of count-ranks hand
    2 = fs/2
]