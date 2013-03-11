function Solitaire() {
    var board = new Board({
        rootId : "Klondike",
        magicalX : 106
    });

    var totalDeck = new Deck(board.collapsedType, 0, 0, {
        filter : function () { return false; },
        draggable : false
    });
    totalDeck.initialize(true);
    
    var discard = new Deck(board.collapsedType, 1, 0, { 
		filter : function(card) { return false; }
	});
    board.addDeck(discard);
    
    totalDeck.shuffle();
    totalDeck.setAction(function () {
        var top = totalDeck.peek();

        if (!top) {
            discard.deal(totalDeck, discard.getCards().length, true);
        } else {
            top.setFaceUp(true);
            totalDeck.remove(top);
            discard.addTop(top);
        }
    });
    board.addDeck(totalDeck);

    var pile;
    var endPiles = [];
    
    for (var i = 1; i <= 7; i++) {
        pile = new Deck(board.defaultType, i - 1, 1);
        totalDeck.deal(pile, i, true);

        var topCard = pile.peek();
        if (topCard) {
            topCard.setFaceUp(true);
        }

        pile.setFilter((function (pile) {
             return function (card) {
                 var top = pile.peek();
				 
                if (top) {
                    return (top.getColor() != card.getColor()) &&
                        top.getRank() - card.getRank() ==1;
                } else {
                    return card.getRank() == 13;
                }
            };
        })(pile));

        pile.observe((function (pile) {
            return function (event) {
                if (event.type == "remove") {
                    if (pile.peek()) {
                        pile.peek().setFaceUp(true);
                    }
                }
            };
        })(pile));
        
        board.addDeck(pile);
    }
    
    for (i = 0; i < 4; i++) {
        endPiles[i] = new Deck(board.collapsedType, 3 + i, 0);
        board.addDeck(endPiles[i]);
        endPiles[i].setFilter((function (pile) {
            return function (card, deck, size) {
                if (size > 1) {
                    return false;
                }
                
                var top = pile.peek();

                if (top) {
                    return (top.getSuit() == card.getSuit()) &&
                        (card.getRank() - top.getRank() == 1);
                } else {
                    return card.getRank() == 1;
                }
            };
        })(endPiles[i]));
    }
}
$(document).ready(function() { new Solitaire(); });