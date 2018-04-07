(ql:quickload :alexandria)
(ql:quickload :fset)

(defpackage :org.xkqr.dev.cl.poker
  (:use :common-lisp :alexandria :fset-user))
(in-package :org.xkqr.dev.cl.poker)

;; * Define custom, generic orderings
(defgeneric ord< (a b)
  (:documentation "Generically check if a is less than b."))

(defmethod ord< ((a null) (b null))
  nil)

(defmethod ord< ((a cons) (b cons))
  "Cons pairs are compared lexicographically."
  (or (ord< (car a) (car b))
      (and (not (ord< (car b) (car a)))
           (ord< (cdr a) (cdr b)))))

(defmacro deforder (name symbols)
  "An order has a NAME and is a set of SYMBOLS associated with a particular
order. This macro generates convenience functions to deal with it when sorting,
etc."
  `(progn
     (defclass ,name ()
       ((name
         :reader name
         :initarg :value)))

     (defmethod print-object ((value ,name) stream)
       (print-unreadable-object (value stream :type t)
         (with-slots (name) value
           (format stream "~s" name))))
     
     (defun ,name (value)
       (make-instance (quote ,name) :value value))

     (defparameter ,(intern (concatenate 'string "*" (symbol-name name) "S*"))
       (mapcar #',name ,(car body)))     
     
     (defmethod ord< ((a ,name) (b ,name))
       (< (position (name a) ,(order-symbol name) :key #'name)
          (position (name b) ,(order-symbol name) :key #'name)))))

;; * Deck of cards
;; TODO test ordering of orders

(deforder rank
  '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(deforder suit
  '(clubs diamonds hearts spades))

(deforder card
  (map-product #'cons *ranks* *suits*))

(defun card-rank (card)
  "Return the rank from a CARD."
  (car (name card)))

(defun card-suit (card)
  "Return the suit from a CARD."
  (cdr (name card)))

(defclass deck ()
  ((cards
    :initform (shuffle *cards*))))

(defun draw-cards (deck &optional (count 1))
  "Draw cards from DECK, optionally with COUNT specified."
  (loop repeat count collect
       (pop (slot-value deck 'cards))))

;; * Player
(defclass player ()
  ((hand
    :reader player-hand
    :initform ())
   (wins
    :reader player-wins
    :initform 0)))

(defmethod print-object ((player player) stream)
  (print-unreadable-object (player stream :type t)
    (with-slots (hand wins) player
      (format stream "(~d) ~a" wins hand))))

(defun drop-cards (player &key (keep nil))
  "Drop all cards from PLAYER except those in KEEP."
  (setf (slot-value player 'hand) keep))

(defmethod reject-card ((player player) card)
  "Default action for PLAYER is to reject no CARD."
  nil)

;; ** Newbie
(defclass newbie (player)
  nil)

(defmethod reject-card ((player newbie) card)
  "Stupid AI rejects any cards with value less than 7."
  (ord< (card-rank card) (rank 7)))

;; ** Human
(defclass human (player)
  nil)

;; * Scoring
(defun hand-counts (hand key)
  "Return how many there are in HAND of each KEY."
  (let ((counts (fset:convert 'fset:bag (mapcar key hand)))
        (inverted (fset:empty-map '())))
    (fset:do-bag-pairs (item mult counts)
      (push item (fset:lookup inverted mult)))
    inverted))

(deforder hand-type
  '(high-card one-pair two-pairs three-of-a-kind
    straight flush full-house four-of-a-kind
    straight-flush))

(defparameter ace-low-straight
  (mapcar #'rank '(ace 2 3 4 5))
  "Represents the only case in which ace is the lowest card.")

(defun player-score (player)
  "Return the best way to view HAND as a standard poker hand."
  (let* ((hand (player-hand player))
         (rank-counts (hand-counts hand #'card-rank))
         (suit-counts (hand-counts hand #'card-suit))
         (ranks (sort (mapcar #'card-rank hand) #'ord<))
         (straight (cond ((equal ranks ace-low-straight)
                          ace-low-straight)
                         ((search ranks *ranks*)
                          ranks)))
         (flush (fset:lookup suit-counts 5))
         (quadruple (fset:lookup rank-counts 4))
         (triple (fset:lookup rank-counts 3))
         (pair (sort (fset:lookup rank-counts 2) #'ord<))
         (high-card (car (last ranks))))
    (cond
      ((and straight flush)
       (list (hand-type 'straight-flush) high-card)) 
      (quadruple
       (list (hand-type 'four-of-a-kind) (car quadruple))) 
      ((and triple pair)
       (list (hand-type 'full-house) (car triple) (car pair))) 
      (flush
       (list (hand-type 'flush) high-card)) 
      (straight
       (list (hand-type 'straight) high-card)) 
      (triple
       (list (hand-type 'three-of-a-kind) (car triple))) 
      ((= 2 (length pair))
       (list (hand-type 'two-pairs) (cadr pair) (car pair))) 
      (pair
       (list (hand-type 'one-pair) (car pair)))
      (t
       (list (hand-type 'high-card) high-card)))))

;; * Game
(defun deal-cards (player deck)
  "Deal cards until PLAYER hand is full, drawing from DECK."
  (let ((count (- 5 (length (player-hand player)))))
    (appendf (slot-value player 'hand)
             (draw-cards deck count))))

(defun replace-cards (player deck)
  "Replace cards rejected by PLAYER with new cards from DECK."
  (drop-cards player :keep (remove-if
                            (curry #'reject-card player)
                            (player-hand player)))
  (deal-cards player deck))

(defun game-round (players)
  (let ((deck (make-instance 'deck)))

    (format t "~%-----")
    (loop for player in players
       for i upfrom 0 do
         (deal-cards player deck)
         (format t "~%Dealt player ~d: ~a"
                 i (player-score player)))

    (loop for player in players
       for i upfrom 0 do
         (replace-cards player deck)
         (format t "~%Player ~d replaced to: ~a"
                 i (player-score player)))
    
    (loop for player in players
       for best = player then
         (if (ord< (player-score best)
                   (player-score player))
             player
             best)
       finally
         (incf (slot-value best 'wins)))

    (mapc #'drop-cards players))  
  
  players)

