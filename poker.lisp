;; * Commentary
;; - TODO: Property testing with check-it
;;         (https://github.com/DalekBaldwin/check-it)


;; * Code
(defpackage :org.xkqr.dev.cl.poker
  (:shadowing-import-from :fset
                          #:set #:empty-set
                          #:bag #:do-bag-pairs
                          #:map #:empty-map
                          #:appendf #:includef
                          #:lookup #:first #:last #:empty?
                          #:compare #:compare-slots #:equal?
                          #:less-than? #:greater-than?
                          #:sort #:greatest #:least)
  (:shadowing-import-from :alexandria #:curry #:map-product)
  (:use :cl))

(in-package :org.xkqr.dev.cl.poker)

;; * Macro to define orders of symbols
(defmacro deforder (name symbols) 
  (let ((order-symbol
         (intern (concatenate 'string
                              "*" (symbol-name name) "S*"))))
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

       (defparameter ,order-symbol
         (mapcar #',name ,symbols))     

       (defmethod compare ((a ,name) (b ,name))
         (compare-slots
          a b (lambda (o)
                (position (name o) ,order-symbol
                          :key #'name)))))))

;; * Deck of cards
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
    :initform (alexandria:shuffle *cards*))))

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
  (less-than? (card-rank card) (rank 7)))

;; ** Human
(defclass human (player)
  nil)

;; * Scoring
(defun multiplicity-map (bag)
  "Invert BAG into a map from multiplicity to set-of-items."
  (let ((inverted (empty-map (empty-set))))
    (do-bag-pairs (item mult bag inverted)
      (fset:includef (lookup inverted mult) item))))

(deforder hand-type
    '(high-card one-pair two-pairs three-of-a-kind
      straight flush full-house four-of-a-kind
      straight-flush))

(defparameter ace-low-straight
  (mapcar #'rank '(ace 2 3 4 5))
  "Represents the only case in which ace is the lowest card.")

(defun player-score (player)
  "Return the best way to view HAND as a standard poker hand."
  (let* ((hand (sort (player-hand player) #'less-than?)) 
         (ranks (fset:convert 'bag (mapcar #'card-rank hand)))
         (suits (fset:convert 'bag (mapcar #'card-suit hand)))
         (rank-counts (multiplicity-map ranks)) 
         (pairs (lookup rank-counts 2))
         (triples (lookup rank-counts 3))
         (quadruples (lookup rank-counts 4))
         
         (three-of-a-kind
          (and (not (empty? triples))
               (list (hand-type 'three-of-a-kind)
                     (greatest triples))))
         (four-of-a-kind
          (and (not (empty? quadruples))
               (list (hand-type 'four-of-a-kind)
                     (greatest quadruples))))
         (two-pairs
          (and (= 2 (fset:set-size pairs))
               (list (hand-type 'two-pairs)
                     (greatest pairs)
                     (least pairs)))) 
         (flush
          (and (= 1 (fset:set-size suits))
               (list (hand-type 'flush)
                     (greatest ranks))))
         (straight
          (and (or (equal? ranks ace-low-straight)
                   (search (fset:convert 'list ranks) *ranks*))
               (list (hand-type 'straight)
                     (if (equal? ranks ace-low-straight)
                         (rank 5)
                         (greatest ranks))))) 
         (full-house
          (and (not (empty? triples)) (not (empty? pairs))
               (list (hand-type 'full-house)
                     (greatest triples)
                     (greatest pairs)))) 
         (straight-flush
          (and straight flush
               (list (hand-type 'straight-flush)
                     (greatest ranks)))))

    (greatest
     (set
      (list (hand-type 'high-card) (greatest ranks))
      (and (not (empty? pairs))
           (list (hand-type 'pair) (greatest pairs)))
      two-pairs
      three-of-a-kind
      four-of-a-kind
      straight
      flush
      full-house
      straight-flush))))

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
         (if (less-than? (player-score best) (player-score player))
             player best)
       finally
         (incf (slot-value best 'wins)))

    (mapc #'drop-cards players))  
  players)

(defun play-game (players rounds)
  (loop repeat rounds do (game-round players))
  players)

(check-it:def-generator new-deck ()
  (make-instance 'deck))

(prove:subtest "Testing PLAYER vs NEWBIE"
  (let ((*num-trials* 10))
    (prove:plan 1)
    (prove:ok (check-it:check-it
               (check-it:generator (new-deck))
               (lambda (deck)
                 (let ((result
                        (play-game (list (make-instance 'newbie)
                                         (make-instance 'player))
                                   100)))
                   (< (car result) (- (cadr result) 10))))))
    (prove:finalize)))
