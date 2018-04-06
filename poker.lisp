(ql:quickload :alexandria)

(defpackage :org.xkqr.dev.cl.poker
  (:use :common-lisp :alexandria))
(in-package :org.xkqr.dev.cl.poker)

;; TODO: create a deforder macro which defines an ordering for given symbols

;; * Card
(defparameter *suits*
  '(hearts clubs spades diamonds))

(defparameter *ranks*
  '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun card-suit (card)
  "Return the suit from a CARD pair (suit . rank)."
  (car card))

(defun card-rank (card)
  "Return the rank from a CARD pair (suit . rank)."
  (cdr card))

(defun rank< (left right)
  (< (position left *ranks*)
     (position right *ranks*)))

;; * Deck
(defparameter *cards*
  (map-product #'cons *suits* *ranks*)
  "A list of all cards, each card being a cons pair.")

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
   (score
    :reader player-score
    :initform 0)))

(defmethod print-object ((player player) stream)
  (print-unreadable-object (player stream :type t)
    (format stream "(~d) ~a"
            (player-score player)
            (player-hand player))))

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
  (rank< (card-rank card) 7))

;; ** Human
(defclass human (player)
  nil)

;; * Utils
(defun counts (seq &key (key #'identity))
  "Each item in SEQ (under KEY) associated with its
count, returned as a hash table."
  (loop with counts = (make-hash-table)
     for elt in seq
     do (incf (gethash (funcall key elt) counts 0))
     finally (return counts)))

(defun hash-table-invert (hash-table)
  "Invert HASH-TABLE, going from a key--value mapping to
a mapping of key--list-of-values."
  (loop with inverted = (make-hash-table)
     for key being the hash-keys of hash-table
     using (hash-value value)
     do (push key (gethash value inverted '()))
     finally (return inverted)))

;; * Scoring
(defun hand-counts (hand key)
  "Return how many there are in HAND of each KEY."
  (hash-table-invert (counts hand :key key)))

(defparameter *hands*
  '(high-card
    one-pair
    two-pairs
    three-of-a-kind
    straight
    flush
    full-house
    four-of-a-kind
    straight-flush))

(defun score (hand)
  "Return the best way to view HAND as a standard poker hand."
  (let* ((rank-counts (hand-counts hand #'card-rank))
         (suit-counts (hand-counts hand #'card-suit))
         (ranks (sort (mapcar #'card-rank hand) #'rank<))
         (straight (cond ((equal ranks '(2 3 4 5 ace))
                          '(ace 2 3 4 5))
                         ((search ranks *ranks*)
                          ranks)))
         (flush (gethash 5 suit-counts))
         (quadruple (gethash 4 rank-counts))
         (triple (gethash 3 rank-counts))
         (pair (sort (gethash 2 rank-counts) #'rank<))
         (high-card (car (last ranks))))
    (cond
      ((and straight flush) (list 'straight-flush high-card)) 
      (quadruple (list 'four-of-a-kind (car quadruple))) 
      ((and triple pair) (list 'full-house (car triple) (car pair))) 
      (flush (list 'flush high-card)) 
      (straight (list 'straight high-card)) 
      (triple (list 'three-of-a-kind (car triple))) 
      ((= 2 (length pair)) (list 'two-pairs (cadr pair) (car pair))) 
      (pair (list 'one-pair (car pair)))
      (t (list 'high-card high-card)))))

(defun list< (left right &key (key #'identity))
  "Compare two lists LEFT and RIGHT lexicographically under KEY."
  (and left right
       (let ((left-val (key (car left)))
             (right-val (key (car right))))
         (if (= left-val right-val)
             (list< (cdr left) (cdr right) :key key)
             (< left-val right-val)))))

(defun hand< (left right)
  "Compare two hands LEFT and RIGHT with poker ranking rules."
  (let* ((left-score (score left))
         (right-score (score right)) 
         (left-hand (position (car left-score) *hands*))
         (right-hand (position (car right-score) *hands*)))
    (if (= left-hand right-hand)
        (rank-list< (cdr left-score) (cdr right-score)) 
        (< left-hand right-hand))))

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
                 i (score (player-hand player))))

    (loop for player in players
       for i upfrom 0 do
         (replace-cards player deck)
         (format t "~%Player ~d replaced to: ~a"
                 i (score (player-hand player))))
    
    (loop for player in players
       for best = player then
         (if (hand< (player-hand best)
                    (player-hand player))
             player
             best)
       finally
         (incf (slot-value best 'score)))

    (mapc #'drop-cards players))  
  
  players)


