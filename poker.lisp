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

(defclass card ()
  ((suit
    :reader card-suit
    :initarg :suit)
   (rank
    :reader card-rank
    :initarg :rank)))

(defmethod print-object ((card card) stream)
  (print-unreadable-object (card stream :type t)
    (with-slots (suit rank) card
      (format stream "~s ~s" suit rank))))

(defun rank< (left right)
  (< (position left *ranks*)
     (position right *ranks*)))

;; * Deck
(defparameter *cards*
  (map-product
   (lambda (suit rank)
     (make-instance 'card :suit suit :rank rank))
   *suits* *ranks*))

(defclass deck ()
  ((cards
    :initform (shuffle *cards*))))

(defun draw-cards (deck &optional (n 1))
  (loop repeat n collect
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
  (setf (slot-value player 'hand) keep))

(defmethod reject-card ((player player) card)
  "Reject no card."
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
  (loop with counts = (make-hash-table)
     for elt in seq
     do (incf (gethash (funcall key elt) counts 0))
     finally (return counts)))

(defun invert (hash-table)
  (loop with inverted = (make-hash-table)
     for key being the hash-keys of hash-table
     using (hash-value value)
     do (push key (gethash value inverted '()))
     finally (return inverted)))

;; * Scoring
(defun hand-counts (hand key)
  (invert (counts hand :key key)))

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
         (pair (gethash 2 rank-counts)))
    (cond
      ((and straight flush) (list 'straight-flush (car (last ranks)))) 
      (quadruple (list 'four-of-a-kind (car quadruple))) 
      ((and triple pair) (list 'full-house (car triple) (car pair))) 
      (flush (list 'flush (car (last ranks)))) 
      (straight (list 'straight (car (last straight)))) 
      (triple (list 'three-of-a-kind (car triple))) 
      ((= 2 (length pair))
       (let ((sorted (sort pair #'rank<)))
         (list 'two-pairs (cadr sorted) (car sorted)))) 
      (pair (list 'one-pair (car pair)))
      (t (list 'high-card (car (last ranks)))))))

(defun rank-list< (left right)
  (and left
       right
       (if (= (position (car left) *ranks*)
              (position (car right) *ranks*))
           (rank-list< (cdr left)
                       (cdr right))
           (< (position (car left) *ranks*)
              (position (car right) *ranks*)))))

(defun hand< (left right)
  (let* ((left-score (score left))
         (right-score (score right)) 
         (left-hand (position (car left-score) *hands*))
         (right-hand (position (car right-score) *hands*)))
    (if (= left-hand right-hand)
        (rank-list< (cdr left-score) (cdr right-score)) 
        (< left-hand right-hand))))

;; * Game
(defun deal-cards (player deck)
  "Draws cards from the deck until hand is full."
  (let ((count (- 5 (length (player-hand player)))))
    (appendf (slot-value player 'hand)
             (draw-cards deck count))))

(defun replace-cards (player deck)
  "Replace cards rejected by player."
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


