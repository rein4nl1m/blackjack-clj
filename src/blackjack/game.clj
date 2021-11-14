(ns blackjack.game
  (:require [card-ascii-art.core :as card]))

; Pontuação Máxima
(def max-point 21)

; A, 2, 3, 4, 5, 6, 7, 8, 9, 10, J, Q, K
; 1...13
(defn new-card
  "Generates a card number between 1 and 13"
  []
  (inc (rand-int 13)))

; Calcular pontos de acordo com as cartas
; J, Q, K = 10 (não 11, 12, 13)
; [A 10] = 11 ou 21 => 21
; [A 5 7] = 1+5+7 (13) ou 11+5+7 (23) => 13
; A = 11, porém se passar de 21, vale 1

(defn JQK->10
  [card]
  (if (> card 10) 10 card))

(defn A->11 [card]
  (if (= card 1) 11 card))

(defn points-cards
  [cards]
  (let [cards-without-JQK (map JQK->10 cards)
        cards-with-A11 (map A->11 cards-without-JQK)
        points-with-A1 (reduce + cards-without-JQK)
        points-with-A11 (reduce + cards-with-A11)]
    (if (> points-with-A11 max-point) points-with-A1 points-with-A11)))

; Representação do Jogador
;{:player-name player-name
; :cards [1 3]
; :points 4}
(defn player
  [player-name]
  (let [card-1 (new-card)
        card-2  (new-card)
        cards [card-1 card-2]
        points (points-cards cards)]
    {:player-name player-name
     :cards cards
     :points points}))

; gerar um novo card
; atulizar vetor de cards dentro do player
; calcular pontos com novo card
; retornar player atualizado
(defn more-card
  [player]
  (let [card (new-card)
        new-player (update player :cards conj card)
        points (points-cards (:cards new-player))]
    (assoc new-player :points points)))

; decisao do player se quer mais cartas
(defn player-decision-continue?
  [player]
  (println (:player-name player) ": mais cartas? (y/n)")
  (= (read-line) "y"))

; decisao do dealer se quer mais cartas
(defn dealer-decision-continue?
  [player-points dealer]
  (let [dealer-points (:points dealer)]
    (if (> player-points max-point)
      false
      (>= player-points dealer-points))))

; Reponsavel por verificar se jogador quer continuar pedindo mais cartas
(defn continue-game
  [player fn-decision-continue?]
  (if (fn-decision-continue? player)
    (let [player-with-more-cards (more-card player)]
      (card/print-player player-with-more-cards)
      (recur player-with-more-cards fn-decision-continue?))
    player))

; Ambos > 21 = perderam
; Pontos iguas = empate
; player-points > 21 = dealer ganhou
; dealer-points > 21 = player ganhou
; player-points > dealer-points = player ganhou
; dealer-points > player-points = dealer ganhou
(defn end-game
  [player dealer]
  (let [player-name (:player-name player)
        player-points (:points player)
        dealer-name (:player-name dealer)
        dealer-points (:points dealer)
        message (cond
                  (and (> player-points max-point) (> dealer-points max-point)) "Ambos perderam"
                  (= player-points dealer-points) "Empate"
                  (> player-points max-point) (str dealer-name " ganhou")
                  (> dealer-points max-point) (str player-name " ganhou")
                  (> player-points dealer-points) (str player-name " ganhou")
                  (> dealer-points player-points) (str dealer-name " ganhou"))]
    (print (str (char 27) "[2J"))
    (card/print-player player)
    (card/print-player dealer)
    (println "")
    (println message)))

(println "Informe seu nome de jogador:")
(print (str (char 27) "[2J"))
(def player-1 (player (read-line)))
(card/print-player player-1)

(def dealer (player "Dealer"))
(card/print-masked-player dealer)

(def player-after-game (continue-game player-1 player-decision-continue?))
(def partial-dealer-decision-continue? (partial dealer-decision-continue? (:points player-after-game)))
(def dealer-after-game (continue-game dealer partial-dealer-decision-continue?))

(end-game player-after-game dealer-after-game)