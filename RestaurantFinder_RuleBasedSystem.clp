;;-------------------------------------------------------------
;; Templates
;;-------------------------------------------------------------

(deftemplate user
"User info"
  (slot name (default "user"))
  (slot user-id (default 0)))
  
(deftemplate restaurant
"Restaurant info"
  (slot restaurant-name)
  (slot cuisine)
  (slot meal-type)
  (slot rating)
  (slot price-per-meal)
)

(deftemplate question
"Type of questions to be asked"
    (slot id)
    (slot text)
    (slot type))

(deftemplate answer
    (slot id)
    (slot text))


;;-------------------------------------------------------------
;;Questions
;;-------------------------------------------------------------
  
(deffacts question-data
  "Questions the system can ask."
  (question (id existing-user) (type yes-no)
            (text "Are you a registered user?"))
  (question (id fetch-restraunts) (type yes-no)
            (text "Are you looking for details about your previous dining places?"))
  (question (id search-restraunt) (type yes-no)
            (text "Would you like us to help you find the perfect dining place for you?"))
  (question (id cusine-type) (type text) ;; accepted values - asian, mexican, continental
            (text "What cusine would you prefer?"))
  (question (id meal-type-italian) (type text)
            (text "What type of meal would you prefer?"))
  (question (id restaurant-rating-italian) (type text)
            (text "What rating of the restaurant would you prefer?"))
  (question (id meal-price-italian) (type text)
            (text "Your preferred price range for the meal"))
  (question (id meal-type-indian) (type text)
            (text "What type of meal would you prefer?"))
  (question (id restaurant-rating-indian) (type text)
            (text "What rating of the restaurant would you prefer?"))
  (question (id meal-price-indian) (type text)
            (text "Your preferred price range for the meal"))
  (question (id meal-price-indian) (type text)
            (text "Your preferred price range for the meal"))
  (question (id meal-type-chinese) (type text)
            (text "What type of meal would you prefer?"))
  (question (id restaurant-rating-chinese) (type text)
            (text "What rating of the restaurant would you prefer?"))
  (question (id meal-price-chinese) (type text)
            (text "Your preferred price range for the meal"))
  (question (id order-details) (type yes-no)
            (text "Should be proceed with the suggested Restaurant for the booking?"))
  (question (id confirm-booking) (type yes-no)
            (text "Do you confirm booking at this restaurant?"))
  (question (id search-again) (type yes-no)
            (text "Would you like to search more?"))
  (question (id try-again) (type yes-no)
            (text "Would you like to try again?"))
  (question (id concerns) (type yes-no)
            (text "Do you have any other issues?"))
 )
            
  (defglobal ?*crlf* = "
")

;;-------------------------------------------------------------
;; Module ask
;;-------------------------------------------------------------


(defmodule ask)

(deffunction ask-user (?question ?type)
  "Ask a question and return the answer"
  (bind ?answer "")
  (while (not (is-of-type ?answer ?type)) do
         (printout t ?question " ")
         (if (eq ?type yes-no) then
           (printout t "(yes or no) "))
         (bind ?answer (read)))
  (return ?answer))

(deffunction is-of-type (?answer ?type)
  "Check if the answer is of the correct type"
  (if (eq ?type yes-no) then
    (return (or (eq ?answer yes) (eq ?answer no)))
    (elif (eq ?type number) then
           (return (numberp ?answer)))
    else (return (> (str-length ?answer) 0))))
   
(defrule ask::ask-question-by-id
  "Given the identifier of a question, ask the question and assert the answer"
  (declare (auto-focus TRUE))
  (MAIN::question (id ?id) (text ?text) (type ?type))
  (not (MAIN::answer (id ?id)))
  ?ask <- (MAIN::ask ?id)
  =>
  (bind ?answer (ask-user ?text ?type))
  (assert (answer (id ?id) (text ?answer)))
  (retract ?ask)
  (return))  


;;-------------------------------------------------------------
;; Module initialize
;;-------------------------------------------------------------

(defmodule initialize)
(defglobal ?*g-ordernum* = 0)
(defglobal ?*item* = 0)
(defglobal ?*uname* = 0)
(defglobal ?*cusine* = 0)
(defglobal ?*mealType* = 0)
(defglobal ?*rating* = 0)
(defglobal ?*price* = 0)
(defrule print-banner
  =>
  (printout t " " crlf)
  (printout t " " crlf)
  (printout t "Welcome to SpotMeal. I am your virtual assistant.!" crlf)
  (printout t "I will be assisting you in finding the perfect restaurant for you!!" crlf)
  (printout t "Please type your name and press Enter> " )
  (bind ?*uname* (read))
  (printout t crlf "***************************************************" crlf)
  (printout t " Hello, " ?*uname* "." crlf)  
  (printout t " Please answer a few questions and" crlf)
  (printout t " I can assist you with your restaurant search." crlf)
  (printout t "***************************************************" crlf crlf)

  (assert (restaurant (restaurant-name Tuscany) (cuisine italian) (meal-type dinner) (rating 5) (price-per-meal 15)))
  (assert (restaurant (restaurant-name GareebNawaz) (cuisine indian) (meal-type breakfast) (rating 3) (price-per-meal 10)))
  (assert (restaurant (restaurant-name CurryOnFire) (cuisine indian) (meal-type lunch) (rating 4) (price-per-meal 25)))
  (assert (restaurant (restaurant-name ChineseHut) (cuisine chinese) (meal-type dinner) (rating 2) (price-per-meal 10)))
)

;;-------------------------------------------------------------
;; Module rules
;;-------------------------------------------------------------

(defmodule rules)

(defrule inquire-user
  =>
  (printout t "Available Cuisines: " crlf)
  (printout t "indian" crlf)
  (printout t "italian" crlf)
  (printout t "chinese" crlf)
  (assert (ask cusine-type)))
;;==========================
;; Italian cuisine check
;;==========================

(defrule check-cusine-type
  (answer (id cusine-type) (text ?st&:(if (= ?st italian) then (bind ?*cusine* ?st))))
  =>
  (printout t "Available Meal Options: " crlf)
  (printout t "breakfast" crlf)
  (printout t "lunch" crlf)
  (printout t "dinner" crlf)
  (assert (MAIN::ask meal-type-italian)))
  
(defrule check-meal-italian
  (answer (id meal-type-italian) (text ?st&:(if (= ?st dinner) then (bind ?*mealType* ?st))))
  =>
  (printout t "Possible ratings: " crlf)
  (printout t "5" crlf)
  (printout t "4" crlf)
  (printout t "3" crlf)
  (printout t "2" crlf)
  (assert (MAIN::ask restaurant-rating-italian)))
  
(defrule check-rating-italian
  (answer (id restaurant-rating-italian) (text ?st&:(if (= ?st 5) then (bind ?*rating* ?st))))
  =>
  (printout t "Price per meal: " crlf)
  (printout t "50" crlf)
  (printout t "25" crlf)
  (printout t "15" crlf)
  (printout t "10" crlf) 
  (assert (MAIN::ask meal-price-italian)))

(defrule check-price-italian
  (answer (id meal-price-italian) (text ?st&:(if (= ?st 15) then (bind ?*price* ?st))))
  =>
  (printout t "Cusine: " ?*cusine* "" crlf)
  (printout t "Meal Time: " ?*mealType* crlf)
  (printout t "Rating: " ?*rating* crlf)
  (printout t "Price Range: " ?*price* crlf)
  (printout t " Suggested Restaurant : Tuscany" crlf)
  (assert (MAIN::ask order-details)))

;;==========================
;; Indian cuisine check
;;==========================

(defrule check-cusine-type2
  (answer (id cusine-type) (text ?st&:(if (= ?st indian) then (bind ?*cusine* ?st))))
  =>
  (printout t "Available Meal Options: " crlf)
  (printout t "breakfast" crlf)
  (printout t "lunch" crlf)
  (printout t "dinner" crlf)
  (assert (MAIN::ask meal-type-indian)))
  
(defrule check-meal-indian
  (answer (id meal-type-indian) (text ?st&:(if (= ?st dinner) then (bind ?*mealType* ?st))))
  =>
  (printout t "Possible ratings: " crlf)
  (printout t "5" crlf)
  (printout t "4" crlf)
  (printout t "3" crlf)
  (printout t "2" crlf)
  (assert (MAIN::ask restaurant-rating-indian)))

(defrule check-meal-indian2
  (answer (id meal-type-indian) (text ?st&:(if (= ?st breakfast) then (bind ?*mealType* ?st))))
  =>
  (printout t "Possible ratings: " crlf)
  (printout t "5" crlf)
  (printout t "4" crlf)
  (printout t "3" crlf)
  (printout t "2" crlf)
  (assert (MAIN::ask restaurant-rating-indian)))
  
(defrule check-rating-indian
  (answer (id restaurant-rating-indian) (text ?st&:(if (= ?st 4) then (bind ?*rating* ?st))))
  =>
  (printout t "Price per meal: " crlf)
  (printout t "50" crlf)
  (printout t "25" crlf)
  (printout t "15" crlf)
  (printout t "10" crlf)
  (assert (MAIN::ask meal-price-indian)))

(defrule check-rating-indian2
  (answer (id restaurant-rating-indian) (text ?st&:(if (= ?st 3) then (bind ?*rating* ?st))))
  =>
  (printout t "Price per meal: " crlf)
  (printout t "50" crlf)
  (printout t "25" crlf)
  (printout t "15" crlf)
  (printout t "10" crlf)
  (assert (MAIN::ask meal-price-indian)))

(defrule check-price-indian
  (answer (id meal-price-indian) (text ?st&:(if (= ?st 25) then (bind ?*price* ?st))))
  =>
  (printout t "Cusine: " ?*cusine* "" crlf)
  (printout t "Meal Time: " ?*mealType* crlf)
  (printout t "Rating: " ?*rating* crlf)
  (printout t "Price Range: " ?*price* crlf)
  (printout t " Suggested Restaurant : Curry On Fire" crlf)
  (assert (MAIN::ask order-details)))

(defrule check-price-indian2
  (answer (id meal-price-indian) (text ?st&:(if (= ?st 10) then (bind ?*price* ?st))))
  =>
  (printout t "Cusine: " ?*cusine* "" crlf)
  (printout t "Meal Time: " ?*mealType* crlf)
  (printout t "Rating: " ?*rating* crlf)
  (printout t "Price Range: " ?*price* crlf)
  (printout t " Suggested Restaurant : Gareeb Nawaz" crlf)
  (assert (MAIN::ask order-details)))

;;==========================
;; Chinese cuisine check
;;==========================

(defrule check-cusine-type3
  (answer (id cusine-type) (text ?st&:(if (= ?st chinese) then (bind ?*cusine* ?st))))
  =>
  (printout t "Available Meal Options: " crlf)
  (printout t "breakfast" crlf)
  (printout t "lunch" crlf)
  (printout t "dinner" crlf)
  (assert (MAIN::ask meal-type-chinese)))
  
(defrule check-meal-chinese
  (answer (id meal-type-chinese) (text ?st&:(if (= ?st dinner) then (bind ?*mealType* ?st))))
  =>
  (printout t "Possible ratings: " crlf)
  (printout t "5" crlf)
  (printout t "4" crlf)
  (printout t "3" crlf)
  (printout t "2" crlf)
  (assert (MAIN::ask restaurant-rating-chinese)))
  
(defrule check-rating-chinese
  (answer (id restaurant-rating-chinese) (text ?st&:(if (= ?st 2) then (bind ?*rating* ?st))))
  =>
  (printout t "Price per meal: " crlf)
  (printout t "50" crlf)
  (printout t "25" crlf)
  (printout t "15" crlf)
  (printout t "10" crlf)
  (assert (MAIN::ask meal-price-chinese)))

(defrule check-price-chinese
  (answer (id meal-price-chinese) (text ?st&:(if (= ?st 10) then (bind ?*price* ?st))))
  =>
  (printout t "Cusine: " ?*cusine* "" crlf)
  (printout t "Meal Time: " ?*mealType* crlf)
  (printout t "Rating: " ?*rating* crlf)
  (printout t "Price Range: " ?*price* crlf)
  (printout t " Suggested Restaurant : ChineseHut" crlf)
  (assert (MAIN::ask order-details)))


(defrule confirm-order-details2
  (answer (id order-details) (text ?st&:(eq ?st yes)))
  =>
  (assert (MAIN::ask confirm-booking)))

(defrule confirm-order-details-no
  (answer (id order-details) (text ?st&:(eq ?st no)))
  =>
  (assert (MAIN::ask search-again))) 

(defrule confirm-booking-yes
  (answer (id confirm-booking) (text ?st&:(eq ?st yes)))
  =>
  (printout t "Your booking has been confirmed, have a great time!!")
  (exit))

(defrule confirm-booking-no
  (answer (id confirm-booking) (text ?st&:(eq ?st no)))
  =>
  (assert (MAIN::ask try-again)))

(defrule cusine-again
  (answer (id try-again) (text ?st&:(eq ?st yes)))
  =>
  (assert (MAIN::ask inquire-user)))

(defrule check-try-again
  (answer (id try-again) (text ?st&:(eq ?st no)))
  =>
  (printout t "Thank You for using our services!!" crlf)
  (exit))

(defrule start-search
  (answer (id search-again) (text ?st&:(eq ?st yes)))
  =>
  (assert (MAIN::ask inquire-user)))

(defrule concerns
  (answer (id concerns) (text ?cc&:(eq ?cc no)))
  =>
  (printout t "Thank You for using our services!!" crlf)
  (exit))

(defrule concerns
  (answer (id concerns) (text ?cc&:(eq ?cc yes)))
  =>
  (printout t "Please call our customer care service number at 1800-200-2001" crlf)
  (printout t "Thank You for using our services!!" crlf)
  (exit))

;; =====================
;; Session Close Rule
;; =====================

(defrule close-session
  (answer (id search-again) (text ?cc&:(eq ?cc no)))
  =>
   (assert (MAIN::ask concerns))
   )


;;-------------------------------------------------------------
;; Run
;;-------------------------------------------------------------

(deffunction start-assistant ()
  (reset)
  (focus initialize rules)
  (run))

(while TRUE
  (start-assistant))