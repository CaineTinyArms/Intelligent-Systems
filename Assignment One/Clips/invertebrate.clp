(deftemplate flowchart-step
(slot question-to-ask)
)

(deftemplate identification
(slot name)
)

(deffacts starting-point
(flowchart-step (question-to-ask prerequisite1))
)

(deftemplate decision
(slot question)
(slot answer)
)

(defrule ask-about-prerequisite1
"Asks if the invertebrate has any of the qualities that the system cannot handle, such as small size or skating etc"
?step <- (flowchart-step (question-to-ask prerequisite1))
=>
(printout t "Welcome to the Freshwater invertebrate Identifier, AKA the FII." crlf)
(printout t "Before we begin identifying invertebrate, we need to make sure the invertebrate is suitable to be identified by this system." crlf)
(printout t "So, first question. Is the invertebrate 2mm or longer? (yes/no)" crlf)
(bind ?answer (lowcase (readline)))
(while (not (or (eq ?answer "yes") (eq ?answer "no")))
(printout t "Invalid input. Please enter 'yes' or 'no'." crlf)
(bind ?answer (lowcase (readline)))
)
(if (eq ?answer "yes") then
   (modify ?step (question-to-ask prerequisite2))
else
   (printout t crlf "----------------------------------------" crlf)
   (printout t "The invertebrate is too small to be reliably identified by this system." crlf)
   (printout t "Please consult a microscope-based identification guide for smaller specimens." crlf)
   (printout t "----------------------------------------" crlf)
   (halt)
))

(defrule ask-about-prerequisite2
"Asks if the invertebrate has any of the qualities that the system cannot handle, such as small size or skating etc"
?step <- (flowchart-step (question-to-ask prerequisite2))
=>
(printout t "Great! The invertebrate is long enough to be identified using this program!" crlf)
(printout t "Next Question. Does the invertebrate live on the water surface, or skate on the water surface? (yes/no)" crlf)
(bind ?answer (lowcase (readline)))
(while (not (or (eq ?answer "yes") (eq ?answer "no")))
(printout t "Invalid input. Please enter 'yes' or 'no'." crlf)
(bind ?answer (lowcase (readline)))
)
(if (eq ?answer "no") then
   (modify ?step (question-to-ask shell))
else
   (printout t crlf "----------------------------------------" crlf)
   (printout t "This system can not be used to identify water skating, or water surface dwelling invertebrates." crlf)
   (printout t "Please consult a water based invertebrate identification system." crlf)
   (printout t "----------------------------------------" crlf)
   (halt)
))

(defrule ask-about-shell
"Asks the very first question about a shell or case."
?step <- (flowchart-step (question-to-ask shell))
=>
(printout t "Great, that's all the prerequisite questions out of the way. Let's get onto the identification!" crlf)
(printout t "Does it have a shell or case (like a snail, or a caddisfly case)? (yes/no)" crlf)
(bind ?answer (lowcase (readline)))
(while (not (or (eq ?answer "yes") (eq ?answer "no")))
(printout t "Invalid input. Please enter 'yes' or 'no'." crlf)
(bind ?answer (lowcase (readline)))
)
(assert (decision (question "Does it have a shell or case?") (answer ?answer)))
(if (eq ?answer "yes") then
(modify ?step (question-to-ask shell-material))
else
(modify ?step (question-to-ask legs))
)
)

(defrule ask-about-shell-material
"Asks if the case is made of organic material."
?step <- (flowchart-step (question-to-ask shell-material))
=>
(printout t "Is the case made of leaves, twigs, or sand stones (for example, a small tube or case made from bits of plant or tiny stones)? (yes/no)" crlf)
(bind ?answer (lowcase (readline)))
(while (not (or (eq ?answer "yes") (eq ?answer "no")))
(printout t "Invalid input. Please enter 'yes' or 'no'." crlf)
(bind ?answer (lowcase (readline)))
)
(assert (decision (question "Is the case made of leaves, twigs or sand stones?") (answer ?answer)))
(if (eq ?answer "yes") then
(assert (identification (name "Cased Caddis fly larvae")))
(modify ?step (question-to-ask done))
else
(modify ?step (question-to-ask two-part-shell))
)
)

(defrule ask-about-two-part-shell
"Asks if the mollusc shell is a bivalve."
?step <- (flowchart-step (question-to-ask two-part-shell))
=>
(printout t "Does the shell have two parts (like a clam)? (yes/no)" crlf)
(bind ?answer (lowcase (readline)))
(while (not (or (eq ?answer "yes") (eq ?answer "no")))
(printout t "Invalid input. Please enter 'yes' or 'no'." crlf)
(bind ?answer (lowcase (readline)))
)
(assert (decision (question "Does the shell have two parts (like a clam)?") (answer ?answer)))
(if (eq ?answer "yes") then
(assert (identification (name "Bi-Valve")))
(modify ?step (question-to-ask done))
else
(modify ?step (question-to-ask shell-shape))
)
)

(defrule ask-about-shell-shape
"Asks about the specific shape of a single-part mollusc shell."
?step <- (flowchart-step (question-to-ask shell-shape))
=>
(printout t "What is the shell's shape? (spiral/helical/not-coiled)" crlf)
(printout t "For example: spiral shells curl in a flat coil like a ramshorn snail, helical shells twist upward like a pond snail, and not-coiled shells are flat or cone-shaped like a limpet." crlf)
(bind ?answer (lowcase (readline)))
(while (not (or (eq ?answer "spiral")
(eq ?answer "helical")
(eq ?answer "not-coiled")
(eq ?answer "not coiled")))
(printout t "Invalid input. Please enter spiral, helical, or not-coiled." crlf)
(bind ?answer (lowcase (readline)))
)
(assert (decision (question "What is the shell's shape?") (answer ?answer)))
(if (eq ?answer "spiral") then
(assert (identification (name "Ramshorn Snail")))
)
(if (eq ?answer "helical") then
(assert (identification (name "Pond Snail")))
)
(if (or (eq ?answer "not-coiled") (eq ?answer "not coiled")) then
(assert (identification (name "Freshwater Limpet")))
)
(modify ?step (question-to-ask done))
)

(defrule ask-about-legs
"Asks if the invertebrate has legs, directing to new branches."
?step <- (flowchart-step (question-to-ask legs))
=>
(printout t "Does it have legs? (yes/no)" crlf)
(bind ?answer (lowcase (readline)))
(while (not (or (eq ?answer "yes") (eq ?answer "no")))
(printout t "Invalid input. Please enter 'yes' or 'no'." crlf)
(bind ?answer (lowcase (readline)))
)
(assert (decision (question "Does it have legs?") (answer ?answer)))
(if (eq ?answer "yes") then
(modify ?step (question-to-ask jointed-legs))
else
(modify ?step (question-to-ask looping-action))
)
)

(defrule ask-about-jointed-legs
"Asks if the legs are jointed to identify arthropods or fly larvae."
?step <- (flowchart-step (question-to-ask jointed-legs))
=>
(printout t "Are the legs jointed (for example, with visible bends like a beetle’s legs)? (yes/no)" crlf)
(bind ?answer (lowcase (readline)))
(while (not (or (eq ?answer "yes") (eq ?answer "no")))
(printout t "Invalid input. Please enter 'yes' or 'no'." crlf)
(bind ?answer (lowcase (readline)))
)
(assert (decision (question "Are the legs jointed?") (answer ?answer)))
(if (eq ?answer "yes") then
(printout t crlf "----------------------------------------" crlf)
(printout t "Conclusion: This is an Arthropod." crlf)
(printout t "Action: Please consult the Arthropod Identifier." crlf)
(printout t "Path Taken:" crlf)
(do-for-all-facts ((?d decision)) TRUE
      (printout t "Q: " (fact-slot-value ?d question) crlf
                   "A: " (fact-slot-value ?d answer) crlf))
(printout t "----------------------------------------" crlf)
(halt)
else
(assert (identification (name "Fly Larvae")))
(modify ?step (question-to-ask done))
)
)

(defrule ask-about-looping-action
"Asks if the invertebrate moves like a leech."
?step <- (flowchart-step (question-to-ask looping-action))
=>
(printout t "Does it move with a looping action (like a leech, stretching and contracting its body as it moves)? (yes/no)" crlf)
(bind ?answer (lowcase (readline)))
(while (not (or (eq ?answer "yes") (eq ?answer "no")))
(printout t "Invalid input. Please enter 'yes' or 'no'." crlf)
(bind ?answer (lowcase (readline)))
)
(assert (decision (question "Does it move with a looping action?") (answer ?answer)))
(if (eq ?answer "yes") then
(assert (identification (name "Leech")))
(modify ?step (question-to-ask done))
else
(modify ?step (question-to-ask flat-body))
)
)

(defrule ask-about-flat-body
"Asks if the invertebrate has a flat body."
?step <- (flowchart-step (question-to-ask flat-body))
=>
(printout t "Does it have a flat body? (yes/no)" crlf)
(bind ?answer (lowcase (readline)))
(while (not (or (eq ?answer "yes") (eq ?answer "no")))
(printout t "Invalid input. Please enter 'yes' or 'no'." crlf)
(bind ?answer (lowcase (readline)))
)
(assert (decision (question "Does it have a flat body?") (answer ?answer)))
(if (eq ?answer "yes") then
(assert (identification (name "Flatworm")))
(modify ?step (question-to-ask done))
else
(modify ?step (question-to-ask segment-count))
)
)

(defrule ask-about-segment-count
"Asks for a number of segments to identify worms or fly larvae."
?step <- (flowchart-step (question-to-ask segment-count))
=>
(printout t "How many segments does it have? (enter a number, e.g., 0, 8, 20)" crlf)
(bind ?answer (read))
(while (or (not (numberp ?answer)) (< ?answer 0))
(printout t "Invalid input. Please enter a number that is 0 or greater." crlf)
(bind ?answer (read))
)
(assert (decision (question "How many segments does it have?") (answer ?answer)))
(if (= ?answer 0) then
(modify ?step (question-to-ask tentacles))
)
(if (and (>= ?answer 1) (<= ?answer 14)) then
(assert (identification (name "Fly Larvae")))
(modify ?step (question-to-ask done))
)
(if (>= ?answer 15) then
(assert (identification (name "Worms")))
(modify ?step (question-to-ask done))
)
)

(defrule ask-about-tentacles
"Asks about tentacles to identify Hydra or Hairworm."
?step <- (flowchart-step (question-to-ask tentacles))
=>
(printout t "Does it have tentacles on the end of its body? (yes/no)" crlf)
(bind ?answer (lowcase (readline)))
(while (not (or (eq ?answer "yes") (eq ?answer "no")))
(printout t "Invalid input. Please enter 'yes' or 'no'." crlf)
(bind ?answer (lowcase (readline)))
)
(assert (decision (question "Does it have tentacles on the end of its body?") (answer ?answer)))
(if (eq ?answer "yes") then
(assert (identification (name "Hydra")))
else
(assert (identification (name "Hairworm")))
)
(modify ?step (question-to-ask done))
)

(defrule report-identification
"When an identification is made, print it and halt."
(identification (name ?creature))
=>
(printout t crlf "---------------------------- ------------" crlf)
(printout t "Identification: " ?creature crlf)
(printout t "Path Taken:" crlf)
(do-for-all-facts ((?d decision)) TRUE
      (printout t "Q: " (fact-slot-value ?d question) crlf
                   "A: " (fact-slot-value ?d answer) crlf))
(printout t "----------------------------------------" crlf)
(halt)
)