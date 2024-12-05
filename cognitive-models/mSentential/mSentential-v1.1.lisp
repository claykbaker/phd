;-------------------------------------------------------------------------------------------------------------------------
; mSentential v1.1  November 1st 2017
;-------------------------------------------------------------------------------------------------------------------------

; ------------------------------------------------------------------------------------------------------------------------
; Copyright (C) November 2017 Mental Models and Reasoning Lab
;
; Website: http://mentalmodels.princeton.edu
; Contact: Phil Johnson-Laird (phil@princeton.edu) or Sangeet Khemlani (skhemlani@gmail.com), who wrote the Tracer, 
;          Batch processing modeling functions, and other code.
; 
; mSentential is licensed under a Creative Commons Attribution-NonCommercial-
; ShareAlike 4.0 International License. You are free to:
;
;    Share - copy and redistribute the material in any medium or format
;    Adapt - remix, transform, and build upon the material
;
; under the following terms
;
;    Attribution   - You must give appropriate credit, provide a link to the license,
;                    and indicate if changes were made. You may do so in any reason-
;                    able manner, but not in any way that suggests the licensor end-
;                    orses you or your use.
;    NonCommercial - You may not use the material for commercial purposes.
;    ShareAlike    - If you remix, transform, or build upon the material, you must
;                    distribute your contributions under the same license as the
;                    original.
;
; The licensor cannot revoke the freedoms above as long as you follow the license
; terms. You may not apply legal terms or technological measures that legally
; restrict others from doing anything the license permits.
;
; For more information on this license, please visit:
; http://creativecommons.org/licenses/by-nc-sa/4.0/
; -------------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------------------
; INTRODUCTION
;--------------------------------------------------------------------------------------------------------------------------

;------------------------
; CHEAT SHEET
;------------------------
#| 
   This program carries out various sorts of task in sentential reasoning (based on if, or, and, not) according to 
the current theory of mental models.  Here are various illustrations of how to use the program:-
I.   To see the mental models and fully explicit models for a set of premises, evaluate expressions such as
     the following one in which the list is of each premise:
                   (inference '((if a or b then c)(a)))
II.  The process of building models in system 2 can modulate the interpretation of premises, and even show
     that a premise is true a priori or false a priori:
                   (inference '((God exists or atheism is right))) 
     for which modulation modifies the interpretation and show that the assertion is true a priori.
III. Some specific tasks:-
  1. 'what-follows? Draws its own simple categorical conclusions. 
                   (inference '((if a or b then c)(a)) 'what-follows?) 
     We haven't implemented the minimization algorithm in the PropAI program, which always draws a conclusion equivalent 
     to a minimal description of the premises, because the algorithm is too sophisticated to be psychologically plausible.
  2. 'necessary?   That is, does the final premise follow necessarily from the previous premises:
                   (inference '((a)(a or b)) 'necessary?) - an example illustrating that the theory differs from logic.
                   (inference '((if a then b)(not b)(not a)) 'necessary?) - draws a necessary conclusion only in system 2.
  3. 'necessary? When the last premise contradicts earlier premises, the program calls DEFEASANCE to use knowledge to 
     try to resolve the inconsistency, so evaluate:
       (inference '((if a poisonous snake bites her then she dies)(A poisonous snake bites her)(not she dies)) 'necessary?)
  4. 'possible? Checks whether or not the last premise follows as a possibility from the previous premises'possible?    
       (inference '((a)(a or b)) 'possible?)
     Task is equivalent to checking the consistency of the entire set of premises. 
  5. 'probability? Returns, if possible, the extensional probability of the last premise given the previous premises.
        (inference '((it is hot or it is humid)(it is hot)) 'probability?)  
  6. 'verify? Uses the second premise, if it is a conjunction, as evidence to verify the first premise. System 1 returns 
      the intuitive truth value.  System 2 returns the counterfactual conditionals that have to hold.
         (inference '((if a then b)(not a and not b)) 'verify?)
  7.  The program can be used to model data by setting two parameters, if a random number from 0 to 1 is greater than 
      *sigma* then system 2 is engaged to check inferences, and if a random number greater *gamma* then the program uses 
      weak validity.  For modeling, see Part 8 of the program.
|#

;--------------------------------------------
;  THE PROGRAM'S OVERALL STRUCTURE
;--------------------------------------------

#|
inference
   describe-task
   INTUITION
      build-models
      conclude
          VERIFICATION
                 counterfactual
          probability
   to-system2?
   DELIBERATION
      build-models
         MODULATION
      conclude (see above)
   DEFEASANCE
      explain
         counterfactual

 The following diagram illustrates the structure of the program:-

             ...................                  ...................
             .    System 1     .                  .     System 2    .                            
             .                 .    probability   .                 .               
             .                 .        ^         .                 .
             .                 .        |         .                 .
             .    INTUITION ---.--> conclude <----.---DELIBERATION--.---- 
             .       |         .        |         .         ^       .   |
             .       |         .        |         .         |       .   |
             .       |         .        v         .         v       .   |                     
             .       |         .   VERIFICATION   .     MODULATION  .   |
             ........|..........        |         ...................   |
                     |                  v                               |
                     |            counterfactuals                       |
                     |                  ^                               |
                     |                  |                               |
                     |               explain                            |
                     |                  ^                               |
                     |                  |                               |
                     -------------> DEFEASANCE <-------------------------          

The five functions in CAPITALS are the main components of the program:-
System 1:
 INTUITION:      calls the parser to build mental models, takes simple premises, and tries to make an inference 
                 based on a single mental model. Calls Deliberation stochastically. 
System 2:
 DELIBERATION:   calls the parser to build fully explicit models to make any sort of inference, and it calls:
 MODULATION:     to modify the interpretation of a connective, using knowledge in the form of fully explicit models to do 
                 so, and to return a priori truth values.  
Both systems 1 and 2 call:
 conclude:       evaluates a conclusion, and can call verification (below), and probability, which computes extensional
                 probabilities in system 2.
 DEFEASANCE:     if a fact is inconsistent with premises, then systems 1 and 2 reason nonmonotonically, deciding which 
                 premise to abandon, creating a causal explanation from knowledge to resolve the inconsistency, and framing
                 a counterfactual version of the original compound premise.
 VERIFICATION:   to assess the truth of a compound assertion in the light of evidence for systems 1 and 2, and 
                 specifying counterfactuals that have to hold for a compound assertion to be true; it also calls  |#

;-----------------------------
; THE PARTS OF THE CODE
;----------------------------
#|
          LANGUAGE (SYSTEM 0)
 Part 0.1:  Lexicon and grammar
 Part 0.2:  The bottom-up parser
 Part 0.3:  The compositional semantics
 Part 0.4:  Negation
 Part 0.5:  Lexical semantics for 'and', 'if', 'iff', and 'or' (inclusive and exclusive)
 Part 0.6:  Conjunction of two sets of models (their Cartesian product)
          1. INTUITION (System 1): inference with mental models
 Part 1.1:  Inference -- the top level function for the whole program
 Part 1.2:  Intuition (System 1)
 Part 1.3:  Conjunctions of mental models in System 1
 Part 1.4:  Interface with Deliberation and fleshing out of models  
         2. DELIBERATION (SYSTEM 2): with fully explicit models
 Part 2.1:  Deliberation
 Part 2.2:  Assesses system 1 conclusions and draws its own  
         3. SHARED FUNCTIONS FOR INTUITION AND DELIBERATION (SYSTEMS 1 AND 2)
 Part 3.1:  Build models
 Part 3.2:  Describe a model
 Part 3.3:  Conclude
 Part 3.4:  VERIFICATION, the construction of counterfactuals, and probabilities
         4. MODULATION, DEFEASANCE (nonmonotonic reasoning), and knowledge
 Part 4.1:  Knowledge base for modulation
 Part 4.2:  MODULATION
 Part 4.3:  DEFEASANCE: mismatch, explanation, and call to counterfactuals
         5. Low-level functions
 Part 5.1:  Models and their manipulation
 Part 5.2:  Matching or removing models or their parts
 Part 5.3:  Print functions
         6. Insert hyphens and remove them from notation
         7. TRACER CLASSES AND FUNCTIONS (SSK)
         8. BATCH PROCESSING (SSK)
         9. ILLUSTRATIONS of the program's performance
 Part 9.1:  Functions for illustrating the program
 Part 9.2:  Examples for illustrating the program 
 Part 9.3:  List of inferences used for modeling data
|#   

;---------------------------------------------------------------------------------------------------------------------------
;                                               LANGUAGE (SYSTEM 0)
;---------------------------------------------------------------------------------------------------------------------------

;--------------------------------
; Part 0.1 Lexicon and grammar
;--------------------------------

#| word, syntactic category, and semantic fn, which is nil for syncategorematic words |#
(defparameter *lexicon* '(
   ((and)      (conn    (and-ing)))       
   ((or)       (conn    (or-psy)))  ;  inclusive
   ((ore)      (conn    (or-e)))    ;  exclusive
   ((unless)   (conn    (or-e)))
   ((if)       (c-antec (if-psy)))  
   ((iff)      (c-antec (if-f)))
   ((then)     (co       nil))      
   ((not)      (neg     (negate)))               
   ((comma)    (PUNCT    nil))     
   ((tautology)(PUNCT    nil)) ))

#| An unambiguous context-free grammar: expansion, non-terminal, compositional semantic fn. Different semantic 
functions for atomic sentences and for variables so that generation of descriptions works properly |#
(defparameter *grammar* '(
   ((var)                                (sentence (sem-aff-var)))
   ((neg var)                            (sentence (sem-neg-var)))
   ((sentence conn sentence)             (sentence (sem-scs) ))
   ((c-antec sentence co sentence)       (sentence (sem-cscs)))
   ((punct sentence conn sentence)       (sentence (sem-scs)))
   ((punct c-antec sentence co sentence) (sentence (sem-cscs)))
   ((neg sentence)                       (sentence (sem-negate)))))

(defvar *system* 1) ; global for Inference (system 1) or Deliberation (system 2) in reasoning

;----------------------------------
; Part  0.2: The bottom-up parser
;----------------------------------

#| The BU Parser assumes an unambiguous grammar and closes off constituents as soon as possible, e.g. it returns, 
in effect,the following for: If a then b and c => ((if a then b) and c)  
But, punctuation can capture the other structure:
   If a then comma b and c => (if a then (b and c))
the word "comma" works as a left parenthesis, and all conectives take just two arguments. |#

#| comp-resul applies semantic fn of item about to go on p-stack to the unreduced p-stack. Update puts result 
into the right slot of the now syntactically revised p-stack |#
(defun parse (sentence)
  "shift and reduce parser that assumes unambiguous grammar. Semantics is sensitive to *system* 1 vs 2"
  (setf sentence (tranhyph sentence))
  (prog (p-stack new-stack)
    loop
    (cond((setf new-stack (reduces p-stack nil *grammar*)) ;nil for initial outlis
              (setf p-stack (update new-stack (comp-resul new-stack p-stack)))
              (go loop))
         ((null sentence)
              (cond((or (null p-stack)(cdr p-stack))
                        (print-sentence '(parse incomplete))(return nil))
                   (t  (return (cadar p-stack)))))
         ((setf p-stack (append p-stack (shift (car sentence) *lexicon*)))
                   (setf sentence (cdr sentence))
                   (go loop))
         (t (return '(error in parse))))))

(defun shift (word lex)
  "shifts word in lexicon, otherwise calls checkvar to treat word as a variable"
  (let ((rule (car lex)))
    (cond((null lex)(checkvar word))
         ((equal word (word_in rule))(category-&-semantics_in rule))
         (t (shift word (cdr lex))))))

(defun word_in(rule)
  (caar rule))

(defun category-&-semantics_in(rule)
  (cdr rule))

(defun checkvar(word)
  "if word not in lexicon, it is treated as a variable"
  (list (append (list 'var)(list(list word)))))

(defun reduces (inlis outlis gram)
  "reduces p-stack a/c to grammar - mapcar deletes semantics from p-stack before search in grammar"
  (let (item)
    (cond((null inlis) nil)
         ((setf item (red (mapcar #'car inlis) gram)) ;e.g. item is (NP1 (NPCODE))
            (append outlis (list item)))
         (t  (reduces (cdr inlis) (append outlis (list (car inlis))) gram)))))

(defun red(lis gram)
  "reduce calls this fn to compare p-stack with rhs of rules in grammar"
  (dolist (rule gram)
    (if (equal lis (expansion_of rule))
        (return (nonterminal_of rule)))))

(defun expansion_of(rule)
  (car rule))

(defun nonterminal_of(rule)
  (cadr rule))

(defun update (p-stack resul)
  "shifts word in lexicon, otherwise calls checkvar to treat word as a variable"
  (reverse(cons (append(cdr(reverse(car(reverse p-stack)))) resul)
              (cdr (reverse p-stack)))))

;----------------------------------------
; Part 0.3: The compositional semantics
;----------------------------------------

#|   new-stack = ((SENTENCE (SEM-CSCS))) 
     p-stack   = ((C-ANTEC (IF-I)) (SENTENCE (((A1) (B1)))) (CO NIL) (SENTENCE (((C1)))))
So, applies SEM-CSCS to p-stack, which retrieves #'IF-I, and applies it to the two sets of models
((A1)(B1)) and (((C1))) => ((((A) (B) (C)) ((T0)))) |#
(defun comp-resul(new-stack p-stack)
  "retrieves semantic_fn from new-stack and applies it ot p-stack"
  (list (apply (semantic-fn_on new-stack)(list p-stack))))

(defun semantic-fn_on(stack)
  (caadar (reverse stack)))

(defun sem-scs (stak)
  "composes semantics for s connectivve s"
  (let ((connective (car(cadadr (reverse stak))))
        (models1 (cadr(caddr(reverse stak))))
        (models2 (cadar (reverse stak))))
    (funcall connective models1 models2)))

(defun sem-cscs(stak)
  "composes semantics for c-antec s connective s for if_then"
  (let ((connective (caadr (cadddr(reverse stak))))
        (models1 (cadr(caddr(reverse stak))))
        (models2 (cadar (reverse stak))))
    (funcall connective models1 models2)))

(defun sem-negate(stak)
  "negates a set of models in order to do sentential negation"
  (negate (cadar (reverse stak))))
 
(defun sem-aff-var(stak)
  "semantics for for variables to get proper generation by gen-sen" 
  (list (list (upfun stak))))

(defun sem-neg-var(stak)
  "sem-neg for variables to get proper generation by gen-sen"
  (list(list(negate(upfun stak)))))

(defun upfun (stak)
  "rtns the semantic-fn in the last item on stak"
  (cadar(reverse stak)))

;-----------------------------------------------------------------------------------------------------------
; Part 0.4: Negation
;-----------------------------------------------------------------------------------------------------------

(defun negate (models)
  "rtns complement of a set of models"
  (cond((truth-valuep models)(not models))
       ((atom (car models))(neg models))
       ((listp (car models))
           (setf models (flesh-out(make-explicit models)))     ; with implicit
           (comp models (allpos (findatms models))))))

(defun truth-valuep(models)
  "Rtns t iff models are t or nil"
  (or (eq models t)(eq models nil)))

(defun comp(models allposmodels)
  "removes each member of models from allposmodels
   (comp '(((a)(b))((- a)(- b))) '(((a)(b))((a)(- b))((- a)(b))((- a)(- b)))) => (((a)(- b))((- a)(b)))"
  (dolist(mod models)
    (if (null models)
        allposmodels
    (setf allposmodels (remove-itm mod allposmodels))))
  allposmodels)

(defun neg (item)
  "puts '- in front of symbol, or removes it if already there"
  (if (equal (car item) '-)
      (cdr item)
     (cons '- item)))

;---------------------------------------------------------------------------------------
; Part 0.5: Lexical semantics for 'if', 'iff', 'or', and 'ore'
;---------------------------------------------------------------------------------------

(defun if-psy (models1 models2)
  (let ((anonymous (gentemp))) ; variable for implicit models
    (set anonymous (negate models1))
    (if (null (eval anonymous))
        (and-ing models1 models2)
    (append-models (and-ing models1 models2)(modelize anonymous)))))

(defun if-f (models1 models2)
  "biconditional interpretation"
  (let ((anonymous (gentemp)))
    (set anonymous (and-ing (negate models1)(negate models2)))
    (if (null (eval anonymous))
        (and-ing models1 models2)
      (append-models (and-ing models1 models2)(modelize anonymous)))))

(defun or-e(models1 models2)
  "exclusive disjunction"
  (let ((anon1 (gentemp))(anon2 (gentemp)))
    (set anon1 (negate models1))
    (set anon2 (negate models2))
    (append-models (and-ing models1 (modelize anon2))
                   (and-ing models2 (modelize anon1)))))

(defun or-psy(models1 models2)
  "inclusive disjunction"
  (let ((anon1 (gentemp))(anon2 (gentemp)))
    (set anon1 (negate models1))
    (set anon2 (negate models2))
    (append-models   (and-ing models1 (modelize anon2))
                     (append-models (and-ing models2 (modelize anon1))
                                    (and-ing models1 models2)))))

(defun append-models(models1 models2)
  "appends models, making them fully expicit for Deliberation"
(cond((= *system* 1)(append-mods models1 models2))
     ((= *system* 2)(flesh-out(make-explicit(append-mods models1 models2))))))

(defun append-mods(models1 models2)
  "makes single set of models from two sets"
  (cond((eq models1 t)
         (cond((eq models2 t) t)
              ((eq models2 nil) t)
              (t (append (list t) models2))))
     ((eq models2 t)(append models1 (list t)))
     (t (append models1 models2))))

(defun modelize(var)
  "for sys 1, turns variable into set of models; for sys 2 evaluates the variable"
  (list(list(list var))))

;-----------------------------------------------------------------------------------------------------------
; Part 0.6: Conjunction of two sets of models (their Cartesian product)
;-----------------------------------------------------------------------------------------------------------

(defun and-ing (models1 models2)
  "ands two sets of models, calling either and-system-1 or and-system-2)" 
  (setf models1 (eval-simple-var models1) models2 (eval-simple-var models2)) 
  (cond((or (null models1)(null models2)) nil)           ; both systems
       ((eq models1 t) models2)                          ; "
       ((eq models2 t) models1)                          ; "
       ((= *system* 1) (tidy-up (and-lis #'and-sys-1 models1 models2)))
       ((= *system* 2)(flesh-out(make-explicit(and-lis #'and-system-2 models1 models2))))
       (t (error "Unrecognized value of *system* variable"))))

(defun and-lis (fun models1 models2)
  "applies fn to every pair of models from two sets of models"
  (let ((mods1 models1)(mods2 models2) new-mod output)
  (dolist (mod1 mods1 output)
    (dolist (mod2 mods2)
        (setf new-mod (apply fun (list mod1 mod2 models1 models2)))
        (setf output (append output new-mod))))
  output))

;-----------------------------------------------------------------------------------------------------------
;                      1. INTUITION (System 1): inference with mental models
;-----------------------------------------------------------------------------------------------------------

;----------------------------------------------------------------------------------
; Part 1.1:               Inference -- the top level function for the whole program
;----------------------------------------------------------------------------------

; Prints footnotes in print-models unless set to nil
(setf *print-footnotes* nil)

(setf *sigma* 0.0) ; if random number is higher then *sigma* calls system 2 to check an inference. 
(setf *gamma* 0.0) ; if random number is higher then uses weak validity.

;;; A global variable setting limit on s1-dolist, so that it iterates on a list only 8 times
(defvar *max-iterations* 8)

(defun inference(premises &optional task)
  (let (sys1-output sys2-output)
  (describe-task premises task)
  (setf sys1-output (INTUITION premises task)) ; intuition deals w what-follows? & other tasks
  (trc "System 1" (format nil "Number of explicit mental models constructed equals ~A." (num-models *tracer*)))
  (call-DEFEASANCE? sys1-output premises)
;  (setf *sigma* 0) ; to call system2 setf *sigma* to 1
  (cond((to-system2?)(setf (num-models *tracer*) 0)
                     (setf sys2-output (DELIBERATION premises task sys1-output))
                     (trc "System 2" (format nil "Number of fully explicit models constructed equals ~A." 
                                             (num-models *tracer*)))
                     (call-DEFEASANCE? sys2-output premises))                              
       (t sys1-output))))

(defun call-defeasance? (sys-output premises)
  (let ((sys (case *system* (1 "System 1") (2 "System 2") (otherwise "Control"))))
    (cond((equal sys-output 'CONTRADICTION)
              (trc sys (format nil "Considering whether knowledge can resolve the contradicion:"))
              (cond((find-models-in-knowledge (build-models (list(first premises))))
                        (DEFEASANCE premises))
                   (t (trc sys (format nil "Knowledge in base cannot resolve the contradicion."))
                      sys-output)))
         (t sys-output))))

#| A wrapper macro for limiting number of operations in dolist to *max-iterations*
When s1-dolist runs for more than *max-iterations* it stops can rtn nil |#
(defmacro s1-dolist ((var list &optional result) &body body)
  `(let ((iterations 0))
     (block loop
       (progn
         (mapc #'(lambda (,var) (if (>= iterations *max-iterations*)
                                    (return-from loop ,result)
                                  (progn (incf iterations) ,@body)))
               ,list)
         (let ((,var nil))
           ,result)))))

(defun describe-task(premises task)
  "describes task and premises; if no task specified or unrecognizable one just rtn mms and then fems of premise"
  (let ((all-premises (format-lis premises))
        (all-but-last (format-lis (reverse (rest (reverse premises)))))
        (last-premise (format-lis (last premises))))
    (trc "Control" 
       (case task
         ('what-follows? 
            (format nil "Set task: Infer what follows from: ~%                ~A                           " all-premises))
         ('necessary?    (format nil "Set task: Given ~A, infer if ~A necessarily follows" all-but-last last-premise))
         ('possible?     (format nil "Set task: Given ~A, infer if ~A possibly follows" all-but-last last-premise))
         ('probability?  (format nil "Set task: Given ~A, infer the probability of ~A" all-but-last last-premise))
         ('verify?       (format nil "Set task: Given evidence ~A, verify ~A" last-premise all-but-last))
         (otherwise      (format nil "No task specified and so builds models of all the premises" all-premises))))))

;--------------------------------------------------------------
; Part 1.2   Intuition (System 1)
;--------------------------------------------------------------

(defun intuition (premises &optional task)
  "to draw conclusion: intuit-what-follows; else builds mental models for premises and concl (last premise)"
  (let ((all-but-last (reverse (rest (reverse premises))))(last-premise (last premises)) 
         premise-models conclusion-models)
   (setf *system* 1)
   (cond((equal task 'what-follows?)(intuit-what-follows premises))  
        ((null task)
              (trc "System 1" (format nil "The premises yield the models")) 
              (build-models premises))
        (t    (setf premise-models (build-models all-but-last) 
                    conclusion-models (build-models last-premise))
              (if (common-atms all-but-last (first last-premise))
                  (conclude premise-models conclusion-models last-premise task)
                (trc "System 1" 
                     (format nil "Conclusion has no propositions in the premises: it's INDEPENDENT.")))))))

(defun intuit-what-follows(premises)
  "tries to draw a conclusion from one model else call intuit-follows"
  (let* ((premises (reorder-premises premises))(categoricals (find-categoricals premises)) 
         (models (build-models premises))(new-models (remove-categoricals-from-models categoricals models)) concl)
    (cond ((and (not (tautologyp new-models))(= (length models)(length new-models)))
              (setf models new-models)))
    (cond ((= (length models) 0)
              (setf concl 'contradiction))
          ((= (length models) 1)
              (if (not (eq-desc-cats (car models) categoricals))
                  (setf concl (list 'nec (describe-m (first models))))
                (setf concl 'nvc)))
          ((tautologyp models)(setf concl 'tautology))
          (t (setf concl (intuit-follows models categoricals))))
    (intuit-conclusion concl)))

(defun intuit-follows(models categoricals)
  "looks for itm constant over all models as basis for concl, else tries for conditional descn, else TMI"
  (let ((models (length-order models)) new-mod concl)
    (cond((setf new-mod (find-constant-itms models))
           (if (not (eq-desc-cats new-mod categoricals))
               (list 'nec (describe-m new-mod))
             (list 'pos (describe-m (first (remove-categoricals-from-models categoricals models))))))
         ((>= (length models) 2)
            (if (second (setf concl (list 'pos (describe-conditional-concl models))))
                   concl
              (list 'TMI (describe-m (first models)))))
         (t (trc "System 1" (format nil "NOTHING FOLLOWS as far as one can tell.")) 'nvc))))

(defun intuit-conclusion(concl)
  "trc for appropriate sort of concl (nil 'tautology 'nvc '(tmi concl) '(pos (A)) '(nec '(A))) which it leaves unaltered"
  (let ((sys "System 1"))
  (cond((null concl)(trc sys (format nil "NOTHING FOLLOWS from inconsistent premises.")))
       ((equal concl 'tautology)(trc sys (format nil "NOTHING FOLLOWS from tautology.")))
       ((equal concl 'nvc) (trc sys (format nil "NOTHING FOLLOWS from the premises.")))
       ((listp concl)
          (cond((equal (car concl) 'nec)
                 (trc sys (format nil "A conclusion that is NECESSARY is: ~A" (second concl))))
               ((equal (car concl) 'pos)
                 (trc sys (format nil "Nothing follows necessarily but a POSSIBLE conclusion is: ~A" (second concl))))
               ((equal (car concl) 'tmi)
                 (trc sys (format nil "Too much information, but a conclusion that is POSSIBLE is: ~A" (second concl)))))))
  concl))

(defun eq-desc-cats(mod categoricals)
  "tests whether model is same as categoricals -- may call for more powerful fn"
  (equal (list (describe-m mod)) categoricals))

(defun describe-conditional-concl(models)
  "for 2 or more models tries to frame conditional conclusion"
  (let ((models (length-order models)) antec conseq)
  (cond((and (> (length (car models))(length (cadr models)))
             (setf antec (car (rem-itms-from-models (cadr models) (list (car models))))))
          (setf conseq (car (rem-itms-from-models antec (list (car models)))))
          (if (> (length conseq) 1)
              (append (list 'if)(describe-m antec)(list 'then 'comma)(describe-m conseq))
            (append (list 'if)(describe-m antec)(list 'then)(describe-m conseq)))))))

(defun length-order (models)
  "puts models into order a/c to length with longest first"
  (cond((length-ordered models) models)
       ((< (length (first models))(length(second models)))
             (length-order(cons (second models)(cons (first models)(cddr models)))))
       (t (length-order (cons(first models)(length-order (rest models)))))))
     
(defun length-ordered (models)
  "checks that models are ordered by their length"
  (cond((null (cdr models)) t) ; only one model
       ((>= (length (car models))(length (cadr models)))(length-ordered (cdr models)))))

(defun tautologyp(models)
  "if not null.models rtns T iff models are all possible for those atoms"
  (let ((models (flesh-out(make-explicit models)))) 
  (if models 
      (matchmodelsets (allpos (car models)) models))))

;------------------------------------------------------
; Part 1.3   Conjunctions of mental models in System 1
;------------------------------------------------------

(defun and-sys-1(m1 m2 &optional mods1 mods2) 
  "Main fn for and in system-1, so lexical entries, and build models, call and-ing, which calls this function"
    (if (reorder m1 m2)
        (setf temp-m2 m1  temp-mods2 mods1  m1 m2  m2 temp-m2  mods1 mods2  mods2 temp-mods2))
    (cond((inconsistent m1 m2) nil)                  
         ((explicit m1)
              (cond((explicit m2)       (join-models m1 m2)) ; rtns set of models containing one model
                   ((partial  m2)       (explicit&partial m1 m2 mods2))                 
                   ((wholly-implicit m2)(explicit&implicit m1 m2 mods2))))
         (t (join-partials m1 m2 mods1 mods2))))

(defun join-models(m1 m2)
  "joins consistent models, which could contain footnotes, and rtns one model in a set containing one token of 
    each model; for models with more than *max-iterations*  models rtns only up to maximum"
  (let (outmod)
    (s1-dolist(itm m1)
      (setf outmod (append outmod (list itm)))
      (if (match itm m2)
          (setf m2 (remove-itm itm m2))))
    (list (append outmod m2))))

(defun explicit&partial(m1 m2 mods2)
  "if itm in m1 not in m2, but in mods2, assumes its negation is in m2, so rtns nil, otherwise adds fn 
   contents, if necessary, to m1 and adds rest of m2 to m1"
  (let ((outmod t) fn)
    (cond((setf outmod (itm-not-in-other-models m1 m2 mods2)) ;rtns m1 else nil
             (setf fn (partial m2) m2 (remove-itm (list fn) m2))
             (setf outmod (join-models (add-models m1 (eval fn)) m2))))
    outmod))

(defun add-models(m1 mods2)
  "adds m1 to content of each model in mods2 provided not inconsistent and not already there"
  (cond((null mods2) m1)
       ((and (not (inconsistent m1 (car mods2)))
             (not (equal m1 (car mods2))))
          (cons (caar mods2)(add-models m1 (cdr mods2))))
       (t (add-models m1 (cdr mods2)))))

(defun explicit&implicit(m1 m2 mods2)
  "joins explicit and implicit model within bounds of *max-iterations*"
  (let ((fn-mods (eval (includes-implicit m2))) new-fn)
    (cond((= (length mods2) 1)(join-models m1 m2))
         (t (s1-dolist(fn-m2 fn-mods new-fn)
              (if (not (inconsistent m1 fn-m2))
                  (setf new-fn (append new-fn (join-models m1 fn-m2)))))
            (if new-fn
                (list m1))))))               ; value of footnote in m2         

(defun join-partials (m1 m2 mods1 mods2)
  "joins two partially implicit models"
  (let ((new-footnotes (amalgamate-footnotes m1 m2)) outmod1 outmod2)  ; amalgamate-footnotes rtns (((t2))) or nil
    (cond((and new-footnotes (wholly-implicit m1))(list new-footnotes))
         ((setf outmod1 (itm-not-in-other-models m1 m2 mods2))
            (cond((wholly-implicit m2)(join-models (remove-implicit outmod1) new-footnotes))
                 ((setf outmod2 (itm-not-in-other-models m2 m1 mods1))                        ; another partial model
                      (join-models (remove-implicit outmod1)(remove-implicit outmod2)))))))) 

(defun amalgamate-footnotes(m1 m2)
  "takes two models of footnotes only, builds set of models of their values, sets it to new fn, and rtns model of fn"
  (let ((fn-1 (eval (includes-implicit m1)))
        (fn-2 (eval (includes-implicit m2))) (anonymous (gentemp)) fn-mods)
    (if (and fn-1 fn-2)                                 ; so partialxpartial can apply to explicit models
        (cond((setf fn-mods (remove-dups (append fn-1 fn-2)))
                    (set anonymous fn-mods)(list (list anonymous)))))))

(defun itm-not-in-other-models(m1 m2 mods2)
  "rtns m1 without fn if no itm that is not in m2 but is in mods2, so item-not-in-other models, otherwise rtns nil;
   cannot use s1-dolist because Not can occur inside a block named NIL." 
  (let (outmod)
  (dolist (itm m1 outmod)
    (cond((and (not (match itm m2))(item-in-models itm mods2))(return (setf outmod nil)))
         ((match itm m2)(setf m2 (remove-itm itm m2))(setf outmod (cons itm outmod)))
         ((not (boundp (car (last itm))))(setf outmod (cons itm outmod))))) ; nb - in '(- A) is boundp
  outmod))

(defun inconsistent(m1 m2)
  "rtns t iff explicit parts of models contain atm and its negation, within limits of *max-iterations*"
  (dolist(itm m1)
    (if (match (negate itm) m2)
        (return t))))

(defun reorder(m1 m2)
  "rtns t if models should be reordered to meet dominance order explict > partial > wholly-implicit"
    (cond((explicit m1) nil)
         ((explicit m2) t)
         ((partial m1) nil)
         ((partial m2) t)))

(defun remove-implicit(model)
  "removes footnotes from an otherwise explicit model"
  (let (outmodel)
  (s1-dolist(lit model outmodel)
    (if (not (footnotep lit))
        (setf outmodel (append outmodel (list lit)))))))

;-----------------------------------------------------------------------------------------------------
; Part 1.4: Interface with Deliberation and fleshing out of models 
;-----------------------------------------------------------------------------------------------------

(defun to-system2?()
  (cond((< (random-probability) *sigma*) 
           (trc "Control" (format nil "Engaging System 2")) t)
       (t  (trc "Control" (format nil "Not engaging System 2")) nil)))

(defun random-probability()
  (/ (random 100) 100.0))

(defun flesh-out (models)
  "fleshes out explicit models from make explicit, e.g, (((a)(b))(- a)) => fem of conditional"
  (if (truth-valuep models)
      models
     (let* ((template (findatms models))(templ template))
       (s1-dolist(itm templ)
         (setf models (flesh itm models)))
       (re-arranges template (tidy-up models)))))

(defun flesh(item models)
  "adds aff item and neg item to make separate models if neither is in a model"
  (let ((mod (car models)))
    (cond((null models) nil)
         ((or (match item mod)
              (match (negate item) mod))
                (cons mod(flesh item (cdr models))))
         (t (cons (cons item mod)
                (cons (cons (negate item) mod)(flesh item (cdr models))))))))

(defun make-explicit(models)
  "makes partly explicit and wholly explicit models explicit by adding their values, e.g., for conditional
     (((a)(b))((t1))) => (((a)(b))((- a))), and so needs fleshing out"
  (cond((null models) nil)
       ((explicit (car models))(cons (car models)(make-explicit (cdr models))))
       ((partial (car models))
          (append-mods (part-impl-to-expl (car models))(make-explicit (cdr models))))
       (t (append-mods (eval (wholly-implicit (car models)))(make-explicit (cdr models))))))

(defun part-impl-to-expl(model)
  "called by make-explicit - makes variable in partly-explicit model into set of fems"
  (let ((imp-mods (eval (partial model)))
        (exp-mods (list (remove-implicit model))))
    (cond((eq imp-mods t) exp-mods)
         ((eq exp-mods t) imp-mods)
         ((or (eq imp-mods nil)(eq exp-mods nil)) nil)
         (t  (and-lis #'and-explicit exp-mods imp-mods)))))

;-----------------------------------------------------------------------------------------------------------
;                                 DELIBERATION (SYSTEM 2): with fully explicit models
;-----------------------------------------------------------------------------------------------------------

;-------------------------------------------
; Part 2.1   Deliberation
;-------------------------------------------

(defun deliberation(premises task &optional sys1-concl)
   "top level system 2 -> delib-follows or delib"
   (let ((all-but-last (reverse (rest (reverse premises))))(last-premise (first (last premises))))
   (setf *system* 2)
   (cond((equal task 'what-follows?)(delib-follows premises sys1-concl))
        ((null task)
            (trc "System 2" (format nil "The premises yield the models: ~A" (build-models premises))))
        (t  (delib all-but-last last-premise task)))))

(defun delib(all-but-last last-premise task)
  "builds models for premises and conclusion, and calls conclude unless they have no atoms in common "
  (let* ((premise-models (build-models all-but-last))(conclusion-models (build-models (list last-premise))))
    (if (common-atms all-but-last (first (list last-premise)))
        (conclude premise-models conclusion-models last-premise task)
      (trc "System 2" (format nil "Conclusion has no propositions in the premises: it's INDEPENDENT.")))))

(defun and-system-2(mod1 mod2 &optional models1 models2)
  "ands models, which may be wholly or partly implicit, to rtn fems, models1 and models2 needed for and-lis"
  (let* ((exps1 (explicit-part mod1))(imps1 (includes-implicit mod1))   
         (exps2 (explicit-part mod2))(imps2 (includes-implicit mod2))
         (expout (output exps1 exps2 imps1 imps2))
         (impout (output (eval imps1)(eval imps2) exps1 exps2))
         (finout (final-exp-models expout impout exps1 exps2 imps1 imps2)))
  (part-imp-output expout finout))) 

(defun output(exps1 exps2 imps1 imps2)
  "applies and-explicit to explicit models"
  (cond( exps1 
         (cond( exps2 (and-lis #'and-explicit exps1 exps2))
              ( imps2 exps1)))
       ( imps1 
         (cond( exps2 exps2)))))

(defun final-exp-models(expout impout exps1 exps2 imps1 imps2)
  "combines explicit output with implicit output => explicit models, or nil if contradiction "
  (cond( expout
         (cond( impout (and-lis #'and-explicit expout impout))   
              ((and imps1 imps2) nil)  
              (t expout)))
       ((and exps1 exps2) nil)    
       (impout impout)))

(defun part-imp-output(expout finout)
  "computes final footnote, subtracting expout from finput to yield one or disjunction of entries"
  (let ((imp-inf nil)(anonymous nil))
    (cond((setf imp-inf (rem-itms-from-models (car expout) finout))
              (setf anonymous (gentemp))
              (set anonymous imp-inf)
              (list (append-mods (car expout)(car (modelize anonymous)))))
         (t finout))))

(defun and-explicit(mod1 mod2 &optional models1 models2)
  "joins two fems rtns models"
  (let (outmod)
    (if (setf outmod (join-explicits mod1 mod2))
        (list outmod))))
      
(defun join-explicits(mod1 mod2)
  "joins two fems without redundancy, rtning nil if one has item negating itm in other"
  (let (output)
    (dolist(m1 mod1 output)
      (cond((match m1 mod2)(setf output (append output (list m1)) mod2 (remove-itm m1 mod2)))
           ((match (negate m1) mod2)(return (setf output nil)))
           (t (setf output (append output (list m1))))))
    (if output (append output mod2))))

;----------------------------------------------------------------------
; Part 2.2 Assesses system 1 conclusions and draws its own
;----------------------------------------------------------------------

(defun delib-follows(premises sys1-concl)
  "calls delib 'necessary or 'possible to assess sys1-concl then calls delib-draws-concl"
  (let ((premises (reorder-premises premises))(categoricals (find-categoricals premises)) 
         sys2-output)
    (trc "        " (format nil "to test system 1's response: ~A" sys1-concl))
    (cond((listp sys1-concl)
            (if (equal (car sys1-concl) 'nec)
                (setf sys2-output (delib premises (cadr sys1-concl) 'necessary?))
              (setf sys2-output (delib premises (cadr sys1-concl) 'possible?)))
            (if (equal sys2-output 'Yes)
                (trc "System 2" (format nil "So, system 1's conclusion is correct."))
              (trc "System 2" (format nil "So, system 1's conclusion is wrong: it is an ILLUSION."))))
         ((equal sys1-concl 'contradiction)
              (trc "System 2" (format nil "System 1 drew no conclusion from contradiction.")))
         (t (trc "System 2" (format nil "System 1 drew no conclusion."))))
  (delib-draws-concl premises categoricals)))

(defun delib-draws-concl(premises categoricals)
  "deliberation draws its own concl"
  (trc "System 2" (format nil "draws its own conclusion now."))
  (let((models (build-models premises)) constant-models concl)
    (cond((null models)(trc "System 2" (format nil "Contradictory premises: NOTHING FOLLOWS.")) 'Contradiction)
         ((tautologyp models)(trc "System 2" (format nil "Tautology: NOTHING FOLLOWS.")) 'Tautology)
         ((setf constant-models (find-constant-itms models categoricals)) ; rtns constants other than categoricals!
              (setf concl (describe-m constant-models))  
              (trc "System 2" (format nil "i. A conclusion that is NECESSARY is: ~A" concl)) concl)
         ((setf concl (end-terms-concl premises (remove-categoricals-from-models categoricals models)))
             (if (not (equal concl 'nvc))
                 (trc "System 2" (format nil "ii. A conclusion that is NECESSARY is: ~A" concl)))
             concl)
         (t  (trc "System 2" (format nil "Inference is too complex to draw a NECESSARY concl") 'NVC)))))

(defun end-terms-concl(premises models)
   "draw conclusion between end terms in premises"
  (let ((non-end-terms (find-non-end-terms premises)))
        (dolist (term non-end-terms)
          (setf models (rem-itms-from-models (list term) models))
          (setf models (rem-itms-from-models (list (neg term)) models)))
        (desc-connective (remove-dups models))))

(defun find-non-end-terms(premises)
  "rtns the non-end terms from premises including e.g. ((A)(B)) and ((not A))"
  (let (list-of-items output)
    (dolist(prem premises list-of-items)
      (setf list-of-items (append list-of-items (make-lis-of-items prem))))
    (prog()
      loop
      (cond((null list-of-items)(return output))
           ((or (member-lis (car list-of-items)(cdr list-of-items))
                (member-lis (neg (car list-of-items))(cdr list-of-items)))
                (setf output (cons (car list-of-items) output))))
      (setf list-of-items (cdr list-of-items))(go loop))))

(defun make-lis-of-items(prem)
  "makes list of itms from prem"
  (let (list-of-items)
  (prog(itm)
    loop   
    (cond((null prem)
             (return list-of-items))
         (t  (setf itm (car prem))
             (cond((equal (caar (shift itm *lexicon*)) 'var)
                       (setf list-of-items (cons (list itm) list-of-items)))
                  ((and (equal itm 'not)(equal (caar (shift (cadr prem) *lexicon*)) 'var))
                         (setf list-of-items (cons (list '- (cadr prem)) list-of-items)) 
                         (setf prem (cdr prem))))
             (setf prem (cdr prem))(go loop))))))

(defun desc-connective (models)
  "finds appropriate connective to describe n models of two items, where 4 > n"
  (let ((itm1 (affirm (caar models)))(itm2 (affirm (cadar models))) concl)
    (cond((or (null models)(tautologyp models))
            (trc "System 2" (format nil "NOTHING FOLLOWS of necessity.")) 'nvc)
         ((setf concl (describe-m (find-constant-itms models)))
            (trc "System 2" (format nil "A conclusion that is NECESSARY is: ~A" concl)) concl)
         ((> (length (car models)) 2)
            (setf concl (describe-m (first models)))
            (trc "System 2" (format nil "Premises are complex. A conclusion that is POSSIBLE is: ~A" concl)) concl)
         ((= (length models) 2)
            (setf concl (append  '(iff) itm1 '(then) itm2))
            (if (matchmodelsets (parse concl) models)
                concl
              (append itm1 '(ore) itm2)))
         (t (find-three-model-conn itm1 itm2 models)))))

(defun find-three-model-conn(itm1 itm2 models)
  "given three models of two itms rtns appropriate conclusion"
  (let (conclusions)
    (setf conclusions (list (append '(if) itm1 '(then) itm2)
                            (append '(if) itm2 '(then) itm1)
                            (append '(if) itm1 '(then) (negate-premise itm2))
                            (append itm1 '(or) itm2)))
    (dolist(concl conclusions)
      (if (matchmodelsets (parse concl) models)
          (return concl)))))

;-------------------------------------------------------------------------------------------
; Part 3. SHARED FUNCTIONS FOR INTUITION AND DELIBERATION (SYSTEMS 1 AND 2)
;-------------------------------------------------------------------------------------------

;----------------------------------
; Part 3.1 Build models
;----------------------------------

(defun build-models(premises)
  "rtns models of premises, sensitive to system"
  (let (models premise-models new-models)
    (dolist(prem premises models) 
      (setf premise-models (check-models (parse prem)))
      (trc "Language" (format nil " Parsing premise:"))
      (print-sentence prem)
      (setf premise-models (MODULATION prem premise-models))  
      (trace-models " Constructs models:" premise-models)
      (if (equal *system* 2) 
          (assess-premise premise-models prem))
      (setf (num-models *tracer*)(+ (count-up premise-models)(num-models *tracer*)))
      (cond((null models)(setf models premise-models))
           (t (setf new-models (check-models (and-ing models premise-models)))
              (trace-models " Model of conjunction of premises:" new-models)
              (setf models new-models)
              (setf (num-models *tracer*)(+ (count-up new-models)(num-models *tracer*))))))                             
    (setf (response *tracer*) models)
    models))

(defun assess-premise (models prem)
  "called only by build-models in system 2 detects contradictions and tautologies"
  (cond((null models)(trc "System 2" (format nil "This premise is a self-contradiction: ~A." prem)))
       ((or (eq models t)(tautologyp models))
                     (trc "System 2" (format nil "This premise is a tautology: ~A." prem)))))

(defun check-models(models)
  "tidy-up removes dups, removes implicit models if number of explicit models is at least 1/2 size of partition"
  (let* ((outmodels (tidy-up models))(no-of-atms (length (remove-implicit (findatms outmodels)))))
    (if (> (length outmodels)(/ (expt 2 no-of-atms) 2))
        (remove-impl outmodels)
      outmodels)))

(defun tidy-up(models)
  "puts models into standard order, and removes duplicates if any, but not if their footnotes differ"
  (if (truth-valuep models) 
      models
     (imp-to-end (re-arranging (remove-dups-amalgamate-fns models)))))

(defun imp-to-end(models)
  "puts wholly-implicit models at the end of the models"
  (cond((null models) nil)
       ((wholly-implicit (car models))(append (imp-to-end (cdr models))(list (car models))))
       (t (cons (car models)(imp-to-end (cdr models))))))

(defun re-arranging(models)
  "puts each model into same order as template from findatms"
  (let ((template (findatms models)))
    (re-arranges template models)))

(defun re-arranges(template models)
  "puts each model into the same order as the template"
  (cond((null models) nil)
       ((wholly-implicit (car models))(cons (car models)(re-arranges template (cdr models))))
       (t (cons (re-arr template (car models))(re-arranges template (cdr models))))))

(defun re-arr(template mod)
  "puts items in a model in same order as template"
  (let ((atm nil))
    (cond((null template) nil)
         ((setf atm (match-literal (car template) mod)) ; rtns atm corresponding to literal
            (cons atm (re-arr (cdr template)(remove-itm atm mod))))
         (t (re-arr (cdr template) mod)))))

(defun common-atms(premises concl)
  "(common-atms '((if a and c then not b)(a)) '(b and c)) => (C B)"
  (let (output)
  (dolist (itm concl output)
    (dolist (prem premises)
      (if (and (equal 'var (caar (shift itm *lexicon*)))(member-lis itm prem))
          (setf output (cons itm output)))))
  output))

(defun reorder-premises(premises)
  "moves categoricals to front and then puts remainder into co-referential order, rejects isolated premises"
  (let* ((categoricals (find-categoricals premises))(compounds (rem-premises categoricals premises)) new-premises)
    (if (or (null premises)(= (length premises) 1))
        premises
    (prog(com)
      loop
      (setf com (first compounds))
      (if (null com)
          (return (append categoricals new-premises))
        (cond((or (common-atms categoricals com)(common-atms new-premises com))
                 (setf new-premises (append new-premises (list com)))
                 (setf compounds (cdr compounds))(go loop))
             ((common-atms (cdr compounds)(car compounds))
                 (cond(categoricals
                         (setf compounds (append (cdr compounds)(list (car compounds)))))
                      ((or (null new-premises)(common-atms new-premises (list (car compounds))))
                         (setf new-premises (append new-premises (list (car compounds))))
                         (setf compounds (cdr compounds)))
                      (t (setf compounds (append (cdr compounds)(list (car compounds))))))
                 (go loop))  
             (t (trc "System 1" (format nil "This premise has nothing in common with any of the others: ~A" com))
                (setf compounds (cdr compounds))(go loop))))))))

(defun find-categoricals (premises)
  "finds categorical premises"
  (let (cats)
  (dolist(prem premises)
    (if (categoricalp prem)
        (if (common-atms (remove-itm prem premises) prem) 
            (setf cats (append cats (list prem))))))
  cats))

(defun remove-categoricals-from-models(categoricals models)
  "removes each itm in categoricals from each model in which it occurs, sensitive to polarity"
  (let (outmodels)
    (setf categoricals (trans-premises categoricals))
    (dolist(mod models outmodels)
      (setf outmodels (append outmodels (rem-itms-from-models categoricals (list mod)))))))

(defun trans-premises(premises)
  "translates conjunctive premises into list of items"
  (let (outprems)
    (dolist(prem premises)
      (setf outprems (append outprems (trans-pre prem))))
    outprems))

(defun trans-pre (premise)
  "translates 'not into '-, and deletes 'and"
  (cond((null premise) nil)
       ((equal (car premise) 'not)(cons (list '- (cadr premise))(trans-pre (cddr premise))))
       ((equal (car premise) 'and)(trans-pre (cdr premise)))
       (t (cons (list (car premise))(trans-pre (cdr premise))))))

(defun rem-itms-from-models(mod1 models2)
  "removes each item in mod1 from each model in models, using rem-mod1-from-mod2"
  (apply #'append                  
         (mapcar #'(lambda(md) 
                     (cond((setf md (rem-mod1-from-mod2 mod1 md))(list md)))) 
                 models2)))                                                 

(defun rem-mod1-from-mod2(mod1 mod2)
  "removes each item in mod1 from mod2"
  (if (null mod1) 
      mod2
    (rem-mod1-from-mod2 (cdr mod1)(rem-atom (car mod1) mod2))))

(defun find-constant-itms(models &optional categoricals)
  "removes categoricals from models, and rtns list of all itms constant over all models"
  (let((models (remove-categoricals-from-models categoricals models)) out-itms)
    (cond((> (length models) 1)
             (setf out-itms (car models) models (cdr models))
             (dolist(mod models)
               (setf out-itms (find-constant out-itms mod))))
         (t (setf out-itms (car models))))
    out-itms))
        
(defun find-constant(const-itms model)
  "rtns a list of those items in const-itms that occur in model"
  (let (out-itms)
      (dolist(itm const-itms out-itms)
        (if (item-in-models itm (list model))
            (setf out-itms (append out-itms (list itm)))))  
   out-itms))

;----------------------------------------
; Part 3.2 Describe a model
;----------------------------------------

(defun describe-m(model)
  "describes a model, uses prog to insert 'and before last itm in mod"
  (prog(descn)
    loop
    (cond((null model) (return (dehyphenate descn)))
         ((= (length model) 1) (setf descn (append descn (describe-item (car model)))))
         (t (setf descn (append descn (describe-item (car model))(list 'and)))))
    (setf model (cdr model))
    (go loop)))

(defun describe-item(item)
  "converts itm in model to descn"
  (if (equal (car item) '-)
      (list 'not (cadr item))
    item))

(defun categoricalp(prem)
  "rtns prem iff it is a categorical or conjunction"
  (if (not (or (and (member 'or prem)(not (equal (car prem) 'not)))  ; no use
               (member 'or prem)
               (member 'ore prem)
               (member 'if prem)
               (member 'iff prem)))
          prem))

;----------------------------------------
; Part 3.3:  Conclude
;----------------------------------------

(defun conclude(premise-models conclusion-models concl task)
  "calls verification or probability or formulates conclusion"
  (let ((sys (case *system* (1 "System 1") (2 "System 2") (otherwise "Control")))
        (new-mods (tidy-up (and-ing premise-models conclusion-models))) output)
    (trace-models "Premise models are:" premise-models)
    (trace-models "Current models are: " conclusion-models)
    (cond((equal task 'verify?)
          (setf output (verification premise-models conclusion-models))
          (trc sys (format nil "The evidence shows that the premise is: ~A" output)))
         ((equal task 'probability?)
          (setf output (probability premise-models conclusion-models))
          (trc sys (format nil "The probability of the conclusion given the premises is: ~A" output)))
         ((null new-mods)
          (trc sys "IMPOSSIBLE conclusion that contradicts the premises (inconsistent).") 
          (setf output (modeling-output 'contradiction task)))
         ((matchmodelsets premise-models conclusion-models)
          (trc sys (format nil "NECESSARY conclusion follows from premises, and vice versa: ~A" concl))
          (modeling-output 'Valid task))
         ((equal 'all-in-all (all-in-all premise-models conclusion-models))
          (trc sys (format nil "NECESSARY conclusion follows from premises: ~A" concl))
          (setf output (modeling-output 'Valid task)))
         ((equal 'some-in-all (all-in-all premise-models conclusion-models))
          (trc sys (format nil "POSSIBLE conclusion follows from premises, weakly valid: ~A" concl)) 
          (setf output (modeling-output 'Weakly-valid task)))
         ((equal 'all-in-some (all-in-all premise-models conclusion-models))
          (trc sys (format nil "POSSIBLE conclusion follows from premises: ~A" concl))
          (trc sys "But a possibility is unsupported by premises (though valid in logic).")
          (setf output (modeling-output 'Unsupported-possibility task)))
         (t   (trc sys (format nil "POSSIBLE conclusion follows from premises, consistent: ~A" concl))
              (setf output (modeling-output 'Possible task))))
    (setf (response *tracer*) (when (symbolp output) (symbol-name output)))
    output
))

(defun modeling-output(conclusion task)
  (let ((gamma-probability (random-probability)))
  (case task
         ('what-follows? conclusion)
         ('necessary?   (if (or (equal conclusion 'valid) 
                                (and (equal conclusion 'weakly-valid)(> gamma-probability *gamma*)))
                               'Yes
                           (if (equal conclusion 'contradiction)
                               'Contradiction
                             'No)))                
         ('possible?     (if (equal conclusion 'contradiction) 
                             'No
                           'Yes)))))      

(defun count-up(mods)
  "counts up all explicit models, ignores implicit models"
  (cond((null mods) 0)
       ((equal mods 'nvc) 0)
       ((eq mods t) 1) ; where mods = t, then rtns 1
       ((wholly-implicit (car mods)) 0)
       (t (+ 1 (count-up (cdr mods))))))

(defun all-in-all(mods1 mods2)
  "removes each mod in mods2 as it finds support for it in mods1 rtng 'all-in-all, 'some-in-all, or all-in-some
   wrt to the possibilities in premise mods1 and concl mods2"
  (let ((outmods1 mods1)(outmods2 mods2))
    (dolist (m1 mods1)
      (dolist (m2 mods2)
        (cond((matchl m2 m1)
                 (setf outmods1 (remove-mod m1 outmods1))
                 (setf outmods2 (remove-mod m2 outmods2))))))
    (if (null outmods2) 
        (if (null outmods1) 
            'all-in-all
          'some-in-all)
      (if (null outmods1) 
          'all-in-some))))

;---------------------------------------------------------------------------------
; Part 3.4: VERIFICATION, the construction of counterfactuals, and probabilities
;---------------------------------------------------------------------------------

(defun verification (premise-mods evidence-mods)
  "verifies premise given evidence, calling counterfactuals in system 2, and yielding 'undetermined in system 1"
  (let ((evidence-mods (discrepant-evidence evidence-mods premise-mods)))
    (cond((null evidence-mods) 'undetermined)
         ((matchmodelsets evidence-mods premise-mods) 'definitely-true)
         ((matchmod (first evidence-mods) premise-mods)
           (cond((= *system* 2)
                    (counterfactual evidence-mods (remove-mod evidence-mods premise-mods)) 'possibly-true)
                (t 'true)))
         ((and (= *system* 1)(not (equal (first evidence-mods)(first (negate premise-mods)))))
                    'undetermined)
         (t 'false))))

(defun discrepant-evidence(evidence-mods premise-mods)
  "deals with cases in which e-mod longer than p-mod, and converse"
  (let ((sys (case *system* (1 "System 1") (2 "System 2") (otherwise "Control"))))
        (cond((or (equal evidence-mods 'nvc)
                  (> (length (first premise-mods))(length (first evidence-mods))))
                 (trc sys "Evidence lacks information in previous assertions") nil)
             (t (list (remove-literals-from-model (first evidence-mods) premise-mods))) )))

(defun remove-literals-from-model(model models) 
  "finds literals in model that are in models, and rtns model with only them"
  (let (outmodel)
    (dolist(lit model outmodel)
      (if (find-literal-in-models lit models)
          (setf outmodel (append outmodel (list lit)))))))
   
(defun remove-mod (mod lis)
  "removes model or itm, which must be a list, from lis-of-lis"
  (cond((null lis) nil)
       ((matchlists mod (car lis))(remove-mod mod (cdr lis)))
       (t (cons (car lis) (remove-mod mod (cdr lis))))))
  
(defun counterfactual(evidence-mods remaining-models)
  "Calls construct-counterfactuals which is kept separate so that explanations can call it."
  (cond((or (= *system* 1)
            (null remaining-models)) 'true)
       (t (trc "System 2" "The evidence shows that the previous premise could be true.") 
          (trc "System 2" (format nil "It would be given the following conditions:")) 
          (print-sentence (construct-counterfactual (first evidence-mods) remaining-models))
           'true)))

(defun construct-counterfactual(e-mod remaining-models)
  "Chief fn for constructing counterfactuals by comparing literals in evidence-model (e-mod) with those in the
   remaining models of a compound assertion. 'possible elicits 'might' and 'necessary' elicits 'would'."
  (let* ((antec (first e-mod))(neg-antec (negate antec))(conseq (second e-mod))(neg-conseq (negate conseq)) 
         output)
    (if (contained (list antec neg-conseq) remaining-models) 
           (setf output (semifactual conseq antec)))
    (if (contained (list neg-antec) remaining-models)
           (setf output (append output (antec-of-cfactual neg-antec))))
    (if (contained (list neg-antec neg-conseq) remaining-models)                   
          (if (contained (list neg-antec conseq) remaining-models)
              (setf output (append output (conseq-of-cfactual conseq 'possible)))
            (setf output (append output (conseq-of-cfactual conseq 'necessary))))
        (if (contained (list neg-antec conseq) remaining-models)                    
            (setf output (append output (conseq-of-cfactual neg-conseq 'necessary)))))
    (dehyphenate output)))

(defun contained (fact-mod premise-models)
  "determines that one mod contained in set of models"
  (dolist (prem-mod premise-models)
    (if (matchl fact-mod prem-mod)
        (return prem-mod))))

(defun antec-of-cfactual(neg-antec)
  "constructs antecedent clause of counterfactual"
  (let ((antec (list 'if (first(last neg-antec)))))
    (if (negatep neg-antec)
        (append antec '(had not occurred then))
      (append antec '(had occurred then)))))

(defun negatep(item)
  (if (= (length item) 1)
      nil
    t))

(defun semifactual (conseq antec)
  "For describing clause of the sort B might have occurred even though A did not occur"
  (let ((clause (list (first(last conseq)))))
  (if (negatep conseq)
      (setf clause (append clause '(might have occurred even though)(remove-hyphens (first (last antec)))))
    (setf clause (append clause  '(might not have occurred even though)(remove-hyphens (first (last antec))))))
  (if (negatep antec)
      (append clause '(did not occur and))
    (append clause '(occurred and)))))

(defun conseq-of-cfactual (conseq mode)
  "For describing the then-clause of a counterfactual"
  (let ((output (remove-hyphens (first(last conseq)))))
  (cond((equal mode 'possible)
           (if (negatep conseq)
               (setf output (append output '(might have occurred)))
             (setf output (append output '(might not have occurred))))) 
       ((equal mode 'necessary)
           (if (negatep conseq)
               (append output '(would have occurred))
             (append output '(would not have ocurred)))))))

(defun probability(earlier-mods prem-mods)
  "makes models explicit, deals with more prem-models than earlier-mods, and prem-mods with atms not in earlier-mods,
   then rtns numerator divided by denominator"
  (let (denominator (numerator 0)(prob 1.0))
    (setf prem-mods (flesh-out(make-explicit prem-mods)) earlier-mods (flesh-out(make-explicit earlier-mods)))
    (setf denominator (length earlier-mods))
    (cond((or (> (length prem-mods)(length earlier-mods))
              (> (length (first prem-mods))(length (remove-literals-from-model (first prem-mods) earlier-mods))))
                   'undetermined)  ; prem-mods contains literal not in earlier-mods
         ((matchmodelsets earlier-mods prem-mods) 1)
         (t (dolist(p-mod prem-mods numerator)
               (dolist (mod earlier-mods)
                 (if (matchl p-mod mod)
                     (setf numerator (+ 1 numerator)))))
             (setf prob (/ numerator denominator))))))


;----------------------------------------------------------------------------------------------------------------------
;                         MODULATION, DEFEASANCE (nonmonotonic reasoning), and knowledge
;----------------------------------------------------------------------------------------------------------------------

;-----------------------------------------
; Part 4.1: Knowledge base for modulation
;-----------------------------------------

(setf *knowledge* '(
      
    (((Louvre-in-Paris)))  ; a fact

    (((Big-Ben-in-London)))

    (((  close)(   near))  ; synonyms to illustrate iff
     ((- close)(-  near))
     ((- close)(   far))
     ((  close)(-  far)))

    (((  novel)  (  Don-Quixote))                 
     ((  novel)  (- Don-Quixote))
     ((- novel)  (- Don-Quixote)))

    (((  raining)(  pouring))                 
     ((  raining)(- pouring))
     ((- raining)(- pouring)))

    (((  triangle)(  has-three-sides))
     ((- triangle)(- has-three-sides)))

    (((  square)  (  has-four-sides))
     ((- square)  (  has-four-sides))
     ((- square)  (- has-four-sides)))

; gun example for defeasance --------------------------------------------------------------------------
    (((  pull-trigger)                          (  gun-fires))
     ((- pull-trigger)                          (- gun-fires))
     ((- pull-trigger)   (- enough-strength)    (- gun-fires)))

    (((  pull-trigger)   (- bullets-in-chamber) (- gun-fires))  
     ((  pull-trigger)   (  gun-jams)           (- gun-fires))
     ((  pull-trigger)   (  gun-broken)         (- gun-fires))
     ((  pull-trigger)   (  safety-is-on)       (- gun-fires)))

    (((  unloads-gun)             (- bullets-in-chamber))
     ((- unloads-gun)             (  bullets-in-chamber)))

    (((loads-gun)                 (  bullets-in-chamber))
     ((- loads-gun)               (- bullets-in-chamber))
     ((- loads-gun)               (  bullets-in-chamber)))

    ((( gun-is-old)               (gun-jams))
     ((- properly-maintained-gun) (gun-jams))
     ((  wrong-caliber-bullets)   (gun-jams)))

    ((( gun-dropped)              (gun-broken))
     (( gun-manhandled)           (gun-broken))
     (( gun-misfired)             (gun-broken)))

    ((( owner-prudent)            (safety-is-on))
     (( user-switched-it-on)      (safety-is-on))
     (( straight-from-factory)    (safety-is-on)))

    (((- enough-strength)         (- pull-trigger))
     ((  enough-strength)         (  pull-trigger)))

    (((  partial-paralysis)       (- enough-strength))
     ((- partial-paralysis)       (  enough-strength)))

    ((( viral-infection)          (partial-paralysis)))

; snake-bite example for defeasance -------------------------------------------------------------------
    (((  a-poisonous-snake-bites-her)                                   (  she-dies))
     ((- a-poisonous-snake-bites-her)                                   (- she-dies))
     ((- a-poisonous-snake-bites-her)(the-snake-has-a-weak-jaw)         (- she-dies))
     ((- a-poisonous-snake-bites her)(the-snake-is-blind)               (- she-dies)))

    (((  a-poisonous-snake-bites-her) (she-takes-antidote)              (- she-dies))  
     ((  a-poisonous-snake-bites-her) (the-tourniquet-blocks-the-poison)(- she-dies)) 
     ((  a-poisonous-snake-bites-her) (someone-sucks-out-the-poison)    (- she-dies))
     ((  a-poisonous-snake-bites her) (its-venom-lacks-potency)         (- she-dies)))

    (((  she-anticipates-bite)        (  she-takes-antidote))
     ((- she-anticipates-bite)        (- she-takes-antidote)))

    (((  she-uses-a-tourniquet)       (  the-tourniquet-blocks-the-poison))
     ((- she-uses-a-tourniquet)       (- the-tourniquet-blocks-the-poison)))

    (((  someone-knows-what-to-do)    (  someone-sucks-out-the-poison))
     ((- someone-knows-what-to-do)    (- someone-sucks-out-the-poison)))

    (((  the-snake-has-a-disease)     (  its-venom-lacks-potency))
     ((- the-snake-has-a-disease)     (- its-venom-lacks-potency)))

    (((  the-snake-is-tired)          (  the-snake-has-a-weak-jaw))
     ((- the-snake-is-tired)          (- the-snake-has-a-weak-jaw)))

    (((  the-snake-is-diseased)       (  the-snake-is-blind))
     ((- the-snake-is-diseased)       (- the-snake-is-blind)))

;-------------------------------------------------------------------------------------------------

    ;;; partial examples
    (((  match-soaked)(- match-lights))
     ((- match-soaked)(- match-lights))
     ((- match-soaked)(  match-lights)))

    (((day)(  Monday)(- Tuesday)(- Wednesday)(- Thursday)(- Friday)(- Saturday)(- Sunday))
     ((day)(- Monday)(  Tuesday)(- Wednesday)(- Thursday)(- Friday)(- Saturday)(- Sunday))
     ((day)(- Monday)(- Tuesday)(  Wednesday)(- Thursday)(- Friday)(- Saturday)(- Sunday))
     ((day)(- Monday)(- Tuesday)(  Wednesday)(  Thursday)(- Friday)(- Saturday)(- Sunday))
     ((day)(- Monday)(- Tuesday)(  Wednesday)(- Thursday)(  Friday)(- Saturday)(- Sunday))
     ((day)(- Monday)(- Tuesday)(  Wednesday)(- Thursday)(- Friday)(  Saturday)(- Sunday))
     ((day)(- Monday)(- Tuesday)(  Wednesday)(- Thursday)(- Friday)(- Saturday)(  Sunday)))

    (((  below-belt)(  disqualified))
     ((- below-belt)(- disqualified))
     ((- below-belt)(  disqualified)))

    (((  in-Rio)(  in-Brazil))
     ((- in-Rio)(  in-Brazil))
     ((- in-Rio)(- in-Brazil)))


    (((blow)(strong-skull)   (  cushions-brain))
     ((blow)(- strong-skull) (  cushions-brain))
     ((blow)(- strong-skull) (- cushions-brain)))

    (((  cushions-brain)                        (- forgets))
     ((  cushions-brain)                        (  forgets))
     ((- cushions-brain)                        (  forgets)))

    (((insulted)(- meant-to-insult) (- angry))
     ((insulted)(- lacks-respect)   (- angry))
     ((insulted)(very-calm)         (- angry)))

    (((  sane)(  meant-to-insult))
     ((  sane)(- mean-to-insult))
     ((- sane)(- meant-to-insult)))

    (((considered-stupid)(lacks-respect)))

    (((Buddhist)(very-calm)))

    (((got-flu) (feeling-weak))
     ((no-sleep)(feeling-weak))
     ((viral-infection)(feeling-weak)))

    (((  follows-diet)                             (  loses-weight))
     ((- follows-diet)                             (- loses-weight)))

    (((  follows-diet) (is-already-at-ideal-weight)(- loses-weight))
     ((  follows-diet) (stops-exercising)          (- loses-weight)))

    (((  eats-this-dish)                           (  gets-indigestion))
     ((- eats-this-dish)                           (- gets-indigestion)))

    (((  eats-this-dish)(has-strong-stomach)       (- gets-indigestion))
     ((  eats-this-dish)(takes-digestive-pill)     (- gets-indigestion)))

    (((  does-aerobics)                            (  strengthens-heart))
     ((- does-aerobics)                            (- strengthens-heart)))

    (((  does-aerobics)(exercises-sloppily)        (- strengthens-heart))
     ((  does-aerobics)(continues-to-smoke)(heart-impervious-to-exercises)(- strengthens-heart))
     ((  does-aerobics)(heart-is-damaged) (- strengthens-heart)))

    (((viral-disease) (heart-damaged)))

    (((  God-exists)(  atheism-is-wrong))
     ((- God-exists)(- atheism-is-wrong))

     ((  God-exists)(- atheism-is-right))
     ((- God-exists)(  atheism-is-right)))

    ))
 
;-----------------------
; Part 4.2: MODULATION
;-----------------------

(defun modulation(premise prem-mods)
  "if relevant knowledge modulates a premise's models yield different possible outcomes, including a priori 
   truth or falsity."
  (let* ((ment-mod (first prem-mods))(refute-models (make-refuters prem-mods))
         (known-mods (find-mods-in-k-from-atms ment-mod))(known-refuting-mods (find-models-in-knowledge refute-models))
         out-models)
    (cond((and (= *system* 2) known-mods)
                (setf out-models (evaluate-modulation prem-mods known-mods known-refuting-mods))
                (cond((not(matchmodelsets prem-mods out-models))
                        ; (trace-models " Finds relevant models in knowledge:" known-mods)  ; uncomment to see models
                         (trc "Modulation" (format nil " Knowledge changes models of premise: ~A" premise))))
                out-models)
         (t prem-mods))))
     
(defun evaluate-modulation(prem-mods known-mods known-refuting-mods)
  "yields 5 sorts of effect of modulation from knowledge"
  (let (facts out-models)
    (cond((setf facts (find-fact (first prem-mods)))                                         
            (trc "Modulation" (format nil "Shows that the following proposition is true a priori"))
            (setf out-models (and-ing (list facts) prem-mods))  
            out-models)                                                                                  
         ((or (and (null known-mods)(null known-refuting-mods))
              (not (relevant-knowledge (first prem-mods) known-mods)))
            prem-mods)                                                                                   
         ((setf out-models (members-common-to-lists prem-mods known-mods))
            (cond((matchmodelsets prem-mods known-mods)
                     (trc "Modulation" "Shows that premise is true a priori."))
                 (known-refuting-mods
                     (trc "Modulation" "Changes the interpretation."))                       
                 (t  (trc "Modulation" "Changes the interpretation but shows that premise is true a priori."))) 
            out-models)
         ( known-mods (trc "Modulation" "Shows that premise is false a priori.") 
                      (relevant-knowledge (first prem-mods) known-mods) nil)                             
         (t  prem-mods))))                                                                              

(defun relevant-knowledge(p-model known-mods)
  "rtns each p-model that occurs as part of whole of a k-model"
  (let ((allpos-p-mods (allpos p-model)) output)
  (dolist(k-mod known-mods output)
    (dolist(pos-p-mod allpos-p-mods)
      (if (matchl k-mod pos-p-mod)
          (setf output (append output (list pos-p-mod))))))))

(defun members-common-to-lists(lis1 lis2)
  "rtns itms or lists in lis1 that occur in lis2"
  (let (outlis)
  (dolist (memb lis1 outlis)
    (if (matchmod memb lis2)
        (setf outlis (append outlis (list memb)))))))

(defun find-fact(model)
  "finds any atms or negations thereof in model that are facts in *knowledge*, i.e., constant in all occurrences there"
  (let (output)
  (dolist(atm model output)
    (cond((and (find-mod-in-knowledge (list atm))
               (not (find-mod-in-knowledge (list (negate atm)))))
             (setf output (append output (list atm))))
         ((and (find-mod-in-knowledge (list (negate atm)))
               (not (find-mod-in-knowledge (list atm))))
             (setf output (append output (list (negate atm)))))))))

(defun find-mods-in-k-from-atms(model)
  "makes a list of all atoms in model, and rtns all models in knowledge containing them"
  (let (models known-mods k-mod)
    (dolist(itm model models)
       (setf models (append models (allpos (list itm)))))
    (dolist(mod models known-mods)
      (if (setf k-mod (find-mod-in-knowledge mod))
          (setf known-mods (append known-mods k-mod))))
    (remove-dups known-mods)))  

(defun make-refuters(fems)
  "constructs list of itms that refute fems, but if they include a constant itm rtns its negation
   ex (make-refuters '((A)(- B)(- A)(- B))) => ((B)) "
  (let (neg-const-lit)
  (cond((null fems) t)
       ((equal (length fems) 4) nil)
       ((equal (length fems) 1)(negate fems))
       ((setf neg-const-lit (neg-constant-literal fems)) neg-const-lit)
       (t (negate fems)))))
 
(defun neg-constant-literal(fems)
  "rtns list of negation of literals in fems that have constant value
   (neg-constant-literal '( ((A)(B))  ((- A)(B)))) => ((- B))"
  (let ((atms (findatms fems)) literal)
    (dolist (atm atms literal)
      (if (not (and (item-in-models atm fems)(item-in-models (neg atm) fems)))
          (if (item-in-models atm fems)
              (setf literal (append literal (list (neg atm))))
            (setf literal (append (list atm))))))))
               
(defun find-models-in-knowledge(models)
  "rtns each mod in models that is in knowledge"
  (let (models-in-knowledge)
    (if (or (equal models t)(null models))
        models
      (dolist(mod models models-in-knowledge)
        (if (find-mod-in-knowledge mod)
            (setf models-in-knowledge (append models-in-knowledge (list mod))))))))

(defun find-mod-in-knowledge(model)
  "rtns all models in knowledge that include model"
  (let (output)
  (dolist(kmodels *knowledge* output)
    (dolist(kmodel kmodels)
       (if (matchl model kmodel)
           (setf output (append output (list kmodel)))))) ; changed to kmodel from model
  output))

(defun footnotep(literal)
  (boundp (car (last literal))))

;--------------------------------------------------------------------------
; Part 4.3: DEFEASANCE: mismatch, explanation, and call to counterfactuals
;--------------------------------------------------------------------------

(defun defeasance(premises)
  "defeasance only i/c of inconsistency; mismatching decides premise to reject; explain resolves inconsistency"
  (let((sys (case *system* (1 "System 1") (2 "System 2") (otherwise "Control"))) models-lis explanation)
    (trc "Defease" "Contradiction elicits defeasance in order to resolve it:-")
      (dolist (prem premises models-lis)
        (setf models-lis (append models-lis (list (parse prem)))))
      (cond((setf explanation (explain (caar (mismatching premises models-lis))))
              (trc "Defease" (format nil "The explanation is:"))
              (trc "" (format nil "~A" (dehyphenate(convert-concl-to-prem explanation))))
              (setf models-lis nil) ; reset for fems
              (defeasance-counterfactual explanation) t)
           (t (trc sys (format nil "Cannot resolve the contradiction."))
              'CONTRADICTION))))

(defun mismatching(premises models-lis)
  "determines which premise, if any, mismatches the fact in the last premise, and so should be rejected."
  (let ((fact (car (reverse premises)))             
        (premises (reverse(cdr(reverse premises))))
        (fact-mod (car (reverse models-lis)))
        (models-lis (reverse (cdr (reverse models-lis)))))
    (cond((= *system* 1)
            (trc "System 1" (format nil "i. Mental models find mismatch with a premise to revise it")))
         (t (trc "System 2" 
                 (format nil "i. Fully explicit models find mismatch with a premise to revise it"))))
    (trace-models "Model of the facts:" fact-mod)
    (revise premises fact (mism (car fact-mod) models-lis premises))))

(defun mism(fact-model mods-of-premises premises)
  "called by mismatching to determine which premise fact mismatches"
  (let (mismatch-premises)
  (dolist(prem-models mods-of-premises mismatch-premises)
    (cond((contained fact-model prem-models)
            (trc "Defease" (format nil "Model of the facts matches model of"))
            (trc " " (format nil "~A" (car premises)))
            (setf premises (rest premises)))
         ((conflict fact-model prem-models)
              (trc "Defease" (format nil "Model of the facts mismatches models of"))
              (trc " " (format nil "~A" (car premises)))
              (setf mismatch-premises (append mismatch-premises (list (list 'conflict (car premises)))))
              (setf premises (rest premises)))
         (t   (trc "Defease" (format nil "Model of the facts is not represented in models"))
              (trc " " (format nil "~A" (car premises)))
              (setf mismatch-premises (append mismatch-premises (list (list 'default-rej (car premises)))))
              (setf premises (rest premises)))))
  mismatch-premises))

(defun conflict(e-mod premise-models)
  "detects conflict between fact and the one explicit model of premises, so this premise is to be rejected" 
  (let ((neg-e-mod (car (negate (list e-mod)))))
    (cond((< (length neg-e-mod) 2) (contained neg-e-mod premise-models))
         ((and (contained (list (car e-mod)) premise-models)(contained (list(cadr e-mod)) premise-models))
                                   (not (contained e-mod premise-models))))))

(defun revise(premises fact reject-lis)
  "chooses premise-to-reject, adds fact to remaining-premises & rtns their models, and rtns models of
   premise-to-reject as counterfactuals"
  (let* ((premise-to-reject (which-to-reject premises reject-lis))       
         (remaining-premises (rem-premises premise-to-reject premises))  
         counterfactuals facts new-premises)
    (trc "Defease" (format nil "So, reject the premise: ~A" premise-to-reject))
    (trc "Defease" "Nonmonotonicity needs a model of the fact and the other premises.")
    (setf new-premises (append remaining-premises (list fact)))   
    (trc "Defease" (format nil "These new premises are:"))
    (trc ""  (format nil "~A" new-premises))
    (setf facts (build-models new-premises))
    (trc "Defease" "Their model needs to be explained.")
    (cond( premise-to-reject  
             (setf counterfactuals (first (build-models premise-to-reject)))
             (trc "Defease" (format nil "Model of facts is ~A" facts))
             (trc "Defease" (format nil "Counterfactuals are: ~A" counterfactuals))
             (list facts counterfactuals)))))

(defun rem-premises(to-reject premises)
  "removes to-reject premises from premises"
  (if (null to-reject) 
      premises
    (rem-premises (cdr to-reject)(remove-mod (car to-reject) premises))))

(defun which-to-reject(premises reject-lis)
  "rtns single premise to reject"
  (cond((= (counts 'conflict reject-lis) 1)   (find-rej 'conflict reject-lis))
       ((= (counts 'default-rej reject-lis) 1)(find-rej 'default-rej reject-lis))
       ((> (length premises)(length reject-lis)) reject-lis)))

(defun counts(label reject-lis)
  "counts number of premises prefaced by 'conflicts or by 'default-rej"
  (cond((null reject-lis) 0)
       ((equal (caar reject-lis) label)(+ 1 (counts label (cdr reject-lis))))
       (t (counts label (cdr reject-lis)))))

(defun find-rej(label reject-lis)
  "rtns premise labeled 'conflict or 'default-rej"
  (let ((to-reject nil))
    (dolist (premise reject-lis to-reject)
      (if (equal (car premise) label)(setf to-reject (cdr premise))))
    to-reject))

(defun explain(model-of-facts)
  "finds models in *knowledge* to explain the facts, and then once more to explain the explanation"
  (let ((explanation model-of-facts) potential-explanations)
    (if (= *system* 1)
        (trc "Defease" "ii. Mental models from knowledge used to resolve inconsistency")  
      (trc "Defease" "ii. Fully explicit models from knowledge used to resolve inconsistency"))
    (dotimes (i 2)
      (cond((setf potential-explanations (rem-itms-from-models model-of-facts (find-mod-in-knowledge explanation)))
                 (setf explanation (elt potential-explanations (random (length potential-explanations))))
                 (setf model-of-facts (append explanation model-of-facts)))
           (t (trc "Defease" "No relevant knowledge to explain the inconsistency.")
              (return t))))
    (append (list (third model-of-facts))(remove-mod (third model-of-facts) model-of-facts)))) 

(defun convert-concl-to-prem(conclusion)
  (cond((null conclusion) nil)
       ((equal conclusion '(nil)) nil)
       ((cdr conclusion)
          (append (convert-lit-to-clause (car conclusion))(cons 'and (convert-concl-to-prem (cdr conclusion)))))
       (t (append (convert-lit-to-clause (car conclusion))(convert-concl-to-prem (cdr conclusion))))))

(defun convert-lit-to-clause(lit)
  "converts (- a) to (not A)"
  (if (negatep lit)
      (list 'not (cadr lit))
    lit))

(defun defeasance-counterfactual(explanation)
  "calls construct-counterfactual with appropriate arguments after construction of explanation"
  (let ((c-factual (construct-counterfactual (list (second explanation)(first (last explanation)))
                                 (list (list (negate (second explanation))(negate (first (last explanation))))))))
    (trc "Defease " "iii. Counterfactual claim is:")
    (trc "" (dehyphenate c-factual))
    c-factual))
  
;------------------------------------------------------------------------------------------------------------------------
;                                          5. Low-level Functions
;------------------------------------------------------------------------------------------------------------------------

;--------------------------------------------
; Part 5.1: Models and their manipulation
;--------------------------------------------

(defun explicit(model)
  "t iff model is wholly explicit"
  (not(includes-implicit model)))

(defun explicit-part(mod)
  "rtns explicit-part of model"
  (if (setf mod (explic-part mod))
      (list mod)))

(defun explic-part(mod)
  "recurses to rtn explicit part"
  (if (or (null mod)(boundp(car(reverse(car mod)))))
      nil
    (cons (car mod)(explic-part (cdr mod)))))

(defun wholly-implicit(model)
  "rtns variable from wholly implicit model"
  (if (= (length model) 1)
      (includes-implicit model)))

(defun implicit(models)
  "rtns t iff models contain at least one part-implicit or wholly implicit model"
  (cond((truth-valuep models) nil) ; if recursion yields null models
       ((includes-implicit (car models)) t)
       (t (implicit (cdr models)))))

(defun partial(model)
  "if model is partly implicit rtns variable"
  (if (> (length model) 1)
      (includes-implicit model)))

(defun includes-implicit(model)
  "rtns variable from first implicit itm in model else nil"
  (if (not (atom model))
      (dolist(itm model)
        (if (boundp (first (last itm)))
            (return (first (last itm)))))))

(defun eval-simple-var(models)
  "where a set of models contains only a wholly implicit model, rtns its truth value if it has one"  
  (let ((var nil))
    (cond((eq models t) models)
         ((and (setf var (only-imp-mod models))(truth-valuep (eval var)))
                 (eval var))                         
         (t models))))

(defun only-imp-mod(models)
  "if models contains only a single model that is wholly implicit rtns its variable"
  (and (= (length models) 1)(wholly-implicit (car models))))

(defun remove-impl(mods)
  "rtns set of models minus the wholly implicit model wherever it is"
  (cond((null mods) nil)
       ((wholly-implicit (car mods))(remove-impl (cdr mods)))
       (t (cons (car mods)(remove-impl (cdr mods))))))

(defun find-literal-in-models(lit models)
  "rtns lit if it finds in models"
  (dolist(mod models)
    (if (match-literal lit mod)
        (return lit))))

(defun match-literal(lit mod)
  "rtns actual atom, neg or aff, in mod that matches lit"
  (cond((match lit mod) lit)
       ((match (negate lit) mod)(negate lit))))

(defun allpos (lst1 &optional lst2)
  "generates all possible contingencies from a list of literals"
  (if (null lst2)
      (setf lst2 '(())))
  (cond((null lst1) nil)
       ((null (cdr lst1))
          (flesh (car lst1) lst2))
     (t (flesh (car lst1)(allpos (cdr lst1) lst2)))))

(defun findatms (mods &optional lis)
  "rtns a list of each atom, once only, in the order that they occur in mods, treatingatm and its negation as same"
  (if (null mods) 
      (reverse lis)
    (findatms (cdr mods) (append (fi-atms (car mods) lis)))))

(defun fi-atms(mod lis)
  "rtns literals in mod with those in lis, and rtns each once only"
  (cond((null mod) lis)
       ((or (match (car mod) lis)
            (match (negate (car mod)) lis)) (fi-atms (cdr mod) lis))
       (t (fi-atms (cdr mod) (cons (car mod) lis)))))
;-------------------------------------------------------
; Part 5.2: Matching or removing models or their parts
;-------------------------------------------------------

(defun matchmodelsets (models1 models2)
  "rtns t iff two sets of fully explicit models are the same, regardless of order of models or of itms in them"
  (if (implicit models1)
      (setf models1 (flesh-out(make-explicit models1))))
  (if (implicit models2)
      (setf models2 (flesh-out(make-explicit models2))))
  (cond((and (null models1)(null models2)) t)
       ((not (eq (length models1)(length models2))) nil)
       ((equal models2 (setf models2 (remove-itm (car models1) models2))) nil) 
       (t (matchmodelsets (cdr models1) models2))))

(defun rem-atom (atom model)
  "remove atom from a model"
  (cond ((null model) nil)
        ((equal atom (car model)) (cdr model))
        (t (cons (car model) (rem-atom atom (cdr model))))))

(defun remove-itm (itm lis)
  "remove itm from lis (or model)"
  (cond((null lis) nil)
       ((matchlists itm (car lis))(remove-itm itm (cdr lis)))
       (t (cons (car lis) (remove-itm itm (cdr lis))))))

(defun matchlists (lis1 lis2)
  "rtns lis2 iff it and lis1 have identical members,e.g. ((a)(b)) = ((b)(a))"
  (cond((equal lis1 lis2) lis2)
       ((null lis1) nil)
       ((and (matchl lis1 lis2)(matchl lis2 lis1)) lis2)))

; (matchl '((a)(b)) '((b)(a)(c))) => t   (matchl '((a)(b)(c)) '((b)(a))) => NIL
(defun matchl (lis1 lis2)
  "rtns t iff each item in lis1 is in lis2"
  (cond((null lis1) t)
       ((match (car lis1) lis2)(matchl (cdr lis1) lis2))))

(defun match (item mod)
  "rtns item (or mod) iff it occurs in mod (or lis in same order), sensitive to polarity"
  (cond((null mod) nil)
       ((equal item (car mod))(car mod))
       (t (match item (cdr mod)))))

#| (remove-dups-amalgamate-fns '(((A) (T104)) ((B) (T105)) ((C) (T106)) ((B) (T107)))) =>
      (((A) (T104)) ((B) (T118)) ((C) (T106))) |#
(defun remove-dups-amalgamate-fns(models)
  "removes duplicate mods in models, including those that differ only in footnotes, which it amalgamates"
  (let (temp new-mod)
  "eliminates duplicate tokens of model, combining identical explicit parts of models & amalgamate their fns"
  (cond((null models) nil)
       ((eq models t) t)
       ((matchmod (car models)(cdr models))(remove-dups-amalgamate-fns (cdr models)))
       ((setf temp (matchmods-with-no-footnotes (car models)(cdr models))) ; needs to remove dup from cdr.models
              (setf new-mod (first temp) models (second temp))
              (cons new-mod (remove-dups-amalgamate-fns models)))
       (t (cons (car models)(remove-dups-amalgamate-fns (cdr models)))))))

(defun matchmods-with-no-footnotes(mod models)
  (let (new-footnote)
  "if explicit part of mod matches explicit part of m in models rtns list of mod with combined footnotes and
   models minus m "
  (dolist (m models)
    (cond((and (matchlists (remove-implicit m)(remove-implicit mod))
               (setf new-footnote (amalgamate-footnotes m mod)))
             (return (list (append (remove-implicit mod) new-footnote)(remove-mod m models))))))))
       
(defun remove-dups (models)
  "eliminates duplicate tokens of models, but sensitive to different footnotes"
  (cond((null models) nil)  
       ((eq models t) t)
       ((matchmod (car models)(cdr models))(remove-dups (cdr models)))
       (t (cons (car models)(remove-dups (cdr models))))))

(defun item-in-models(item models)
  "rtns itm if it occurs in any model in models"
  (cond((null models) nil)
       ((match item (car models)) item)
       (t (item-in-models item (cdr models)))))

(defun matchmod(mod models)
  "rtns mod if it matches m in models"
  (dolist (m models)
    (cond((matchlists m mod)(return mod)))))

(defun member-lis(itm lists)
  (dolist (lis lists)
    (if (equal itm lis)(return lis))))

;----------------------------
; Part 5.3: Print functions
;----------------------------

;;; Printing sentences and literals

(defun print-sentence (lis)
  "given tracer initialized, converts a list into a string, capitalizing initial letter of first word, adds period, 
  and rtns it"
  (trc "Language" (format nil "~A" 
      (concatenate 'string (string-capitalize (atm-to-string (car lis)))(conversion (cdr lis))))))

(defun conversion(lis)
  "converts lis of atms to string"
(if (null lis) 
    "."
     (concatenate 'string (atm-to-string (car lis))(conversion (cdr lis)))))

(defun atm-to-string(atm)
  "turns atm into a string with space at end"
  (concatenate 'string " " (string-downcase (symbol-name atm))))

(defun format-lis (premise)
  "prints lis without parentheses"
  (format nil "~{~A~#[~:; ~]~}" premise))

(defun remove-hyphens(atm)
  "removes hyphens from atm"
  (let ((item-s (replace-all (symbol-name atm) "-" " ")))
    (convert-str-to-lis item-s)))

(defun convert-str-to-lis(str)
  "converts a string of words separated with white space into a list of words"
  (with-input-from-string (s str)
             (loop for x = (read s nil :end) until (eq x :end) collect x)))

(defun replace-all (string part replacement &key (test #'char=))
  "rtns a new string in which all the occurences of the part is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

;;; Printing models

(defun print-models (models &key (output *standard-output*))
  "prints a set of models properly aligned"
  (setf models (tidy-up models))
  (let ((template (make-print-template models)))
  (cond((null models)(print-sentence '(null model)))
       ((eq models t)(print t))
       (t (dolist(mod models)
            (terpri output)
            (cond((and (not *print-footnotes*)(wholly-implicit mod))
                      (print-space (center-impl-mod template) output)(princ ". . ." output))
                 (t (pm template mod output))))))))  

(defun pm (template model &optional (output *standard-output*))
  "uses template to print items in model with appropriate separations"
  (let ((lead-in 4))
  (cond((null model) t)
       ((null template)
        (print-item (car model) output)
        (pm template (cdr model) output))    
       ((and (footnotep (car template))(footnotep (car model)))                       
        (print-item (car model) output)
        (pm (cdr template)(cdr model) output))
       ((equal (affirm (first template))(affirm (first model)))                 
        (print-item (car model) output)
        (pm (cdr template)(cdr model) output))
       ((match (affirm (car template))(affirm-mod model))
        (pm template (append-mods (cdr model)(list(car model))) output))
       (t  (print-space (+ lead-in (length (symbol-name (caar template)))) output)
           (pm (cdr template) model output)))))

(defun center-impl-mod(template)
  "rtns number of spaces to print in order to center . . . for implicit model, depending on template"
  (let ((num 0))
  (dolist(item template num)
    (setf num (+ num 2 (length (symbol-name (first item))))))
  (- (floor (/ num 2)) 2)))

(defun make-print-template(models)
  "lists all literals in models, making them affirmative, and ignoring footnotes, then adds one if there is one 
   in models, treating all bound variables as though they were the same."
  (let* ((temp (affirm-mod (findatms models)))(impl-var (includes-implicit temp))(temp (rem-bound-vars temp)))
    (if  impl-var 
        (append temp (car (modelize impl-var)))
      temp)))
     
(defun rem-bound-vars(model)
  "removes all bound-variables from models, incl those of the form '(- t23) "
  (cond((null model) nil)
       ((eq (caar model) '-)
         (cond((boundp (cadar model))(rem-bound-vars (cdr model)))
              (t (cons (car model)(rem-bound-vars (cdr model))))))
       ((boundp (caar model))(rem-bound-vars (cdr model)))
       (t (cons (car model)(rem-bound-vars (cdr model))))))

(defun print-item (item &optional (output *standard-output*))
  "prints single prop e.g. '(a1) '(- a1) or '(t121) which is a gentemp variable"
(cond((null item) t)
     ((eq (car item) '-)
           (print-space 2 output)(princ "¬ " output)(princ (cadr item) output))
     ((boundp (car item))
        (cond( *print-footnotes*
               (princ " " output)(princ "{" output)(princ (car item) output)(princ " " output)
               (princ (eval(car item)) output)(princ "}" output))))
     (t (print-space 4 output)(princ (car item) output))))

(defun affirm-mod(mod)
  "changes all items in a model to affirmative"
  (mapcar #'affirm mod))

(defun affirm (itm)
  "rtns affirmative itm if neg, otherwise the affirmative itm"
  (if (eq '- (car itm))
      (negate itm)
     itm))

(defun negate-premise(premise)
  "converts negative sentence to affirmative and vice versa"
  (cond((match 'not premise)(rem-atom 'not premise))
       ((> (length premise) 1)(append '(not comma) premise))
       (t (cons 'not premise))))

(defun print-space(number &optional (output *standard-output*))
  "prints number of spaces"
  (cond((<= number 0) t)
       (t  (format output " ")                      
           (print-space (- number 1) output))))     

;--------------------------------------------------------------------------------------------------------------------
;                                  Part 6. Insert hyphens and remove them from notation
;--------------------------------------------------------------------------------------------------------------------

(defun tranhyph(s)
  "inserts hyphens into s"
  (prog(clause output-s)
    loop
    (cond((null s)
              (if clause (setf output-s (append output-s (list (read-from-string (hyphenate clause))))))
              (return output-s))
         ((eq 'var (caar (shift (first s) *lexicon*)))
              (setf clause (append clause (list (first s))))
              (setf s (rest s))
              (go loop))
         (t (if clause 
                (setf output-s (append output-s (list (read-from-string (hyphenate clause))))))
            (setf output-s (append output-s (list (first s))))
            (setf s (rest s) clause nil)
            (go loop)))))

(defun hyphenate(lis)
  "converts lis into a string with hypens between the items"
  (if (null (rest lis))
      (symbol-name (first lis))
    (concatenate 'string (symbol-name (first lis)) "-" (hyphenate (rest lis)))))

(defun dehyphenate(lis)
  "removes hyphens from items, and converts back to atoms in a list"
  (let (output)
  (dolist(itm lis output)
    (setf output (append output (convert-str-to-lis (replace-all (symbol-name itm) "-" " ")))))))

;---------------------------------------------------------------------------------------------------------------------------
;                                       Part 7: TRACER CLASSES AND FUNCTIONS (Sunny Khemlani;s code)
;---------------------------------------------------------------------------------------------------------------------------

#| to elicit: (initialize-tracer :verbose t) ; for full descn or :verbose nil |#
(defclass tracer ()
  ((enabled       :accessor enabled        :initarg :e   :initform nil)
   (steps         :accessor steps          :initarg :s   :initform -1)
   (verbose       :accessor verbose        :initarg :v   :initform nil)
   (runtime       :accessor runtime        :initarg :r   :initform (get-internal-run-time))
   (response      :accessor response       :initarg :res :initform nil)
   (initial-model :accessor initial-model  :initarg :im  :initform nil)
   (final-model   :accessor final-model    :initarg :fm  :initform nil)
   (num-models    :accessor num-models     :initarg :nm  :initform 0)   ; for number of models
   (trace         :accessor trace-output   :initarg :tr  :initform nil))
  (:documentation "Class for tracer logging"))

(defparameter *tracer* (make-instance 'tracer))

(defun compute-runtime ()
  "Converts runtime to process cycle"
  (- (get-internal-run-time) (runtime *tracer*)))

(defun trace-header ()
  "Adds header to system trace on initial output of tracer and on every tracer reset"
  (case (steps *tracer*)
    (-1
     (format t "---- --------- --------------------------------------------------------------------------- ------- ~%")
     (format t "Step System    Description                                                                 Runtime    ~%")
     (format t "---- --------- --------------------------------------------------------------------------- ------- ~%")
     (setf (runtime *tracer*) (get-internal-run-time))
     (incf (steps *tracer*))
     (format t "~4@<~A~> ~9@<~A~> ~75@<~A~> ~7@<~A~>~%"
             (steps *tracer*) "--" "Initialized trace" (compute-runtime)))
    (0
     (format t "---- --------- --------------------------------------------------------------------------- ------- ~%")
     (setf (runtime *tracer*) (get-internal-run-time))
     (format t "~4@<~A~> ~9@<~A~> ~75@<~A~> ~7@<~A~>~%"
             (steps *tracer*) "--" "Reset trace" (compute-runtime)))))

(defun tracer (system description &key (model nil))
  "Fn to add a line to the system trace and optionally print it out"
  (let ((model (cond
                ((null model) nil)
                ((listp model) (mapcar #'copy-class-instance model))
                (model         (copy-class-instance model)))))
    (when (enabled *tracer*)
        (when (member system (list "System 0" "System 1" "System 2" "System 3" "Control" "Language") :test #'string-equal)
          (incf (steps *tracer*)))
        (push (list (if (string-equal system "") "" (steps *tracer*)) system description (compute-runtime) model)
              (trace-output *tracer*))
      (when (verbose *tracer*)
        (trace-header)
        (format t "~4@<~A~> ~9@<~A~> ~75@<~A~> ~7@<~A~>~%" (if (string-equal system "") "" (steps *tracer*))
                system description (compute-runtime))))))

(defun trc (system description &key (m nil))
  "Abbreviation wrapper fn for tracer"
  (tracer system description :model m))

(defun initialize-tracer (&key (enabled t) (steps -2) (verbose nil) (runtime (get-internal-run-time)))
  "Initializes tracer to defaults based on parameters"
  (setf *tracer* (make-instance 'tracer :e enabled :s steps :v verbose :r runtime)))

(defun enable-tracer (&key (verbose nil))
  "Enables tracer and sets trace verbosity"
  (if *tracer*
      (progn
        (setf (enabled *tracer*) t)
        (setf (verbose *tracer*) verbose))
    (initialize-tracer :v verbose)))

(defun disable-tracer ()
  "Disables tracer"
  (setf (enabled *tracer*) nil))

(defun reset-tracer ()
  "Resets tracer"
  (unless (< (steps *tracer*) 0)
    (setf (steps *tracer*) -1))
  (setf (trace-output *tracer*) nil)
  (setf (response *tracer*) nil)
  (setf (initial-model *tracer*) nil)
  (setf (final-model *tracer*) nil)
  (setf (runtime *tracer*) (get-internal-run-time))
  *tracer*)

(defun trace-models (text models)
  "Outputs model in tracer format"
  (let (lines
        (output (make-array 0
                            :element-type 'character 
                            :adjustable t 
                            :fill-pointer 0))
        (sys (case *system* (1 "System 1") (2 "System 2") )))
    (cond((null models)(trc sys (format nil "Model of conjunction of premises: ~A" nil)))
         ((equal models 'nvc)(trc sys (format nil "Conjunction of premises yield no valid conclusion")))
         (t   
          (with-output-to-string (o output)
            (print-models models :output o))
          (setf lines (rest (split-sequence (format nil "~%") output)))
          (trc sys text)
          (dolist (line lines)
            (trc "" line))))))

;---------------------------------------------------------------------------------------------------------------------------
;                                       Part 8: BATCH PROCESSING (Sunny Khemlani's code)
;---------------------------------------------------------------------------------------------------------------------------

; Discrete parameter settings for grid-search
(defparameter *parameters* (let ((parameters nil))
                             (dolist (g '(0.0 0.2 0.4 0.6 0.8 1.0))
                                 (dolist (s '(0.0 0.2 0.4 0.6 0.8 1.0))
                                   (push (list g s) parameters)))
                             parameters))

(defun parameter-search (problems &key (directory nil) (N 1000) (parameters *parameters*) (verbose t))
  (let ((g *gamma*) (s *sigma*)
        experiments)
    (if (listp parameters)
        (setf experiments (length parameters))
      (progn
        (setf experiments parameters)
        (setf parameters (randomize *parameters*))))
    #+lispworks (when (not directory) (setf directory (capi:prompt-for-directory "Save data file here:")))
    (format t "~%Exporting data to: ~A~%" directory)
    (dotimes (i experiments)
      (run-experiment problems :N N :verbose verbose :parameters (nth i parameters) :status (list (1+ i) experiments))
      (export-synthetic-data *synthetic-data* :directory directory :parameter-string (format nil "g~A-s~A" *gamma* *sigma*)))
 
    (setf *gamma* g *sigma* s)))

(defun run-experiment (problems &key (N 20) (verbose t) (parameters nil) (status nil))
  (setf *synthetic-data* nil)

  (when verbose
    (format t "~%--------------------------~%~
               Experiment ~A~%~
               --------------------------~%~
               Problems:            ~5d~%~
               Simulated subjects:  ~5d~%~%" (if status (format nil "(~A of ~A)" (first status) (second status)) "") (length problems) N))

  (dotimes (subject N)
    (dotimes (problem (length problems))
      (let ((problem-list (nth problem problems))
            (data-point   (make-list 11 :initial-element "NA")))

        (run-problem problem-list :parameters parameters)
        (setf (nth 0  data-point) (format nil "S~A" (1+ subject)))
        (setf (nth 1  data-point) (format nil "P~A" (1+ problem)))
        (setf (nth 2  data-point) (response *tracer*))
        (setf (nth 3  data-point) "NA")    ;;; !!! SSK serialize premise models
        (setf (nth 4  data-point) "NA")    ;;; !!! SSK serialize conclusion models
        (setf (nth 5  data-point) (format nil "~A" (first problem-list)))
        (setf (nth 6  data-point) (format nil "~A" (second problem-list)))
        (setf (nth 7  data-point) (format nil "~A" (third problem-list)))
        (setf (nth 8  data-point) *gamma*)
        (setf (nth 9  data-point) *sigma*)
        (setf (nth 10 data-point) (fourth problem-list))
        (push data-point *synthetic-data*))) )
  (when verbose
    (format t "Parameter settings:~%~
               ~T           gamma   = ~4d~%~
               ~T           sigma   = ~4d~%~
               --------------------------~%"  *gamma* *sigma*)))

(defun export-synthetic-data (data &key (directory nil) (parameter-string nil))
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    
    #+lispworks (when (not directory) (setf directory (capi:prompt-for-directory "Save data file here:")))
  
    (when directory
      (setf pathname (merge-pathnames directory (if (null parameter-string)
                                                    (format nil "mSententialData ~A-~A-~A.csv" year month date)
                                                  (format nil "mSententialData ~A-~A-~A ~A.csv" year month date parameter-string))))
      (with-open-file (output pathname
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (format output "Subject,ProblemNumber,Response,PremiseModels,ConclusionModels,Premises,Task,Correct,Gamma,Sigma,Data~%")
        (dolist (datum (reverse data))
          (format output "~{~a~^,~}~%" datum))))))

(defun run-problem (problem &key (verbose nil) (parameters nil))
  (reset-tracer)

  (when parameters
      (setf *gamma*  (nth 0 parameters)
            *sigma*  (nth 1 parameters)))  

  (when verbose
    (format t "gamma = ~A sigma = ~A~%" *gamma* *sigma*))

  (let* ((premises (nth 0 problem))
         (task (nth 1 problem))
         (correct (nth 1 problem))
         (data (nth 2 problem)))
    (funcall 'inference premises task)))

#|
With *gamma* of .01, weak validity is almost certain to occur, and so:
(deliberation '((a or b)(a ore b)) 'necessary?) => YES
(deliberation '((a ore b)(a or b)) 'necessary?) => NO
With *gamma* of .99 weak validity is almost certain NOT to occur, and so:
(deliberation '((a or b)(a ore b)) 'necessary?) => NO
(deliberation '((a ore b)(a or b)) 'necessary?) => N)
|#

;---------------------------------------------------------------------------------------------------------------------------
;                                Part 9: ILLUSTRATIONS of the program's performance
;---------------------------------------------------------------------------------------------------------------------------

;---------------------------------------------------
; Part 9.1: Functions for illustrating the program
;---------------------------------------------------

(defun test-task(task lis)
  "carries out task for each set of premises in lis"
  (let ((count 1))
    (initialize-tracer :verbose t)
    (dolist(itm lis)
      (terpri)(terpri)(princ "PROBLEM ")(princ count)
      (princ ": ")(princ itm)(terpri)
      (inference itm task)
      (reset-tracer)
      (setf count (+ 1 count)))))

(defun test-fn(fn lis)
  "funcall of fn to each itm in lis"
  (let ((count 1) output)
    (setf *print-flag* t)
    (dolist(itm lis)
      (terpri)(terpri)(princ count)(princ " ")(princ itm)
      (setf count (+ count 1))
      (setf output (funcall fn itm))
      (print output)
      output)))

(defun make-lis-premises(p1 p2)
  "makes list of basic inferences" 
  (mapcan #'(lambda(c1)
       (mapcar #'(lambda(c2)
                   (list c1 c2))    
               p1))
            p2))

(defvar *prem1* '((a)(not a)(b)(not b)))
(defvar *prem2* '((if a then b)(iff a then b)(a or b)(a ore b)))
(defvar *prem3* '((a and b)(a and not b)(not a and b)(not a and not b)))

(setf *premises-for-what-follows*
      (make-lis-premises *prem1* *prem2*))

(setf *premises-to-verify*
      (make-lis-premises *prem3* *prem2*))

;-------------------------------------------------
; Part 9.2: Examples for illustrating the program
;-------------------------------------------------

#| Illustrative calls using test-task for different tasks 
                                                                    
(test-task nil            *single-premises*)                                                      
(test-task 'necessary?    *illusions-2*)                                           
(test-task 'what-follows? *premises-for-what-follows*)  
(test-task 'what-follows? *premises-for-modulation*)    
(test-task 'necessary?    *premises-it-follows*)                          
(test-task 'necessary?    *it-follows-modulation*)                    
(test-task 'necessary?    *defeasance-examples*)        
(test-task 'necessary?    *infer-cases*)
(test-task 'possible?     *hinterecker*)                
(test-task 'verify?       *premises-to-verify*)         
(test-task 'probability?  *premises-to-verify*)         
(test-task nil            *sklarek-1*)            |#

(defvar *single-premises* '(            
     ((if a then b))                  
     ((iff a then b))                 
     ((a ore b))                      
     ((a or b))                       
     ((a and not a))                            
     ((a or not a))                  
     ((iff j ore not q then j))       
     ((n or b ore comma b or c))      
     ((r and s ore r))                
     ((a ore b ore c)) ))            

(defvar *premises-it-follows* '(
     ((If A then B)(A)(B))               
     ((If A then B)(Not A)(Not B))
     ((If A then B)(B)(A))
     ((If A then B)(Not B)(Not A))
     ((Iff A then B)(A)(B))      
     ((Iff A then B)(Not A)(Not B))
     ((Iff A then B)(B)(A))
     ((Iff A then B)(Not B)(Not A))
     ((A or B)(A)(Not B))
     ((A or B)(B)(Not A))
     ((A or B)(Not A)(B))
     ((A or B)(Not B)(A))
     ((A ore B)(A)(Not B))
     ((A ore B)(B)(Not A))
     ((A ore B)(Not A)(B))
     ((A ore B)(Not B)(A))
     ((A or B)(A ore B))
     ((A ore B)(A or B))))

(setf *test-intuition* '(
 ((if a then b)(if b then c))        
 ((a ore b)(b ore c))                
 ((a ore b)(not a ore b))            
 ((if a then b)(if not a then b))    
 ((if a then b)(if a then not b))    

 ((a and b)(c ore d))                
 ((a and b)(a ore b))                
 ((if a then b)(a ore b))            
 ((if a then b)(a ore c))            
 ((if a then b)(not a ore c))        
 ((if a then b)(not a ore b))        
 ((a ore b ore c)(a))                
 ((a ore b ore c)(not a and not c))  
; illusions
 ((if a then b ore if not a then b))                   
 ((a ore b ore c))                                    
 ((a ore b ore comma c ore b))                         
 ((a ore b ore b))                                     
 ((a ore b ore c)(a))                                  
 ((if k then a ore if not k then a)(k))                
 ((if k then a ore if q then a)(k))                    
 ((a or b ore comma not a or b)(b))                    
 ((a ore b and comma not a ore b)(b))                    
 ((r and s ore r)(not s))                              
 ((iff j ore not q then j)(q)) ))

(setf *test-m-d* '(
    ((a or b)(b or a))                  
    ((a and b and c)(a and b))          
    ((a or b)(a ore b))                 
    ((a or b)(a and b))
    ((a ore b)(a))
    ((a ore b)(a or b))                 
    ((a and b)(a or b))
    ((a)(a ore b))      
    ((a ore b)(a ore c))                
    ((a ore b)(not a ore b))            
    ((a)(b))))

(setf *hinterecker* '(
                      ((A or B)(A and B))
                      ((A ore B)(A and B))
                      ((A or B)(A ore B))
                      ((A ore B)(A or B))))

(defvar *sklarek-1* '(
                    ((A ore B)(not A ore B))
                    ((A ore B)(A ore not B))
                    ((A ore B)(not A ore not B))
                    ((A ore not B)(not A ore B))

                    ((A or B)(A or not B))
                    ((A or B)(not A or B))
                    ((A or B)(not A or not B))
                    ((A or not B)(not A or B))
                    ((A and B)(A or B))
                    ((not A and not B)(not A or not B)) ))

#|
A ore B, ¬A ore B         1
A ore B, A ore ¬B         2
A ore B, ¬A ore ¬B        3
A ore ¬B, ¬A ore B        4
(A and B) ore B           5
(A and ¬B) ore ¬B         6
(A and B) ore (¬A and ¬B) 7
(A and B) ore (¬A and B)  8
(A ore ¬B), ¬B            9
(¬A ore ¬B), ¬B          10
(A ore ¬B), B            11        
(¬A ore ¬B), B           12
|#

(defvar *sklarek-1.2* '(
                    ((A ore B)(C ore B))
                    ((A ore B)(A ore C))
                    ((A ore B)(not A ore not B))
                    ((A ore not B)(not A ore B))

                    ((A or B)(A or not B))
                    ((A or B)(not A or B))
                    ((A or B)(not A or not B))
                    ((A or not B)(not A or B))
                    ((A and B)(A or B))
                    ((not A and not B)(not A or not B)) ))

; illusions and controls
(setf *illusions-1* '(
      ((if a then b ore comma if not a then b)(a))    
      ((a and not b ore comma not a and b)(a))        
      ((if a then b ore a)(a))                        
      ((if a then b ore not a)(a))                    

      ((a or b ore comma c or b)(not a and not b))   
      ((a ore b ore comma c ore b)(a and c))       
      ((a ore b ore comma c ore b)(not a and not c))
))

(defvar *illusions-2* '(                               
     ((if k then a ore if not k then a)(k)(a))         
     ((if k then a ore if q then a)(k)(a))             
     ((a or b ore comma not a or b)(b))               
     ((a or b ore comma c or b)(not a and not c)(b))   
     ((a ore b and comma not a ore b)(b))              
     ((a or b and comma not a or b)(b))                
     ((r and s ore r)(not s))                          
     ((iff j ore not q then j)(q)) ))                 

(defvar *infer-cases* '( 
                         ((If A then B)(A)(not B))                    
                         ((A)(If A then comma B and C)(A and B and C))  
                         ((A)(if A or B then C)(C))                     
                         ((A or B)(A and B))                            
                         ((A)(A or B))                                  
                         ((A ore B)(C and D))))                         

; Longer list of illustrative inferences:
(defvar *illustrations* '(                                               
                        ((A and not A))                                                                  
                        ((A or not A))                                                                   
                        ((B)(If A then B))                                                          
                        ((Not A)(If A then B))                                                    
                        ((A)(If A then B)(B))                                                          
                        ((A)(If A then B)(A and B and C))                  
                        ((A)(If A then comma B and C)(A and B and C))         
                        ((A)(If A then B)(B and A))                                             
                        ((If A then B)(A and not B))                   
                        ((If A then B)(A)(not B))                      
                        ((A or B)(A))                                  
                        ((A or B)(B))                                  
                        ((A or B)(A and B))                            
                        ((A)(A or B))                                 
                        ((A or B)(C))                                  
                        ((A)(A ore B)(not B))                          
                        ((not A)(A ore B)(B))                                                     
                        ((not A)(A or B)(B))                                                    
                        ((A or B)(Not B)(A))                           
                        ((A or B)(Not A)(Not A and B and C))           
                        ((A or B)(Not B)(A and not B))                      
                        ((A or B)(Not A)(Not B))                       
                        ((A ore B)(A or B))                            
                        ((A or B)(A ore B))                            
                        ))

(defvar *illusions* '(
     ((if there is a king then there is an ace ore if not there is a king then there is an ace)(there is an ace)) 
     ((you have the bread ore comma you have the soup ore you have the salad)(you have the bread)
      (you have the soup ore you have the salad))
     ((albert is here or betty is here ore comma charlie is here or betty is here)
      (not albert is here and not charlie is here)(betty is here))
     ((king or ace ore comma queen or ace)(ace))
     ((iff jack ore not queen then jack))
     ((there is a nail or there is a bolt ore comma there is a bolt and there is a wrench)
      (there is a nail and there is a bolt and there is a wrench))
     ((red and square ore red))))

(defvar *premises-for-modulation* '(
   ((if raining then hot)(not raining))                            
   ((if not raining then not Louvre in Paris)(raining))            
   ((if raining then pouring)(not raining))                        
   ((not raining or not pouring)(not raining))                     
   ((if god exists then atheism is wrong)(not god exists))         
   ((god exists or atheism is right)(atheism is right))            
   ((iff god exists then atheism is right)(god exists))))         


(defvar *it-follows-modulation* '(
    ((if louvre in paris then he is married)(he is married))
    ((if louvre in paris then not he is married)(he is married))
    ((if not louvre in paris then he is married)(not he is married))
    ((if not louvre in paris then not he is married)(not he is married))
    ((if she is married then not louvre in paris)(not she is married))
    ))                    

(setf *defeasance-examples* '(
   ((if a poisonous snake bites her then she dies)(a poisonous snake bites her)(not she dies))
   ((if pull trigger then gun fires)(pull trigger)(not gun fires))))

;----------------------------------------------------
; Part 9.3: List of inferences used for modeling data
;----------------------------------------------------
#|
I.Necessary?
 To model Hinterecker et al Expt 1: inferences from disjunctions to disjunction
A or B but not both.
Therefore, A or B or both.	Valid	 Reject	 3
A or B or both.
Therefore, A or B but not both.	Invalid	 Reject	24

With *gamma* of .01, weak validity is almost certain to occur, and so:
(inference '((a or b)(a ore b)) 'necessary?) => YES   
(inference '((a ore b)(a or b)) 'necessary?) => NO    
With *gamma* of .99 weak validity is almost certain NOT to occur, and so:
(inference '((a or b)(a ore b)) 'necessary?) => NO    
(inference '((a ore b)(a or b)) 'necessary?) => NO    

II. POSSIBLE?
1. Hinterecker et al Expt 3:
  A or B or both 
  (inference '((a or b)(a)) 'possible?) sys1: yes sys2: yes                         		 	
  Therefore, possibly A	 	        91% respond yes
  (inference '((a or b)(b)) 'possible?) sys1: yes sys2: yes                       
  Therefore, possibly B	 	        94% respond yes
  (inference '((a or b)(a and b)) 'possible?) sys1: yes sys2: yes                 
  Therefore, possibly A and B	        88% respond yes
  (inference '((a or b)(not a and not b)) 'possible?) sys1: no sys2: no           
  Therefore, possibly not A and not B	18% respond yes
5. Goodwin & Johnson-Laird (2016):
  If A then B
  (inference '((if a then b)(a and b)) 'possible?)         sys1: yes  sys2: yes  ok
  Therefore, possibly A and B.
  (inference '((if a then b)(a and not b)) 'possible?)     sys1: no   sys2: no   ok
  Therefore, possibly A and not B.
  (inference '((if a then b)(not a and b)) 'possible?)     sys1: no  sys2: yes   ok
  Therefore, possible not-A and B.
  (inference '((if a then b)(not a and not b)) 'possible?) sys1: yes  sys2: yes  ok
  Therefore, possibly not-A and not-B.
6. Hinterecker et al Expt 1:
  A or B,
  (inference '((if a then b)(not a and not b)) 'possible?)  sys1: yes  sys2: yes ok
  Therefore, possibly A and B.
  A ore B,
  (inference '((a ore b)(a and b)) 'possible?) sys1: no   sys2: no               ok
  Therefore, possibly A and B.
  A or B.
  (inference '((a or b)(a ore b)) 'possible?)  sys1: no  sys2: yes               ok
  Therefore, possibly A ore B.
|#

(defparameter *schroyens-&-schaeken-2003-exp1*
  '((((if a then b) (a))                                     necessary? YES  100)
    (((if a then b) (b))                                     necessary  NO   -1)
    (((if a then b) (not a))                                 necessary? NO   -1)
    (((if a then b) (not b))                                 necessary? YES  -1)))

(defparameter *hinterecker-et-al-2016-exp1*
  '((((A or B) (A and B))                                    possible?  YES  82)
    (((A ore B) (A and B))                                   possible?  NO   10)
    (((A ore B) (A or B))                                    necessary? NO   3)
    (((A or B) (A ore B))                                    necessary? YES  24)))

(defparameter *hinterecker-et-al-2016-exp3*
  '((((A or B)(A))                                           possible?  YES  91)
    (((A or B)(B))                                           possible?  YES  94)
    (((A or B)(A and B))                                     possible?  YES  88)
    (((A or B)(Not A and not B))                             possible?  YES  18)))

(defparameter *khemlani-&-jl-2009-exp2*
  '((((a and b ore comma b ore c) (not a and b and not c))   possible?  YES  90)
    (((a and b ore comma b ore c) (a and b and not c))       possible?  NO   40)
    (((a and b ore comma b or c) (not a and b and not c))    possible?  YES  100)
    (((a and b ore comma b or c) (a and b and not c))        possible?  NO   20)
    (((a and b or comma b ore c) (not a and b and c))        possible?  NO   80)
    (((a and b or comma b ore c) (a and not b and c))        possible?  YES  20)
    (((a and b or comma b or c) (not a and not b and not c)) possible?  NO   100)
    (((a and b or comma b or c) (a and not b and c))         possible?  YES  30)))

;---------------------------------------------------------------------------------------------------------------------------
; End of file
;---------------------------------------------------------------------------------------------------------------------------





