;All functions assume that the codo triplestore is open and bound to *db*
;Also, there must be a Text Index set up with the Object being the value
;of the codo:statePatientID property. This enables fast lookups for patients based on their hasID
;Note: when compiling this file or individual functions you will get warnings that variable
;matched-p is not used. These are not serious. One function returns multiple values and we 
;need the second value but not the first but to get a handle on the second value we need to 
;provide a variable that binds to both. Matched-p is that variable. 

(in-package :db.agraph.user)

(defun analyze-ad-hoc-pid-strings1 ()
  (print "Entering ad hoc transforms 1")
  ;Father of P154 not in model so using 154
  (sparql:run-sparql 
"DELETE {
	?p codo:reasonString ?rs.}
INSERT { 
	?nexp a codo:CloseContact.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:contractedVirusFrom ?pc1.
	?p codo:hasRelationship ?pc1. 
	?p codo:contractedVirusFrom ?pc2.
	?p codo:hasRelationship ?pc2. 
	}
WHERE {
	?p codo:reasonString ?rs.
	?p codo:statePatientID ?pid.
	?pc1 codo:statePatientID '104'.
	?pc2 codo:statePatientID '154'.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#CloseContact-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Contact with P104 and father of P154')
	}")
  (sparql:run-sparql 
"DELETE {
	?p codo:reasonString ?rs.}
INSERT { 
	?nexp a codo:CloseContact.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:contractedVirusFrom ?pc1.
	?p codo:hasRelationship ?pc1. 
	?p codo:contractedVirusFrom ?pc2.
	?p codo:hasRelationship ?pc2. 
	}
WHERE {
	?p codo:reasonString ?rs.
	?p codo:statePatientID ?pid.
	?pc1 codo:statePatientID '159'.
	?pc2 codo:statePatientID '103'.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#CloseContact-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Contact of P159 and son of P103')}")
  (sparql:run-sparql 
"DELETE {
	?p codo:reasonString ?rs.}
INSERT { 
	?nexp a codo:CloseContact.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:contractedVirusFrom ?pc1.
	?p codo:hasRelationship ?pc1. 
	?p codo:contractedVirusFrom ?pc2.
	?p codo:hasRelationship ?pc2. 
	}
WHERE {
	?p codo:reasonString ?rs.
	?p codo:statePatientID ?pid.
	?pc1 codo:statePatientID '19'.
	?pc2 codo:statePatientID '94'.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#CloseContact-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Contact with P19 and sister of P94')
	}")
  (sparql:run-sparql 
"DELETE {
	?p codo:reasonString ?rs.}
INSERT { 
	?nexp a codo:CloseContact.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:contractedVirusFrom ?pc1.
	?p codo:hasRelationship ?pc1. 
	?p codo:contractedVirusFrom ?pc2.
	?p codo:hasRelationship ?pc2. 
	?p codo:contractedVirusFrom ?pc3.
	?p codo:hasRelationship ?pc3. 
	}
WHERE {
	?p codo:reasonString ?rs.
	?p codo:statePatientID ?pid.
	?pc1 codo:statePatientID '1243'.
	?pc2 codo:statePatientID '1244'.
	?pc3 codo:statePatientID '1245'.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#CloseContact-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Contact of P1243 to P1245')
	}")
   (sparql:run-sparql 
"DELETE {
	?p codo:reasonString ?rs.}
INSERT { 
	?nexp a codo:CloseContact.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:contractedVirusFrom ?pc1.
	?p codo:hasRelationship ?pc1. 
	?p codo:contractedVirusFrom ?pc2.
	?p codo:hasRelationship ?pc2. 
	?p codo:contractedVirusFrom ?pc3.
	?p codo:hasRelationship ?pc3. 
	?p codo:contractedVirusFrom ?pc4.
	?p codo:hasRelationship ?pc4. 
	?p codo:contractedVirusFrom ?pc5.
	?p codo:hasRelationship ?pc5. 
	}
WHERE {
	?p codo:reasonString ?rs.
	?p codo:statePatientID ?pid.
	?pc1 codo:statePatientID '134'.
	?pc2 codo:statePatientID '135'.
	?pc3 codo:statePatientID '136'.
	?pc4 codo:statePatientID '137'.
	?pc5 codo:statePatientID '138'.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#CloseContact-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(?rs,'Contact with P134-P135-P136-P137 and P138') || CONTAINS(?rs,'Contact with P134- P135- P136- P137 and P138'))
	}")
  (sparql:run-sparql 
"DELETE {
	?p codo:reasonString ?rs.}
INSERT { 
	?nexp a codo:CloseContact.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:contractedVirusFrom ?pc1.
	?p codo:hasRelationship ?pc1. 
	?p codo:contractedVirusFrom ?pc2.
	?p codo:hasRelationship ?pc2. 
	?p codo:contractedVirusFrom ?pc3.
	?p codo:hasRelationship ?pc3. 
	?p codo:contractedVirusFrom ?pc4.
	?p codo:hasRelationship ?pc4. 
	?p codo:contractedVirusFrom ?pc5.
	?p codo:hasRelationship ?pc5. 
	}
WHERE {
	?p codo:reasonString ?rs.
	?p codo:statePatientID ?pid.
	?pc1 codo:statePatientID '134'.
	?pc2 codo:statePatientID '135'.
	?pc3 codo:statePatientID '136'.
	?pc4 codo:statePatientID '137'.
	?pc5 codo:statePatientID '139'.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#CloseContact-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(?rs,'Contact with P134-P135-P136-P137 and P139'))
	}")
  (sparql:run-sparql 
"DELETE {
	?p codo:reasonString ?rs.}
INSERT { 
	?nexp a codo:CloseContact.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:contractedVirusFrom ?pc1.
	?p codo:hasRelationship ?pc1. 
	?p codo:contractedVirusFrom ?pc2.
	?p codo:hasRelationship ?pc2. 
	?p codo:contractedVirusFrom ?pc3.
	?p codo:hasRelationship ?pc3. 
	?p codo:contractedVirusFrom ?pc4.
	?p codo:hasRelationship ?pc4. 
	?p codo:contractedVirusFrom ?pc5.
	?p codo:hasRelationship ?pc5. 
	}
WHERE {
	?p codo:reasonString ?rs.
	?p codo:statePatientID ?pid.
	?pc1 codo:statePatientID '134'.
	?pc2 codo:statePatientID '135'.
	?pc3 codo:statePatientID '136'.
	?pc4 codo:statePatientID '137'.
	?pc5 codo:statePatientID '140'.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#CloseContact-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(?rs,'Contact with P134-P135-P136-P137 and P140'))
	}")
)

(defun analyze-ad-hoc-pid-strings2 ()
  (print "Entering ad hoc transforms 2")
  (sparql:run-sparql 
   "DELETE {
	?p codo:reasonString ?rs.}
INSERT { 
	?nexp a codo:CloseContact.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:contractedVirusFrom ?pc1.
	?p codo:hasRelationship ?pc1. 
	?p codo:contractedVirusFrom ?pc2.
	?p codo:hasRelationship ?pc2. 
	?p codo:contractedVirusFrom ?pc3.
	?p codo:hasRelationship ?pc3. 
	?p codo:contractedVirusFrom ?pc4.
	?p codo:hasRelationship ?pc4. 
	?p codo:contractedVirusFrom ?pc5.
	?p codo:hasRelationship ?pc5. 
	}
WHERE {
	?p codo:reasonString ?rs.
	?p codo:statePatientID ?pid.
	?pc1 codo:statePatientID '89'.
	?pc2 codo:statePatientID '90'.
	?pc3 codo:statePatientID '91'.
	?pc4 codo:statePatientID '141'.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#CloseContact-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(?rs,'Contact of P89-P90-P91 and P141'))}")
  (sparql:run-sparql 
   "DELETE {
	?p codo:reasonString ?rs.}
INSERT { 
	?nexp a codo:CloseContact.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:contractedVirusFrom ?pc1.
	?p codo:hasRelationship ?pc1. 
	?p codo:contractedVirusFrom ?pc2.
	?p codo:hasRelationship ?pc2. 
	}
WHERE {
	?p codo:reasonString ?rs.
	?p codo:statePatientID ?pid.
	?pc1 codo:statePatientID '16752'.
	?pc2 codo:statePatientID '16753'.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#CloseContact-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(?rs,'Contact of P- 16752 and P-16753'))
	}")
  (sparql:run-sparql "DELETE {
	?p codo:reasonString ?rs.}
INSERT { 
	?nexp a codo:CloseContact.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:contractedVirusFrom ?pc1.
	?p codo:hasRelationship ?pc1. 
	?p codo:contractedVirusFrom ?pc2.
	?p codo:hasRelationship ?pc2. 
	?p codo:contractedVirusFrom ?pc3.
	?p codo:hasRelationship ?pc3. 
	?p codo:contractedVirusFrom ?pc4.
	?p codo:hasRelationship ?pc4. 
	?p codo:contractedVirusFrom ?pc5.
	?p codo:hasRelationship ?pc5. 
	}
WHERE {
	?p codo:reasonString ?rs.
	?p codo:statePatientID ?pid.
	?pc1 codo:statePatientID '23238'.
	?pc2 codo:statePatientID '23239'.
	?pc3 codo:statePatientID '23240'.
	?pc4 codo:statePatientID '23241'.
	?pc5 codo:statePatientID '23231'.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#CloseContact-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Contact of P23238 to P23241 and P23231')
	}"))


(defun make-close-contact-object (patient-upi rs-upi)
  ;A utility function for all the functions that process 
  ;reasonString properties with patient ID's in them
  ;Creates an instance of the codo:CloseContact object and makes it the value of the 
  ;suspectedReasonOfCatchingCovid-19 property
  ;upi->value gets the string representation of the name of the object
  (let* ((pid-upi (object (get-triple :s patient-upi :p !codo:statePatientID)))
         (pid-string  (upi->value pid-upi))
         (nexp-uri-string  (concatenate 'string "http://www.isibang.ac.in/ns/codo#CloseContact-" pid-string))
         (nexp-upi (intern-resource nexp-uri-string)))
    (add-triple nexp-upi !rdf:type !codo:CloseContact)
    (add-triple patient-upi !codo:suspectedReasonOfCatchingCovid-19 nexp-upi)
    (if *test-modep* (add-triple patient-upi  !codo:reasonStringTD  rs-upi))
    (delete-triples :s patient-upi :p !codo:reasonString :o rs-upi)
    )
  )


(defun analyze-pid-strings1 ()
  (print "Entering pattern matching transforms 1")
  ; For strings of the form: "Contact of P1606-1608"
  (let ((results 
         (sparql:run-sparql 
          "SELECT ?p ?rs WHERE {?p codo:reasonString ?rs. FILTER(REGEX(?rs, 'Contact of P\\\\d+-\\\\d+'))}" :results-format :lists)))
    ; Don't really need to loop for current data since only one string matches this pattern but best to do this
    ; in case new data has more strings like this
    (loop for result in results do
          ;Each result has two bindings, each in a list. First binding is the patient-upi ?p second is the reasonString-upi ?rs
          ;Following Let gets the bindings and gets the reasonString from its UPI
          (let* ((patient-upi (first result))
                 (patient-value (part->terse patient-upi))
                 (rs-upi (second result))
                 (rs (upi->value rs-upi))
                 )
            ;Need multiple-value-bind because match-re returns a boolean if it succeeds and then the actual matched string
            ;Don't need the boolean but need to bind to it anyway to get the string. This results in a compilation warning
            ;because the variable matched-p is not used
            (multiple-value-bind (matched-p ids) (match-re "\\d+-\\d+" rs)
              ;Split-re returns a list with the two integers between the dash
              (let* ((id-string-list (split-re "-"   ids))
                     (first-id-num (parse-integer (first id-string-list)))
                     (last-id-num (parse-integer (second id-string-list))))
                ;Use freetext index to efficiently retrieve the patient from its ID number
                (loop for id-index from first-id-num to last-id-num
                    do 
                      ;String+ converts the integer back to a string so it can be used to find the patient
                      (let* ((id-string (string+ id-index))
                             (host-upi (first (freetext-get-unique-subjects (list 'match id-string) :index "PatientIDs")))
                             )
                        (print (list patient-value id-string))
                        (add-triple patient-upi !codo:contractedVirusFrom host-upi)
                        ))))
            (make-close-contact-object patient-upi rs-upi)
            )))
  (commit-triple-store)
  )

(defun analyze-pid-strings2 () 
  ; For strings of the form: "Contact of P6137 to P6139"
  (print "Entering pattern matching transforms 2")
  (let ((results 
         (sparql:run-sparql "SELECT ?p ?rs WHERE {?p codo:reasonString ?rs. FILTER(REGEX(?rs, 'Contact of P\\\\d+ to P\\\\d+'))}" :results-format :lists)))
    ; Don't really need to loop for current data since only one string matches this pattern but best to do this
    ; in case new data has more strings like this
    (loop for result in results do
          ;Each result has two bindings, each in a list. First binding is the patient-upi ?p second is the reasonString-upi ?rs
          ;Following Let gets the bindings and gets the reasonString from its UPI
          (let* ((patient-upi (first result))
                 (patient-value (part->terse patient-upi))
                 (rs-upi (second result))
                 (rs (upi->value rs-upi))
                 )
            ;Need multiple-value-bind because match-re returns a boolean if it succeeds and then the actual matched string
            ;Don't need the boolean but need to bind to it anyway to get the string. This results in a compilation warning
            ;because the variable matched-p is not used
            (multiple-value-bind (matched-p ids) (match-re "\\d+ to P\\d+" rs)
              ;Split-re returns a list with the two integers between the dash
              (let* ((id-string-list (split-re " to P"   ids))
                     (first-id-num (parse-integer (first id-string-list)))
                     (last-id-num (parse-integer (second id-string-list))))
                ;Use freetext index to efficiently retrieve the patient from its ID number
                (loop for id-index from first-id-num to last-id-num
                    do 
                      ;String+ converts the integer back to a string so it can be used to find the patient
                      (let* ((id-string (string+ id-index))
                             (host-upi (first (freetext-get-unique-subjects (list 'match id-string))))
                             )
                        (print (list patient-value id-string))
                        (add-triple patient-upi !codo:contractedVirusFrom host-upi)
                        ))))
             (make-close-contact-object patient-upi rs-upi)
            )))
  (commit-triple-store)
  )

(defun analyze-pid-strings3 () 
  ; For strings of the form: "Contact of P1942-P1944 and P1947"
  ; See analyze-pid-strings1 for more detailed comments
  (print "Entering pattern matching transforms 3")
  (let ((results 
         (sparql:run-sparql 
          "SELECT ?p ?rs WHERE {?p codo:reasonString ?rs. FILTER(REGEX(?rs, ' P\\\\d+-P\\\\d+ and P\\\\d+'))}" :results-format :lists)))
    ;As in previous function, each result contains a list of the patient UPI and the reasonString UPI
    ;Everything else works as in analyze-seq-reason-strings1 except this checks for a different pattern and has to match
    ;for an additional patient ID that comes after the "and"
    (loop for result in results do
          (let* ((patient-upi (first result))
                 (rs-upi (second result))
                 (rs (upi->value rs-upi))
                 )
            (multiple-value-bind (matched-p ids) (match-re "\\d+-P\\d+" rs)
              (let* ((id-string-list (split-re "-P"   ids))
                     (first-id-num (parse-integer (first id-string-list)))
                     (last-id-num (parse-integer (second id-string-list)))
                     )
                (loop for id-index from first-id-num to last-id-num
                    do 
                      (let* ((id-string (string+ id-index))
                             (host-upi (first (freetext-get-unique-subjects (list 'match id-string))))
                             )
                        (print (list  patient-upi id-string))
                        (add-triple patient-upi !codo:contractedVirusFrom host-upi)
                        ))))
            (multiple-value-bind (matched-p and-id-string) (match-re "and P\\d+" rs)
              (let* ((id-string (subseq and-id-string 5))
                     (host-upi (first (freetext-get-unique-subjects (list 'match id-string)))))
                (print (list  patient-upi id-string))
                (add-triple patient-upi !codo:contractedVirusFrom host-upi)
                )
              )
            (make-close-contact-object patient-upi rs-upi))))
  (commit-triple-store)
  )




(defun analyze-pid-strings8 () 
  ; For strings of the form: "Contact of P1942-P1945"
  (print "Entering pattern matching transforms 8")
  (let ((results 
         (sparql:run-sparql "SELECT ?p ?rs WHERE {?p codo:reasonString ?rs. FILTER(REGEX(?rs, 'Contact of P\\\\d+-P\\\\d+'))}" :results-format :lists)))
    ; Don't really need to loop for current data since only one string matches this pattern but best to do this
    ; in case new data has more strings like this
    (loop for result in results do
          ;Each result has two bindings, each in a list. First binding is the patient-upi ?p second is the reasonString-upi ?rs
          ;Following Let gets the bindings and gets the reasonString from its UPI
          (let* ((patient-upi (first result))
                 (patient-value (part->terse patient-upi))
                 (rs-upi (second result))
                 (rs (upi->value rs-upi))
                 )
            ;Need multiple-value-bind because match-re returns a boolean if it succeeds and then the actual matched string
            ;Don't need the boolean but need to bind to it anyway to get the string. This results in a compilation warning
            ;because the variable matched-p is not used
            (multiple-value-bind (matched-p ids) (match-re "\\d+-P\\d+" rs)
              ;Split-re returns a list with the two integers between the dash
              (let* ((id-string-list (split-re "-P"   ids))
                     (first-id-num (parse-integer (first id-string-list)))
                     (last-id-num (parse-integer (second id-string-list))))
                ;Use freetext index to efficiently retrieve the patient from its ID number
                (loop for id-index from first-id-num to last-id-num
                    do 
                      ;String+ converts the integer back to a string so it can be used to find the patient
                      (let* ((id-string (string+ id-index))
                             (host-upi (first (freetext-get-unique-subjects (list 'match id-string))))
                             )
                        (print (list rs patient-value id-string host-upi))
                        (add-triple patient-upi !codo:contractedVirusFrom host-upi)
                        ))))
                        (make-close-contact-object patient-upi rs-upi)
            )))
  (commit-triple-store)
  )

(defun analyze-pid-strings9 () 
  ; For strings of the form: "Contact of P28441 to 28443"
  (print "Entering pattern matching transforms 9")
  (let ((results 
         (sparql:run-sparql "SELECT ?p ?rs WHERE {?p codo:reasonString ?rs. FILTER(REGEX(?rs, 'Contact of P\\\\d+ to \\\\d+'))}" :results-format :lists)))
    ; Don't really need to loop for current data since only one string matches this pattern but best to do this
    ; in case new data has more strings like this
    (loop for result in results do
          ;Each result has two bindings, each in a list. First binding is the patient-upi ?p second is the reasonString-upi ?rs
          ;Following Let gets the bindings and gets the reasonString from its UPI
          (let* ((patient-upi (first result))
                 (patient-value (part->terse patient-upi))
                 (rs-upi (second result))
                 (rs (upi->value rs-upi))
                 )
            ;Need multiple-value-bind because match-re returns a boolean if it succeeds and then the actual matched string
            ;Don't need the boolean but need to bind to it anyway to get the string. This results in a compilation warning
            ;because the variable matched-p is not used
            (multiple-value-bind (matched-p ids) (match-re "\\d+ to \\d+" rs)
              ;Split-re returns a list with the two integers between the dash
              (let* ((id-string-list (split-re " to "   ids))
                     (first-id-num (parse-integer (first id-string-list)))
                     (last-id-num (parse-integer (second id-string-list))))
                ;Use freetext index to efficiently retrieve the patient from its ID number
                (loop for id-index from first-id-num to last-id-num
                    do 
                      ;String+ converts the integer back to a string so it can be used to find the patient
                      (let* ((id-string (string+ id-index))
                             (host-upi (first (freetext-get-unique-subjects (list 'match id-string))))
                             )
                        (print (list patient-value id-string))
                        (add-triple patient-upi !codo:contractedVirusFrom host-upi)
                        ))))
             (make-close-contact-object patient-upi rs-upi)
            )))
  (commit-triple-store)
  )



(defun analyze-pid-stringsG () 
  ; For strings of the form: "Contact of 16752 and 16753 and 19794"
  ; This is a general function that will replace many of the functions previously above
  ; It should match any string that starts with "Contact of" and will find each of the patient Ids.
  ; This should work for any string with patient Ids in it however, it will not process intervals of Ids.
  ; E.g. "P123-P125" would get processed by this function by just using the ids 123 and 125 and would not include 123
  ; Hence, the functions that process intervals must run before this function
  ; See analyze-pid-strings1 for more detailed comments
  (print "Entering pattern matching transforms G")
  (let ((results 
         (sparql:run-sparql 
          "SELECT ?p ?rs WHERE {?p codo:reasonString ?rs. FILTER(STRSTARTS(?rs, 'Contact'))}" :results-format :lists)))
    (loop for result in results do
          (let* (
                 (patient-upi (first result))
                 (patient-value (part->terse patient-upi))
                 (rs-upi (second result))
                 (rs (upi->value rs-upi))
                 (ids  (split-re "\\D+" rs))
                 )
            ;Need multiple-value-bind because match-re returns a boolean if it succeeds and then the actual matched string
            ;Don't need the boolean but need to bind to it anyway to get the string. This results in a compilation warning
            ;because the variable matched-p is not used
            (print rs)
            (print ids)
            (loop for id-string in ids do
                  (if (string= id-string "") (print "Ignoring blank id string") 
                    (let ((host-upi (first (freetext-get-unique-subjects (list 'match id-string)))))
                      (print (list patient-value id-string))
                      (add-triple patient-upi !codo:contractedVirusFrom host-upi)                  
                      (make-close-contact-object patient-upi rs-upi)))))))
  (commit-triple-store)
  )



(defun codo-regex-transforms ()
  (analyze-ad-hoc-pid-strings1)
  (analyze-ad-hoc-pid-strings2)
  (analyze-pid-strings1)
  (analyze-pid-strings2)
  (analyze-pid-strings3)
  (analyze-pid-strings8)
  (analyze-pid-strings9)
  (analyze-pid-stringsG))
  