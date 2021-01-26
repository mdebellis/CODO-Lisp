#|
Before running the following functions, make sure you follow the setup instructions in 
the document "Running Lisp to Access Allegrograph.pdf"

Make sure that the CODO knowledge graph is open
and that the codo package is registered:

(open-triple-store "CODORealData"  
  :triple-store-class 'remote-triple-store  
  :server "localhost"  :port 10035 :user "mdebellis" :password "df1559")

(register-namespace "codo" "http://www.isibang.ac.in/ns/codo#")
(register-namespace "schema" "https://schema.org/")

All functions assume that the codo triplestore is open and bound to *db*

Also, make sure that an index is created for the property codo:statePatientID. This can be done
in Gruff or in the listener:

(create-freetext-index "PatientIDs" :predicates (list !codo:statePatientID !codo:districtPatientID) :minimum-word-size 1)

(create-freetext-index "PlaceNames" :predicates (list !codo:geoName))

Note: order is important. Run the functions in this file first and then those in sparql-regex-transforms.
The earlier transforms remove data that may be problematic for later transforms.
The function at the bottom of this file (and sparql-regex-transforms) runs all the functions for you.
They were divided up into smaller functions to aid debugging. 

|#

(in-package :db.agraph.user) 

(defvar *test-modep* nil)

(defun enable-test-mode ()
  ;Set *test-modep* to t and run SPARQL query
  ;to create test property if it doesn't already exist
  (setq *test-modep* t)
  (sparql:run-sparql "INSERT DATA  {
	codo:reasonStringTD a owl:DatatypeProperty;
	rdfs:range xsd:string ;
	rdfs:label 'reason string TD' .
        codo:cityStringTD a owl:DatatypeProperty;
	rdfs:range xsd:string ;
	rdfs:label 'city string TD' .
	}")
  (commit-triple-store))

(defun disable-test-mode ()
  (setq *test-modep* nil)
  (sparql:run-sparql "DELETE {?p codo:reasonStringTD ?rstd} WHERE {?p codo:reasonStringTD ?rstd.}")
  (sparql:run-sparql "DELETE {?p codo:cityStringTD ?cstd} WHERE {?p codo:cityStringTD ?cstd.}")
  (sparql:run-sparql "DELETE DATA  {
	codo:reasonStringTD a owl:DatatypeProperty ;
	rdfs:subPropertyOf owl:topDataProperty ;
	rdfs:range xsd:string ;
	rdfs:label 'reason string TD' .
        codo:cityStringTD a owl:DatatypeProperty;
	rdfs:range xsd:string ;
	rdfs:label 'city string TD' .
	}")
  ;Delete any remaining outdateString
  (sparql:run-sparql "DELETE  {?p codo:outdateString ?outstr.} WHERE {?p codo:outdateString ?outstr.}")
  (commit-triple-store))	

(defun run-initial-codo-sparql-transforms ()
  (print "Entering initial transforms")
  ; First transform deletes test data
  (sparql:run-sparql "DELETE {?td ?p ?o. } WHERE {?td a codo:TestData.
		?td ?p ?o.}")
  ;Check for pStrings with non-zero value and process accordingly
  (sparql:run-sparql "DELETE {?p codo:pString ?ps.} 
   INSERT {?p codo:contractedVirusFrom ?sh.} 
   WHERE {?p codo:statePatientID ?pid. ?p codo:pString ?ps. ?sh codo:statePatientID ?ps. 
   FILTER (?ps != '0')}")
  ;Delete all the 0 pString
  (sparql:run-sparql "DELETE {?p codo:pString ?ps.} WHERE {?p codo:pString ?ps.
  FILTER (?ps = '0')}")
  
  ;Add a hasSpouse value to the patient
  (sparql:run-sparql "DELETE {?p codo:relationString ?rs.} 
  INSERT {?p codo:hasSpouse ?sh.} 
  WHERE {?p codo:relationString ?rs.
  ?p codo:statePatientID ?pid. 
  BIND (SUBSTR(?rs, 1,1) AS ?rt). 
  BIND (SUBSTR(?rs, 2) AS ?rid).
  ?sh codo:statePatientID ?rid. 
  FILTER (?rt = 'S')}")
  
  ;Add a hasCloseRelationship value to the patient
  (sparql:run-sparql "DELETE {?p codo:relationString ?rs.} INSERT {?p codo:hasCloseRelationship ?sh.}
  WHERE {?p codo:relationString ?rs. ?p codo:statePatientID ?pid. BIND (SUBSTR(?rs, 1,1) AS ?rt). 
  BIND (SUBSTR(?rs, 2) AS ?rid). ?sh codo:statePatientID ?rid. FILTER (?rt = 'C')}")
  ;Add a hasFamilyRelationship value to the patient
  (sparql:run-sparql "DELETE {?p codo:relationString ?rs.} INSERT {?p codo:hasFamilyMember ?sh.} 
  WHERE {?p codo:relationString ?rs. ?p codo:statePatientID ?pid. BIND (SUBSTR(?rs, 1,1) AS ?rt). 
  BIND (SUBSTR(?rs, 2) AS ?rid). ?sh codo:statePatientID ?rid. FILTER (?rt = 'F')}") 
  
  ;Add a hasDaughter value to the patient
  (sparql:run-sparql "DELETE {?p codo:relationString ?rs.} INSERT {?p codo:hasDaughter ?sh.} 
  WHERE {?p codo:relationString ?rs. ?p codo:statePatientID ?pid. BIND (SUBSTR(?rs, 1,1) AS ?rt). 
  BIND (SUBSTR(?rs, 2) AS ?rid). ?sh codo:statePatientID ?rid. FILTER (?rt = 'D')}") 
  
  ;Add a hasTravelCompanion value to the patient
  (sparql:run-sparql "DELETE {?p codo:relationString ?rs.} INSERT {?p codo:hasTravelCompanion ?sh.}
  WHERE {?p codo:relationString ?rs.?p codo:statePatientID ?pid.
  BIND (SUBSTR(?rs, 1,1) AS ?rt). BIND (SUBSTR(?rs, 2) AS ?rid).
  ?sh codo:statePatientID ?rid. FILTER (?rt = 'T')}")
  ;Commit the changes
  (commit-triple-store)
  )

#|

Run the following queries after initial transformations function. Both should return no triples:

SELECT ?p ?rs
WHERE {
  ?p codo:relationString ?rs. 
} 

SELECT ?p ?ps
WHERE {
  ?p codo:pString ?ps. 
} 

|#


(defun run-second-codo-sparql-transforms ()
  (print "Entering second transforms")
  ; First transform deletes reason strings with unknown value
  (sparql:run-sparql "DELETE	{?p codo:reasonString ?rs.} WHERE {?p codo:reasonString ?rs.
		FILTER(LCASE(?rs) = 'unknown')}")
  ;First transform checks for strings with two place names. Need to check these first because 
  ;If we run the later transforms they will match the first place name and neglect to process the second
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
   INSERT { 
	?nexp a codo:InfectedCo-Passenger.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:hasTravelCompanion ?sh.
	?p codo:travelledFrom ?place1.
	?nexp codo:travelledFrom ?place1.
	?p codo:travelledFrom ?place2.
	?nexp codo:travelledFrom ?place2.
	?sh codo:travelledFrom ?place1.
	?sh codo:travelledFrom ?place2.
	}
   WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs. ?p codo:contractedVirusFrom ?sh.
	BIND
	(IF (CONTAINS(?rs, 'Jammu and Kashmir'), codo:Jamu,
	(IF (CONTAINS(?rs, 'Daman and Diu'), codo:Daman,
	(IF (CONTAINS(?rs, 'Hassan and Kodagu'), codo:Hassan,
	(IF (CONTAINS(?rs, 'Hindupur and Anantpur'), codo:Hindupur,
	owl:Nothing))))))) AS ?place1).
	BIND
	(IF (CONTAINS(?rs, 'Jammu and Kashmir'), codo:Kashmir,
	(IF (CONTAINS(?rs, 'Daman and Diu'), codo:Diu,
	(IF (CONTAINS(?rs, 'Hassan and Kodagu'), codo:Kodagu,
	(IF (CONTAINS(?rs, 'Hindupur and Anantpur'), codo:Anantpur,
	owl:Nothing))))))) AS ?place2).
	BIND (IRI(CONCAT('http://www.isibang.ac.in/ns/codo#InfectedCo-Passenger-', ?pid)) AS ?nexp).
	FILTER(?place1 != owl:Nothing)}") 
  ;Same as transform above except for patients where suspected host is unknown
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedCo-Passenger.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp. ?p codo:travelledFrom ?place1.
	?nexp codo:travelledFrom ?place1. ?p codo:travelledFrom ?place2.
	?nexp codo:travelledFrom ?place2.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND
  #Wards of Cities
	(IF (CONTAINS(?rs, 'Jammu and Kashmir'), codo:Jamu,
	(IF (CONTAINS(?rs, 'Daman and Diu'), codo:Daman,
	(IF (CONTAINS(?rs, 'Hassan and Kodagu'), codo:Hassan,
	(IF (CONTAINS(?rs, 'Hindupur and Anantpur'), codo:Hindupur,
	owl:Nothing))))))) AS ?place1).
	BIND
	(IF (CONTAINS(?rs, 'Jammu and Kashmir'), codo:Kashmir,
	(IF (CONTAINS(?rs, 'Daman and Diu'), codo:Diu,
	(IF (CONTAINS(?rs, 'Hassan and Kodagu'), codo:Kodagu,
	(IF (CONTAINS(?rs, 'Hindupur and Anantpur'), codo:Anantpur,
	owl:Nothing))))))) AS ?place2).
	BIND (IRI(CONCAT('http://www.isibang.ac.in/ns/codo#InfectedCo-Passenger-', ?pid)) AS ?nexp).
	FILTER(?place1 != owl:Nothing)}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedCo-Passenger. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:hasTravelCompanion ?sh. ?p codo:travelledFrom ?place.
	?nexp codo:travelledFrom ?place. ?sh codo:travelledFrom ?place.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs. ?p codo:contractedVirusFrom ?sh.
	BIND
	(IF (CONTAINS(?rs, 'Ward 135'), codo:Ward135,
# Cities
# Start with cities because if the city is there the additional info (state, country)
# is in the ontology and we can ignore that data in the string
	(IF (CONTAINS(?rs, 'Abu Dhabi'), codo:AbuDhabi, (IF (CONTAINS(?rs, 'Ahmedabad'), codo:Ahmedabad, (IF (CONTAINS(?rs, 'Ajmer'), codo:Ajmer, (IF (CONTAINS(?rs, 'Amsterdam'), codo:Amsterdam,
	(IF (CONTAINS(?rs, 'Athens'), codo:Athens, (IF (CONTAINS(?rs, 'Bagalkote'), codo:Bagalkote, (IF (CONTAINS(?rs, 'Baroda'), codo:Baroda,
	(IF (CONTAINS(?rs, 'Bangalore'), codo:Bangalore, (IF (CONTAINS(?rs, 'Bangalore-Urban'), codo:Bangalore, (IF (CONTAINS(?rs, 'Belgavi'), codo:Belgavi,
	(IF (CONTAINS(?rs, 'Bellary'), codo:Bellary, (IF (CONTAINS(?rs, 'Bidar'), codo:Bidar, 
	(IF (CONTAINS(?rs, 'Channagiri'), codo:Channagiri,(IF (CONTAINS(?rs, 'Chamarajanagar'), codo:Chamarajanagar,(IF (CONTAINS(?rs, 'Chennai'), codo:Chennai, (IF (CONTAINS(?rs, 'Chikballarpur'), codo:Chikballarpur,
	(IF (CONTAINS(?rs, 'Chikkamagalur'), codo:Chikmagalur, (IF (CONTAINS(?rs, 'Chitradurga'), codo:Chitradurga, (IF (CONTAINS(?rs, 'Colombo'), codo:Colombo,(IF (CONTAINS(?rs, 'Dammam'), codo:Dammam,
	(IF (CONTAINS(?rs, 'Davangere'), codo:Davangere, (IF (CONTAINS(?rs, 'Dharwad'), codo:Dharwad,(IF (CONTAINS(?rs, 'Debaspete'), codo:Debaspete, (IF (CONTAINS(?rs, 'Delhi'), codo:Delhi,
	(IF (CONTAINS(?rs, 'Doha'), codo:Doha, (IF (CONTAINS(?rs, 'Dubai'), codo:Dubai, (IF (CONTAINS(?rs, 'Edinburgh'), codo:Edinburgh, (IF (CONTAINS(?rs, 'Gadag'), codo:Gadag, 
	(IF (CONTAINS(?rs, 'Goa'), codo:Goa, (IF (CONTAINS(?rs, 'Gunturu'), codo:Gunturu, (IF (CONTAINS(?rs, 'Hassan'), codo:Hassan, (IF (CONTAINS(?rs, 'Haveri'), codo:Haveri,(IF (CONTAINS(?rs, 'Hindupur'), codo:Hindupur,
	(IF (CONTAINS(?rs, 'Hubbali'), codo:Hubli, (IF (CONTAINS(?rs, 'Humnabad'), codo:Humnabad, 
	(IF (CONTAINS(?rs, 'Hyderabad'), codo:Hyderabad, (IF (CONTAINS(?rs, 'Jalgaon'), codo:Jalgaon,(IF (CONTAINS(?rs, 'Jeddah'), codo:Jeddah,(IF (CONTAINS(?rs, 'Kalburgi'), codo:Kalaburagi,
	(IF (CONTAINS(?rs, 'Jammu'), codo:Jammu, (IF (CONTAINS(?rs, 'Kolar'), codo:Kolar, (IF (CONTAINS(?rs, 'Kolkata'), codo:Kolkata,  (IF (CONTAINS(?rs, 'Kolhapur'), codo:Kolhapur, (IF (CONTAINS(?rs, 'Koppal'), codo:Koppal,
	(IF (CONTAINS(?rs, 'London'), codo:London, (IF (CONTAINS(?rs, 'Madikeri'), codo:Madikeri, (IF (CONTAINS(?rs, 'Madrid'), codo:Madrid, (IF (CONTAINS(?rs, 'Madurai'), codo:Madurai, 
	(IF (CONTAINS(?rs, 'Mandya'), codo:Mandya, (IF (CONTAINS(?rs, 'Mangalore'), codo:Mangalore, (IF (CONTAINS(?rs, 'Mecca'), codo:Mecca, (IF (CONTAINS(?rs, 'Mumbai'), codo:Mumbai, (IF (CONTAINS(?rs, 'Muscat'), codo:Muscat, 
	(IF (CONTAINS(?rs, 'Nandurbar'), codo:Nandurbar,(IF (CONTAINS(?rs, 'Nelamangala'), codo:Nelamangala, (IF (CONTAINS(?rs, 'New York'), codo:NewYorkCity,
	(IF (CONTAINS(?rs, 'Palghar'), codo:Palghar, (IF (CONTAINS(?rs, 'Panvel'), codo:Panvel, (IF (CONTAINS(?rs, 'Paris'), codo:Paris, (IF (CONTAINS(?rs, 'Pune'), codo:Pune, (IF (CONTAINS(?rs, 'Raichur'), codo:Raichur,
	(IF (CONTAINS(?rs, 'Rayachuru'), codo:Raichur, (IF (CONTAINS(?rs, 'Ratnagiri'), codo:Ratnagiri, (IF (CONTAINS(?rs, 'Ratnagiri'), codo:Ratnagiri, 
	(IF (CONTAINS(?rs, 'Ramanagar'), codo:Ramnagar, (IF (CONTAINS(?rs, 'Ramnagar'), codo:Ramnagar, (IF (CONTAINS(?rs, 'Riyadh'), codo:Riyadh, 
	(IF (CONTAINS(?rs, 'Sharjah'), codo:Sharjah, (IF (CONTAINS(?rs, 'Shivamogga'), codo:Shivamogga,(IF (CONTAINS(?rs, 'Shikharaji'), codo:Shikharaji, (IF (CONTAINS(?rs, 'Shivamogga'), codo:Shivamogga,
	(IF (CONTAINS(?rs, 'Singapore'), codo:Singapore, (IF (CONTAINS(?rs, 'Solapur'), codo:Solapur,(IF (CONTAINS(?rs, 'Surat'), codo:Surat, (IF (CONTAINS(?rs, 'Thane'), codo:Thane, (IF (CONTAINS(?rs, 'Tumkur'), codo:Tumkur,
	(IF (CONTAINS(?rs, 'Udupi'), codo:Udupi,(IF (CONTAINS(?rs, 'Vellore'), codo:Vellore, (IF (CONTAINS(?rs, 'Vijayapur'), codo:Vijayapura,
	(IF (CONTAINS(?rs, 'Vishakapatnam'), codo:Vishakapatnam,(IF (CONTAINS(?rs, 'Yadgir'), codo:Yadgir, 
	owl:Nothing))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) AS ?place).
	BIND (IRI(CONCAT('http://www.isibang.ac.in/ns/codo#InfectedCo-Passenger-', ?pid)) AS ?nexp).
	FILTER(?place != owl:Nothing)}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedCo-Passenger. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:hasTravelCompanion ?sh. ?p codo:travelledFrom ?place.
	?nexp codo:travelledFrom ?place. ?sh codo:travelledFrom ?place.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND
# States
	(IF (CONTAINS(?rs, 'Andhra Pradesh'), codo:AndhraPradesh, (IF (CONTAINS(?rs, 'Assam'), codo:Assam,
	(IF (CONTAINS(?rs, 'Bihar'), codo:Bihar, (IF (CONTAINS(?rs, 'Chattisgadh'), codo:Chattisgadh,
	(IF (CONTAINS(?rs, 'Haryana'), codo:Haryana, (IF (CONTAINS(?rs, 'Himachal Pradesh'), codo:HimachalPradesh, (IF (CONTAINS(?rs, 'Jharkhand'), codo:Jharkhand,
	(IF (CONTAINS(?rs, 'Kerala'), codo:Kerala, (IF (CONTAINS(?rs, 'Madhya Pradesh'), codo:MadhyaPradesh,
	(IF (CONTAINS(?rs, 'Manipur'), codo:Manipur, (IF (CONTAINS(?rs, 'Nagaland'), codo:Nagaland,  (IF (CONTAINS(?rs, 'Orissa'), codo:Orissa, 
	(IF (CONTAINS(?rs, 'Tamil Nadu'), codo:TamilNadu, (IF (CONTAINS(?rs, 'Telengana'), codo:Telengana, (IF (CONTAINS(?rs, 'Texas'), codo:Texas,
	(IF (CONTAINS(?rs, 'Uttar Pradesh'), codo:UttarPradesh, (IF (CONTAINS(?rs, 'West Bengal'), codo:WestBengal, (IF (CONTAINS(?rs, 'West Bangal'), codo:WestBengal,
#Districts
	(IF (CONTAINS(LCASE(?rs), 'andaman and nicobar'), codo:AndamanAndNicobarIslands, (IF (CONTAINS(LCASE(?rs), 'dakshin kannada'), codo:DakshinKannadaDistrict, 
	(IF (CONTAINS(?rs, 'Kodagu'), codo:KodaguDistrict,
	(IF (CONTAINS(?rs, 'Mysore'), codo:Mysore, (IF (CONTAINS(?rs, 'Raigadh'), codo:Raigadh, 
	(IF (CONTAINS(?rs, 'Uttarakhand'), codo:Uttarakhand, (IF (CONTAINS(?rs, 'Uttar Kannada'), codo:UttarKannada,(IF (CONTAINS(?rs, 'Uttara Kannada'), codo:UttarKannada,
# Countries 
	(IF (CONTAINS(?rs, 'Argentina'), codo:Argentina,(IF (CONTAINS(?rs, 'Bangladesh'), codo:Bangladesh,(IF (CONTAINS(?rs, 'Bahrain'), codo:Bahrain, 
	(IF (CONTAINS(?rs, 'Brazil'), codo:Brazil, 
	(IF (CONTAINS(?rs, 'Domestic travel'), codo:India,(IF (CONTAINS(?rs, 'Interdistrict travel'), codo:India,(IF (CONTAINS(?rs, 'France'), codo:France,
	(IF (CONTAINS(?rs, 'Germany'), codo:Germany, (IF (CONTAINS(?rs, 'Greece'), codo:Greece, (IF (CONTAINS(?rs, 'Guyana'), codo:Guyana, 
	(IF (CONTAINS(?rs, 'Indonesia'), codo:Indonesia, (IF (CONTAINS(?rs, 'Iraq'), codo:Iraq, (IF (CONTAINS(?rs, 'Ireland'), codo:Ireland,
	(IF (CONTAINS(?rs, 'Italy'), codo:Italy, (IF (CONTAINS(?rs, 'Kuwait'), codo:Kuwait, (IF (CONTAINS(?rs, 'Malaysia'), codo:Malaysia,(IF (CONTAINS(?rs, 'Nepal'), codo:Nepal, 
	(IF (CONTAINS(?rs, 'Philippines'), codo:Philippines,(IF (CONTAINS(?rs, 'Saudi Arabia'), codo:SaudiArabia,(IF (CONTAINS(?rs, 'South Africa'), codo:SouthAfrica,
	(IF (CONTAINS(?rs, 'Qatar'), codo:Qatar, (IF (CONTAINS(?rs, 'Spain'), codo:Spain, 
	(IF (CONTAINS(?rs, 'Switzerland'), codo:Switzerland, (IF (CONTAINS(?rs, 'Turkey'), codo:Turkey, (IF (CONTAINS(?rs, 'UAE'), codo:UnitedArabEmirates,
	(IF (CONTAINS(?rs, 'UK'), codo:UK, (IF (CONTAINS(?rs, 'United States'), codo:UnitedStates,
# Regions 
	(IF (CONTAINS(?rs, 'Punjab'), codo:Punjab, 
	owl:Nothing))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) AS ?place).
	BIND (IRI(CONCAT('http://www.isibang.ac.in/ns/codo#InfectedCo-Passenger-', ?pid)) AS ?nexp).
	FILTER(?place != owl:Nothing)}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedSpouse. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:hasSpouse ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedSpouse-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Spouse')}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedSpouse. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:hasWife ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedSpouse-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Wife')}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedSpouse. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:hasHusband ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedSpouse-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Husband')
	}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedDaughter.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp. ?p codo:hasDaughter ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedDaughter-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Daughter')
	}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedSister.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:hasSister ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedSister-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Sister')
	}")
  (sparql:run-sparql "DELETE {
	?p codo:reasonString ?rs.} INSERT {?nexp a codo:InfectedMother.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:hasMother ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedMother-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Mother')
	}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedMother. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?ps codo:hasMother ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh. ?p codo:hasSpouse ?ps.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedMother-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(?rs,'Mother in law'))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedMother.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedMother-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(?rs,'Mother in law'))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedFather. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:hasFather ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedFather-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Father')
	}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedSon. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:hasSon ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedSon-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Son')}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedBrother. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:hasBrother ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedBrother-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Brother')}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedCousin.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:hasCousin ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedCousin-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Cousin')}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedNiece. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:hasNiece ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedNiece-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Daughter of Brother')}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedDomesticHelp.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp. ?p codo:hasDomesticHelp ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedDomesticHelp-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(?rs, 'Domestic help'))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedCo-Passenger.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp. ?p codo:hasTravelCompanion ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedDomesticHelp-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(?rs, 'Co passenger'))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:ContactWithHealthWorkers. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?sh a codo:HealthCareProfessional.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#ContactWithHealthWorkers-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(?rs, 'Contact with Health Workers'))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:ContactWithHealthWorkers.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp. ?sh a codo:HealthCareProfessional.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#ContactWithHealthWorkers-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(?rs, 'Contact with Health Workers'))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:ContactWithHealthWorkers. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#ContactWithHealthWorkers-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(?rs, 'Contact with Health Workers'))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:ExposureViaCongregation. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid.
	?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#ExposureViaCongregation-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(LCASE(?rs), 'attended congregation') || CONTAINS(LCASE(?rs), 'attended the congregation'))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:CloseContact. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#CloseContact-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Contact' || ?rs = 'Contact of AP patient')}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedRoomMate.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp. ?p codo:hasRoommate ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedRoomMate-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(?rs,'Roommate'))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedViaPoliceWork. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p a codo:PolicePerson.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedViaPoliceWork-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(LCASE(?rs),'police'))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedViaHealthcareWork.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p a codo:HealthCareProfessional.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedViaHealthcareWorkb-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(LCASE(?rs),'healthcare worker') || CONTAINS(LCASE(?rs),'hospital staff') || CONTAINS(LCASE(?rs),'health care worker')
        || CONTAINS(LCASE(?rs),'doctor'))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedViaJob. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedViaJob-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(?rs,'Pharma Company Worker') || CONTAINS(LCASE(?rs),'ictc counselor') || CONTAINS(LCASE(?rs),'anganwadi worker'))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedNephew.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:hasNephew ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedNephew-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Son of sister')}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedFamilyMember.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp. ?p codo:kinswomen ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedFamilyMember-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Daughter of Brother in law' || ?rs = 'Daughter in law' || ?rs = 'Wife of brother')}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedFamilyMember.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp. ?p codo:kinsman ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedFamilyMember-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Son of Brother in law')}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedNeighbor.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:hasNeighbor ?sh.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	?p codo:contractedVirusFrom ?sh.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedNeighbor-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(?rs,'Neighbor'))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedNeighbor.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedNeighbor-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(?rs,'Neighbor'))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedViaJob.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p a codo:SecurityGuard.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedNeighbor-', ?pid))) AS ?nexp).
	FILTER(?rs = 'Security Guard')}")
  ;Commit the changes
  (commit-triple-store)
  )


(defun run-third-codo-sparql-transforms ()
  (print "Entering third transforms")
  ; This is different order than in SPARQL files. Need to handle the strings
  ; with "Ward 135" first because otherwise they will match the regex for numeric strings
  (sparql:run-sparql "DELETE {
	?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedCo-Passenger.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:hasTravelCompanion ?sh.
	?p codo:travelledFrom ?place.
	?nexp codo:travelledFrom ?place.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND
#Wards of Cities
	(IF (CONTAINS(?rs, 'Ward 135'), codo:Ward135,
# Cities
# Start with cities because if the city is there the additional info (state, country)
# is in the ontology and we can ignore that data in the string
	(IF (CONTAINS(?rs, 'Abu Dhabi'), codo:AbuDhabi, (IF (CONTAINS(?rs, 'Ahmedabad'), codo:Ahmedabad, (IF (CONTAINS(?rs, 'Ajmer'), codo:Ajmer, (IF (CONTAINS(?rs, 'Amsterdam'), codo:Amsterdam,
	(IF (CONTAINS(?rs, 'Athens'), codo:Athens, (IF (CONTAINS(?rs, 'Bagalkote'), codo:Bagalkote, (IF (CONTAINS(?rs, 'Baroda'), codo:Baroda,
	(IF (CONTAINS(?rs, 'Bangalore'), codo:Bangalore, (IF (CONTAINS(?rs, 'Bangalore-Urban'), codo:Bangalore, (IF (CONTAINS(?rs, 'Belgavi'), codo:Belgavi,
	(IF (CONTAINS(?rs, 'Bellary'), codo:Bellary, (IF (CONTAINS(?rs, 'Bidar'), codo:Bidar, 
	(IF (CONTAINS(?rs, 'Channagiri'), codo:Channagiri,(IF (CONTAINS(?rs, 'Chamarajanagar'), codo:Chamarajanagar,(IF (CONTAINS(?rs, 'Chennai'), codo:Chennai, (IF (CONTAINS(?rs, 'Chikballarpur'), codo:Chikballarpur,
	(IF (CONTAINS(?rs, 'Chikkamagalur'), codo:Chikmagalur, (IF (CONTAINS(?rs, 'Chitradurga'), codo:Chitradurga, (IF (CONTAINS(?rs, 'Colombo'), codo:Colombo,(IF (CONTAINS(?rs, 'Dammam'), codo:Dammam,
	(IF (CONTAINS(?rs, 'Davangere'), codo:Davangere, (IF (CONTAINS(?rs, 'Dharwad'), codo:Dharwad,(IF (CONTAINS(?rs, 'Debaspete'), codo:Debaspete, (IF (CONTAINS(?rs, 'Delhi'), codo:Delhi,
	(IF (CONTAINS(?rs, 'Doha'), codo:Doha, (IF (CONTAINS(?rs, 'Dubai'), codo:Dubai, (IF (CONTAINS(?rs, 'Edinburgh'), codo:Edinburgh, (IF (CONTAINS(?rs, 'Gadag'), codo:Gadag, 
	(IF (CONTAINS(?rs, 'Goa'), codo:Goa, (IF (CONTAINS(?rs, 'Gunturu'), codo:Gunturu, (IF (CONTAINS(?rs, 'Hassan'), codo:Hassan, (IF (CONTAINS(?rs, 'Haveri'), codo:Haveri,(IF (CONTAINS(?rs, 'Hindupur'), codo:Hindupur,
	(IF (CONTAINS(?rs, 'Hubbali'), codo:Hubli, (IF (CONTAINS(?rs, 'Humnabad'), codo:Humnabad, 
	(IF (CONTAINS(?rs, 'Hyderabad'), codo:Hyderabad, (IF (CONTAINS(?rs, 'Jalgaon'), codo:Jalgaon,(IF (CONTAINS(?rs, 'Jeddah'), codo:Jeddah,(IF (CONTAINS(?rs, 'Kalburgi'), codo:Kalaburagi,
	(IF (CONTAINS(?rs, 'Jammu'), codo:Jammu, (IF (CONTAINS(?rs, 'Kolar'), codo:Kolar, (IF (CONTAINS(?rs, 'Kolkata'), codo:Kolkata,  (IF (CONTAINS(?rs, 'Kolhapur'), codo:Kolhapur, (IF (CONTAINS(?rs, 'Koppal'), codo:Koppal,
	(IF (CONTAINS(?rs, 'London'), codo:London, (IF (CONTAINS(?rs, 'Madikeri'), codo:Madikeri, (IF (CONTAINS(?rs, 'Madrid'), codo:Madrid, (IF (CONTAINS(?rs, 'Madurai'), codo:Madurai, 
	(IF (CONTAINS(?rs, 'Mandya'), codo:Mandya, (IF (CONTAINS(?rs, 'Mangalore'), codo:Mangalore, (IF (CONTAINS(?rs, 'Mecca'), codo:Mecca, (IF (CONTAINS(?rs, 'Mumbai'), codo:Mumbai, (IF (CONTAINS(?rs, 'Muscat'), codo:Muscat, 
	(IF (CONTAINS(?rs, 'Nandurbar'), codo:Nandurbar,(IF (CONTAINS(?rs, 'Nelamangala'), codo:Nelamangala, (IF (CONTAINS(?rs, 'New York'), codo:NewYorkCity,
	(IF (CONTAINS(?rs, 'Palghar'), codo:Palghar, (IF (CONTAINS(?rs, 'Panvel'), codo:Panvel, (IF (CONTAINS(?rs, 'Paris'), codo:Paris, (IF (CONTAINS(?rs, 'Pune'), codo:Pune, (IF (CONTAINS(?rs, 'Raichur'), codo:Raichur,
	(IF (CONTAINS(?rs, 'Rayachuru'), codo:Raichur, (IF (CONTAINS(?rs, 'Ratnagiri'), codo:Ratnagiri, (IF (CONTAINS(?rs, 'Ratnagiri'), codo:Ratnagiri, 
	(IF (CONTAINS(?rs, 'Ramanagar'), codo:Ramnagar, (IF (CONTAINS(?rs, 'Ramnagar'), codo:Ramnagar, (IF (CONTAINS(?rs, 'Riyadh'), codo:Riyadh, 
	(IF (CONTAINS(?rs, 'Sharjah'), codo:Sharjah, (IF (CONTAINS(?rs, 'Shivamogga'), codo:Shivamogga,(IF (CONTAINS(?rs, 'Shikharaji'), codo:Shikharaji, (IF (CONTAINS(?rs, 'Shivamogga'), codo:Shivamogga,
	(IF (CONTAINS(?rs, 'Singapore'), codo:Singapore, (IF (CONTAINS(?rs, 'Solapur'), codo:Solapur,(IF (CONTAINS(?rs, 'Surat'), codo:Surat, (IF (CONTAINS(?rs, 'Thane'), codo:Thane, (IF (CONTAINS(?rs, 'Tumkur'), codo:Tumkur,
	(IF (CONTAINS(?rs, 'Udupi'), codo:Udupi,(IF (CONTAINS(?rs, 'Vellore'), codo:Vellore, (IF (CONTAINS(?rs, 'Vijayapur'), codo:Vijayapura,
	(IF (CONTAINS(?rs, 'Vishakapatnam'), codo:Vishakapatnam,(IF (CONTAINS(?rs, 'Yadgir'), codo:Yadgir, 
	owl:Nothing))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) AS ?place).
	BIND (IRI(CONCAT('http://www.isibang.ac.in/ns/codo#InfectedCo-Passenger-', ?pid)) AS ?nexp).
	FILTER(?place != owl:Nothing)}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedCo-Passenger.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:hasTravelCompanion ?sh. ?p codo:travelledFrom ?place.
	?nexp codo:travelledFrom ?place.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND
# States
        (IF (CONTAINS(?rs, 'Andhra Pradesh'), codo:AndhraPradesh, (IF (CONTAINS(?rs, 'Assam'), codo:Assam,
	(IF (CONTAINS(?rs, 'Bihar'), codo:Bihar, (IF (CONTAINS(?rs, 'Chattisgadh'), codo:Chattisgadh,
	(IF (CONTAINS(?rs, 'Haryana'), codo:Haryana, (IF (CONTAINS(?rs, 'Himachal Pradesh'), codo:HimachalPradesh, (IF (CONTAINS(?rs, 'Jharkhand'), codo:Jharkhand,
	(IF (CONTAINS(?rs, 'Kerala'), codo:Kerala, (IF (CONTAINS(?rs, 'Madhya Pradesh'), codo:MadhyaPradesh,
	(IF (CONTAINS(?rs, 'Manipur'), codo:Manipur, (IF (CONTAINS(?rs, 'Nagaland'), codo:Nagaland,  (IF (CONTAINS(?rs, 'Orissa'), codo:Orissa, 
	(IF (CONTAINS(?rs, 'Tamil Nadu'), codo:TamilNadu, (IF (CONTAINS(?rs, 'Telengana'), codo:Telengana, (IF (CONTAINS(?rs, 'Texas'), codo:Texas,
	(IF (CONTAINS(?rs, 'Uttar Pradesh'), codo:UttarPradesh, (IF (CONTAINS(?rs, 'West Bengal'), codo:WestBengal, (IF (CONTAINS(?rs, 'West Bangal'), codo:WestBengal,
#Districts
	(IF (CONTAINS(LCASE(?rs), 'andaman and nicobar'), codo:AndamanAndNicobarIslands, (IF (CONTAINS(LCASE(?rs), 'dakshin kannada'), codo:DakshinKannadaDistrict, 
	(IF (CONTAINS(?rs, 'Kodagu'), codo:KodaguDistrict,
	(IF (CONTAINS(?rs, 'Mysore'), codo:Mysore, (IF (CONTAINS(?rs, 'Raigadh'), codo:Raigadh, 
	(IF (CONTAINS(?rs, 'Uttarakhand'), codo:Uttarakhand, (IF (CONTAINS(?rs, 'Uttar Kannada'), codo:UttarKannada,(IF (CONTAINS(?rs, 'Uttara Kannada'), codo:UttarKannada,
# Countries 
	(IF (CONTAINS(?rs, 'Argentina'), codo:Argentina,(IF (CONTAINS(?rs, 'Bangladesh'), codo:Bangladesh,(IF (CONTAINS(?rs, 'Bahrain'), codo:Bahrain, 
	(IF (CONTAINS(?rs, 'Brazil'), codo:Brazil, 
	(IF (CONTAINS(?rs, 'Domestic travel'), codo:India,(IF (CONTAINS(?rs, 'Interdistrict travel'), codo:India,(IF (CONTAINS(?rs, 'France'), codo:France,
	(IF (CONTAINS(?rs, 'Germany'), codo:Germany, (IF (CONTAINS(?rs, 'Greece'), codo:Greece, (IF (CONTAINS(?rs, 'Guyana'), codo:Guyana, 
	(IF (CONTAINS(?rs, 'Indonesia'), codo:Indonesia, (IF (CONTAINS(?rs, 'Iraq'), codo:Iraq, (IF (CONTAINS(?rs, 'Ireland'), codo:Ireland,
	(IF (CONTAINS(?rs, 'Italy'), codo:Italy, (IF (CONTAINS(?rs, 'Kuwait'), codo:Kuwait, (IF (CONTAINS(?rs, 'Malaysia'), codo:Malaysia,(IF (CONTAINS(?rs, 'Nepal'), codo:Nepal, 
	(IF (CONTAINS(?rs, 'Philippines'), codo:Philippines,(IF (CONTAINS(?rs, 'Saudi Arabia'), codo:SaudiArabia,(IF (CONTAINS(?rs, 'South Africa'), codo:SouthAfrica,
	(IF (CONTAINS(?rs, 'Qatar'), codo:Qatar, (IF (CONTAINS(?rs, 'Spain'), codo:Spain, 
	(IF (CONTAINS(?rs, 'Switzerland'), codo:Switzerland, (IF (CONTAINS(?rs, 'Turkey'), codo:Turkey, (IF (CONTAINS(?rs, 'UAE'), codo:UnitedArabEmirates,
	(IF (CONTAINS(?rs, 'UK'), codo:UK, (IF (CONTAINS(?rs, 'United States'), codo:UnitedStates,
# Regions 
	(IF (CONTAINS(?rs, 'Punjab'), codo:Punjab, 
	owl:Nothing))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) AS ?place).

	FILTER(?place != owl:Nothing)}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedSpouse.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedSpouse-', ?pid))) AS ?nexp).
	OPTIONAL{?p codo:contractedVirusFrom ?sh.}
	FILTER((?rs = 'Spouse') && !bound(?sh))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedSpouse. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedSpouse-', ?pid))) AS ?nexp).
	OPTIONAL{?p codo:contractedVirusFrom ?sh.}
	FILTER((?rs = 'Wife') && !bound(?sh))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedSpouse.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid.
	?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedSpouse-', ?pid))) AS ?nexp).
	OPTIONAL{?p codo:contractedVirusFrom ?sh.}
	FILTER((?rs = 'Husband') && !bound(?sh))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedDaughter.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedDaughter-', ?pid))) AS ?nexp).
	OPTIONAL{?p codo:contractedVirusFrom ?sh.}
	FILTER((?rs = 'Daughter') && !bound(?sh))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedSister.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid.
	?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedSister-', ?pid))) AS ?nexp).
	OPTIONAL{?p codo:contractedVirusFrom ?sh.}
	FILTER((?rs = 'Sister') && !bound(?sh))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedMother.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedMother-', ?pid))) AS ?nexp).
	OPTIONAL{?p codo:contractedVirusFrom ?sh.}
	FILTER((?rs = 'Mother') && !bound(?sh))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedFather.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedFather-', ?pid))) AS ?nexp).
	OPTIONAL{?p codo:contractedVirusFrom ?sh.}
	FILTER((?rs = 'Father') && !bound(?sh))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedSon. ?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedSon-', ?pid))) AS ?nexp).
	OPTIONAL{?p codo:contractedVirusFrom ?sh.}
	FILTER((?rs = 'Son') && !bound(?sh))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedBrother.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedBrother-', ?pid))) AS ?nexp).
	OPTIONAL{?p codo:contractedVirusFrom ?sh.}
	FILTER((?rs = 'Brother') && !bound(?sh))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedCousin.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid.
	?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedCousin-', ?pid))) AS ?nexp).
	OPTIONAL{?p codo:contractedVirusFrom ?sh.}
	FILTER((?rs = 'Cousin') && !bound(?sh))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedDomesticHelp.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid.
	?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedDomesticHelp-', ?pid))) AS ?nexp).
	OPTIONAL{?p codo:contractedVirusFrom ?sh.}
	FILTER(CONTAINS(?rs, 'Domestic help') && !bound(?sh))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:InfectedCo-Passenger.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid.
	?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#InfectedDomesticHelp-', ?pid))) AS ?nexp).
	OPTIONAL{?p codo:contractedVirusFrom ?sh.}
	FILTER(CONTAINS(?rs, 'Co passenger') && !bound(?sh))}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        INSERT {?nexp a codo:SecondaryContact.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#SecondaryContact-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(LCASE(?rs), 'secondary contact'))}")
  (sparql:run-sparql "DELETE {
	?p codo:reasonString ?rs.}
        INSERT { 
	?dg a codo:COVID-19Diagnosis.
	?dg a codo:UnderTracing.
	?p codo:hasDiagnosis ?dg.}
        WHERE {
	?p codo:statePatientID ?pid.
	?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#UnderTracingDiagnosis-', ?pid))) AS ?dg).
	FILTER(CONTAINS(LCASE(?rs), 'under tracing'))
	}")
  (sparql:run-sparql "DELETE {
	?p codo:reasonString ?rs.}
        INSERT { 
	?dg a codo:COVID-19Diagnosis.
	?dg a codo:PreSurgeryTest.
	?p codo:hasDiagnosis ?dg.}
        WHERE {
	?p codo:statePatientID ?pid.
	?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#PreSurgeryTestDiagnosis-', ?pid))) AS ?dg).
	FILTER(CONTAINS(LCASE(?rs), 'pre -op') || CONTAINS(LCASE(?rs), 'pre-surgery'))
	}")
  (sparql:run-sparql "DELETE {
	?p codo:reasonString ?rs.}
        INSERT { 
	?dg a codo:COVID-19Diagnosis.
	?dg a codo:PregnancyScreening.
	?p codo:hasDiagnosis ?dg.}
        WHERE {
	?p codo:statePatientID ?pid.
	?p codo:reasonString ?rs.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#PregnancyScreeningDiagnosis-', ?pid))) AS ?dg).
	FILTER(CONTAINS(LCASE(?rs), 'pregnant'))
	}")
  (sparql:run-sparql "DELETE {
	?p codo:reasonString ?rs.}
        INSERT { 
	?nexp a codo:CloseContact.
	?p1 a schema:Patient.
	?p1 codo:districtPatientID ?rs.
	?p codo:suspectedReasonOfCatchingCovid-19 ?nexp.
	?p codo:contractedVirusFrom ?p1.
	?p codo:hasRelationship ?p1. 
	}
        WHERE {
	?p codo:reasonString ?rs.
	?p codo:statePatientID ?pid.
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#Patient-', ?rs))) AS ?p1).
	BIND (IRI((CONCAT('http://www.isibang.ac.in/ns/codo#CloseContact-', ?pid))) AS ?nexp).
	FILTER(CONTAINS(?rs, 'CKB-'))
	}")
  (sparql:run-sparql "DELETE {?p codo:reasonString ?rs.}
        WHERE {?p codo:statePatientID ?pid. ?p codo:reasonString ?rs.
	FILTER(CONTAINS(?rs, 'No details') || ?rs = ''|| ?rs = ' ')}")
  ;Commit the changes
  (commit-triple-store))

  

  
(defun run-codo-sparql-transforms ()
  (run-initial-codo-sparql-transforms)
  (run-second-codo-sparql-transforms)
  (run-third-codo-sparql-transforms))
  
  
  
  
  
  
  
  