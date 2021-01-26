;File to deal with City strings and admission dates

(in-package :db.agraph.user)

(defun codo-city-transforms ()
  (print "Entering City transforms")
  (sparql:run-sparql "DELETE {?p codo:cityString ?cstring.}
INSERT {?p codo:isCurrentResidentOf codo:Bangalore-Urban;
           codo:isPermanentResidentOf codo:Anantapura.} 
WHERE {?p codo:cityString ?cstring.
        filter(?cstring = 'Bangalore-Urban although from Ananthpura in AP' || CONTAINS(?cstring,'Bangalore-Urban although resident of Ananthpur'))}")
  (sparql:run-sparql "DELETE {?p codo:cityString ?cstring.}
INSERT {?p codo:isCurrentResidentOf codo:Bidar;
           codo:isPermanentResidentOf codo:PaheliChouki.} 
WHERE {?p codo:cityString ?cstring.
        filter(?cstring = 'Bidar although resident of Paheli Chouki in Hyderabad')}")
  (sparql:run-sparql "DELETE {?p codo:cityString ?cstring.}
INSERT {?p codo:isCurrentResidentOf codo:Bidar;
           codo:isPermanentResidentOf codo:Hyderabad.} 
WHERE {?p codo:cityString ?cstring.
        filter(CONTAINS(?cstring, 'Bidar and resident of Hyderabad'))}")
  (sparql:run-sparql "DELETE {?p codo:cityString ?cstring.}
INSERT {?p codo:isCurrentResidentOf codo:Bidar;
           codo:isPermanentResidentOf codo:PaheliChouki.} 
WHERE {?p codo:cityString ?cstring.
        filter(?cstring = 'Bidar although resident of Paheli Chouki in Hyderabad')}")	
  (sparql:run-sparql "DELETE {?p codo:cityString ?cstring.}
INSERT {?p codo:isCurrentResidentOf ?crcity;
           codo:isPermanentResidentOf ?prcity.} 
WHERE {?p codo:cityString ?cstring.
        BIND(STRBEFORE(?cstring, ' although resident of ') AS ?crstring).
		BIND(STRAFTER(?cstring, ' although resident of ') AS ?prstring).
		BIND(IF(CONTAINS(?crstring, ' '), STRBEFORE(?crstring, ' '), ?crstring) AS ?crstr).
		BIND(IF(CONTAINS(?prstring, ' '), STRBEFORE(?prstring, ' '), ?prstring) AS ?prstr).
		?crcity codo:geoName ?crstr.
		?prcity codo:geoName ?prstr.
        filter(contains(?cstring, ' although resident of '))}")
  (sparql:run-sparql "DELETE {?p codo:cityString ?cstring.}
INSERT {?p codo:isCurrentResidentOf ?crcity;
           codo:isPermanentResidentOf ?prcity.} 
WHERE {?p codo:cityString ?cstring.
        BIND(STRBEFORE(?cstring, ' but resident of ') AS ?crstring).
		BIND(STRAFTER(?cstring, ' but resident of ') AS ?prstring).
		BIND(IF(CONTAINS(?crstring, ' '), STRBEFORE(?crstring, ' '), ?crstring) AS ?crstr).
		BIND(IF(CONTAINS(?prstring, ' '), STRBEFORE(?prstring, ' '), ?prstring) AS ?prstr).
		?crcity codo:geoName ?crstr.
		?prcity codo:geoName ?prstr.
        filter(contains(?cstring, ' but resident of '))}")
  (sparql:run-sparql "DELETE {?p codo:cityString ?cstring.}
INSERT {?p codo:isCurrentResidentOf ?crcity;
           codo:isPermanentResidentOf ?crcity.} 
WHERE {?p codo:cityString ?cstring.
        BIND(STRBEFORE(?cstring, ' ') AS ?crstring).
		?crcity codo:geoName ?crstring.
        filter(regex(?cstring, '\\\\D+ \\\\D+'))}")
  (sparql:run-sparql "DELETE {?p codo:cityString ?cstring.}
INSERT {?p codo:isCurrentResidentOf ?crcity;
           codo:isPermanentResidentOf ?crcity.} 
WHERE {?p codo:cityString ?cstring.
        BIND(STRBEFORE(?cstring, ' in ') AS ?crstring).
		?crcity codo:geoName ?crstring.
        filter(contains(?cstring, ' in '))}")
  (sparql:run-sparql "DELETE {?p codo:cityString ?cstring.}
INSERT {?p codo:isCurrentResidentOf ?city;
           codo:isPermanentResidentOf ?city.} 
WHERE {?p codo:cityString ?cstring.
        BIND(IF(STRENDS(?cstring, ' '), STRBEFORE(?cstring, ' '), ?cstring) AS ?trmdcstring). 
		?city codo:geoName ?trmdcstring.}")
  (commit-triple-store))
    
	   
(defun codo-outdate-transforms ()
  (print "Entering Out Date transforms")
  ;Transform all properly formatted outdateString
  (sparql:run-sparql "DELETE {?p codo:outdateString ?outstr.}
INSERT {?p codo:releasedOn ?outdt.}
WHERE {?p codo:outdateString ?outstr.
	   BIND(SUBSTR(?outstr, 1, 2) AS ?dstr).
	   BIND(SUBSTR(?outstr, 4, 2) AS ?mstr).
	   BIND(SUBSTR(?outstr, 7, 4) AS ?ystr).
	   BIND(CONCAT(?ystr,'-',?mstr,'-',?dstr,'T00:00:00') AS ?dtstr).
	   BIND(xsd:dateTime(?dtstr) AS ?outdt).
	   FILTER(STRLEN(?outstr) = 10)}")
  ;If not in test mode delete all remaining cityString If in test mode
  ;keep them to examine in case some strings that should have been processed weren't
  (if (not *test-modep*) (sparql:run-sparql "DELETE  {?p codo:outdateString ?outstr.}
WHERE {?p codo:outdateString ?outstr.}"))
  (commit-triple-store))
	   
(defun city-outdate-transforms ()
  (codo-city-transforms)
  (codo-outdate-transforms))
	   
