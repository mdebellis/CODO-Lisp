; to transform data from geographic cluster strings
; this function is typically run last although it should be 
; independent of the others

(in-package :db.agraph.user)

(defun codo-cluster-transforms ()
  (print "Entering Cluster transforms")
  (sparql:run-sparql 
"DELETE {
	?p codo:clusterString ?cs.}
INSERT { 
	?cl codo:hasMember ?p.
	?p codo:isMemberOf ?cl.
	?p codo:travelledFrom ?place.
	}
WHERE {
	?p codo:clusterString ?cs.
	BIND 
	(IF (CONTAINS(?cs, 'From Middle East'), codo:MiddleEastCluster,
	(IF (CONTAINS(?cs, 'From USA'), codo:USACluster,
	(IF (CONTAINS(?cs, 'From South America'), codo:SouthAmericaCluster,
	(IF (CONTAINS(?cs, 'From the rest of Europe'), codo:RestOfEuropeCluster,
	(IF (CONTAINS(?cs, 'From the Southern States'), codo:SouthernStatesCluster,
	(IF (CONTAINS(?cs, 'TJ Congregation from 13th to 18th March in Delhi'), codo:TJCongregation13thTo18thCluster,
	(IF (CONTAINS(?cs, 'From Gujarat'), codo:GujaratCluster,
	(IF (CONTAINS(?cs, 'From Maharashtra'), codo:MaharashtraCluster,
	(IF (CONTAINS(?cs, 'From Rajasthan'), codo:RajasthanCluster,
	(IF (CONTAINS(?cs, 'Domestic Travel History Absent'), codo:DomesticTravelHistoryAbsentCluster,
	(IF (CONTAINS(?cs, 'International Travel History Absent'), codo:InternationalTravelHistoryAbsentCluster,
	(IF (CONTAINS(?cs, 'From United Kingdom'), codo:UnitedKingdomCluster, owl:Nothing))))))))))))))))))))))) AS ?cl).
	?cl codo:hasLocation ?place.
	FILTER(?cl != owl:Nothing) 
}
")
  (sparql:run-sparql 
"DELETE {
	?p codo:clusterString ?cs.}
INSERT { 
	?cl codo:hasMember ?p.
	?p codo:isMemberOf ?cl.
	}
WHERE {
	?p codo:clusterString ?cs.
	BIND 
	(IF (CONTAINS(?cs, 'Others'), codo:OthersCluster, 
	(IF (CONTAINS(?cs, 'Unknown'), codo:UnknownCluster, 
	(IF (CONTAINS(?cs, 'Severe Acute Respiratory Infection'), codo:SevereAcuteRespiratoryInfectionCluster,
	(IF (CONTAINS(?cs, 'Influenza like illness'), codo:InfluenzaLikeIllnessCluster,
	(IF (CONTAINS(?cs, 'Containment Zones'), codo:ContainmentZonesCluster,
	(IF (CONTAINS(?cs, '27-June Trace History Absent'), codo:June-27TraceHistoryAbsentCluster,
	(IF (CONTAINS(?cs, '28-June Trace History Absent'), codo:June-28TraceHistoryAbsentCluster,
	(IF (CONTAINS(?cs, '29-June Trace History Absent'), codo:June-29TraceHistoryAbsentCluster,
	(IF (CONTAINS(?cs, 'Second Generation Contact Absent'), codo:SecondGenerationContactAbsentCluster,
	(IF (CONTAINS(?cs, 'Second Generation Contact'), codo:SecondGenerationContactCluster,
	owl:Nothing))))))))))))))))))) AS ?cl).
	FILTER(?cl != owl:Nothing) 
}
")
   (sparql:run-sparql 
"DELETE {
	?p codo:clusterString ?cs.}
INSERT { 
	?cl codo:hasMember ?p.
	?p codo:isMemberOf ?cl.
	?p  codo:co-worker ?sh.
	?sh  codo:co-worker ?p.
	}
WHERE {
	?p codo:clusterString ?cs.
	?p codo:contractedVirusFrom ?sh.
	BIND	
	(IF (CONTAINS(?cs, 'Pharmaceutical Company in Nanjangud'), codo:PharmaceuticalCompanyInNanjangudCluster,
    	owl:Nothing) AS ?cl).
	FILTER(?cl != owl:Nothing) 
}")
  (sparql:run-sparql 
"DELETE {
	?p codo:clusterString ?cs.}
INSERT { 
	?cl codo:hasMember ?p.
	?p codo:isMemberOf ?cl.
	}
WHERE {
	?p codo:clusterString ?cs.
	BIND 
	(IF (CONTAINS(?cs, 'Pharmaceutical Company in Nanjangud'), codo:PharmaceuticalCompanyInNanjangudCluster,
    	owl:Nothing) AS ?cl).
	OPTIONAL{?p codo:contractedVirusFrom ?sh.}
	FILTER(?cl != owl:Nothing && !bound(?sh))
}")
  (commit-triple-store)
  )


