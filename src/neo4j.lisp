;;;; Methods and functions specifically relating to Neo4J

(in-package #:restagraph)


(defmethod get-classes-from-db ((db neo4cl:neo4j-rest-server))
  (mapcar #'car
          (neo4cl::extract-rows-from-get-request
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT . "MATCH (c:rgClass) RETURN c.name"))))))))

(defmethod get-class-relationships-from-db ((db neo4cl:neo4j-rest-server))
          (neo4cl::extract-rows-from-get-request
            (neo4cl:neo4j-transaction
              db
              `((:STATEMENTS
                  ((:STATEMENT . "MATCH (c:rgClass)-[r]->(t:rgClass) RETURN c.name, type(r), t.name")))))))
