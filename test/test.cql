// Restagraph test schema


// Clear the way
MATCH (n:rgResource)-[r]->(m:rgResource) DELETE r;
MATCH (n:rgResource)-[r]->(m:rgAttribute) DELETE r;
MATCH (n:rgAttribute) DELETE n;
MATCH (n:rgResource) DELETE n;

// Resources
//
// First-class
// I.e, have independent existence
CREATE (r:rgResource {name: 'routers'}), (r)-[:rgHasAttribute]->(:rgAttribute {name: 'comment', required: 'false'});
CREATE (r:rgResource {name: 'make'});
CREATE (a:rgResource {name: 'asn'}), (a)-[:rgHasAttribute]->(:rgAttribute {name: 'comment'});
CREATE (:rgResource {name: 'any', notes: 'Represents any type of resource'});
CREATE (t:rgResource {name: 'tags', notes: 'For categorising any kind of resource'});
//
// Dependent
// I.e, exist only in the context of other resources,
// which may themselves be dependent resources.
CREATE (i:rgResource {name: 'interfaces', dependent: 'true'}), (i)-[:rgHasAttribute]->(:rgAttribute {name: 'mac-address', required: 'false'}), (i)-[:rgHasAttribute]->(:rgAttribute {name: 'enabled'});
CREATE (a:rgResource {name: 'ipv4Addresses', dependent: 'true'}), (a)-[:rgHasAttribute]->(:rgAttribute {name: 'static'});
CREATE (r:rgResource {name: 'model', dependent: 'true'});

// Relationships between resources
//
MATCH (a:rgResource {name: 'any'}), (t:rgResource {name: 'tags'}) MERGE (a)-[:Tags]->(t);
MATCH (m:rgResource {name: 'make'}), (d:rgResource {name: 'model'}) CREATE (m)-[:HasModel {dependent: 'true'}]->(d);
MATCH (r:rgResource {name: 'routers'}), (d:rgResource {name: 'model'}) CREATE (r)-[:Model]->(d);
MATCH (r:rgResource {name: 'routers'}), (a:rgResource {name: 'asn'}) CREATE (r)-[:Asn]->(a);
MATCH (r:rgResource {name: 'routers'}), (i:rgResource {name: 'interfaces'}) CREATE (r)-[:Interfaces {dependent: 'true'} ]->(i);
MATCH (r:rgResource {name: 'routers'}), (a:rgResource {name: 'ipv4Addresses'}) CREATE (r)-[:Addresses {dependent: 'true'}]->(a);
MATCH (i:rgResource {name: 'interfaces'}), (a:rgResource {name: 'ipv4Addresses'}) CREATE (i)-[:Addresses {dependent: 'true'}]->(a);
// Interfaces can have subinterfaces
MATCH (i:rgResource {name: 'interfaces'}) CREATE (i)-[:SubInterfaces {dependent: 'true'}]->(i);
