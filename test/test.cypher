CREATE (r:rgResource {name: 'routers'}), (r)-[:rgHasAttribute]->(:rgAttribute {name: 'comment', required: 'false'});
CREATE (i:rgResource {name: 'interfaces'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'mac-address', required: 'false'}),
    (i)-[:rgHasAttribute]->(:rgAttribute {name: 'enabled'});
CREATE (s:rgResource {name: 'subInterfaces'});
CREATE (:rgResource {name: 'ipv4Addresses'});
MATCH (r:rgResource {name: 'routers'}), (i:rgResource {name: 'interfaces'}) CREATE (r)-[:Interfaces]->(i);
MATCH (i:rgResource {name: 'interfaces'}), (a:rgResource {name: 'ipv4Addresses'}) CREATE (i)-[:Ipv4Addresses]->(a);
MATCH (i:rgResource {name: 'interfaces'}), (s:rgResource {name: 'subInterfaces', dependent: 'True'}) CREATE (i)-[:SubInterfaces]->(a);
