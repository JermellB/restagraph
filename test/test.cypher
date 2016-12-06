CREATE (:rgResource {name: 'routers'});
CREATE (:rgResource {name: 'interfaces'});
CREATE (:rgResource {name: 'ipv4Addresses'});
MATCH (r:rgResource {name: 'routers'}), (i:rgResource {name: 'interfaces'}) CREATE (r)-[:Interfaces]->(i);
MATCH (i:rgResource {name: 'interfaces'}), (a:rgResource {name: 'ipv4Addresses'}) CREATE (i)-[:Ipv4Addresses]->(a);
