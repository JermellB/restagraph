CREATE (:rgClass {name: 'router'});
CREATE (:rgClass {name: 'interface'});
CREATE (:rgClass {name: 'ipv4Address'});
MATCH (r:rgClass {name: 'router'}), (i:rgClass {name: 'interface'}) CREATE (r)-[:Interfaces]->(i);
MATCH (i:rgClass {name: 'interface'}), (a:rgClass {name: 'ipv4Address'}) CREATE (i)-[:Ipv4Addresses]->(a);
