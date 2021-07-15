#
# Untested tests follow
#

@pytest.mark.skip()
class TestResourceAttributesEnums(unittest.TestCase):
    '''
    Test enumerated resource-attributes.
    '''
    resourcetype = 'places'
    resourceuid = 'Midian'
    attr1name = 'kind'
    attr1val = 'Fictional'
    attr1valbad = 'Real'
    result = None
    def test_create_and_use_enums(self):
        print('Test: create a resource with a valid enum attribute')
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resourceuid,
                                             self.attr1name: self.attr1val}).status_code,
                         201)
        requests.delete('%s/%s/%s' % (API_BASE_URL, self.resourcetype, self.resourceuid))
        print('Test: fail to create a resource with an invalid enum attribute')
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resourceuid,
                                             self.attr1name: self.attr1valbad}).status_code,
                         400)
        print('Test: fail to add an invalid enum attribute to an existing resource')
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.resourcetype),
                                       data={'uid': self.resourceuid}).status_code,
                         201)
        self.assertEqual(requests.put('%s/%s/%s' % (API_BASE_URL,
                                                    self.resourcetype,
                                                    self.attr1name),
                                      data={'uid': self.resourceuid,
                                            self.attr1name: self.attr1valbad}).status_code,
                         400)
        print('Test: add a valid attribute to an existing resource.')
        self.assertEqual(requests.put('%s/%s/%s' % (API_BASE_URL,
                                                    self.resourcetype,
                                                    self.attr1name),
                                      data={'uid': self.resourceuid,
                                            self.attr1name: self.attr1val}).status_code,
                         204)
        requests.delete('%s/%s/%s' % (API_BASE_URL, self.resourcetype, self.resourceuid))

@pytest.mark.skip()
class TestMoveDependentResources(unittest.TestCase):
    p1type = 'countries'
    p1uid = 'Netherlands'
    p1targetrel = 'Cities'
    p2type = 'states'
    p2uid = 'Noord Holland'
    p1p2rel = 'States'
    p2targetrel = 'Cities'
    targettype = 'cities'
    targetuid = 'Amsterdam'
    result = None
    def test_move_dependent_resource(self):
        print('Test: test_move_dependent_resource')
        # Create the initial parent resource
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.p1type),
                                       data={'uid': self.p1uid}).status_code,
                         201)
        # Create the second parent resource, as a dependent to the first
        self.assertEqual(requests.post('%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                           self.p1type,
                                                           sanitise_uid(self.p1uid),
                                                           self.p1p2rel,
                                                           self.p2type),
                                       data={'uid': self.p2uid}).status_code,
                         201)
        # Create the dependent resource
        self.assertEqual(requests.post('%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                           self.p1type,
                                                           sanitise_uid(self.p1uid),
                                                           self.p1targetrel,
                                                           self.targettype),
                                       data={'uid': self.targetuid}).status_code,
                         201)
        # Move the dependent resource to its new parent
        self.result = requests.post('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                           self.p1type,
                                                           sanitise_uid(self.p1uid),
                                                           self.p1targetrel,
                                                           self.targettype,
                                                           sanitise_uid(self.targetuid)),
                                    data={'target': '/%s/%s/%s/%s/%s/%s' %
                                          (self.p1type,
                                           sanitise_uid(self.p1uid),
                                           self.p1p2rel,
                                           self.p2type,
                                           sanitise_uid(self.p2uid),
                                           self.p2targetrel)})
        self.assertEqual(self.result.status_code, 201)
        self.assertEqual(self.result.text,
                         '/%s/%s/%s/%s/%s/%s/%s/%s' % (self.p1type,
                                                       sanitise_uid(self.p1uid),
                                                       self.p1p2rel,
                                                       self.p2type,
                                                       sanitise_uid(self.p2uid),
                                                       self.p2targetrel,
                                                       self.targettype,
                                                       sanitise_uid(self.targetuid)))
        # Confirm it's where it should be
        self.assertEqual(requests.get('%s/%s/%s/%s/%s/%s/%s/%s/%s' %
                                      (API_BASE_URL,
                                       self.p1type,
                                       sanitise_uid(self.p1uid),
                                       self.p1p2rel,
                                       self.p2type,
                                       sanitise_uid(self.p2uid),
                                       self.p2targetrel,
                                       self.targettype,
                                       sanitise_uid(self.targetuid))).status_code,
                         200)
        # Confirm it's no longer attached to the initial parent
        self.assertEqual(requests.get('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                             self.p1type,
                                                             sanitise_uid(self.p1uid),
                                                             self.p1targetrel,
                                                             self.targettype,
                                                             sanitise_uid(self.targetuid))).json(),
                         [])
        # Delete the parent resource, which should take the rest with it
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.p1type,
                                                       sanitise_uid(self.p1uid)),
                                         data={'recursive': 'true'}).status_code,
                         204)


@pytest.mark.skip()
class TestValidRelationships(unittest.TestCase):
    '''
    Basic CRD functions for relationships
    '''
    res1type = 'routers'
    res1uid = 'bikini'
    relationship = 'Asn'
    res2type = 'asn'
    res2name = '64512'
    res3type = 'brands'
    res3uid = 'NetBoxes'
    depres1type = 'models'
    depres1uid = 'Packetshuffler3000'
    depres1deprel = 'Produces'
    res1todepres1rel = 'Model'
    result = None
    def test_basic_relationship(self):
        print('Test: test_basic_relationship')
        # Create two new resources
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.res1type),
                                       data={'uid': self.res1uid}).status_code,
                         201)
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.res2type),
                                       data={'uid': self.res2name}).status_code,
                         201)
        # Create a valid relationship between them
        self.result = requests.post('%s/%s/%s/%s'% (API_BASE_URL,
                                                    self.res1type,
                                                    self.res1uid,
                                                    self.relationship),
                                    data={'target': '/%s/%s' % (self.res2type,
                                                                self.res2name)})
        self.assertEqual(self.result.text, '/%s/%s/%s/%s/%s'% (self.res1type,
                                                               self.res1uid,
                                                               self.relationship,
                                                               self.res2type,
                                                               self.res2name))
        self.assertEqual(self.result.status_code, 201)
        # Confirm that the relationship is there
        self.assertEqual(requests.get('%s/%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid,
                                                       self.relationship)).status_code,
                         200)
        self.result = requests.get('%s/%s/%s/%s' % (API_BASE_URL,
                                                    self.res1type,
                                                    self.res1uid,
                                                    self.relationship)).json()
        self.assertEqual(self.result[0]['original_uid'], self.res2name)
        self.assertEqual(self.result[0]['uid'], self.res2name)
        self.assertEqual(self.result[0]['type'], self.res2type)
        # Delete the relationship
        self.assertEqual(requests.delete('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                                self.res1type,
                                                                self.res1uid,
                                                                self.relationship,
                                                                self.res2type,
                                                                self.res2name)).status_code,
                         204)
        # Confirm the relationship is gone
        self.assertEqual(requests.get('%s/%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid,
                                                       self.relationship)).json(),
                         [])
        # Delete the destination resource
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.res2type,
                                                       self.res2name)).status_code,
                         204)
        # Delete the resources
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid)).status_code,
                         204)
    def test_rels_between_primary_and_secondary_resources(self):
        print('Test: test_rels_between_primary_and_secondary_resources')
        # Create two first-class resources, with a third dependent on the first
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.res1type),
                                       data={'uid': self.res1uid}).status_code,
                         201)
        self.assertEqual(requests.post('%s/%s/' % (API_BASE_URL, self.res3type),
                                       data={'uid': self.res3uid}).status_code,
                         201)
        self.assertEqual(requests.post('%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                           self.res3type,
                                                           self.res3uid,
                                                           self.depres1deprel,
                                                           self.depres1type),
                                       data={'uid': self.depres1uid}).status_code,
                         201)
        # Link to the dependent from the other first-class
        # Confirm that the linked first-class resource is at the end of that path
        # Link from the first-class to the dependent
        self.assertEqual(
            requests.post('%s/%s/%s/%s' % (API_BASE_URL,
                                           self.res1type,
                                           self.res1uid,
                                           self.res1todepres1rel),
                          data={'target': '/%s/%s/%s/%s/%s' % (self.res3type,
                                                               self.res3uid,
                                                               self.depres1deprel,
                                                               self.depres1type,
                                                               self.depres1uid)}).status_code,
            201)
        # Confirm that the linked dependent resource is at the end of that path
        self.assertEqual(requests.get('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                             self.res1type,
                                                             self.res1uid,
                                                             self.res1todepres1rel,
                                                             self.depres1type,
                                                             self.depres1uid)).status_code,
                         200)
        # What do we get when we just ask for what's at the end of that relationship?
        self.assertEqual(requests.get('%s/%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid,
                                                       self.res1todepres1rel)).status_code,
                         200)
        # Delete both first-class resources
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid),
                                         data={'recursive': 'true'}).status_code,
                         204)
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.res3type,
                                                       self.res3uid),
                                         data={'recursive': 'true'}).status_code,
                         204)


@pytest.mark.skip()
class TestInvalidRelationships(unittest.TestCase):
    '''
    Basic CRD functions for relationships
    '''
    res1type = 'routers'
    res1uid = 'bikini'
    res2type = 'asn'
    res2uid = '64512'
    relationship_valid = 'Asn'
    relationship_invalid = 'dysfunctionalRelationship'
    def test_basic_relationship(self):
        print('Test: test_basic_relationship')
        # Create two new resources
        print('Test: create the resources to link')
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.res1type),
                                       data={'uid': self.res1uid}).status_code,
                         201)
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.res2type),
                                       data={'uid': self.res2uid}).status_code,
                         201)
        # Attempt to create a relationship between them,
        # of a type that doesn't exist for the source resource-type
        print('Test: create invalid relationship')
        self.assertEqual(requests.post('%s/%s/%s/%s'% (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid,
                                                       self.relationship_invalid),
                                       data={'target': '/%s/%s' % (self.res2type,
                                                                   self.res2uid)}).status_code,
                         409)
        # Attempt to create a valid relationship between them
        print('Test: create valid relationship')
        self.assertEqual(requests.post('%s/%s/%s/%s'% (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid,
                                                       self.relationship_valid),
                                       data={'target': '/%s/%s' % (self.res2type,
                                                                   self.res2uid)}).status_code,
                         201)
        # Delete the resources
        print('Test: delete the resources')
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.res1type,
                                                       self.res1uid)).status_code,
                         204)
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.res2type,
                                                       self.res2uid)).status_code,
                         204)

@pytest.mark.skip()
class TestAnyType(unittest.TestCase):
    '''
    Confirm handling of the 'any' type
    '''
    p1type = 'routers'
    p1uid = 'Enewetak'
    p1rel = 'Tags'
    invalidrel = 'invalid'
    t1type = 'tags'
    t1uid = 'Tagged'
    def test_01_rejection(self):
        print('Test: test_any_type')
        self.assertEqual(requests.get('%s/any/foo' % API_BASE_URL).status_code,
                         200)
        self.assertEqual(requests.get('%s/any/foo' % API_BASE_URL).json(),
                         [])
    def test_02_create_valid_any_rel(self):
        print('Test: test_create_valid_any_rel')
        # Create the resources
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.p1type),
                                       data={'uid': self.p1uid}).status_code,
                         201)
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.t1type),
                                       data={'uid': self.t1uid}).status_code,
                         201)
        # Create the relationship between them
        self.assertEqual(requests.post('%s/%s/%s/%s' % (API_BASE_URL,
                                                        self.p1type,
                                                        self.p1uid,
                                                        self.p1rel),
                                       data={'target': '/%s/%s' % (self.t1type,
                                                                   self.t1uid)}).status_code,
                         201)
        # Delete the relationship
        self.assertEqual(requests.delete('%s/%s/%s/%s/%s/%s' % (API_BASE_URL,
                                                                self.p1type,
                                                                self.p1uid,
                                                                self.p1rel,
                                                                self.t1type,
                                                                self.t1uid)).status_code,
                         204)
        # Delete the resources
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.p1type,
                                                       self.p1uid)).status_code,
                         204)
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.t1type,
                                                       self.t1uid)).status_code,
                         204)
    def test_03_create_invalid_any_rel(self):
        print('Test: test_create_valid_any_rel')
        # Create the resources
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.p1type),
                                       data={'uid': self.p1uid}).status_code,
                         201)
        self.assertEqual(requests.post('%s/%s' % (API_BASE_URL, self.t1type),
                                       data={'uid': self.t1uid}).status_code,
                         201)
        # Attempt to create the relationship between them.
        # This should fail, because we're trying to create a relationship
        # that hasn't been defined.
        self.assertEqual(requests.post('%s/%s/%s/%s' % (API_BASE_URL,
                                                        self.p1type,
                                                        self.p1uid,
                                                        self.invalidrel),
                                       data={'target': '/%s/%s' % (self.t1type,
                                                                   self.t1uid)}).status_code,
                         409)
        # Delete the resources
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.p1type,
                                                       self.p1uid)).status_code,
                         204)
        self.assertEqual(requests.delete('%s/%s/%s' % (API_BASE_URL,
                                                       self.t1type,
                                                       self.t1uid)).status_code,
                         204)
