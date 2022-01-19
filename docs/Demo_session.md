# Restagraph demo session

A walkthrough, demonstrating various features of Restagraph. Best read in conjunction with the [HTTP API reference](HTTP_API.md).

The following sequence gives you an overview of what you can do with Restagraph, based on the standard Neo4j demo Movies dataset, with a few alterations to fit Restagraph's approach to things.

It's written for somebody using the command-line on Linux, and probably assumes more knowledge than it should about your knowledge of Docker. At this point, if you're _not_ an early-adopter, I'm not sure what you're even doing here, but welcome to the ride anyway.

It also gets a little long, but there's a fair bit of ground to cover.


## Tools used

This uses two command-line tools:

- `curl` - the canonical tool for using HTTP from the CLI.
- `jq` - an incredibly useful tool for filtering JSON and presenting it in human-readable form. You'll see the output of each HTTP GET request piped through `jq` in this example, because it's just so much more readable.
    - It's _useful_, not _required_, so you'll probably want to leave it out when you're piping the output into some other utility, unless you're using it for filtering.
    - It's the reason for using `curl -s`, where the `-s` flag means "don't print out extra information about the HTTP request.
    - The trailing dot in `jq .` _is_ significant: it tells `jq` to print out everything it receives, without filtering its input.
    - Although it's not shown in this tutorial, `jq` colour-codes its output to make it even more readable.

You may wish to substitute other equivalents, such as Postman.


## Conventions

Indented sections show commands you should enter, prefixed with `$` to indicate the command-line prompt, and the response you get from the server. Responses are simply not given a prefix.


## Setup

This tutorial assumes your workstation is configured to use 192.0.2.1 as Docker's gateway address, because [that subnet was reserved for testing and documentation in RFC5727](https://datatracker.ietf.org/doc/html/rfc5737). If you're using another address, you'll need to substitute that one in the examples that follow. If you want to configure your system to use that address, create/edit the file `/etc/docker/daemon.json` and ensure that it contains the clause `{ "bip": "192.0.2.1/24" }`.

From the top of this repo, create two Docker volumes (one for the db, one for files) and then launch the docker images:

    $ docker volume create -d local rgtestdb
    $ docker volume create -d local rgtestfiles
    $ docker stack deploy -c scripts/docker/docker-compose.yml rgtest

Restagraph should now be listening on `http://192.0.2.1:4950/` with its default core schema already installed. You should now be able to augment it with the Movies subschema, using the following command:

    $ curl -X POST --data-urlencode schema@schemas/movies_demo.json http://192.0.2.1:4950/schema/v1
    Created

This demonstrates one of Restagraph's features: you can augment the schema and API at any time, by uploading more definitions.

If you want to check the schema at any point, you can request the whole thing and use `jq` to make it human-readable:

    $ curl -s http://192.0.2.1:4950/schema/v1 | jq .

I've left out the response to this because it's too long to usefully include here.

Remember to include the trailing dot in `jq .` - it's effectively a command to operate on the entire thing. If you're curious about why I make a point about human-readability, try that request again _without_ piping it through `jq`.


## Differences from the Neo4j dataset - relationship attributes

If you're comparing this with the Neo4j tutorial, you'll notice that roles are resourcetypes, not just attributes on the `ACTED_IN` relationship. It's because Restagraph doesn't support attributes on relationships.

I considered and eventually dismissed the idea in [Github issue #18](https://github.com/equill/restagraph/issues/18), because I've yet to find a real-world case where it wouldn't be better to reify the relationship as a resource in itself.

I'm willing to be convinced to implement this feature; it's a question of finding enough value in it to justify the work involved. The hardest part will be figuring out how that part of the API might work.


# The session

Create a person. This is in the default core schema, and Restagraph adopts the REST style of using plurals, so it's a "People" resource:

    $ curl -X POST -d 'uid=Lana W' http://192.0.2.1:4950/raw/v1/People
    /People/Lana_W

Look at the person you created:

    $ curl -s http://192.0.2.1:4950/raw/v1/People/Lana_W | jq .
    {
      "uid": "Lana_W",
      "createddate": 3851667010,
      "original_uid": "Lana W"
    }

Note that the space in his UID has become an underscore. Restagraph does this automatically, so that everything can be referred to by its URL, with as few issues as possible. You'll also see the UID that was originally requested, and the date and time at which this entry was created in seconds since 01 January 1970.

Check all the characteristics of a `People` resource, to see what else you can record about them:

    $ curl -s http://192.0.2.1:4950/schema/v1/People | jq .
    {
      "name": "People",
      "dependent": null,
      "description": "Real people, imaginary people, security roles, members of an external organisation... if they're a person, this is the type.",
      "attributes": [
        {
          "name": "born",
          "description": "The year in which this person was born.",
          "read-only": null,
          "values": null
        },
        {
          "name": "displayname",
          "description": "The human-friendly version of their name, to be displayed in the UI.",
          "read-only": null,
          "values": null
        },
        {
          "name": "name",
          "description": "The person's full name",
          "read-only": null,
          "values": null
        },
        {
          "name": "notes",
          "description": "Notes about this person.",
          "read-only": null,
          "values": null
        }
      ],
      "relationships": [
        {
          "name": "DIRECTED",
          "target-type": "Movie",
          "cardinality": "many:many",
          "dependent": null,
          "description": "This person directed that movie, possibly in conjunction with other people."
        },
        {
          "name": "MEMBER_OF",
          "target-type": "Organisations",
          "cardinality": "many:many",
          "dependent": null,
          "description": "Denotes membership of an organisation. Counterpart to /Organisations/MEMBERS/People."
        },
        {
          "name": "PERFORMED",
          "target-type": "Role",
          "cardinality": "many:many",
          "dependent": null,
          "description": "This person performed that role. It's possible that they shared this role with other people."
        },
        {
          "name": "PRONOUNS",
          "target-type": "Pronouns",
          "cardinality": "many:many",
          "dependent": null,
          "description": "She/her, they/them, he/him and whatever others you choose to add. These are defined as a separate resourcetype partly because some people accept more than one set, and partly to make it easier to add more as necessary."
        },
        {
          "name": "WROTE",
          "target-type": "Movie",
          "cardinality": "many:many",
          "dependent": null,
          "description": "This person wrote that movie, possibly in conjunction with other people."
        }
      ]
    }

OK, so we can add a display-name and a note to a person. We use the `PUT` method for this, because we're updating an attribute on an existing resource:

    $ curl -X PUT -d 'displayname=Lana Wachowski' -d 'notes=Best known as co-creator of the Matrix trilogy.' http://192.0.2.1:4950/raw/v1/People/Lana_W
    Updated

Note for the language-lawyers: this returns 200/Updated in all cases, for two reasons:

- Multiple attributes can be updated in a single PUT request, leading to a conflict where one or more is updated, and one or more is not.
  The simplest solution to this conflict is to use the one return-code shared by these cases, and interpret the spec as "ensure that these attributes have these values" rather than "conditionally update whichever of these attributes doesn't already have the value specified in this request."
- Within the semantics of this API, all attributes are null by default. Thus, PUT requests are all updates by definition anyway.


Back to the API. Look at Lana and now we see the extra details we just added:

    $ curl -s http://192.0.2.1:4950/raw/v1/People/Lana_W | jq .
    {
      "uid": "Lana_W",
      "notes": "Best known as co-creator of the Matrix trilogy.",
      "createddate": 3851667010,
      "original_uid": "Lana W",
      "displayname": "Lana Wachowski",
      "updateddate": 3851667136
    }

Now there's also an `updateddate` timestamp, so you can see when a resource was last changed, separately from when it was created.

Add a movie she directed. I've stuck with Neo4j's convention of using the singular for node-labels when naming resourcetypes, so this is a "Movie" resource:

    $ curl -X POST -d 'uid=Speed Racer' -d 'released=2008' -d 'tagline=Speed has no limits' http://192.0.2.1:4950/raw/v1/Movie
    /Movies/Speed_Racer

New thing here: when you create a resource, you can set some or all of its attributes at the same time - you don't have to do set them afterwards via PUT requests. In this case, 

Now link them together, in both directions. Why _both_ directions? Because Restagraph enables you to trace paths through the graph with URLs, and I didn't see a practical way of embedding "follow this link backwards" in a URL.

    $ curl -X POST -d 'target=/Movie/Speed_Racer' http://192.0.2.1:4950/raw/v1/People/Lana_W/DIRECTED
    /People/Lana_W/DIRECTED/Movie/Speed_Racer
    
    $ curl -X POST -d 'target=/People/Lana_W' http://192.0.2.1:4950/raw/v1/Movie/Speed_Racer/DIRECTED_BY
    /Movie/Speed_Racer/DIRECTED_BY/People/Lana_W

OK, so what can we find out about movies? What does the schema say we can record about them? On second thought, the schema can be pretty verbose; let's just find out what kind of _relationships_ we can record from movies to other things:

    $ curl -s http://192.0.2.1:4950/schema/v1/Movie | jq .relationships
    [
      {
        "name": "CONTAINS",
        "target-type": "Role",
        "cardinality": "1:many",
        "dependent": true,
        "description": "That role exists in the context of this movie, and thus depends on this movie for its existence. One movie may contain many roles, but each role is specific to that movie, even if it corresponds to a role in a different movie, hence the 1:many cardinality."
      },
      {
        "name": "DIRECTED_BY",
        "target-type": "People",
        "cardinality": "many:many",
        "dependent": null,
        "description": "This movie was directed by that person, possibly in conjunction with others."
      },
      {
        "name": "WRITTEN_BY",
        "target-type": "People",
        "cardinality": "many:many",
        "dependent": null,
        "description": "This movie was written by that person, possibly along with others."
      }
    ]

Roles, writers and directors, huh? OK, let's connect Lana to the movie as its writer, as well as its director.

    $ curl -X POST -d 'target=/Movie/Speed_Racer' http://192.0.2.1:4950/raw/v1/People/Lana_W/WROTE
    /People/Lana_W/WROTE/Movie/Speed_Racer
    
    $ curl -X POST -d 'target=/People/Lana_W' http://192.0.2.1:4950/raw/v1/Movie/Speed_Racer/WRITTEN_BY
    /Movie/Speed_Racer/WRITTEN_BY/People/Lana_W

So now Lana (`/People/Lana_W`) is connected to that movie (`/Movie/Speed_Racer`) by two separate relationships: `WROTE` and `DIRECTED`. The movie is also connected to her by two corresponding relationships in the other direction.

This is an important moment: what we just did here is a key feature of Restagraph. I created RG specifically because you _can't do this_ in a relational database - not in any practical sense, and certainly not in a way that scales beyond a toy example.

To really hammer on the point, this is a crucial shift in perspective from relational databases to graph databases, at least in the ways in which they're typically used. In a relational DB, the schema normally defines things in terms of their relationship to a fixed point of origin - you don't have people, you have writers and directors, plus the hanging problem of how to minimise and resolve the duplicate entries. This flows naturally from the limited ways in which the relational model provides for modelling the world. The graph model, by contrast, positively encourages you to separate a thing's intrinsic identity from its relationships to other things. This is great! Just a shame about the lack of SQL's enforceable schema... which is what let to me building this thing.

Now we move on to another concept in Restagraph, but one that doesn't have a parallel in relational databases: the dependent resource. That is, resources whose existence _depends on_ the presence of another:

    $ curl -X POST -d 'uid=Trixie' http://192.0.2.1:4950/raw/v1/Movie/Speed_Racer/CONTAINS/Role
    /Movie/Speed_Racer/CONTAINS/Role/Trixie

Note the way we created that role: roles depend on movies via the `CONTAINED` relationship.

Do we know who performed that role? Yes, we do:

    $ curl -X POST -d 'uid=Christina R' -d 'displayname=Christina Ricci' http://192.0.2.1:4950/raw/v1/People
    /People/Christina_R
    $ curl -X POST -d 'target=/People/Christina_R' http://192.0.2.1:4950/raw/v1/Movie/Speed_Racer/CONTAINS/Role/Trixie/PERFORMED_BY
    /Movie/Speed_Racer/CONTAINS/Role/Trixie/PERFORMED_BY/People/Christina_R
    
    $ curl -X POST -d 'target=/Movie/Speed_Racer/CONTAINS/Role/Trixie' http://192.0.2.1:4950/raw/v1/People/Christina_R/PERFORMED
    /People/Christina_R/PERFORMED/RoleTrixie

Was that last response what you expected? This is an important part of the way Restagraph works: URIs describe relationships by following their path through the graph, without stopping to canonicalise them. This is why it's so useful to create complementary relationships - you can follow this path through the role to the movie it's in:

    $ curl -s http://192.0.2.1:4950/raw/v1/People/Christina_R/PERFORMED/RoleTrixie/APPEARS_IN
    /Movie/Speed_Racer

Can we follow this path a little further? Of course!

    $ curl -s http://192.0.2.1:4950/raw/v1/People/Christina_R/PERFORMED/RoleTrixie/APPEARS_IN/Movie/Speed_Racer/DIRECTED_BY
    /People/Lana_W

Does it mean you can create a relationship to a resource at the end of _any_ path? Yes, it does.

By now, hopefully you have a good feel for the way Restagraph takes the _idea_ of a predictable, composable schema from relational databases, applies it to a graph database, and in the process takes away the idea of a fixed starting point.

Let's explore things a little further, by adding Lana's sister to the mix. Lana is also credited as a writer and director, so we can do this bit pretty quickly:

    $ curl -X POST -d 'uid=Lilly W' -d 'displayname=Lilly Wachowski' http://192.0.2.1:4950/raw/v1/People
    /People/Lilly_W
    $ curl -X POST -d 'target=/Movie/Speed_Racer' http://192.0.2.1:4950/raw/v1/People/Lilly_W/WROTE
    /People/Lilly_W/WROTE/Movie/Speed_Racer
    $ curl -X POST -d 'target=/People/Lilly_W' http://192.0.2.1:4950/raw/v1/Movie/Speed_Racer/WRITTEN_BY
    /Movie/Speed_Racer/WRITTEN_BY/People/Lilly_W
    $ curl -X POST -d 'target=/Movie/Speed_Racer' http://192.0.2.1:4950/raw/v1/People/Lilly_W/DIRECTED
    /People/Lilly_W/DIRECTED/Movie/Speed_Racer
    $ curl -X POST -d 'target=/People/Lilly_W' http://192.0.2.1:4950/raw/v1/Movie/Speed_Racer/DIRECTED_BY
    /Movie/Speed_Racer/DIRECTED_BY/People/Lilly_W

We already knew how to do that, so what was the point of running through it a second time? To make a better illustration of the way Restagraph returns data:

    $ curl -s http://192.0.2.1:4950/raw/v1/Movie | jq .
    [
      {
        "uid": "Speed_Racer",
        "createddate": 3851674015,
        "original_uid": "Speed Racer",
        "tagline": "Speed has no limits",
        "released": "2008"
      }
    ]
    
    $ curl -s http://192.0.2.1:4950/raw/v1/Movie/Speed_Racer/DIRECTED_BY | jq .
    [
      {
        "type": "People",
        "uid": "Lana_W",
        "notes": "Best known as co-creator of the Matrix trilogy.",
        "createddate": 3851667010,
        "original_uid": "Lana W",
        "displayname": "Lana Wachowski",
        "updateddate": 3851667136
      },
      {
        "type": "People",
        "uid": "Lilly_W",
        "createddate": 3851680082,
        "original_uid": "Lilly W",
        "displayname": "Lilly Wachowski",
      }
    ]

Notice how when we specify the resourcetype, Restagraph doesn't bother including it in the results, but when we just specify the relationship it does? That's on purpose, because of the other side of Restagraph's approach to relationships: you can create the same kind of relationship from one type of resource to multiple other resourcetypes.

Neo4j's demo dataset doesn't include anything that demonstrates this well, so I added `TV Series` to the set of resourcetypes. Let's add one:

    $ curl -X POST -d 'uid=Sense8' http://192.0.2.1:4950/raw/v1/TV_Series
    /TV_Series/Sense8
    $ curl -X POST -d 'target=/TV_Series/Sense8' http://192.0.2.1:4950/raw/v1/People/Lilly_W/WROTE
    /People/Lilly_W/WROTE/TV_Series/Sense8
    $ curl -s http://192.0.2.1:4950/raw/v1/People/Lilly_W/WROTE | jq .
    [
      {
        "type": "TV_Series",
        "uid": "Sense8",
        "createddate": 3851681327,
        "original_uid": "Sense8"
      },
      {
        "type": "Movie",
        "uid": "Speed_Racer",
        "createddate": 3851674015,
        "original_uid": "Speed Racer",
        "tagline": "Speed has no limits",
        "released": "2008"
      }
    ]

Now it should make a little more sense. If you've been paying close attention to the patterns in the URIs, you'll probably be wondering whether we can put the resourcetype on the end of that query to be more specific about the results. Naturally, the answer is yes:

    $ curl -s http://192.0.2.1:4950/raw/v1/People/Lilly_W/WROTE/Movie | jq .
    [
      {
        "uid": "Speed_Racer",
        "createddate": 3851674015,
        "original_uid": "Speed Racer",
        "tagline": "Speed has no limits",
        "released": "2008"
      }
    ]

Can we take it one further, and ask specifically for the details of the movie at the end of that link? Of course!

    $ curl -s http://192.0.2.1:4950/raw/v1/People/Lilly_W/WROTE/Movie/Speed_Racer | jq .
    {
      "uid": "Speed_Racer",
      "createddate": 3851674015,
      "original_uid": "Speed Racer",
      "tagline": "Speed has no limits",
      "released": "2008"
    }


So far, so good, but that only allows us to filter "things relating to this one thing." What about filtering the other way, looking for "things of this type, that happen to have this relationship to that other thing?" As an example, let's filter for all the people who directed Speed Racer:

    $ curl -s http://192.0.2.1:4950/raw/v1/People?RGoutbound=/DIRECTED/Movie/Speed_Racer | jq .
    [
      {
        "uid": "Lana_W",
        "notes": "Best known as co-creator of the Matrix trilogy.",
        "createddate": 3851667010,
        "original_uid": "Lana W",
        "displayname": "Lana Wachowski",
        "updateddate": 3851667136
      },
      {
        "uid": "Lilly_W",
        "createddate": 3851680082,
        "original_uid": "Lilly W"
        "displayname": "Lilly Wachowski",
      }
    ]

Why `RGoutbound`? Because you can also filter on attributes with regular expressions, and I figured it's unlikely that anybody will have a pressing need to name attributes that way.

You can also combine these things. Let's find all the directors of Speed Racer whose display name starts with Lilly:

    $ curl -s 'http://192.0.2.1:4950/raw/v1/People?RGoutbound=/DIRECTED/Movie/Speed_Racer&displayname=^Lilly.*' | jq .
    [
      {
        "uid": "Lilly_W",
        "createddate": 3851680082,
        "original_uid": "Lilly W",
        "displayname": "Lilly Wachowski",
      }
    ]

Why yes, those _are_ [Java-style regexes](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)! Beware the need to wrap the entire URI in quote-marks when doing this from the CLI, though.

Conversely, you can also use `RGinbound` to pick out, say, all people who happened to be a director of that movie:

    $ curl -s http://192.0.2.1:4950/raw/v1/People?RGinbound=/Movie/Speed_Racer/DIRECTED_BY | jq .
    [
      {
        "uid": "Lana_W",
        "notes": "Best known as co-creator of the Matrix trilogy.",
        "createddate": 3851667010,
        "original_uid": "Lana W",
        "displayname": "Lana Wachowski",
        "updateddate": 3851667136
      },
      {
        "uid": "Lilly_W",
        "createddate": 3851680082,
        "original_uid": "Lilly W",
        "displayname": "Lilly Wachowski",
        "updateddate": 3851683911
      }
    ]

For bonus points, we can negate those filters, using `!` at the start of the regex:

    $ curl -s 'http://192.0.2.1:4950/raw/v1/People?displayname=!^Lilly.*' | jq .
    [
      {
        "uid": "Lana_W",
        "notes": "Best known as co-creator of the Matrix trilogy.",
        "createddate": 3851667010,
        "original_uid": "Lana W",
        "displayname": "Lana Wachowski",
        "updateddate": 3851667136
      },
      {
        "uid": "Christina_R",
        "createddate": 3851674135,
        "original_uid": "Christina R",
        "displayname": "Christina Ricci"
      }
    ]

There's also the Files API, described in the [HTTP API doc](HTTP_API.md), but by now you should have a clear idea of how to use it and what to expect. Likewise with the rest of the Schema API
