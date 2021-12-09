# Restagraph demo session

A walkthrough, demonstrating various features of Restagraph.

The following sequence gives you an overview of what you can do with Restagraph, using the familiar arena of movies, actors and directors.

It's written for somebody using the command-line on Linux, and probably assumes more knowledge than it should about your knowledge of Docker. At this point, if you're _not_ an early-adopter, I'm not sure what you're even doing here, but welcome to the ride anyway.


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
    $ docker stack deploy -c docker-compose.yml rgtest

Restagraph should now be listening on `http://192.0.2.1:4950/` so you should be able to upload the Movies subschema with the following command:

    $ curl -X POST --data-urlencode schema@schemas/movies_demo.json http://192.0.2.1:4950/schema/v1
    Created

This demonstrates one of Restagraph's features: you can augment the schema and API at any time, by uploading more definitions.

If you want to check the schema at any point, you can request the whole thing and use `jq` to make it human-readable:

    $ curl -s http://192.0.2.1:4950/schema/v1 | jq .

I've left out the response to this because it's 353 lines long.

Remember to include the trailing dot. If you're curious about why I make a point about human-readability, try that request again _without_ piping it through `jq`.


## Interactions

Create a person:

    $ curl -X POST -d 'uid=Keanu Reeves' http://192.0.2.1:4950/raw/v1/People
    /People/Keanu_Reeves

Look at the person you created:

    $ curl -s http://192.0.2.1:4950/raw/v1/People/Keanu_Reeves | jq .
    {
      "uid": "Keanu_Reeves",
      "createddate": 3847886778,
      "original_uid": "Keanu Reeves"
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
          "name": "displayname",
          "description": "The human-friendly version of their name, to be displayed in the UI.",
          "values": null
        },
        {
          "name": "notes",
          "description": "Notes about this person.",
          "values": null
        }
      ],
      "relationships": [
        {
          "name": "ACTED_IN",
          "target-type": "Movies",
          "cardinality": "many:many",
          "dependent": null,
          "description": null
        },
        {
          "name": "ACTED_IN",
          "target-type": "TvSeries",
          "cardinality": "many:many",
          "dependent": null,
          "description": null
        },
        {
          "name": "MEMBER_OF",
          "target-type": "Organisations",
          "cardinality": "many:many",
          "dependent": null,
          "description": null
        },
        {
          "name": "PRONOUNS",
          "target-type": "Pronouns",
          "cardinality": "many:many",
          "dependent": null,
          "description": "She/her, they/them, he/him and whatever others you choose to add. These are defined as a separate resourcetype partly because some people accept more than one set, and partly to make it easier to add more as necessary."
        }
      ]
    }

OK, so we can add a display-name and a note to a person. We use the `PUT` method for this, because we're updating an attribute on an existing resource:

    $ curl -X PUT -d 'displayname=Keanu Reeves' -d 'notes=May or may not be married to Winona Ryder.' http://192.0.2.1:4950/raw/v1/People/Keanu_Reeves
    Updated

Note for the language-lawyers: this returns 200/Updated in all cases, for two reasons:

- Multiple attributes can be updated in a single PUT request, leading to a conflict where one or more is updated, and one or more is not.
  The simplest solution to this conflict is to use the one return-code shared by these cases, and interpret the spec as "ensure that these attributes have these values" rather than "conditionally update whichever of these attributes doesn't already have the value specified in this request."
- Within the semantics of this API, all attributes are null by default. Thus, PUT requests are all updates by definition anyway.


Back to the API. Look at Keanu again (hardships, I know) and now we see the extra details we just added:

    $ curl -s http://192.0.2.1:4950/raw/v1/People/Keanu_Reeves | jq .
    {
      "uid": "Keanu_Reeves",
      "notes": "May or may not be married to Winona Ryder.",
      "createddate": 3847886778,
      "original_uid": "Keanu Reeves",
      "displayname": "Keanu Reeves",
      "updateddate": 3847888125
    }

Now there's also an `updateddate` timestamp, so you can see when a resource was last changed, separately from when it was created.

Add a movie he acted in:

    $ curl -X POST -d 'uid=Dracula' -d 'year_released=1992' http://192.0.2.1:4950/raw/v1/Movies
    /Movies/Dracula

Now link them together, in both directions. Why _both_ directions? Because Restagraph enables you to trace paths through the graph with URLs, and I didn't see a practical way of embedding "follow this link backwards" in a URL.

    $ curl -X POST -d 'target=/Movies/Dracula' http://192.0.2.1:4950/raw/v1/People/Keanu_Reeves/ACTED_IN
    /People/Keanu_Reeves/ACTED_IN/Movies/Dracula
    
    $ curl -X POST -d 'target=/People/Keanu_Reeves' http://192.0.2.1:4950/raw/v1/Movies/Dracula/ACTORS
    /Movies/Dracula/ACTORS/People/Keanu_Reeves

OK, so what can we find out about movies? What does the schema say we can record about them?

    $ curl -s http://192.0.2.1:4950/schema/v1/Movies | jq .
    {
      "name": "Movies",
      "dependent": null,
      "description": null,
      "attributes": [
        {
          "name": "year_released",
          "description": "The year in which this movie was first released.",
          "values": null
        }
      ],
      "relationships": [
        {
          "name": "ACTORS",
          "target-type": "People",
          "cardinality": "many:many",
          "dependent": null,
          "description": null
        }
      ]
    }

That's a little more information than I really wanted. Let's use `jq` to filter out only the information about what relationships you can create from movies to other things:

    $ curl -s http://192.0.2.1:4950/schema/v1/Movies | jq .relationships
    [
      {
        "name": "ACTORS",
        "target-type": "People",
        "cardinality": "many:many",
        "dependent": null,
        "description": null
      }
    ]

Actors and directors. OK, let's see who we already know acted in that movie?

    $ curl -s http://192.0.2.1:4950/raw/v1/Movies/Dracula/ACTORS | jq .
    [
      {
        "type": "People",
        "uid": "Keanu_Reeves",
        "notes": "May or may not be married to Winona Ryder.",
        "createddate": 3847886778,
        "original_uid": "Keanu Reeves",
        "displayname": "Keanu Reeves",
        "updateddate": 3847888125
      }
    ]

Keanu - what a surprise. But we already know he's a person (or a People), and right now we're only interested in the human actors in that movie anyway, so we have two reasons to ask for just the _people_ who acted in it:

    $ curl -s http://192.0.2.1:4950/raw/v1/Movies/Dracula/ACTORS/People | jq .
    [
      {
        "uid": "Keanu_Reeves",
        "notes": "May or may not be married to Winona Ryder.",
        "createddate": 3847886778,
        "original_uid": "Keanu Reeves",
        "displayname": "Keanu Reeves",
        "updateddate": 3847888125
      }
    ]

Ah, that's a little more compact. This seems kinda verbose either way, though, compared to a REST query. It's because Restagraph allows you to create the same kind of relationship to different things (a person might direct a movie, a TV show, or a music video). So it tells you explicitly about the type of each thing at the far end of the link, and you can ask for all the things of one particular type that this thing has some relationship to. It makes for some extra typing when you're doing this by hand, but who the heck does this kind of thing by hand when you can put a web page in front of it, or write a script to do it?

Let's add another actor for that movie. This time, we'll include all the details while creating it, instead of adding them afterward:

    $ curl -X POST -d 'uid=Winona Ryder' -d 'displayname=Winona Ryder' -d 'notes=May or may not be married to Keanu Reeves' http://192.0.2.1:4950/raw/v1/People
    /People/Winona_Ryder
    $ curl -X POST -d 'target=/Movies/Dracula' http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN
    /People/Winona_Ryder/ACTED_IN/Movies/Dracula
    $ curl -X POST -d 'target=/People/Winona_Ryder' http://192.0.2.1:4950/raw/v1/Movies/Dracula/ACTORS
    /Movies/Dracula/ACTORS/People/Winona_Ryder

Now she's in that movie, right?

    $ curl -s http://192.0.2.1:4950/raw/v1/Movies/Dracula/ACTORS/People | jq .
    [
      {
        "uid": "Keanu_Reeves",
        "notes": "May or may not be married to Winona Ryder.",
        "createddate": 3847886778,
        "original_uid": "Keanu Reeves",
        "displayname": "Keanu Reeves",
        "updateddate": 3847888125
      },
      {
        "uid": "Winona_Ryder",
        "notes": "May or may not be married to Keanu Reeves",
        "createddate": 3847888511,
        "original_uid": "Winona Ryder",
        "displayname": "Winona Ryder"
      }
    ]

OK, we're looking good.

But that's not all Winona's done, is it? She's been in other movies, and on TV as well:

    $ curl -X POST -d 'uid=Beetlejuice' http://192.0.2.1:4950/raw/v1/Movies
    /Movies/Beetlejuice
    $ curl -X POST -d 'target=/Movies/Beetlejuice' http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN
    /People/Winona_Ryder/ACTED_IN/Movies/Beetlejuice
    $ curl -X POST -d 'uid=Stranger Things' http://192.0.2.1:4950/raw/v1/TvSeries
    /TvSeries/Stranger_Things
    $ curl -X POST -d 'target=/TvSeries/Stranger_Things' http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN
    /People/Winona_Ryder/ACTED_IN/TvSeries/Stranger_Things

OK, now let's look at what she's acted in:

    $ curl -s http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN | jq .
    [
      {
        "type": "TvSeries",
        "uid": "Stranger_Things",
        "createddate": 3847888608,
        "original_uid": "Stranger Things"
      },
      {
        "type": "Movies",
        "uid": "Beetlejuice",
        "createddate": 3847888581,
        "original_uid": "Beetlejuice"
      },
      {
        "type": "Movies",
        "uid": "Dracula",
        "createddate": 3847888237,
        "original_uid": "Dracula",
        "year_released": "1992"
      }
    ]

We see both movies and TV series, and now it becomes a little more clear just why these URLs include both the relationship _and_ the type of thing there's a relationship to. Let's see just the movies she's been in:

    $ curl -s http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN/Movies | jq .
    [
      {
        "uid": "Beetlejuice",
        "createddate": 3847888581,
        "original_uid": "Beetlejuice"
      },
      {
        "uid": "Dracula",
        "createddate": 3847888237,
        "original_uid": "Dracula",
        "year_released": "1992"
      }
    ]

Now just the TV series:

    $ curl -s http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN/TvSeries | jq .
    [
      {
        "uid": "Stranger_Things",
        "createddate": 3847888608,
        "original_uid": "Stranger Things"
      }
    ]
