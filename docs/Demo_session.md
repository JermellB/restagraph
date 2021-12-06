# Restagraph demo session

A walkthrough, demonstrating various features of Restagraph.

The following sequence gives you an overview of what you can do with Restagraph, using the familiar arena of movies, actors and directors.

This uses two command-line tools:

- `curl` - the canonical tool for using HTTP from the CLI.
- `jq` - an incredibly useful tool for filtering JSON and presenting it in human-readable form. You'll see the output of each HTTP GET request piped through `jq` in this example, because it's just so much more readable.
    - It's _useful_, not _required_, so you'll probably want to leave it out when you're piping the output into some other utility, unless you're using it for filtering.
    - It's the reason for using `curl -s`, where the `-s` flag means "don't print out extra information about the HTTP request.
    - The trailing dot in `jq .` _is_ significant: it tells `jq` to print out everything it receives, without filtering its input.

A core schema is automatically installed, which includes people. You'll need to install the movies demo schema, though. Assuming it's listening on port 4950 on address 192.0.2.1, you can `cd` to the top of the repo, and upload it with the following command:

    curl -X POST --data-urlencode schema@schemas/movies_demo.json http://192.0.2.1:4950/schema/v1

This demonstrates one of Restagraph's features: you can augment the schema and API at any time, by uploading more definitions.

Create a person:

    curl -X POST -d 'uid=Keanu Reeves' http://192.0.2.1:4950/raw/v1/People

Look at the person you created:

    curl -s http://192.0.2.1:4950/raw/v1/People/Keanu_Reeves | jq .

Note that the space in his UID has become an underscore. Restagraph does this automatically, so that everything can be referred to by its URL, with as few issues as possible. You'll also see the UID that was originally requested, and the date and time at which this entry was created in seconds since 01 January 1970.

Check all the characteristics of a `People` resource:

    curl http://192.0.2.1:4950/schema/v1/People | jq .

We can add a display-name and a note to a person. We use PUT for this, because we're updating an attribute on an existing resource:

    curl -X PUT -d 'displayname=Keanu Reeves' -d 'notes=May or may not be married to Winona Ryder.' http://192.0.2.1:4950/raw/v1/People/Keanu_Reeves

Look at him again, and now we see the extra details we just added:

    curl -s http://192.0.2.1:4950/raw/v1/People/Keanu_Reeves | jq .

Now there's also an `updateddate` timestamp, so you can see when a resource was last changed, separately from when it was created.

Add a movie he acted in:

    curl -X POST -d 'uid=Dracula' -d 'year_released=1992' http://192.0.2.1:4950/raw/v1/Movies

Now link them together, in both directions. Why _both_ directions? Because Restagraph enables you to trace paths through the graph with URLs, and I didn't see a practical way of embedding "follow this link backwards" in a URL.

    curl -X POST -d 'target=/Movies/Dracula' http://192.0.2.1:4950/raw/v1/People/Keanu_Reeves/ACTED_IN
    curl -X POST -d 'target=/People/Keanu_Reeves' http://192.0.2.1:4950/raw/v1/Movies/Dracula/ACTORS

OK, so what can we find out about movies? What does the schema say we can record about them?

    curl -s http://192.0.2.1:4950/schema/v1/Movies | jq .

That's a little more information than I really wanted. Let's use `jq` to filter out only the information about what relationships you can create from movies to other things:

    curl -s http://192.0.2.1:4950/schema/v1/Movies | jq .relationships

Actors and directors. OK, let's see who we already know acted in that movie?

    curl -s http://192.0.2.1:4950/raw/v1/Movies/Dracula/ACTORS | jq .

Keanu - what a surprise. But we already know he's a person (or a people), and right now we're only interested in the human actors in that movie anyway, so we have two reasons to ask for just the _people_ who acted in it:

    curl -s http://192.0.2.1:4950/raw/v1/Movies/Dracula/ACTORS/People | jq .

Ah, that's a little more compact. This seems kinda verbose either way, though, compared to a REST query. It's because Restagraph allows you to create the same kind of relationship to different things (a person might direct a movie, a TV show, or a music video). So it tells you explicitly about the type of each thing at the far end of the link, and you can ask for all the things of one particular type that this thing has some relationship to. It makes for some extra typing when you're doing this by hand, but who the heck does this kind of thing by hand when you can put a web page in front of it, or write a script to do it?

Let's add another actor for that movie. This time, we'll include all the details while creating it, instead of adding them afterward:

    curl -X POST -d 'uid=Winona Ryder' -d 'displayname=Winona Ryder' -d 'notes=May or may not be married to Keanu Reeves' http://192.0.2.1:4950/raw/v1/People
    curl -X POST -d 'target=/Movies/Dracula' http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN
    curl -X POST -d 'target=/People/Winona_Ryder' http://192.0.2.1:4950/raw/v1/Movies/Dracula/ACTORS

Now she's in that movie, right?

    curl -s http://192.0.2.1:4950/raw/v1/Movies/Dracula/ACTORS/People | jq .

OK, we're looking good.

But that's not all Winona's done, is it? She's been in other movies, and on TV as well:

    curl -X POST -d 'uid=Beetlejuice' http://192.0.2.1:4950/raw/v1/Movies
    curl -X POST -d 'target=/Movies/Beetlejuice' http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN
    curl -X POST -d 'uid=Stranger Things' http://192.0.2.1:4950/raw/v1/TvSeries
    curl -X POST -d 'target=/TvSeries/Stranger_Things' http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN

OK, now let's look at what she's acted in:

    curl -s http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN | jq .

We see both movies and TV series, and now it becomes a little more clear just why these URLs include both the relationship _and_ the type of thing there's a relationship to. Let's see just the movies she's been in:

    curl -s http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN/Movies | jq .

Now just the TV series:

    curl -s http://192.0.2.1:4950/raw/v1/People/Winona_Ryder/ACTED_IN/TvSeries | jq .
