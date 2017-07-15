# How to build and run

- Install Docker
- Run `brew install fswatch`
- Run `scripts/builder`
- Open your browser to http://localhost:8000/admin/ui
- Wait til the terminal says "Starting" - this means the build server is ready
- Run `scripts/trigger-build` - this triggers a build of everything
- Edit code normally - on each save, the app will be rebuilt and the browser will reload as necessary


# Design

## Example: blog
- datastore:
  - content: markdown
  - published: date
- page: create datastore entry
- admin page: list entries, allow delete
- sitemap
- page: show story
- 404 page which lists other blog posts


Notes:
- datastore is pretty much like a rails model
- there are two "flows": schema and data
- make an SEO thing with a sitemap.xml automatically
- dont create pages for API endpoints, go for user actions (also allow creating API endpoints)
- the important thing is not being able to zoom into your app, it's being able to zoom out
- views / layers:
 - API view
 - system view
 - errors view
 - performance layer
 - page view (see all the pages)
 
 - the data flows one way. But we create a source from the defitinion of the sink. Then we type check changes along those arrows. The definition of "create" isn't magic, it's derived from the DS when it's instantiated.

features:
- optimized page size
- android and web page and ios app
- static typing and error checking the whole way through
- automatic sitemap, rss feed
- automatic AMP, opengraph, twitter card, FB-optimized page


use cases:
- build twitter
- build a sales pipeline, semi-automated emails, calendering, check support tickets, build org structure
- build bug tracking site which tells customers when their tickets are done, integrated with slack and github
- build accounting reconciliation pipeline, with human in the loop



schema is just another port 
ah, we need multiple "output" ports from DSes!


what happens when you want inputs of two different types?
- to_table wants the schema as the header
  - maybe data should be transported with its schema?
  - two entry "ports" to a fn, being 2 params of different types



typesystem:
- DBs return objects
- we must know that the value is a Url
- therefore DBs must return Dict[str, Url]
- the URL has a string representation
- therefore it must be a tagged value
- Blog entry must then return:
 - {url: Url, title: Title, date: Date, content: Markdown}
- we have to be able to easily extract the str from value
- as implementation, Urls and Titles will have hidden values that you can only extract via functions.
- User-defined types are all DB entries (not quite - should allow tagged types too)
- you can extract the value via the name of the value. UI can make this sweet, including destructuring)
