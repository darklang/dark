This directory seeds "package space," until the PM is stable enough that it'll
"only" live in prod.

Each top-level dir here corresponds to a user/org, whatever they 'own.' So, an
'openai.dark' file in the 'darklang' dir just mean that those are the functions
we maintain for ourselves. If someone were to create a user/org account with the
name 'openai' and fork all of our relevant stuff, they'd maintain their fork there.
(this all won't directly apply when things live only in prod, but it somewhat
represents the org/module structure/dance that'll occur there, so we're maintaining
a similar thing here.)

Please note that Dark code will not be in ".dark files" long-term -- this is a temp.
measure. Code will be stored in the cloud, rarely as simple files on your disk. That
said, you'll be able to edit in a 'normal' text editor, via virtual documents and
workspaces.

Also, the syntax seen here is directional, but is not the intended syntax (esp the
over-use of nested modules with their own lines and indentation, etc.). We're
currently writing a new parser, and have been constrained by our old (hacky F#-based)
parser. Once the new one is complete, we can kill the old one and adjust the grammar
appropriately.
