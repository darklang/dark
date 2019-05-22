#!/usr/bin/env python

# Doesn't run in docker because we're just operating on files already in the
# workspace

import json

APP_SUPPORT = "backend/static/appsupport.js"


app_support_file = ""
with open(APP_SUPPORT, 'r') as in_file:
    app_support_file = in_file.read()
    # print("FILE: {}".format(len(app_support_file)))

with open('backend/static/etags.json') as json_file:
    etags = json.load(json_file)

    for file, etag in etags.iteritems():
        if file == '__date' or file.startswith("vendor/"):
            continue

        name = ".".join(file.split('.')[:-1])
        ext = file.split('.')[-1]

        newFile = "{}-{}.{}".format(name, etag, ext)

        print("{} -> {}".format(file, newFile))

        app_support_file = app_support_file.replace(file, newFile)

with open(APP_SUPPORT, 'w') as out_file:
    for line in app_support_file:
        out_file.write(line)
