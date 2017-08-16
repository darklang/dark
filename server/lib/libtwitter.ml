open Core
open Runtime

open Lib

let gets = [ "account/settings"
           ; "account/verify_credentials"
           ; "application/rate_limit_status"
           ; "blocks/ids"
           ; "blocks/list"
           ; "collections/entries"
           ; "collections/list"
           ; "collections/show"
           ; "direct_messages"
           ; "direct_messages/events/list"
           ; "direct_messages/events/show"
           ; "direct_messages/sent"
           ; "direct_messages/show"
           ; "direct_messages/welcome_messages/list"
           ; "direct_messages/welcome_messages/rules/list"
           ; "direct_messages/welcome_messages/rules/show"
           ; "direct_messages/welcome_messages/show"
           ; "favorites/list"
           ; "followers/ids"
           ; "followers/list"
           ; "friends/ids"
           ; "friends/list"
           ; "friendships/incoming"
           ; "friendships/lookup"
           ; "friendships/no_retweets/ids"
           ; "friendships/outgoing"
           ; "friendships/show"
           (* ; "geo/id/:place_id" *)
           ; "geo/reverse_geocode"
           ; "geo/search"
           ; "help/configuration"
           ; "help/languages"
           ; "help/privacy"
           ; "help/tos"
           ; "lists/list"
           ; "lists/members"
           ; "lists/members/show"
           ; "lists/memberships"
           ; "lists/ownerships"
           ; "lists/show"
           ; "lists/statuses"
           ; "lists/subscribers"
           ; "lists/subscribers/show"
           ; "lists/subscriptions"
           (* ; "media/upload (STATUS)" *)
           ; "mutes/users/ids"
           ; "mutes/users/list"
           ; "saved_searches/list"
           (* ; "saved_searches/show/:id" *)
           ; "search/tweets"
           ; "statuses/home_timeline"
           ; "statuses/lookup"
           ; "statuses/mentions_timeline"
           ; "statuses/oembed"
           ; "statuses/retweeters/ids"
           (* ; "statuses/retweets/:id" *)
           ; "statuses/retweets_of_me"
           (* ; "statuses/show/:id" *)
           ; "statuses/user_timeline"
           ; "trends/available"
           ; "trends/closest"
           ; "trends/place"
           ; "users/lookup"
           (* ; "users/lookup", screen_name, String list, optional; user_id, optional, string list; include_entities optional bool *)
           ; "users/profile_banner"
           ; "users/search"
           ; "users/show"
           ; "users/suggestions"
             (* ; "users/suggestions/:slug" *)
             (* ; "users/suggestions/:slug/members" *)
           ]
let posts = [ "account/remove_profile_banner"
            ; "account/settings"
            ; "account/update_profile"
            ; "account/update_profile_background_image"
            ; "account/update_profile_banner"
            ; "account/update_profile_image"
            ; "blocks/create"
            ; "blocks/destroy"
            ; "collections/create"
            ; "collections/destroy"
            ; "collections/entries/add"
            ; "collections/entries/curate"
            ; "collections/entries/move"
            ; "collections/entries/remove"
            ; "collections/update"
            ; "direct_messages/destroy"
            ; "direct_messages/events/new (message_create)"
            ; "direct_messages/new"
            ; "direct_messages/welcome_messages/new"
            ; "direct_messages/welcome_messages/rules/new"
            ; "favorites/create"
            ; "favorites/destroy"
            ; "friendships/create"
            ; "friendships/destroy"
            ; "friendships/update"
            ; "geo/place"
            ; "lists/create"
            ; "lists/destroy"
            ; "lists/members/create"
            ; "lists/members/create_all"
            ; "lists/members/destroy"
            ; "lists/members/destroy_all"
            ; "lists/subscribers/create"
            ; "lists/subscribers/destroy"
            ; "lists/update"
            ; "media/metadata/create"
            ; "media/upload"
            ; "media/upload (APPEND)"
            ; "media/upload (FINALIZE)"
            ; "media/upload (INIT)"
            ; "mutes/users/create"
            ; "mutes/users/destroy"
            ; "saved_searches/create"
            ; "saved_searches/destroy/:id"
            ; "statuses/destroy/:id"
            ; "statuses/retweet/:id"
            ; "statuses/unretweet/:id"
            ; "statuses/update"
            ; "statuses/update_with_media (deprecated)"
            ; "users/report_spam"
            ]

let deletes = [ "direct_messages/welcome_messages/destroy"
              ; "direct_messages/welcome_messages/rules/destroy"
              ]

let fns : shortfn list =
  List.map
    gets
    ~f:(fun name -> { n = "Twitter::" ^ name
                    ; o = []
                    ; p = [req "argument" tObj]
                    ; r = tAny
                    ; f = function
                        | [arg] -> Twitter.get (name ^ ".json") arg
                        | args -> expected "obj" args
                    }
       )
