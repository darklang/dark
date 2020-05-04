#!/usr/bin/env ruby
require 'csv'

# Usage:
# ./add_emails_to_csv.rb <filename>.csv
#
# <filename> is a CSV file (probably downloaded/exported from Google Sheets or
# Airtable) containing an Email field and a Username field.
#
# This script gets an email address from prodclone for each row in the table,
# and outputs to `<filename>2` a csv like the input file, but with the Email
# column filled in.
#
# This output is suitable for import back to Sheets/Airtable/etc if desired.

###################################
# Caveats and implementation notes:
###################################
#
# 1. Yes, it's not terribly hardened. This was a one-off script, preserved under
# the assumption that we might want it again later. Given intended use
# (human-observed, read-only) a lot of possible hardening/packaging/etc is work
# we won't prioritize. (Author deliberately refrained from using gems not in
# stdlib, to avoid requiring a Gemfile.)
#
# 2. Why Ruby, when nothing else is? It was written as a one-off script under time pressure by an
# author (ismith) for whom Ruby is more comfortable than Python.
#
# 3. We make a SQL query per row, instead of a batch query. Given the small
# scale of intended use, and the time constraints under which this was written,
# this seemed an appropriate compromise, though see #4.
#
# 4. prodclone/prod tradeoffs. prodclone unfortunately takes hours to download.
# On the other hand, querying prod involves more overhead (gcp-psql sets up a
# cloudsql proxy per-query). Could be improved by keeping gcp-psql running or
# reusing cloudsql proxy if it is already open. Could also be improved (probably
# more simply) by doing a batch query returning (username,email) and doing a
# join against the csv hash.
#
# 4b. Preemptively: load on prod postgres is not a concern due to the
# anticipated scale of this script.

if RUBY_VERSION =~ /^2\./
  # happy path, no error needed
else
  puts "This script has only been tested in Ruby 2; your Ruby is #{RUBY_VERSION}."
  puts "Feel free to remove this check! It is simple enough code that it may work."
  puts "This message is only intended to advise caution."
  exit 1
end

# Abstracted b/c we might want to allow a switch between querying prodclone and
# querying prod itself with ./scripts/gcp-psql
sql_cmd = "psql -t prodclone"

# Filename is anything in ARGV; this allows for us to support filenames with
# spaces. Janky, but since we don't do flag-handling, it works.
args = ARGV
filename = args.join(" ")
if !File.exists?(filename)
  puts "Error: no such file '#{filename}'."
  exit 1
end

file = CSV.read(filename, headers:true)

new_data = file.map do |row|
  # the 'username' field is actually canvas name, so trim it down to username
  username = row['Username'].gsub(/-.*/, '')

  # OPTIMIZATION: we could do batch SQL queries, but it's good enough
  # for the scale we need now against prodclone; running against prod is much
  # slower due to overhead of starting cloud_sql_proxy
  sql_res = `echo "select email from accounts where username = '#{username}';" | run-in-docker #{sql_cmd}`
  # Drop any headers/metadata; select just the line with the email address
  # (not strictly necessary given `psql -t`, but will be needed if we later
  # support ./scripts/gcp-psql which doesn't handle flags, including -t.
  email = sql_res.lines.select{|l| l =~ /@/}.join("").strip

  if row['Email'] == nil || row['Email'] == ''
    row['Email'] = email.strip
  end

  row.to_h
end

CSV.open(filename + "2", "wb") do |csv|
  # Header line
  csv << new_data.first.keys

  new_data.each do |row|
    csv << row.values
  end
end
