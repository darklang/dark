/* Doesn't deserialize, possibly due to using db migrations before they
 * were ready. */
delete from oplists where host = 'steve'
