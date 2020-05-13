-- Debian packages nginx-extras, lua-zlib required
--
-- This file was originally from https://gist.github.com/davidcaste/05b2f9461ebe4a3bb3fc
-- That implementation was used to gunzip (inflate) request bodies before
-- passing off the inflated body to a proxy. What we want instead is to leave
-- the request body untouched (upstream server has Accept-Encoding: gzip), but
-- to put the inflated body in a variable so nginx can log it.

ngx.ctx.max_chunk_size = tonumber(ngx.var.max_chunk_size)
ngx.ctx.max_body_size = tonumber(ngx.var.max_body_size)

function create_error_response (code, description)
	local message = string.format('{"status":400,"statusReason":"Bad Request","code":%d,"exception":"","description":"%s","message":"HTTP 400 Bad Request"}', code, description)
	ngx.status = ngx.HTTP_BAD_REQUEST
	ngx.header.content_type = "application/json"
	ngx.say(message)
	ngx.exit(ngx.HTTP_OK)
end


function inflate_chunk (stream, chunk)
	return stream(chunk)
end


function inflate_body (data)
	local stream = require("zlib").inflate()
	local buffer = ""
	local chunk = ""

	for index = 0, data:len(), ngx.ctx.max_chunk_size do
		chunk = string.sub(data, index, index + ngx.ctx.max_chunk_size - 1)
		local status, output, eof, bytes_in, bytes_out = pcall(stream, chunk)

		if not status then
			-- corrupted chunk
			ngx.log(ngx.ERR, output)
			create_error_response(4001, "Corrupted GZIP body")
		end

		if bytes_in == 0 and bytes_out == 0 then
			-- body is not gzip compressed
			create_error_response(4002, "Invalid GZIP body")
		end

		buffer = buffer .. output

		if bytes_out > ngx.ctx.max_body_size then
			-- uncompressed body too large
			create_error_response(4003, "Uncompressed body too large")
		end
	end

	return buffer
end


local content_encoding = ngx.req.get_headers()["Content-Encoding"]
if content_encoding == "gzip" then
	ngx.req.read_body()
	local data = ngx.req.get_body_data()

	if data ~= '' then
		local new_data = inflate_body(data)
		ngx.var.inflated_request_body = new_data
	end
else
    -- if content wasn't gzipped, we don't need to inflate it, just pass it
    -- along
	ngx.req.read_body()
	local data = ngx.req.get_body_data()
	ngx.var.inflated_request_body = data
end
