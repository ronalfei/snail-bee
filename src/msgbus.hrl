%Last modified: 2012-05-10 13:01:36
%Author: BlackAnimal <ronalfei@qq.com>
%Create by vim: ts=4



-compile([{parse_transform, lager_transform}]).
%-compile([{parse_transform, lager_transform}, export_all]).

-include("deps/amqp_client/include/amqp_client.hrl").


-record(mail_format, {
	from,
	to,
	subject, 
	display_from, 
	display_to, 
	content_type="Content-Type: text/html; charset=utf-8", 
	content_encode="Content-Transfer-Encoding: binary", 
	body
	}
).


