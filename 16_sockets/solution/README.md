telnet-клиент
tcp-сервер в пассивном режиме с текстовым протоколом

memcached имеет текстовый протокол, с которым можно работать через telnet

https://github.com/memcached/memcached/blob/master/doc/protocol.txt

SET key value\n
STORED

GET key
VALUE key value
END
or NOT FOUND

GETS key1 key2 key3
VALUE key1 value1
VALUE key2 value2
VALUE key3 NOT FOUND
END

DELETE key
DELETED
or NOT FOUND

"add" means "store this data, but only if the server *doesn't* already hold data for this key".

ADD key value
STORED
or EXISTS

"replace" means "store this data, but only if the server *does* already hold data for this key".

REPLACE
STORED
or NOT FOUND

"append" means "add this data to an existing key after existing data".

APPEND
STORED
or NOT FOUND

"prepend" means "add this data to an existing key before existing data".

PREPEND
STORED
or NOT FOUND
