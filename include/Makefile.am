
nobase_include_HEADERS = chop/block-indexers.h	\
			 chop/block-server.h	\
			 chop/block_rstore.h	\
			 chop/buffers.h		\
			 chop/chop-config.h	\
			 chop/chop-errors.h	\
			 chop/chop.h		\
			 chop/choppers.h	\
			 chop/filters.h		\
			 chop/hash.h		\
			 chop/cipher.h		\
			 chop/indexers.h	\
			 chop/logs.h		\
			 chop/objects.h		\
			 chop/store-stats.h	\
			 chop/store-browsers.h	\
			 chop/stores.h		\
			 chop/streams.h

if HAVE_GNUTLS
nobase_include_HEADERS += chop/sunrpc-tls.h chop/store-sunrpc-tls.h
else
nobase_noinst_HEADERS   = chop/sunrpc-tls.h chop/store-sunrpc-tls.h
endif

# Built in `src/'.
BUILT_SOURCES = chop/block_rstore.h chop/chop-errors.h
