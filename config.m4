PHP_ARG_ENABLE(erlang, whether to enable Erlang C-node support,
[  --enable-erlang         Enable Erlang C-node support])

if test "$PHP_ERLANG" = "yes"; then
   AC_DEFINE(HAVE_ERLANG, 1, [whether to enable Erlang C-node support])
   PHP_ADD_LIBRARY(ei, 1, ERLANG_SHARED_LIBADD)
   PHP_NEW_EXTENSION(erlang, erlang.c encode.c decode.c, $ext_shared)
   PHP_SUBST(ERLANG_SHARED_LIBADD)
fi
