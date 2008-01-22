PHP_ARG_ENABLE(erlang, whether to enable Erlang C-node support,
[ --enable-erlang   Enable Erlang C-node support])

if test "$PHP_ERLANG" = "yes"; then
   AC_DEFINE(HAVE_ERLANG, 1, [Whether you have Erlang C-node support])
   PHP_SUBST(ERLANG_SHARED_LIBADD)
   PHP_ADD_LIBRARY(ei, 1, ERLANG_SHARED_LIBADD)
   PHP_NEW_EXTENSION(erlang, erlang.c, $ext_shared)
fi
