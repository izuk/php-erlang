#ifndef PHP_ERLANG_H
#define PHP_ERLANG_H 1

#define PHP_ERLANG_VERSION "1.0"
#define PHP_ERLANG_EXTNAME "erlang"

#ifdef ZTS
#include "TSRM.h"
#endif

// Necessary for erl_errno.
#define _REENTRANT

#include "ei.h"

// Utility functions.
zval * php_erlang_decode( ei_x_buff * x );
int php_erlang_encode_term( ei_x_buff * x, char * fmt, int * idx, HashTable * arr, HashPosition * point );

ZEND_BEGIN_MODULE_GLOBALS(erlang)
ei_cnode ec;
int instance;
int fd;
ZEND_END_MODULE_GLOBALS(erlang)

#ifdef ZTS
#define ERLANG_G(v) TSRMG(erlang_globals_id, zend_erlang_globals *, v)
#else
#define ERLANG_G(v) (erlang_globals.v)
#endif

#define PHP_ERLANG_X_BUFF_RES_NAME "Erlang X-Buff"
#define PHP_ERLANG_PID_RES_NAME "Erlang PID"

// Resources.
extern int le_erlang_x_buff;
extern int le_erlang_pid;

PHP_MINIT_FUNCTION(erlang);
PHP_MSHUTDOWN_FUNCTION(erlang);

PHP_RINIT_FUNCTION(erlang);
PHP_RSHUTDOWN_FUNCTION(erlang);

PHP_FUNCTION(erlang_init);
PHP_FUNCTION(erlang_self);
PHP_FUNCTION(erlang_term);
PHP_FUNCTION(erlang_extract);
PHP_FUNCTION(erlang_send);
PHP_FUNCTION(erlang_send_reg);
PHP_FUNCTION(erlang_receive);

extern zend_module_entry erlang_module_entry;
#define phpext_erlang_ptr &erlang_module_entry

#endif
