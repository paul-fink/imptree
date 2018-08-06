#include <R.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("imptree", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

