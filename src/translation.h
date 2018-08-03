#include <R.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#define _tr(String) dgettext ("imptree", String)
/* replace pkg as appropriate */
#else
#define _tr(String) (String)
#endif

