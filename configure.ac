AC_INIT([Haskell fplll bindings], [0.1.0.0], [ldr709@gmail.com])
AC_CONFIG_SRCDIR([fplll.cabal])

AC_PROG_CXX
AC_LANG([C++])

LDFLAGS="${LDFLAGS} -v"
AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])], [
CXX_LIB_FLAGS=`awk '
BEGIN {
	RS=" ";
	ORS=" ";
}
/^-l.*(c\+\+|std)/ {
	print substr($0, 3)
}' conftest.err`
], [CXX_LIB_FLAGS=""])

AC_SUBST([CXX_LIB_FLAGS])

AC_CONFIG_FILES([fplll.buildinfo])
AC_OUTPUT
