# SYNOPSIS
#
#   AX_AC_DEFINE_RESOLVED([variable],[value],[description])
#
# DESCRIPTION
#
#   Extended version of AC_DEFINE_UNQUOTED (variable, value, description)
#   to resolve [value] including variables contained in there replacing
#   exec_prefix -> prefix, prefix -> ac_default_prefix if those are NONE
#
# LICENSE
#
#   Copyright (c) 2020 Simon Sobisch <simonsobisch@gnu.org>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

#serial 1

AC_DEFUN([AX_AC_DEFINE_RESOLVED],[

_VAR_RES_EXEC_PREFIX=$(eval echo "${exec_prefix}" | $SED "s#NONE#${prefix}#")
_VAR_RES_PREFIX=$(eval echo "${prefix}")
_VAR_RES_RESOLVED=$(echo "$2"	\
	| $SED 's#${exec_prefix}#'"${_VAR_RES_EXEC_PREFIX}"'#' \
	| $SED 's#${prefix}#'"${_VAR_RES_PREFIX}"'#' \
	| $SED "s#NONE#${ac_default_prefix}#" )
AC_DEFINE_UNQUOTED([$1], ["$(eval echo "${_VAR_RES_RESOLVED}")"], [$3])
unset _VAR_RES_EXEC_PREFIX _VAR_RES_PREFIX _VAR_RES_RESOLVED
])