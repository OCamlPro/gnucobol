/*
   Copyright (C) 2002-2012, 2014-2021 Free Software Foundation, Inc.
   Written by Keisuke Nishida, Roger While, Simon Sobisch, Ron Norman,
   Edward Hart

   This file is part of GnuCOBOL.

   The GnuCOBOL runtime library is free software: you can redistribute it
   and/or modify it under the terms of the GNU Lesser General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   GnuCOBOL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with GnuCOBOL.  If not, see <https://www.gnu.org/licenses/>.
*/

#ifndef COB_COBCAPI_H
#define COB_COBCAPI_H

/****************************************/
/* C - COBOL API Functions in cobcapi.c */
/****************************************/

COB_EXPIMP int		cob_get_num_params ( void );
COB_EXPIMP int		cob_get_param_constant ( int num_param );
COB_EXPIMP int		cob_get_param_digits( int num_param );
COB_EXPIMP int		cob_get_param_scale( int num_param );
COB_EXPIMP int		cob_get_param_sign ( int num_param );
COB_EXPIMP int		cob_get_param_size ( int num_param );
COB_EXPIMP int		cob_get_param_type ( int num_param );
COB_EXPIMP void *	cob_get_param_data ( int num_param );
COB_EXPIMP int		cob_get_param_right ( int num_param );
COB_EXPIMP cob_s64_t	cob_get_s64_param  ( int num_param );
COB_EXPIMP cob_u64_t	cob_get_u64_param  ( int num_param );
COB_EXPIMP double	cob_get_dbl_param  ( int num_param );
COB_EXPIMP char *	cob_get_picx_param ( int num_param, void *charfld, size_t charlen );
COB_EXPIMP void *	cob_get_grp_param  ( int num_param, void *charfld, size_t charlen );
COB_EXPIMP void		cob_put_dbl_param  ( int num_param, double value );
COB_EXPIMP void		cob_put_s64_param  ( int num_param, cob_s64_t value );
COB_EXPIMP void		cob_put_u64_param  ( int num_param, cob_u64_t value );
COB_EXPIMP void 	cob_put_picx_param ( int num_param, void *charfld );
COB_EXPIMP void  	cob_put_grp_param  ( int num_param, void *charfld, size_t charlen );

COB_EXPIMP const char	*cob_get_param_str ( int num_param, char *buff, size_t size);
COB_EXPIMP const char	*cob_get_param_str_buffered ( int num_param );
COB_EXPIMP int		cob_put_param_str ( int num_param, const char *src );

/* get access to one of the fields (to only operate with libcob functions on it!) */
COB_EXPIMP cob_field	*cob_get_param_field (int n, const char *caller_name);
COB_EXPIMP int		cob_get_field_size (const cob_field *);
COB_EXPIMP int		cob_get_field_type (const cob_field *);
COB_EXPIMP int		cob_get_field_digits	(const cob_field *);
COB_EXPIMP int		cob_get_field_scale	(const cob_field *);
COB_EXPIMP int		cob_get_field_sign	(const cob_field *);
COB_EXPIMP int		cob_get_field_constant (const cob_field *);
COB_EXPIMP int		cob_get_field_right (const cob_field *);
COB_EXPIMP const char	*explain_field_type (const cob_field *);

/* get the field's pretty-display value */
COB_EXPIMP const char	*cob_get_field_str (const cob_field *, char *buff, size_t size);
/* get the field's pretty-display value with an internal buffer for one-time access */
COB_EXPIMP const char	*cob_get_field_str_buffered (const cob_field *);
/* set the field's data using the appropriate internal type, returns EINVAL if data is invalid */
COB_EXPIMP int		cob_put_field_str (const cob_field *, const char *);
COB_EXPIMP int		cob_get_field_value (char *mod_name, char *field_ref, int len, char *buf);
COB_EXPIMP int		cob_put_field_value (char *mod_name, char *field_ref, int len, char *buf);
COB_EXPIMP int		cob_watch_field_value (char *mod_name, char *field_ref);
COB_EXPIMP int		cob_watch_field_free (char *mod_name, char *field_ref);
COB_EXPIMP int		cob_watch_check (char *mod_name, char *field_ref);

/*****************************************/
/* defines for MicroFocus C -> COBOL API */
/*****************************************/
typedef	char *		cobchar_t;
#define	cobs8_t		cob_s8_t
#define	cobuns8_t	cob_u8_t
#define	cobs16_t	cob_s16_t
#define	cobuns16_t	cob_u16_t
#define	cobs32_t	cob_s32_t
#define	cobuns32_t	cob_u32_t
#define	cobs64_t	cob_s64_t
#define	cobuns64_t	cob_u64_t

#define	cobsetjmp(x)	setjmp (cob_savenv (x))
#define	coblongjmp(x)	cob_longjmp (x)
#define	cobsavenv(x)	cob_savenv (x)
#define	cobsavenv2(x,z)	cob_savenv2 (x, z)
#define	cobfunc(x,y,z)	cob_func (x, y, z)
#define	cobcall(x,y,z)	cob_call (x, y, z)
#define	cobcancel(x)	cob_cancel (x)

#define	cobgetenv(x)	cob_getenv (x)
#define	cobputenv(x)	cob_putenv (x)
#define cobrescanenv()	0 	/* not necessary as GnuCOBOL always reads the process environment */
#define	cobtidy()	cob_tidy ()
#define	cobinit()	cob_extern_init ()
#define	cobexit(x)	cob_stop_run (x)
#define	cobcommandline(v,w,x,y,z)	cob_command_line (v,w,x,y,z)

#define cobclear()	(void) cob_sys_clear_screen ()
#define cobmove(y,x)	cob_set_cursor_pos (y, x)
#define	cobcols()	cob_get_scr_cols ()
#define	coblines()	cob_get_scr_lines ()
#define cobaddstrc(x)	cob_display_text (x) 		/* no limit [MF=255] */
#define cobprintf	cob_display_formatted_text	/* limit of 2047 [MF=255] */
#define cobgetch()	cob_get_char ()

#define cobget_x1_compx(d)	(cobuns8_t) 	cob_get_u64_compx(d, 1)
#define cobget_x2_compx(d)	(cobuns16_t)	cob_get_u64_compx(d, 2)
#define cobget_x4_compx(d)	(cobuns32_t)	cob_get_u64_compx(d, 4)
#define cobget_x8_compx(d)	(cobuns64_t)	cob_get_u64_compx(d, 8)
#define cobget_sx1_compx(d)	(cobs8_t) 	cob_get_s64_compx(d, 1)
#define cobget_sx2_compx(d)	(cobs16_t)	cob_get_s64_compx(d, 2)
#define cobget_sx4_compx(d)	(cobs32_t)	cob_get_s64_compx(d, 4)
#define cobget_sx8_compx(d)	(cobs64_t)	cob_get_s64_compx(d, 8)
#define cobget_x1_comp5(d)	(cobuns8_t) 	cob_get_u64_comp5(d, 1)
#define cobget_x2_comp5(d)	(cobuns16_t)	cob_get_u64_comp5(d, 2)
#define cobget_x4_comp5(d)	(cobuns32_t)	cob_get_u64_comp5(d, 4)
#define cobget_x8_comp5(d)	(cobuns64_t)	cob_get_u64_comp5(d, 8)
#define cobget_sx1_comp5(d)	(cobs8_t) 	cob_get_s64_comp5(d, 1)
#define cobget_sx2_comp5(d)	(cobs16_t)	cob_get_s64_comp5(d, 2)
#define cobget_sx4_comp5(d)	(cobs32_t)	cob_get_s64_comp5(d, 4)
#define cobget_sx8_comp5(d)	(cobs64_t)	cob_get_s64_comp5(d, 8)
#define cobget_xn_comp5(d,n)	(cobuns64_t)	cob_get_u64_comp5(d, n)
#define cobget_xn_compx(d,n)	(cobuns64_t)	cob_get_u64_compx(d, n)
#define cobget_sxn_comp5(d,n)	(cobs64_t)	cob_get_s64_comp5(d, n)
#define cobget_sxn_compx(d,n)	(cobs64_t)	cob_get_s64_compx(d, n)

#define cobput_x1_compx(d,v)	(void)	cob_put_u64_compx((cob_u64_t)v,d,1)
#define cobput_x2_compx(d,v)	(void)	cob_put_u64_compx((cob_u64_t)v,d,2)
#define cobput_x4_compx(d,v)	(void)	cob_put_u64_compx((cob_u64_t)v,d,4)
#define cobput_x8_compx(d,v)	(void)	cob_put_u64_compx((cob_u64_t)v,d,8)
#define cobput_x1_comp5(d,v)	(void)	cob_put_u64_comp5((cob_u64_t)v,d,1)
#define cobput_x2_comp5(d,v)	(void)	cob_put_u64_comp5((cob_u64_t)v,d,2)
#define cobput_x4_comp5(d,v)	(void)	cob_put_u64_comp5((cob_u64_t)v,d,4)
#define cobput_x8_comp5(d,v)	(void)	cob_put_u64_comp5((cob_u64_t)v,d,8)
#define cobput_sx1_comp5(d,v)	(void)	cob_put_s64_comp5((cob_s64_t)v,d,1)
#define cobput_sx2_comp5(d,v)	(void)	cob_put_s64_comp5((cob_s64_t)v,d,2)
#define cobput_sx4_comp5(d,v)	(void)	cob_put_s64_comp5((cob_s64_t)v,d,4)
#define cobput_sx8_comp5(d,v)	(void)	cob_put_s64_comp5((cob_s64_t)v,d,8)
#define cobput_xn_comp5(d,n,v)	(void)	cob_put_u64_comp5(v, d, n)
#define cobput_xn_compx(d,n,v)	(void)	cob_put_u64_compx(v, d, n)
#define cobput_sxn_comp5(d,n,v)	(void)	cob_put_s64_comp5(v, d, n)
#define cobput_sxn_compx(d,n,v)	(void)	cob_put_s64_compx(v, d, n)

#endif	/* COB_COBCAPI_H */
