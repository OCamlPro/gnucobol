/*
   Copyright (C) 2005-2012, 2014-2023 Free Software Foundation, Inc.
   Written by Roger While, Simon Sobisch, Edward Hart, Brian Tiffin

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


#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <time.h>
#ifdef	HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#include <math.h>

/* include decimal definitions, allowing their use in common.h later */
#ifdef	HAVE_GMP_H
#include <gmp.h>
#elif defined HAVE_MPIR_H
#include <mpir.h>
#else
#error either HAVE_GMP_H or HAVE_MPIR_H needs to be defined
#endif

/* include internal and external libcob definitions, forcing exports */
#define	COB_LIB_EXPIMP
#include "coblocal.h"

/* Note we include the Cygwin version of windows.h here */
#if defined(_WIN32) || defined(__CYGWIN__) || defined(HAVE_LANGINFO_CODESET)
#define LOCTIME_BUFSIZE 128

#if defined(_WIN32) || defined(__CYGWIN__)
#undef	HAVE_LANGINFO_CODESET
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#ifdef	HAVE_LANGINFO_CODESET
#include <langinfo.h>
#endif
#endif

#ifdef	HAVE_LOCALE_H
#include <locale.h>
#endif

/* Function prototypes */
static cob_u32_t	integer_of_date (const int, const int, const int);
static void		get_iso_week (const int, int *, int *);
static void		cob_mpf_log (mpf_t, const mpf_t);
static int		get_seconds_past_midnight (void);

/* Local variables */

static cob_global	*cobglobptr;

static const cob_field_attr	const_alpha_attr =
				{COB_TYPE_ALPHANUMERIC, 0, 0, 0, NULL};

/* Working fields */
static cob_field	*move_field;

static cob_decimal	d1;
static cob_decimal	d2;
static cob_decimal	d3;
static cob_decimal	d4;
static cob_decimal	d5;

#ifndef DISABLE_GMP_RANDOM
static mpf_t			rand_float;	/* Hold our random float numbers */
static gmp_randstate_t	rand_state;			/* Random generator state object */
#endif
static int		rand_needs_seeding = 1;

static mpz_t		cob_mexp;
static mpz_t		cob_mpzt;

static mpf_t		cob_mpft;
static mpf_t		cob_mpft2;
static mpf_t		cob_mpft_get;

static mpf_t		cob_pi;
static mpf_t		cob_sqrt_two;
static mpf_t		cob_log_half;
static mpf_t		cob_log_ten;
static int		set_cob_pi;
static int		set_cob_sqrt_two;
static int		set_cob_log_half;
static int		set_cob_log_ten;


/* Stack definitions for created fields */

struct calc_struct {
	cob_field	calc_field;
	cob_field_attr	calc_attr;
	size_t		calc_size;
};

static struct calc_struct	*calc_base;
static cob_field		*curr_field;
static cob_u32_t		curr_entry;

/* Constants for date/day calculations */
static const int normal_days[] =
	{0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365};
static const int leap_days[] =
	{0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366};
static const int normal_month_days[] =
	{0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
static const int leap_month_days[] =
	{0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};


#define COB_DATESTR_LEN		11
#define	COB_DATESTR_MAX		(COB_DATESTR_LEN - 1)

#define	COB_TIMEDEC_MAX		9

#define COB_TIMESTR_LEN		26 /* including max decimal places */
#define	COB_TIMESTR_MAX		(COB_TIMESTR_LEN - 1)

#define COB_DATETIMESTR_LEN		37
#define	COB_DATETIMESTR_MAX		(COB_DATETIMESTR_LEN - 1)

/* Locale name to Locale ID table */
#if defined(_WIN32) || defined(__CYGWIN__)

struct winlocale {
	const char	*winlocalename;
	const int	winlocaleid;
};

static const struct winlocale	wintable[] =
{
	{ "af_ZA",		0x0436 },
	{ "am_ET",		0x045e },
	{ "ar_AE",		0x3801 },
	{ "ar_BH",		0x3c01 },
	{ "ar_DZ",		0x1401 },
	{ "ar_EG",		0x0c01 },
	{ "ar_IQ",		0x0801 },
	{ "ar_JO",		0x2c01 },
	{ "ar_KW",		0x3401 },
	{ "ar_LB",		0x3001 },
	{ "ar_LY",		0x1001 },
	{ "ar_MA",		0x1801 },
	{ "ar_OM",		0x2001 },
	{ "ar_QA",		0x4001 },
	{ "ar_SA",		0x0401 },
	{ "ar_SY",		0x2801 },
	{ "ar_TN",		0x1c01 },
	{ "ar_YE",		0x2401 },
	{ "arn_CL",		0x047a },
	{ "as_IN",		0x044d },
	{ "az_Cyrl_AZ",		0x082c },
	{ "az_Latn_AZ",		0x042c },
	{ "ba_RU",		0x046d },
	{ "be_BY",		0x0423 },
	{ "bg_BG",		0x0402 },
	{ "bn_IN",		0x0445 },
	{ "bo_BT",		0x0851 },
	{ "bo_CN",		0x0451 },
	{ "br_FR",		0x047e },
	{ "bs_Cyrl_BA",		0x201a },
	{ "bs_Latn_BA",		0x141a },
	{ "ca_ES",		0x0403 },
	{ "cs_CZ",		0x0405 },
	{ "cy_GB",		0x0452 },
	{ "da_DK",		0x0406 },
	{ "de_AT",		0x0c07 },
	{ "de_CH",		0x0807 },
	{ "de_DE",		0x0407 },
	{ "de_LI",		0x1407 },
	{ "de_LU",		0x1007 },
	{ "dsb_DE",		0x082e },
	{ "dv_MV",		0x0465 },
	{ "el_GR",		0x0408 },
	{ "en_029",		0x2409 },
	{ "en_AU",		0x0c09 },
	{ "en_BZ",		0x2809 },
	{ "en_CA",		0x1009 },
	{ "en_GB",		0x0809 },
	{ "en_IE",		0x1809 },
	{ "en_IN",		0x4009 },
	{ "en_JM",		0x2009 },
	{ "en_MY",		0x4409 },
	{ "en_NZ",		0x1409 },
	{ "en_PH",		0x3409 },
	{ "en_SG",		0x4809 },
	{ "en_TT",		0x2c09 },
	{ "en_US",		0x0409 },
	{ "en_ZA",		0x1c09 },
	{ "en_ZW",		0x3009 },
	{ "es_AR",		0x2c0a },
	{ "es_BO",		0x400a },
	{ "es_CL",		0x340a },
	{ "es_CO",		0x240a },
	{ "es_CR",		0x140a },
	{ "es_DO",		0x1c0a },
	{ "es_EC",		0x300a },
	{ "es_ES",		0x040a },
	{ "es_GT",		0x100a },
	{ "es_HN",		0x480a },
	{ "es_MX",		0x080a },
	{ "es_NI",		0x4c0a },
	{ "es_PA",		0x180a },
	{ "es_PE",		0x280a },
	{ "es_PR",		0x500a },
	{ "es_PY",		0x3c0a },
	{ "es_SV",		0x440a },
	{ "es_US",		0x540a },
	{ "es_UY",		0x380a },
	{ "es_VE",		0x200a },
	{ "et_EE",		0x0425 },
	{ "eu_ES",		0x042d },
	{ "fa_IR",		0x0429 },
	{ "fi_FI",		0x040b },
	{ "fil_PH",		0x0464 },
	{ "fo_FO",		0x0438 },
	{ "fr_BE",		0x080c },
	{ "fr_CA",		0x0c0c },
	{ "fr_CH",		0x100c },
	{ "fr_FR",		0x040c },
	{ "fr_LU",		0x140c },
	{ "fr_MC",		0x180c },
	{ "fy_NL",		0x0462 },
	{ "ga_IE",		0x083c },
	{ "gbz_AF",		0x048c },
	{ "gd",			0x043c },
	{ "gl_ES",		0x0456 },
	{ "gsw_FR",		0x0484 },
	{ "gu_IN",		0x0447 },
	{ "ha_Latn_NG",		0x0468 },
	{ "he_IL",		0x040d },
	{ "hi_IN",		0x0439 },
	{ "hr_BA",		0x101a },
	{ "hr_HR",		0x041a },
	{ "hu_HU",		0x040e },
	{ "hy_AM",		0x042b },
	{ "id_ID",		0x0421 },
	{ "ig_NG",		0x0470 },
	{ "ii_CN",		0x0478 },
	{ "is_IS",		0x040f },
	{ "it_CH",		0x0810 },
	{ "it_IT",		0x0410 },
	{ "iu_Cans_CA",		0x045d },
	{ "iu_Latn_CA",		0x085d },
	{ "ja_JP",		0x0411 },
	{ "ka_GE",		0x0437 },
	{ "kh_KH",		0x0453 },
	{ "kk_KZ",		0x043f },
	{ "kl_GL",		0x046f },
	{ "kn_IN",		0x044b },
	{ "ko_KR",		0x0412 },
	{ "kok_IN",		0x0457 },
	{ "ky_KG",		0x0440 },
	{ "lb_LU",		0x046e },
	{ "lo_LA",		0x0454 },
	{ "lt_LT",		0x0427 },
	{ "lv_LV",		0x0426 },
	{ "mi_NZ",		0x0481 },
	{ "mk_MK",		0x042f },
	{ "ml_IN",		0x044c },
	{ "mn_Cyrl_MN",		0x0450 },
	{ "mn_Mong_CN",		0x0850 },
	{ "moh_CA",		0x047c },
	{ "mr_IN",		0x044e },
	{ "ms_BN",		0x083e },
	{ "ms_MY",		0x043e },
	{ "mt_MT",		0x043a },
	{ "nb_NO",		0x0414 },
	{ "ne_NP",		0x0461 },
	{ "nl_BE",		0x0813 },
	{ "nl_NL",		0x0413 },
	{ "nn_NO",		0x0814 },
	{ "ns_ZA",		0x046c },
	{ "oc_FR",		0x0482 },
	{ "or_IN",		0x0448 },
	{ "pa_IN",		0x0446 },
	{ "pl_PL",		0x0415 },
	{ "ps_AF",		0x0463 },
	{ "pt_BR",		0x0416 },
	{ "pt_PT",		0x0816 },
	{ "qut_GT",		0x0486 },
	{ "quz_BO",		0x046b },
	{ "quz_EC",		0x086b },
	{ "quz_PE",		0x0c6b },
	{ "rm_CH",		0x0417 },
	{ "ro_MO",		0x0818 },
	{ "ro_RO",		0x0418 },
	{ "ru_MO",		0x0819 },
	{ "ru_RU",		0x0419 },
	{ "rw_RW",		0x0487 },
	{ "sa_IN",		0x044f },
	{ "sah_RU",		0x0485 },
	{ "se_FI",		0x0c3b },
	{ "se_NO",		0x043b },
	{ "se_SE",		0x083b },
	{ "si_LK",		0x045b },
	{ "sk_SK",		0x041b },
	{ "sl_SI",		0x0424 },
	{ "sma_NO",		0x183b },
	{ "sma_SE",		0x1c3b },
	{ "smj_NO",		0x103b },
	{ "smj_SE",		0x143b },
	{ "smn_FI",		0x243b },
	{ "sms_FI",		0x203b },
	{ "sq_AL",		0x041c },
	{ "sr_Cyrl_BA",		0x1c1a },
	{ "sr_Cyrl_CS",		0x0c1a },
	{ "sr_Latn_BA",		0x181a },
	{ "sr_Latn_CS",		0x081a },
	{ "st",			0x0430 },
	{ "sv_FI",		0x081d },
	{ "sv_SE",		0x041d },
	{ "sw_KE",		0x0441 },
	{ "syr_SY",		0x045a },
	{ "ta_IN",		0x0449 },
	{ "te_IN",		0x044a },
	{ "tg_Cyrl_TJ",		0x0428 },
	{ "th_TH",		0x041e },
	{ "tk_TM",		0x0442 },
	{ "tmz_Latn_DZ",	0x085f },
	{ "tn_ZA",		0x0432 },
	{ "tr_IN",		0x0820 },
	{ "tr_TR",		0x041f },
	{ "ts",			0x0431 },
	{ "tt_RU",		0x0444 },
	{ "ug_CN",		0x0480 },
	{ "uk_UA",		0x0422 },
	{ "ur_PK",		0x0420 },
	{ "uz_Cyrl_UZ",		0x0843 },
	{ "uz_Latn_UZ",		0x0443 },
	{ "vi_VN",		0x042a },
	{ "wen_DE",		0x042e },
	{ "wo_SN",		0x0488 },
	{ "xh_ZA",		0x0434 },
	{ "yi",			0x043d },
	{ "yo_NG",		0x046a },
	{ "zh_CN",		0x0804 },
	{ "zh_HK",		0x0c04 },
	{ "zh_MO",		0x1404 },
	{ "zh_SG",		0x1004 },
	{ "zh_TW",		0x0404 },
	{ "zu_ZA",		0x0435 }
};

#define	WINLOCSIZE	sizeof(wintable) / sizeof(struct winlocale)

#endif

static COB_NOINLINE void
setup_cob_pi (void)
{
	/* Pi - Next 3 digits 000 */
	const char	cob_pi_str[] =
		"3.141592653589793238462643383279502884197169399375"
		"10582097494459230781640628620899862803482534211706"
		"79821480865132823066470938446095505822317253594081"
		"28481117450284102701938521105559644622948954930381"
		"96442881097566593344612847564823378678316527120190"
		"91456485669234603486104543266482133936072602491412"
		"73724587006606315588174881520920962829254091715364"
		"36789259036001133053054882046652138414695194151160"
		"94330572703657595919530921861173819326117931051185"
		"48074462379962749567351885752724891227938183011949"
		"12983367336244065664308602139494639522473719070217"
		"98609437027705392171762931767523846748184676694051"
		"32000568127145263560827785771342757789609173637178"
		"72146844090122495343014654958537105079227968925892"
		"35420199561121290219608640344181598136297747713099"
		"60518707211349999998372978049951059731732816096318"
		"59502445945534690830264252230825334468503526193118"
		"817101";
	const unsigned long COB_PI_LEN = 2820UL;

	mpf_init2 (cob_pi, COB_PI_LEN);
	mpf_set_str (cob_pi, cob_pi_str, 10);
	set_cob_pi = 1;
}

static COB_NOINLINE void
setup_cob_sqrt_two (void)
{
	/* Sqrt 2 - Next 3 digits 001 */
	const char	cob_sqrt_two_str[] =
		"1.414213562373095048801688724209698078569671875376"
		"94807317667973799073247846210703885038753432764157"
		"27350138462309122970249248360558507372126441214970"
		"99935831413222665927505592755799950501152782060571"
		"47010955997160597027453459686201472851741864088919"
		"86095523292304843087143214508397626036279952514079"
		"89687253396546331808829640620615258352395054745750"
		"28775996172983557522033753185701135437460340849884"
		"71603868999706990048150305440277903164542478230684"
		"92936918621580578463111596668713013015618568987237"
		"23528850926486124949771542183342042856860601468247"
		"20771435854874155657069677653720226485447015858801"
		"62075847492265722600208558446652145839889394437092"
		"65918003113882464681570826301005948587040031864803"
		"42194897278290641045072636881313739855256117322040"
		"24509122770022694112757362728049573810896750401836"
		"98683684507257993647290607629969413804756548237289"
		"97180326802474420629269124859052181004459842150591"
		"12024944134172853147810580360337107730918286931471"
		"01711116839165817268894197587165821521282295184884"
		"72089694633862891562882765952635140542267653239694"
		"61751129160240871551013515045538128756005263146801"
		"71274026539694702403005174953188629256313851881634"
		"78";
	const unsigned long COB_SQRT_TWO_LEN = 3827UL;

	mpf_init2 (cob_sqrt_two, COB_SQRT_TWO_LEN);
	mpf_set_str (cob_sqrt_two, cob_sqrt_two_str, 10);
	set_cob_sqrt_two = 1;
}

static COB_NOINLINE void
setup_cob_log_half (void)
{
	/* Log 0.5 - Next 3 digits 000 */
	const char	cob_log_half_str[] =
		"-0.69314718055994530941723212145817656807550013436"
		"02552541206800094933936219696947156058633269964186"
		"87542001481020570685733685520235758130557032670751"
		"63507596193072757082837143519030703862389167347112"
		"33501153644979552391204751726815749320651555247341"
		"39525882950453007095326366642654104239157814952043"
		"74043038550080194417064167151864471283996817178454"
		"69570262716310645461502572074024816377733896385506"
		"95260668341137273873722928956493547025762652098859"
		"69320196505855476470330679365443254763274495125040"
		"60694381471046899465062201677204245245296126879465"
		"46193165174681392672504103802546259656869144192871"
		"60829380317271436778265487756648508567407764845146"
		"44399404614226031930967354025744460703080960850474"
		"86638523138181676751438667476647890881437141985494"
		"23151997354880375165861275352916610007105355824987"
		"94147295092931138971559982056543928717";
	const unsigned long COB_LOG_HALF_LEN = 2784UL;

	mpf_init2 (cob_log_half, COB_LOG_HALF_LEN);
	mpf_set_str (cob_log_half, cob_log_half_str, 10);
	set_cob_log_half = 1;
}

static COB_NOINLINE void
setup_cob_log_ten (void)
{
	mpf_init2 (cob_log_ten, COB_MPF_PREC);
	mpf_set_ui (cob_log_ten, 10UL);
	cob_mpf_log (cob_log_ten, cob_log_ten);
	set_cob_log_ten = 1;
}

#define RETURN_IF_NOT_ZERO(expr)		\
	do {					\
		int error_pos = (expr);		\
		if (error_pos != 0) {		\
			return error_pos;	\
		}				\
	} ONCE_COB

/* Local functions */

static void
make_field_entry (cob_field *f)
{
	struct calc_struct	*calc_temp;
	unsigned char		*s;

	calc_temp = calc_base + curr_entry;
	curr_field = &calc_temp->calc_field;
	if (f->size > calc_temp->calc_size) {
		/* set new temporary field data, storing its size */
		if (curr_field->data) {
			cob_free (curr_field->data);
		}
		calc_temp->calc_size = f->size + 1;
		s = cob_malloc (f->size + 1U);
	} else {
		/* reuse last temporary field data */
		s = curr_field->data;
		memset (s, 0, f->size);
	}

	*curr_field = *f;
	calc_temp->calc_attr = *(f->attr);
	curr_field->attr = &calc_temp->calc_attr;

	curr_field->data = s;

	if (++curr_entry >= COB_DEPTH_LEVEL) {
		curr_entry = 0;
	}
}

static int
leap_year (const int year)
{
	return ((year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)) ? 1 : 0;
}

static int
comp_field (const void *m1, const void *m2)
{
	cob_field	*f1;
	cob_field	*f2;

	f1 = *(cob_field **) m1;
	f2 = *(cob_field **) m2;
	return cob_cmp (f1, f2);
}

/* Reference modification */
static void
calc_ref_mod (cob_field *f, const int offset, const int length)
{
	size_t		calcoff;
	size_t		size;

	if ((size_t)offset <= f->size) {
		calcoff = (size_t)offset - 1;
		size = f->size - calcoff;
		if (length > 0 && (size_t)length < size) {
			size = (size_t)length;
		}
		f->size = size;
		if (calcoff > 0) {
			memmove (f->data, f->data + calcoff, size);
		}
	}
}

/* Trim trailing zeros in decimal places */
static void
cob_trim_decimal (cob_decimal *d)
{
	if (mpz_sgn (d->value) == 0) {
		/* Value is zero */
		d->scale = 0;
		return;
	}
	for ( ; d->scale > 0; d->scale--) {
		if (!mpz_divisible_ui_p (d->value, 10UL)) {
			break;
		}
		mpz_tdiv_q_ui (d->value, d->value, 10UL);
	}
}

static void
cob_alloc_set_field_int (const int val)
{
	cob_u16_t	attrsign;
	cob_field_attr	attr;
	cob_field	field;

	if (val < 0) {
		attrsign = COB_FLAG_HAVE_SIGN;
	} else {
		attrsign = 0;
	}
	COB_ATTR_INIT (COB_TYPE_NUMERIC_BINARY, 9,
		       0, attrsign, NULL);
	COB_FIELD_INIT (4, NULL, &attr);
	make_field_entry (&field);
	memcpy (curr_field->data, &val, sizeof(int));
}

static void
cob_alloc_set_field_uint (const cob_u32_t val)
{
	cob_field_attr	attr;
	cob_field	field;

	COB_ATTR_INIT (COB_TYPE_NUMERIC_BINARY, 9, 0, 0, NULL);
	COB_FIELD_INIT (4, NULL, &attr);
	make_field_entry (&field);
	memcpy (curr_field->data, &val, sizeof(cob_u32_t));
}

static void
cob_alloc_field (cob_decimal *d)
{
	size_t		bitnum;
	int 		negative_sign_pos;
	unsigned short	attrsign;
	short	size, scale;
	cob_field_attr	attr;
	cob_field	field;

	if (unlikely (d->scale == COB_DECIMAL_NAN)) {
		/* Check this */
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		COB_ATTR_INIT (COB_TYPE_NUMERIC_BINARY, 9, 0, 0, NULL);
		COB_FIELD_INIT (4, NULL, &attr);
		make_field_entry (&field);
		return;
	}

	if (mpz_sgn (d->value) == -1) {
		attrsign = COB_FLAG_HAVE_SIGN;
		negative_sign_pos = 1;
	} else {
		attrsign = 0;
		negative_sign_pos = 0;
	}

	cob_trim_decimal (d);

	bitnum = mpz_sizeinbase (d->value, 2);
	if (bitnum < (33 - negative_sign_pos) && d->scale < 10) {
		/* 4 bytes binary */
		COB_ATTR_INIT (COB_TYPE_NUMERIC_BINARY, 9,
			       (short)d->scale, attrsign, NULL);
		COB_FIELD_INIT (4, NULL, &attr);
		make_field_entry (&field);
	} else if (bitnum < (65 - negative_sign_pos) && d->scale < 19) {
		/* 8 bytes binary */
		COB_ATTR_INIT (COB_TYPE_NUMERIC_BINARY, 20,
			       (short)d->scale, attrsign, NULL);
		COB_FIELD_INIT (8, NULL, &attr);
		make_field_entry (&field);
	} else {
		/* Display decimal */
		size = (short)mpz_sizeinbase (d->value, 10);
		if (d->scale > size) {
			size = (short)d->scale;
		}
		scale = (short)d->scale;
		COB_ATTR_INIT (COB_TYPE_NUMERIC_DISPLAY, size,
			       scale, attrsign, NULL);
		COB_FIELD_INIT (size, NULL, &attr);
		make_field_entry (&field);
	}
}

/* Common function for intrinsics MOD and REM */

static cob_field *
cob_mod_or_rem (cob_field *f1, cob_field *f2, const int func_is_rem)
{
	cobglobptr->cob_exception_code = 0;
	cob_decimal_set_field (&d2, f1);
	cob_decimal_set_field (&d3, f2);

	if (mpz_sgn (d3.value) == 0) {
		/* function argument violation */
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}

	cob_decimal_div (&d2, &d3);

	/* Calculate integer / integer-part */
	if (d2.scale < 0) {
		mpz_ui_pow_ui (cob_mexp, 10UL, (cob_uli_t)-d2.scale);
		mpz_mul (d2.value, d2.value, cob_mexp);
	} else if (d2.scale > 0) {
		mpz_ui_pow_ui (cob_mexp, 10UL, (cob_uli_t)d2.scale);
		if (func_is_rem) {
			/* REMAINDER function - INTEGER-PART */
			mpz_tdiv_q (d2.value, d2.value, cob_mexp);
		} else {
			const int	sign = mpz_sgn (d2.value);
			/* MOD function - INTEGER */
			mpz_tdiv_qr (d2.value, cob_mpzt, d2.value, cob_mexp);
			/* Check negative and has decimal places */
			if (sign == -1 && mpz_sgn (cob_mpzt) != 0) {
				mpz_sub_ui (d2.value, d2.value, 1UL);
			}
		}
	}
	d2.scale = 0;

	cob_decimal_set_field (&d1, f2);
	cob_decimal_mul (&d2, &d1);
	cob_decimal_set_field (&d1, f1);
	cob_decimal_sub (&d1, &d2);

	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

/* TEST-NUMVAL-F implementation */

/* Validate NUMVAL-F item */
/* sp = spaces */
/* [sp][+|-][sp]{digits[.[digits]]|.digits}[sp][E[sp]{+|-}[sp]digits[sp]] */

int
cob_check_numval_f (const cob_field *srcfield)
{
	unsigned char	*p = srcfield->data;
	size_t		plus_minus;
	size_t		digits;
	size_t		decimal_seen;
	size_t		space_seen;
	size_t		e_seen;
	size_t		break_needed;
	size_t		exponent;
	size_t		e_plus_minus;
	int		n;
	const unsigned char	dec_pt = COB_MODULE_PTR->decimal_point;

	if (!srcfield->size) {
		return 1;
	}

	/* FIXME later: srcfield may be of category national... */

	plus_minus = 0;
	digits = 0;
	decimal_seen = 0;
	space_seen = 0;
	e_seen = 0;
	break_needed = 0;
	exponent = 0;
	e_plus_minus = 0;

	/* Check leading positions */
	for (n = 0; n < (int)srcfield->size; ++n, ++p) {
		switch (*p) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			break_needed = 1;
			break;
		case ' ':
			continue;
		case '+':
		case '-':
			if (plus_minus) {
				return n + 1;
			}
			plus_minus = 1;
			continue;
		case ',':
		case '.':
			if (*p != dec_pt) {
				return n + 1;
			}
			break_needed = 1;
			break;
		default:
			return n + 1;
		}
		if (break_needed) {
			break;
		}
	}

	if (n == (int)srcfield->size) {
		return n + 1;
	}

	for (; n < (int)srcfield->size; ++n, ++p) {
		switch (*p) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			if (e_seen) {
				if (++exponent > 4 || !e_plus_minus) {
					return n + 1;
				}
			} else if (++digits > COB_MAX_DIGITS || space_seen) {
				return n + 1;
			}
			continue;
		case ',':
		case '.':
			if (decimal_seen || space_seen || e_seen) {
				return n + 1;
			}
			if (*p == dec_pt) {
				decimal_seen = 1;
				continue;
			}
			return n + 1;
		case ' ':
			space_seen = 1;
			continue;
		case 'E':
		case 'e':
			if (e_seen) {
				return n + 1;
			}
			e_seen = 1;
			continue;
		case '+':
		case '-':
			if (e_seen) {
				if (e_plus_minus) {
					return n + 1;
				}
				e_plus_minus = 1;
			} else {
				if (plus_minus) {
					return n + 1;
				}
				plus_minus = 1;
			}
			continue;
		default:
			return n + 1;
		}
	}

	if (!digits || (e_seen && !exponent)) {
		return n + 1;
	}

	return 0;
}

/* Trigonometric formulae (formulas?) from Wikipedia */


/* Exp function */
/* e ^ x = {n = 0, ...} ( (x ^ n) / n! ) */

static void
cob_mpf_exp (mpf_t dst_val, const mpf_t src_val)
{
	mpf_t			vf1, vf2, vf3;
	mpf_t			dst_temp;
	cob_sli_t		expon, i;
	cob_uli_t		n;
	cob_u32_t		is_negative;


	mpf_init2 (dst_temp, COB_MPF_PREC);

	mpf_init2 (vf1, COB_MPF_PREC);
	mpf_set (vf1, src_val);
	mpf_init2 (vf2, COB_MPF_PREC);
	mpf_set_ui (vf2, 1UL);
	mpf_init2 (vf3, COB_MPF_PREC);

	mpf_set_ui (dst_temp, 1UL);

	if (mpf_sgn (vf1) < 0) {
		mpf_neg (vf1, vf1);
		is_negative = 1;
	} else {
		is_negative = 0;
	}

	mpf_get_d_2exp (&expon, vf1);
	if (expon > 0) {
		mpf_div_2exp (vf1, vf1, (cob_uli_t)expon);
	}

	n = 1;
	do {
		mpf_mul (vf2, vf2, vf1);
		mpf_div_ui (vf2, vf2, (cob_uli_t)n);
		mpf_set (vf3, dst_temp);
		mpf_add (dst_temp, dst_temp, vf2);
		++n;
	} while (!mpf_eq (vf3, dst_temp, COB_MPF_CUTOFF));

	for (i = 0; i < expon; ++i) {
		mpf_mul (dst_temp, dst_temp, dst_temp);
	}

	if (is_negative) {
		mpf_ui_div (dst_temp, 1UL, dst_temp);
	}

	mpf_set (dst_val, dst_temp);
	mpf_clear (dst_temp);

	mpf_clear (vf3);
	mpf_clear (vf2);
	mpf_clear (vf1);
}

/* Log function */
/* logn (x) = {n = 1, ...} ( ((1 - x) ^ n) / n ) */

static void
cob_mpf_log (mpf_t dst_val, const mpf_t src_val)
{
	mpf_t			vf1, vf2, vf3, vf4;
	mpf_t			dst_temp;
	cob_sli_t		expon;
	cob_uli_t		n;



	if (mpf_sgn (src_val) <= 0 || !mpf_cmp_ui (src_val, 1UL)) {
		mpf_set_ui (dst_val, 0UL);
		return;
	}

	mpf_init2 (dst_temp, COB_MPF_PREC);
	if (!set_cob_log_half) setup_cob_log_half ();

	mpf_init2 (vf1, COB_MPF_PREC);
	mpf_set (vf1, src_val);
	mpf_init2 (vf2, COB_MPF_PREC);
	mpf_init2 (vf3, COB_MPF_PREC);
	mpf_set_si (vf3, -1L);
	mpf_init2 (vf4, COB_MPF_PREC);

	mpf_set_ui (dst_temp, 0UL);
	mpf_get_d_2exp (&expon, vf1);
	if (expon != 0) {
		mpf_set (dst_temp, cob_log_half);
		if (expon > 0) {
			mpf_mul_ui (dst_temp, dst_temp, (cob_uli_t)expon);
			mpf_neg (dst_temp, dst_temp);
			mpf_div_2exp (vf1, vf1, (cob_uli_t)expon);
		} else {
			mpf_mul_ui (dst_temp, dst_temp, (cob_uli_t)-expon);
			mpf_mul_2exp (vf1, vf1, (cob_uli_t)-expon);
		}
	}
	mpf_ui_sub (vf1, 1UL, vf1);

	n = 1;
	do {
		mpf_mul (vf3, vf3, vf1);
		mpf_div_ui (vf2, vf3, n);
		mpf_set (vf4, dst_temp);
		mpf_add (dst_temp, dst_temp, vf2);
		++n;
	} while (!mpf_eq (vf4, dst_temp, COB_MPF_CUTOFF));

	mpf_set (dst_val, dst_temp);
	mpf_clear (dst_temp);

	mpf_clear (vf4);
	mpf_clear (vf3);
	mpf_clear (vf2);
	mpf_clear (vf1);
}

/* Log10 function */
/* log10 (x) = log (x) / log (10) */

static void
cob_mpf_log10 (mpf_t dst_val, const mpf_t src_val)
{
	mpf_t			dst_temp;

	mpf_init2 (dst_temp, COB_MPF_PREC);
	if (!set_cob_log_ten) setup_cob_log_ten ();

	cob_mpf_log (dst_temp, src_val);
	mpf_div (dst_temp, dst_temp, cob_log_ten);

	mpf_set (dst_val, dst_temp);
	mpf_clear (dst_temp);
}

/* Sin function */
/* sin (x) = (reduce to pi/2) */
/* {n = 0, ...} ( (-1 ^ n) * ( x ^ (2n + 1)) / (2n + 1) ) */

static void
cob_mpf_sin (mpf_t dst_val, const mpf_t src_val)
{
	mpf_t			vf1, vf2, vf3, vf4, vf5;
	mpf_t			dst_temp;
	cob_uli_t		arcquad;
	cob_uli_t		n;
	int			sign;

	mpf_init2 (dst_temp, COB_MPF_PREC);
	if (!set_cob_pi) setup_cob_pi ();

	mpf_init2 (vf1, COB_MPF_PREC);
	mpf_init2 (vf2, COB_MPF_PREC);
	mpf_init2 (vf3, COB_MPF_PREC);
	mpf_init2 (vf4, COB_MPF_PREC);
	mpf_init2 (vf5, COB_MPF_PREC);
	sign = mpf_sgn (src_val);

	mpf_abs (vf4, src_val);
	mpf_set (vf3, cob_pi);
	mpf_div_2exp (vf3, vf3, 1UL);
	mpf_div (vf1, vf4, vf3);
	mpf_floor (vf4, vf1);

	if (mpf_cmp_ui (vf4, 4UL) >= 0) {
		mpf_div_2exp (vf2, vf4, 2UL);
		mpf_floor (vf2, vf2);
		mpf_mul_2exp (vf2, vf2, 2UL);
		mpf_sub (vf2, vf4, vf2);
	} else {
		mpf_set (vf2, vf4);
	}

	arcquad = mpf_get_ui (vf2);
	mpf_sub (vf2, vf1, vf4);
	mpf_mul (vf4, vf3, vf2);

	if (arcquad > 1) {
		sign = -sign;
	}
	if (arcquad & 1) {
		mpf_sub (vf4, vf3, vf4);
	}

	mpf_mul (vf3, vf4, vf4);
	mpf_neg (vf3, vf3);

	n = 1;
	mpf_set_ui (vf2, 1UL);
	mpf_set_ui (dst_temp, 1UL);

	do {
		++n;
		mpf_div_ui (vf2, vf2, n);
		++n;
		mpf_div_ui (vf2, vf2, n);
		mpf_mul (vf2, vf2, vf3);
		mpf_set (vf5, dst_temp);
		mpf_add (dst_temp, dst_temp, vf2);
	} while (!mpf_eq (vf5, dst_temp, COB_MPF_PREC));

	mpf_mul (dst_temp, dst_temp, vf4);
	if (sign < 0) {
		mpf_neg (dst_temp, dst_temp);
	}

	mpf_set (dst_val, dst_temp);
	mpf_clear (dst_temp);

	mpf_clear (vf5);
	mpf_clear (vf4);
	mpf_clear (vf3);
	mpf_clear (vf2);
	mpf_clear (vf1);
}

/* Cos function */
/* cos (x) = sin ((pi / 2) - x) */

static void
cob_mpf_cos (mpf_t dst_val, const mpf_t src_val)
{
	mpf_t		vf1;

	mpf_init2 (vf1, COB_MPF_PREC);
	if (!set_cob_pi) setup_cob_pi ();

	mpf_set (vf1, cob_pi);
	mpf_div_2exp (vf1, vf1, 1UL);
	mpf_sub (vf1, vf1, src_val);
	cob_mpf_sin (dst_val, vf1);

	mpf_clear (vf1);
}

/* Tan function */
/* tan (x) = sin(x) / cos(x) */

static void
cob_mpf_tan (mpf_t dst_val, const mpf_t src_val)
{
	mpf_t		vf1;
	mpf_t		vf2;

	mpf_init2 (vf1, COB_MPF_PREC);
	mpf_init2 (vf2, COB_MPF_PREC);

	cob_mpf_sin (vf1, src_val);
	cob_mpf_cos (vf2, src_val);
	mpf_div (dst_val, vf1, vf2);

	mpf_clear (vf1);
	mpf_clear (vf2);
}

/* Atan function */

static void
cob_mpf_atan (mpf_t dst_val, const mpf_t src_val)
{
	mpf_t			vf1, vf2, vf3, vf4;
	mpf_t			dst_temp;
	cob_uli_t		n;

	mpf_init2 (dst_temp, COB_MPF_PREC);
	if (!set_cob_pi) setup_cob_pi ();
	if (!set_cob_sqrt_two) setup_cob_sqrt_two ();

	mpf_init2 (vf1, COB_MPF_PREC);
	mpf_init2 (vf2, COB_MPF_PREC);
	mpf_init2 (vf3, COB_MPF_PREC);
	mpf_init2 (vf4, COB_MPF_PREC);

	mpf_abs (vf1, src_val);
	mpf_add_ui (vf3, cob_sqrt_two, 1UL);

	if (mpf_cmp (vf1, vf3) > 0) {
		mpf_set (dst_temp, cob_pi);
		mpf_div_2exp (dst_temp, dst_temp, 1UL);
		mpf_ui_div (vf1, 1UL, vf1);
		mpf_neg (vf1, vf1);
	} else {
		mpf_sub_ui (vf4, cob_sqrt_two, 1UL);
		if (mpf_cmp (vf1, vf4) > 0) {
			mpf_set (dst_temp, cob_pi);
			mpf_div_2exp (dst_temp, dst_temp, 2UL);
			mpf_sub_ui (vf3, vf1, 1UL);
			mpf_add_ui (vf4, vf1, 1UL);
			mpf_div (vf1, vf3, vf4);
		} else {
			mpf_set_ui (dst_temp, 0UL);
		}
	}
	mpf_mul (vf2, vf1, vf1);
	mpf_neg (vf2, vf2);
	mpf_add (dst_temp, dst_temp, vf1);

	n = 1;

	do {
		mpf_mul (vf1, vf1, vf2);
		mpf_div_ui (vf3, vf1, 2ULL * n + 1);
		mpf_set (vf4, dst_temp);
		mpf_add (dst_temp, dst_temp, vf3);
		++n;
	} while (!mpf_eq (vf4, dst_temp, COB_MPF_PREC));

	if (mpf_sgn (src_val) < 0) {
		mpf_neg (dst_temp, dst_temp);
	}

	mpf_set (dst_val, dst_temp);
	mpf_clear (dst_temp);

	mpf_clear (vf4);
	mpf_clear (vf3);
	mpf_clear (vf2);
	mpf_clear (vf1);
}

/* Asin function */
/* asin (x) = 2 * atan (x / (1 + sqrt (1 - (x ** 2)))) */

static void
cob_mpf_asin (mpf_t dst_val, const mpf_t src_val)
{
	mpf_t			vf1, vf2;
	mpf_t			dst_temp;

	mpf_init2 (dst_temp, COB_MPF_PREC);
	if (!set_cob_pi) setup_cob_pi ();

	if (!mpf_cmp_ui (src_val, 1UL) || !mpf_cmp_si (src_val, -1L)) {
		mpf_set (dst_temp, cob_pi);
		mpf_div_ui (dst_temp, dst_temp, 2UL);
		if (mpf_sgn (src_val) < 0) {
			mpf_neg (dst_temp, dst_temp);
		}
		mpf_set (dst_val, dst_temp);
		mpf_clear (dst_temp);
		return;
	}
	if (mpz_sgn (src_val) == 0) {
		mpf_set_ui (dst_val, 0UL);
		mpf_clear (dst_temp);
		return;
	}

	mpf_init2 (vf1, COB_MPF_PREC);
	mpf_init2 (vf2, COB_MPF_PREC);

	mpf_mul (vf2, src_val, src_val);
	mpf_ui_sub (vf2, 1UL, vf2);
	mpf_sqrt (vf2, vf2);

	mpf_add_ui (vf2, vf2, 1UL);

	mpf_div (vf1, src_val, vf2);
	cob_mpf_atan (dst_temp, vf1);
	mpf_mul_ui (dst_temp, dst_temp, 2UL);

	mpf_set (dst_val, dst_temp);
	mpf_clear (dst_temp);

	mpf_clear (vf2);
	mpf_clear (vf1);
}

/* Acos function */
/* acos (x) = 2 * atan (sqrt (1 - (x ** 2)) / (1 + x)) */

static void
cob_mpf_acos (mpf_t dst_val, const mpf_t src_val)
{
	mpf_t			vf1, vf2;
	mpf_t			dst_temp;

	mpf_init2 (dst_temp, COB_MPF_PREC);
	if (!set_cob_pi) setup_cob_pi ();

	if (!mpf_sgn (src_val)) {
		mpf_set (dst_temp, cob_pi);
		mpf_div_ui (dst_temp, dst_temp, 2UL);
		mpf_set (dst_val, dst_temp);
		mpf_clear (dst_temp);
		return;
	}
	if (!mpf_cmp_ui (src_val, 1UL)) {
		mpf_set_ui (dst_val, 0UL);
		mpf_clear (dst_temp);
		return;
	}
	if (!mpf_cmp_si (src_val, -1L)) {
		mpf_set (dst_val, cob_pi);
		mpf_clear (dst_temp);
		return;
	}

	mpf_init2 (vf1, COB_MPF_PREC);
	mpf_init2 (vf2, COB_MPF_PREC);

	mpf_add_ui (vf2, src_val, 1UL);
	mpf_mul (vf1, src_val, src_val);
	mpf_ui_sub (vf1, 1UL, vf1);
	mpf_sqrt (vf1, vf1);
	mpf_div (vf1, vf1, vf2);
	cob_mpf_atan (dst_temp, vf1);
	mpf_mul_ui (dst_temp, dst_temp, 2UL);

	mpf_set (dst_val, dst_temp);
	mpf_clear (dst_temp);

	mpf_clear (vf2);
	mpf_clear (vf1);
}

/* SUBSTITUTE(-CASE) functions */

static size_t
get_substituted_size (cob_field *original, cob_field **matches, cob_field **reps,
		      const int numreps,
		      int (*cmp_func)(const void *, const void *, size_t))
{
	unsigned char	*match_begin = original->data;
	size_t	        orig_size = original->size;
	size_t		calcsize = 0;
	size_t		cur_idx;
	size_t		found = 0;
	int		i;

	for (cur_idx = 0; cur_idx < orig_size; ) {
		/* Try to find a match at this point */
		for (i = 0; i < numreps; ++i) {
			/* If we overflow the string */
			if (cur_idx + matches[i]->size > orig_size) {
			        continue;
			}

			/* If we find a match */
			if (!(*cmp_func) (match_begin, matches[i]->data, matches[i]->size)) {
				/* Go past it */
				match_begin += matches[i]->size;
				cur_idx += matches[i]->size;
				/* Keep track how long new string will be */
				calcsize += reps[i]->size;

				found = 1;
				break;
			}
		}

		if (found) {
			found = 0;
		} else {
			/* Move forward one char */
			++cur_idx;
			++match_begin;
			++calcsize;
		}
	}

	return calcsize;
}

static void
substitute_matches (cob_field *original, cob_field **matches, cob_field **reps,
		    const int numreps,
		    int (*cmp_func)(const void *, const void *, size_t),
		    unsigned char *replaced_begin)
{
	unsigned char	*match_begin = original->data;
	size_t	        orig_size = original->size;
	size_t		cur_idx;
	size_t		found = 0;
	int		i;

	for (cur_idx = 0; cur_idx < orig_size; ) {
		/* Try to find a match at this point. */
		for (i = 0; i < numreps; ++i) {
			/* If we overrun */
			if (cur_idx + matches[i]->size > orig_size) {
				continue;
			}

			/* If we find a match */
			if (!(*cmp_func) (match_begin, matches[i]->data, matches[i]->size)) {
				/* Write the replacement */
				memcpy (replaced_begin, reps[i]->data, reps[i]->size);
				/* Move past the match/replacement */
				match_begin += matches[i]->size;
				replaced_begin += reps[i]->size;
				cur_idx += matches[i]->size;

				found = 1;
				break;
			}
		}

		if (found) {
			found = 0;
			continue;
		} else {
			/* Add unmatched char to final string and move on one */
			++cur_idx;
			*replaced_begin++ = *match_begin++;
		}
	}
}

static cob_field *
substitute (const int offset, const int length, const int params,
	    int (*cmp_func)(const void *, const void *, size_t),
	    va_list args)
{

	cob_field	*original;
	cob_field	**matches;
	cob_field	**reps;
	int		i;
	size_t		calcsize;
	int		numreps = params / 2;
	cob_field	field;

	matches = cob_malloc ((size_t)numreps * sizeof (cob_field *));
	reps = cob_malloc ((size_t)numreps * sizeof (cob_field *));

	/* Extract args */
	original = va_arg (args, cob_field *);
	for (i = 0; i < params - 1; ++i) {
		if ((i % 2) == 0) {
			matches[i / 2] = va_arg (args, cob_field *);
		} else {
			reps[i / 2] = va_arg (args, cob_field *);
		}
	}

	va_end (args);

	/* Perform substitution */

	calcsize = get_substituted_size (original, matches, reps, numreps, cmp_func);

	COB_FIELD_INIT (0, NULL, &const_alpha_attr);
	field.size = calcsize;
	make_field_entry (&field);

	substitute_matches (original, matches, reps, numreps, cmp_func, curr_field->data);

	/* Output placed in curr_field */

	cob_free (matches);
	cob_free (reps);

	if (unlikely (offset > 0)) {
		calc_ref_mod (curr_field, offset, length);
	}
	return curr_field;
}

static int
int_strncasecmp (const void *s1, const void *s2, size_t n)
{
	return (int) strncasecmp (s1, s2, n);
}

/* NUMVAL + NUMVAL-C implementation */

static COB_INLINE COB_A_INLINE size_t
space_left (unsigned char * p, unsigned char *p_end)
{
	return p_end - p + 1;
}

/* checks for CR / DB at current position, case-insensitive */
static COB_INLINE COB_A_INLINE int
at_cr_or_db (const unsigned char *p)
{
	return (toupper (p[0]) == 'C' && toupper (p[1]) == 'R')
	    || (toupper (p[0]) == 'D' && toupper (p[1]) == 'B');
}

/* get first and last position of possible numeric data */
static size_t
calculate_start_end_for_numval (cob_field *srcfield,
				unsigned char **pp, unsigned char **pp_end)
{
	unsigned char *p = srcfield->data;
	unsigned char *p_end;

	if (srcfield->size == 0 
	 || p == NULL) {
		return 0;
	}

	/* skip trailing space and low-value */
	p_end = p + srcfield->size - 1;
	while (p != p_end) {
		if (*p_end != ' ' && *p_end != 0) break;
		p_end--;
	}

	/* skip leading space and zero (but not low-value) */
	while (p != p_end) {
		if (*p != ' ' && *p != '0') break;
		p++;
	}

	*pp = p;
	*pp_end = p_end;

	return p_end - p + 1;
}

enum numval_type {
	NUMVAL,
	NUMVAL_C
};

static cob_field *
numval (cob_field *srcfield, cob_field *currency, const enum numval_type type)
{
	unsigned char	*final_buff = NULL;
	unsigned char	*p, *p_end;
	unsigned char	*currency_data = NULL;
	size_t		datasize;
	int		digits, decimal_digits;
	int		sign, decimal_seen, currency_seen, exception;
	const unsigned char	dec_pt = COB_MODULE_PTR->decimal_point;
	const unsigned char	num_sep = COB_MODULE_PTR->numeric_separator;
	const unsigned char	cur_symb = COB_MODULE_PTR->currency_symbol;

	/* note: versions before 3.2 did a pre-validation here,
			 we now parse "as valid as possible" by default
			 (the testsuite checks both variants) */
#ifdef INVALID_NUMVAL_IS_ZERO 
	/* Validate source field */
	if (cob_check_numval (srcfield, currency, type == NUMVAL_C, 0)) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}
#endif

	/* FIXME later: srcfield may be of category national... */

	/* get size along with first/last relevant position */
	datasize = calculate_start_end_for_numval (srcfield, &p, &p_end);

	/* no data -> zero */
	if (datasize == 0) {
		cob_alloc_set_field_uint (0);
		return curr_field;
	}
	/* not wasting buffer space (COBOL2022: 35/34 max)... */
	if (datasize > COB_MAX_DIGITS) {
		datasize = COB_MAX_DIGITS;
	}

	/* acquire temp buffer long enugh */
	final_buff = cob_malloc (datasize + 1U);

	sign = 0;
	digits = 0;
	decimal_digits = 0;
	decimal_seen = 0;
	currency_seen = 0;
	exception = 0;

	if (type == NUMVAL_C && currency && currency->size < datasize) {
		currency_data = currency->data;
	}

	for ( /* start value for p set above */ ; p <= p_end ; ++p) {
		if (space_left (p, p_end) >= 2
		 && at_cr_or_db (p)) {
			/* CR / DB always wins in GnuCOBOL, sets the sign and ends */
			if (sign) {
				/* that's an error, no need to check further */
				exception = 1;
			} else {
				/* post validation - only spaces allowed */
				p += 2;
				while (p <= p_end) {
					if (*p != ' ') {
						exception = 1;
						break;
					}
					p++;
				}
			}
			sign = -1;
			goto game_over;
		}

		if (currency_data) {
			if (space_left (p, p_end) >= currency->size
			 && !memcmp (p, currency_data, currency->size)) {
				if (currency_seen) {
					exception = 1;
				} else {
					if (digits != 0 || decimal_seen) {
						exception = 1;
					}
					currency_seen = 1;
				}
				p += (currency->size - 1);
				continue;
			}
		} else if (type == NUMVAL_C && *p == cur_symb) {
			if (currency_seen) {
				exception = 1;
			} else {
				currency_seen = 1;
			}
			continue;
		}

		switch (*p) {
		case '0':
			if (digits == 0 && !decimal_seen) {
				/* no data yet, so just skip */
				continue;
			}
			/* Fall through */
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			if (decimal_seen) {
				decimal_digits++;
			}
			final_buff[digits++] = *p;
			if (digits > COB_MAX_DIGITS) {
				exception = 1;
				goto game_over;
			}
			continue;
		case '+':
			if (sign) {
				exception = 1;
			} else {
				sign = 1;
			}
			continue;
		case '-':
			if (sign) {
				exception = 1;
			} else {
				sign = -1;
			}
			continue;
		case ' ':
			/* note: we don't check for bad embedded spaces
			   because of performance reasons */
			continue;
		default:
			if (*p == dec_pt) {
				if (decimal_seen) {
					exception = 1;
				}
				decimal_seen = 1;
			} else
			if (*p == num_sep && type == NUMVAL_C) {
				/* note: we don't check for bad numeric seperator places
				   because of performance reasons */
			} else {
				/* must be invalid data, set exception and go on */
				exception = 1;
			}
			continue;
		}
	}

game_over:

	if (!digits) {
		/* srcfield is an empty / all zero string */
		mpz_set_ui (d1.value, 0UL);
	} else {
		mpz_set_str (d1.value, (char *)final_buff, 10);
		if (sign == -1) {
			mpz_neg (d1.value, d1.value);
		}
	}

	cob_free (final_buff);

	if (exception) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
	}

	d1.scale = decimal_digits;
	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);

	return curr_field;
}

/* Numeric functions */

static void
get_min_and_max_of_args (const int num_args, va_list args, cob_field **min, cob_field **max)
{
	int		i;
	cob_field	*f;

	*min = va_arg (args, cob_field *);
	*max = *min;

	for (i = 1; i < num_args; ++i) {
		f = va_arg (args, cob_field *);
		if (cob_cmp (f, *min) < 0) {
			*min = f;
		}
		if (cob_cmp (f, *max) > 0) {
			*max = f;
		}
	}
}

/* Uses d1 and d2. Return value in d1. */
static void
calc_mean_of_args (const int num_args, va_list args)
{
	int		i;
	cob_field	*f;

	mpz_set_ui (d1.value, 0UL);
	d1.scale = 0;

	for (i = 0; i < num_args; ++i) {
		f = va_arg (args, cob_field *);
		cob_decimal_set_field (&d2, f);
		cob_decimal_add (&d1, &d2);
	}

	mpz_set_ui (d2.value, (cob_uli_t)num_args);
	d2.scale = 0;
	cob_decimal_div (&d1, &d2);
}

/* Return variance in d1. Uses d2, d3 and d4. */
static void
calc_variance_of_args (const int n, va_list numbers, cob_decimal *mean)
{
	cob_field	*f;
	int		i;
	cob_decimal	*difference = &d2;
	cob_decimal	*sum = &d3;
	cob_decimal	*num_numbers = &d4;

	if (n == 1) {
		mpz_set_ui (d1.value, 0UL);
		d1.scale = 0;
		return;
	}

	mpz_set_ui (sum->value, 0UL);
	sum->scale = 0;

	/* Get the sum of the squares of the differences from the mean */
	/* i.e., Sum ((arg - mean)^2) */
	for (i = 0; i < n; ++i) {
		f = va_arg (numbers, cob_field *);

		cob_decimal_set_field (difference, f);
		cob_decimal_sub (difference, mean);
		cob_decimal_mul (difference, difference);
		cob_decimal_add (sum, difference);
	}

	/* Divide sum by n */
	mpz_set_ui (num_numbers->value, (cob_uli_t)n);
	num_numbers->scale = 0;
	cob_decimal_div (sum, num_numbers);

	mpz_set (d1.value, sum->value);
	d1.scale = sum->scale;
}

/* Date/time functions */

static void
get_interval_and_current_year_from_args (const int num_args, va_list args,
					 int * const interval, int * const current_year)
{
	cob_field	*f;
	time_t		t;
	struct tm	*timeptr;

	if (num_args > 1) {
		f = va_arg (args, cob_field *);
		*interval = cob_get_int (f);
	} else {
		*interval = 50;
	}

	if (num_args > 2) {
		f = va_arg (args, cob_field *);
		*current_year = cob_get_int (f);
	} else {
		t = time (NULL);
		timeptr = localtime (&t);
		*current_year = 1900 + timeptr->tm_year;
	}
}

/* Locale time */

#if defined(_WIN32) || defined(__CYGWIN__) || defined (HAVE_LANGINFO_CODESET)
#ifdef HAVE_LANGINFO_CODESET
static int
locale_time (const int hours, const int minutes, const int seconds,
	     cob_field *locale_field, char *buff)
{
	int 	deflocale = 0;
	struct tm	tstruct;
	char		buff2[LOCTIME_BUFSIZE] =  { '\0' };

	/* Initialize tstruct to given time */
	memset ((void *)&tstruct, 0, sizeof(struct tm));
	tstruct.tm_hour = hours;
	tstruct.tm_min = minutes;
	tstruct.tm_sec = seconds;

	if (locale_field) {
		char		locale_buff[COB_MINI_BUFF];
		deflocale = cob_field_to_string (locale_field, locale_buff,
				COB_MINI_MAX, CCM_NONE);
		if (deflocale < 1) {
			return 1;
		}
		(void) setlocale (LC_TIME, locale_buff);
	}

	/* Get strftime format string for locale */
	memset (buff2, 0, LOCTIME_BUFSIZE);
	snprintf (buff2, LOCTIME_BUFSIZE - 1, "%s", nl_langinfo (T_FMT));

	/* Set locale if not done yet */
	if (deflocale) {
		(void) setlocale (LC_ALL, cobglobptr->cob_locale);
	}

	strftime (buff, LOCTIME_BUFSIZE, buff2, &tstruct);

	return 0;
}
#else
static int
locale_time (const int hours, const int minutes, const int seconds,
	     cob_field *locale_field, char *buff)
{
	size_t		len;
	LCID		localeid = LOCALE_USER_DEFAULT;
	SYSTEMTIME	syst;

	/* Initialize syst with given time */
	memset ((void *)&syst, 0, sizeof(syst));
	syst.wHour = (WORD)hours;
	syst.wMinute = (WORD)minutes;
	syst.wSecond = (WORD)seconds;

	/* Get specified locale */
	if (locale_field) {
		char		locale_buff[COB_MINI_BUFF];
		int flen = cob_field_to_string (locale_field, locale_buff,
					COB_MINI_MAX, CCM_NONE);
#if 0	/* re-null-terminate last char (first space/comma/...)
		   of the locale string
		   -> Simon: Why? We already have it rtrimmed */
		unsigned char	*p;
		for (p = (unsigned char *)locale_buff; *p; ++p) {
			if (isalnum((int)*p) || *p == '_') {
				continue;
			}
			break;
		}
		*p = 0;
#endif
		if (flen < 1) {
			return 1;
		}

		/* Find locale ID */
		for (len = 0; len < WINLOCSIZE; ++len) {
			if (!strcmp (locale_buff, wintable[len].winlocalename)) {
				localeid = wintable[len].winlocaleid;
				break;
			}
		}
		if (len == WINLOCSIZE) {
			return 1;
		}
	}

	/* Get locale time */
	if (!GetTimeFormat (localeid, LOCALE_NOUSEROVERRIDE, &syst, NULL, buff,
			    LOCTIME_BUFSIZE)) {
		return 1;
	}

	return 0;
}
#endif
#endif

/* offset and length are for reference modification */
static void
cob_alloc_set_field_str (const char *str, const int offset, const int length)
{
	const size_t	str_len = strlen (str);
	cob_field	field;

	COB_FIELD_INIT (str_len, NULL, &const_alpha_attr);
	make_field_entry (&field);
	memcpy (curr_field->data, str, str_len);

	if (unlikely (offset > 0)) {
		calc_ref_mod (curr_field, offset, length);
	}
}

static void
cob_alloc_set_field_spaces (const int n)
{
	cob_field	field;

	COB_FIELD_INIT (n, NULL, &const_alpha_attr);
	make_field_entry (&field);
	memset (curr_field->data, ' ', (size_t)n);
}

/* Date/time functions */

static int
days_in_year (const int year)
{
	return 365 + leap_year (year);
}

static COB_INLINE COB_A_INLINE int
in_range (const int min, const int max, const int val)
{
	return min <= val && val <= max;
}

static int
valid_integer_date (const int days)
{
	return in_range (1, 3067671, days);
}

static int
valid_year (const int year)
{
	return in_range (1601, 9999, year);
}

static int
valid_month (const int month)
{
	return in_range (1, 12, month);
}

static int
valid_day_of_year (const int year, const int day)
{
	return in_range (1, days_in_year (year), day);
}

static int
valid_day_of_month (const int year, const int month, const int day)
{
	if (leap_year (year)) {
		return in_range (1, leap_month_days[month], day);
	} else {
		return in_range (1, normal_month_days[month], day);
	}
}

static int
max_week (int year)
{
	int	first_day = integer_of_date (year, 1, 1);
	int	last_day = first_day + days_in_year (year) - 1;
	int	week;

	get_iso_week (last_day, &year, &week);
	return week;
}

/* 86400 = 60 * 60 * 24. We'll ignore leap seconds for now. */
#define SECONDS_IN_DAY 86400

static int
valid_time (const int seconds_from_midnight)
{
	return in_range (0, SECONDS_IN_DAY, seconds_from_midnight);
}

/* Uses d5. */
static int
valid_decimal_time (cob_decimal *seconds_from_midnight)
{
	cob_decimal	*seconds_in_day = &d5;
	mpz_set_ui (seconds_in_day->value, (unsigned long) SECONDS_IN_DAY);
	seconds_in_day->scale = 0;

	return cob_decimal_cmp (seconds_from_midnight, seconds_in_day) <= 0;
}

#undef SECONDS_IN_DAY

static int
valid_offset_time (const int offset)
{
	const int minutes_in_day = 1440; /* 60 * 24 */
	return abs (offset) < minutes_in_day;
}

/* calculate date from days since 1601 */
static void
date_of_integer (const int day_num, int *year, int *month, int *day)
{
	int days = day_num;
	int baseyear = 1601;
	int leapyear = 365;
	int i;

	while (days > leapyear) {
		days -= leapyear;
		++baseyear;
		leapyear = days_in_year (baseyear);
	}
	for (i = 0; i < 13; ++i) {
		if (leap_year (baseyear)) {
			if (i && days <= leap_days[i]) {
				days -= leap_days[i - 1];
				break;
			}
		} else {
			if (i && days <= normal_days[i]) {
				days -= normal_days[i - 1];
				break;
			}
		}
	}

	*year = baseyear;
	*month = i;
	*day = days;
}

/* set year and day-of-year from integer */
static void
day_of_integer (const int day_num, int *year, int *day)
{
	int leapyear = 365;
	int days = day_num;

	*year = 1601;

	while (days > leapyear) {
		days -= leapyear;
		++*year;
		leapyear = days_in_year (*year);
	}

	*day = days;
}

/* calculate number of days between 1601 and given year */
static cob_u32_t
days_up_to_year (const int year)
{
	cob_u32_t	totaldays = 0;
	int		baseyear = 1601;

	while (baseyear != year) {
		totaldays += days_in_year (baseyear);
		++baseyear;
	}
	return totaldays;
}

/* calculate number of days between 1601/01/01 and given date */
static cob_u32_t
integer_of_date (const int year, const int month, const int days)
{
	cob_u32_t	totaldays;

	totaldays = days_up_to_year (year);

	if (leap_year (year)) {
		totaldays += leap_days[month - 1];
	} else {
		totaldays += normal_days[month - 1];
	}
	totaldays += days;

	return totaldays;
}

/* calculate number of days between 1601/01/01 and given year + day-of-year */
static cob_u32_t
integer_of_day (const int year, const int days)
{
	cob_u32_t	totaldays;

	totaldays = days_up_to_year (year);
	totaldays += days;

	return totaldays;
}

enum formatted_time_extra {
	EXTRA_NONE = 0,
	EXTRA_Z,
	EXTRA_OFFSET_TIME
};

struct time_format {
	int with_colons;
	int decimal_places;
	enum formatted_time_extra extra;
};

/* Uses d2 */
static void
seconds_from_formatted_time (const struct time_format format, const char *str,
			     cob_decimal *seconds_decimal)
{
	const char	*scanf_str = format.with_colons ? "%2d:%2d:%2d" : "%2d%2d%2d";
	int		hours;
	int		minutes;
	int		seconds;
	int		total_seconds;
	int		offset;
	int		end_of_decimal;
	int		unscaled_fraction = 0;
	cob_decimal	*fractional_seconds = &d2;

	/* LCOV_EXCL_START */
	if (unlikely (!sscanf (str, scanf_str, &hours, &minutes, &seconds))) {
		cob_fatal_error (COB_FERROR_CODEGEN);
	}
	/* LCOV_EXCL_STOP */

	total_seconds = (hours * 60 * 60) + (minutes * 60) + seconds;

	if (format.decimal_places != 0) {
		offset = format.with_colons ? 9 : 7;
		end_of_decimal = offset + format.decimal_places;
		for (; offset != end_of_decimal; ++offset) {
			unscaled_fraction = unscaled_fraction * 10 + COB_D2I (str[offset]);
		}

		mpz_set_ui (fractional_seconds->value, unscaled_fraction);
		fractional_seconds->scale = format.decimal_places;

		mpz_set_ui (seconds_decimal->value, total_seconds);
		cob_decimal_add (seconds_decimal, fractional_seconds);
	} else {
		mpz_set_ui (seconds_decimal->value, total_seconds);
		seconds_decimal->scale = 0;
	}
}

static int
valid_day_and_format (const int day, const char *format)
{
	return valid_integer_date (day) && cob_valid_date_format (format);
}

static size_t
num_leading_nonspace (const char *str, const size_t str_len)
{
	size_t	i;

	for (i = 0; i < str_len && !isspace ((int) str[i]); ++i);
	return i;
}

static void
format_as_yyyymmdd (const int day_num, const int with_hyphen, char *buff)
{
	int		day_of_month;
	int		month;
	int		year;
	const char	*format_str;

	date_of_integer (day_num, &year, &month, &day_of_month);

	format_str = with_hyphen ? "%4.4d-%2.2d-%2.2d" : "%4.4d%2.2d%2.2d";
	sprintf (buff, format_str, year, month, day_of_month);
}

static void
format_as_yyyyddd (const int day_num, const int with_hyphen, char *buff)
{
	int		day_of_year;
	int		year;
	const char	*format_str;

	day_of_integer (day_num, &year, &day_of_year);

	format_str = with_hyphen ? "%4.4d-%3.3d" : "%4.4d%3.3d";
	sprintf (buff, format_str, year, day_of_year);
}

/* 0 = Monday, ..., 6 = Sunday */
static int
get_day_of_week (const int day_num)
{
	return (day_num - 1) % 7;
}

static int
get_iso_week_one (const int day_num, const int day_of_year)
{
	int jan_4 = day_num - day_of_year + 4;
	int day_of_week = get_day_of_week (jan_4);
	int first_monday = jan_4 - day_of_week;
	return first_monday;
}

/*
 * Derived from "Calculating the ISO week number for a date" by Julian M.
 * Bucknall (https://www.boyet.com/articles/publishedarticles/calculatingtheisoweeknumb.html).
 */
static void
get_iso_week (const int day_num, int *year, int *week)
{
	int day_of_year;
	int days_to_dec_29;
	int dec_29;
	int week_one;

	day_of_integer (day_num, year, &day_of_year);

	days_to_dec_29 = days_in_year (*year) - 2;
	dec_29 = day_num - day_of_year + days_to_dec_29;

	if (day_num >= dec_29) {
		/* If the day is (after) December 29, it may be in the first
		    week of the following year
		*/
		week_one = get_iso_week_one (day_num + days_in_year (*year), day_of_year);
		if (day_num < week_one) {
			week_one = get_iso_week_one (day_num, day_of_year);
		} else {
			++*year;
		}
	} else {
		week_one = get_iso_week_one (day_num, day_of_year);

		/* If the day is before December 29, it may be in the last week
		   of the previous year
		*/
		if (day_num < week_one) {
			--*year;
			week_one = get_iso_week_one (day_num - day_of_year,
						     days_in_year (*year));
		}
	}

	*week = (day_num - week_one) / 7 + 1;
}

static void
format_as_yyyywwwd (const int day_num, const int with_hyphen, char *buff)
{
	int		ignored_day_of_year;
	int		week;
	int		year;
	int		day_of_week;
	const char	*format_str;

	day_of_integer (day_num, &year, &ignored_day_of_year);
	get_iso_week (day_num, &year, &week);
	day_of_week = get_day_of_week (day_num);

	format_str = with_hyphen ? "%4.4d-W%2.2d-%1.1d" : "%4.4dW%2.2d%1.1d";
	sprintf (buff, format_str, year, week, day_of_week + 1);
}

enum days_format {
	DAYS_MMDD,
	DAYS_DDD,
	DAYS_WWWD
};

struct date_format {
	enum days_format days;
	int with_hyphens;
};

static struct date_format
parse_date_format_string (const char *format_str)
{
	struct date_format format;

	if (!strcmp (format_str, "YYYYMMDD") || !strcmp (format_str, "YYYY-MM-DD")) {
		format.days = DAYS_MMDD;
	} else if (!strcmp (format_str, "YYYYDDD") || !strcmp (format_str, "YYYY-DDD")) {
		format.days = DAYS_DDD;
	} else { /* YYYYWwwD or YYYY-Www-D */
		format.days = DAYS_WWWD;
	}

	format.with_hyphens = format_str[4] == '-';

	return format;
}

static void
format_date (const struct date_format format, const int days, char *buff)
{
	void	(*formatting_func) (int, int, char *);

	if (format.days == DAYS_MMDD) {
		formatting_func = &format_as_yyyymmdd;
	} else if (format.days == DAYS_DDD) {
		formatting_func = &format_as_yyyyddd;
	} else { /* DAYS_WWWD */
		formatting_func = &format_as_yyyywwwd;
	}
	(*formatting_func) (days, format.with_hyphens, buff);
}

/* Uses d5. */
static void
get_fractional_seconds (cob_field *time, cob_decimal *fraction)
{
	int		seconds;
	cob_decimal	*whole_seconds;


	seconds = cob_get_int (time);
	whole_seconds = &d5;
	mpz_set_ui (whole_seconds->value, (unsigned long) seconds);
	whole_seconds->scale = 0;

	cob_decimal_set_field (fraction, time);
	cob_decimal_sub (fraction, whole_seconds);
}

static unsigned int
decimal_places_for_seconds (const char *str, const unsigned int point_pos)
{
	unsigned int offset = point_pos;
	int decimal_places = 0;

	while (str[++offset] == 's') {
		++decimal_places;
	}

	return decimal_places;
}

static int
rest_is_z (const char *str)
{
	return !strcmp (str, "Z");
}

static int
rest_is_offset_format (const char *str, const int with_colon)
{
	if (with_colon) {
		return !strcmp (str, "+hh:mm");
	} else {
		return !strcmp (str, "+hhmm");
	}
}

/* CHECKME: Why not using GMP string functions here? */
static void
add_decimal_digits (int decimal_places, cob_decimal *second_fraction,
		    char *buff, ptrdiff_t *buff_pos)
{
	int	scale = second_fraction->scale;
	int	power_of_ten;
	unsigned int	fraction = mpz_get_ui (second_fraction->value);

	/* Add decimal point */
	buff[*buff_pos] = COB_MODULE_PTR->decimal_point;
	++*buff_pos;

	/* Append decimal digits from second_fraction from left to right */
	while (scale != 0 && decimal_places != 0) {
		--scale;
		power_of_ten = cob_s32_pow (10, scale);
		buff[*buff_pos] = (char) ('0' + (fraction / power_of_ten));

		fraction %= power_of_ten;
		++*buff_pos;
		--decimal_places;
	}

	/* Set remaining digits to zero */
	if (decimal_places != 0) {
		memset (buff + *buff_pos, (int)'0', decimal_places);
		*buff_pos += decimal_places;
	}
}

static void
add_z (const ptrdiff_t buff_pos, char *buff)
{
	buff[buff_pos] = 'Z';
}

static void
add_offset_time (const int with_colon, int const *offset_time,
		 const ptrdiff_t buff_pos, char *buff)
{
	int		hours;
	int		minutes;
	const char	*format_str;
	char	local_buff[13]; /* 13: make the compiler happy as "(un)signed short" *could*
						           have more digits than we "assume" */

	if (offset_time) {
		hours = *offset_time / 60;
		minutes = abs (*offset_time) % 60;

		format_str = with_colon ? "%+2.2d:%2.2d" : "%+2.2d%2.2d";
		snprintf (local_buff, sizeof (local_buff), format_str,
			(cob_s16_t) hours,
			(cob_u16_t) minutes);
		memcpy (buff + buff_pos, local_buff, (size_t)6);
	} else {
		snprintf (buff + buff_pos, (size_t)6, "00000");
	}
}

static struct time_format
parse_time_format_string (const char *str)
{
	struct time_format	format;
	unsigned int		offset;

	if (!strncmp (str, "hhmmss", 6)) {
		format.with_colons = 0;
		offset = 6;
	} else { /* "hh:mm:ss" */
		format.with_colons = 1;
		offset = 8;
	}

	if (str[offset] == '.' || str[offset] == ',') {
		format.decimal_places = decimal_places_for_seconds (str, offset);
		offset += format.decimal_places + 1;
	} else {
		format.decimal_places = 0;
	}

	if (strlen (str) > (size_t) offset) {
		if (rest_is_z (str + offset)) {
			format.extra = EXTRA_Z;
		} else { /* the rest is the offset time */
			format.extra = EXTRA_OFFSET_TIME;
		}
	} else {
		format.extra = EXTRA_NONE;
	}

	return format;
}

static int
format_time (const struct time_format format, int time,
	     cob_decimal *second_fraction, int *offset_time, char *buff)
{
	int		hours;
	int		minutes;
	int		seconds;
	int		date_overflow = 0;
	ptrdiff_t	buff_pos;
	const char	*format_str;

	if (format.with_colons) {
		format_str = "%2.2d:%2.2d:%2.2d";
		buff_pos = 8;
	} else {
		format_str = "%2.2d%2.2d%2.2d";
		buff_pos = 6;
	}

	/* Duplication! */
	hours = time / 3600;
	time %= 3600;
	minutes = time / 60;
	seconds = time % 60;

	if (format.extra == EXTRA_Z) {
		if (offset_time == NULL) {
			cob_set_exception (COB_EC_IMP_UTC_UNKNOWN);
			return 0;
		}

		hours -= *offset_time / 60;
		minutes -= *offset_time % 60;

		/* Handle minute and hour overflow */
		if (minutes >= 60) {
			minutes -= 60;
			++hours;
		} else if (minutes < 0) {
			minutes += 60;
			--hours;
		}

		if (hours >= 24) {
			hours -= 24;
			date_overflow = 1;
		} else if (hours < 0) {
			hours += 24;
			date_overflow = -1;
		}
	}

	sprintf (buff, format_str, hours, minutes, seconds);

	if (format.decimal_places != 0) {
		add_decimal_digits (format.decimal_places, second_fraction,
				    buff, &buff_pos);
	}

	if (format.extra == EXTRA_Z) {
		add_z (buff_pos, buff);
	} else if (format.extra == EXTRA_OFFSET_TIME) {
		add_offset_time (format.with_colons, offset_time, buff_pos, buff);
	}

	return date_overflow;
}

/*
  Copies as many character as possible from before the first space
  from f->data into out_str and add a null terminator to out_str.
*/
static void
copy_data_to_null_terminated_str (cob_field *f, char * const out_str,
				  const size_t out_str_max)
{
	size_t	chars_before_space = num_leading_nonspace ((char *)f->data,
							   f->size);
	size_t	length = cob_min_int (chars_before_space, out_str_max);

	strncpy (out_str, (char *)f->data, length);
	out_str[length] = '\0';
}

static int
split_around_t (const char *str, char *first, char *second)
{
	int i, ret = 0;
	size_t first_length;
	size_t second_length;

	/* Find 'T' */
	for (i = 0; str[i] != '\0' && str[i] != 'T'; ++i);

	/* Copy everything before 'T' into first (if present) */
	if (i > COB_DATESTR_MAX) {
		first_length = COB_DATESTR_MAX;
		ret = COB_DATESTR_MAX + 1;
	} else {
		first_length = i;
	}
	if (first != NULL) {
		/* possible overflow checked above,
		   snprintf ensures terminated buffer */
		snprintf (first, first_length + 1, "%s", str);
		first[first_length] = 0;	/* win32 fun... */
	}

	/* If there is anything after 'T', copy it into second (if present) */
	if (second != NULL && str[i]) {
		str += i + 1;
		second_length = strlen (str);
		if (second_length == 0) {
			second[0] = '\0';
		} else {
			if (second_length > COB_TIMESTR_MAX) {
				second_length = COB_TIMESTR_MAX;
				ret = COB_TIMESTR_MAX + 1 + i;
			}
			snprintf (second, second_length + 1, "%s", str);
			second[second_length] = 0;	/* win32 fun... */
		}
	}
	return ret;
}

static int
try_get_valid_offset_time (cob_field *offset_time_field, int *offset_time)
{
	if (offset_time_field != NULL) {
		*offset_time = cob_get_int (offset_time_field);
		if (valid_offset_time (*offset_time)) {
			return 0;
		}
	} else {
		*offset_time = 0;
		return 0;
	}

	return 1;
}

static int *
get_system_offset_time_ptr (int * const offset_time)
{
	struct cob_time	current_time;

	current_time = cob_get_current_datetime (DTR_FULL);
	if (current_time.offset_known) {
		*offset_time = current_time.utc_offset;
		return offset_time;
	} else {
		return NULL;
	}
}

static int
test_char_cond (const int cond, int *offset)
{
	if (cond) {
		++(*offset);
		return 0;
	} else {
		return *offset + 1;
	}
}

static int
test_char (const char wanted, const char *str, int *offset)
{
	return test_char_cond (wanted == str[*offset], offset);
}

static COB_INLINE COB_A_INLINE int
test_char_in_range (const char min, const char max, const char ch, int *offset)
{
	return test_char_cond (min <= ch && ch <= max, offset);
}

static COB_INLINE COB_A_INLINE int
test_digit (const unsigned char ch, int *offset)
{
#if 0	/* note: as isdigit is locale-aware (slower and not what we want), we use the range instead */
	return test_char_cond (isdigit (ch), offset);
#else
	return test_char_in_range ('0', '9', ch, offset);
#endif
}

static int test_millenium (const char *date, int *offset, int *millenium)
{
	RETURN_IF_NOT_ZERO (test_char_in_range ('1', '9', date[*offset], offset));

	*millenium = COB_D2I (date[*offset - 1]);
	return 0;
}

static int
test_century (const char *date, int *offset, int *state)
{
	if (*state != 1) {
		RETURN_IF_NOT_ZERO (test_digit (date[*offset], offset));
	} else {
		RETURN_IF_NOT_ZERO (test_char_in_range ('6', '9', date[*offset],
							offset));
	}

	*state = *state * 10 + COB_D2I (date[*offset - 1]);
	return 0;
}

static int
test_decade (const char *date, int *offset, int *state)
{
	RETURN_IF_NOT_ZERO (test_digit (date[*offset], offset));
	*state = *state * 10 + COB_D2I (date[*offset - 1]);
	return 0;
}

static int
test_unit_year (const char *date, int *offset, int *state)
{
	if (*state != 160) {
		RETURN_IF_NOT_ZERO (test_digit (date[*offset], offset));
	} else {
		RETURN_IF_NOT_ZERO (test_char_in_range ('1', '9', date[*offset],
							offset));
	}

	*state = *state * 10 + COB_D2I (date[*offset - 1]);
	return 0;
}

static int
test_year (const char *date, int *offset, int *state)
{
	RETURN_IF_NOT_ZERO (test_millenium (date, offset, state));
	RETURN_IF_NOT_ZERO (test_century (date, offset, state));
	RETURN_IF_NOT_ZERO (test_decade (date, offset, state));
	RETURN_IF_NOT_ZERO (test_unit_year (date, offset, state));

	return 0;
}

static int
test_hyphen_presence (const int with_hyphens, const char *date, int *offset)
{
	return with_hyphens ? test_char ('-', date, offset) : 0;
}

static int
test_month (const char *date, int *offset, int *month)
{
	int	first_digit;

	/* Validate first digit */
	RETURN_IF_NOT_ZERO (test_char_cond (date[*offset] == '0' || date[*offset] == '1',
					    offset));
	first_digit = COB_D2I (date[*offset - 1]);

	/* Validate second digit */
	if (first_digit == 0) {
		RETURN_IF_NOT_ZERO (test_char_in_range ('1', '9', date[*offset],
							offset));
	} else { /* first digit == 1 */
		RETURN_IF_NOT_ZERO (test_char_in_range ('0', '2', date[*offset],
						    offset));
	}

	*month = first_digit * 10 + COB_D2I (date[*offset - 1]);
	return 0;
}

static int
test_day_of_month (const char *date, const int year, const int month,
		   int *offset)
{
	int	days_in_month;
	char	max_first_digit;
	char	max_second_digit;
	int	first_digit;

	if (leap_year (year)) {
		days_in_month = leap_month_days[month];
	} else {
		days_in_month = normal_month_days[month];
	}
	max_first_digit = '0' + (char) (days_in_month / 10);
	max_second_digit = '0' + (char) (days_in_month % 10);

	/* Validate first digit */
	RETURN_IF_NOT_ZERO (test_char_in_range ('0', max_first_digit,
						date[*offset], offset));
	first_digit = date[*offset - 1];

	/* Validate second digit */
	if (first_digit == '0') {
		RETURN_IF_NOT_ZERO (test_char_in_range ('1', '9', date[*offset],
							offset));
	} else if (first_digit != max_first_digit) {
		RETURN_IF_NOT_ZERO (test_digit (date[*offset], offset));
	} else {
		RETURN_IF_NOT_ZERO (test_char_in_range ('0', max_second_digit,
							date[*offset], offset));
	}

	return 0;
}

static int
test_day_of_year (const char *date, const int year, int *offset)
{
	char	max_last_digit;
	int	state;

	/* Validate first digit */
	/* Check day is not greater than 399 */
	RETURN_IF_NOT_ZERO (test_char_in_range ('0', '3', date[*offset], offset));
	state = COB_D2I (date[*offset - 1]);

	/* Validate second digit */
	if (state != 3) {
		RETURN_IF_NOT_ZERO (test_digit (date[*offset], offset));
	} else {
		/* Check day is not greater than 369 */
		RETURN_IF_NOT_ZERO (test_char_in_range ('0', '6', date[*offset],
							offset));
	}
	state = state * 10 + COB_D2I (date[*offset - 1]);

	/* Validate third digit */
	if (state == 0) {
		RETURN_IF_NOT_ZERO (test_char_in_range ('1', '9', date[*offset],
							offset));
	} else if (state != 36) {
		RETURN_IF_NOT_ZERO (test_digit (date[*offset], offset));
	} else {
		/* Check day is not greater than 366/365 */
		max_last_digit = leap_year (year) ? '6' : '5';
		RETURN_IF_NOT_ZERO (test_char_in_range ('0', max_last_digit,
							date[*offset], offset));
	}

	return 0;
}

static int
test_w_presence (const char *date, int *offset)
{
	return test_char ('W', date, offset);
}

static int
test_week (const char *date, const int year, int *offset)
{
	int	first_digit;
	char	max_last_digit;

	/* Validate first digit */
	RETURN_IF_NOT_ZERO (test_char_in_range ('0', '5', date[*offset], offset));
	first_digit = COB_D2I (date[*offset - 1]);

	/* Validate second digit */
	if (first_digit == 0) {
		RETURN_IF_NOT_ZERO (test_char_in_range ('1', '9', date[*offset],
							offset));
	} else if (first_digit != 5) {
		RETURN_IF_NOT_ZERO (test_digit (date[*offset], offset));
	} else {
		max_last_digit = max_week (year) == 53 ? '3' : '2';
		RETURN_IF_NOT_ZERO (test_char_in_range ('0', max_last_digit,
							date[*offset], offset));
	}

	return 0;
}

static int
test_day_of_week (const char *date, int *offset)
{
	RETURN_IF_NOT_ZERO (test_char_in_range ('1', '7', date[*offset], offset));
	return 0;
}

static int
test_date_end (const struct date_format format, const char *date, const int year, int *offset)
{
	int	month;

	if (format.days == DAYS_MMDD) {
		RETURN_IF_NOT_ZERO (test_month (date, offset, &month));
		RETURN_IF_NOT_ZERO (test_hyphen_presence (format.with_hyphens, date, offset));
		RETURN_IF_NOT_ZERO (test_day_of_month (date, year, month, offset));
	} else if (format.days == DAYS_DDD) {
		RETURN_IF_NOT_ZERO (test_day_of_year (date, year, offset));
	} else { /* DAYS_WWWD */
		RETURN_IF_NOT_ZERO (test_w_presence (date, offset));
		RETURN_IF_NOT_ZERO (test_week (date, year, offset));
		RETURN_IF_NOT_ZERO (test_hyphen_presence (format.with_hyphens, date, offset));
		RETURN_IF_NOT_ZERO (test_day_of_week (date, offset));
	}

	return 0;
}

static int
test_no_trailing_junk (const char *str, int offset, int end_of_string)
{
	if (end_of_string) {
		/* Allow trailing spaces at the end of strings */
		while (str[offset] != '\0') {
			if (str[offset] != ' ') {
				return offset + 1;
			}
			++offset;
		}
		return 0;
	} else {
		return str[offset] == '\0' ? 0 : offset + 1;
	}

}

static int
test_formatted_date (const struct date_format format, const char *date,
		     const int end_of_string)
{
	int	offset = 0;
	int	year;

	RETURN_IF_NOT_ZERO (test_year (date, &offset, &year));
	RETURN_IF_NOT_ZERO (test_hyphen_presence (format.with_hyphens, date, &offset));
	RETURN_IF_NOT_ZERO (test_date_end (format, date, year, &offset));
	RETURN_IF_NOT_ZERO (test_no_trailing_junk (date, offset, end_of_string));
	return 0;
}

static int
test_less_than_60 (const char *time, int *offset)
{
	RETURN_IF_NOT_ZERO (test_char_in_range ('0', '5', time[*offset], offset));
	RETURN_IF_NOT_ZERO (test_digit (time[*offset], offset));
	return 0;
}

static int
test_hour (const char *time, int *offset)
{
	int	first_digit;

	RETURN_IF_NOT_ZERO (test_char_in_range ('0', '2', time[*offset], offset));
	first_digit = COB_D2I (time[*offset - 1]);

	if (first_digit != 2) {
		RETURN_IF_NOT_ZERO (test_digit (time[*offset], offset));
	} else {
		RETURN_IF_NOT_ZERO (test_char_in_range ('0', '3', time[*offset], offset));
	}

	return 0;
}

static int
test_minute (const char *time, int *offset)
{
	RETURN_IF_NOT_ZERO (test_less_than_60 (time, offset));
	return 0;
}

static int
test_second (const char *time, int *offset)
{
	RETURN_IF_NOT_ZERO (test_less_than_60 (time, offset));
	return 0;
}

static int
test_colon_presence (const int with_colons, const char *time,
		     int *offset)
{
	if (with_colons) {
		RETURN_IF_NOT_ZERO (test_char (':', time, offset));
	}

	return 0;
}

static int
test_decimal_places (const int num_decimal_places, const char decimal_point,
		     const char *time, int *offset)
{
	int	i;

	if (num_decimal_places != 0) {
		RETURN_IF_NOT_ZERO (test_char (decimal_point, time, offset));
		for (i = 0; i < num_decimal_places; ++i) {
			RETURN_IF_NOT_ZERO (test_digit (time[*offset], offset));
		}
	}

	return 0;
}

static int
test_z_presence (const char *time, int *offset)
{
	return test_char ('Z', time, offset);
}

static int
test_two_zeroes (const char *str, int *offset)
{
	RETURN_IF_NOT_ZERO (test_char ('0', str, offset));
	RETURN_IF_NOT_ZERO (test_char ('0', str, offset));
	return 0;
}

static int
test_offset_time (const struct time_format format, const char *time, int *offset)
{
	if (time[*offset] == '+' || time[*offset] == '-') {
		++*offset;
		RETURN_IF_NOT_ZERO (test_hour (time, offset));
		RETURN_IF_NOT_ZERO (test_colon_presence (format.with_colons,
							 time, offset));
		RETURN_IF_NOT_ZERO (test_minute (time, offset));
	} else if (time[*offset] == '0') {
		++*offset;
		RETURN_IF_NOT_ZERO (test_two_zeroes (time, offset));
		RETURN_IF_NOT_ZERO (test_colon_presence (format.with_colons,
							 time, offset));
		RETURN_IF_NOT_ZERO (test_two_zeroes (time, offset));
	} else {
		return *offset + 1;
	}

	return 0;
}

static int
test_time_end (const struct time_format format, const char *time,
	       int *offset)
{
	if (format.extra == EXTRA_Z) {
		RETURN_IF_NOT_ZERO (test_z_presence (time, offset));
	} else if (format.extra == EXTRA_OFFSET_TIME) {
		RETURN_IF_NOT_ZERO (test_offset_time (format, time, offset));
	}

	return 0;
}

static int
test_formatted_time (const struct time_format format, const char *time,
		     const char decimal_point)
{
	int	offset = 0;

	RETURN_IF_NOT_ZERO (test_hour (time, &offset));
	RETURN_IF_NOT_ZERO (test_colon_presence (format.with_colons, time, &offset));
	RETURN_IF_NOT_ZERO (test_minute (time, &offset));
	RETURN_IF_NOT_ZERO (test_colon_presence (format.with_colons, time, &offset));
	RETURN_IF_NOT_ZERO (test_second (time, &offset));
	RETURN_IF_NOT_ZERO (test_decimal_places (format.decimal_places,
						 decimal_point, time, &offset));
	RETURN_IF_NOT_ZERO (test_time_end (format, time, &offset));
	RETURN_IF_NOT_ZERO (test_no_trailing_junk (time, offset, 1));

	return 0;
}

#undef RETURN_IF_NOT_ZERO

static cob_u32_t
integer_of_mmdd (const struct date_format format, const int year,
		 const char *final_part)
{
	const char	*scanf_str = format.with_hyphens ? "%2d-%2d" : "%2d%2d";
	int		month;
	int		day;

	/* LCOV_EXCL_START */
	if (unlikely (!sscanf (final_part, scanf_str, &month, &day))) {
		cob_fatal_error (COB_FERROR_CODEGEN);
	}
	/* LCOV_EXCL_STOP */
	return integer_of_date (year, month, day);

}

static cob_u32_t
integer_of_ddd (const int year, const char *final_part)
{
	int	day;

	/* LCOV_EXCL_START */
	if (unlikely (!sscanf (final_part, "%3d", &day))) {
		cob_fatal_error (COB_FERROR_CODEGEN);
	}
	/* LCOV_EXCL_STOP */
	return integer_of_day (year, day);
}

static cob_u32_t
integer_of_wwwd (const struct date_format format, const int year,
		 const char *final_part)
{
	int		first_week_monday;
	const char	*scanf_str = format.with_hyphens ? "W%2d-%1d" : "W%2d%1d";
	int		week;
	int		day_of_week;
	cob_u32_t	total_days = 0;

	first_week_monday = get_iso_week_one (days_up_to_year (year) + 1, 1);
	/* LCOV_EXCL_START */
	if (unlikely (!sscanf (final_part, scanf_str, &week, &day_of_week))) {
		cob_fatal_error (COB_FERROR_CODEGEN);
	}
	/* LCOV_EXCL_STOP */
	total_days = first_week_monday + ((week - 1) * 7) + day_of_week - 1;

	return total_days;
}

static cob_u32_t
integer_of_formatted_date (const struct date_format format,
			   const char *formatted_date)
{
	int		year;
	int		final_part_start = 4 + format.with_hyphens;

	/* LCOV_EXCL_START */
	if (unlikely (!sscanf (formatted_date, "%4d", &year))) {
		cob_fatal_error (COB_FERROR_CODEGEN);
	}
	/* LCOV_EXCL_STOP */

	if (format.days == DAYS_MMDD) {
		return integer_of_mmdd (format, year, formatted_date + final_part_start);
	} else if (format.days == DAYS_DDD) {
		return integer_of_ddd (year, formatted_date + final_part_start);
	} else { /* DAYS_WWWD */
		return integer_of_wwwd (format, year, formatted_date + final_part_start);
	}

}

static void
format_datetime (const struct date_format date_fmt,
		 const struct time_format time_fmt,
		 const int days,
		 const int whole_seconds,
		 cob_decimal *fractional_seconds,
		 int *offset_time,
		 char *buff)
{
	int	overflow;
	char	formatted_time[COB_TIMESTR_LEN] = { '\0' };
	char	formatted_date[COB_DATESTR_LEN] = { '\0' };

	overflow = format_time (time_fmt, whole_seconds, fractional_seconds,
				offset_time, formatted_time);
	format_date (date_fmt, days + overflow, formatted_date);

	sprintf (buff, "%sT%s", formatted_date, formatted_time);
}

/* Uses d1 */
static void
format_current_date (const struct date_format date_fmt,
		     const struct time_format time_fmt,
		     char *formatted_datetime)
{
	struct cob_time	time = cob_get_current_datetime (DTR_FULL);
	int		days
		= integer_of_date (time.year, time.month, time.day_of_month);
	int		seconds_from_midnight
		= time.hour * 60 * 60 + time.minute * 60 + time.second;
	cob_decimal	*fractional_second = &d1;
	int		*offset_time;

	mpz_set_ui (fractional_second->value, (unsigned long) time.nanosecond);
	fractional_second->scale = 9;

	if (time.offset_known) {
		offset_time = &time.utc_offset;
	} else {
		offset_time = NULL;
	}

	format_datetime (date_fmt, time_fmt, days, seconds_from_midnight,
			 fractional_second, offset_time, formatted_datetime);
}

static DECLNORET COB_A_NORETURN void
error_not_implemented (void)
{
	cob_set_exception (COB_EC_IMP_FEATURE_MISSING);
	cob_fatal_error (COB_FERROR_FUNCTION);
}

/* Global functions */

/* Return switch value as field */

cob_field *
cob_switch_value (const int id)
{
	cob_alloc_set_field_int (cob_get_switch (id));
	return curr_field;
}

/* Decimal exponentiation function */
/* x ^ z = e ^ (z * log(x)) */

void
cob_decimal_pow (cob_decimal *pd1, cob_decimal *pd2)
{
	cob_uli_t		n;
	const int		sign = mpz_sgn (pd1->value);

	if (unlikely (pd1->scale == COB_DECIMAL_NAN)) {
		return;
	}
	if (unlikely (pd2->scale == COB_DECIMAL_NAN)) {
		pd1->scale = COB_DECIMAL_NAN;
		return;
	}

	if (mpz_sgn (pd2->value) == 0) {
		/* Exponent is zero */
		if (sign == 0) {
			/* 0 ^ 0 */
			cob_set_exception (COB_EC_SIZE_EXPONENTIATION);
		}
		mpz_set_ui (pd1->value, 1UL);
		pd1->scale = 0;
		return;
	}
	if (sign == 0) {
		/* Value is zero */
		pd1->scale = 0;
		return;
	}

	cob_trim_decimal (pd2);

	if (sign == -1 && pd2->scale) {
		/* Negative exponent and non-integer power */
		pd1->scale = COB_DECIMAL_NAN;
		cob_set_exception (COB_EC_SIZE_EXPONENTIATION);
		return;
	}

	cob_trim_decimal (pd1);

	if (!pd2->scale) {
		/* Integer power */
		if (!mpz_cmp_ui (pd2->value, 1UL)) {
			/* Power is 1 */
			return;
		}
		if (mpz_sgn (pd2->value) == -1
		 && mpz_fits_slong_p (pd2->value)) {
			/* Negative power */
			mpz_abs (pd2->value, pd2->value);
			n = mpz_get_ui (pd2->value);
			mpz_pow_ui (pd1->value, pd1->value, n);
			if (pd1->scale) {
				pd1->scale *= n;
				cob_trim_decimal (pd1);
			}
			mpz_set (pd2->value, pd1->value);
			pd2->scale = pd1->scale;
			mpz_set_ui (pd1->value, 1UL),
			pd1->scale = 0;
			cob_decimal_div (pd1, pd2);
			cob_trim_decimal (pd1);
			return;
		}
		if (mpz_fits_ulong_p (pd2->value)) {
			/* Positive power */
			n = mpz_get_ui (pd2->value);
			mpz_pow_ui (pd1->value, pd1->value, n);
			if (pd1->scale) {
				pd1->scale *= n;
				cob_trim_decimal (pd1);
			}
			return;
		}
	}

	if (sign == -1) {
		mpz_abs (pd1->value, pd1->value);
	}
	cob_decimal_get_mpf (cob_mpft, pd1);
	if (pd2->scale == 1 && !mpz_cmp_ui (pd2->value, 5UL)) {
		/* Square root short cut */
		mpf_sqrt (cob_mpft2, cob_mpft);
	} else {
		cob_decimal_get_mpf (cob_mpft2, pd2);
		cob_mpf_log (cob_mpft, cob_mpft);
		mpf_mul (cob_mpft, cob_mpft, cob_mpft2);
		cob_mpf_exp (cob_mpft2, cob_mpft);
	}
	cob_decimal_set_mpf (pd1, cob_mpft2);
	if (sign == -1) {
		mpz_neg (pd1->value, pd1->value);
	}
}

/* Indirect field get/put functions */

void
cob_put_indirect_field (cob_field *f)
{
	make_field_entry (f);
	memcpy (curr_field->data, f->data, f->size);
	move_field = curr_field;
}

void
cob_get_indirect_field (cob_field *f)
{
	cob_move (move_field, f);
}

/* Indirect move */

void
cob_decimal_move_temp (cob_field *src, cob_field *dst)
{
	short		size, scale;
	cob_field_attr	attr;
	cob_field	field;

	cob_decimal_set_field (&d1, src);
	cob_trim_decimal (&d1);

	size = (short)mpz_sizeinbase (d1.value, 10);
	if (d1.scale > size) {
		size = (short)d1.scale;
	}
	scale = (short)d1.scale;
	COB_ATTR_INIT (COB_TYPE_NUMERIC_DISPLAY, size,
		       scale, COB_FLAG_HAVE_SIGN, NULL);
	COB_FIELD_INIT (size, NULL, &attr);
	make_field_entry (&field);
	(void)cob_decimal_get_field (&d1, curr_field, 0);
	cob_move (curr_field, dst);
}

/* TEST-NUMVAL + TEST-NUMVAL-C implementation */

/* Validate NUMVAL / NUMVAL-C item */
/* [spaces][+|-][spaces]{digits[.[digits]]|.digits}[spaces] */
/* [spaces]{digits[.[digits]]|.digits}[spaces][+|-|CR|DB][spaces] */
int
cob_check_numval (const cob_field *srcfield, const cob_field *currency,
		  const int chkcurr, const int anycase)
{
	unsigned char	*p;
	unsigned char	*begp;
	unsigned char	*endp;
	const size_t	max_pos = srcfield->size;
	size_t		pos;
	size_t		plus_minus;
	size_t		digits;
	size_t		decimal_seen;
	size_t		space_seen;
	size_t		break_needed;
	size_t		currcy_size;
	int		n;
	const unsigned char	dec_pt = COB_MODULE_PTR->decimal_point;
	unsigned char	cur_symb;

	/* variabe-length zero-size field -> error */
	if (!max_pos) {
		return 1;
	}

	/* FIXME later: srcfield may be of category national... */

	begp = NULL;
	currcy_size = 0;
	if (currency) {
		const size_t	currency_max_pos = currency->size;
		endp = NULL;
		p = currency->data;
		for (pos = 0; pos < currency_max_pos; pos++, p++) {
			switch (*p) {
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
			case '+':
			case '-':
			case '.':
			case ',':
			case '*':
				return 1;
			case ' ':
				break;
			default:
				if (pos < currency_max_pos - 1) {
					if (!memcmp (p, "CR", (size_t)2)) {
						return 1;
					}
					if (!memcmp (p, "DB", (size_t)2)) {
						return 1;
					}
				}
				if (!begp) {
					begp = p;
				}
				endp = p;
				break;
			}
		}
		if (!endp || !begp) {
			return 1;
		}
		currcy_size = endp - begp;
		currcy_size++;
		if (currcy_size >= max_pos) {
			begp = NULL;
			currcy_size = 0;
		}
	} else if (chkcurr) {
		cur_symb = COB_MODULE_PTR->currency_symbol;
		begp = &cur_symb;
		currcy_size = 1;
	}

	p = srcfield->data;
	plus_minus = 0;
	break_needed = 0;
	/* check leading positions */
	for (n = 0; n < (int)max_pos; ++n, ++p) {
		switch (*p) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			break_needed = 1;
			break;
		case ' ':
			continue;
		case '+':
		case '-':
			if (plus_minus) {
				return n + 1;
			}
			plus_minus = 1;
			continue;
		case ',':
		case '.':
			if (*p != dec_pt) {
				return n + 1;
			}
			break_needed = 1;
			break;
		default:
			if (begp && n < (int)(max_pos - currcy_size)) {
				if (!memcmp (p, begp, currcy_size)) {
					break;
				}
			}
			return n + 1;
		}
		if (break_needed) {
			break;
		}
	}

	/* end reached without digit -> definitely not numeric */
	if (n == (int)max_pos) {
		return max_pos + 1;
	}

	/* check actual data */
	break_needed = 0;
	digits = 0;
	decimal_seen = 0;
	space_seen = 0;

	for (; n < (int)max_pos; ++n, ++p) {
		switch (*p) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			if (++digits > COB_MAX_DIGITS || space_seen) {
				return n + 1;
			}
			continue;
		case ',':
		case '.':
			if (decimal_seen || space_seen) {
				return n + 1;
			}
			if (*p == dec_pt) {
				decimal_seen = 1;
			} else if (!chkcurr) {
				return n + 1;
			}
			if (digits) {
				/* digit seen: must be at previous position */
				const char prev = *(p - 1);
				if (prev < '0' || prev > '9') {
					return n + 1;
				}
				
			} else if (n < (int)max_pos - 1) {
				/* no digit seen so far: must be at next position */
				const char next = *(p + 1);
				if (next < '0' || next > '9') {
					return n + 2;
				}
			}
			continue;
		case ' ':
			space_seen = 1;
			continue;
		case '+':
		case '-':
			if (plus_minus) {
				return n + 1;
			}
			p++; n++;	/* trailing +/-, only space afterwards allowed */
			break_needed = 1;
			break;
		case 'c':
			if (!anycase) {
				return n + 1;
			}
			/* Fall through */
		case 'C':
			if (plus_minus) {
				return n + 1;
			}
			if (n < (int)max_pos - 1) {
				if (*(p + 1) == 'R' ||
				    (anycase && *(p + 1) == 'r')) {
					p++; n++;	/* trailing +/-, only space afterwards allowed */
					p++; n++;	/* skip cR */
					break_needed = 1;
					break;
				}
			}
			return n + 2;
		case 'd':
			if (!anycase) {
				return n + 1;
			}
			/* Fall through */
		case 'D':
			if (plus_minus) {
				return n + 1;
			}
			if (n < (int)max_pos - 1) {
				if (*(p + 1) == 'B' ||
				    (anycase && *(p + 1) == 'b')) {
					p++; n++;	/* trailing +/-, only space afterwards allowed */
					p++; n++;	/* skip dB */
					break_needed = 1;
					break;
				}
			}
			return n + 2;
		default:
			return n + 1;
		}
		if (break_needed) {
			break;
		}
	}

	/* no digit -> definitely not numeric */
	if (!digits) {
		return max_pos + 1;
	}

	/* check for trailing spaces only */
	for (; n < (int)max_pos; ++n, ++p) {
		if (*p != ' ') {
			return n + 1;
		}
	}

	return 0;
}

/* Date/time format validation */

int
cob_valid_date_format (const char *format)
{
	return !strcmp (format, "YYYYMMDD")
		|| !strcmp (format, "YYYY-MM-DD")
		|| !strcmp (format, "YYYYDDD")
		|| !strcmp (format, "YYYY-DDD")
		|| !strcmp (format, "YYYYWwwD")
		|| !strcmp (format, "YYYY-Www-D");
}

int
cob_valid_time_format (const char *format, const char decimal_point)
{
	int		with_colons;
	unsigned int	format_offset;
	unsigned int	decimal_places = 0;

	if (!strncmp (format, "hhmmss", 6)) {
		with_colons = 0;
		format_offset = 6;
	} else if (!strncmp (format, "hh:mm:ss", 8)) {
		with_colons = 1;
		format_offset = 8;
	} else {
		return 0;
	}

	/* Validate number of decimal places */
	if (format[format_offset] == decimal_point) {
		decimal_places = decimal_places_for_seconds (format, format_offset);
		format_offset += decimal_places + 1;
		if (decimal_places == 0
		 || decimal_places > COB_TIMEDEC_MAX) {
			return 0;
		}
	}

	/* Check for trailing garbage */
	if (strlen (format) > (size_t) format_offset
	 && !rest_is_z (format + format_offset)
	 && !rest_is_offset_format (format + format_offset, with_colons)) {
		return 0;
	}

	return 1;
}

int
cob_valid_datetime_format (const char *format, const char decimal_point)
{
	char	date_format_str[COB_DATETIMESTR_LEN] = { '\0' };
	char	time_format_str[COB_DATETIMESTR_LEN] = { '\0' };
	struct date_format	date_format;
	struct time_format	time_format;

	if (split_around_t (format, date_format_str, time_format_str)) {
		return 0;
	}

	if (!cob_valid_date_format (date_format_str)
	 || !cob_valid_time_format (time_format_str, decimal_point)) {
		return 0;
	}

	/* Check time and date formats match */
	date_format = parse_date_format_string (date_format_str);
	time_format = parse_time_format_string (time_format_str);
	if (date_format.with_hyphens != time_format.with_colons) {
		return 0;
	}

	return 1;
}

/* Numeric expressions */

cob_field *
cob_intr_binop (cob_field *f1, const int op, cob_field *f2)
{
	switch (op) {
	case 'a':
		cob_alloc_set_field_uint (cob_get_int (f1) & cob_get_int (f2));
		return curr_field;
	case 'o':
		cob_alloc_set_field_uint (cob_get_int (f1) | cob_get_int (f2));
		return curr_field;
	case 'e':
		cob_alloc_set_field_uint (cob_get_int (f1) ^ cob_get_int (f2));
		return curr_field;
	case 'l':
		cob_alloc_set_field_uint (cob_get_int (f1) << cob_get_int (f2));
		return curr_field;
	case 'r':
		cob_alloc_set_field_uint (cob_get_int (f1) >> cob_get_int (f2));
		return curr_field;
	case 'n':
		cob_alloc_set_field_uint ( ~ cob_get_int (f2));
		return curr_field;
	default:
		break;
	}

	cob_decimal_set_field (&d1, f1);
	cob_decimal_set_field (&d2, f2);
	switch (op) {
	case '+':
		cob_decimal_add (&d1, &d2);
		break;
	case '-':
		cob_decimal_sub (&d1, &d2);
		break;
	case '*':
		cob_decimal_mul (&d1, &d2);
		break;
	case '/':
		cobglobptr->cob_exception_code = 0;
		if (mpz_sgn (d2.value) == 0) {
			/* Divide by zero */
			cob_set_exception (COB_EC_SIZE_ZERO_DIVIDE);
			mpz_set_ui (d1.value, 0UL);
			d1.scale = 0;
		} else {
			cob_decimal_div (&d1, &d2);
		}
		break;
	case '^':
		cob_decimal_pow (&d1, &d2);
		break;
	default:
		break;
	}

	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

/* Intrinsics */

/* FUNCTION LENGTH - amount of positions */
cob_field *
cob_intr_length (cob_field *srcfield)
{
	if (COB_FIELD_IS_NATIONAL (srcfield)) {
		cob_alloc_set_field_uint ((cob_u32_t)srcfield->size / COB_NATIONAL_SIZE);
	} else {
		cob_alloc_set_field_uint ((cob_u32_t)srcfield->size);
	}
	return curr_field;
}


/* FUNCTION BYTE-LENGTH (or, as an extension: LENGTH-AN) - amount of bytes */
cob_field *
cob_intr_byte_length (cob_field *srcfield)
{
	cob_alloc_set_field_uint ((cob_u32_t)srcfield->size);
	return curr_field;
}

cob_field *
cob_intr_integer (cob_field *srcfield)
{
	cob_decimal_set_field (&d1, srcfield);
	/* Check scale */
	if (d1.scale < 0) {
		mpz_ui_pow_ui (cob_mexp, 10UL, (cob_uli_t)-d1.scale);
		mpz_mul (d1.value, d1.value, cob_mexp);
	} else if (d1.scale > 0) {
		const int	sign = mpz_sgn (d1.value);
		mpz_ui_pow_ui (cob_mexp, 10UL, (cob_uli_t)d1.scale);
		mpz_tdiv_qr (d1.value, cob_mpzt, d1.value, cob_mexp);
		/* Check negative and has decimal places */
		if (sign == -1 && mpz_sgn (cob_mpzt) != 0) {
			mpz_sub_ui (d1.value, d1.value, 1UL);
		}
	}
	d1.scale = 0;

	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_integer_part (cob_field *srcfield)
{
	cob_decimal_set_field (&d1, srcfield);
	/* Check scale */
	if (d1.scale < 0) {
		mpz_ui_pow_ui (cob_mexp, 10UL, (cob_uli_t)-d1.scale);
		mpz_mul (d1.value, d1.value, cob_mexp);
	} else if (d1.scale > 0) {
		mpz_ui_pow_ui (cob_mexp, 10UL, (cob_uli_t)d1.scale);
		mpz_tdiv_q (d1.value, d1.value, cob_mexp);
	}
	d1.scale = 0;

	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_fraction_part (cob_field *srcfield)
{
	cob_decimal_set_field (&d1, srcfield);
	/* Check scale */
	if (d1.scale > 0) {
		mpz_ui_pow_ui (cob_mexp, 10UL, (cob_uli_t)d1.scale);
		mpz_tdiv_r (d1.value, d1.value, cob_mexp);
	} else {
		/* No decimals */
		mpz_set_ui (d1.value, 0UL);
		d1.scale = 0;
	}

	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_sign (cob_field *srcfield)
{
	cob_decimal_set_field (&d1, srcfield);
	cob_alloc_set_field_int (mpz_sgn (d1.value));
	return curr_field;
}

cob_field *
cob_intr_upper_case (const int offset, const int length, cob_field *srcfield)
{
	size_t		i, size;

	make_field_entry (srcfield);

	size = srcfield->size;
	for (i = 0; i < size; ++i) {
		curr_field->data[i] = (cob_u8_t)toupper ((unsigned char)srcfield->data[i]);
	}
	if (unlikely (offset > 0)) {
		calc_ref_mod (curr_field, offset, length);
	}
	return curr_field;
}

cob_field *
cob_intr_lower_case (const int offset, const int length, cob_field *srcfield)
{
	size_t		i, size;

	make_field_entry (srcfield);

	size = srcfield->size;
	for (i = 0; i < size; ++i) {
		curr_field->data[i] = (cob_u8_t)tolower (srcfield->data[i]);
	}
	if (unlikely (offset > 0)) {
		calc_ref_mod (curr_field, offset, length);
	}
	return curr_field;
}

cob_field *
cob_intr_reverse (const int offset, const int length, cob_field *srcfield)
{
	size_t		i, size;

	make_field_entry (srcfield);

	size = srcfield->size;
	for (i = 0; i < size; ++i) {
		curr_field->data[i] = srcfield->data[size - i - 1];
	}
	if (unlikely (offset > 0)) {
		calc_ref_mod (curr_field, offset, length);
	}
	return curr_field;
}

cob_field *
cob_intr_bit_of (cob_field *srcfield)
{
	cob_field	field;
	/* FIXME later: srcfield may be of category national - or later bit... */
	const size_t		size = srcfield->size * 8;
	unsigned char		*byte = srcfield->data;
	size_t		i, j;

	COB_FIELD_INIT (size, NULL, &const_alpha_attr);
	make_field_entry (&field);

	for (i = j = 0; i < srcfield->size; ++i) {
		curr_field->data[j++] = *byte & 0x80 ? '1' : '0';
		curr_field->data[j++] = *byte & 0x40 ? '1' : '0';
		curr_field->data[j++] = *byte & 0x20 ? '1' : '0';
		curr_field->data[j++] = *byte & 0x10 ? '1' : '0';
		curr_field->data[j++] = *byte & 0x08 ? '1' : '0';
		curr_field->data[j++] = *byte & 0x04 ? '1' : '0';
		curr_field->data[j++] = *byte & 0x02 ? '1' : '0';
		curr_field->data[j++] = *byte & 0x01 ? '1' : '0';
		byte++;
	}
	return curr_field;
}

static int
has_bit_checked (const unsigned char byte) {
	if (byte == '0') return 0;
	if (byte != '1') {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
	}
	return 1;
}

cob_field *
cob_intr_bit_to_char (cob_field *srcfield)
{
	cob_field	field;
	const size_t		size = srcfield->size / 8;
	unsigned char		*byte_val, *char_val;
	size_t		i;

	COB_FIELD_INIT (size, NULL, &const_alpha_attr);
	make_field_entry (&field);

	byte_val = srcfield->data;
	char_val = curr_field->data;
	for (i = 0; i < size; i++) {
		*char_val = 0;
		if (has_bit_checked(*byte_val++)) *char_val |= 0x80;
		if (has_bit_checked(*byte_val++)) *char_val |= 0x40;
		if (has_bit_checked(*byte_val++)) *char_val |= 0x20;
		if (has_bit_checked(*byte_val++)) *char_val |= 0x10;
		if (has_bit_checked(*byte_val++)) *char_val |= 0x08;
		if (has_bit_checked(*byte_val++)) *char_val |= 0x04;
		if (has_bit_checked(*byte_val++)) *char_val |= 0x02;
		if (has_bit_checked(*byte_val++)) *char_val |= 0x01;
		char_val++;
	}
	return curr_field;
}

cob_field *
cob_intr_hex_of (cob_field *srcfield)
{
	const char hex_val[] = "0123456789ABCDEF";

	/* FIXME later: srcfield may be of category national - or later bit... */
	const size_t		size = srcfield->size * 2;
	cob_field	field;

	COB_FIELD_INIT (size, NULL, &const_alpha_attr);
	make_field_entry (&field);

	{
		register unsigned char *ret_pos = curr_field->data;
		register unsigned char *src_pos = srcfield->data;
		const unsigned char *src_end = src_pos + srcfield->size;

		while (src_pos < src_end) {
			*ret_pos++ = hex_val[(*src_pos >> 4) & 0xF];
			*ret_pos++ = hex_val[*src_pos++ & 0xF];
		}
	}
	return curr_field;
}

cob_field *
cob_intr_hex_to_char (cob_field *srcfield)
{
	cob_field	field;
	const size_t		size = srcfield->size / 2;
	const unsigned char *end = srcfield->data + size * 2;
	register unsigned char *hex_char, *p;

	if (size * 2 != srcfield->size) {
		/* possibly raise nonfatal exception here -> we only process the valid ones */
	}

	COB_FIELD_INIT (size, NULL, &const_alpha_attr);
	make_field_entry (&field);

	hex_char = curr_field->data;

	p = srcfield->data;
	while (p < end) {
		unsigned char dst;
		if (*p >= '0' && *p <= '9') {
			dst = COB_D2I (*p);
		} else if (*p >= 'A' && *p <= 'F') {
			dst = *p - 'A' + 10;
		} else if (*p >= 'a' && *p <= 'f') {
			dst = *p - 'a' + 10;
		} else {
			dst = 0;
			cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		}
		p++;
		dst *= 16;
		if (*p >= '0' && *p <= '9') {
			dst = dst + COB_D2I (*p);
		} else if (*p >= 'A' && *p <= 'F') {
			dst = dst + *p - 'A' + 10;
		} else if (*p >= 'a' && *p <= 'f') {
			dst = dst + *p - 'a' + 10;
		} else {
			cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		}
		p++;
		*hex_char++ = dst;
	}
	return curr_field;
}

cob_field *
cob_intr_module_date (void)
{
	cob_field_attr	attr;
	cob_field	field;
	char		buff[16];

	COB_ATTR_INIT (COB_TYPE_NUMERIC_DISPLAY, 8, 0, 0, NULL);
	COB_FIELD_INIT (8, NULL, &attr);
	make_field_entry (&field);
	snprintf (buff, sizeof(buff), "%8.8u", COB_MODULE_PTR->module_date);
	memcpy (curr_field->data, buff, (size_t)8);
	return curr_field;
}

cob_field *
cob_intr_module_time (void)
{
	cob_field_attr	attr;
	cob_field	field;
	char		buff[8];

	COB_ATTR_INIT (COB_TYPE_NUMERIC_DISPLAY, 6, 0, 0, NULL);
	COB_FIELD_INIT (6, NULL, &attr);
	make_field_entry (&field);
	snprintf (buff, sizeof(buff), "%6.6u", COB_MODULE_PTR->module_time);
	memcpy (curr_field->data, buff, (size_t)6);
	return curr_field;
}

cob_field *
cob_intr_module_id (void)
{
	cob_alloc_set_field_str (COB_MODULE_PTR->module_name, 0, 0);
	return curr_field;
}

cob_field *
cob_intr_module_caller_id (void)
{
	if (!COB_MODULE_PTR->next) {
		cob_field	field;
		COB_FIELD_INIT (1, NULL, &const_alpha_attr);
		make_field_entry (&field);
		curr_field->size = 0;
		curr_field->data[0] = ' ';
		return curr_field;
	}
	cob_alloc_set_field_str (COB_MODULE_PTR->next->module_name, 0, 0);
	return curr_field;
}

cob_field *
cob_intr_module_formatted_date (void)
{
	cob_alloc_set_field_str (COB_MODULE_PTR->module_formatted_date, 0, 0);
	return curr_field;
}

cob_field *
cob_intr_module_source (void)
{
	cob_alloc_set_field_str (COB_MODULE_PTR->module_source, 0, 0);
	return curr_field;
}

cob_field *
cob_intr_module_path (void)
{
	if (!COB_MODULE_PTR->module_path
	 || !(*COB_MODULE_PTR->module_path)) {
		cob_field	field;
		COB_FIELD_INIT (1, NULL, &const_alpha_attr);
		make_field_entry (&field);
		curr_field->size = 0;
		curr_field->data[0] = ' ';
		return curr_field;
	}
	cob_alloc_set_field_str (*COB_MODULE_PTR->module_path, 0, 0);
	return curr_field;
}

cob_field *
cob_intr_concatenate (const int offset, const int length,
		      const int params, ...)
{
	cob_field	**f;
	unsigned char	*p;
	size_t		calcsize;
	int		i;
	cob_field	field;
	va_list		args;

	f = cob_malloc ((size_t)params * sizeof (cob_field *));

	va_start (args, params);

	/* Extract args / calculate size */
	calcsize = 0;
	for (i = 0; i < params; ++i) {
		f[i] = va_arg (args, cob_field *);
		calcsize += f[i]->size;
	}
	va_end (args);

	COB_FIELD_INIT (calcsize, NULL, &const_alpha_attr);
	make_field_entry (&field);

	p = curr_field->data;
	for (i = 0; i < params; ++i) {
		memcpy (p, f[i]->data, f[i]->size);
		p += f[i]->size;
	}

	if (unlikely (offset > 0)) {
		calc_ref_mod (curr_field, offset, length);
	}
	cob_free (f);
	return curr_field;
}

cob_field *
cob_intr_substitute (const int offset, const int length,
		     const int params, ...)
{
	cob_field	*ret;
	va_list		args;

	va_start (args, params);
	ret = substitute (offset, length, params, &memcmp, args);
	va_end (args);

	return ret;
}

cob_field *
cob_intr_substitute_case (const int offset, const int length,
			  const int params, ...)
{
	cob_field	*ret;
	va_list		args;

	va_start (args, params);
	ret = substitute (offset, length, params, &int_strncasecmp, args);
	va_end (args);

	return ret;
}

cob_field *
cob_intr_trim (const int offset, const int length,
		cob_field *srcfield, const int direction)
{
	unsigned char	*begin;
	unsigned char	*end;
	size_t		i;
	size_t		size;

	make_field_entry (srcfield);

	for (i = 0; i < srcfield->size; ++i) {
		if (srcfield->data[i] != ' ') {
			break;
		}
	}
	if (i == srcfield->size) {
		curr_field->size = 0;
		curr_field->data[0] = ' ';
		return curr_field;
	}

	begin = srcfield->data;
	if (direction != 2) {
		for (; *begin == ' '; ++begin) ;
	}
	end = srcfield->data + srcfield->size - 1;
	if (direction != 1) {
		for (; *end == ' '; end--) ;
	}

	size = 0;
	for (i = 0; begin <= end; ++begin, ++i) {
		curr_field->data[i] = *begin;
		++size;
	}
	curr_field->size = size;
	if (unlikely (offset > 0)) {
		calc_ref_mod (curr_field, offset, length);
	}
	return curr_field;
}

/* get variable length (at least 2) temporary field containing last file exception status + name */
cob_field *
cob_intr_exception_file (void)
{
	size_t		flen;
	cob_field	field;

	COB_FIELD_INIT (0, NULL, &const_alpha_attr);
	/* check if last-exception is active and a file-exception */
	if (!cobglobptr->cob_error_file ||
	    (!cob_last_exception_is (COB_EC_I_O))) {
		field.size = 2;
		make_field_entry (&field);
		memcpy (curr_field->data, "00", (size_t)2);
	} else {
		flen = strlen (cobglobptr->cob_error_file->select_name);
		field.size = flen + 2;
		make_field_entry (&field);
		memcpy (curr_field->data,
			cobglobptr->cob_error_file->file_status, (size_t)2);
		memcpy (&(curr_field->data[2]),
			cobglobptr->cob_error_file->select_name, flen);
	}
	return curr_field;
}

/* implementation of FUNCTION EXCEPTION-LOCATION
   get variable length (at least 1) temporary field containing last exception location */
cob_field *
cob_intr_exception_location (void)
{
	/* check if last-exception is active and if LOCATION is available */
	if (!cobglobptr->last_exception_id) {
		cob_field	field;
		COB_FIELD_INIT (0, NULL, &const_alpha_attr);
		field.size = 1;
		make_field_entry (&field);
		*(curr_field->data) = ' ';
	} else {
		char buff[COB_SMALL_BUFF];
		if (cobglobptr->last_exception_section
		 && cobglobptr->last_exception_paragraph) {
			snprintf (buff, (size_t)COB_SMALL_MAX, "%s; %s OF %s; %u",
				  cobglobptr->last_exception_id,
				  cobglobptr->last_exception_paragraph,
				  cobglobptr->last_exception_section,
				  cobglobptr->last_exception_line);
		} else if (cobglobptr->last_exception_section) {
			snprintf (buff, (size_t)COB_SMALL_MAX, "%s; %s; %u",
				  cobglobptr->last_exception_id,
				  cobglobptr->last_exception_section,
				  cobglobptr->last_exception_line);
		} else if (cobglobptr->last_exception_paragraph) {
			snprintf (buff, (size_t)COB_SMALL_MAX, "%s; %s; %u",
				  cobglobptr->last_exception_id,
				  cobglobptr->last_exception_paragraph,
				  cobglobptr->last_exception_line);
		} else {
			snprintf (buff, (size_t)COB_SMALL_MAX, "%s; ; %u",
				  cobglobptr->last_exception_id,
				  cobglobptr->last_exception_line);
		}
		buff[COB_SMALL_MAX] = 0; /* silence warnings */
		cob_alloc_set_field_str (buff, 0, 0);
	}
	return curr_field;
}

/* implementation of FUNCTION EXCEPTION-STATUS
   get X(31) temporary field containing last exception name */
cob_field *
cob_intr_exception_status (void)
{
	const char	*except_name;
	cob_field	field;

	COB_FIELD_INIT (31, NULL, &const_alpha_attr);
	make_field_entry (&field);

	memset (curr_field->data, ' ', (size_t)31);
	if (cob_get_last_exception_code() != 0) {
		except_name = cob_get_last_exception_name ();
		if (except_name == NULL) {
			except_name = "EXCEPTION-OBJECT";
		}
		memcpy (curr_field->data, except_name, strlen (except_name));
	}
	return curr_field;
}

/* implementation of FUNCTION EXCEPTION-STATEMENT
   get X(31) temporary field containing last exception statement */
cob_field *
cob_intr_exception_statement (void)
{
	size_t		flen;
	cob_field	field;

	COB_FIELD_INIT (31, NULL, &const_alpha_attr);
	make_field_entry (&field);

	memset (curr_field->data, ' ', (size_t)31);
	if (cobglobptr->last_exception_statement != STMT_UNKNOWN) {
		const char *statement = cob_statement_name[cobglobptr->last_exception_statement];
		flen = strlen (statement);
		if (flen > 31) {
			flen = 31;
		}
		memcpy (curr_field->data, statement, flen);
	}
	return curr_field;
}

/* implementation of FUNCTION WHEN-COMPILED */
cob_field *
cob_intr_when_compiled (const int offset, const int length, cob_field *f)
{
	make_field_entry (f);

	memcpy (curr_field->data, f->data, f->size);
	if (unlikely (offset > 0)) {
		calc_ref_mod (curr_field, offset, length);
	}
	return curr_field;
}

/* implementation of FUNCTION CURRENT-DATE,
   using the actual current date adjusted/replaced with COB_CURRENT_DATE */
cob_field *
cob_intr_current_date (const int offset, const int length)
{
	cob_field	field;
	struct cob_time time;
	char		buff[22] = { '\0' };

	COB_FIELD_INIT (21, NULL, &const_alpha_attr);
	make_field_entry (&field);

	if (offset == 1 && length <= 14) {
		time = cob_get_current_datetime (DTR_TIME_NO_NANO);
	} else {
		time = cob_get_current_datetime (DTR_FULL);
	}

	sprintf (buff, "%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d%2.2d",
		  time.year, time.month, time.day_of_month, time.hour,
		  time.minute, time.second, (int) time.nanosecond / 10000000);

	add_offset_time (0, &time.utc_offset, 16, buff);

	memcpy (curr_field->data, buff, (size_t)21);
	if (offset != 0) {
		calc_ref_mod (curr_field, offset, length);
	}
	return curr_field;
}

/* implementation of FUNCTION CHAR - character from ordinal
   FIXME: Should use the program's alphanumeric program collating sequence! */
cob_field *
cob_intr_char (cob_field *srcfield)
{
	int		i;
	cob_field	field;

	COB_FIELD_INIT (1, NULL, &const_alpha_attr);
	make_field_entry (&field);

	i = cob_get_int (srcfield);
	if (i < 1 || i > 256) {
		*curr_field->data = 0;
	} else {
		*curr_field->data = (unsigned char)i - 1;
	}
	return curr_field;
}

cob_field *
cob_intr_ord (cob_field *srcfield)
{
	cob_alloc_set_field_uint ((cob_u32_t)(*srcfield->data + 1U));
	return curr_field;
}

cob_field *
cob_intr_stored_char_length (cob_field *srcfield)
{
	unsigned char	*p;
	cob_u32_t	count;

	count = srcfield->size;
	p = srcfield->data + srcfield->size - 1;
	for (; count > 0; count--, p--) {
		if (*p != ' ') {
			break;
		}
	}

	cob_alloc_set_field_uint (count);
	return curr_field;
}

cob_field *
cob_intr_combined_datetime (cob_field *srcdays, cob_field *srctime)
{
	int		srdays;
	cob_decimal	*combined_datetime;
	cob_decimal	*srtime;
	cob_decimal	*hundred_thousand;

	cobglobptr->cob_exception_code = 0;

	/* Validate and extract the value of srcdays */
	srdays = cob_get_int (srcdays);
	if (!valid_integer_date (srdays)) {
		goto invalid_args;
	}
	combined_datetime = &d1;
	mpz_set_ui (combined_datetime->value, (unsigned long) srdays);
	combined_datetime->scale = 0;

	/* Extract and validate the value of srctime */
	srtime = &d2;
	cob_decimal_set_field (srtime, srctime);
	if (!valid_decimal_time (srtime)) {
		goto invalid_args;
	}

	/* Set a decimal to 100 000. */
	hundred_thousand = &d3;
	mpz_set_ui (hundred_thousand->value, 100000UL);
	hundred_thousand->scale = 0;

	/* Combined datetime = date + (time / 100 000) */
	cob_decimal_div (srtime,  hundred_thousand);
	cob_decimal_add (combined_datetime, srtime);

	cob_alloc_field (combined_datetime);
	(void) cob_decimal_get_field (combined_datetime, curr_field, 0);
	goto end_of_func;

 invalid_args:
	cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
	cob_alloc_set_field_uint (0);

 end_of_func:
	return curr_field;
}

cob_field *
cob_intr_date_of_integer (cob_field *srcdays)
{
	int		days;
	int		month;
	int		year;
	cob_field_attr	attr;
	cob_field	field;
	char		buff[16];

	COB_ATTR_INIT (COB_TYPE_NUMERIC_DISPLAY, 8, 0, 0, NULL);
	COB_FIELD_INIT (8, NULL, &attr);
	make_field_entry (&field);

	cobglobptr->cob_exception_code = 0;
	/* Base 1601-01-01 */
	days = cob_get_int (srcdays);
	if (!valid_integer_date (days)) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		memset (curr_field->data, (int)'0', (size_t)8);
		return curr_field;
	}

	date_of_integer (days, &year, &month, &days);

	snprintf (buff, (size_t)15, "%4.4d%2.2d%2.2d", year, month, days);
	memcpy (curr_field->data, buff, (size_t)8);
	return curr_field;
}

cob_field *
cob_intr_day_of_integer (cob_field *srcdays)
{
	int		days;
	int		baseyear;
	cob_field_attr	attr;
	cob_field	field;
	char		buff[13]; /* 13: make the compiler happy as "unsigned short" *could*
						         have more digits than we "assume" */

	COB_ATTR_INIT (COB_TYPE_NUMERIC_DISPLAY, 7, 0, 0, NULL);
	COB_FIELD_INIT (7, NULL, &attr);
	make_field_entry (&field);

	cobglobptr->cob_exception_code = 0;
	/* Base 1601-01-01 */
	days = cob_get_int (srcdays);
	if (!valid_integer_date (days)) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		memset (curr_field->data, (int)'0', (size_t)7);
		return curr_field;
	}

	day_of_integer (days, &baseyear, &days);
	snprintf (buff, sizeof (buff), "%4.4d%3.3d",
		(cob_u16_t) baseyear,
		(cob_u16_t) days);

	memcpy (curr_field->data, buff, (size_t)7);
	return curr_field;
}

cob_field *
cob_intr_integer_of_date (cob_field *srcfield)
{
	int		indate;
	int		days;
	int		month;
	int		year;

	cobglobptr->cob_exception_code = 0;
	/* Base 1601-01-01 */
	indate = cob_get_int (srcfield);
	year = indate / 10000;
	if (!valid_year (year)) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}
	indate %= 10000;
	month = indate / 100;
	if (!valid_month (month)) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}
	days = indate % 100;
	if (!valid_day_of_month (year, month, days)) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}

	cob_alloc_set_field_uint (integer_of_date (year, month, days));
	return curr_field;
}

cob_field *
cob_intr_integer_of_day (cob_field *srcfield)
{
	int		indate;
	int		days;
	int		year;

	cobglobptr->cob_exception_code = 0;
	/* Base 1601-01-01 */
	indate = cob_get_int (srcfield);
	year = indate / 1000;
	if (!valid_year (year)) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}
	days = indate % 1000;
	if (!valid_day_of_year (year, days)) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}

	cob_alloc_set_field_uint (integer_of_day (year, days));
	return curr_field;
}

cob_field *
cob_intr_test_date_yyyymmdd (cob_field *srcfield)
{
	int		indate;
	int		days;
	int		month;
	int		year;

	/* Base 1601-01-01 */
	indate = cob_get_int (srcfield);
	year = indate / 10000;
	if (!valid_year (year)) {
		cob_alloc_set_field_uint (1);
		return curr_field;
	}
	indate %= 10000;
	month = indate / 100;
	if (!valid_month (month)) {
		cob_alloc_set_field_uint (2);
		return curr_field;
	}
	days = indate % 100;
	if (!valid_day_of_month (year, month, days)) {
		cob_alloc_set_field_uint (3);
		return curr_field;
	}
	cob_alloc_set_field_uint (0);
	return curr_field;
}

cob_field *
cob_intr_test_day_yyyyddd (cob_field *srcfield)
{
	int		indate;
	int		days;
	int		year;

	/* Base 1601-01-01 */
	indate = cob_get_int (srcfield);
	year = indate / 1000;
	if (!valid_year (year)) {
		cob_alloc_set_field_uint (1);
		return curr_field;
	}
	days = indate % 1000;
	if (!valid_day_of_year (year, days)) {
		cob_alloc_set_field_uint (2);
		return curr_field;
	}
	cob_alloc_set_field_uint (0);
	return curr_field;
}

cob_field *
cob_intr_factorial (cob_field *srcfield)
{
	int		srcval;

	cobglobptr->cob_exception_code = 0;
	srcval = cob_get_int (srcfield);
	d1.scale = 0;
	if (srcval < 0) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	} else {
		mpz_fac_ui (d1.value, (cob_uli_t)srcval);
	}

	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_e (void)
{
	mpf_set_ui (cob_mpft, 1UL);
	cob_mpf_exp (cob_mpft, cob_mpft);
	cob_decimal_set_mpf (&d1, cob_mpft);
	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);

	return curr_field;
}

cob_field *
cob_intr_pi (void)
{
	if (!set_cob_pi) setup_cob_pi ();

	mpf_set (cob_mpft, cob_pi);
	cob_decimal_set_mpf (&d1, cob_mpft);
	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);

	return curr_field;
}

cob_field *
cob_intr_exp (cob_field *srcfield)
{
	cob_decimal_set_field (&d1, srcfield);

	cobglobptr->cob_exception_code = 0;

	if (mpz_sgn (d1.value) == 0) {
		/* Power is zero */
		cob_alloc_set_field_uint (1);
		return curr_field;
	}

	cob_decimal_get_mpf (cob_mpft, &d1);
	cob_mpf_exp (cob_mpft, cob_mpft);
	cob_decimal_set_mpf (&d1, cob_mpft);
	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);

	return curr_field;
}

cob_field *
cob_intr_exp10 (cob_field *srcfield)
{
	int		sign;	/* no const as we need the decimal set before */

	cob_decimal_set_field (&d1, srcfield);

	cobglobptr->cob_exception_code = 0;

	sign = mpz_sgn (d1.value);
	if (sign == 0) {
		/* Power is zero */
		cob_alloc_set_field_uint (1);
		return curr_field;
	}

	cob_trim_decimal (&d1);

	if (!d1.scale) {
		/* Integer positive/negative powers */
		if (sign == -1 && mpz_fits_sint_p (d1.value)) {
			mpz_abs (d1.value, d1.value);
			d1.scale = mpz_get_si (d1.value);
			mpz_set_ui (d1.value, 1UL);
			cob_alloc_field (&d1);
			(void)cob_decimal_get_field (&d1, curr_field, 0);
			return curr_field;
		}
		if (sign == 1 && mpz_fits_ulong_p (d1.value)) {
			mpz_ui_pow_ui (d1.value, 10UL, mpz_get_ui (d1.value));
			cob_alloc_field (&d1);
			(void)cob_decimal_get_field (&d1, curr_field, 0);
			return curr_field;
		}
	}

	mpz_set_ui (d2.value, 10UL);
	d2.scale = 0;
	cob_decimal_pow (&d2, &d1);
	cob_alloc_field (&d2);
	(void)cob_decimal_get_field (&d2, curr_field, 0);

	return curr_field;
}

cob_field *
cob_intr_log (cob_field *srcfield)
{
	cob_decimal_set_field (&d1, srcfield);

	cobglobptr->cob_exception_code = 0;
	if (mpz_sgn (d1.value) != 1) {
		/* value must be > 0 */
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}

	if (d1.scale) {
		cob_trim_decimal (&d1);
	}

	if (!d1.scale && !mpz_cmp_ui (d1.value, 1UL)) {
		/* Log (1) = 0 */
		cob_alloc_set_field_uint (0);
		return curr_field;
	}

	cob_decimal_get_mpf (cob_mpft, &d1);
	cob_mpf_log (cob_mpft, cob_mpft);
	cob_decimal_set_mpf (&d1, cob_mpft);
	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);

	return curr_field;
}

cob_field *
cob_intr_log10 (cob_field *srcfield)
{
	cob_decimal_set_field (&d1, srcfield);

	cobglobptr->cob_exception_code = 0;
	if (mpz_sgn (d1.value) != 1) {
		/* value must be > 0 */
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}

	if (d1.scale) {
		cob_trim_decimal (&d1);
	}

	if (!d1.scale && !mpz_cmp_ui (d1.value, 1UL)) {
		/* Log10 (1) = 0 */
		cob_alloc_set_field_uint (0);
		return curr_field;
	}

	cob_decimal_get_mpf (cob_mpft, &d1);
	cob_mpf_log10 (cob_mpft, cob_mpft);
	cob_decimal_set_mpf (&d1, cob_mpft);
	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);

	return curr_field;
}

cob_field *
cob_intr_abs (cob_field *srcfield)
{
	cob_decimal_set_field (&d1, srcfield);
	mpz_abs (d1.value, d1.value);

	make_field_entry (srcfield);
	(void)cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_acos (cob_field *srcfield)
{
	cob_decimal_set_field (&d1, srcfield);

	mpz_set (d4.value, d1.value);
	mpz_set (d5.value, d1.value);
	d4.scale = d1.scale;
	d5.scale = d1.scale;
	mpz_set_si (d2.value, -1L);
	d2.scale = 0;
	mpz_set_ui (d3.value, 1UL);
	d3.scale = 0;

	cobglobptr->cob_exception_code = 0;
	if (cob_decimal_cmp (&d4, &d2) < 0 || cob_decimal_cmp (&d5, &d3) > 0) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}

	cob_decimal_get_mpf (cob_mpft, &d1);
	cob_mpf_acos (cob_mpft, cob_mpft);
	cob_decimal_set_mpf (&d1, cob_mpft);
	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);

	return curr_field;
}

cob_field *
cob_intr_asin (cob_field *srcfield)
{
	cob_decimal_set_field (&d1, srcfield);

	mpz_set (d4.value, d1.value);
	mpz_set (d5.value, d1.value);
	d4.scale = d1.scale;
	d5.scale = d1.scale;
	mpz_set_si (d2.value, -1L);
	d2.scale = 0;
	mpz_set_ui (d3.value, 1UL);
	d3.scale = 0;

	cobglobptr->cob_exception_code = 0;
	if (cob_decimal_cmp (&d4, &d2) < 0
	 || cob_decimal_cmp (&d5, &d3) > 0) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}

	if (mpz_sgn (d1.value) == 0) {
		/* Asin (0) = 0 */
		cob_alloc_set_field_uint (0);
		return curr_field;
	}

	cob_decimal_get_mpf (cob_mpft, &d1);
	cob_mpf_asin (cob_mpft, cob_mpft);
	cob_decimal_set_mpf (&d1, cob_mpft);
	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);

	return curr_field;
}

cob_field *
cob_intr_atan (cob_field *srcfield)
{
	cob_decimal_set_field (&d1, srcfield);

	cobglobptr->cob_exception_code = 0;

	if (mpz_sgn (d1.value) == 0) {
		/* Atan (0) = 0 */
		cob_alloc_set_field_uint (0);
		return curr_field;
	}

	cob_decimal_get_mpf (cob_mpft, &d1);
	cob_mpf_atan (cob_mpft, cob_mpft);
	cob_decimal_set_mpf (&d1, cob_mpft);
	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);

	return curr_field;
}

cob_field *
cob_intr_cos (cob_field *srcfield)
{
	cob_decimal_set_field (&d1, srcfield);

	cobglobptr->cob_exception_code = 0;

	cob_decimal_get_mpf (cob_mpft, &d1);
	cob_mpf_cos (cob_mpft, cob_mpft);
	cob_decimal_set_mpf (&d1, cob_mpft);
	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);

	return curr_field;
}

cob_field *
cob_intr_sin (cob_field *srcfield)
{
	cob_decimal_set_field (&d1, srcfield);

	cobglobptr->cob_exception_code = 0;

	cob_decimal_get_mpf (cob_mpft, &d1);
	cob_mpf_sin (cob_mpft, cob_mpft);
	cob_decimal_set_mpf (&d1, cob_mpft);
	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);

	return curr_field;
}

cob_field *
cob_intr_tan (cob_field *srcfield)
{
	cob_decimal_set_field (&d1, srcfield);

	cobglobptr->cob_exception_code = 0;

	cob_decimal_get_mpf (cob_mpft, &d1);
	cob_mpf_tan (cob_mpft, cob_mpft);
	cob_decimal_set_mpf (&d1, cob_mpft);
	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);

	return curr_field;
}

cob_field *
cob_intr_sqrt (cob_field *srcfield)
{
	cob_decimal_set_field (&d1, srcfield);

	cobglobptr->cob_exception_code = 0;
	if (mpz_sgn (d1.value) == -1) {
		/* value must be >= 0 */
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}

	mpz_set_ui (d2.value, 5UL);
	d2.scale = 1;
	cob_trim_decimal (&d1);
	cob_decimal_pow (&d1, &d2);

	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);

	return curr_field;
}

cob_field *
cob_intr_numval (cob_field *srcfield)
{
	return numval (srcfield, NULL, NUMVAL);
}

cob_field *
cob_intr_numval_c (cob_field *srcfield, cob_field *currency)
{
	return numval (srcfield, currency, NUMVAL_C);
}

/* NUMVAL-F implementation */

cob_field *
cob_intr_numval_f (cob_field *srcfield)
{
	unsigned char	*final_buff;
	unsigned char	*p, *p_end;
	size_t		digits;
	size_t		decimal_digits;
	size_t		exponent;
	size_t		datasize;
	int		decimal_seen, e_seen, plus_minus, e_plus_minus, exception;
	const unsigned char	dec_pt = COB_MODULE_PTR->decimal_point;

	/* note: versions before 3.2 did a pre-validation here,
	         we now parse "as valid as possible" by default
	         (the testsuite checks both variants) */
#ifdef INVALID_NUMVAL_IS_ZERO 
	/* Validate source field */
	if (cob_check_numval_f (srcfield)) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}
#endif

	/* FIXME later: srcfield may be of category national... */

	/* get size along with first/last relevant position */
	datasize = calculate_start_end_for_numval (srcfield, &p, &p_end);

	/* no data -> zero */
	if (datasize == 0) {
		cob_alloc_set_field_uint (0);
		return curr_field;
	}
	/* not wasting buffer space (COBOL2022: 35/34 max)... */
	if (datasize > COB_MAX_DIGITS) {
		datasize = COB_MAX_DIGITS;
	}

	/* acquire temp buffer long enuogh */
	final_buff = cob_malloc (datasize + 1U);

	plus_minus = 0;
	digits = 0;
	decimal_digits = 0;
	decimal_seen = 0;
	e_seen = 0;
	exponent = 0;
	e_plus_minus = 0;
	exception = 0;

	for ( /* start value for p set above */; p <= p_end; ++p) {
		switch (*p) {
		case '0':
			if (digits == 0 && !decimal_seen && exponent == 0) {
				/* no data yet, so just skip */
				continue;
			}
			/* Fall through */
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			if (e_seen) {
				exponent *= 10;
				exponent += COB_D2I (*p);
			} else	{
				if (decimal_seen) {
					decimal_digits++;
				}
				final_buff[digits++] = *p;
				if (digits > COB_MAX_DIGITS) {
					exception = 1;
					goto game_over;
				}
			}
			continue;
		case '+':
			if (e_seen) {
				if (e_plus_minus) {
					exception = 1;
				} else {
					e_plus_minus = 1;
				}
			} else {
				if (plus_minus) {
					exception = 1;
				} else {
					plus_minus = 1;
				}
			}
			continue;
		case '-':
			if (e_seen) {
				if (e_plus_minus) {
					exception = 1;
				} else {
					e_plus_minus = -1;
				}
			} else {
				if (plus_minus) {
					exception = 1;
				} else {
					plus_minus = -1;
				}
			}
			continue;
		case 'e':
		case 'E':
			if (e_seen) {
				exception = 1;
			} else {
				if (digits == 0 && decimal_digits == 0) {
					exception = 1;
					goto game_over;
				}
				e_seen = 1;
			}
			continue;
		case ' ':
			/* note: we don't check for bad embedded spaces
			   because of performance reasons */
			continue;
		default:
			if (*p == dec_pt) {
				if (decimal_seen) {
					exception = 1;
				} else {
					decimal_seen = 1;
				}
			} else {
				/* must be invalid data, set exception and go on */
				exception = 1;
			}
			continue;
		}
	}

game_over:

	if (!digits) {
		/* srcfield is an empty / all zero string */
		final_buff[0] = '0';
	}

	mpz_set_str (d1.value, (char *)final_buff, 10);
	cob_free (final_buff);

	if (exponent > 9999) {
		exponent = 9999;
		exception = 1;
	}

	if (exception) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
	}

	if (mpz_sgn (d1.value) == 0) {
		/* Value is zero ; sign and exponent irrelevant */
		d1.scale = 0;
		cob_alloc_field (&d1);
		(void)cob_decimal_get_field (&d1, curr_field, 0);
		return curr_field;
	}
	if (plus_minus == -1) {
		mpz_neg (d1.value, d1.value);
	}
	if (exponent) {
		if (e_plus_minus == -1) {
			/* Negative exponent */
			d1.scale = decimal_digits + exponent;
		} else {
			/* Positive exponent */
			if (decimal_digits >= exponent) {
				d1.scale = decimal_digits - exponent;
			} else {
				exponent -= decimal_digits;
				mpz_ui_pow_ui (cob_mexp, 10UL,
					       (cob_uli_t)exponent);
				mpz_mul (d1.value, d1.value, cob_mexp);
				d1.scale = 0;
			}
		}
	} else {
		/* No exponent */
		d1.scale = decimal_digits;
	}

	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);

	return curr_field;
}

cob_field *
cob_intr_annuity (cob_field *srcfield1, cob_field *srcfield2)
{
	int		sign;	/* no const as we need the decimal set before */

	cob_decimal_set_field (&d1, srcfield1);
	cob_decimal_set_field (&d2, srcfield2);

	/* P1 >= 0, P2 > 0 and integer */
	sign = mpz_sgn (d1.value);
	if (sign < 0 || mpz_sgn (d2.value) <= 0 || d2.scale != 0) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}

	if (sign == 0) {
		mpz_set_ui (d1.value, 1UL);
		d1.scale = 0;
		cob_decimal_div (&d1, &d2);
		cob_alloc_field (&d1);
		(void)cob_decimal_get_field (&d1, curr_field, 0);
		return curr_field;
	}

	/* x = P1 / (1 - (1 + P1) ^ (-P2)) */
	mpz_neg (d2.value, d2.value);

	mpz_set (d3.value, d1.value);
	d3.scale = d1.scale;
	mpz_set_ui (d4.value, 1UL);
	d4.scale = 0;
	cob_decimal_add (&d3, &d4);
	cob_trim_decimal (&d3);
	cob_trim_decimal (&d2);
	cob_decimal_pow (&d3, &d2);
	mpz_set_ui (d4.value, 1UL);
	d4.scale = 0;
	cob_decimal_sub (&d4, &d3);
	cob_trim_decimal (&d4);
	cob_trim_decimal (&d1);
	cob_decimal_div (&d1, &d4);
	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_sum (const int params, ...)
{
	cob_field	*f;
	va_list		args;
	int		i;

	mpz_set_ui (d1.value, 0UL);
	d1.scale = 0;

	va_start (args, params);

	for (i = 0; i < params; ++i) {
		f = va_arg (args, cob_field *);
		cob_decimal_set_field (&d2, f);
		cob_decimal_add (&d1, &d2);
	}
	va_end (args);

	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_ord_min (const int params, ...)
{
	cob_field	*f;
	cob_field	*basef;
	int		i;
	cob_u32_t	ordmin;
	va_list		args;

	va_start (args, params);

	ordmin = 1;
	basef = va_arg (args, cob_field *);
	for (i = 1; i < params; ++i) {
		f = va_arg (args, cob_field *);
		if (cob_cmp (f, basef) < 0) {
			basef = f;
			ordmin = i + 1;
		}
	}
	va_end (args);

	cob_alloc_set_field_uint (ordmin);
	return curr_field;
}

cob_field *
cob_intr_ord_max (const int params, ...)
{
	cob_field	*f;
	cob_field	*basef;
	cob_u32_t	ordmax;
	int		i;
	va_list		args;

	va_start (args, params);

	ordmax = 1;
	basef = va_arg (args, cob_field *);
	for (i = 1; i < params; ++i) {
		f = va_arg (args, cob_field *);
		if (cob_cmp (f, basef) > 0) {
			basef = f;
			ordmax = i + 1;
		}
	}
	va_end (args);

	cob_alloc_set_field_uint (ordmax);
	return curr_field;
}

cob_field *
cob_intr_min (const int params, ...)
{
	cob_field	*f;
	cob_field	*basef;
	va_list		args;
	int		i;

	va_start (args, params);

	basef = va_arg (args, cob_field *);
	for (i = 1; i < params; ++i) {
		f = va_arg (args, cob_field *);
		if (cob_cmp (f, basef) < 0) {
			basef = f;
		}
	}
	va_end (args);

	make_field_entry (basef);
	memcpy (curr_field->data, basef->data, basef->size);
	return curr_field;
}

cob_field *
cob_intr_max (const int params, ...)
{
	cob_field	*f;
	cob_field	*basef;
	va_list		args;
	int		i;

	va_start (args, params);

	basef = va_arg (args, cob_field *);
	for (i = 1; i < params; ++i) {
		f = va_arg (args, cob_field *);
		if (cob_cmp (f, basef) > 0) {
			basef = f;
		}
	}
	va_end (args);

	make_field_entry (basef);
	memcpy (curr_field->data, basef->data, basef->size);
	return curr_field;
}

cob_field *
cob_intr_midrange (const int params, ...)
{
	cob_field	*basemin;
	cob_field	*basemax;
	va_list		args;

	va_start (args, params);
	get_min_and_max_of_args (params, args, &basemin, &basemax);
	va_end (args);

	/* Return (max + min) / 2 */
	cob_decimal_set_field (&d1, basemin);
	cob_decimal_set_field (&d2, basemax);
	cob_decimal_add (&d1, &d2);
	mpz_set_ui (d2.value, 2UL);
	d2.scale = 0;
	cob_decimal_div (&d1, &d2);

	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_median (const int params, ...)
{
	cob_field	*f;
	cob_field	**field_alloc;
	va_list		args;
	int		i;

	va_start (args, params);

	f = va_arg (args, cob_field *);
	if (params == 1) {
		va_end (args);
		make_field_entry (f);
		memcpy (curr_field->data, f->data, f->size);
		return curr_field;
	}

	field_alloc = cob_malloc ((size_t)params * sizeof (cob_field *));
	field_alloc[0] = f;

	for (i = 1; i < params; ++i) {
		field_alloc[i] = va_arg (args, cob_field *);
	}
	va_end (args);

	qsort (field_alloc, (size_t)params, (size_t)sizeof (cob_field *),
	       comp_field);

	i = params / 2;
	if (params % 2) {
		f = field_alloc[i];
		make_field_entry (f);
		memcpy (curr_field->data, f->data, f->size);
	} else {
		cob_decimal_set_field (&d1, field_alloc[i-1]);
		cob_decimal_set_field (&d2, field_alloc[i]);
		cob_decimal_add (&d1, &d2);
		mpz_set_ui (d2.value, 2UL);
		d2.scale = 0;
		cob_decimal_div (&d1, &d2);
		cob_alloc_field (&d1);
		(void)cob_decimal_get_field (&d1, curr_field, 0);
	}
	cob_free (field_alloc);
	return curr_field;
}

cob_field *
cob_intr_mean (const int params, ...)
{
	cob_field	*f;
	va_list		args;
	int		i;

	va_start (args, params);

	if (params == 1) {
		f = va_arg (args, cob_field *);
		va_end (args);
		make_field_entry (f);
		memcpy (curr_field->data, f->data, f->size);
		return curr_field;
	}

	mpz_set_ui (d1.value, 0UL);
	d1.scale = 0;

	for (i = 0; i < params; ++i) {
		f = va_arg (args, cob_field *);
		cob_decimal_set_field (&d2, f);
		cob_decimal_add (&d1, &d2);
	}
	va_end (args);

	mpz_set_ui (d2.value, (cob_uli_t)params);
	d2.scale = 0;
	cob_decimal_div (&d1, &d2);

	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);

	return curr_field;
}

cob_field *
cob_intr_mod (cob_field *srcfield1, cob_field *srcfield2)
{
	return cob_mod_or_rem (srcfield1, srcfield2, 0);
}

cob_field *
cob_intr_range (const int params, ...)
{
	cob_field	*basemin, *basemax;
	va_list		args;

	va_start (args, params);
	get_min_and_max_of_args (params, args, &basemin, &basemax);
	va_end (args);

	cob_decimal_set_field (&d1, basemax);
	cob_decimal_set_field (&d2, basemin);
	cob_decimal_sub (&d1, &d2);

	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_rem (cob_field *srcfield1, cob_field *srcfield2)
{
	return cob_mod_or_rem (srcfield1, srcfield2, 1);
}

cob_field *
cob_intr_random (const int params, ...)
{
	cob_field	*f;
	va_list		args;
	double		val;
#ifdef DISABLE_GMP_RANDOM
	unsigned int		seed = 0;
#else
	unsigned long		seed = 0;
#endif
	cob_field_attr	attr;
	cob_field	field;

	va_start (args, params);
	if (params) {
		cob_s64_t specified_seed;
		f = va_arg (args, cob_field *);
		specified_seed = cob_get_llint (f);
		if (specified_seed < 0) {
			cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		} else {
			seed = (unsigned long)specified_seed;
		}
		rand_needs_seeding++;
#ifdef DISABLE_GMP_RANDOM
	} else {
		rand_needs_seeding = 0;
#else
	} else if (rand_needs_seeding) {
		/* first invocation without explicit seed, use a random one;
		   note: we need an explicit integer cast to get around some warnings,
		   but then need a matching size to get around others...*/
 #ifdef COB_64_BIT_POINTER
		seed = get_seconds_past_midnight ()
		     * (((cob_s64_t)COB_MODULE_PTR) & 0xFFFFF);
 #else
		seed = get_seconds_past_midnight ()
			* (((cob_s32_t)COB_MODULE_PTR) & 0xFFFF);
 #endif
		rand_needs_seeding = 2;
#endif
	}
	va_end (args);


#ifdef DISABLE_GMP_RANDOM
	/* note: the following code is suboptimal in multiple places
	   but is "explicit legacy" so it isn't changed */
	if (rand_needs_seeding) {
 #ifdef	__CYGWIN__
		srandom ((unsigned int)seed);
 #else
		srand ((unsigned int)seed);
 #endif
		rand_needs_seeding = 0;
	}
	{
		int		randnum;
 #ifdef	__CYGWIN__
	randnum = (int)random ();
 #else
	randnum = rand ();
 #endif
	val = (double)randnum / (double)RAND_MAX;
		/* sole adjustment: otherwise breaks returned value rules */
		if (val >= 1) val = 0.099999999999999999;
	}

#else	/* DISABLE_GMP_RANDOM */

	if (rand_needs_seeding) {
		if (rand_needs_seeding > 1) {
			/* initialize state for a Mersenne Twister algorithm,
			   this algorithm is fast and has good randomness properties;
			   also initialize return value */
			gmp_randinit_mt (rand_state);
			mpf_init (rand_float);
		}
		gmp_randseed_ui (rand_state, seed);
		rand_needs_seeding = 0;
	}

	mpf_urandomb (rand_float, rand_state, 63);
	val = mpf_get_d (rand_float);
#endif

	COB_ATTR_INIT (COB_TYPE_NUMERIC_DOUBLE, 20, 9, COB_FLAG_HAVE_SIGN, NULL);
	COB_FIELD_INIT (sizeof (double), NULL, &attr);
	make_field_entry (&field);
	memcpy (curr_field->data, &val, sizeof(val));
	return curr_field;
}

#define GET_VARIANCE(num_args, args)				\
	do {							\
		/* Get mean in d1 */				\
		va_start (args, num_args);			\
		calc_mean_of_args (num_args, args);		\
		va_end (args);					\
								\
		mpz_set (d5.value, d1.value);	\
		d5.scale = d1.scale;			\
								\
		/* Get variance in d1 */			\
		va_start (args, num_args);			\
		calc_variance_of_args (num_args, args, &d5);	\
		va_end (args);					\
	} ONCE_COB

cob_field *
cob_intr_variance (const int num_args, ...)
{
	va_list	args;

	GET_VARIANCE (num_args, args);

	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_standard_deviation (const int num_args, ...)
{
	va_list		args;

	GET_VARIANCE (num_args, args);
	cob_trim_decimal (&d1);

	cobglobptr->cob_exception_code = 0;

	/* Take square root of variance */
	mpz_set_ui (d3.value, 5UL);
	d3.scale = 1;

	cob_decimal_pow (&d1, &d3);

	cob_alloc_field (&d1);
	(void)cob_decimal_get_field (&d1, curr_field, 0);
	return curr_field;
}

#undef GET_VARIANCE

cob_field *
cob_intr_present_value (const int params, ...)
{
	cob_field	*f;
	va_list		args;
	int		i;

	va_start (args, params);

	f = va_arg (args, cob_field *);

	cob_decimal_set_field (&d1, f);
	mpz_set_ui (d2.value, 1UL);
	d2.scale = 0;
	cob_decimal_add (&d1, &d2);

	mpz_set_ui (d4.value, 0UL);
	d4.scale = 0;

	for (i = 1; i < params; ++i) {
		f = va_arg (args, cob_field *);
		cob_decimal_set_field (&d2, f);
		mpz_set (d3.value, d1.value);
		d3.scale = d1.scale;
		if (i > 1) {
			mpz_pow_ui (d3.value, d3.value, (cob_uli_t)i);
			d3.scale *= i;
		}
		cob_decimal_div (&d2, &d3);
		cob_decimal_add (&d4, &d2);
	}
	va_end (args);

	cob_alloc_field (&d4);
	(void)cob_decimal_get_field (&d4, curr_field, 0);
	return curr_field;
}

cob_field *
cob_intr_year_to_yyyy (const int params, ...)
{
	cob_field	*f;
	struct tm	*timeptr;
	va_list		args;
	time_t		t;
	int		year;
	int		interval;
	int		current_year;
	int		maxyear;

	cobglobptr->cob_exception_code = 0;
	va_start (args, params);
	f = va_arg (args, cob_field *);
	year = cob_get_int (f);
	if (params > 1) {
		f = va_arg (args, cob_field *);
		interval = cob_get_int (f);
	} else {
		interval = 50;
	}
	if (params > 2) {
		f = va_arg (args, cob_field *);
		current_year = cob_get_int (f);
	} else {
		t = time (NULL);
		timeptr = localtime (&t);
		current_year = 1900 + timeptr->tm_year;
	}
	va_end (args);

	if (year < 0 || year > 99) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}
	if (!valid_year (current_year)) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}
	maxyear = current_year + interval;
	if (maxyear < 1700 || maxyear > 9999) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}
	if (maxyear % 100 >= year) {
		year += 100 * (maxyear / 100);
	} else {
		year += 100 * ((maxyear / 100) - 1);
	}
	cob_alloc_set_field_int (year);
	return curr_field;
}

cob_field *
cob_intr_date_to_yyyymmdd (const int params, ...)
{
	cob_field	*f;
	va_list		args;
	int		year;
	int		mmdd;
	int		interval;
	int		current_year;
	int		maxyear;

	cobglobptr->cob_exception_code = 0;

	va_start (args, params);

	f = va_arg (args, cob_field *);
	year = cob_get_int (f);
	mmdd = year % 10000;
	year /= 10000;

	get_interval_and_current_year_from_args (params, args, &interval,
						 &current_year);

	va_end (args);

	maxyear = current_year + interval;
	/* The unusual year checks are as specified in the standard */
	if (year < 0 || year > 999999
	    || !valid_year (current_year)
	    || (maxyear < 1700 || maxyear > 9999)) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}

	if (maxyear % 100 >= year) {
		year += 100 * (maxyear / 100);
	} else {
		year += 100 * ((maxyear / 100) - 1);
	}
	year *= 10000;
	year += mmdd;
	cob_alloc_set_field_int (year);
	return curr_field;
}

cob_field *
cob_intr_day_to_yyyyddd (const int params, ...)
{
	cob_field	*f;
	va_list		args;
	int		year;
	int		days;
	int		interval;
	int		current_year;
	int		maxyear;

	cobglobptr->cob_exception_code = 0;

	va_start (args, params);

	f = va_arg (args, cob_field *);
	year = cob_get_int (f);
	days = year % 1000;
	year /= 1000;

	get_interval_and_current_year_from_args (params, args, &interval,
						 &current_year);

	va_end (args);

	if (year < 0 || year > 999999) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}
	if (!valid_year (current_year)) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}
	maxyear = current_year + interval;
	if (maxyear < 1700 || maxyear > 9999) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		return curr_field;
	}
	if (maxyear % 100 >= year) {
		year += 100 * (maxyear / 100);
	} else {
		year += 100 * ((maxyear / 100) - 1);
	}
	year *= 1000;
	year += days;
	cob_alloc_set_field_int (year);
	return curr_field;
}

static int
get_seconds_past_midnight (void)
{
	struct tm	*timeptr;
	time_t		t;
	int		seconds;

	t = time (NULL);
	timeptr = localtime (&t);
	/* Leap seconds ? */
	if (timeptr->tm_sec >= 60) {
		timeptr->tm_sec = 59;
	}
	seconds = (timeptr->tm_hour * 3600) + (timeptr->tm_min * 60) +
			timeptr->tm_sec;
	return seconds;
}

cob_field*
cob_intr_seconds_past_midnight (void)
{
	cob_alloc_set_field_int (get_seconds_past_midnight ());
	return curr_field;
}

cob_field *
cob_intr_seconds_from_formatted_time (cob_field *format_field, cob_field *time_field)
{
	size_t		str_length;
	char		format_str[2 * COB_DATETIMESTR_LEN] = { '\0' };
	char *		time_format_str = format_str;
	const char	decimal_point = COB_MODULE_PTR->decimal_point;
	int		is_datetime = 0;
	char		time_str[COB_DATETIMESTR_LEN] = { '\0' };
	struct time_format	time_fmt;
	cob_decimal	*seconds = &d1;

	str_length = num_leading_nonspace ((char *) format_field->data,
					   format_field->size);
	memcpy (format_str, format_field->data, str_length);

	cobglobptr->cob_exception_code = 0;

	/* Validate the format string */
	if (cob_valid_datetime_format (format_str, decimal_point)) {
		is_datetime = 1;
	} else if (!cob_valid_time_format (format_str, decimal_point)) {
		goto invalid_args;
	}

	/* Extract the time part of the strings */
	if (is_datetime) {
		time_format_str = format_str + sizeof(format_str) / 2;
		split_around_t (format_str, NULL, time_format_str);
		split_around_t ((char *) time_field->data, NULL, time_str);
	} else {
		memcpy (time_str, time_field->data, str_length);
	}

	/* Validate the formatted time */
	time_fmt = parse_time_format_string (time_format_str);
	if (test_formatted_time (time_fmt, time_str, decimal_point) != 0) {
		goto invalid_args;
	}

	seconds_from_formatted_time (time_fmt, time_str, seconds);

	cob_alloc_field (seconds);
	(void) cob_decimal_get_field (seconds, curr_field, 0);

	return curr_field;

 invalid_args:
	cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
	cob_alloc_set_field_uint (0);
	return curr_field;
}

cob_field *
cob_intr_locale_date (const int offset, const int length,
		      cob_field *srcfield, cob_field *locale_field)
{
#if defined(_WIN32) || defined(__CYGWIN__) || defined(HAVE_LANGINFO_CODESET)
	size_t		len;
	int		indate;
	int		days;
	int		month;
	int		year;
#ifdef	HAVE_LANGINFO_CODESET
	int 	deflocale = 0;
	struct tm	tstruct;
	char		buff2[128];
#else
	LCID		localeid = LOCALE_USER_DEFAULT;
	SYSTEMTIME	syst;
#endif
	char		buff[128];
	char		locale_buff[COB_MINI_BUFF];
#endif

	cobglobptr->cob_exception_code = 0;

#if defined(_WIN32) || defined(__CYGWIN__) || defined(HAVE_LANGINFO_CODESET)
	if (COB_FIELD_IS_NUMERIC (srcfield)) {
		indate = cob_get_int (srcfield);
	} else {
		unsigned char *p;
		if (srcfield->size < 8) {
			goto derror;
		}
		p = srcfield->data;
		indate = 0;
		for (len = 0; len < 8; ++len, ++p) {
			/* note: as isdigit is locale-aware (slower and not what we want),
			   we use a range check instead */
			if (*p >= '0' && *p <= '9') {
				indate = indate * 10 + COB_D2I (*p);
			} else {
				goto derror;
			}
		}
	}
	year = indate / 10000;
	if (!valid_year (year)) {
		goto derror;
	}
	indate %= 10000;
	month = indate / 100;
	if (!valid_month (month)) {
		goto derror;
	}
	days = indate % 100;
	if (!valid_day_of_month (year, month, days)) {
		goto derror;
	}
#ifdef	HAVE_LANGINFO_CODESET
	month--;

	memset ((void *)&tstruct, 0, sizeof(struct tm));
	tstruct.tm_year = year - 1900;
	tstruct.tm_mon = month;
	tstruct.tm_mday = days;
	if (locale_field) {
		deflocale = cob_field_to_string (locale_field, locale_buff,
				COB_MINI_MAX, CCM_NONE);
		if (deflocale < 1) {
			goto derror;
		}
		(void) setlocale (LC_TIME, locale_buff);
	}
	memset (buff2, 0, sizeof(buff2));
	snprintf(buff2, sizeof(buff2) - 1, "%s", nl_langinfo (D_FMT));
	if (deflocale) {
		(void) setlocale (LC_ALL, cobglobptr->cob_locale);
	}
	strftime (buff, sizeof(buff), buff2, &tstruct);
#else
	memset ((void *)&syst, 0, sizeof(syst));
	syst.wYear = (WORD)year;
	syst.wMonth = (WORD)month;
	syst.wDay = (WORD)days;
	if (locale_field) {
		int flen = cob_field_to_string (locale_field, locale_buff,
				     COB_MINI_MAX, CCM_NONE);
#if 0	/* re-null-terminate last char (first space/comma/...)
		   of the locale string
		   -> Simon: Why? We already have it rtrimmed */
		unsigned char *p;
		for (p = (unsigned char *)locale_buff; *p; ++p) {
			if (isalnum((int)*p) || *p == '_') {
				continue;
			}
			break;
		}
		*p = 0;
#endif
		if (flen < 1) {
			goto derror;
		}
		for (len = 0; len < WINLOCSIZE; ++len) {
			if (!strcmp (locale_buff, wintable[len].winlocalename)) {
				localeid = wintable[len].winlocaleid;
				break;
			}
		}
		if (len == WINLOCSIZE) {
			goto derror;
		}
	}
	if (!GetDateFormat (localeid, DATE_SHORTDATE, &syst, NULL, buff, sizeof(buff))) {
		goto derror;
	}
#endif
	cob_alloc_set_field_str (buff, offset, length);
	return curr_field;
derror:
#endif
	cob_alloc_set_field_spaces (10);
	cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
	return curr_field;
}

cob_field *
cob_intr_locale_time (const int offset, const int length,
		      cob_field *srcfield, cob_field *locale_field)
{
#if defined(_WIN32) || defined(__CYGWIN__) || defined(HAVE_LANGINFO_CODESET)
	unsigned char	*p;
	size_t		len;
	int		indate;
	int		hours;
	int		minutes;
	int		seconds;
	char		buff[LOCTIME_BUFSIZE] = { '\0' };
#endif

	cobglobptr->cob_exception_code = 0;

#if defined(_WIN32) || defined(__CYGWIN__) || defined(HAVE_LANGINFO_CODESET)
	if (COB_FIELD_IS_NUMERIC (srcfield)) {
		indate = cob_get_int (srcfield);
	} else {
		if (srcfield->size < 6) {
			goto derror;
		}
		p = srcfield->data;
		indate = 0;
		for (len = 0; len < 6; ++len, ++p) {
			/* note: as isdigit is locale-aware (slower and not what we want),
			   we use a range check instead */
			if (*p >= '0' && *p <= '9') {
				indate = indate * 10 + COB_D2I (*p);
			} else {
				goto derror;
			}
		}
	}
	hours = indate / 10000;
	if (hours < 0 || hours > 24) {
		goto derror;
	}
	indate %= 10000;
	minutes = indate / 100;
	if (minutes < 0 || minutes > 59) {
		goto derror;
	}
	seconds = indate % 100;
	if (seconds < 0 || seconds > 59) {
		goto derror;
	}

	if (locale_time (hours, minutes, seconds, locale_field, buff)) {
		goto derror;
	}

	cob_alloc_set_field_str (buff, offset, length);
	return curr_field;
derror:
#endif
	cob_alloc_set_field_spaces (10);
	cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
	return curr_field;
}

cob_field *
cob_intr_lcl_time_from_secs (const int offset, const int length,
			     cob_field *srcfield, cob_field *locale_field)
{
#if defined(_WIN32) || defined(__CYGWIN__) || defined(HAVE_LANGINFO_CODESET)
	int		indate;
	int		hours;
	int		minutes;
	int		seconds;
	char		buff[LOCTIME_BUFSIZE] = { '\0' };
#endif

	cobglobptr->cob_exception_code = 0;

#if defined(_WIN32) || defined(__CYGWIN__) || defined(HAVE_LANGINFO_CODESET)
	if (COB_FIELD_IS_NUMERIC (srcfield)) {
		indate = cob_get_int (srcfield);
	} else {
		goto derror;
	}
	if (!valid_time (indate)) {
		goto derror;
	}
	hours = indate / 3600;
	indate %= 3600;
	minutes = indate / 60;
	seconds = indate % 60;

	if (locale_time (hours, minutes, seconds, locale_field, buff)) {
		goto derror;
	}

	cob_alloc_set_field_str (buff, offset, length);
	return curr_field;
derror:
#endif
	cob_alloc_set_field_spaces (10);
	cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
	return curr_field;
}

cob_field *
cob_intr_mon_decimal_point (void)
{
#ifdef	HAVE_LOCALECONV
	struct lconv	*p;
	size_t		size;
#endif
	cob_field	field;

	COB_FIELD_INIT (0, NULL, &const_alpha_attr);
	cobglobptr->cob_exception_code = 0;

#ifdef	HAVE_LOCALECONV
	p = localeconv ();
	size = strlen (p->mon_decimal_point);
	if (size) {
		field.size = size;
	} else {
		field.size = 1;
	}
	make_field_entry (&field);
	if (size) {
		memcpy (curr_field->data, p->mon_decimal_point, size);
	} else {
		curr_field->size = 0;
		curr_field->data[0] = 0;
	}
#else
	field.size = 1;
	make_field_entry (&field);
	curr_field->data[0] = COB_MODULE_PTR->decimal_point;
#endif
	return curr_field;
}

cob_field *
cob_intr_num_decimal_point (void)
{
#ifdef	HAVE_LOCALECONV
	struct lconv	*p;
	size_t		size;
#endif
	cob_field	field;

	COB_FIELD_INIT (0, NULL, &const_alpha_attr);
	cobglobptr->cob_exception_code = 0;

#ifdef	HAVE_LOCALECONV
	p = localeconv ();
	size = strlen (p->decimal_point);
	if (size) {
		field.size = size;
	} else {
		field.size = 1;
	}
	make_field_entry (&field);
	if (size) {
		memcpy (curr_field->data, p->decimal_point, size);
	} else {
		curr_field->size = 0;
		curr_field->data[0] = 0;
	}
#else
	field.size = 1;
	make_field_entry (&field);
	curr_field->data[0] = COB_MODULE_PTR->decimal_point;
#endif
	return curr_field;
}

cob_field *
cob_intr_mon_thousands_sep (void)
{
#ifdef	HAVE_LOCALECONV
	struct lconv	*p;
	size_t		size;
#endif
	cob_field	field;

	COB_FIELD_INIT (0, NULL, &const_alpha_attr);
	cobglobptr->cob_exception_code = 0;

#ifdef	HAVE_LOCALECONV
	p = localeconv ();
	size = strlen (p->mon_thousands_sep);
	if (size) {
		field.size = size;
	} else {
		field.size = 1;
	}
	make_field_entry (&field);
	if (size) {
		memcpy (curr_field->data, p->mon_thousands_sep, size);
	} else {
		curr_field->size = 0;
		curr_field->data[0] = 0;
	}
#else
	field.size = 1;
	make_field_entry (&field);
	curr_field->data[0] = COB_MODULE_PTR->decimal_point;
#endif
	return curr_field;
}

cob_field *
cob_intr_num_thousands_sep (void)
{
#ifdef	HAVE_LOCALECONV
	struct lconv	*p;
	size_t		size;
#endif
	cob_field	field;

	COB_FIELD_INIT (0, NULL, &const_alpha_attr);
	cobglobptr->cob_exception_code = 0;

#ifdef	HAVE_LOCALECONV
	p = localeconv ();
	size = strlen (p->thousands_sep);
	if (size) {
		field.size = size;
	} else {
		field.size = 1;
	}
	make_field_entry (&field);
	if (size) {
		memcpy (curr_field->data, p->thousands_sep, size);
	} else {
		curr_field->size = 0;
		curr_field->data[0] = 0;
	}
#else
	field.size = 1;
	make_field_entry (&field);
	curr_field->data[0] = COB_MODULE_PTR->decimal_point;
#endif
	return curr_field;
}

cob_field *
cob_intr_currency_symbol (void)
{
#ifdef	HAVE_LOCALECONV
	struct lconv	*p;
	size_t		size;
#endif
	cob_field	field;

	COB_FIELD_INIT (0, NULL, &const_alpha_attr);
	cobglobptr->cob_exception_code = 0;

#ifdef	HAVE_LOCALECONV
	p = localeconv ();
	size = strlen (p->currency_symbol);
	if (size) {
		field.size = size;
	} else {
		field.size = 1;
	}
	make_field_entry (&field);
	if (size) {
		memcpy (curr_field->data, p->currency_symbol, size);
	} else {
		curr_field->size = 0;
		curr_field->data[0] = 0;
	}
#else
	field.size = 1;
	make_field_entry (&field);
	curr_field->data[0] = COB_MODULE_PTR->currency_symbol;
#endif
	return curr_field;
}

cob_field *
cob_intr_test_numval (cob_field *srcfield)
{
	cob_alloc_set_field_int (cob_check_numval (srcfield, NULL, 0, 0));
	return curr_field;
}

cob_field *
cob_intr_test_numval_c (cob_field *srcfield, cob_field *currency)
{
	cob_alloc_set_field_int (cob_check_numval (srcfield, currency, 1, 0));
	return curr_field;
}

cob_field *
cob_intr_test_numval_f (cob_field *srcfield)
{
	cob_alloc_set_field_int (cob_check_numval_f (srcfield));
	return curr_field;
}

cob_field *
cob_intr_lowest_algebraic (cob_field *srcfield)
{
	cob_uli_t	expo;
	cob_field	field;

	switch (COB_FIELD_TYPE (srcfield)) {
	case COB_TYPE_ALPHANUMERIC:
	case COB_TYPE_NATIONAL:
		COB_FIELD_INIT (COB_FIELD_SIZE (srcfield), NULL, &const_alpha_attr);
		make_field_entry (&field);
		break;

	case COB_TYPE_ALPHANUMERIC_EDITED:
	case COB_TYPE_NATIONAL_EDITED:
		COB_FIELD_INIT (COB_FIELD_DIGITS (srcfield), NULL, &const_alpha_attr);
		make_field_entry (&field);
		break;

	case COB_TYPE_NUMERIC_BINARY:
	case COB_TYPE_NUMERIC_COMP5:
		if (!COB_FIELD_HAVE_SIGN (srcfield)) {
			cob_alloc_set_field_uint (0);
			break;
		}
		if (COB_FIELD_REAL_BINARY (srcfield) 
		|| !COB_FIELD_BINARY_TRUNC (srcfield)) {
			expo = (cob_uli_t)((COB_FIELD_SIZE (srcfield) * 8U) - 1U);
			mpz_ui_pow_ui (d1.value, 2UL, expo);
		} else {
			expo = (cob_uli_t)COB_FIELD_DIGITS (srcfield);
			mpz_ui_pow_ui (d1.value, 10UL, expo);
			mpz_sub_ui (d1.value, d1.value, 1UL);
		}
		mpz_neg (d1.value, d1.value);
		d1.scale = COB_FIELD_SCALE (srcfield);
		cob_alloc_field (&d1);
		(void)cob_decimal_get_field (&d1, curr_field, 0);
		break;

	case COB_TYPE_NUMERIC_FLOAT:
	case COB_TYPE_NUMERIC_DOUBLE:
	case COB_TYPE_NUMERIC_L_DOUBLE:
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		break;

	case COB_TYPE_NUMERIC_DISPLAY:
	case COB_TYPE_NUMERIC_PACKED:
	case COB_TYPE_NUMERIC_EDITED:
		if (!COB_FIELD_HAVE_SIGN (srcfield)) {
			cob_alloc_set_field_uint (0);
			break;
		}
		expo = (cob_uli_t)COB_FIELD_DIGITS (srcfield);
		mpz_ui_pow_ui (d1.value, 10UL, expo);
		mpz_sub_ui (d1.value, d1.value, 1UL);
		mpz_neg (d1.value, d1.value);
		d1.scale = COB_FIELD_SCALE (srcfield);
		cob_alloc_field (&d1);
		(void)cob_decimal_get_field (&d1, curr_field, 0);
		break;
	default:
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		break;
	}
	return curr_field;
}

cob_field *
cob_intr_highest_algebraic (cob_field *srcfield)
{
	cob_uli_t	expo;
	size_t		size;
	cob_field	field;

	switch (COB_FIELD_TYPE (srcfield)) {
	case COB_TYPE_ALPHANUMERIC:
	case COB_TYPE_NATIONAL:
		size = COB_FIELD_SIZE (srcfield);
		COB_FIELD_INIT (size, NULL, &const_alpha_attr);
		make_field_entry (&field);
		memset (curr_field->data, 255, size);
		break;

	case COB_TYPE_ALPHANUMERIC_EDITED:
	case COB_TYPE_NATIONAL_EDITED:
		size = COB_FIELD_DIGITS (srcfield);
		COB_FIELD_INIT (size, NULL, &const_alpha_attr);
		make_field_entry (&field);
		memset (curr_field->data, 255, size);
		break;

	case COB_TYPE_NUMERIC_BINARY:
	case COB_TYPE_NUMERIC_COMP5:
		if (COB_FIELD_REAL_BINARY (srcfield) 
		|| !COB_FIELD_BINARY_TRUNC (srcfield)) {
			expo = COB_FIELD_SIZE (srcfield) * 8U;
			if (COB_FIELD_HAVE_SIGN (srcfield)) {
				expo--;
			}
			mpz_ui_pow_ui (d1.value, 2UL, expo);
		} else {
			expo = (cob_uli_t)COB_FIELD_DIGITS (srcfield);
			mpz_ui_pow_ui (d1.value, 10UL, expo);
		}
		mpz_sub_ui (d1.value, d1.value, 1UL);
		d1.scale = COB_FIELD_SCALE (srcfield);
		cob_alloc_field (&d1);
		(void)cob_decimal_get_field (&d1, curr_field, 0);
		break;

	case COB_TYPE_NUMERIC_FLOAT:
	case COB_TYPE_NUMERIC_DOUBLE:
	case COB_TYPE_NUMERIC_L_DOUBLE:
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		break;

	case COB_TYPE_NUMERIC_DISPLAY:
	case COB_TYPE_NUMERIC_PACKED:
	case COB_TYPE_NUMERIC_EDITED:
		expo = (cob_uli_t)COB_FIELD_DIGITS (srcfield);
		mpz_ui_pow_ui (d1.value, 10UL, expo);
		mpz_sub_ui (d1.value, d1.value, 1UL);
		d1.scale = COB_FIELD_SCALE (srcfield);
		cob_alloc_field (&d1);
		(void)cob_decimal_get_field (&d1, curr_field, 0);
		break;
	default:
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		cob_alloc_set_field_uint (0);
		break;
	}
	return curr_field;
}

cob_field *
cob_intr_locale_compare (const int params, ...)
{
	cob_field	*f1;
	cob_field	*f2;
	cob_field	*locale_field;
#ifdef	HAVE_STRCOLL
	unsigned char	*p;
	unsigned char	*p1;
	unsigned char	*p2;
	int 		deflocale = 0;
	size_t		size;
	size_t		size2;
	int		ret;
#endif
	cob_field	field;
	va_list		args;

	cobglobptr->cob_exception_code = 0;
	va_start (args, params);
	f1 = va_arg (args, cob_field *);
	f2 = va_arg (args, cob_field *);
	if (params > 2) {
		locale_field = va_arg (args, cob_field *);
	} else {
		locale_field = NULL;
	}
	va_end (args);

	COB_FIELD_INIT (1, NULL, &const_alpha_attr);
	make_field_entry (&field);

#ifdef	HAVE_STRCOLL
	size = f1->size;
	size2 = size;
	for (p = f1->data + size - 1U; p != f1->data; --p) {
		if (*p != ' ') {
			break;
		}
		size2--;
	}
	p1 = cob_malloc (size2 + 1U);
	memcpy (p1, f1->data, size2);

	size = f2->size;
	size2 = size;
	for (p = f2->data + size - 1U; p != f2->data; --p) {
		if (*p != ' ') {
			break;
		}
		size2--;
	}
	p2 = cob_malloc (size2 + 1U);
	memcpy (p2, f2->data, size2);

	if (locale_field) {
		char		locale_buff[COB_MINI_BUFF];
		deflocale = cob_field_to_string (locale_field, locale_buff,
			COB_MINI_MAX, CCM_NONE);
		if (deflocale < 1) {
			goto derror;
		}
#ifdef	HAVE_SETLOCALE
		(void) setlocale (LC_COLLATE, locale_buff);
#else
		goto derror;
#endif
	}

	ret = strcoll ((char *)p1, (char *)p2);
	if (ret < 0) {
		curr_field->data[0] = '<';
	} else if (ret > 0) {
		curr_field->data[0] = '>';
	} else {
		curr_field->data[0] = '=';
	}
	cob_free (p1);
	cob_free (p2);

#ifdef	HAVE_SETLOCALE
	if (deflocale) {
		(void) setlocale (LC_ALL, cobglobptr->cob_locale);
	}
#endif

	return curr_field;

derror:
	cob_free (p1);
	cob_free (p2);
#endif
	curr_field->data[0] = ' ';
	cob_set_exception (COB_EC_ARGUMENT_FUNCTION);

	return curr_field;
}

cob_field *
cob_intr_formatted_date (const int offset, const int length,
			 cob_field *format_field, cob_field *days_field)
{
	cob_field	field;
	size_t		field_length;
	char		format_str[COB_DATESTR_LEN] = { '\0' };
	int		days;
	struct date_format	format;
	char		buff[COB_DATESTR_LEN] = { '\0' };

	copy_data_to_null_terminated_str (format_field, format_str,
					  COB_DATESTR_MAX);
	field_length = strlen (format_str);

	COB_FIELD_INIT (field_length, NULL, &const_alpha_attr);
	make_field_entry (&field);

	cobglobptr->cob_exception_code = 0;
	days = cob_get_int (days_field);

	if (!valid_day_and_format (days, format_str)) {
		goto invalid_args;
	}

	format = parse_date_format_string (format_str);
	format_date (format, days, buff);

	memcpy (curr_field->data, buff, field_length);
	goto end_of_func;

 invalid_args:
	cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
	memset (curr_field->data, ' ', strlen (format_str));

 end_of_func:
	if (unlikely (offset > 0)) {
		calc_ref_mod (curr_field, offset, length);
	}
	return curr_field;
}

cob_field *
cob_intr_formatted_time (const int offset, const int length,
			 const int params, ...)
{
	va_list		args;
	cob_field	*format_field;
	cob_field	*time_field;
	cob_field	*offset_time_field;
	cob_field	field;
	size_t		field_length;
	char		buff[COB_TIMESTR_LEN] = { '\0' };
	char		format_str[COB_TIMESTR_LEN] = { '\0' };
	int		whole_seconds;
	cob_decimal	*fractional_seconds;
	int		use_system_offset;
	int		offset_time;
	int		*offset_time_ptr;
	struct time_format	format;

	if (!(params == 3 || params == 4)) {
		COB_FIELD_INIT (0, NULL, &const_alpha_attr);
		make_field_entry (&field);
		goto invalid_args;
	}

	/* Get args */
	va_start (args, params);

	format_field = va_arg (args, cob_field *);
	time_field = va_arg (args, cob_field *);
	if (params == 4) {
		offset_time_field = va_arg (args, cob_field *);
	} else {
		offset_time_field = NULL;
	}
	use_system_offset = va_arg (args, int);

	va_end (args);

	/* Initialise buffers */
	copy_data_to_null_terminated_str (format_field, format_str,
					  COB_TIMESTR_MAX);
	field_length = strlen (format_str);

	COB_FIELD_INIT (field_length, NULL, &const_alpha_attr);
	make_field_entry (&field);

	cobglobptr->cob_exception_code = 0;

	/* Extract and validate the times and time format */

	whole_seconds = cob_get_int (time_field);
	if (!valid_time (whole_seconds)) {
		goto invalid_args;
	}

	fractional_seconds = &d2;
	get_fractional_seconds (time_field, fractional_seconds);

	if (!cob_valid_time_format (format_str, COB_MODULE_PTR->decimal_point)) {
		goto invalid_args;
	}
	format = parse_time_format_string (format_str);

	if (use_system_offset) {
		offset_time_ptr = get_system_offset_time_ptr (&offset_time);
	} else {
		if (try_get_valid_offset_time (offset_time_field,
					       &offset_time)) {
			goto invalid_args;
		} else {
			offset_time_ptr = &offset_time;
		}
	}

	format_time (format, whole_seconds, fractional_seconds, offset_time_ptr,
		     buff);

	memcpy (curr_field->data, buff, field_length);
	goto end_of_func;

 invalid_args:
	cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
	memset (curr_field->data, ' ', strlen (format_str));

 end_of_func:
	if (unlikely (offset > 0)) {
		calc_ref_mod (curr_field, offset, length);
	}
	return curr_field;
}

cob_field *
cob_intr_formatted_datetime (const int offset, const int length,
			     const int params, ...)
{
	va_list		args;
	cob_field	*fmt_field;
	cob_field	*days_field;
	cob_field	*time_field;
	cob_field	*offset_time_field;
	cob_field	field;
	size_t		field_length;
	char		fmt_str[COB_DATETIMESTR_LEN] = { '\0' };
	char		date_fmt_str[COB_DATESTR_LEN] = { '\0' };
	char		time_fmt_str[COB_TIMESTR_LEN] = { '\0' };
	struct date_format     date_fmt;
	struct time_format     time_fmt;
	int		days;
	int		whole_seconds;
	cob_decimal	*fractional_seconds;
	int		use_system_offset;
	int		offset_time;
	int		*offset_time_ptr;
	char		buff[COB_DATETIMESTR_LEN] = { '\0' };

	if (!(params == 4 || params == 5)) {
		COB_FIELD_INIT (0, NULL, &const_alpha_attr);
		make_field_entry (&field);
		goto invalid_args;
	}

	/* Get arguments */
	va_start (args, params);

	fmt_field = va_arg (args, cob_field *);
	days_field = va_arg (args, cob_field *);
	time_field = va_arg (args, cob_field *);
	if (params == 5) {
		offset_time_field = va_arg (args, cob_field *);
	} else {
		offset_time_field = NULL;
	}
	use_system_offset = va_arg (args, int);

	va_end (args);

	copy_data_to_null_terminated_str (fmt_field, fmt_str,
					  COB_DATETIMESTR_MAX);
	field_length = strlen (fmt_str);

	COB_FIELD_INIT (field_length, NULL, &const_alpha_attr);
	make_field_entry (&field);

	cobglobptr->cob_exception_code = 0;

	/* Validate the formats, dates and times */
	if (!cob_valid_datetime_format (fmt_str, COB_MODULE_PTR->decimal_point)) {
		goto invalid_args;
	}

	days = cob_get_int (days_field);
	whole_seconds = cob_get_int (time_field);

	if (!valid_integer_date (days) || !valid_time (whole_seconds)) {
		goto invalid_args;
	}

	if (split_around_t (fmt_str, date_fmt_str, time_fmt_str)) {
		goto invalid_args;
	}

	time_fmt = parse_time_format_string (time_fmt_str);
	if (use_system_offset) {
		offset_time_ptr = get_system_offset_time_ptr (&offset_time);
	} else {
		if (try_get_valid_offset_time (offset_time_field,
					       &offset_time)) {
			goto invalid_args;
		} else {
			offset_time_ptr = &offset_time;
		}
	}
	date_fmt = parse_date_format_string (date_fmt_str);

	/* Format */

	fractional_seconds = &d1;
	get_fractional_seconds (time_field, fractional_seconds);

	format_datetime (date_fmt, time_fmt, days, whole_seconds,
			 fractional_seconds, offset_time_ptr, buff);

	memcpy (curr_field->data, buff, (size_t) field_length);
	goto end_of_func;

 invalid_args:
	cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
	memset (curr_field->data, ' ', strlen (fmt_str));

 end_of_func:
	if (unlikely (offset > 0)) {
		calc_ref_mod (curr_field, offset, length);
	}
	return curr_field;
}

cob_field *
cob_intr_test_formatted_datetime (cob_field *format_field,
				  cob_field *datetime_field)
{
	char	datetime_format_str[COB_DATETIMESTR_LEN] = { '\0' };
	char	date_format_str[COB_DATESTR_LEN] = { '\0' };
	char	time_format_str[COB_TIMESTR_LEN] = { '\0' };
	int	date_present;
	int	time_present;
	char	formatted_datetime[COB_DATETIMESTR_LEN] = { '\0' };
	char	formatted_date[COB_DATESTR_LEN] = { '\0' };
	char	formatted_time[COB_TIMESTR_LEN] = { '\0' };
	int	time_part_offset;
	int	error_pos;

	cobglobptr->cob_exception_code = 0;

	/* Copy to null-terminated strings */
	copy_data_to_null_terminated_str (format_field, datetime_format_str,
					  COB_DATETIMESTR_MAX);
	copy_data_to_null_terminated_str (datetime_field, formatted_datetime,
					  COB_DATETIMESTR_MAX);

	/* Check whether date or time is present. */
	if (cob_valid_date_format (datetime_format_str)) {
		date_present = 1;
		time_present = 0;
	} else if (cob_valid_time_format (datetime_format_str,
				COB_MODULE_PTR->decimal_point)) {
		date_present = 0;
		time_present = 1;
	} else if (cob_valid_datetime_format (datetime_format_str,
				COB_MODULE_PTR->decimal_point)) {
		date_present = 1;
		time_present = 1;
	} else {
		goto invalid_args;
	}

	/* Move date/time to respective variables;
	   note: all fields and sizes were validated above */
	if (date_present && time_present) {
		split_around_t (datetime_format_str,
			date_format_str, time_format_str);
	} else if (date_present) {
		strcpy (date_format_str, datetime_format_str);
	} else { /* time_present */
		strcpy (time_format_str, datetime_format_str);
	}

	/* Move format fields respective variables;
	   note: all fields and sizes were validated during compile */
	if (date_present && time_present) {
		split_around_t (formatted_datetime, formatted_date, formatted_time);
	} else if (date_present) {
		strcpy (formatted_date, formatted_datetime);
	} else { /* time_present */
		strcpy (formatted_time, formatted_datetime);
	}

	/* Set time offset */
	if (date_present) {
		time_part_offset = (int)strlen (formatted_date) + 1;
	} else {
		time_part_offset = 0;
	}

	/* Parse and validate the formatted date/time */
	if (date_present) {
		error_pos = test_formatted_date (parse_date_format_string (date_format_str),
						 formatted_date, !time_present);
		if (error_pos != 0) {
			cob_alloc_set_field_uint (error_pos);
			goto end_of_func;
		}
	}
	if (date_present && time_present
	    && formatted_datetime[strlen (formatted_date)] != 'T') {
		cob_alloc_set_field_uint ((unsigned int)strlen (formatted_date) + 1U);
		goto end_of_func;
	}
	if (time_present) {
		error_pos = test_formatted_time (parse_time_format_string (time_format_str),
						 formatted_time, COB_MODULE_PTR->decimal_point);
		if (error_pos != 0) {
			cob_alloc_set_field_uint (time_part_offset + error_pos);
			goto end_of_func;
		}
	}

	cob_alloc_set_field_uint (0);
	goto end_of_func;

 invalid_args:
	cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
	cob_alloc_set_field_uint (0);

 end_of_func:
	return curr_field;
}

cob_field *
cob_intr_integer_of_formatted_date (cob_field *format_field,
				    cob_field *date_field)
{
	char	original_format_str[COB_DATETIMESTR_LEN] = { '\0' };
	char	original_date_str[COB_DATETIMESTR_LEN] = { '\0' };
	char	format_str[COB_DATESTR_LEN] = { '\0' };
	char	date_str[COB_DATESTR_LEN] = { '\0' };
	int	is_date;
	struct date_format date_fmt;

	cobglobptr->cob_exception_code = 0;

	copy_data_to_null_terminated_str (format_field, original_format_str,
					  COB_DATETIMESTR_MAX);
	copy_data_to_null_terminated_str (date_field, original_date_str,
					  COB_DATETIMESTR_MAX);

	/* Get date format string and parse it */
	is_date = cob_valid_date_format (original_format_str);
	if (is_date) {
		strcpy (format_str, original_format_str);
	} else if (cob_valid_datetime_format (original_format_str,
					      COB_MODULE_PTR->decimal_point)) { /* Datetime */
		split_around_t (original_format_str, format_str, NULL);
	} else { /* Invalid format string */
		goto invalid_args;
	}
	date_fmt = parse_date_format_string (format_str);

	/* Get formatted date and validate it */
	if (is_date) {
		strcpy (date_str, original_date_str);
	} else { /* Datetime */
		split_around_t (original_date_str, date_str, NULL);
	}
	if (test_formatted_date (date_fmt, date_str, 1) != 0) {
		goto invalid_args;
	}

	cob_alloc_set_field_uint (integer_of_formatted_date (date_fmt, date_str));
	goto end_of_func;

 invalid_args:
	cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
	cob_alloc_set_field_uint (0);

 end_of_func:
	return curr_field;
}

/* implementation of FUNCTION FORMATTED-CURRENT-DATE,
   using the actual current date adjusted/replaced with COB_CURRENT_DATE
   with a specified formatting */
cob_field *
cob_intr_formatted_current_date (const int offset, const int length,
				 cob_field *format_field)
{
	cob_field	field;
	char		format_str[COB_DATETIMESTR_LEN] = { '\0' };
	size_t		field_length;
	char		date_format_str[COB_DATESTR_LEN] = { '\0' };
	char		time_format_str[COB_TIMESTR_LEN] = { '\0' };
	struct date_format	date_fmt;
	struct time_format	time_fmt;
	char		formatted_date[COB_DATETIMESTR_LEN] = { '\0' };

	copy_data_to_null_terminated_str (format_field, format_str,
					  COB_DATETIMESTR_MAX);
	field_length = strlen (format_str);

	COB_FIELD_INIT (field_length, NULL, &const_alpha_attr);
	make_field_entry (&field);

	cobglobptr->cob_exception_code = 0;

	/* Validate format */
	if (!cob_valid_datetime_format (format_str, COB_MODULE_PTR->decimal_point)) {
		cob_set_exception (COB_EC_ARGUMENT_FUNCTION);
		memset (curr_field->data, ' ', field_length);
		goto end_of_func;
	}

	/* Parse format */
	split_around_t (format_str, date_format_str, time_format_str);
	date_fmt = parse_date_format_string (date_format_str);
	time_fmt = parse_time_format_string (time_format_str);

	/* Format current date */
	format_current_date (date_fmt, time_fmt, formatted_date);
	memcpy (curr_field->data, formatted_date, field_length);

 end_of_func:
	if (unlikely (offset > 0)) {
		calc_ref_mod (curr_field, offset, length);
	}
	return curr_field;
}

/**
  FUNCTION CONTENT-LENGTH(pointer).  NUMERIC.

  Return the nul byte terminated "string" length of data
  addressed by the given pointer.
**/
cob_field *
cob_intr_content_length (cob_field *srcfield)
{
	unsigned char	*pointed;
	cob_u32_t	val = 0;

	cob_set_exception (0);
	if (srcfield) {
		pointed = *((unsigned char **)srcfield->data);
	} else {
		pointed = NULL;
	}
	/* check if the pointer is set and does not point to NULL */
	if (pointed && *pointed) {
		val = (cob_u32_t)strlen ((char *)pointed);
	} else {
		cob_set_exception (COB_EC_DATA_PTR_NULL);
	}
	cob_alloc_set_field_uint (val);
	return curr_field;
}

/**
  FUNCTION CONTENT-OF (pointer, [len]). ALPHANUMERIC, ref-mod allowed.

  Retrieve the content of a pointer indirection.
  Either for given length, or if omitted or 0, by NUL terminator scan.
  If the source pointer is null, points to null or an empty string,
  return a zero length space.
**/
cob_field *
cob_intr_content_of (const int offset, const int length, const int params, ...)
{
	size_t          size = 0;
	unsigned char   *pointed;
	unsigned int    request_len;
	va_list         args;
	cob_field       field;
	cob_field       *srcfield;
	cob_field       *lenfield;

	cob_set_exception (0);

	va_start (args, params);
	srcfield = va_arg(args, cob_field *);
	if (params > 1) {
		lenfield = va_arg (args, cob_field *);
		request_len = cob_get_int (lenfield);
	} else {
		request_len = 0;
	}
	va_end (args);

	if (srcfield) {
		pointed = *((unsigned char **)srcfield->data);
	} else {
		pointed = NULL;
	}
	/* check if the pointer is set and does not point to NULL */
	if (pointed && *pointed) {
		/* Fixed length (may include NUL) or C NUL terminated string */
		if (request_len != 0) {
			size = request_len;
		} else {
			size = strlen ((char *)pointed);
		}
		if (size > COB_MAX_UNBOUNDED_SIZE) {
			cob_set_exception (COB_EC_SIZE_TRUNCATION);
			size = COB_MAX_UNBOUNDED_SIZE;
		}
	} else {
		cob_set_exception (COB_EC_DATA_PTR_NULL);
		size = 0;
	}
	if (size != 0) {
		COB_FIELD_INIT (size, NULL, &const_alpha_attr);
		make_field_entry (&field);
		/* Testing for memory access permissions is canonically: */
		/*   open fake pipe, use write and test for -1 and EFAULT */
		/* Not used here, performance hit versus programmer error */
		memcpy (curr_field->data, pointed, size);
	} else {
		COB_FIELD_INIT (1, NULL, &const_alpha_attr);
		make_field_entry (&field);
		curr_field->data[0] = ' ';
		curr_field->size = 0;
	}
	if (unlikely(offset > 0)) {
		calc_ref_mod (curr_field, offset, length);
	}
	return curr_field;
}

/* RXWRXW - To be implemented */

cob_field *
cob_intr_boolean_of_integer (cob_field *f1, cob_field *f2)
{
	COB_UNUSED (f1);
	COB_UNUSED (f2);

	error_not_implemented ();
}

/* implementation of FUNCTION CHAR-NATIONAL - character from ordinal
   FIXME: Not implemented, Should use the program's national program collating sequence! */
cob_field *
cob_intr_char_national (cob_field *srcfield)
{
	COB_UNUSED (srcfield);

	error_not_implemented ();
}

/* implementation of FUNCTION DISPLAY-OF - alphanumeric for national character
   FIXME: Not implemented! */
cob_field *
cob_intr_display_of (const int offset, const int length,
		     const int params, ...)
{
	COB_UNUSED (offset);
	COB_UNUSED (length);
	COB_UNUSED (params);

	error_not_implemented ();
}

/* implementation of FUNCTION EXCEPTION-FILE-N
   national representation for filename that had an error last
   FIXME: Not implemented! */
cob_field *
cob_intr_exception_file_n (void)
{
	error_not_implemented ();
}

/* implementation of FUNCTION EXCEPTION-LOCATION-N
   national representation for location that had an error last
   FIXME: Not implemented! */
cob_field *
cob_intr_exception_location_n (void)
{
	error_not_implemented ();
}

cob_field *
cob_intr_integer_of_boolean (cob_field *srcfield)
{
	COB_UNUSED (srcfield);

	error_not_implemented ();
}

cob_field *
cob_intr_national_of (const int offset, const int length, const int params, ...)
{
	COB_UNUSED (offset);
	COB_UNUSED (length);
	COB_UNUSED (params);

	error_not_implemented ();
}

cob_field *
cob_intr_standard_compare (const int params, ...)
{
	COB_UNUSED (params);

	error_not_implemented ();
}

/* Initialization/exit routines */

void
cob_exit_intrinsic (void)
{
	if (set_cob_sqrt_two) {
		mpf_clear (cob_sqrt_two);
	}
	if (set_cob_pi) {
		mpf_clear (cob_pi);
	}
	if (set_cob_log_half) {
		mpf_clear (cob_log_half);
	}
	if (set_cob_log_ten) {
		mpf_clear (cob_log_ten);
	}
#ifndef DISABLE_GMP_RANDOM
	if (rand_needs_seeding == 0) {
		mpf_clear (rand_float);
		gmp_randclear (rand_state);
	}
#endif

	mpf_clear (cob_mpft_get);
	mpf_clear (cob_mpft2);
	mpf_clear (cob_mpft);

	mpz_clear (d5.value);
	mpz_clear (d4.value);
	mpz_clear (d3.value);
	mpz_clear (d2.value);
	mpz_clear (d1.value);

	mpz_clear (cob_mpzt);
	mpz_clear (cob_mexp);

	if (calc_base) {
		struct calc_struct	*calc_temp = calc_base;
		cob_u32_t		i;
		for (i = 0; i < COB_DEPTH_LEVEL; ++i, ++calc_temp) {
			if (calc_temp->calc_field.data) {
				cob_free (calc_temp->calc_field.data);
			}
		}
		cob_free (calc_base);
	}
}

void
cob_init_intrinsic (cob_global *lptr)
{
	struct calc_struct	*calc_temp;
	cob_u32_t		i;

	cobglobptr = lptr;

	move_field = NULL;
	curr_entry = 0;
	curr_field = NULL;
	calc_base = cob_malloc (COB_DEPTH_LEVEL * sizeof(struct calc_struct));
	calc_temp = calc_base;
	for (i = 0; i < COB_DEPTH_LEVEL; ++i, ++calc_temp) {
		calc_temp->calc_field.data = cob_malloc ((size_t)256);
		calc_temp->calc_field.size = 256;
		calc_temp->calc_size = 256;
	}

	/* mpf_init2 length = ceil (log2 (10) * strlen (x)) */

	mpz_init2 (cob_mexp, COB_MPZ_DEF);
	mpz_init2 (cob_mpzt, COB_MPZ_DEF);
	cob_decimal_init2 (&d1, 1536UL);
	cob_decimal_init2 (&d2, 1536UL);
	cob_decimal_init2 (&d3, 1536UL);
	cob_decimal_init2 (&d4, 1536UL);
	cob_decimal_init2 (&d5, 1536UL);

	mpf_init2 (cob_mpft, COB_MPF_PREC);
	mpf_init2 (cob_mpft2, COB_MPF_PREC);
	mpf_init2 (cob_mpft_get, COB_MPF_PREC);
}

#undef COB_DATETIMESTR_LEN
#undef COB_TIMESTR_LEN
#undef COB_DATESTR_LEN
