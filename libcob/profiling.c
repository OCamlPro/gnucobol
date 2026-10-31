/*
   Copyright (C) 2023-2024, 2026 Free Software Foundation, Inc.
   Written by Emilien Lemaire, Fabrice Le Fessant, David Declerck,
   Simon Sobisch.

   This file is part of GnuCOBOL.

   The GnuCOBOL compiler is free software: you can redistribute it
   and/or modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   GnuCOBOL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GnuCOBOL.  If not, see <https://www.gnu.org/licenses/>.
*/

#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "config.h"

/* include internal and external libcob definitions, forcing exports */
#define COB_LIB_EXPIMP
#include "coblocal.h"

#include "tarstamp.h"
#include "common.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

/* Local types and variables */

struct cob_prof_module_list {
	struct cob_prof_module_list *next;
	struct cob_prof_module *info ;
};

static struct cob_prof_module_list *prof_info_list ;

/* We maintain a stack of the procedures entered as 3 different
 * arrays, with "current_idx" being the stack pointer. */
struct cob_prof_frame {
	cob_ns_time              start_time;
	struct cob_prof_module  *module;
	int                      procedure;
};

static struct cob_prof_frame *prof_stack;
/* Current size of previous arrays */
static int max_prof_depth;
static int current_idx = -1;

/* Whether profiling is active or not. */
static int is_active = -1;

/* Whether COB_PROF_TRACE is also specified */
static int has_traces = -1;

/* Which clock to use for clock_gettime (if available) */
#ifdef HAVE_CLOCK_GETTIME
static clockid_t	clockid = CLOCK_REALTIME;
#endif

/* Cached clock frequency on Windows */
#ifdef _WIN32
static LONGLONG 	qpc_freq = 0;
#endif

/* Remember static and dynamic configuration */
static cob_global			*cobglobptr = NULL;
static cob_settings			*cobsetptr = NULL;



/* Return the current time in nanoseconds. The result is guaranteed
   to be monotonic, by using an internal storage of the previous time. */
static cob_ns_time
get_ns_time (void)
{
	static cob_ns_time ns_time = 0;
	if (cob_is_test == 1) {
		ns_time += 1000000;
		return ns_time;
	}

#ifdef _WIN32
	if (qpc_freq) {
		LARGE_INTEGER performance_counter;
		QueryPerformanceCounter (&performance_counter);
		return performance_counter.QuadPart * 1000000000 / qpc_freq;
	}
#endif
	{
		cob_ns_time nanoseconds;
#ifdef HAVE_CLOCK_GETTIME
		struct timespec ts;
		clock_gettime (clockid, &ts);
		/* we may only have CLOCK_REALTIME, so need the monotonic check below */
		nanoseconds = COB_U64_C (1000000000) * ts.tv_sec + ts.tv_nsec;
#else
		nanoseconds = COB_U64_C (1000000000) * clock () / CLOCKS_PER_SEC;
#endif
		if (nanoseconds > ns_time) return nanoseconds;
		return ns_time;
	}
}

static void
prof_setup_clock ()
{
#ifdef HAVE_CLOCK_GETTIME
	struct timespec ts;
#endif

#ifdef _WIN32
	/* Should always succeed on Windows XP and above, but might
	   fail on Windows 2000. Not available on Windows 9x & NT. */
	LARGE_INTEGER performance_frequency;
	if (QueryPerformanceFrequency (&performance_frequency)) {
		qpc_freq = performance_frequency.QuadPart;
		return;
	}
#endif

#ifdef HAVE_CLOCK_GETTIME
	/* only CLOCK_REALTIME is guaranteed to be defined (and work),
 	   not all defined clocks are guaranteed to work */
 #ifdef CLOCK_MONOTONIC_RAW
	if (clock_gettime (CLOCK_MONOTONIC_RAW, &ts) == 0) {
		clockid = CLOCK_MONOTONIC_RAW;
		return;
	}
 #endif
 #ifdef CLOCK_MONOTONIC
	if (clock_gettime (CLOCK_MONOTONIC, &ts) == 0) {
		clockid = CLOCK_MONOTONIC;
		return;
	}
 #endif
#endif
}

static void
prof_init_static ()
{
	if (is_active == -1 && cobsetptr) {
		is_active = cobsetptr->cob_prof_enable;
	}
	if (is_active) {
		if (cob_check_env_true (cob_getenv_direct ("COB_PROF_TRACE"))) {
			has_traces = 1;
		  } else {
			has_traces = 0;
		  }
		prof_setup_clock ();
	}
}

const char *procedure_kind[] = {
	"MODULE", "SECTION", "PARAGRAPH", "ENTRY", "CALL"
};

static void print_stack (const char *msg)
{
	int j;

	for (j = 0; j <= current_idx; j++) {
		const struct cob_prof_frame *frame = &prof_stack[j];
		const struct cob_prof_procedure *p =
			& frame->module->procedures[frame->procedure];
		fprintf (stderr, "%s (%s:%d %s) -> ",
			p->text,
			p->file,
			p->line,
			procedure_kind[p->kind]);
	}
	fprintf(stderr, "%s\n", msg);
}

void
cob_init_prof (cob_global *lptr, cob_settings *sptr)
{
	cobglobptr = lptr;
	cobsetptr  = sptr;
}

struct cob_prof_module *
cob_prof_init_module (cob_module *module,
		      struct cob_prof_procedure *procedures,
		      size_t procedure_count)
{
	COB_UNUSED (module);

	prof_init_static ();
	if (is_active) {
		struct cob_prof_module *info;
		struct cob_prof_module_list *item;

		/* allocating the prof_module and its data (accessed via
		   element instead of sizeof to include potential padding) */
		info = cob_malloc (offsetof (struct cob_prof_module, data)
	                   + procedure_count * sizeof (info->data[0]));
		info->procedures = procedures;
		info->procedure_count = procedure_count;

		item = cob_malloc (sizeof(struct cob_prof_module_list));
		item->next = prof_info_list;
		item->info = info;
		
		prof_info_list = item;
		return info;
	}
	return NULL;
}

static void
cob_prof_realloc_arrays (void)
{
	int new_size = max_prof_depth * 2 + 16;

	if (new_size > cobsetptr->cob_prof_max_depth) {
		new_size = cobsetptr->cob_prof_max_depth;
	}

	if (max_prof_depth >= new_size) {
		const int	i_max = cob_min_int (10, current_idx);
		int 	dump_idx = current_idx;
		int 	i;
		cob_runtime_warning (_("[cob_prof] Profiling overflow at %d calls, aborting profiling."),
			current_idx);
		cob_runtime_warning (_("  Last %d calls on stack:"), i_max);
		for (i = 0; i < i_max; i++) {
			const struct cob_prof_frame *frame = &prof_stack[--dump_idx];
			const struct cob_prof_module *info = frame->module;
			struct cob_prof_procedure *proc = info->procedures + frame->procedure;
			cob_runtime_warning (_("  * %s at %s:%d"), proc->text,
					     proc->file, proc->line);
		}
		is_active = 0;
		return;
	}

	if (prof_stack) {
		prof_stack = cob_realloc (prof_stack,
			max_prof_depth * sizeof (*prof_stack),
			new_size * sizeof (*prof_stack));
	} else {
		prof_stack = cob_malloc (new_size * sizeof (*prof_stack));
	}
	max_prof_depth = new_size;
}

void
cob_prof_enter_procedure (struct cob_prof_module *info, int proc_idx)
{
	if (!is_active) return;


	current_idx++;
	if (current_idx >= max_prof_depth) {
		cob_prof_realloc_arrays ();
		if (!is_active) return;
	}

	{
		struct cob_prof_frame *frame = &prof_stack[current_idx];
		frame->procedure = proc_idx;
		frame->start_time = get_ns_time ();
		frame->module = info;
	}
	{
		struct cob_prof_proc_data *data = &info->data[proc_idx];
		data->recursions++;
		data->called_count++;
	}	
	if (has_traces) print_stack("ENTER");
}

void
cob_prof_exit_procedure (struct cob_prof_module *info, int proc_idx)
{
	/* Exit all the sections/paragraphs */
	cob_ns_time t;

	if (!is_active) return;

	if (has_traces) print_stack("EXIT");
	t = get_ns_time ();

	while (current_idx >= 0) {
		const struct cob_prof_frame *frame = &prof_stack[current_idx];
		struct cob_prof_module *curr_info = frame->module;
		struct cob_prof_proc_data *curr_data = &curr_info->data[frame->procedure];

		curr_data->recursions--;
		if (curr_data->recursions == 0){
			curr_data->total_time  += t - frame->start_time;
		}
		current_idx--;
		if (frame->procedure == proc_idx && curr_info == info) {
			return;
		}
	}
}

void
cob_prof_enter_section (struct cob_prof_module *info, int proc_idx)
{
	if (!is_active) return;
	/* We do not measure time on section enter/exit, we use the cumulative time
	   of all paragraphs of the section, calculating directly before output */
	info->data[proc_idx].called_count++;
}

void
cob_prof_use_paragraph_entry (struct cob_prof_module *info,
			      int paragraph_idx, int entry_idx){
	if (!is_active) return;
	info->data[entry_idx].called_count++;
	cob_prof_enter_procedure (info, paragraph_idx);
}

void
cob_prof_exit_section (struct cob_prof_module *info, int proc_idx)
{
	/* For now, nothing to do */
	COB_UNUSED (info);
	COB_UNUSED (proc_idx);
}

void
cob_prof_goto (struct cob_prof_module *info)
{
	COB_UNUSED (info);

	if (!is_active) return;

	{
		const struct cob_prof_frame *frame = &prof_stack[current_idx];
		struct cob_prof_module *curr_info = frame->module;
		const int curr_proc = frame->procedure;

		if (curr_info->procedures[curr_proc].kind == COB_PROF_PROCEDURE_PARAGRAPH) {
			cob_prof_exit_procedure (curr_info, curr_proc);
		}
	}
}

static void
print_monotonic_time (FILE *file, cob_ns_time t) {

	cob_ns_time nanoseconds = t ;
	cob_ns_time milliseconds = nanoseconds / 1000000;
	unsigned int seconds = (unsigned int)(milliseconds / 1000);

	if (seconds > 1000) {
		fprintf (file, "%u s", seconds);
	} else {
		milliseconds = milliseconds - 1000 * seconds;
		fprintf (file, "%u.%03u s", seconds, (unsigned int)milliseconds);
	}
}

/* Default format is: "%m,%s,%p,%e,%w,%k,%t,%h,%n" (in common.c) */
static void
cob_prof_print_line (
	FILE *file,
	struct cob_prof_module *info,
	int proc_idx)
{
	cob_ns_time	time = 0;
	const char	*module = NULL;
	const char	*section = NULL;
	const char	*paragraph = NULL;
	const char	*entry = NULL;
	const char	*kind = NULL;
	const char	*source_file;
	const char	*format;
	int 		line;
	int 		ncalls;

	if (info) {
		const struct cob_prof_proc_data	*data = &info->data[proc_idx];
		const struct cob_prof_procedure	*proc = info->procedures + proc_idx;
		const struct cob_prof_procedure	*proc_detail = &info->procedures[proc->section];
		time = data->total_time;
		ncalls = data->called_count;

		source_file = proc->file;
		line = proc->line;

		switch (proc->kind) {

		case COB_PROF_PROCEDURE_MODULE:
			module = proc->text;
			section = "";
			paragraph = "";
			entry = "";
			kind = "PROGRAM";
			break;

		case COB_PROF_PROCEDURE_SECTION:
			module = info->procedures[0].text;
			section = proc->text;
			paragraph = "";
			entry = "";
			kind = "SECTION";
			break;

		case COB_PROF_PROCEDURE_PARAGRAPH:
			module = info->procedures[0].text;
			section = proc_detail->text;
			paragraph = proc->text;
			entry = "";
			kind = "PARAGRAPH";
			break;

		case COB_PROF_PROCEDURE_ENTRY:
			module = info->procedures[0].text;
			section = info->procedures[proc_detail->section].text;
			paragraph = proc_detail->text;
			entry = proc->text;
			kind = "ENTRY";
			break;
		case COB_PROF_PROCEDURE_CALL:
			module = info->procedures[0].text;
			section = info->procedures[proc_detail->section].text;
			paragraph = proc_detail->text;
			entry = proc->text;
			kind = "CALL";
			break;
		}
	} else {
		module = "program-id";
		section = "section";
		paragraph = "paragraph";
		entry = "entry";
		kind = "kind";
		source_file = "file";
		ncalls = line = 0;
	}

	for (format = cobsetptr->cob_prof_format; *format; format++) {
		if (*format == '%') {
			format++;
			switch (*format) {
			case 'M':
			case 'm':
				fputs (module, file);
				break;
			case 'S':
			case 's':
				fputs (section, file);
				break;
			case 'P':
			case 'p':
				fputs (paragraph, file);
				break;
			case 'E':
			case 'e':
				fputs (entry, file);
				break;
			case 'F':
			case 'f':
				fputs (source_file, file);
				break;
			case 'L':
			case 'l':
				if (info) {
					fprintf (file, "%d", line);
				} else {
					fputs ("line", file);
				}
				break;
			case 'I':
			case 'i':
				if (info) {
					if (cob_is_test == 1) {
						fprintf (file, "%d", 123456);
					} else {
						fprintf (file, "%d", cob_sys_getpid ());
					}
				} else {
					fputs ("pid", file);
				}
				break;
			case 'W':
			case 'w':
				if (info) {
					fprintf (file, "%s:%d", source_file, line);
				} else {
					fputs ("location", file);
				}
				break;
			case 'K':
			case 'k':
				fputs (kind, file);
				break;
			case 'T':
			case 't':
				if (info) {
					fprintf (file, "%lld", time);
				} else {
					fputs ("time-ns", file);
				}
				break;
			case 'H':
			case 'h':
				if (info) {
					print_monotonic_time (file, time);
				} else {
					fputs ("time", file);
				}
				break;
			case 'N':
			case 'n':
				if (info) {
					fprintf (file, "%d", ncalls);
				} else {
					fputs ("ncalls", file);
				}
				break;
			default:
				fputc ('%', file);
				fputc (*format, file);
			}
		} else {
			fputc (*format, file);
		}
	}
	fputc ('\n', file);
#if 0 /* Simon: Do we really need this -only useful for multiple PIDs to a single file?
         If yes, then better place in local buffer
		 do a single unbuffered write, ensuring that all parts stay together*/
	fflush (file);
#endif
}

static void
cob_prof_free_stack (void)
{
	if (prof_stack) {
		cob_free (prof_stack);
		prof_stack = NULL;
	}
	max_prof_depth = 0;
	current_idx = -1;
}

static void
cob_prof_free_list (void)
{
	struct cob_prof_module_list *l;

	while ((l = prof_info_list) != NULL) {
		prof_info_list = l->next;
		cob_free (l->info);
		cob_free (l);
	}
}

void
cob_prof_end ()
{
	FILE *file;
	struct cob_prof_module_list *l;

	prof_init_static ();
	if (!cobsetptr || !is_active || !prof_info_list) {
		/* if not active then */
		cob_prof_free_stack ();
		cob_prof_free_list ();
		return;
	}

	while (current_idx >= 0) {
		const struct cob_prof_frame *frame = &prof_stack[current_idx];
		cob_prof_exit_procedure (frame->module,
					 frame->procedure);
	}

	file = cob_open_logfile (cobsetptr->cob_prof_filename);

	if (!!file) {
		const size_t prof_flen = strlen (cobsetptr->cob_prof_filename);

		/* First pass: accumulate section times */
		for (l = prof_info_list ; l != NULL; l=l->next){

			struct cob_prof_module *info = l->info;
			int i;

			for (i = 0; i < info->procedure_count; i++) {
				struct cob_prof_procedure  *proc = &info->procedures[i];
				if (proc->kind == COB_PROF_PROCEDURE_PARAGRAPH) {
					struct cob_prof_proc_data *data = &info->data[i];
					
					info->data[proc->section].total_time
						+= data->total_time;
				}
			}
		}

		cob_prof_print_line (file, NULL, 0);
		for (l = prof_info_list; l != NULL; l = l->next){

			struct cob_prof_module *info = l->info;
			int i;

			for (i = 0; i < info->procedure_count; i++) {
				cob_prof_print_line (file, info, i);
			}
		}
		fclose (file);
		if (prof_flen < 10
		 || memcmp (cobsetptr->cob_prof_filename + prof_flen - 10, 
		            "hidden.csv", 11)) {
			/* Only print this line when we are not in test mode,
			   or the file is called hidden.csv, so that we can
			   run profiling while running the full testsuite */
			fprintf (COB_STDERR, "File %s generated\n",
				cobsetptr->cob_prof_filename);
		}
	} else {
		cob_runtime_warning (_("error '%s' opening COB_PROF_FILE '%s'"),
				     cob_get_strerror (), cobsetptr->cob_prof_filename);
	}
	cob_prof_free_stack ();
	cob_prof_free_list ();
	is_active = 0;
}
