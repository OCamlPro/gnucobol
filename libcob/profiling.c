/*
   Copyright (C) 2023-2024 Free Software Foundation, Inc.
   Written by Emilien Lemaire and Fabrice Le Fessant.

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

#ifdef	_WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

/* Local types and variables */

struct cob_prof_module_list {
	struct cob_prof_module *info ;
	struct cob_prof_module_list *next;
};

static struct cob_prof_module_list *prof_info_list ;

/* We maintain a stack of the procedures entered as 3 different
 * arrays, with "current_idx" being the stack pointer. */
static cob_ns_time              *start_times;
static int                      *called_procedures;
static struct cob_prof_module*  *called_runtimes;
/* Current size of previous arrays */
static int max_prof_depth;
static int current_idx = -1;

/* Whether profiling is active or not. */
static int is_active = 0;
/* Whether we are in testsuite mode */
static int is_test = 0;

/* Which clock to use for clock_gettime (if available) */
#if !defined (_WIN32) && defined (HAVE_CLOCK_GETTIME)
static clockid_t clockid = CLOCK_REALTIME;
#endif

/* Cached clock frequency on Windows */
#ifdef _WIN32
static LONGLONG qpc_freq = 0;
#endif

/* Remember static and dynamic configuration */
static cob_global               *cobglobptr = NULL;
static cob_settings             *cobsetptr = NULL;



/* Return the current time in nanoseconds. The result is guarranteed
 * to be monotonic, by using an internal storage of the previous
 * time. */
static cob_ns_time
get_ns_time (void)
{
	if (is_test){
		static cob_ns_time ns_time = 0;
		ns_time += 1000000;
		return ns_time;
	} else {
		cob_ns_time ns_time = 0;
		unsigned long long nanoseconds;
#ifdef _WIN32
		if (qpc_freq) {
			LARGE_INTEGER performance_counter;
			QueryPerformanceCounter(&performance_counter);
			performance_counter.QuadPart *= 1000000000;
			performance_counter.QuadPart /= qpc_freq;
			nanoseconds = performance_counter.QuadPart;
		} else {
#endif /* _WIN32 */
#ifdef HAVE_CLOCK_GETTIME
			struct timespec ts;
			clock_gettime(clockid, &ts);
			nanoseconds = ts.tv_sec * 1000000000 + ts.tv_nsec;
#else
			nanoseconds = clock() * 1000000000 / CLOCKS_PER_SEC;
#endif /* HAVE_CLOCK_GETTIME */
#ifdef _WIN32
		}
#endif /* _WIN32 */
		if (nanoseconds > ns_time) ns_time = nanoseconds;
		return ns_time;
	}
}

static void
prof_init_static ()
{
	static int init_done = 0;

	if (!init_done && cobsetptr){
#ifdef HAVE_CLOCK_GETTIME
		struct timespec ts;
		if (clock_gettime(CLOCK_MONOTONIC_RAW, &ts) == 0) {
			clockid = CLOCK_MONOTONIC_RAW;
		} else if (clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
			clockid = CLOCK_MONOTONIC;
		}
#endif
#ifdef _WIN32
		/* Should always succeed on Windows XP and above, but might
		   fail on Windows 2000. Not available on Windows 9x & NT. */
		LARGE_INTEGER performance_frequency;
		if (QueryPerformanceFrequency(&performance_frequency)) {
			qpc_freq = performance_frequency.QuadPart;
		}
#endif
		init_done = 1;
		is_active = cobsetptr->cob_prof_enable;
		if (is_active) {
			is_test = !!getenv ("COB_IS_RUNNING_IN_TESTMODE");
		}
	}
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
	prof_init_static();
	if (is_active){
		struct cob_prof_module *info;
		struct cob_prof_module_list *item;

		info = cob_malloc (sizeof(struct cob_prof_module));
		info->total_times = cob_malloc ( procedure_count * sizeof(cob_ns_time) );
		info->called_count = cob_malloc ( procedure_count * sizeof(unsigned int) );
		info->procedure_recursions = cob_malloc ( procedure_count * sizeof(unsigned int) );
		info->procedures = procedures;
		info->procedure_count = procedure_count;

		item = cob_malloc (sizeof(struct cob_prof_module_list));
		item->info = info;
		item->next = prof_info_list;
		prof_info_list = item;
		return info;
	}
	return NULL;
}

static void
cob_prof_realloc_arrays (void)
{
	int new_size = max_prof_depth * 2 + 16;

	if (new_size > cobsetptr->cob_prof_max_depth)
		new_size = cobsetptr->cob_prof_max_depth;

	if (max_prof_depth >= new_size){
		int i;
		cob_runtime_warning (_("[cob_prof] Profiling overflow at %d calls, aborting profiling."), current_idx);
		cob_runtime_warning (_("  Last 10 calls on stack:"));
		for (i = 0; i < cob_min_int(10, current_idx); i++){
			struct cob_prof_module *info = called_runtimes[current_idx-1-i];
			int proc_idx = called_procedures[current_idx-1-i];
			struct cob_prof_procedure *proc = info->procedures + proc_idx;
			cob_runtime_warning (_("  * %s at %s:%d"), proc->text,
					     proc->file, proc->line);
		}
		is_active = 0;
		return;
	}

	if (max_prof_depth){
		start_times = cob_realloc (
			start_times,
			max_prof_depth * sizeof(cob_ns_time),
			new_size * sizeof(cob_ns_time)
			);
		called_procedures = cob_realloc (
			called_procedures,
			max_prof_depth * sizeof(int),
			new_size * sizeof(int)
			);
		called_runtimes = cob_realloc (
			called_runtimes,
			max_prof_depth * sizeof(struct cob_prof_module*),
			new_size * sizeof(struct cob_prof_module*)
			);

	} else {
		start_times = cob_malloc (new_size * sizeof(cob_ns_time));
		called_procedures = cob_malloc (new_size * sizeof(int));
		called_runtimes = cob_malloc (new_size * sizeof(struct cob_prof_module*));
	}
	max_prof_depth = new_size;
}

void
cob_prof_enter_procedure (struct cob_prof_module *info, int proc_idx)
{
	cob_ns_time t;

	if (!is_active) return;

	t = get_ns_time ();

	current_idx++;
	if (current_idx >= max_prof_depth) {
		cob_prof_realloc_arrays();
		if (!is_active) return;
	}

	called_procedures[current_idx] = proc_idx;
	called_runtimes[current_idx] = info;
	start_times[current_idx] = t;

	info->procedure_recursions[proc_idx] ++;
	info->called_count[proc_idx] ++;
}

void
cob_prof_exit_procedure (struct cob_prof_module *info, int proc_idx)
{
	/* Exit all the sections/paragraphs */
	cob_ns_time t;

	if (!is_active) return;

	t = get_ns_time ();

	while (current_idx >= 0) {
		int curr_proc = called_procedures[current_idx];
		struct cob_prof_module *curr_info = called_runtimes[current_idx];

		curr_info->procedure_recursions[curr_proc]--;
		if (curr_info->procedure_recursions[curr_proc]==0){
			curr_info->total_times[curr_proc] += t - start_times[current_idx];
		}
		current_idx--;
		if (curr_proc == proc_idx && curr_info == info) {
			return;
		}
	}
}

void
cob_prof_enter_section (struct cob_prof_module *info, int proc_idx)
{
	if (!is_active) return;
	/* We do not measure time on section enter/exit, we use the cumulative time
	   of all paragraphs of the section */
	info->called_count[proc_idx] ++;
}

void
cob_prof_use_paragraph_entry (struct cob_prof_module *info,
			      int paragraph_idx, int entry_idx){
	if (!is_active) return;
	info->called_count[entry_idx] ++;
	cob_prof_enter_procedure (info, paragraph_idx);
}

void
cob_prof_exit_section (struct cob_prof_module *info, int proc_idx)
{
	/* For now, nothing to do */
}

void
cob_prof_goto (struct cob_prof_module *info)
{
	int curr_proc;
	struct cob_prof_module *curr_info;

	if (!is_active) return;

	curr_proc = called_procedures[current_idx];
	curr_info = called_runtimes[current_idx];

	if (curr_info->procedures[curr_proc].kind == COB_PROF_PROCEDURE_PARAGRAPH){
		cob_prof_exit_procedure (curr_info, curr_proc);
	}
}

static void
print_monotonic_time (FILE *file, cob_ns_time t) {

	cob_ns_time nanoseconds = t ;
	cob_ns_time milliseconds = nanoseconds / 1000000;
	unsigned int seconds = milliseconds / 1000;
	milliseconds = milliseconds - 1000 * seconds;

	if (seconds > 1000) {
		fprintf (file, "%d s", seconds);
	} else {
		fprintf (file, "%d.%03Ld s", seconds, milliseconds);
	}
}

/* Default format is: "%m,%s,%p,%e,%w,%k,%t,%h,%n" (in common.c) */
static void
cob_prof_print_line (
	FILE *file,
	struct cob_prof_module *info,
	int proc_idx)
{
	int i;
	const char *module = NULL;
	const char *section = NULL;
	const char *paragraph = NULL;
	const char *entry = NULL;
	const char *kind = NULL;
	const char *source_file;
	int         line;
	int         ncalls;
	cob_ns_time time = 0;
	struct cob_prof_procedure *proc;

	if (info){
		time = info->total_times[proc_idx];
		ncalls = info->called_count[proc_idx];
		proc = info->procedures + proc_idx;

		source_file = proc->file;
		line = proc->line;
		switch (proc->kind){

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
			section = info->procedures[proc->section].text;
			paragraph = proc->text;
			entry = "";
			kind = "PARAGRAPH";
			break;

		case COB_PROF_PROCEDURE_ENTRY:
			module = info->procedures[0].text;
			section = info->procedures[
				info->procedures[proc->section].section].text;
			paragraph = info->procedures[proc->section].text;
			entry = proc->text;
			kind = "ENTRY";
			break;
		case COB_PROF_PROCEDURE_CALL:
			module = info->procedures[0].text;
			section = info->procedures[
				info->procedures[proc->section].section].text;
			paragraph = info->procedures[proc->section].text;
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
		ncalls = 0;
	}

	for (i = 0; cobsetptr->cob_prof_format[i] != 0; i++) {
		if (cobsetptr->cob_prof_format[i] == '%') {
			i++;
			switch (cobsetptr->cob_prof_format[i]) {
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
				if (info){
					fprintf (file, "%d", line);
				} else {
					fputs ("line", file);
				}
				break;
			case 'I':
			case 'i':
				if (info){
					if (is_test){
						fprintf (file, "%d", 42);
					} else {
						fprintf (file, "%d", cob_sys_getpid());
					}
				} else {
					fputs ("pid", file);
				}
				break;
			case 'W':
			case 'w':
				if (info){
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
				if (info){
					fprintf (file, "%lld", time);
				} else {
					fputs ("time-ns", file);
				}
				break;
			case 'H':
			case 'h':
				if (info){
					print_monotonic_time (file, time);
				} else {
					fputs ("time", file);
				}
				break;
			case 'N':
			case 'n':
				if (info){
					fprintf (file, "%d", ncalls);
				} else {
					fputs ("ncalls", file);
				}
				break;
			default:
				fputc ('%', file);
				fputc (cobsetptr->cob_prof_format[i], file);
			}
		} else {
			fputc (cobsetptr->cob_prof_format[i], file);
		}
	}
	fputc ('\n', file);
	fflush (file);
}


void
cob_prof_end ()
{
	FILE *file;
	struct cob_prof_module_list *l;

	prof_init_static ();

	if (!cobsetptr || !is_active || !prof_info_list) return;

	while (current_idx >= 0) {
		cob_prof_exit_procedure (called_runtimes[current_idx],
					 called_procedures[current_idx]);
	}

	file = cob_open_logfile (cobsetptr->cob_prof_filename);

	if (!!file) {

		/* First pass: accumulate section times */
		for (l = prof_info_list ; l != NULL; l=l->next){

			struct cob_prof_module *info = l->info;
			int i;

			for (i = 0; i < info->procedure_count; i++) {
				if (info->procedures[i].kind == COB_PROF_PROCEDURE_PARAGRAPH){
					info->total_times[info->procedures[i].section]
						+= info->total_times[i];
				}
			}
		}

		cob_prof_print_line (file, NULL, 0);
		for (l = prof_info_list ; l != NULL; l=l->next){

			struct cob_prof_module *info = l->info;
			int i;

			for (i = 0; i < info->procedure_count; i++) {
				cob_prof_print_line (file, info, i);
			}
		}
		fclose (file);
		fprintf(stderr, "File %s generated\n", cobsetptr->cob_prof_filename);
	} else {
		cob_runtime_warning (_("error '%s' opening COB_PROF_FILE '%s'"),
				     cob_get_strerror (), cobsetptr->cob_prof_filename);
	}
	current_idx = -1;
	is_active = 0;
}
