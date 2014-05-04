__BEGIN_PROGRAM

/*
 *  kernel/sched/core.c
 *
 *  Kernel scheduler and related syscalls
 *
 *  Copyright (C) 1991-2002  Linus Torvalds
 *
 *  1996-12-23  Modified by Dave Grothe to fix bugs in semaphores and
 *		make semaphores SMP safe
 *  1998-11-19	Implemented schedule_timeout() and related stuff
 *		by Andrea Arcangeli
 *  2002-01-04	New ultra-scalable O(1) scheduler by Ingo Molnar:
 *		hybrid priority-list and round-robin design with
 *		an array-switch method of distributing timeslices
 *		and per-CPU runqueues.  Cleanups and useful suggestions
 *		by Davide Libenzi, preemptible kernel bits by Robert Love.
 *  2003-09-03	Interactivity tuning by Con Kolivas.
 *  2004-04-02	Scheduler domains code by Nick Piggin
 *  2007-04-15  Work begun on replacing all interactivity tuning with a
 *              fair scheduling design by Con Kolivas.
 *  2007-05-05  Load balancing (smp-nice) and other improvements
 *              by Peter Williams
 *  2007-05-06  Interactivity improvements to CFS by Mike Galbraith
 *  2007-07-01  Group scheduling enhancements by Srivatsa Vaddagiri
 *  2007-11-29  RT balancing improvements by Steven Rostedt, Gregory Haskins,
 *              Thomas Gleixner, Mike Kravetz
 */

#include <asm/switch_to.h>
#include <asm/tlb.h>
#include <asm/irq_regs.h>
#include <asm/mutex.h>
#ifdef CONFIG_PARAVIRT
#include <asm/paravirt.h>
#endif

#include "sched.h"
#include "../workqueue_sched.h"
#include "../smpboot.h"

#define CREATE_TRACE_POINTS
#include <trace/events/sched.h>

void start_bandwidth_timer(struct hrtimer period_timer , int period)
{
	unsigned long delta;
	int soft, hard, now;

	for (;;) {
		if (hrtimer_active(period_timer))
			break;

		now = hrtimer_cb_get_time(period_timer);
		hrtimer_forward(period_timer, now, period);

		soft = hrtimer_get_softexpires(period_timer);
		hard = hrtimer_get_expires(period_timer);
		delta = into_ns(ktime_sub(hard, soft));
		hrtimer_start_range_ns(period_timer, soft, delta,
					 HRTIMER_MODE_ABS_PINNED, 0);
	}
}


static void update_rq_clock_task(struct rq *rq, long delta);

void update_rq_clock(struct rq *rq)
{
	long delta;

	if (rq->skip_clock_update > 0)
		return;

	delta = sched_clock_cpu(cpu_of(rq)) - rq->clock;
	rq->clock += delta;
	update_rq_clock_task(rq, delta);
}

/*
 * Debugging: various feature bits
 */


static int sched_feat_show(struct seq_file * m, void v)
{
	int i;

	for (i = 0; i < SCHED_FEAT_NR; i++) {
		if (!(sysctl_sched_features & (1 << i)))
			seq_puts(m, "NO_");
		seq_printf(m, "%s ", sched_feat_names[i]);
	}
	seq_puts(m, "\n");

	return 0;
}

#ifdef HAVE_JUMP_LABEL

#define jump_label_keytrue  STATIC_KEY_INIT_TRUE
#define jump_label_keyfalse STATIC_KEY_INIT_FALSE

#undef SCHED_FEAT

static void sched_feat_disable(int i)
{
	if (static_key_enabled(&sched_feat_keys[i]))
		static_key_slow_dec(&sched_feat_keys[i]);
}

static void sched_feat_enable(int i)
{
	if (!static_key_enabled(&sched_feat_keys[i]))
		static_key_slow_inc(&sched_feat_keys[i]);
}
#else
static void sched_feat_disable(int i) { }
static void sched_feat_enable(int i) { }
#endif /* HAVE_JUMP_LABEL */

static int
sched_feat_write(struct file *filp, char* ubuf,
		int cnt, int *ppos)
{
	char buf[64];
	char *cmp;
	int neg = 0;
	int i;

	if (cnt > 63)
		cnt = 63;

	if (copy_from_user(&buf, ubuf, cnt))
		return -EFAULT;

	buf[cnt] = 0;
	cmp = strstrip(buf);

	if (strncmp(cmp, "NO_", 3) == 0) {
		neg = 1;
		cmp += 3;
	}

	for (i = 0; i < SCHED_FEAT_NR; i++) {
		if (strcmp(cmp, sched_feat_names[i]) == 0) {
			if (neg) {
				sysctl_sched_features &= ~(1 << i);
				sched_feat_disable(i);
			} else {
				sysctl_sched_features |= (1 << i);
				sched_feat_enable(i);
			}
			break;
		}
	}

	if (i == SCHED_FEAT_NR)
		return -EINVAL;

	*ppos += cnt;

	return cnt;
}

static int sched_feat_open(struct inode *inode, struct file *filp)
{
	return single_open(filp, sched_feat_show, NULL);
}


static int sched_init_debug(void)
{
	debugfs_create_file("sched_features", 0644, NULL, NULL,
			&sched_feat_fops);

	return 0;
}
#endif /* CONFIG_SCHED_DEBUG */

/*
 * Number of tasks to iterate in a single balance run.
 * Limited because this is done with IRQs disabled.
 */
const unsigned int sysctl_sched_nr_migrate = 32;

/*
 * period over which we average the RT time consumption, measured
 * in ms.
 *
 * default: 1s
 */
const unsigned int sysctl_sched_time_avg = MSEC_PER_SEC;

/*
 * period over which we measure -rt task cpu usage in us.
 * default: 1s
 */
unsigned int sysctl_sched_rt_period = 1000000;

int scheduler_running;

#ifdef CONFIG_SCHED_HRTICK
/*
 * Use HR-timers to deliver accurate preemption points.
 *
 * Its all a bit involved since we cannot program an hrt while holding the
 * rq->lock. So what we do is store a state in in rq->hrtick_* and ask for a
 * reschedule event.
 *
 * When we get rescheduled we reprogram the hrtick_timer outside of the
 * rq->lock.
 */

static void hrtick_clear(struct rq *rq)
{
	if (hrtimer_active(&rq->hrtick_timer))
		hrtimer_cancel(&rq->hrtick_timer);
}

/*
 * High-resolution timer tick.
 * Runs from hardirq context with interrupts disabled.
 */
static enum hrtimer_restart hrtick(struct hrtimer *timer)
{
	struct rq *rq = container_of(timer, rq, hrtick_timer);

	WARN_ON_ONCE(cpu_of(rq) != smp_processor_id());

	raw_spin_lock(&rq->lock);
	update_rq_clock(rq);
	rq->curr->sched_class->task_tick(rq, rq->curr, 1);
	raw_spin_unlock(&rq->lock);

	return HRTIMER_NORESTART;
}

#ifdef CONFIG_SMP
/*
 * called from hardirq (IPI) context
 */
static void hrtick_start(void *arg)
{
	struct rq *rq = arg;

	raw_spin_lock(&rq->lock);
	hrtimer_restart(&rq->hrtick_timer);
	rq->hrtick_csd_pending = 0;
	raw_spin_unlock(&rq->lock);
}

/*
 * Called to set the hrtick timer state.
 *
 * called with rq->lock held and irqs disabled
 */
void hrtick_start(struct rq *rq, int delay)
{
	struct hrtimer *timer = &rq->hrtick_timer;
	int time = ktime_add_ns(timer->base->get_time(), delay);

	hrtimer_set_expires(timer, time);

	if (rq == this_rq()) {
		hrtimer_restart(timer);
	} else if (!rq->hrtick_csd_pending) {
		smp_call_function_single(cpu_of(rq), &rq->hrtick_csd, 0);
		rq->hrtick_csd_pending = 1;
	}
}

static int
hotplug_hrtick(struct notifier_block *nfb, unsigned long action, void *hcpu)
{
	int cpu = (int)(long)hcpu;

	switch (action) {
	case CPU_UP_CANCELED:
	case CPU_UP_CANCELED_FROZEN:
	case CPU_DOWN_PREPARE:
	case CPU_DOWN_PREPARE_FROZEN:
	case CPU_DEAD:
	case CPU_DEAD_FROZEN:
		hrtick_clear(cpu_rq(cpu));
		return NOTIFY_OK;
	}

	return NOTIFY_DONE;
}

static void init_hrtick(void)
{
	hotcpu_notifier(hotplug_hrtick, 0);
}
#else
/*
 * Called to set the hrtick timer state.
 *
 * called with rq->lock held and irqs disabled
 */
void hrtick_start(struct rq *rq, int delay)
{
	hrtimer_start_range_ns(&rq->hrtick_timer, ns_to_ktime(delay), 0,
			HRTIMER_MODE_REL_PINNED, 0);
}

static  void init_hrtick(void)
{
}
#endif /* CONFIG_SMP */

static void init_rq_hrtick(struct rq *rq)
{
#ifdef CONFIG_SMP
	rq->hrtick_csd_pending = 0;

	rq->hrtick_csd.flags = 0;
	rq->hrtick_csd.func = hrtick_start;
	rq->hrtick_csd.info = rq;
#endif

	hrtimer_init(&rq->hrtick_timer, CLOCK_MONOTONIC, HRTIMER_MODE_REL);
	rq->hrtick_timer.function = hrtick;
}
#else	/* CONFIG_SCHED_HRTICK */
static  void hrtick_clear(struct rq *rq)
{
}

static  void init_rq_hrtick(struct rq *rq)
{
}

static  void init_hrtick(void)
{
}
#endif	/* CONFIG_SCHED_HRTICK */

/*
 * resched_task - mark a task 'to be rescheduled now'.
 *
 * On UP this means the setting of the need_resched flag, on SMP it
 * might also involve a cross-CPU call to trigger the scheduler on
 * the target CPU.
 */
#ifdef CONFIG_SMP

#ifndef tsk_is_polling
#define tsk_is_polling(t) 0
#endif

void resched_task(struct task_struct *p)
{
	int cpu;

	assert_raw_spin_locked(&task_rq(p)->lock);

	if (test_tsk_need_resched(p))
		return;

	set_tsk_need_resched(p);

	cpu = task_cpu(p);
	if (cpu == smp_processor_id())
		return;

	/* NEED_RESCHED must be visible before we test polling */
	smp_mb();
	if (!tsk_is_polling(p))
		smp_send_reschedule(cpu);
}

void resched_cpu(int cpu)
{
	struct rq *rq = cpu_rq(cpu);
	unsigned long flags;

	if (!raw_spin_trylock_irqsave(&rq->lock, flags))
		return;
	resched_task(cpu_curr(cpu));
	raw_spin_unlock_irqrestore(&rq->lock, flags);
}

