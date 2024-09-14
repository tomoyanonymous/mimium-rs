use std::cmp::Reverse;
use std::collections::BinaryHeap;

use crate::interner::{Symbol, ToSymbol};

use super::vm::{self, ClosureIdx, ExtFunType, ReturnCode};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Time(pub u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Task {
    when: Time,
    cls: ClosureIdx,
}
impl PartialOrd for Task {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Task {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.when.cmp(&other.when)
    }
}
pub trait Scheduler {
    fn new() -> Self
    where
        Self: Sized;
    fn schedule_at(&mut self, time: Time, task: ClosureIdx);
    fn pop_task(&mut self, now: Time, prog: &vm::Program) -> Option<ClosureIdx>;
    fn set_cur_time(&mut self, time: Time);
}

pub struct SyncScheduler {
    tasks: BinaryHeap<Reverse<Task>>,
    cur_time: Time,
}

impl Scheduler for SyncScheduler {
    fn new() -> Self {
        Self {
            tasks: Default::default(),
            cur_time: Time(0),
        }
    }
    fn schedule_at(&mut self, when: Time, cls: ClosureIdx) {
        if when <= self.cur_time {
            // TODO: ideally, this should not stop runtime.
            panic!(
                "A task must be scheduled in future (current time: {}, schedule: {})",
                self.cur_time.0, when.0
            )
        }
        self.tasks.push(Reverse(Task { when, cls }));
    }

    fn pop_task(&mut self, now: Time, prog: &vm::Program) -> Option<ClosureIdx> {
        match self.tasks.peek() {
            Some(Reverse(Task { when, cls })) if *when <= now => {
                let res = Some(*cls);
                let _ = self.tasks.pop();
                res
            }
            _ => None,
        }
    }

    fn set_cur_time(&mut self, time: Time) {
        self.cur_time = time
    }
}

pub(crate) struct DummyScheduler;
impl Scheduler for DummyScheduler {
    fn new() -> Self
    where
        Self: Sized,
    {
        Self
    }

    fn schedule_at(&mut self, time: Time, task: ClosureIdx) {
        debug_assert!(false, "dummy scheduler invoked");
    }

    fn pop_task(&mut self, now: Time, prog: &vm::Program) -> Option<ClosureIdx> {
        panic!("dummy scheduler invoked")
    }

    fn set_cur_time(&mut self, _time: Time) {
        // do nothing
    }
}

pub(crate) fn mimium_schedule_at(machine: &mut vm::Machine) -> ReturnCode {
    let time = Time(vm::Machine::get_as::<f64>(machine.get_stack(0)) as u64);
    let cls = vm::Machine::get_as::<ClosureIdx>(machine.get_stack(1));
    machine.scheduler.schedule_at(time, cls);
    0
}
pub fn gen_schedule_at() -> (Symbol, ExtFunType) {
    ("_mimium_schedule_at".to_symbol(), mimium_schedule_at)
}
