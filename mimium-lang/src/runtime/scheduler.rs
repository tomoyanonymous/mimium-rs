use std::collections::BinaryHeap;
use std::{cmp::Reverse, sync::Arc};

use crate::interner::{Symbol, ToSymbol};

use super::vm::{self, ClosureIdx, ExtClsType, ExtFunType, ReturnCode};

pub struct RuntimeCtx<'a> {
    machine: &'a mut vm::Machine,
    prog: &'a vm::Program,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Time(u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Task {
    when: Time,
    cls: ClosureIdx,
}
impl PartialOrd for Task {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.when.partial_cmp(&other.when)
    }
}
impl Ord for Task {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}
pub trait Scheduler {
    fn new() -> Self
    where
        Self: Sized;
    fn schedule_at(&mut self, time: Time, task: ClosureIdx);
    fn run_task<'a>(&mut self, now: Time, ctx: &mut RuntimeCtx<'a>);
}

pub struct SyncScheduler {
    tasks: BinaryHeap<Reverse<Task>>,
}

impl Scheduler for SyncScheduler {
    fn new() -> Self {
        Self {
            tasks: Default::default(),
        }
    }
    fn schedule_at(&mut self, when: Time, cls: ClosureIdx) {
        self.tasks.push(Reverse(Task { when, cls }));
    }

    fn run_task(&mut self, now: Time, ctx: &mut RuntimeCtx<'_>) {
        match self.tasks.peek() {
            Some(Reverse(Task { when, cls })) if *when <= now => {
                let closure = ctx.machine.get_closure(*cls);
                ctx.machine
                    .execute(closure.fn_proto_pos, ctx.prog, Some(*cls));
                let _ = self.tasks.pop();
            }
            _ => {}
        }
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

    fn run_task<'a>(&mut self, now: Time, ctx: &mut RuntimeCtx<'a>) {
        debug_assert!(false, "dummy scheduler invoked");
    }
}

fn mimium_schedule_at(machine: &mut vm::Machine) -> ReturnCode {
    let time = Time(machine.get_stack(0));
    let cls = vm::Machine::get_as::<ClosureIdx>(machine.get_stack(1));
    machine.scheduler.schedule_at(time, cls);
    0
}
pub fn gen_schedule_at() -> (Symbol, ExtFunType) {
    ("_mimium_schedule_at".to_symbol(), mimium_schedule_at)
}
