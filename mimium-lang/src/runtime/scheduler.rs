use std::cmp::Reverse;
use std::collections::BinaryHeap;

use super::vm::{self, ClosureIdx};

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

pub enum Error {
    QueueIsFull,
}
pub trait Scheduler {
    fn schedule_at(&mut self, time: Time, task: ClosureIdx) -> Result<(), Error>;
    fn run_task<'a>(&mut self, now: Time, ctx: &mut RuntimeCtx<'a>) -> Result<(), Error>;
}

struct SyncScheduler {
    tasks: BinaryHeap<Reverse<Task>>,
}
impl Scheduler for SyncScheduler {
    fn schedule_at(&mut self, when: Time, cls: ClosureIdx) -> Result<(), Error> {
        self.tasks.push(Reverse(Task { when, cls }));
        Ok(())
    }

    fn run_task(&mut self, now: Time, ctx: &mut RuntimeCtx<'_>) -> Result<(), Error> {
        if let Some(Reverse(Task { when, cls })) = self.tasks.peek() {
            if *when <= now {
                let closure = ctx.machine.get_closure(*cls);
                ctx.machine
                    .execute(closure.fn_proto_pos, ctx.prog, Some(*cls));
                Ok(())
            } else {
                Ok(())
            }
        } else {
            Ok(())
        }
    }
}
