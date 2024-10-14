use std::cmp::Reverse;
use std::collections::BinaryHeap;
use std::sync::Arc;

use mimium_lang::interner::{Symbol, ToSymbol};
use mimium_lang::plugin::Plugin;
use mimium_lang::runtime::vm::{self, ClosureIdx, ExtClsInfo, ExtFunType, Machine, ReturnCode};
use mimium_lang::runtime::Time;
use mimium_lang::{
    function, numeric,
    types::{PType, Type},
    unit,
};

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
    fn pop_task(&mut self, now: Time) -> Option<ClosureIdx>;
    fn set_cur_time(&mut self, time: Time);
}

#[derive(Clone)]
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

    fn pop_task(&mut self, now: Time) -> Option<ClosureIdx> {
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

#[derive(Clone)]
pub(crate) struct DummyScheduler;
impl Scheduler for DummyScheduler {
    fn new() -> Self
    where
        Self: Sized,
    {
        Self
    }

    fn schedule_at(&mut self, time: Time, task: ClosureIdx) {
        // do nothing
    }

    fn pop_task(&mut self, now: Time) -> Option<ClosureIdx> {
        // do nothing
        None
    }

    fn set_cur_time(&mut self, _time: Time) {
        // do nothing
    }
}

pub(crate) fn mimium_schedule_at(machine: &mut vm::Machine) -> ReturnCode {}
pub fn gen_schedule_at() -> (Symbol, ExtFunType) {}

pub struct SchedulerPlugin<T: Scheduler + Default> {
    sched: T,
    extcls: ExtClsInfo,
}
impl<T: Scheduler + Default> std::default::Default for SchedulerPlugin<T> {
    fn default() -> Self {
        let name = "_mimium_schedule_at".to_symbol();
        let t = function!(vec![numeric!(), function!(vec![], unit!())], unit!());
        let cls = |machine:&mut Machine| -> ReturnCode {
            let time = Time(vm::Machine::get_as::<f64>(machine.get_stack(0)) as u64);
            let clsid = vm::Machine::get_as::<ClosureIdx>(machine.get_stack(1));
            self.sched.schedule_at(time, clsid);
            0
        };
        Self {
            sched: Default::default(),
            extcls: (name, Arc::new(cls), t),
        }
    }
}

impl<T: Scheduler+Default> Plugin for T {
    fn get_ext_functions(&self) -> Vec<vm::ExtFnInfo> {
        vec![]
    }

    fn get_ext_closures(&self) -> Vec<vm::ExtClsInfo> {
        let name = "_mimium_schedule_at".to_symbol();
        let t = function!(vec![numeric!(), function!(vec![], unit!())], unit!());
        let cls =move  |machine:&mut Machine| -> ReturnCode {
            let time = Time(vm::Machine::get_as::<f64>(machine.get_stack(0)) as u64);
            let clsid = vm::Machine::get_as::<ClosureIdx>(machine.get_stack(1));
            self.schedule_at(time, clsid);
            0
        };
        vec![ (name, Arc::new(cls), t)]
    }
    fn on_sample(&mut self, machine: &mut vm::Machine) -> ReturnCode {


        self.sched.set_cur_time(now);
        while let Some(task_cls) = self.sched.pop_task(now) {
            let closure = self.get_closure(task_cls);
            self.execute(closure.fn_proto_pos, Some(task_cls));
            drop_closure(&mut vm.closures, task_cls);
        }

        0
    }
}
