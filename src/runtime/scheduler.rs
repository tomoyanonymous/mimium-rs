use std::collections::LinkedList;

struct Value();
struct Scheduler();

trait AudioBackend {
    fn getSampleRate(&self) -> f64
    where
        Self: Sized;
}
struct AudioDriver();

struct Runtime {
    upvalues: LinkedList<Value>,
    scheduler: Scheduler,
    driver: dyn AudioBackend,
}
