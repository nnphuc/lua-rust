use lua_core::{LuaClosure, LuaTable, LuaValue, UpvalueInner};
use std::collections::HashSet;
use std::sync::{Arc, RwLock, Weak};

/// Cycle-collecting garbage collector overlay for Arc-based Lua values.
///
/// Tracks all allocated tables and closures via `Weak` references. On
/// collection, marks reachable objects from roots via iterative DFS, then
/// clears the internals of unreachable objects so Arc reference cycles break
/// and memory is freed.
pub struct GcState {
    tables: Vec<Weak<RwLock<LuaTable>>>,
    closures: Vec<Weak<LuaClosure>>,
    alloc_count: usize,
    alloc_threshold: usize,
    pub collections: usize,
}

impl GcState {
    pub fn new() -> Self {
        Self {
            tables: Vec::new(),
            closures: Vec::new(),
            alloc_count: 0,
            alloc_threshold: 256,
            collections: 0,
        }
    }

    pub fn track_table(&mut self, weak: Weak<RwLock<LuaTable>>) {
        self.tables.push(weak);
        self.alloc_count += 1;
    }

    pub fn track_closure(&mut self, weak: Weak<LuaClosure>) {
        self.closures.push(weak);
        self.alloc_count += 1;
    }

    pub fn should_collect(&self) -> bool {
        self.alloc_count >= self.alloc_threshold
    }

    pub fn tracked_count(&self) -> usize {
        let live_tables = self.tables.iter().filter(|w| w.strong_count() > 0).count();
        let live_closures = self.closures.iter().filter(|w| w.strong_count() > 0).count();
        live_tables + live_closures
    }

    /// Run a full mark-and-sweep collection cycle.
    pub fn collect(&mut self, roots: &[LuaValue]) {
        // Mark phase: iterative DFS from roots
        let mut marked_tables: HashSet<usize> = HashSet::new();
        let mut marked_closures: HashSet<usize> = HashSet::new();
        let mut worklist: Vec<LuaValue> = roots.to_vec();

        while let Some(val) = worklist.pop() {
            match &val {
                LuaValue::Table(arc) => {
                    let ptr = Arc::as_ptr(arc) as usize;
                    if !marked_tables.insert(ptr) {
                        continue;
                    }
                    if let Ok(tbl) = arc.read() {
                        for v in tbl.array.iter() {
                            worklist.push(v.clone());
                        }
                        for v in tbl.hash.values() {
                            worklist.push(v.clone());
                        }
                        if let Some(mt) = &tbl.metatable {
                            worklist.push(LuaValue::Table(mt.clone()));
                        }
                    }
                }
                LuaValue::Closure(arc) => {
                    let ptr = Arc::as_ptr(arc) as usize;
                    if !marked_closures.insert(ptr) {
                        continue;
                    }
                    for uv in &arc.upvalues {
                        if let Ok(inner) = uv.0.read() {
                            if let UpvalueInner::Closed(v) = &*inner {
                                worklist.push(v.clone());
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        // Also mark tables referenced directly (e.g., type_metatables stored as Arc)
        // These are handled by the caller adding them to roots as LuaValue::Table.

        // Sweep tables: clear internals of unmarked live tables
        for weak in &self.tables {
            if let Some(arc) = weak.upgrade() {
                let ptr = Arc::as_ptr(&arc) as usize;
                if !marked_tables.contains(&ptr) {
                    if let Ok(mut tbl) = arc.write() {
                        tbl.array.clear();
                        tbl.hash.clear();
                        tbl.metatable = None;
                    }
                }
            }
        }

        // Sweep closures: nil out upvalue cells of unmarked closures
        for weak in &self.closures {
            if let Some(arc) = weak.upgrade() {
                let ptr = Arc::as_ptr(&arc) as usize;
                if !marked_closures.contains(&ptr) {
                    for uv in &arc.upvalues {
                        if let Ok(mut inner) = uv.0.write() {
                            *inner = UpvalueInner::Closed(LuaValue::Nil);
                        }
                    }
                }
            }
        }

        // Purge dead weak references
        self.tables.retain(|w| w.strong_count() > 0);
        self.closures.retain(|w| w.strong_count() > 0);

        let live_count = self.tables.len() + self.closures.len();
        self.alloc_count = 0;
        self.alloc_threshold = std::cmp::max(live_count * 2, 256);
        self.collections += 1;
    }
}
