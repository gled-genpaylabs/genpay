use std::fs;
use std::env;
use std::sync::Arc;

use anyhow::Result;
use object::{Object, ObjectSection};
use solana_sbpf::{
    aligned_memory::AlignedMemory,
    ebpf,
    elf::Executable,
    memory_region::{MemoryMapping, MemoryRegion},
    program::{BuiltinProgram, FunctionRegistry, SBPFVersion},
    verifier::RequisiteVerifier,
    vm::{Config, ContextObject, EbpfVm},
};

/// Simple instruction meter for testing
#[derive(Debug, Clone, Default)]
pub struct TestContextObject {
    /// Contains the register state at every instruction in order of execution
    pub trace_log: Vec<[u64; 12]>,
    /// Maximal amount of instructions which still can be executed
    pub remaining: u64,
}

impl ContextObject for TestContextObject {
    fn trace(&mut self, state: [u64; 12]) {
        self.trace_log.push(state);
    }

    fn consume(&mut self, amount: u64) {
        self.remaining = self.remaining.saturating_sub(amount);
    }

    fn get_remaining(&self) -> u64 {
        self.remaining
    }
}

impl TestContextObject {
    /// Initialize with instruction meter
    pub fn new(remaining: u64) -> Self {
        Self {
            trace_log: Vec::new(),
            remaining,
        }
    }
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: bpf-tester <path-to-object-file>");
        return Ok(());
    }

    let elf_bytes = fs::read(&args[1])?;
    let obj_file = object::File::parse(&*elf_bytes)?;
    let text_section = obj_file.section_by_name(".text").ok_or_else(|| anyhow::anyhow!(".text section not found"))?;
    let text_bytes = text_section.data()?;

    let config = Config::default();
    let loader = Arc::new(BuiltinProgram::new_loader(config));
    let function_registry = FunctionRegistry::default();

    let executable = Executable::<TestContextObject>::new_from_text_bytes(text_bytes, loader.clone(), SBPFVersion::V2, function_registry)
        .map_err(|e| anyhow::anyhow!("Failed to create executable from text bytes: {}", e))?;

    executable.verify::<RequisiteVerifier>()
        .map_err(|e| anyhow::anyhow!("Failed to verify executable: {}", e))?;

    let mut context_object = TestContextObject::new(u64::MAX);
    let sbpf_version = executable.get_sbpf_version();

    let mut stack = AlignedMemory::<{ebpf::HOST_ALIGN}>::zero_filled(executable.get_config().stack_size());
    let mut heap = AlignedMemory::<{ebpf::HOST_ALIGN}>::with_capacity(0);

    let regions: Vec<MemoryRegion> = vec![
        executable.get_ro_region(),
        MemoryRegion::new_writable(
            stack.as_slice_mut(),
            ebpf::MM_STACK_START,
        ),
        MemoryRegion::new_writable(heap.as_slice_mut(), ebpf::MM_HEAP_START),
    ];

    let memory_mapping = MemoryMapping::new(regions, executable.get_config(), sbpf_version)
        .map_err(|e| anyhow::anyhow!("Failed to create memory mapping: {}", e))?;

    let mut vm = EbpfVm::new(loader, sbpf_version, &mut context_object, memory_mapping, stack.len());
    let (instruction_count, result) = vm.execute_program(&executable, true);

    println!("Program executed in {} instructions", instruction_count);
    println!("Program returned: {:?}", result);

    Ok(())
}
