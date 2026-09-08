// Appended only to separately built study stubs by environment-study.py.
// Observe the common action stream without participating in environment storage.

#[derive(Clone, Copy)]
struct ProbeActivation {
    id: usize,
    layout: usize,
    words: usize,
}

struct ProbeSuspension {
    activation: ProbeActivation,
    captures: usize,
}

struct EnvironmentProbe {
    active: Option<ProbeActivation>,
    pending: Vec<ProbeSuspension>,
    entries: usize,
    suspensions: usize,
    resumptions: usize,
    captured_words: usize,
    restored_words: usize,
    suspended_layout_words: usize,
    pending_words: usize,
    peak_pending_words: usize,
    peak_pending: usize,
    same_owner_suspensions: usize,
    collections: usize,
    root_slots: usize,
    peak_root_slots: usize,
}

impl EnvironmentProbe {
    const EMPTY: Self = Self {
        active: None,
        pending: Vec::new(),
        entries: 0,
        suspensions: 0,
        resumptions: 0,
        captured_words: 0,
        restored_words: 0,
        suspended_layout_words: 0,
        pending_words: 0,
        peak_pending_words: 0,
        peak_pending: 0,
        same_owner_suspensions: 0,
        collections: 0,
        root_slots: 0,
        peak_root_slots: 0,
    };

    fn step(&mut self, action: &Action<Word>) {
        use zydeco_machine::frames::ActionKind;
        match action.kind as u64 {
            | kind if kind == ActionKind::Enter as u64 => {
                self.entries += 1;
                self.active = Some(ProbeActivation {
                    id: self.entries,
                    layout: action.layout,
                    words: action.words,
                });
            }
            | kind if kind == ActionKind::Suspend as u64 => {
                let active = self.active.unwrap();
                assert_eq!(active.layout, action.layout);
                self.same_owner_suspensions +=
                    usize::from(self.pending.iter().any(|saved| saved.activation.id == active.id));
                self.pending.push(ProbeSuspension { activation: active, captures: action.words });
                self.suspensions += 1;
                self.captured_words += action.words;
                self.suspended_layout_words += active.words;
                self.pending_words += action.words;
                self.peak_pending_words = self.peak_pending_words.max(self.pending_words);
                self.peak_pending = self.peak_pending.max(self.pending.len());
            }
            | kind if kind == ActionKind::Resume as u64 => {
                let saved = self.pending.pop().unwrap();
                assert_eq!(saved.activation.layout, action.layout);
                self.active = Some(saved.activation);
                self.resumptions += 1;
                self.restored_words += saved.captures;
                self.pending_words -= saved.captures;
            }
            | _ => panic!("unexpected environment probe action"),
        }
    }

    fn roots(&mut self, slots: usize) {
        self.collections += 1;
        self.root_slots += slots;
        self.peak_root_slots = self.peak_root_slots.max(slots);
    }

    fn report(&self) {
        eprintln!(
            concat!(
                "zydeco_transitions: {{\"entries\":{},\"suspensions\":{},\"resumptions\":{},",
                "\"captured_words\":{},\"restored_words\":{},\"suspended_layout_words\":{},",
                "\"peak_pending_capture_words\":{},\"peak_pending_suspensions\":{},",
                "\"same_owner_suspensions\":{},\"collections\":{},",
                "\"root_slots\":{},\"peak_root_slots\":{}}}"
            ),
            self.entries,
            self.suspensions,
            self.resumptions,
            self.captured_words,
            self.restored_words,
            self.suspended_layout_words,
            self.peak_pending_words,
            self.peak_pending,
            self.same_owner_suspensions,
            self.collections,
            self.root_slots,
            self.peak_root_slots,
        );
    }
}

static ENVIRONMENT_PROBE: RuntimeCell<EnvironmentProbe> = RuntimeCell::new(EnvironmentProbe::EMPTY);
