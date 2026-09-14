struct Compact;
impl zydeco_utils::fold::Driver for Compact {
    fn run<F: zydeco_utils::fold::Folder>(folder: &mut F, input: F::Input) -> F::Output {
        use zydeco_utils::fold::Step;
        enum State<I,O> { Enter(I), Return(O) }
        let mut frames=Vec::new();
        let mut state=State::Enter(input);
        loop {
            let step=match state {
                State::Enter(input)=>folder.enter(input),
                State::Return(output)=>match frames.pop() {
                    Some(frame)=>folder.resume(frame,output),
                    None=>return output,
                },
            };
            state=match step {
                Step::TailCall(input)=>State::Enter(input),
                Step::Call{input,frame}=>{frames.push(frame);State::Enter(input)},
                Step::Return(output)=>State::Return(output),
            };
        }
    }
}
