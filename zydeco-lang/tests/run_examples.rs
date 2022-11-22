#[test]
fn pure_test() -> Result<(), String> {
    use std::io::Read;
    use zydeco_lang::parse::syntax::{TCompute, ValOrComp};
    use zydeco_lang::zydeco;
    let mut buf = String::new();
    std::fs::File::open("tests/booleans.zy")
        .map_err(|e| e.to_string())?
        .read_to_string(&mut buf)
        .map_err(|e| e.to_string())?;
    match zydeco::parse_exp(&buf)? {
        ValOrComp::Comp(m) => match zydeco::typecheck_computation(&m)? {
            TCompute::Ret(_, _) => zydeco::eval_returning_computation(m)?,
            a => Err(format!("Wrong output type: {}", a))?,
        },
        _ => Err(format!("Didn't parse"))?,
    };
    Ok(())
}

#[test]
fn batch_test() -> Result<(), String> {
    use std::io::Read;
    use zydeco_lang::zydeco;
    let mut buf = String::new();
    std::fs::File::open("../zydeco/deterministic-pushdown-automaton.zydeco")
        .map_err(|e| e.to_string())?
        .read_to_string(&mut buf)
        .map_err(|e| e.to_string())?;
    let p = zydeco::parse_prog(&buf)?;
    zydeco::typecheck_prog(&p)?;

    let mut input = std::io::empty();
    let mut output = std::io::sink();
    let exit_code = zydeco::eval_virtual_prog(p, &mut input, &mut output)?;
    if exit_code != 0 {
        Err(format!("Non-zero exit code: {}", exit_code))?
    }

    Ok(())
}
