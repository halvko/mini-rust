use std::io;

pub fn store(ty: &str, from: &str, to: &str, o: &mut impl io::Write) -> anyhow::Result<()> {
    writeln!(o, "call void @store_{ty}(ptr %mm, {ty} {from}, {ty}* {to})")?;
    Ok(())
}

pub fn load(ty: &str, to: &str, from: &str, o: &mut impl io::Write) -> anyhow::Result<()> {
    writeln!(o, "{to} = call {ty} @load_{ty}(ptr %mm, {ty} {from})")?;
    Ok(())
}
