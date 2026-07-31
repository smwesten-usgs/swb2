"""Fix doubled paths in integration test control files."""
from pathlib import Path

prefix = "../../test_data/cs/"
for ctl in [
    Path(r"E:\projects\swb_development\git\swb2\test\integration_tests\cs_phenology\phenology_test.ctl"),
    Path(r"E:\projects\swb_development\git\swb2\test\integration_tests\cs_interception\interception_test.ctl"),
]:
    text = ctl.read_text()
    if prefix in text:
        new_text = text.replace(prefix, "")
        ctl.write_text(new_text)
        count = text.count(prefix)
        print(f"{ctl.name}: removed {count} path prefixes")
    else:
        print(f"{ctl.name}: no changes needed")
