# Design Note: Directive Name Variants — Leave in Place, Canonicalize Going Forward

**Date:** 2026-07-17  
**Status:** Decision recorded  

---

## Background

SWB2 accepts multiple spelling/formatting variants for many control file directives (e.g., `MODIFIED_GROWING_DEGREE_DAY`, `MODIFIED_GROWING_DEGREE-DAY`, `SIMPLE_GROWING_DEGREE_DAY`, `SIMPLE`). This was originally a user-friendliness choice inspired by PERL's "there's more than one way to do it" philosophy.

In practice, this approach:
- Makes parsing code harder to read (long `.strapprox.` chains)
- Creates ambiguity about what the "real" name is
- Makes documentation harder (which variant do you document?)
- Gives a false sense that *any* reasonable variant works, when only the ones explicitly coded are accepted
- Adds no functional value — users still have to look something up; they just have multiple names to remember

## Decision

**Do not refactor existing variant acceptance.** The cost of auditing every parsing chain, choosing canonical names, deprecating old ones, and updating all existing control files is high, and the functional benefit is zero. Existing control files in the wild would break.

**Going forward, apply these rules:**

1. **One canonical name per directive.** New directives get exactly one accepted string. Document that string.
2. **Old variants keep working silently.** No deprecation warnings, no removal. They're harmless.
3. **The directives registry (when implemented) lists only canonical names.** Users see one clear option.
4. **New code uses only the canonical form** in examples, tests, and documentation.

The ambiguity resolves itself over time through documentation without any breaking change.
