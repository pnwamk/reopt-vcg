
universes u


abbrev DecidableLess (α : Type u) [HasLess α] :=
∀ (a b : α), Decidable (a < b)

class HasLessOrder (α : Type u) extends HasLess α :=
(transitive : ∀ (x y z : α), x < y → y < z → x < z)
(asymmetric : ∀ (x y : α), x < y → ¬(y < x))
(total : ∀ (x y : α), x < y ∨ x = y ∨ y < x)


class DecidableLessOrder (α : Type u) extends HasLessOrder α :=
(ltDec : DecidableLess α)
(eqDec : DecidableEq α)


namespace Subtype
variables {α : Type u} {p : α → Prop}
instance [h : HasLess α] : HasLess {x : α // p x} :=
⟨λ ⟨a, _⟩ ⟨b, _⟩ => a < b⟩

protected theorem lt [HasLess α] : ∀ {a1 a2 : {x // p x}}, a1.val < a2.val → a1 < a2
| ⟨a, h1⟩, ⟨b, h2⟩, ltPf => ltPf

protected theorem ltInv [HasLess α] : ∀ {a1 a2 : {x // p x}}, a1 < a2 → a1.val < a2.val
| ⟨a, h1⟩, ⟨b, h2⟩, ltPf => ltPf

protected theorem ltInvNeg [HasLess α] : ∀ {a1 a2 : {x // p x}}, ¬(a1.val < a2.val) → ¬(a1 < a2)
| ⟨a, h1⟩, ⟨b, h2⟩, valNotLtPf => λ ltPf => absurd (Subtype.ltInv ltPf) valNotLtPf

instance [HasLess α] [DecidableLess α] : DecidableLess {x : α // p x} :=
fun ⟨a, h₁⟩ ⟨b, h₂⟩ =>
  if valLtPf : a < b then isTrue (Subtype.lt valLtPf)
  else isFalse (fun ltPf => absurd ltPf (Subtype.ltInvNeg valLtPf))

end Subtype


namespace LessOrder


-- FIXME prove
axiom SubtypeLessOrder {α : Type u} {p : α → Prop} [HasLessOrder α] : HasLessOrder {x : α // p x}

axiom SubtypeDecidableLessOrder {α : Type u} {p : α → Prop} [DecidableLessOrder α] : DecidableLessOrder {x : α // p x}

end LessOrder

namespace DecidableLessOrder

instance {α : Type u} [h : DecidableLessOrder α] : DecidableLess α :=
h.ltDec

instance {α : Type u} [h : DecidableLessOrder α] : DecidableEq α :=
h.eqDec


end DecidableLessOrder
