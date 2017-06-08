module Text_editor_composition = Composition.Make(Text_editor.Prog(Line_editor))
module Text_editor = Consolate_term.Loop(Text_editor_composition)

module Line_editor = Consolate_term.Loop(Line_editor)

module Menu_test = Consolate_term.Loop(Menu.Make(Menu.Test))
