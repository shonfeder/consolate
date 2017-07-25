module Loop = Consolate_term.Loop

module Text_editor_composition = Composition.Make(Text_editor.Prog(Line_editor.Program))

module Text_editor = Loop(Text_editor_composition)
module Line_editor = Loop(Line_editor.Program)
module Json_config = Loop(Json_config)
module Menu_test   = Loop(Menu.Make(Menu.Test))

module Options_test = Loop(Options.Make(Options.Test))

module Browser_test = Loop(Browse.Program)
