module Event = Portmidi.Portmidi_event

module Codes = struct

  type canal = Synth1 | Synth2 | Drum

  let off = function
    | Synth1 -> '\128'
    | Synth2 -> '\129'
    | Drum -> '\137'

  let on = function
    | Synth1 -> '\144'
    | Synth2 -> '\145'
    | Drum -> '\153'

end

module Note = struct
  
  type note = Do | Re | Mi | Fa | Sol | La | Si 

  type t = {
    note: note;
    octave: int;
    diese: bool;
  }

  let code octave note  diese =
    let decalage = 12 * octave + 24 in
    let decalage_note = 
      match note with
      | Do -> 0
      | Re -> 2
      | Mi -> 4
      | Fa -> 5
      | Sol -> 7
      | La -> 9
      | Si -> 11
    in
    let decalage_diese =
      if diese then
        1
      else
        0
    in
    Char.chr (decalage + decalage_note + decalage_diese)
  
end

module Drum = struct 
  let code numero =
    if numero = 1 then
      '\060'
    else if numero = 2 then
      '\062'
    else if numero = 3 then
      '\064'
    else if numero = 4 then
      '\065'
    else
      failwith "numero invalide"
end

module Sequence = struct 

  type evenement = 
    | Note of Note.t 
    | Silence
    | Drum of int

  type t = evenement list 


  let pause bpm =
    let duree_battement = 60. /. (Float.of_int bpm) in 
    let duree_pause = duree_battement /. 4. in
    Unix.sleepf duree_pause

  let jouer_note instrument temps code bpm =
    let message = Event.create ~status:(Codes.on Synth1) ~data1:code ~data2:'\090' ~timestamp:0l in 
    let _result = Portmidi.write_output instrument  [message] in
    pause bpm;
    let message = Event.create ~status:(Codes.off Synth1) ~data1:code ~data2:'\090' ~timestamp:0l in 
    let _result = Portmidi.write_output instrument  [message] in ()

    let jouer_drum instrument temps code bpm =
      let message = Event.create ~status:(Codes.on Drum) ~data1:code ~data2:'\090' ~timestamp:0l in 
      let _result = Portmidi.write_output instrument  [message] in
      pause bpm;
      let message = Event.create ~status:(Codes.off Drum) ~data1:code ~data2:'\090' ~timestamp:0l in 
      let _result = Portmidi.write_output instrument  [message] in ()

  let jouer_evenement instrument bpm evenement  =
    match evenement with 
    | Silence -> pause bpm 
    | Note note -> jouer_note instrument 1 (Note.code note.octave note.note note.diese) bpm
    | Drum n -> jouer_drum instrument 1 (Drum.code n) bpm

  let jouer sequence bpm instrument =
    List.iter (jouer_evenement instrument bpm) sequence
end

let seq = [
  Sequence.Note {note=Do; octave=2; diese=false};
  Silence;
  Drum 1;
  Silence;
  Sequence.Note {note=Re; octave=2; diese=false};
  Silence;
  Drum 1;
  Silence;
  Sequence.Note {note=Do; octave=2; diese=true};
  Silence;
  Drum 1;
  Drum 1;
  Sequence.Note {note=Si; octave=2; diese=false};
  Silence;
  Drum 1;
  Silence
]

let () = 
let _ = Portmidi.initialize () in 
print_endline "initialize";
let device = Portmidi.open_output ~device_id:1 ~buffer_size:0l ~latency:0l in 
let instrument = 
  match device with
  | Ok instrument -> print_endline "instrument"; instrument
  | Error _ -> print_endline "erreur"; assert false
in
Sequence.jouer seq 120 instrument