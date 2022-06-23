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

  let concat a b =
    b @ a 

  let rec boucle nombre_de_fois liste =
    if nombre_de_fois = 0 then
      []
    else
     let liste_repeter = boucle (nombre_de_fois - 1) liste in
     concat liste liste_repeter

  let make taille =
    List.init taille (fun i -> Silence)

  let every periode fn liste =
    List.mapi (fun position valeur -> 
      if position mod periode = 0 then
        fn valeur
      else
        valeur)
      liste




  let pause bpm t =
    let duree_battement = 60. /. (Float.of_int bpm) in 
    let duree_pause = duree_battement /. 4. in
    Unix.sleepf ((Float.of_int t) *. duree_pause)



  



  let instruction sequence =
    List.fold_left 
      (fun (temps, instructions) evenement -> 
        match evenement with
        | Silence -> (temps+1, instructions)
        | Note n -> 
          let code =
            Note.code n.octave n.note n.diese in
          let message_on = Event.create ~status:(Codes.on Synth1) ~data1:code ~data2:'\090' ~timestamp:0l in 
          let message_off = Event.create ~status:(Codes.off Synth1) ~data1:code ~data2:'\090' ~timestamp:0l in
        (temps+1, (temps, message_on) :: (temps + 1, message_off) :: instructions)
        | Drum d ->
          let code =
            Drum.code d in
          let message_on = Event.create ~status:(Codes.on Drum) ~data1:code ~data2:'\090' ~timestamp:0l in 
          let message_off = Event.create ~status:(Codes.off Drum) ~data1:code ~data2:'\090' ~timestamp:0l in
        (temps+1, (temps, message_on) :: (temps + 1, message_off) :: instructions)
        )
        (0, [])
        sequence
    |> snd

    let merge_assoc l1 l2 =
      let rec aux l1 l2 c =
        match l1, l2 with
      | ((t1, e1) :: r1), ((t2, e2) ::r2) ->
        (
          if t1=t2 then
            aux r1 r2 ((t1, e1 @ e2)::c)
          else if t1<t2 then
            aux r1 l2 ((t1, e1)::c)
          else
            aux l1 r2 ((t2, e2)::c)
        )
      | [], ((t2, e2) ::r2) ->
        aux l1 r2 ((t2, e2)::c)
      | ((t1, e1) :: r1), [] ->
        aux r1 l2 ((t1, e1)::c)
      | [], [] -> c 
      in
      List.rev (aux l1 l2 [])
    let fusion instructions =
      let rec aux l resultat =
        match l with
        | [] -> resultat
        | e1 :: r -> aux r (merge_assoc resultat (List.map (fun (t, n)-> t, [n]) e1))
      in
      aux instructions [] 
         
  let rec executer bpm instrument instructions =
      match instructions with
      | [] -> ()
      | [temps, message] -> 
        let _result = Portmidi.write_output instrument  message in ()
      | (temps1, message1) :: (temps2, message2) :: reste ->
        let _result = Portmidi.write_output instrument  message1 in 
        pause bpm (temps2 - temps1);
        executer bpm instrument ((temps2, message2) :: reste)

  let jouer sequences bpm instrument =
    let instructions = List.map instruction sequences |> List.rev in
    let total = fusion instructions in
    executer bpm instrument total
end

let seq2 = [
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
|> Sequence.boucle 1

let rapide =
  Sequence.make 4
  |> Sequence.every 2 (fun i -> Sequence.Drum 3)

let plus_rapide =
  Sequence.make 4
  |> Sequence.every 1 (fun i -> Sequence.Drum 3)

let seq =
  Sequence.make 8
  |> Sequence.every 4 (fun i -> Sequence.Drum 3)
  |> Sequence.concat rapide
  |> Sequence.concat plus_rapide
  |> Sequence.boucle 10 


let () = 
let _ = Portmidi.initialize () in 
print_endline "initialize";
let device = Portmidi.open_output ~device_id:1 ~buffer_size:0l ~latency:0l in 
let instrument = 
  match device with
  | Ok instrument -> print_endline "instrument"; instrument
  | Error _ -> print_endline "erreur"; assert false
in
Sequence.jouer [seq] 120 instrument