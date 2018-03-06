(* Sortowanie topologiczne
Mikolaj Grzywacz 394321
CR: Jan Kobalski *)

exception Cykliczne

(* kolor wierzkolka grafu*
    Bialy - jeszcze nie ruszony 
    Szary - przetwarzany 
    Czarny - przetworzony *)
type color =
    | Bialy
    | Szary
    | Czarny

(* tworzy mape gdzie bylismy w grafie
klucz - wierzcholek, wartosc - kolor *)
let create_visited l =
    let rec helper_visited lista acc =
        match lista with
            | (a,b)::t -> helper_visited t (PMap.add a Bialy acc)
            | [] -> acc
    in
    helper_visited l PMap.empty


(* tworzy graf
klucz - wierzcholek, wartosc - lista wierzcholkow do ktorych prowadzi *)
let create_graph l =
    let rec helper_graph lista acc =
        match lista with
            | (a,b)::t -> helper_graph t (PMap.add a b acc)
            | [] -> acc
    in
    helper_graph l PMap.empty

let topol lista =
    let graf = ref (create_graph lista) 
    and visited = ref (create_visited lista)
    and result = ref [] (* lista wynikowa *)
    and holder = ref Czarny in (* trzyma wynik z finda na mapie *)

    (* bierze wierzcholek ustawia go na szaro, i iteruje liste polaczen od siebie *)
    (* 'a -> unit *)
    let rec dfs a =
        begin
        visited := (PMap.add a Szary (!visited));

        List.iter (fun h -> 
            if (PMap.mem h (!visited)) then
            begin
                holder := PMap.find h (!visited);
                if (!holder) = Szary then (* jesli h jest bialy to adpala sie dalej *)
                    raise Cykliczne
                else 
                begin
                    if (!holder) = Bialy then (* jesli h jest szary to mamy cykl *)
                        dfs h
                end
                (* jesli h jest czarny to nic nie robi (nie idzie dalej) *)
            end
            else
            begin
                (* jesli poloczenia h nie ma w visited to znaczy ze jest to "slepa uliczka"
                i nigdzie dalej nie prowadzi (ustawiamy go na czarno dopisuemy do wyniku) *)
                visited := (PMap.add h Czarny (!visited));
                result := h::(!result)
            end
        ) (PMap.find a (!graf));
        (* wierzcholek przetworzony dodajemy go do wyniku *)
        visited := (PMap.add a Czarny (!visited));
        result := a::(!result);
        end
    in

    (* iterujemy po bialych wierzcholkach *)
    List.iter (fun (v, _) -> if (PMap.find v (!visited))=Bialy then dfs v) lista;

    !result;;