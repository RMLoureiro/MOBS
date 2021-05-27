type id = int

type t = {
    id: id;
    region : Network.region;
    out_links: Network.links
}

let init id out_links region = 
    {
        id;
        region;
        out_links
    }

let print node =
    print_string "ID: "; print_string (string_of_int node.id); print_string " | ";
    print_string "REGION: "; print_string (string_of_int node.region);  print_string " | ";
    print_string "LINKS: "; List.iter (fun x -> print_string (string_of_int x); print_string ",") node.out_links; print_endline ""
