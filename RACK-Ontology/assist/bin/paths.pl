:- module(paths,
          [
              paths_dir/1
          ]).

this_is_paths. % marker for obtaining the path to this file

paths_dir(Dir) :- source_file(this_is_paths, DirAndFile), file_directory_name(DirAndFile, Dir).

user:file_search_path(checks, P) :- paths_dir(D), directory_file_path(D, 'checks', P).
user:file_search_path(ontology, P) :- paths_dir(D), directory_file_path(D, 'ontology', P).
user:file_search_path(queries, P) :- paths_dir(D), directory_file_path(D, 'queries', P).
user:file_search_path(rack, P) :- paths_dir(D), directory_file_path(D, 'rack', P).
user:file_search_path(utils, P) :- paths_dir(D), directory_file_path(D, 'utils', P).
user:file_search_path(visualization, P) :- paths_dir(D), directory_file_path(D, 'visualization', P).
