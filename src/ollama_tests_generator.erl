-module(ollama_tests_generator).
-author("Green Mice").

%% Public API
-export([
    generate_tests/1, generate_tests/2,
    generate_tests_from_file/1, generate_tests_from_file/2,
    default_config/0,
    get_env_config/0
]).

-type config() :: map().
-type generate_result() :: {ok, binary()} | {error, term()}.
-type source_code() :: string() | binary().
-type file_path() :: string() | binary().

%% =============================================================================
%% Main test generation functions
%% =============================================================================

%% @doc
%% Generate unit tests for the provided source code using environment/default config.
-spec generate_tests(source_code()) -> generate_result().
generate_tests(SourceCode) ->
    generate_tests(SourceCode, get_env_config()).

%% @doc
%% Generate unit tests for the provided source code using a custom config.
-spec generate_tests(source_code(), config()) -> generate_result().
generate_tests(SourceCode, Config) ->
    %% Ensure source code is a string
    SourceCodeStr = case SourceCode of
        Code when is_binary(Code) -> binary_to_list(Code);
        Code when is_list(Code) -> Code
    end,

    %% Detect the programming language from the source code
    Language = detect_language(SourceCodeStr),

    %% Build the prompt using the template from config or default
    PromptTemplate = maps:get(prompt_template, Config, default_prompt_template()),
    Prompt = format_prompt(PromptTemplate, [Language, Language, SourceCodeStr]),

    %% Call the handler and return its result directly
    ollama_handler:generate(Prompt, Config).

%% @doc
%% Generate unit tests from a source code file using environment/default config.
-spec generate_tests_from_file(file_path()) -> generate_result().
generate_tests_from_file(FilePath) ->
    generate_tests_from_file(FilePath, get_env_config()).

%% @doc
%% Generate unit tests from a source code file using a custom config.
-spec generate_tests_from_file(file_path(), config()) -> generate_result().
generate_tests_from_file(FilePath, Config) ->
    %% Ensure file path is a string
    FilePathStr = case FilePath of
        Path when is_binary(Path) -> binary_to_list(Path);
        Path when is_list(Path) -> Path
    end,

    %% Read the file content
    case file:read_file(FilePathStr) of
        {ok, FileContent} ->
            generate_tests(FileContent, Config);
        {error, Reason} ->
            {error, {file_read_error, Reason}}
    end.

%% =============================================================================
%% Configuration helpers
%% =============================================================================

%% @doc
%% Returns the default config (handler defaults + our default prompt template).
-spec default_config() -> config().
default_config() ->
    maps:merge(
        ollama_handler:default_config(),
        #{prompt_template => default_prompt_template()}
    ).

%% @doc
%% Returns config from environment variables, falling back to defaults.
-spec get_env_config() -> config().
get_env_config() ->
    maps:merge(
        ollama_handler:get_env_config(),
        #{prompt_template => os:getenv("OLLAMA_TESTS_PROMPT", default_prompt_template())}
    ).

%% =============================================================================
%% Internal functions
%% =============================================================================

%% @doc
%% The default prompt template for test generation.
-spec default_prompt_template() -> string().
default_prompt_template() ->
    "Generate comprehensive unit tests for the following ~s code. "
    "Include tests for all public functions, edge cases, error conditions, and typical use cases. "
    "Follow the testing conventions and best practices for ~s. "
    "Only return the test code, no explanations:\n\n~s".

%% @doc
%% Format the prompt using io_lib:format.
-spec format_prompt(string(), list(term())) -> string().
format_prompt(PromptTemplate, Args) ->
    case catch io_lib:format(PromptTemplate, Args) of
        FormattedPrompt when is_list(FormattedPrompt) -> lists:flatten(FormattedPrompt);
        _ -> "Failed to format prompt"
    end.

%% @doc
%% Detect the programming language from source code content.
-spec detect_language(string()) -> string().
detect_language(SourceCode) ->
    %% Simple language detection based on common patterns
    case detect_by_patterns(SourceCode) of
        unknown -> detect_by_keywords(SourceCode);
        Language -> Language
    end.

%% @doc
%% Detect language by file patterns and syntax.
-spec detect_by_patterns(string()) -> string() | unknown.
detect_by_patterns(SourceCode) ->
    %% Check for module declarations and specific syntax patterns
    case {
        string:find(SourceCode, "-module(") =/= nomatch,
        string:find(SourceCode, "defmodule ") =/= nomatch,
        string:find(SourceCode, "def ") =/= nomatch andalso string:find(SourceCode, "end") =/= nomatch,
        string:find(SourceCode, "function ") =/= nomatch,
        string:find(SourceCode, "class ") =/= nomatch,
        string:find(SourceCode, "public class ") =/= nomatch,
        string:find(SourceCode, "#include") =/= nomatch,
        string:find(SourceCode, "package ") =/= nomatch,
        string:find(SourceCode, "import ") =/= nomatch andalso string:find(SourceCode, "from ") =/= nomatch,
        string:find(SourceCode, "const ") =/= nomatch orelse string:find(SourceCode, "let ") =/= nomatch
    } of
        {true, _, _, _, _, _, _, _, _, _} -> "Erlang";
        {_, true, _, _, _, _, _, _, _, _} -> "Elixir";
        {_, _, true, _, _, _, _, _, _, _} -> "Ruby";
        {_, _, _, true, _, _, _, _, _, _} -> "JavaScript";
        {_, _, _, _, true, _, _, _, _, _} -> "Python";
        {_, _, _, _, _, true, _, _, _, _} -> "Java";
        {_, _, _, _, _, _, true, _, _, _} -> "C/C++";
        {_, _, _, _, _, _, _, true, _, _} -> "Go";
        {_, _, _, _, _, _, _, _, true, _} -> "Python";
        {_, _, _, _, _, _, _, _, _, true} -> "JavaScript";
        _ -> unknown
    end.

%% @doc
%% Detect language by common keywords.
-spec detect_by_keywords(string()) -> string().
detect_by_keywords(SourceCode) ->
    LowerCase = string:to_lower(SourceCode),

    %% Count occurrences of language-specific keywords
    ErlangScore = count_keywords(LowerCase, ["spawn", "receive", "gen_server", "supervisor", "application"]),
    ElixirScore = count_keywords(LowerCase, ["defp", "alias", "use", "import", "require"]),
    PythonScore = count_keywords(LowerCase, ["def", "__init__", "self", "import", "from"]),
    JavaScore = count_keywords(LowerCase, ["public", "private", "static", "void", "import"]),
    JSScore = count_keywords(LowerCase, ["var", "const", "let", "function", "require"]),

    %% Return the language with the highest score
    Scores = [
        {ErlangScore, "Erlang"},
        {ElixirScore, "Elixir"},
        {PythonScore, "Python"},
        {JavaScore, "Java"},
        {JSScore, "JavaScript"}
    ],

    case lists:keysort(1, Scores) of
        [{0, _} | _] -> "Unknown";
        SortedScores ->
            {_, Language} = lists:last(SortedScores),
            Language
    end.

%% @doc
%% Count occurrences of keywords in text.
-spec count_keywords(string(), [string()]) -> integer().
count_keywords(Text, Keywords) ->
    lists:sum([length(string:tokens(Text, Keyword)) - 1 || Keyword <- Keywords]).

