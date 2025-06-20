%%%-------------------------------------------------------------------
%%% @doc Suite de tests exemple pour ollama_tests_generator
%%% Génère des tests pour une implémentation Fibonacci en Python
%%% @end
%%%-------------------------------------------------------------------
-module(fibonacci_test_suite).
-author("Green Mice").

-export([
    run_fibonacci_test/0,
    run_all_examples/0,
    save_generated_tests/2,
    main/0
]).

%% =============================================================================
%% Code source Python - Fibonacci avec différentes implémentations
%% =============================================================================

-spec get_fibonacci_python_code() -> string().
get_fibonacci_python_code() ->
    "#!/usr/bin/env python3\n"
    "\"\"\"\n"
    "Fibonacci implementations with different approaches\n"
    "\"\"\"\n"
    "\n"
    "import functools\n"
    "import sys\n"
    "\n"
    "def fibonacci_recursive(n):\n"
    "    \"\"\"\n"
    "    Simple recursive implementation of Fibonacci sequence.\n"
    "    Args:\n"
    "        n (int): Position in the sequence (0-indexed)\n"
    "    Returns:\n"
    "        int: Fibonacci number at position n\n"
    "    Raises:\n"
    "        ValueError: If n is negative\n"
    "        TypeError: If n is not an integer\n"
    "    \"\"\"\n"
    "    if not isinstance(n, int):\n"
    "        raise TypeError(\"n must be an integer\")\n"
    "    if n < 0:\n"
    "        raise ValueError(\"n must be non-negative\")\n"
    "    if n <= 1:\n"
    "        return n\n"
    "    return fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2)\n"
    "\n"
    "@functools.lru_cache(maxsize=None)\n"
    "def fibonacci_memoized(n):\n"
    "    \"\"\"\n"
    "    Memoized recursive implementation for better performance.\n"
    "    \"\"\"\n"
    "    if not isinstance(n, int):\n"
    "        raise TypeError(\"n must be an integer\")\n"
    "    if n < 0:\n"
    "        raise ValueError(\"n must be non-negative\")\n"
    "    if n <= 1:\n"
    "        return n\n"
    "    return fibonacci_memoized(n - 1) + fibonacci_memoized(n - 2)\n"
    "\n"
    "def fibonacci_iterative(n):\n"
    "    \"\"\"\n"
    "    Iterative implementation - most efficient for large numbers.\n"
    "    \"\"\"\n"
    "    if not isinstance(n, int):\n"
    "        raise TypeError(\"n must be an integer\")\n"
    "    if n < 0:\n"
    "        raise ValueError(\"n must be non-negative\")\n"
    "    if n <= 1:\n"
    "        return n\n"
    "    \n"
    "    a, b = 0, 1\n"
    "    for _ in range(2, n + 1):\n"
    "        a, b = b, a + b\n"
    "    return b\n"
    "\n"
    "def fibonacci_sequence(length):\n"
    "    \"\"\"\n"
    "    Generate a Fibonacci sequence of given length.\n"
    "    Args:\n"
    "        length (int): Number of elements to generate\n"
    "    Returns:\n"
    "        list: List of Fibonacci numbers\n"
    "    \"\"\"\n"
    "    if not isinstance(length, int):\n"
    "        raise TypeError(\"length must be an integer\")\n"
    "    if length < 0:\n"
    "        raise ValueError(\"length must be non-negative\")\n"
    "    if length == 0:\n"
    "        return []\n"
    "    if length == 1:\n"
    "        return [0]\n"
    "    \n"
    "    sequence = [0, 1]\n"
    "    for i in range(2, length):\n"
    "        sequence.append(sequence[i-1] + sequence[i-2])\n"
    "    return sequence\n"
    "\n"
    "class Fibonacci:\n"
    "    \"\"\"\n"
    "    A class-based approach to Fibonacci calculations.\n"
    "    \"\"\"\n"
    "    \n"
    "    def __init__(self, cache_size=128):\n"
    "        self.cache = {}\n"
    "        self.cache_size = cache_size\n"
    "    \n"
    "    def calculate(self, n):\n"
    "        \"\"\"\n"
    "        Calculate Fibonacci number with instance caching.\n"
    "        \"\"\"\n"
    "        if not isinstance(n, int):\n"
    "            raise TypeError(\"n must be an integer\")\n"
    "        if n < 0:\n"
    "            raise ValueError(\"n must be non-negative\")\n"
    "        \n"
    "        if n in self.cache:\n"
    "            return self.cache[n]\n"
    "        \n"
    "        if n <= 1:\n"
    "            result = n\n"
    "        else:\n"
    "            result = self.calculate(n - 1) + self.calculate(n - 2)\n"
    "        \n"
    "        # Simple cache management\n"
    "        if len(self.cache) < self.cache_size:\n"
    "            self.cache[n] = result\n"
    "        \n"
    "        return result\n"
    "    \n"
    "    def clear_cache(self):\n"
    "        \"\"\"Clear the internal cache.\"\"\"\n"
    "        self.cache.clear()\n"
    "\n"
    "if __name__ == \"__main__\":\n"
    "    # Example usage\n"
    "    print(\"Fibonacci(10) recursive:\", fibonacci_recursive(10))\n"
    "    print(\"Fibonacci(10) iterative:\", fibonacci_iterative(10))\n"
    "    print(\"Fibonacci sequence(10):\", fibonacci_sequence(10))\n"
    "    \n"
    "    fib = Fibonacci()\n"
    "    print(\"Fibonacci(10) class-based:\", fib.calculate(10))\n".

%% =============================================================================
%% Fonctions de test principales
%% =============================================================================

%% @doc
%% Exécute la génération de tests pour le code Fibonacci Python
-spec run_fibonacci_test() -> ok.
run_fibonacci_test() ->
    io:format("=== Génération de tests pour Fibonacci Python ===~n~n"),
    
    SourceCode = get_fibonacci_python_code(),
    
    %% Configuration spécifique pour Python avec des instructions détaillées
    Config = #{
        model => "codellama:7b",
        temperature => 0.1,
        prompt_template => 
            "Generate comprehensive unit tests for the following ~s code using pytest framework. "
            "Include the following test categories:\n"
            "1. Basic functionality tests for all functions\n"
            "2. Edge cases (n=0, n=1, large numbers)\n"
            "3. Error handling tests (negative inputs, wrong types)\n"
            "4. Performance comparison tests between different implementations\n"
            "5. Class-based tests with setup and teardown\n"
            "6. Property-based tests if applicable\n"
            "Follow Python testing best practices and include fixtures where appropriate.\n"
            "Only return the complete test code:\n\n~s"
    },
    
    io:format("Code source à tester:~n"),
    io:format("~s~n", [SourceCode]),
    io:format("~n" ++ string:copies("=", 80) ++ "~n~n"),
    
    case ollama_tests_generator:generate_tests(SourceCode, Config) of
        {ok, GeneratedTests} ->
            io:format("Tests générés avec succès!~n~n"),
            io:format("~s~n", [GeneratedTests]),
            
            %% Sauvegarder les tests dans un fichier
            TestFile = "test_fibonacci.py",
            save_generated_tests(TestFile, GeneratedTests),
            
            %% Afficher quelques statistiques
            show_test_statistics(GeneratedTests),
            ok;
        {error, Reason} ->
            io:format("Erreur lors de la génération des tests: ~p~n", [Reason]),
            error
    end.

%% @doc
%% Exécute plusieurs exemples de génération de tests
-spec run_all_examples() -> ok.
run_all_examples() ->
    io:format("=== Suite complète d'exemples de génération de tests ===~n~n"),
    
    %% Exemple 1: Fibonacci Python
    io:format("1. Test Fibonacci Python:~n"),
    run_fibonacci_test(),
    
    io:format("~n" ++ string:copies("-", 80) ++ "~n~n"),
    
    %% Exemple 2: Code Erlang simple
    io:format("2. Test module Erlang simple:~n"),
    run_erlang_example(),
    
    io:format("~n" ++ string:copies("-", 80) ++ "~n~n"),
    
    %% Exemple 3: Code JavaScript
    io:format("3. Test JavaScript:~n"),
    run_javascript_example(),
    
    ok.

%% @doc
%% Exemple avec du code Erlang
-spec run_erlang_example() -> ok.
run_erlang_example() ->
    ErlangCode = 
        "-module(list_utils).\n"
        "-export([reverse/1, sum/1, max/1, filter_even/1]).\n"
        "\n"
        "reverse(List) -> reverse(List, []).\n"
        "reverse([], Acc) -> Acc;\n"
        "reverse([H|T], Acc) -> reverse(T, [H|Acc]).\n"
        "\n"
        "sum([]) -> 0;\n"
        "sum([H|T]) -> H + sum(T).\n"
        "\n"
        "max([]) -> error(empty_list);\n"
        "max([H|T]) -> max(T, H).\n"
        "max([], Max) -> Max;\n"
        "max([H|T], Max) when H > Max -> max(T, H);\n"
        "max([_|T], Max) -> max(T, Max).\n"
        "\n"
        "filter_even(List) -> [X || X <- List, X rem 2 =:= 0].\n",
    
    Config = #{
        temperature => 0.2,
        prompt_template => 
            "Generate EUnit tests for this ~s module. "
            "Include tests for all exported functions, edge cases, and error conditions. "
            "Use proper EUnit assertions and test fixtures:\n\n~s"
    },
    
    case ollama_tests_generator:generate_tests(ErlangCode, Config) of
        {ok, Tests} ->
            io:format("Tests Erlang générés:~n~s~n", [Tests]),
            save_generated_tests("test_list_utils.erl", Tests);
        {error, Reason} ->
            io:format("Erreur: ~p~n", [Reason])
    end.

%% @doc
%% Exemple avec du code JavaScript
-spec run_javascript_example() -> ok.
run_javascript_example() ->
    JSCode = 
        "class Calculator {\n"
        "  constructor() {\n"
        "    this.history = [];\n"
        "  }\n"
        "\n"
        "  add(a, b) {\n"
        "    if (typeof a !== 'number' || typeof b !== 'number') {\n"
        "      throw new Error('Arguments must be numbers');\n"
        "    }\n"
        "    const result = a + b;\n"
        "    this.history.push({operation: 'add', operands: [a, b], result});\n"
        "    return result;\n"
        "  }\n"
        "\n"
        "  divide(a, b) {\n"
        "    if (typeof a !== 'number' || typeof b !== 'number') {\n"
        "      throw new Error('Arguments must be numbers');\n"
        "    }\n"
        "    if (b === 0) {\n"
        "      throw new Error('Division by zero');\n"
        "    }\n"
        "    const result = a / b;\n"
        "    this.history.push({operation: 'divide', operands: [a, b], result});\n"
        "    return result;\n"
        "  }\n"
        "\n"
        "  getHistory() {\n"
        "    return [...this.history];\n"
        "  }\n"
        "\n"
        "  clearHistory() {\n"
        "    this.history = [];\n"
        "  }\n"
        "}\n"
        "\n"
        "module.exports = Calculator;\n",
    
    Config = #{
        temperature => 0.1,
        prompt_template => 
            "Generate Jest unit tests for this ~s class. "
            "Include tests for all methods, error conditions, and class state management. "
            "Use proper Jest syntax with describe blocks and beforeEach setup:\n\n~s"
    },
    
    case ollama_tests_generator:generate_tests(JSCode, Config) of
        {ok, Tests} ->
            io:format("Tests JavaScript générés:~n~s~n", [Tests]),
            save_generated_tests("calculator.test.js", Tests);
        {error, Reason} ->
            io:format("Erreur: ~p~n", [Reason])
    end.

%% =============================================================================
%% Fonctions utilitaires
%% =============================================================================

%% @doc
%% Sauvegarde les tests générés dans un fichier
-spec save_generated_tests(string(), binary() | string()) -> ok | {error, term()}.
save_generated_tests(FileName, Tests) ->
    TestsStr = case Tests of
        T when is_binary(T) -> binary_to_list(T);
        T when is_list(T) -> T
    end,
    
    case file:write_file(FileName, TestsStr) of
        ok ->
            io:format("✓ Tests sauvegardés dans: ~s~n", [FileName]),
            ok;
        {error, Reason} ->
            io:format("✗ Erreur lors de la sauvegarde: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc
%% Affiche des statistiques sur les tests générés
-spec show_test_statistics(binary() | string()) -> ok.
show_test_statistics(Tests) ->
    TestsStr = case Tests of
        T when is_binary(T) -> binary_to_list(T);
        T when is_list(T) -> T
    end,
    
    Lines = length(string:tokens(TestsStr, "\n")),
    TestFunctions = count_occurrences(TestsStr, "def test_"),
    Assertions = count_occurrences(TestsStr, "assert"),
    Classes = count_occurrences(TestsStr, "class Test"),
    
    io:format("📊 Statistiques des tests générés:~n"),
    io:format("   • Nombre de lignes: ~p~n", [Lines]),
    io:format("   • Fonctions de test: ~p~n", [TestFunctions]),
    io:format("   • Assertions: ~p~n", [Assertions]),
    io:format("   • Classes de test: ~p~n", [Classes]),
    ok.

%% @doc
%% Compte les occurrences d'une sous-chaîne dans un texte
-spec count_occurrences(string(), string()) -> integer().
count_occurrences(Text, Pattern) ->
    length(string:find(Text, Pattern, all)).

%% =============================================================================
%% Fonctions d'exemple pour tester le module directement
%% =============================================================================

%% @doc
%% Fonction d'entrée principale pour tester
-spec main() -> ok.
main() ->
    io:format("Démarrage des tests ollama_tests_generator...~n~n"),
    
    %% Vérifier que le module ollama_tests_generator est disponible
    case code:which(ollama_tests_generator) of
        non_existing ->
            io:format("❌ Module ollama_tests_generator non trouvé!~n"),
            io:format("Assurez-vous que le module est compilé et dans le path.~n");
        _ ->
            io:format("✓ Module ollama_tests_generator trouvé~n~n"),
            run_all_examples()
    end.
