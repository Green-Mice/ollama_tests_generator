# Ollama Tests Generator

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Erlang/OTP](https://img.shields.io/badge/Erlang-OTP%2024+-red.svg)](https://www.erlang.org/)
[![AI Powered](https://img.shields.io/badge/AI-Powered-green.svg)](https://ollama.ai/)

**The Ollama Tests Generator** is a powerful Erlang-based tool that automatically generates comprehensive unit tests for your code using AI models through Ollama. This tool supports multiple programming languages and can adapt to your existing testing frameworks and coding style, making it an invaluable addition to your development workflow.

## ✨ Features

- 🤖 **AI-Powered Test Generation**: Leverages Ollama's language models to create intelligent, comprehensive test suites
- 🌍 **Multi-Language Support**: Automatically detects and generates tests for Python, JavaScript, Java, Erlang, Elixir, Ruby, C/C++, Go, and more
- 🎯 **Smart Language Detection**: Analyzes code patterns and syntax to accurately identify programming languages
- ⚙️ **Highly Configurable**: Customizable prompts, model selection, and generation parameters
- 📁 **File and String Input**: Generate tests from source code files or directly from code strings
- 🏗️ **Framework Aware**: Generates tests following best practices for popular testing frameworks (pytest, Jest, EUnit, etc.)
- 🚀 **Edge Case Coverage**: Automatically includes tests for error conditions, boundary cases, and typical use scenarios
- 💾 **Environment Integration**: Supports configuration through environment variables

## 🚀 Quick Start

### Prerequisites

- Erlang/OTP 24 or higher
- [Ollama](https://ollama.ai/) installed and running
- A compatible language model (e.g., `codellama:7b`, `llama2:7b`)

### Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/ollama_tests_generator.git
cd ollama_tests_generator
```

2. Compile the Erlang modules:
```bash
erlc src/*.erl
```

3. Make sure Ollama is running with your preferred model:
```bash
ollama run codellama:7b
```

### Basic Usage

#### Generate Tests from Code String

```erlang
%% Start Erlang shell
erl

%% Generate tests for Python code
SourceCode = "
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)
",

{ok, Tests} = ollama_tests_generator:generate_tests(SourceCode).
```

#### Generate Tests from File

```erlang
%% Generate tests from a source file
{ok, Tests} = ollama_tests_generator:generate_tests_from_file("src/my_module.py").
```

#### Custom Configuration

```erlang
%% Use custom configuration
Config = #{
    model => "codellama:13b",
    temperature => 0.1,
    prompt_template => "Generate pytest tests for the following ~s code with focus on edge cases:\n\n~s"
},

{ok, Tests} = ollama_tests_generator:generate_tests(SourceCode, Config).
```

## 📖 API Reference

### Main Functions

#### `generate_tests/1`
```erlang
-spec generate_tests(source_code()) -> generate_result().
```
Generate unit tests using default/environment configuration.

#### `generate_tests/2`
```erlang
-spec generate_tests(source_code(), config()) -> generate_result().
```
Generate unit tests with custom configuration.

#### `generate_tests_from_file/1`
```erlang
-spec generate_tests_from_file(file_path()) -> generate_result().
```
Generate tests from a source code file using default configuration.

#### `generate_tests_from_file/2`
```erlang
-spec generate_tests_from_file(file_path(), config()) -> generate_result().
```
Generate tests from a source code file with custom configuration.

### Configuration Functions

#### `default_config/0`
```erlang
-spec default_config() -> config().
```
Returns the default configuration map.

#### `get_env_config/0`
```erlang
-spec get_env_config() -> config().
```
Returns configuration from environment variables, falling back to defaults.

## ⚙️ Configuration

### Environment Variables

- `OLLAMA_MODEL`: Model to use (default: `codellama:7b`)
- `OLLAMA_HOST`: Ollama server host (default: `localhost`)
- `OLLAMA_PORT`: Ollama server port (default: `11434`)
- `OLLAMA_TEMPERATURE`: Generation temperature (default: `0.2`)
- `OLLAMA_TESTS_PROMPT`: Custom prompt template

### Configuration Map Options

```erlang
Config = #{
    model => "codellama:7b",           % Ollama model to use
    host => "localhost",               % Ollama host
    port => 11434,                     % Ollama port  
    temperature => 0.2,                % Generation randomness (0.0-1.0)
    max_tokens => 2048,                % Maximum tokens to generate
    timeout => 30000,                  % Request timeout in milliseconds
    prompt_template => "Custom prompt" % Custom prompt template
}.
```

## 🎯 Supported Languages

The tool automatically detects the following languages:

| Language   | Detection Method | Testing Framework |
|------------|------------------|-------------------|
| Python     | Keywords + syntax | pytest, unittest |
| JavaScript | Keywords + syntax | Jest, Mocha |
| Java       | Keywords + syntax | JUnit |
| Erlang     | Module syntax | EUnit |
| Elixir     | Module syntax | ExUnit |
| Ruby       | Keywords + syntax | RSpec, Test::Unit |
| C/C++      | Include patterns | Google Test, Unity |
| Go         | Package syntax | Go testing |

## 📝 Example Usage

### Complete Example: Python Fibonacci

```erlang
%% Complete example generating tests for Fibonacci implementations
-module(fibonacci_test_example).
-export([run/0]).

run() ->
    %% Python Fibonacci code
    SourceCode = "
def fibonacci_recursive(n):
    if not isinstance(n, int):
        raise TypeError('n must be an integer')
    if n < 0:
        raise ValueError('n must be non-negative')
    if n <= 1:
        return n
    return fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2)

def fibonacci_iterative(n):
    if not isinstance(n, int):
        raise TypeError('n must be an integer')
    if n < 0:
        raise ValueError('n must be non-negative')
    if n <= 1:
        return n
    
    a, b = 0, 1
    for _ in range(2, n + 1):
        a, b = b, a + b
    return b
",

    %% Custom configuration for comprehensive testing
    Config = #{
        model => "codellama:7b",
        temperature => 0.1,
        prompt_template => 
            "Generate comprehensive pytest tests for the following ~s code. "
            "Include tests for: basic functionality, edge cases (n=0, n=1), "
            "error handling (negative inputs, wrong types), and performance comparison. "
            "Only return the test code:\n\n~s"
    },

    %% Generate tests
    case ollama_tests_generator:generate_tests(SourceCode, Config) of
        {ok, Tests} ->
            %% Save to file
            file:write_file("test_fibonacci.py", Tests),
            io:format("✅ Tests generated successfully!~n"),
            io:format("📁 Saved to: test_fibonacci.py~n");
        {error, Reason} ->
            io:format("❌ Error: ~p~n", [Reason])
    end.
```

### Run the Test Suite Example

The repository includes a comprehensive example in `test/fibonacci_test_suite.erl`:

```bash
# Compile and run the example
erlc test/fibonacci_test_suite.erl
erl -noshell -eval "fibonacci_test_suite:main()." -s init stop
```

## 🔧 Advanced Usage

### Custom Prompt Templates

Create specialized prompts for different testing scenarios:

```erlang
%% Property-based testing prompt
PropertyTestPrompt = 
    "Generate property-based tests using Hypothesis for the following ~s code. "
    "Focus on testing invariants and generating diverse input data:\n\n~s",

%% Performance testing prompt  
PerformanceTestPrompt = 
    "Generate performance benchmarks and tests for the following ~s code. "
    "Include timing tests and memory usage analysis:\n\n~s",

%% Security testing prompt
SecurityTestPrompt = 
    "Generate security-focused tests for the following ~s code. "
    "Include input validation, injection testing, and boundary checks:\n\n~s".
```

### Batch Processing

Process multiple files:

```erlang
process_directory(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    SourceFiles = [F || F <- Files, filename:extension(F) =:= ".py"],
    
    lists:foreach(fun(File) ->
        FilePath = filename:join(Dir, File),
        case ollama_tests_generator:generate_tests_from_file(FilePath) of
            {ok, Tests} ->
                TestFile = "test_" ++ filename:basename(File, ".py") ++ ".py",
                file:write_file(TestFile, Tests),
                io:format("✅ Generated tests for ~s~n", [File]);
            {error, Reason} ->
                io:format("❌ Failed to generate tests for ~s: ~p~n", [File, Reason])
        end
    end, SourceFiles).
```

## 🤝 Contributing

We welcome contributions! Please follow these steps:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

**Language detection issues**
```
Language detected as "Unknown"
```
- The code might be too short or ambiguous
- Try providing more context or specify the language manually


## 📄 License

This project is licensed under the Apache License 2.0 - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- [Ollama](https://ollama.ai/) team for providing the excellent local LLM platform
- The Erlang/OTP team for the robust runtime system
- All contributors who help improve this tool

## 📞 Support

- 📧 Email: support@greenmices.dev  
- 🐛 Issues: [GitHub Issues](https://github.com/yourusername/ollama_tests_generator/issues)
- 💬 Discussions: [GitHub Discussions](https://github.com/yourusername/ollama_tests_generator/discussions)

---

⭐ **Star this repository if you find it useful!** ⭐
