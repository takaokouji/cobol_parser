# frozen_string_literal: true

require_relative "lib/cobol_parser/version"

Gem::Specification.new do |spec|
  spec.name          = "cobol_parser"
  spec.version       = CobolParser::VERSION
  spec.authors       = ["Kouji Takao"]
  spec.licenses      = ["GPL-2.0-or-later", "MIT"]
  spec.email         = ["kouji.takao@gmail.com"]

  spec.summary       = "cobol_parser is a COBOL parser written in pure ruby"
  spec.description   =
    "cobol_parser is a COBOL parser written in pure ruby. It outputs\n" \
    "s-expressions which like whitequark/parser's outputs."
  spec.homepage      = "https://github.com/takaokouji/cobol_parser"
  spec.required_ruby_version = Gem::Requirement.new(">= 3.0.0")

  spec.metadata["homepage_uri"] = spec.homepage
  spec.metadata["source_code_uri"] = "https://github.com/takaokouji/cobol_parser"
  spec.metadata["changelog_uri"] = "https://github.com/takaokouji/cobol_parser/blob/main/CHANGELOG.md"

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  spec.files = Dir.chdir(File.expand_path(__dir__)) do
    `git ls-files -z`.split("\x0").reject { |f| f.match(%r{\A(?:test|spec|features)/}) }
  end
  spec.bindir        = "exe"
  spec.executables   = spec.files.grep(%r{\Aexe/}) { |f| File.basename(f) }
  spec.require_paths = ["lib"]

  spec.add_dependency "lex", "~> 0.1.0"
  spec.add_dependency "parser", "~> 3.0.1"
  spec.add_dependency "unparser", "~> 0.6.0"

  spec.add_development_dependency "rake", "~> 13.0"
  spec.add_development_dependency "rubocop", "~> 1.12.1"
  spec.add_development_dependency "rubocop-rake", "~> 0.5.1"
  spec.add_development_dependency "simplecov", "~> 0.21.2"
  spec.add_development_dependency "test-unit", "~> 3.4.0"
  spec.add_development_dependency "test-unit-rr", "~> 1.0.5"
end
