# frozen_string_literal: true

require "bundler/gem_tasks"
require "rake/testtask"
require "rubocop/rake_task"

RuboCop::RakeTask.new(:rubocop) do |task|
  task.options = ENV["RUBOCOPOPTS"].split if ENV["RUBOCOPOPTS"]
end

Rake::TestTask.new do |t|
  t.libs << "test"
  t.test_files = Dir["test/**/*_test.rb"]
end

rule %r{\.rb} => ".ry" do |t|
  sh "racc", "-v", "-o", t.name.to_s, t.source.to_s
end

desc "Generate parser by racc"
task racc: %w[lib/cobol_parser/pp_parser.rule.rb lib/cobol_parser/parser.rule.rb]

task default: %i[racc rubocop test]
