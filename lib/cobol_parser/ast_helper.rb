# frozen_string_literal: true

require "parser/current"

module CobolParser::AstHelper
  def s_(type, *children)
    Parser::AST::Node.new(type, children)
  end
end
