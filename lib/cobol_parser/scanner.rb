# frozen_string_literal: true

require "lex"
require_relative "context"
require_relative "error_helper"
require_relative "tree"
require_relative "tree_helper"

module CobolParser
  class Scanner < Lex::Lexer
    include Context::Helper
    include ErrorHelper
    include TreeHelper

    option :caseless

    attr_accessor :lev78
    attr_accessor :in_procedure
    attr_accessor :cb_force_pid_literal
    attr_accessor :last_token_is_dot
    attr_accessor :integer_is_label
    attr_accessor :inside_bracket
    attr_accessor :inside_repository

    def initialize(context, options = {})
      super({ lint: false }.merge(options))

      @context = context
      @context.scanner = self

      @lev78 = []
      @in_procedure = false
      @cb_force_pid_literal = false
      @last_token_is_dot = false
      @integer_is_label = false
      @inside_bracket = 0
      @inside_repository = false
    end

    s %i[DECIMAL_IS_PERIOD DECIMAL_IS_COMMA]
    x %i[PICTURE_STATE FUNCTION_STATE]

    before_scan {
      if current_program
        if current_program.decimal_point == "."
          begins(:DECIMAL_IS_PERIOD)
        else
          begins(:DECIMAL_IS_COMMA)
        end
      end

      # We treat integer literals immediately after '.' as labels;
      # that is, they must be level numbers or section names.
      @integer_is_label = false
      if @last_token_is_dot
        @integer_is_label = true
        @last_token_is_dot = false
      end
    }

    rule(:*, /\n/) {
      self.cb_source_line += 1
    }

    rule(/^\#.*/) {
      # line directive
      if (md = /^\# (\d+) "(.*)"$/.match(yytext))
        self.cb_source_line = md[1].to_i
        self.cb_source_file = md[2]
      end

      nil
    }

    rule("PIC",
         "PICTURE") {
      begins(:PICTURE_STATE)
    }

    rule("FUNCTION") {
      if @inside_repository
        :FUNCTION
      else
        begins(:FUNCTION_STATE)
      end
    }

    rule("DIVISION") {
      @inside_repository = false
      :DIVISION
    }

    rule("PROGRAM-ID") {
      @inside_repository = false
      @cb_force_pid_literal = true
      :PROGRAM_ID
    }

    rule("FUNCTION-ID") {
      @inside_repository = false
      @cb_force_pid_literal = true
      :FUNCTION_ID
    }

    rule("REPOSITORY") {
      @inside_repository = true
      :REPOSITORY
    }

    rule(/[\'\"]/) {
      # string literal
      @cb_force_pid_literal = false
      read_literal(yytext[0])
    }

    rule(/X\'[^\'\n]*\'/,
         /X\"[^\"\n]*\"/) {
      # X string literal
      @cb_force_pid_literal = false
      scan_x(yytext)
    }

    rule("(") {
      @inside_bracket += 1
      "("
    }

    rule(")") {
      @inside_bracket -= 1 if @inside_bracket > 0
      ")"
    }

    rule(/[0-9]+/) {
      @cb_force_pid_literal = false
      if @integer_is_label
        # integer label
        yylval.value = cb_build_reference(yytext)
        set_location(yylval.value)
        :WORD
      else
        # numeric literal
        scan_numeric(yytext)
      end
    }

    rule(:*, /[ \t]+/) {
      # Ignore
    }

    rule(:*, /;+/) {
      if @inside_bracket
        :SEMI_COLON
      else # rubocop:disable Style/EmptyElse
        nil # Ignore
      end
    }

    rule(:DECIMAL_IS_PERIOD, /[+-]?[0-9.]*[0-9]+/) {
      # numeric literal
      scan_numeric(yytext)
    }

    rule(:DECIMAL_IS_PERIOD, /,+/) {
      if @inside_bracket
        :COMMA_DELIM
      else # rubocop:disable Style/EmptyElse
        nil # Ignore
      end
    }

    rule(:DECIMAL_IS_COMMA, /[+-]?[0-9]+[,]?[0-9]+/) {
      # numeric literal
      scan_numeric(yytext)
    }

    rule(:DECIMAL_IS_COMMA, /[+-]?,[0-9]+/) {
      # numeric literal
      scan_numeric(yytext)
    }

    rule(:DECIMAL_IS_COMMA, /[+-]?[0-9]+/) {
      # numeric literal
      scan_numeric(yytext)
    }

    rule(:DECIMAL_IS_COMMA, ",,") {
      unput(",")
    }

    rule(:DECIMAL_IS_COMMA, ",") {
      if @inside_bracket
        :COMMA_DELIM
      else # rubocop:disable Style/EmptyElse
        nil # Ignore
      end
    }

    rule(/H\'[^\'\n]*\'/,
         /H\"[^\"\n]*\"/) {
      # H numeric literal
      @cb_force_pid_literal = false
      scan_h(yytext)
    }

    rule(/END[ \t\n]+PROGRAM/) {
      @cb_force_pid_literal = true
      count_lines(yytext)
      :END_PROGRAM
    }

    rule(/END[ \t\n]+FUNCTION/) {
      @cb_force_pid_literal = true
      count_lines(yytext)
      :END_FUNCTION
    }

    rule(/NEXT[ \t\n]+SENTENCE/) {
      count_lines(yytext)
      :NEXT_SENTENCE
    }

    rule(/SCREEN[ \t\n]+CONTROL/) {
      count_lines(yytext)
      :SCREEN_CONTROL
    }

    rule(/BLANK[ \t\n]+SCREEN/) {
      count_lines(yytext)
      :BLANK_SCREEN
    }

    rule(/CONTROL[ \t\n]+IS/) {
      count_lines(yytext)
      :CONTROL
    }

    rule(/CONTROLS[ \t\n]+ARE/) {
      count_lines(yytext)
      :CONTROLS
    }

    rule(/CONTROL[ \t\n]+HEADING/) {
      count_lines(yytext)
      :CONTROL_HEADING
    }

    rule(/CONTROL[ \t\n]+FOOTING/) {
      count_lines(yytext)
      :CONTROL_FOOTING
    }

    rule(/PAGE[ \t\n]+HEADING/) {
      count_lines(yytext)
      :PAGE_HEADING
    }

    rule(/PAGE[ \t\n]+FOOTING/) {
      count_lines(yytext)
      :PAGE_FOOTING
    }

    rule(/REPORT[ \t\n]+HEADING/) {
      count_lines(yytext)
      :REPORT_HEADING
    }

    rule(/REPORT[ \t\n]+FOOTING/) {
      count_lines(yytext)
      :REPORT_FOOTING
    }

    rule(/LAST[ \t\n]+DETAIL/) {
      count_lines(yytext)
      :LAST_DETAIL
    }

    rule(/LAST[ \t\n]+DE/) {
      count_lines(yytext)
      :LAST_DETAIL
    }

    rule("LIMIT") {
      # Ignore
    }

    rule("LIMITS") {
      # Ignore
    }

    rule(/NO[ \t\n]+ADVANCING/) {
      count_lines(yytext)
      :NO_ADVANCING
    }

    rule(/NOT[ \t\n]+ON[ \t\n]+SIZE[ \t\n]+ERROR[ \t\n]/,
         /NOT[ \t\n]+SIZE[ \t\n]+ERROR[ \t\n]/) {
      count_lines(yytext)
      :NOT_SIZE_ERROR
    }

    rule(/ON[ \t\n]+SIZE[ \t\n]+ERROR[ \t\n]/,
         /SIZE[ \t\n]+ERROR[ \t\n]/) {
      count_lines(yytext)
      :SIZE_ERROR
    }

    rule(/NOT[ \t\n]+ON[ \t\n]+EXCEPTION[ \t\n]/,
         /NOT[ \t\n]+EXCEPTION[ \t\n]/) {
      count_lines(yytext)
      :NOT_EXCEPTION
    }

    rule(/ON[ \t\n]+EXCEPTION[ \t\n]/,
         /EXCEPTION[ \t\n]/) {
      count_lines(yytext)
      :EXCEPTION
    }

    rule(/NOT[ \t\n]+ON[ \t\n]+OVERFLOW[ \t\n]/,
         /NOT[ \t\n]+OVERFLOW[ \t\n]/) {
      count_lines(yytext)
      :NOT_OVERFLOW
    }

    rule(/NOT[ \t\n]+AT[ \t\n]+END[ \t\n]/,
         /NOT[ \t\n]+END[ \t\n]/) {
      count_lines(yytext)
      :NOT_END
    }

    rule(/AT[ \t\n]+END[ \t\n]/) {
      count_lines(yytext)
      :END
    }

    rule(/ON[ \t\n]+OVERFLOW[ \t\n]/,
         /OVERFLOW[ \t\n]/) {
      count_lines(yytext)
      :OVERFLOW
    }

    rule(/NOT[ \t\n]+AT[ \t\n]+END-OF-PAGE[ \t\n]/,
         /NOT[ \t\n]+AT[ \t\n]+EOP[ \t\n]/,
         /NOT[ \t\n]+END-OF-PAGE[ \t\n]/,
         /NOT[ \t\n]+EOP[ \t\n]/) {
      count_lines(yytext)
      :NOT_EOP
    }

    rule(/AT[ \t\n]+END-OF-PAGE[ \t\n]/,
         /AT[ \t\n]+EOP[ \t\n]/,
         /END-OF-PAGE[ \t\n]/,
         /EOP[ \t\n]/) {
      count_lines(yytext)
      :EOP
    }

    rule(/NOT[ \t\n]+INVALID[ \t\n]+KEY[ \t\n]/) {
      count_lines(yytext)
      :NOT_INVALID_KEY
    }

    rule(/NOT[ \t\n]+INVALID[ \t\n]/) {
      count_lines(yytext)
      :NOT_INVALID_KEY
    }

    rule(/INVALID[ \t\n]+KEY[ \t\n]/) {
      count_lines(yytext)
      :INVALID_KEY
    }

    rule(/INVALID[ \t\n]/) {
      count_lines(yytext)
      :INVALID_KEY
    }

    rule(/UPON[ \t\n]+ENVIRONMENT-NAME/) {
      count_lines(yytext)
      :UPON_ENVIRONMENT_NAME
    }

    rule(/UPON[ \t\n]+ENVIRONMENT-VALUE/) {
      count_lines(yytext)
      :UPON_ENVIRONMENT_VALUE
    }

    rule(/UPON[ \t\n]+ARGUMENT-NUMBER/) {
      count_lines(yytext)
      :UPON_ARGUMENT_NUMBER
    }

    rule(/UPON[ \t\n]+COMMAND-LINE/) {
      count_lines(yytext)
      :UPON_COMMAND_LINE
    }

    rule(/ID[ ]+DIVISION[ ]*./,
         /IDENTIFICATION[ ]+DIVISION[ ]*./) {
      # Ignore
    }

    rule(/SWITCH[ ]+1/) {
      yylval.value = cb_build_reference("SWITCH-1")
      set_location(yylval.value)
      :WORD
    }

    rule(/SWITCH[ ]+2/) {
      yylval.value = cb_build_reference("SWITCH-2")
      set_location(yylval.value)
      :WORD
    }

    rule(/SWITCH[ ]+3/) {
      yylval.value = cb_build_reference("SWITCH-3")
      set_location(yylval.value)
      :WORD
    }

    rule(/SWITCH[ ]+4/) {
      yylval.value = cb_build_reference("SWITCH-4")
      set_location(yylval.value)
      :WORD
    }

    rule(/SWITCH[ ]+5/) {
      yylval.value = cb_build_reference("SWITCH-5")
      set_location(yylval.value)
      :WORD
    }

    rule(/SWITCH[ ]+6/) {
      yylval.value = cb_build_reference("SWITCH-6")
      set_location(yylval.value)
      :WORD
    }

    rule(/SWITCH[ ]+7/) {
      yylval.value = cb_build_reference("SWITCH-7")
      set_location(yylval.value)
      :WORD
    }

    rule(/SWITCH[ ]+8/) {
      yylval.value = cb_build_reference("SWITCH-8")
      set_location(yylval.value)
      :WORD
    }

    rule(/[A-Z0-9]([_A-Z0-9-]*[A-Z0-9]+)?/) {
      # Check word length
      cb_warning("User defined name must be less than 32 characters:%s", yytext) if yytext.length > 31

      # Check FUNCTION name without keyword
      if @in_procedure && functions_are_all
        cbp = cb_lookup_intrinsic(yytext, true)
        if cbp
          yylval.value = cb_build_reference(yytext)
          set_location(yylval.value)

          next function_name_to_token_name(yytext)
        end
      end

      # Check reserved word
      token = lookup_reserved_word(yytext)
      if token
        yylval.value = nil
        next token
      end

      @lev78.each do |p78|
        if yytext.casecmp?(p78.name)
          if non_const_word
            cb_error("CONSTANT (78 level) may not be used here - '%s'", yytext)
            yylval.value = cb_error_node
            next :WORD
          end
          yylval.value = p78.values
          next :LITERAL
        end
      end

      # User word
      if @cb_force_pid_literal
        # Force PROGRAM-ID / END PROGRAM
        @cb_force_pid_literal = false
        yylval.value = cb_build_alphanumeric_literal(yytext)
        set_location(yylval.value)
        next :PROGRAM_NAME
      end

      yylval.value = cb_build_reference(yytext)
      set_location(yylval.value)

      # Special name handling
      word = yylval.value.word
      if word.count > 0
        x = word.items.value
        next :MNEMONIC_NAME if x.is_a?(CobolParser::Tree::SystemName)
      end

      :WORD
    }

    rule("<=") {
      yylval.value = nil
      :LE
    }

    rule(">=") {
      yylval.value = nil
      :GE
    }

    rule("<>") {
      yylval.value = nil
      :NE
    }

    rule("**") {
      yylval.name = "^"
      yylval.value = nil

      yylval
    }

    rule(".") {
      @last_token_is_dot = true
      yylval.name = "."
      yylval.value = nil

      yylval
    }

    rule(/./) {
      yylval.name = yytext[0]
      yylval.value = nil

      yylval
    }

    rule_for(:PICTURE_STATE) do
      rule("IS") {
        # ignore
      }

      rule(/[^ \t\n;]+/) {
        begins(:INITIAL)
        scan_picture(yytext)
      }
    end

    rule_for(:FUNCTION_STATE) do
      rule(/[a-z0-9-]+/) {
        begins(:INITIAL)

        yylval.value = cb_build_reference(yytext)
        set_location(yylval.value)

        function_name_to_token_name(yytext)
      }

      rule(/./) {
        yylval.name = yytext[0]
        yylval.value = nil

        yylval
      }
    end

    eof {
      @last_token_is_dot = false
      @integer_is_label = false
      @inside_bracket = false
      @inside_repository = false
      @lev78.clear
      @cb_force_pid_literal = 0
      yyterminate
    }

    def check_level_78(name)
      @lev78.detect { |x| name.casecmp?(x.name) }
    end

    private

    # #define SET_LOCATION(x)
    def set_location(tree) # rubocop:disable Naming/AccessorMethodName
      tree.source_file = cb_source_file
      tree.source_line = cb_source_line
    end

    def read_literal(mark)
      buff = String.new
      while (c = input)
        buff << c
        if c == mark && (c = input) != mark
          buff[-1] = ""
          unput(c)
          break
        end
      end

      if buff.empty?
        cb_warning("Alphanumeric literal has zero length")
        cb_warning("A SPACE will be assumed")
        buff = " "
      end

      yylval.value = cb_build_alphanumeric_literal(buff)
      set_location(yylval.value)
      :LITERAL
    end

    def scan_x(text)
      md = /^X["']([0-9a-f]{2}+)["']$/i.match(text)
      if md
        yylval.value = cb_build_alphanumeric_literal([md[1]].pack("H*"))
        set_location(yylval.value)
      else
        cb_error("Invalid X literal: %s", text)
        yylval.value = cb_error_node
      end
      :LITERAL
    end

    def scan_h(text)
      md = /^H["']([0-9a-f]{2}+)["']$/i.match(text)
      if md
        yylval.value = cb_build_numeric_literal(:UNSIGNED, md[1].to_i(16).to_s, 0)
        set_location(yylval.value)
      else
        cb_error("Invalid H literal: %s", text)
        yylval.value = cb_error_node
      end
      :LITERAL
    end

    def scan_numeric(text)
      # get sign
      case text[0]
      when "+"
        sign = :POSITIVE
        text[0] = ""
      when "-"
        sign = :NEGATIVE
        text[0] = ""
      else
        sign = :UNSIGNED
      end

      # get decimal point
      integer, decimal = text.split(".", 2)
      scale = decimal&.length || 0
      text = "#{integer}#{decimal}"
      cb_error("Invalid numeric literal") if text.include?(".")
      cb_error("Invalid numeric literal") if text.include?(",")

      yylval.value = cb_build_numeric_literal(sign, text, scale)
      set_location(yylval.value)
      :LITERAL;
    end

    def scan_picture(text)
      text = text.dup

      c = text[-1]
      if [".", ","].include?(c)
        unput(c)
        text[-1] = ""
      end

      text.upcase!

      @lev78.each do |p78|
        next if p78.values == cb_error_node || p78.values.value == cb_error_node

        data = p78.values.value.data
        text.gsub!(/(?:^|([^\w-]))#{Regexp.quote(p78.name)}(?:([^\w-])|$)/) {
          "#{Regexp.last_match(1)}#{data}#{Regexp.last_match(2)}"
        }
      end

      yylval.value = cb_build_picture(text)
      :PICTURE
    end

    def count_lines(text)
      self.cb_source_line += text.count("\n")
    end

    def function_name_to_token_name(function_name)
      case function_name.upcase
      when "CONCATENATE"
        :CONCATENATE_FUNC
      when "CURRENT-DATE"
        :CURRENT_DATE_FUNC
      when "UPPER-CASE"
        :UPPER_CASE_FUNC
      when "LOWER-CASE"
        :LOWER_CASE_FUNC
      when "REVERSE"
        :REVERSE_FUNC
      when "SUBSTITUTE"
        :SUBSTITUTE_FUNC
      when "SUBSTITUTE-CASE"
        :SUBSTITUTE_CASE_FUNC
      when "TRIM"
        :TRIM_FUNCTION
      when "WHEN-COMPILED"
        :WHEN_COMPILED_FUNC
      when "NUMVAL-C"
        :NUMVALC_FUNC
      when "LOCALE-DATE", "LOCALE-TIME", "LOCALE-TIME-FROM-SECONDS"
        :LOCALE_DT_FUNC
      else
        :FUNCTION_NAME
      end
    end
  end
end
