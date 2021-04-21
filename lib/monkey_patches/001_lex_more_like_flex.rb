# frozen_string_literal: true

require "lex"
require "set"
require "stringio"

module Lex
  class Logger
    def initialize(path = nil)
      @logger = ::Logger.new(path || STDERR)
    end
  end

  class Lexeme
    attr_accessor :bol

    def ==(other)
      if @name
        @name == other.name
      else
        @pattern == other.pattern
      end
    end

    def bol?
      @bol = @pattern.source[0] == "^" if @bol.nil?
      @bol
    end
  end

  class Lexer
    class RuleDSL
      attr_reader :options,
                  :before_scans,
                  :eof_actions

      def initialize
        @state_info     = { initial: :inclusive }
        @state_ignore   = { initial: '' }  # Ignored characters for each state
        @state_error    = {} # Error conditions for each state
        @state_re       = Hash.new { |hash, name| hash[name] = {}} # Regexes for each state
        @state_names    = {} # Symbol names for each state
        @state_lexemes  = Hash.new { |hash, name| hash[name] = State.new(name) }
        @lex_tokens     = []  # List of valid tokens
        @logger         = Lex::Logger.new
        @options        = {}
        @current_states = nil
        @before_scans    = []
        @eof_actions    = []
      end

      # Specify lexing option
      #
      # @param [Symbol, Hash<Symbol, String>] arg
      #   If arg is Symbol, arg is the option name.
      #   If arg is Hash<Symbol, String>, arg's key is the option name, arg's value is the option value.
      #
      # @api public
      def option(arg)
        case arg
        when Hash
          @options.merge!(arg)
        when Symbol
          @options[arg] = true
        else
          complain("Invalid option: #{arg.inspect}")
        end
      end

      # Add inclusive states to lexer
      #
      # @param [Symbol, Array<Symbol>] names
      #   an array of inclusive state names
      #
      # @api public
      def inclusive_states(names)
        @state_info.merge!(Array(names).to_h { |n| [n, :inclusive] })
      end

      alias_method :s, :inclusive_states

      # Add exclusive states to lexer
      #
      # @param [Symbol, Array<Symbol>] names
      #   an array of exclusive state names
      #
      # @api public
      def exclusive_states(names)
        @state_info.merge!(Array(names).to_h { |n| [n, :exclusive] })
      end

      alias_method :x, :exclusive_states

      # Add states to lexer
      #
      # @param [Symbol, Array<Symbol>] names
      #   an array of state names for specify lexing rule
      #
      # @api public
      def rule_for(names, &block)
        complain("Already specified '#{@current_states.join(', ')}' states") if @current_states

        @current_states = Array(names)
        begin
          block.call
        ensure
          @current_states = nil
        end
      end

      # Specify lexing rule
      #
      # @param [Symbol] name
      #   the rule name
      #
      # @param [Regex] pattern
      #   the regex pattern
      #
      # @api public
      def rule(*args, &action)
        return rule_name_pattern_action(*args, &action) if !action || action.arity == 2

        complain("Specify an action") if !action

        state_names_pattern = []
        states = @current_states || []
        args.each do |arg|
          case arg
          when Symbol
            complain("Already specified '#{@current_states.join(', ')}' states") if @current_states

            name = arg
            if name == :*
              states = @state_info.keys
            else
              states << name
            end
          when Regexp, String
            if arg.is_a?(Regexp)
              re = arg
            else
              re = Regexp.new(Regexp.quote(arg))
            end
            re = /#{re.source}/i if @options[:caseless]

            if states.empty?
              states = [:initial]
            else
              states.uniq!
              if states.include?(:INITIAL)
                states.delete(:INITIAL)
                states << :initial
              end
            end
            state_names_pattern << [states, re]

            states = @current_states || []
          end
        end

        complain("Specify a pattern") if state_names_pattern.empty?

        case action.arity
        when 0
          action_0 = action
          action_2 = ->(lexer, token) {
            lexer.instance_exec(&action_0)
          }
        else
          complain("Specified action takes an invalid number of arguments")
        end
        action = ->(lexer, token) {
          result = action_2.call(lexer, token)
          case result
          when Symbol
            token.name = result
            token
          when String
            token.name = result
            token.value = result
            token
          when Token
            result
          else
            nil
          end
        }

        state_names_pattern.each do |state_names, pattern|
          state_names.each do |state_name|
            state = @state_lexemes[state_name]
            state << Lexeme.new(nil, pattern, &action)
          end
        end

        update_inclusive_states
      end

      # Specify action when the begining of scan each tokens
      #
      # @api public
      def before_scan(&action)
        @before_scans << action if action
      end

      # Specify action when the EOF is reached
      #
      # @api public
      def eof(&action)
        @eof_actions << action if action
      end

      private

      # @api private
      # :nocov:
      def rule_name_pattern_action(name, pattern, &action)
        state_names, token_name = *extract_state_token(name)
        if token_name =~ /^[[:upper:]]*$/ && !@lex_tokens.include?(token_name)
          complain("Rule '#{name}' defined for" \
            " an unspecified token #{token_name}")
        end
        state_names.each do |state_name|
          state = @state_lexemes[state_name]
          state << Lexeme.new(token_name, pattern, &action)
        end
        update_inclusive_states
        state_names.each do |state_name|
          if @state_re[state_name].key?(token_name)
            complain("Rule '#{name}' redefined.")
          end
          @state_re[state_name][token_name] = pattern
        end
      end
      # :nocov:
    end
  end


  class Lexer
    # from flex's texinfo:
    # https://github.com/westes/flex/blob/04c5b7c9209801aa1bdbf279ccdcde0d57874a55/doc/flex.texi#L2988
    BE_ALIASED_METHODS = %w[
      yy_create_buffer
      yy_delete_buffer
      yy_flex_debug
      yy_init_buffer
      yy_flush_buffer
      yy_load_buffer_state
      yy_switch_to_buffer
      yyin
      yyleng
      yylex
      yylineno
      yyout
      yyrestart
      yytext
      yywrap
      yyalloc
      yyrealloc
      yyfree
    ] + %w[
      yylval
      yyterminate
    ]

    SPECIAL_BE_ALIASED_METHODS = {
      in: :input_file,
      out: :output_file,
      switch_to_buffer: :current_buffer=,
      text: :current_value,
      lval: :current_token,
    }

    class TerminateException < RuntimeError; end

    class Buffer
      extend Forwardable

      def_delegators :@scanner, *StringScanner.public_instance_methods(false)

      attr_accessor :input_file

      def initialize(input_file)
        @input_file = input_file
        @scanner = StringScanner.new("")
      end
    end

    class << self
      # @api private
      def define_flex_methods(prefix = "yy")
        public_methods = Set.new(public_instance_methods)
        protected_methods = Set.new(protected_instance_methods)
        private_methods = Set.new(private_instance_methods)
        methods = public_methods + protected_methods + private_methods

        BE_ALIASED_METHODS.each do |name|
          orig_name = name.to_s.sub(/^yy_?/, "").to_sym
          orig_name = SPECIAL_BE_ALIASED_METHODS[orig_name] if SPECIAL_BE_ALIASED_METHODS.key?(orig_name)
          prefixed_name = name.to_s.sub(/^yy/, prefix).to_sym

          [
            [orig_name, prefixed_name],
            [:"#{orig_name}=", :"#{prefixed_name}="],
          ].each do |orig, prefixed|
            next if !methods.include?(orig) || methods.include?(prefixed)

            define_method(prefixed) do |*args|
              send(orig, *args)
            end
            protected(prefixed) if protected_methods.include?(orig)
            private(prefixed) if private_methods.include?(orig)
          end
        end
      end

      # Hack for define_flex_methods for `option prefix: "pp"`
      #
      # @api private
      def option(arg)
        define_flex_methods(arg[:prefix]) if arg.is_a?(Hash) && arg.key?(:prefix)
        dsl.option(arg)
      end
    end

    attr_reader :current_token,
                :current_value

    attr_accessor :current_buffer,
                  :output_file

    def_delegators :@dsl,
                   :options,
                   :prefix,
                   :before_scans,
                   :eof_actions

    def_delegators :@current_buffer,
                   :input_file

    def initialize(options = {}, &block)
      @unput_strings = []

      rewind

      @logger           = Lex::Logger.new(options[:log_path])
      @linter           = Lex::Linter.new
      @dsl              = self.class.dsl
      @debug            = options[:debug] || self.options[:debug]
      @output_file      = StringIO.new

      @dsl.instance_eval(&block) if block
      @linter.lint(self) if !options.key?(:lint) || options[:lint]
    end

    # Tokenizes input and returns all tokens
    #
    # @param [String] input_string
    #
    # @return [Enumerator]
    #   the tokens found
    #
    # @api public
    # :nocov:
    def lex(input_string)
      return enum_for(:lex, input_string) unless block_given?

      if debug
        logger.info "lex: tokens   = #{@dsl.lex_tokens}"
        logger.info "lex: states   = #{@dsl.state_info}"
        logger.info "lex: ignore   = #{@dsl.state_ignore}"
        logger.info "lex: error    = #{@dsl.state_error}"
      end

      stream_tokens(input_string) do |token|
        yield token
      end
    end
    # :nocov:

    # Advances through input and streams tokens
    #
    # @param [String] input_string
    #
    # @yield [Lex::Token]
    #
    # @api public
    # :nocov:
    def stream_tokens(input_string, &block)
      input_file = StringIO.new(input_string)
      self.current_buffer = create_buffer(input_file)
      each_token(&block)
    end
    # :nocov:

    # Tokenizes input and yield each token
    #
    # @return [Enumerator]
    #   the tokens found
    #
    # @api public
    def each_token(&block)
      return enum_for(:each_token) if !block_given?

      begins(:initial)

      called_eof_actions = false

      loop do
        while input_string = (respond_to?(:yy_input, true) ? yy_input : read)
          if debug
            if input_string.length > 100
              s = input_string[0, 100].inspect + "..."
            else
              s = input_string.inspect
            end
            logger.info "lex: input_string = #{s}"
          end

          called_eof_actions = false

          current_buffer.string = input_string
          each_token_in_current_buffer(&block)
        end

        break if called_eof_actions

        eof_actions.each do |action|
          instance_exec(&action)
        end

        called_eof_actions = true
      end
    rescue TerminateException
      logger.info "lex: terminated" if debug
    end

    def begin(state)
      state = :initial if state.to_s.casecmp?("initial")

      unless @dsl.state_info.key?(state)
        complain("Undefined state: #{state}")
      end
      @current_state = state

      logger.info "lex: [#{current_state}]: lexemes = #{@dsl.state_lexemes[current_state].map { |x| x.name || x.pattern }}" if debug

      nil
    end

    alias_method :begin_state, :begin
    alias_method :begins, :begin

    def rewind
      @current_line     = 1
      @current_pos      = 1 # Position in input
      @char_pos_in_line = 0
      @current_state    = :initial
      @state_stack      = []
      @unput_strings.clear
      @current_token    = nil
      @current_value    = nil
    end

    # Gets a character from the input stream
    #
    # @return [String]
    #
    # @api public
    def input
      if @unput_strings.empty?
        current_buffer.getch
      else
        unput_string = @unput_strings.reverse.join
        @unput_strings.clear
        @unput_strings << unput_string[1..-1]
        unput_string[0]
      end
    end

    # Puts the string back onto the input stream
    #
    # @param [String] str
    #
    # @api public
    def unput(str)
      @unput_strings << str if str

      nil
    end

    # Output current_value token, like flex's ECHO
    #
    # @api public
    def echo
      output_file.print(current_value)
    end

    # Terminate lexer
    #
    # @api public
    def terminate
      raise TerminateException
    end

    # Create buffer
    #
    # @param [IO] input_file
    #
    # @return [Buffer]
    #
    # @api public
    def create_buffer(input_file)
      Buffer.new(input_file)
    end

    # Read input string
    #
    # @return [String]
    #
    # @api public
    def read
      return nil if input_file.eof?

      input_file.read
    end

    private

    # @api private
    def each_token_in_current_buffer(&block)
      executed_before_scans = false
      while !current_buffer.eos?
        if !executed_before_scans
          before_scans.each do |action|
            instance_exec(&action)
          end
          executed_before_scans = true
        end

        current_char = current_buffer.peek(1)
        if @dsl.state_ignore[current_state]&.include?(current_char)
          current_buffer.pos += current_char.bytesize
          @char_pos_in_line += current_char.size
          next
        end

        # Look for regex match
        longest_token = nil
        @dsl.state_lexemes[current_state].each do |lexeme|
          next if lexeme.bol? && !current_buffer.bol?
          match = lexeme.match(current_buffer)
          next if match.nil?
          longest_token = match if longest_token.nil?
          next if longest_token.value.bytesize >= match.value.bytesize
          longest_token = match
        end

        @current_token = longest_token
        @current_value = @current_token&.value&.dup

        if longest_token
          longest_token_value_length = longest_token.value.length
          current_buffer.pos += @current_value.bytesize
          if longest_token.action
            new_token = longest_token.action.call(self, longest_token)
            # No value returned from action move to the next token
            if new_token.nil? || !new_token.is_a?(Token)
              chars_to_skip = longest_token_value_length
              unless longest_token.name == :newline
                @char_pos_in_line += chars_to_skip
              end
              num_newlines = @current_value.encode("utf-8", invalid: :replace, undef: :replace).count("\n")
              advance_line(num_newlines) if num_newlines > 0
              next
            end
          end
          start_char_pos_in_token = @char_pos_in_line + current_char.size
          longest_token.update_line(current_line, start_char_pos_in_token)
          advance_column(longest_token_value_length)
          num_newlines = @current_value.encode("utf-8", invalid: :replace, undef: :replace).count("\n")
          advance_line(num_newlines) if num_newlines > 0
        end

        # No match
        if longest_token.nil?
          # Check in errors
          if @dsl.state_error[current_state]
            token = Token.new(:error, current_char)
            start_char_pos_in_token = @char_pos_in_line + current_char.size
            token.update_line(current_line, start_char_pos_in_token)
            new_token = @dsl.state_error[current_state].call(self, token)
            advance_column(current_char.length)
            advance_line(1) if current_char == "\n"
            current_buffer.pos += current_char.bytesize
            if new_token.is_a?(Token) || !new_token.nil?
              longest_token = new_token
            else
              next
            end
          end

          if longest_token.nil?
            complain("Illegal character #{current_char.inspect}")
          end
        end

        logger.info "lex: #{longest_token.to_ary.inspect}" if debug
        block.call(longest_token)
        executed_before_scans = false

        if @unput_strings.length > 0
          @current_buffer.string = @unput_strings.reverse.join + @current_buffer.rest
          @unput_strings.clear
        end
      end
    end

    define_flex_methods
  end
end
