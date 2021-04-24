# frozen_string_literal: true

require "lex"
require "nkf"
require_relative "mbs_mixin"
require_relative "context"
require_relative "error_helper"

module CobolParser
  class PPLexer < Lex::Lexer
    include MbsMixin
    include Context::Helper
    include Config::Helper
    include ErrorHelper

    option :caseless
    option prefix: "pp"

    CopyInfo = Struct.new(
      :file,
      :line,
      :replacing,
      :quotation_mark,
      :buffer,
      keyword_init: true
    )

    attr_accessor :current_replace_list

    module Helper
      def ppout
        @context.pp_lexer.ppout
      end

      def ppout=(val)
        @context.pp_lexer.ppout = val
      end

      def pplex
        @context.pp_lexer.each_token
      end

      def ppopen(name, replace_list = nil)
        @context.pp_lexer.ppopen(name, replace_list)
      end

      def ppcopy(name, lib, replace_list)
        @context.pp_lexer.ppcopy(name, lib, replace_list)
      end

      def pp_set_replace_list(replace_list)
        @context.pp_lexer.current_replace_list = replace_list
      end
    end

    def initialize(context, options = {})
      super({ lint: false }.merge(options))

      @context = context
      @context.pp_lexer = self

      @newline_count = 0
      @within_comment = false
      @inside_bracket = 0
      @consecutive_quotation = false
      @quotation_mark = nil
      @last_line_1 = -1
      @last_line_2 = -1

      @current_replace_list = nil

      @text_queue = []

      @copy_stack = []
    end

    WORD = /[_0-9A-Z-]+/
    NUMRIC_LITERAL = /[+-]?[0-9,.]*[0-9]/
    ALNUM_LITERAL = /"[^\"\n]*"|'[^\'\n]*'/

    x %i[PROCESS_STATE COPY_STATE PSEUDO_STATE]

    rule(:*, /\*>.*/) {
      ppecho(" ")
    }

    rule(:*, /^\*.*/,
         :*, /^\/.*/) {
      ppecho(" ")
      ppecho(yytext) if cb_source_format != CB_FORMAT_FIXED
    }

    rule("PROCESS") { begins(:PROCESS_STATE) }

    rule_for(:PROCESS_STATE) do
      rule(/\n/) { begins(:INITIAL); unput("\n") }
      rule(/.*/) { cb_warning("PROCESS statement is ignored") }
    end

    rule("COPY") { begins(:COPY_STATE); :COPY }
    rule("INCLUDE") { begins(:COPY_STATE); :COPY }
    rule("REPLACE") { begins(:COPY_STATE); :REPLACE }

    rule_for(:COPY_STATE) do
      rule(/[,;]?\n/) { echo; self.cb_source_line += 1 }
      rule(/[,;]?[ ]+/) {} # ignore
      rule(".") { begins(:INITIAL); "." }
      rule("==") { begins(:PSEUDO_STATE); :EQEQ }
      rule("//") { begins(:PSEUDO_STATE); :EQEQ }
      rule("(") { "(" }
      rule(")") { ")" }
      rule("BY") { :BY }
      rule("IN") { :IN }
      rule("OF") { :OF }
      rule("OFF") { :OFF }
      rule("SUPPRESS") { :SUPPRESS }
      rule("PRINTING") { :PRINTING }
      rule("REPLACING") { :REPLACING }
      rule(WORD,
           NUMRIC_LITERAL,
           ALNUM_LITERAL,
           /./) { pplval.value = yytext; :TOKEN }
    end

    rule_for(:PSEUDO_STATE) do
      rule(/[,;]?\n/) { echo; self.cb_source_line += 1 }
      rule(/[,;]?[ ]+/) { pplval.value = " "; :TOKEN }
      rule("==") { begins(:COPY_STATE); :EQEQ }
      rule("//") { begins(:COPY_STATE); :EQEQ }
      rule(WORD,
           NUMRIC_LITERAL,
           ALNUM_LITERAL,
           /./) { pplval.value = yytext; :TOKEN }
    end

    rule("AUTHOR",
         "DATE-WRITTEN",
         "DATE-MODIFIED",
         "DATE-COMPILED",
         "INSTALLATION",
         "REMARKS",
         "SECURITY") {
      # these words are treated as comments
      if cb_verify(cb_author_paragraph, yytext)
        # skip comments until the end of line
        @within_comment = true
        while (c = input)
          break if c == "\n"
        end
        unput(c)
      end
    }

    rule(/EJECT\.?/,
         /SKIP1\.?/,
         /SKIP2\.?/,
         /SKIP3\.?/) {
      # these words are comments in IBM COBOL
      if cb_verify(cb_eject_statement, yytext)
        # do nothing for now
      else
        echo
      end
    }

    rule(/[,;]?\n/) { ppecho("\n"); self.cb_source_line += 1 }

    rule(/[;]?[ ]+/) { ppecho(" ") }

    rule(/[,]?[ ]+/) {
      if @inside_bracket > 0
        ppecho(", ")
      else
        ppecho(" ")
      end
    }

    rule("(") {
      @inside_bracket += 1
      ppecho("(")
    }

    rule(")") {
      @inside_bracket -= 1 if @inside_bracket > 0
      ppecho(")")
    }

    rule(WORD,
         NUMRIC_LITERAL,
         ALNUM_LITERAL,
         /./) { ppecho(yytext) }

    eof {
      c = @copy_stack.pop

      # NOTE: yy_delete_buffer (YY_CURRENT_BUFFER); unnecessary

      # Terminate at the end of all input
      if @copy_stack.empty?
        @within_comment = false
        @newline_count = 0
        @inside_bracket = 0
        @current_replace_list = nil
        @text_queue.clear
        # NOTE: copy_stack = NULL; @copy_stack is already empty
        @quotation_mark = nil
        @consecutive_quotation = false
        @last_line_1 = -1
        @last_line_2 = -1
        yyterminate
      end

      # Close the current file
      ppin.close

      # Switch to the last buffer
      self.current_replace_list = nil if c.replacing
      switch_to_buffer(c.line, c.file, c.buffer)
      @quotation_mark = c.quotation_mark
    }

    def ppopen(name, replace_list = nil)
      if @newline_count > 0
        @newline_count.times do
          ppin.ungetc("\n")
        end
        @newline_count = 0
      end

      # Open the copy file
      begin
        # NOTE: ppin = fopen (name, "rb"); ppin is current_buffer.ppin, so does not set directly
        new_ppin = File.open(name, "rb")
      rescue Exception => e
        if cb_source_file
          cb_error("%s: %s", name, e.message)
        else
          $stderr.puts(e.message) # rubocop:todo Style/StderrPuts
        end
        return false
      end

      # Add to dependency list
      cb_depend_list << name if cb_depend_file

      # Preserve the current buffer
      copy_info = CopyInfo.new(
        line: cb_source_line,
        file: cb_source_file,
        replacing: replace_list && replace_list.length > 0,
        buffer: current_buffer,
        quotation_mark: @quotation_mark
      )
      @copy_stack.push(copy_info)

      # Switch to new buffer
      self.current_replace_list = replace_list if replace_list
      switch_to_buffer(1, name, yy_create_buffer(new_ppin))

      true
    end

    def ppcopy(name, lib = nil, replace_list = nil)
      path = name
      path = File.join(lib, path) if lib
      return ppopen(path, replace_list) if File.exist?(path)

      cb_extension_list.each do |ext|
        s = path + ext
        return ppopen(s, replace_list) if File.exist?(s)
      end

      if path[0] != "/"
        cb_include_list.each do |inc|
          cb_extension_list.each do |ext|
            s = File.join(inc, path + ext)
            return ppopen(s, replace_list) if File.exist?(s)
          end
        end
      end

      cb_error("%s: %s", name, "No such file")

      false
    end

    private

    def switch_to_buffer(line, file, buffer)
      self.cb_source_line = line
      self.cb_source_file = file.gsub(/\\/, "/")

      self.current_buffer = buffer

      ppout.printf("# %d \"%s\"\n", line, cb_source_file)
    end

    # Check directives
    # This is horrible but we have to parse directives directly after the read
    # as flex buffers up input and it is then too late to use the flex parser
    def check_directive(buff)
      if cb_source_format == CB_FORMAT_FIXED
        return buff if mbs_length(buff) < 8
        return buff if mbs_slice_nth(buff, 6) != " "

        s = mbs_slice_range(buff, 7..-1)
      else
        s = buff
      end

      return buff if / *>>/ !~ s

      if / *>>D/ =~ s
        if cb_flag_debugging_line
          buff = mbs_slice_range(buff, 0..6) + s.gsub!(/( *)>>D/, "\\1   ")
        else
          buff = "\n" * @newline_count
          buff += "      *> DEBUG\n"
          @newline_count = 0
        end
        return buff
      end

      sbuff = s.scan(/^ *>>([^ ]+)? ([^ ]+)? ([^ ]+)? ([^ ]+)? ([^ ]+)?/).first
      buff = "\n" * @newline_count * "      *> DIRECTIVE\n"
      @newline_count = 0

      if sbuff.length < 2 || sbuff[0] != "SOURCE"
        cb_warning("Invalid directive - ignored")
        return buff
      end

      case buff.length
      when 2
        if sbuff[1].casecmp?("FIXED")
          self.cb_source_format = CB_FORMAT_FIXED
        elsif sbuff[1].casecmp?("FREE")
          self.cb_source_format = CB_FORMAT_FREE
        end
        return buff
      when 3
        if ["FORMAT", "IS"].include?(sbuff[1].upcase)
          if sbuff[2].casecmp?("FIXED")
            self.cb_source_format = CB_FORMAT_FIXED
          elsif sbuff[2].casecmp?("FREE")
            self.cb_source_format = CB_FORMAT_FREE
          end
          return buff
        end
      else
        if sbuff[1].casecmp?("FORMAT") && sbuff[2].casecmp?("IS")
          if sbuff[3].casecmp?("FIXED")
            self.cb_source_format = CB_FORMAT_FIXED
          elsif sbuff[3].casecmp?("FREE")
            self.cb_source_format = CB_FORMAT_FREE
          end
          return buff
        end
      end
      cb_warning("Invalid directive - ignored")

      buff
    end

    # Read line.
    def ppinput
      continuation = false
      while (buff = ppin.gets)
        break if buff[-1] != "\n"

        buff = NKF.nkf("-w", buff)
        buff = " " + buff if cb_source_format != CB_FORMAT_FIXED && buff[0] != " " && buff[0] != "\n"
        buff.gsub!(/\r\n/, "\n")
        if buff.index("\t")
          n = 0
          buff = buff.chars.map { |c|
            n += 1
            n += 1 if !c.ascii_only?
            if c == "\t"
              num_spaces = 1
              num_spaces += (cb_tab_width - (n % cb_tab_width)) % cb_tab_width
              " " * num_spaces
            else
              c
            end
          }.join
        end

        buff = check_directive(buff)

        # nothing more to do with free format
        return buff if cb_source_format != CB_FORMAT_FIXED

        # line too short
        if mbs_length(buff) < 8
          @newline_count += 1
          next
        end

        if cb_flag_mfcomment && (buff[0] == "*" || buff[0] == "/")
          @newline_count += 1
          next
        end

        # check the indicator (column 7)
        bp = mbs_slice_range(buff, 7..-1)
        case mbs_slice_nth(buff, 6)
        when " "
        when "-"
          continuation = true
        when "d", "D"
          # debugging line
          if !cb_flag_debugging_line
            @newline_count += 1
            next
          end
        when "*", "/"
          # comment line
          @newline_count += 1
          next
        else
          # invalid indicator
          cb_error("Invalid indicator '%c' at column 7", buff[6])
          return nil
        end

        # skip comments that follow after AUTHORS, etc.
        if @within_comment
          # Check all of "Area A"
          case mbs_length(buff)
          when 8..10
            if /^ +$/ =~ mbs_slice_range(buff, 7..-1)
              @newline_count += 1
              next
            end
          else
            @within_comment = false
          end
        end

        # check the text that is longer than cb_text_column
        if mbs_length(buff) > cb_text_column + 1
          # show warning if it is not whitespaces
          if cb_warn_column_overflow && @last_line_2 < cb_source_line - 1
            if /[^ \n]/ =~ mbs_slice_range(buff, cb_text_column..-1)
              cb_warning("Source text after column %d", cb_text_column)
            end
            # remove it
            buff = mbs_slice_nth_length(buff, 0, cb_text_column)
          end
        end

        # skip blank lines
        if /^[ \n]*$/ =~ mbs_slice_range(buff, 7..-1)
          @newline_count += 1
          next
        end

        if continuation
          # line continuation
          bp.gsub!(/^ +/, "")

          # validate concatenation
          if @consecutive_quotation
            if bp[0] == @quotation_mark && bp[1] == @quotation_mark
              bp = bp[1..-1]
            else
              cb_error("Invalid line continuation")
              return nil
            end
            @quotation_mark = nil
            @consecutive_quotation = false
          elsif @quotation_mark
            # literal concatenation
            if bp[0] == @quotation_mark
              bp = bp[1..-1]
            else
              cb_error("Invalid line continuation")
              return nil
            end
          end
        else
          # normal line
          @quotation_mark = nil
          @consecutive_quotation = false
        end

        # check if string literal is to be continued
        i = mbs_length(buff) - mbs_length(bp)
        bp.chars.each do |c|
          if ["'", '"'].include?(c)
            if !@quotation_mark
              # literal start
              @quotation_mark = c
            elsif @quotation_mark == c
              if i >= cb_text_column - 1
                # consecutive quotation
                @consecutive_quotation = true
              else
                # literal end
                @quotation_mark = nil
              end
            end
          end
          i += mbs_length(c)
        end

        buff = bp

        # truncate trailing spaces, including the newline
        if @quotation_mark
          buff += " " * (cb_text_column - mbs_length(buff))
        else
          buff.chomp!
          buff.gsub!(/ +$/, "")
          buff += " " if buff[-1] == "'" || buff[-1] == '"'
        end

        if continuation
          @newline_count += 1
        else
          # insert newlines at the start of the buffer
          buff = "\n" * @newline_count + buff
          @newline_count = 1
        end

        return buff
      end

      return nil if @newline_count == 0

      s = "\n" * @newline_count
      @newline_count = 0
      s
    end

    alias_method :yy_input, :ppinput

    def ppecho(text)
      if @text_queue.empty? && text[0] == " " || text[0] == "\n"
        ppout.print(text)
      elsif !@current_replace_list
        while (t = @text_queue.shift)
          ppout.print(t)
        end
        ppout.print(text)
      else
        # Do replacement
        @text_queue.push(text)

        @current_replace_list.each do |r|
          queue = @text_queue.each
          flag = false
          matched = true
          buff = nil
          r.old_text.each do |l|
            next if [" ", "\n"].include?(l)

            while [" ", "\n"].include?(queue.peek)
              begin
                queue.next
              rescue StopIteration
                return # partial match # rubocop:disable Lint/NonLocalExitFromIterator
              end
            end

            if !l.casecmp?(queue.peek)
              if queue.peek.include?(l)
                buff = queue.peek.sub(/#{Regexp.quote(l)}/, r.new_text.first)
                flag = true
              else
                matched = false
                break
              end
            end

            queue.next rescue nil
          end

          if matched
            r.new_text.each do |l|
              if flag
                ppout.print(buff)
              else
                ppout.print(l)
              end
            end

            begin
              @text_queue = []
              while (x = queue.next)
                @text_queue << x
              end
            rescue StopIteration
            end
          end
        end

        # no match
        while (t = @text_queue.shift)
          ppout.print(t)
        end
      end
    end
  end
end
