# frozen_string_literal: true

module CobolParser::ErrorHelper
  attr_reader :warningcount
  attr_reader :errorcount

  def warning(format, *arg)
    print_error(nil, 0, "Warning: ", format, *arg)

    @warningcount += 1
  end

  def error(format, *arg)
    print_error(nil, 0, "Error: ", format, *arg)

    @errorcount += 1
  end

  def warning_x(tree, format, *arg)
    print_error(tree.source_file, tree.source_line, "Warning: ", format, *arg)

    @warningcount += 1
  end

  def error_x(tree, format, *arg)
    print_error(tree.source_file, tree.source_line, "Error: ", format, *arg)

    @errorcount += 1
  end

  def redefinition_error(x)
    w = x.word
    @cb.error_x(x, "Redefinition of '%s'", w.name)
    @cb.error_x(w.items.value, "'%s' previously defined here", w.name)
  end

  def redefinition_warning(x, y)
    w = x.word
    @cb.warning_x(x, "Redefinition of '%s'", w.name)
    if y
      @cb.warning_x(y, "'%s' previously defined here", w.name)
    else
      @cb.warning_x(w.items.value, "'%s' previously defined here", w.name)
    end
  end

  def undefined_error(x)
    s = "'#{x.name}'"
    x.each_chain do |c|
      s += " in '#{c.name}'"
    end
    @cb.error_x(x, "%s undefined", s)
  end

  def ambiguous_error(x)
    w = x.word
    return if w.error

    # display error on the first time
    s = "'#{x.name}'"
    x.each_chain do |l|
      s += " in '#{l.name}'"
    end
    @cb.error_x(x, "%s ambiguous; need qualification", s)
    w.error = true

    # display all fields with the same name
    w.items.each_chain do |l|
      s = "'#{w.name}' "
      y = l.value
      case y
      when CobolParser::Tree::Field
        y.each_parent do |p|
          s += "in '#{p.name}' "
        end
      when CobolParser::Tree::Label
        if y.section
          s += "in '#{y.section.name}' "
        end
      end
      s += "defined here"
      @cb.error_x(y, "%s", s)
    end
  end

  private

  def print_error(file, line, prefix, format, *arg)
    @last_section ||= nil
    @last_paragraph ||= nil

    file ||= @source_file
    line ||= @source_line

    if @current_section != @last_section || @current_paragraph != @last_paragraph
      if @current_paragraph&.name == "MAIN PARAGRAPH"
        $stderr.printf("%s: In paragraph '%s':\n", file, @current_paragraph.name)
      elsif @current_section&.name == "MAIN SECTION"
        $stderr.printf("%s: In section '%s':\n", file, @current_section.name)
      end
      @last_section = @current_section
      @last_paragraph = @current_paragraph
    end

    $stderr.puts("#{file}:#{line}: #{prefix}" + sprintf(format, *arg)) # rubocop:todo Style/StderrPuts
  end
end
