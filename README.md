# cobol_parser

cobol_parser is a COBOL parser written in pure ruby. It outputs s-expressions which like [whitequark/parser](https://github.com/whitequark/parser)'s outputs. So you can translate COBOL to Ruby using [unparser](https://github.com/mbj/unparser).

The following class of cobol_parser is ported from the [open-cobol-1.1](https://sourceforge.net/projects/gnucobol/files/open-cobol/1.1/) source code.

 - CobolParser::PPLexer - [cobc/pplex.l](https://sourceforge.net/p/gnucobol/code/HEAD/tree/tags/1.1_release_open-cobol/cobc/pplex.l)
 - CobolParser::PPParser - [cobc/ppparse.y](https://sourceforge.net/p/gnucobol/code/HEAD/tree/tags/1.1_release_open-cobol/cobc/ppparse.y)
 - CobolParser::Scanner - [cobc/scanner.l](https://sourceforge.net/p/gnucobol/code/HEAD/tree/tags/1.1_release_open-cobol/cobc/scanner.l)
 - CobolParser::Parser - [cobc/parser.y](https://sourceforge.net/p/gnucobol/code/HEAD/tree/tags/1.1_release_open-cobol/cobc/parser.y)
 - CobolParser::Tree, CobolParser::TreeHelpler, CobolParser::Tree::* - [cobc/tree.c](https://sourceforge.net/p/gnucobol/code/HEAD/tree/tags/1.1_release_open-cobol/cobc/tree.c)

## License

All programs except those in lib/monkey_patches are distributed under the **GNU General Public License**.  See COPYING for details.

lib/monkey_patches/001_lex_more_like_flex.rb is distributed under the MIT License.  See [piotrmurach/lex - LICENSE.txt](https://github.com/piotrmurach/lex/blob/868ed2a8c54656771e6b9614195f28b06ebc734b/LICENSE.txt) for details.

## Installation

Add this line to your application's Gemfile:

```ruby
gem 'cobol_parser'
```

And then execute:

    $ bundle install

Or install it yourself as:

    $ gem install cobol_parser

## Usage

TODO: Write usage instructions here

## Development

After checking out the repo, run `bin/setup` to install dependencies. Then, run `rake spec` to run the tests. You can also run `bin/console` for an interactive prompt that will allow you to experiment.

To install this gem onto your local machine, run `bundle exec rake install`. To release a new version, update the version number in `version.rb`, and then run `bundle exec rake release`, which will create a git tag for the version, push git commits and the created tag, and push the `.gem` file to [rubygems.org](https://rubygems.org).

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/takaokouji/cobol_parser.
