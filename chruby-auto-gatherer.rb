def chruby_auto_gatherer
  ruby_engine = Object.const_defined?(:RUBY_ENGINE) ? RUBY_ENGINE : 'ruby'
  ruby_version = RUBY_VERSION
  gem_root = begin
    require 'rubygems'
    Gem.default_dir
  rescue LoadError
    nil
  end

  [ruby_engine, ruby_version, gem_root]
end

puts chruby_auto_gatherer.join(' ')
