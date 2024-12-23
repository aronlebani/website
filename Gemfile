# frozen_string_literal: true

source 'https://rubygems.org'

gem 'nanoc', '~> 4.13'
gem 'nanoc-live'
gem 'adsf'
gem 'kramdown'

require 'rbconfig'

if RbConfig::CONFIG['target_os'] =~ /(?i-mx:bsd|dragonfly)/
	gem 'rb-kqueue', '>= 0.2'
end
