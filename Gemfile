# frozen_string_literal: true

source 'https://rubygems.org'

gem 'adsf'
gem 'kramdown'
gem 'nanoc', '~> 4.13'
gem 'nanoc-live'
gem 'pony'
gem 'sinatra'
gem 'sinatra-flash'

require 'rbconfig'

if RbConfig::CONFIG['target_os'] =~ /(?i-mx:bsd|dragonfly)/
	gem 'rb-kqueue', '>= 0.2'
end
