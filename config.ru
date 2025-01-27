#!/usr/bin/env rackup

# frozen_string_literal: true

require 'dotenv'
require 'pony'
require 'securerandom'
require 'sinatra'
require 'sinatra/flash'
require 'uri'

Dotenv.load

set :sessions, true
set :session_secret, SecureRandom.hex(64)
set :views, File.join(__dir__, 'layouts')

if ENV.fetch('RACK_ENV') == 'development'
	set :public_folder, File.join(__dir__, 'public')

	get '/' do
		send_file(File.join(__dir__, 'public', 'index.html'))
	end

	get '/make-coffee' do
		status 418
		send_file(File.join(__dir__, 'public', '418.html'))
	end
end

EMAIL_REGEX = /\A#{URI::MailTo::EMAIL_REGEXP}\z/

Dir.glob('**/*.rb').each do |rb|
	next if rb.include?('lib')

	require_relative rb
end

run Sinatra::Application
