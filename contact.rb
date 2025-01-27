# frozen_string_literal: true

get '/contact.php' do
	html = <<~HTML
		<h1>Get in touch</h1>
		<p>
			If you want a website, you've got an idea for a fun project, or
			you're interested in working together, feel free to drop me a
			line at <a href="mailto:aron@lebani.dev">aron@lebani.dev</a>.
			Alternatively, just fill out this form, and I'll be right back
			to you as soon as I can.
		</p>
		<form method="post">
			<fieldset>
				<label for="name">Name</label>
				<input id="name" name="name" />
				<label for="email">Email</label>
				<input id="email" name="email" />
				<label for="subject">Subject</label>
				<input id="subject" name="subject" />
				<label for="message">Message</label>
				<textarea id="message" name="message"></textarea>
				<% if flash[:errors] %>
					<p class="error">
						<%= flash[:errors].join('<br>') %>
					</p>
				<% end %>
				<button type="submit">Submit</button>
			</fieldset>
			<% if flash[:success] %>
				<p class="success">
					Thanks for getting in touch! I'll get back to you as soon
					as I can!  In the meantime, feel free to have a wander
					around my website and check out some of the things I've
					been up to.
				</p>
			<% end %>
		</form>
	HTML

	erb html, :layout => :default
end

post '/contact.php' do
	errors = []
	errors << 'Invalid email' unless params[:email].match?(EMAIL_REGEX)
	errors << 'Name is required' if params[:name].empty?
	errors << 'Email is required' if params[:email].empty?
	errors << 'Subject is required' if params[:subject].empty?
	errors << 'Message is required' if params[:message].empty?

	unless errors.empty?
		flash[:errors] = errors
		redirect '/contact.php'
	end

	begin
		Pony.mail(
			:to => 'aron@lebani.dev',
			:from => "#{params[:name]} <#{params[:email]}>",
			:subject => params[:subject],
			:body => params[:message],
			:via => :smtp,
			:via_options => {
				:address => ENV.fetch('SMTP_HOST'),
				:port => ENV.fetch('SMTP_PORT'),
				:user_name => ENV.fetch('SMTP_USER'),
				:password => ENV.fetch('SMTP_PASS'),
				:authentication => :plain,
				:domain => ENV.fetch('HOSTNAME')
			}
		)
		flash[:success] = true
	rescue
		flash[:errors] = ['Oops, there was an error sending the email...']
	ensure
		redirect '/contact.php'
	end
end
