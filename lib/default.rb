module Helpers
  def breadcrumb_title(item)
    if item.path == '/'
      'home'
    else
      File.basename item.path, '.html'
    end
  end
end

include Nanoc::Helpers::Blogging
include Nanoc::Helpers::XMLSitemap
include Nanoc::Helpers::Breadcrumbs
include Helpers
