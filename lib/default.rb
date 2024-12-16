module Helpers
  def breadcrumb_title(item)
    case item.path
    when '/'
      'home'
    else
      File.basename(item.path, '.html')
    end
  end
end

include Nanoc::Helpers::Blogging
include Nanoc::Helpers::XMLSitemap
include Nanoc::Helpers::Breadcrumbs
include Helpers
