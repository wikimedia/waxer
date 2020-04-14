# waxer 0.9.4

* Adds `include_redirects` argument to `wx_page_views` ([issue 3](https://github.com/bearloga/waxer/issues/3))
* Internally, adds (non-expo) `wx_mediawiki_api` for any other future functions which may need to query the MediaWiki API

# waxer 0.9.3

* Adds 'project' column to the output ([issue 2](https://github.com/bearloga/waxer/issues/2))
* `wx_total_pages` output now includes 'existing_pages' and 'new_pages'
* Minor documentation updates

# waxer 0.9.2

* Adds mediarequest endpoints ([issue 1](https://github.com/bearloga/waxer/issues/1)):
  - Mediarequest counts per referer (`wx_mediareqs_referer`)
  - Mediarequest counts for a media file (`wx_mediareqs_file`)
  - Most requested files for a referer (`wx_most_requested_files`)

# waxer 0.9.1

This is the first, early release of waxer. In this release it has metrics & data for:
- **Traffic**
  - Project views count (`wx_project_views`)
  - Page views count (`wx_page_views`)
  - Unique devices count (`wx_unique_devices`)
  - Top 1000 pages by views (`wx_top_viewed_pages`)
- **Users**
  - Active editors count (`wx_active_editors`)
  - Top 100 daily editors by edits (`wx_top_editors`)
  - Newly registered users count (`wx_newly_registered_users`)
- **Content**
  - New pages count (`wx_new_pages`)
  - Total pages count (`wx_total_pages`)
  - Project edits count (`wx_project_edits`)
  - Page edits count (`wx_page_edits`)
  - Edited pages count (`wx_edited_pages`)
  - Top 100 edited pages by edits (`wx_top_edited_pages`)
