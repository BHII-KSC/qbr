% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qb_apps.R
\name{copy_app}
\alias{copy_app}
\title{Copy an app}
\usage{
copy_app(
  subdomain,
  auth,
  app_id,
  app_name,
  app_desc = NULL,
  agent = NULL,
  users_and_roles = FALSE,
  keep_data = FALSE,
  exclude_files = TRUE,
  assign_user_token = TRUE
)
}
\arguments{
\item{subdomain}{Character vector with one element. Found at the beginning of
the Quickbase URL. Realm specific.}

\item{auth}{Character vector with one element. The Quickbase authentication
scheme you are using to authenticate the request (e.g., user token).}

\item{app_id}{Character vector with one element. Unique identifier of an app.
Found in the URL of the app's homepage after /db/}

\item{app_name}{Character vector with one element. Name the app's copy.}

\item{app_desc}{Optional. Character vector with one element. Describe the
app's copy.}

\item{agent}{Optional. Character vector with one element. Describes
user/agent making API call.}

\item{users_and_roles}{Logical. If true, users will be copied along with
their assigned roles. If false, users and roles will be copied but roles
will not be assigned.}

\item{keep_data}{Logical. Whether to copy the app's data along with the
schema.}

\item{exclude_files}{Logical. If keep_data is true, whether to copy the file
attachments as well. If keep_data is false, this parameter is ignored.}

\item{assign_user_token}{Logical. Whether to add the user token used to make
this request to the new app.}
}
\value{
A list.
}
\description{
Copy an app. Provides options to copy data and users.
}
\examples{
\dontrun{
   copy_app(subdomain = "abc",
            auth = keyring::key_get("qb_example"),
            app_id = "bn9d8f78g",
            app_name = "Copy of my app",
            keep_data = TRUE)
}
}
