## Author: Ingo Feinerer

readMail <- tm::FunctionGenerator(function(DateFormat = "%d %B %Y %H:%M:%S", ...) {
    format <- DateFormat
    function(elem, language, id) {
        mail <- elem$content
        author <- gsub("From: ", "", grep("^From:", mail, value = TRUE))
        datetimestamp <- strptime(gsub("Date: ", "", grep("^Date:", mail, value = TRUE)),
                                  format = format,
                                  tz = "GMT")
        mid <- gsub("Message-ID: ", "", grep("^Message-ID:", mail, value = TRUE))
        origin <- gsub("Newsgroups: ", "", grep("^Newsgroups:", mail, value = TRUE))
        heading <- gsub("Subject: ", "", grep("^Subject:", mail, value = TRUE))

        # The header is separated from the body by a blank line.
        # Reference: \url{http://en.wikipedia.org/wiki/E-mail#Internet_e-mail_format}
        for (index in seq_along(mail)) {
            if (mail[index] == "")
                break
        }
        header <- mail[1:index]
        content <- mail[(index + 1):length(mail)]

        MailDocument(content, author, datetimestamp, character(0), header, heading,
                     if (length(mid)) mid else id, origin, language)
    }
})
