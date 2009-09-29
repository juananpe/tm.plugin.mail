## Author: Ingo Feinerer

readMail <- tm::FunctionGenerator(function(DateFormat = "%d %B %Y %H:%M:%S", ...) {
    format <- DateFormat
    function(elem, language, id) {
        mail <- elem$content

        # The header is separated from the body by a blank line.
        # Reference: \url{http://en.wikipedia.org/wiki/E-mail#Internet_e-mail_format}
        for (index in seq_along(mail))
            if (identical(mail[index], "")) break

        header <- mail[1:index]
        content <- mail[(index + 1):length(mail)]

        author <- gsub("From: ", "", grep("^From:", header, value = TRUE))
        datetimestamp <- strptime(gsub("Date: ", "", grep("^Date:", header, value = TRUE)),
                                  format = format,
                                  tz = "GMT")
        mid <- gsub("Message-ID: ", "", grep("^Message-ID:", header, value = TRUE))
        origin <- gsub("Newsgroups: ", "", grep("^Newsgroups:", header, value = TRUE))
        heading <- gsub("Subject: ", "", grep("^Subject:", header, value = TRUE))

        MailDocument(content, author, datetimestamp, character(0), header, heading,
                     if (length(mid)) mid else id, origin, language)
    }
})
