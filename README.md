## Pinboard to Pocket

Grab recent posts from Pinboard tagged "readlater" and add them to Pocket. Meant to be run on a `cron` job or such.

### Required `ENV` vars:

```
PINBOARD_API_TOKEN
POCKET_ACCESS_TOKEN
POCKET_CONSUMER_KEY
```

You can find you `PINBOARD_API_TOKEN` on [https://pinboard.in/settings/password](https://pinboard.in/settings/password)

Pocket is a bit more difficult, requiring you to create an "application" and do an OAuth Request/Reponse cycle to obtain your `POCKET_ACCESS_TOKEN` and `POCKET_CONSUMER_KEY`. You can find out more at [https://getpocket.com/developer/docs/authentication](https://getpocket.com/developer/docs/authentication). This module **does not** handle getting Pocket keys. You can read a nice tutorial on doing so [here](http://www.jamesfmackenzie.com/getting-started-with-the-pocket-developer-api/).

### Build

This project requires [Racket 6.3](https://download.racket-lang.org/) or above.

To build a distributable/exectuable on your platform of choice, simply `make build`.
