This is the client portion of a client-server-client application for
monitoring the barking habits of a dog. We have a dog with separation anxiety issues,
so we like to monitor his barking while we're away so we can track his improvement
as we train him (and know how bad things are for our neighbors).

Eventually this app will also have a server component and a web client (and possibly
mobile client) for viewing the barking habits and trends.

This portion of the app runs on a computer in the home and makes POST requests to the
server portion whenever the dog barks (aka whenever the RMS of a windowed portion of
the audio signal surpasses a threshold).
