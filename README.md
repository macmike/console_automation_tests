**Rules:**

1. No external binaries or tools other than those directly invoked (bash, cmd, keygen etc.)
2. Try not to infect this file too much, I don't care about this app or gui, I want a class that does the interactions and raises events. This is just a test stub.
3. No modifications are allowed to the test commands (e.g. extra switches to redirect input/output without prior agreement)
4. No inclusion of units or libraries that have *any* license restrictions. The solution should be free to share, sell and exploit by anyone without restrictions or credits.
5. Although not all tests are aimed at all platforms, the same class should handle all requests so that client code does not need to add different units and use different classes for different platforms (although obviously command text will be different).

**Notes:**

I'd expect some code to create a process executer class, send command details and respond to events. I don't require programmatic test passing and parsing. Seeing the expected output in the memo, and being able to interact via edtSendInput is all that's required.