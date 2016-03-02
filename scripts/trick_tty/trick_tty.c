/**
 * Overrides the glibc function. Will always return true.
 *
 * Note: Although this should be ok for most applications it can
 * lead to unwanted side effects. It depends on the question
 * why the programm calls isatty()
 */
int isatty(int param) {
    return 1;
}
