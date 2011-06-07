extern int nondet ();
extern void solveHaltingProblem (int tm);

int main () {
    solveHaltingProblem (nondet ());

    return 0;
}
