from numpy.random import default_rng
rng = default_rng()

def transition(p, state):
    if rng.uniform() <= p:
        state[0] += 1
    else:
        state[1] += 1
    if state[0] > state[1]:
        return [0, 0]
    return state

def simulate(p, confs):
    # valid vs. invalid blocks
    state = [0, 0]
    n = 1000000
    badevents = 0
    for i in range(n):
        state = transition(p, state)
        if state[1] >= confs:
            badevents += 1
    print("Probability that unupdated node is on >= %s block invalid fork with %s%% threshold: %.2f%%" % (confs, p *100, badevents/n *100))

simulate(0.95, 2)
simulate(0.95, 3)
simulate(0.9, 2)
simulate(0.9, 3)
