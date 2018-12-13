import random
import numpy as np
import time

def simulate_monopoly(dice_size, turn):
    """ simulate n-turn monopoly game with d-sides dice.
    """

    #b2b function to reset card
    def b2b(now, end):
        if now >= end:
            now = 0
        else:
            now += 1
        return now

    def row_dice(dice_size):
        dice1 = random.randint(1,dice_size)
        dice2 = random.randint(1,dice_size)
        return dice1, dice2

    #define special box
    CC = [2, 17, 33]
    CH = [7, 22, 36]

    Community_Chest = random.sample([0,10], 2)
    CC_turn = 0
    Chance = random.sample([0,10,11,24,39,35,'RR','RR','UT','Min3'], 10)
    Chance_turn = 0

    step_records = []
    current_step = 0
    for i in range(turn):
        #row dice and move
        row_count = 0
        while True:
            row_count += 1
            #some null object to initial define step
            step = 0
            dice = row_dice(dice_size)
            if dice[0] != dice[1]:
                step = dice[0] + dice[1]
                break
            if row_count == 3:
                current_step = 10
                break

        current_step += step
        if current_step > 39:
            current_step = current_step % 41

        #handle case when in G2J
        if current_step == 30:
            step_records.append(current_step)
            current_step = 10

        #handle case when in community chest
        if current_step in CC:
            step_records.append(current_step)
            current_step = Community_Chest[CC_turn]
            CC_turn = b2b(CC_turn, 1)

        #handle case when in chance
        if current_step in CH:
            step_records.append(current_step)
            if Chance[Chance_turn] == 'RR':
                if current_step == 7:
                    current_step = 15
                elif current_step == 22:
                    current_step = 25
                elif current_step == 36:
                    current_step = 5
            elif Chance[Chance_turn] == 'UT':
                if current_step == 7:
                    current_step = 12
                elif current_step == 22:
                    current_step = 28
                elif current_step == 36:
                    current_step = 12
            elif Chance[Chance_turn] == 'Min3':
                current_step += (-3)
            else:
                current_step = Chance[Chance_turn]
            Chance_turn = b2b(Chance_turn, 9)

        step_records.append(current_step)

    return step_records

def estimate_monopoly(dice_size, turn):
    """Calculate the probability of each box
    """
    result = simulate_monopoly(dice_size, turn)
    return [round(result.count(i)/len(result), 5) for i in range(40)]

############################################################
################## I am a separation line###################
############################################################

#simulation
start_sim = time.time()
sim_1000 = [estimate_monopoly(6, 10000) for i in range(1000)]
sim_jail = [i[10] for i in sim_1000]

sim_se = np.std(sim_jail)
sim_se
end_sim = time.time()


#bootstrap
start_boot = time.time()
boot = simulate_monopoly(6, 10000)
boot_result = [[boot[i] for i in random.choices(list(range(1000)), k=1000)] for i in range(1000)]

boot_se = np.std([sum([1 if i == 10 else 0 for i in j])/1000 for j in boot_result])
boot_se
end_boot = time.time()

sim_time = end_sim - start_sim
sim_boot = end_boot - start_boot
sim_time
sim_boot
#boot strap is 10 times faster
