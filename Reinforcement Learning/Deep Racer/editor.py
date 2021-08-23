
import math 
import numpy as np


def reward_function(params):
    # Read input parameters
    track_width = params['track_width']
    distance_from_center = params['distance_from_center']
    speed = params["speed"]
    heading = params["heading"]
    all_wheels_on_track = params['all_wheels_on_track']
    steering = abs(params['steering_angle']) 
    progress = params['progress']
    waypoints = params['waypoints']
    closest_waypoints = params['closest_waypoints']
    heading = params['heading']
    reward = 1
       
    def get_angle(p0, p1, p2):
        v0 = np.array(p0) - np.array(p1)
        v1 = np.array(p2) - np.array(p1)

        angle = np.math.atan2(np.linalg.det([v0, v1]), np.dot(v0, v1))
        return np.degrees(angle)
    
    # calculate center line with square root 

     # Give higher reward if the car is closer to center line and vice versa
   # reward += 1 - math.sqrt(distance_from_center / (track_width/2))
   

    
    #speed
    fast = 0.8
    medium =0.5
    slow = 0.1
    vertical = 80
    vertical_2 = 100
    invvertical = -80
    invvertical_2= -100
    horizontal = -10
    horizontal_2 = 10

    
    
     # Reward if the agent stays inside the two borders of the track
    if all_wheels_on_track and (0.5 * track_width - distance_from_center) >= 0.05:
        reward += 0.5
    else:
        reward += -0.2

    
 
    next_point = waypoints[closest_waypoints[1]]
    prev_point = waypoints[closest_waypoints[0]]
    next_point_2 = waypoints[closest_waypoints[0] + 1]

    # Calculate the direction in radius, arctan2(dy, dx), the result is (-pi, pi) in radians
    track_direction = math.atan2(next_point[1] - prev_point[1], next_point[0] - prev_point[0])
    # Convert to degree
    track_direction = math.degrees(track_direction)

    # Calculate the difference between the track direction and the heading direction of the car
    direction_diff = abs(track_direction - heading)
    if direction_diff > 180:
        direction_diff = 360 - direction_diff

    # Penalize the reward if the difference is too large
    DIRECTION_THRESHOLD = 10.0
    if direction_diff > DIRECTION_THRESHOLD:
        malus=1-(direction_diff/50)
        if malus<0 or malus>1:
            malus = 0
        reward *= malus

    angle = get_angle(p0 = prev_point, p1 = next_point, p2 = next_point_2)
    #increase speed on straight lines
    if (angle <= vertical_2 and angle >= vertical) or (angle <= invvertical and angle >= invvertical_2) or (angle <= horizontal_2 and angle >= horizontal):
        if speed >= fast:
            reward += 0.8
        elif speed >=  medium and speed < fast:
            reward += 0.3
        else:
            reward += -0.1 
    # Steering penality threshold, change the number based on your action space setting
    if angle < 100:
        ABS_STEERING_THRESHOLD = 20
    else :
        ABS_STEERING_THRESHOLD = 15

    # Penalize reward if the agent is steering too much
    if steering > ABS_STEERING_THRESHOLD:
        reward *= 0.9

         #Give reward for track progress
    if progress == 100:    
        reward += 10
    elif progress > 75:
        reward += 2

    return float(reward)

