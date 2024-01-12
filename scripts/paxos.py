import json

results = {}

def read_json_file(file_path):
    try:
        with open(file_path, 'r') as file:
            json_array = json.load(file)
            for json_object in json_array:
                read_json_object(json_object)
            for key in results:
                if key != None:
                    print('Value: ' + str(key) + ' Reached at: ' + str(results[key]["timestamp"]) + 'ms nodes agreed before: ' + str(len(results[key]["agreed_before"])) + ' nodes agreed after:' + str(len(results[key]["agreed_after"])))
    except FileNotFoundError:
        print(f"File not found: {file_path}")
    except json.JSONDecodeError as e:
        print(f"Error decoding JSON: {e}")

def read_json_object(json_object):
    if json_object.get('kind') == 'flow-message':
        message_type = json_object.get('content').get('msg-data').get('type')
        value = json_object.get('content').get('msg-data').get('Value')
        if message_type == 'Consensus Reached':
            initialize_value(value)
            results[value]["timestamp"] = json_object.get('content').get('reception-timestamp')
        elif message_type == 'Accepted':
            initialize_value(value)
            if results[value]["timestamp"] == -1:
                results[value]["agreed_before"].add(json_object.get('content').get('begin-node-id'))
            else:
                node_id = json_object.get('content').get('begin-node-id')
                if not node_id in results[value]["agreed_before"]:
                    results[value]["agreed_after"].add(node_id)
                
            
def initialize_value(value):
    if not value in results:
        results[value] = {"timestamp": -1, "agreed_before": set(), "agreed_after": set()}


# Replace 'your_file_path.json' with the actual path to your JSON file
file_path = '/Users/loureiro/Desktop/FCT/Thesis RL/Simulators/Simulador-MOBS/MOBS/output_files/out0-1.json'
read_json_file(file_path)
