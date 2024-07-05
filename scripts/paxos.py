import json

results = {}
approvals = {}
errors = []
rounds = {}

def read_json_file(file_path):
    try:
        with open(file_path, 'r') as file:
            total_nodes = 100
            consensus = 0
            runtime = 0
            average_approval = []
            json_array = json.load(file)
            # Read array create data
            for json_object in json_array:
                read_json_object(json_object)
            
            # Print detailed data
            for key in results:
                    if key != None and results[key]["timestamp"] > 0:
                        consensus += + 1
                        if results[key]["timestamp"] > runtime:
                            runtime = results[key]["timestamp"]
                        agreed_before = len(results[key]["agreed_before"])
                        agreed_after = len(results[key]["agreed_after"])
                        total_agreement = agreed_before + agreed_after + 1
                        average_approval.append(total_agreement/ total_nodes)
                        proposed_at = ""

                        if results[key]["proposed"] == -1:
                            proposed_at = " THIS VALUE WAS NOT PROPOSED"
                            errors.append("VALUE: " + str(key) + " was accepted but not proposed")
                        elif results[key]["proposed"] >= results[key]["timestamp"]:
                            proposed_at = " THIS VALUE PROPOSE TIME IS INCONSISTENT"
                        else:
                            proposed_at = " This value was proposed at: " + str(results[key]["proposed"])

                        print('Value: ' + str(key) + ' Reached at: ' + str(results[key]["timestamp"]) + 'ms nodes agreed before: ' + str(agreed_before) +
                              ' nodes agreed after: ' + str(agreed_after) + ' total agreement: ' + str(total_agreement) + proposed_at)


            # Print global data
            print()
            print('================================APPROVAL DATA=====================================')
            print()
            
            for value in approvals:
                print('Node: ' + str(value) + ' approved ' + str(len(approvals[value])))

            print()
            print('================================GLOBAL DATA=====================================')
            print()
            print('No of consensus reached: ' + str(consensus))
            print('Runtime: ' + str(runtime) + 'ms')
            print('Average time per consensus: ' + str(runtime/consensus) + 'ms')
            print('Average acceptance percentage: ' + str(sum(average_approval)/consensus * 100) + '%')

            if(any(errors)):
                print()
                print('================================ERRORS=====================================')
                print()
                for error in errors:
                    print(error)
                print()

    except FileNotFoundError:
        print(f"File not found: {file_path}")
    except json.JSONDecodeError as e:
        print(f"Error decoding JSON: {e}")

def read_json_object(json_object):
    if json_object.get('kind') == 'flow-message':
        message_type = json_object.get('content').get('msg-data').get('type')
        value = json_object.get('content').get('msg-data').get('Value')
        receiver = json_object.get('content').get('end-node-id')
        round = json_object.get('content').get('N')
        initialize_and_validate(value, receiver, message_type, round)
        if message_type == 'Proposing':
            results[value]["proposed"] = json_object.get('content').get('reception-timestamp')
        elif message_type == 'Consensus Reached':
            if not value in approvals[receiver]:
                approvals[receiver].add(value)
            results[value]["timestamp"] = json_object.get('content').get('reception-timestamp')
        elif message_type == 'Accepted':
            if results[value]["timestamp"] == -1:
                results[value]["agreed_before"].add(json_object.get('content').get('begin-node-id'))
            else:
                node_id = json_object.get('content').get('begin-node-id')
                if not node_id in results[value]["agreed_before"]:
                    results[value]["agreed_after"].add(node_id)
                
            
def initialize_and_validate(value, node_id, message_type, round):
    if not value in results:
        results[value] = {"timestamp": -1, "agreed_before": set(), "agreed_after": set(), "proposed": -1}
    if message_type == 'Consensus Reached':
        if not node_id in approvals and not node_id == -1:
            approvals[node_id] = set()
        if not round is None:
            if rounds[round] is None:
                rounds[round] = value
            elif not rounds[round] == value:
                errors.append('In round N there were two different values accepted, ' + str(value) + ' and ' + str(rounds[round]))
                
# Replace 'your_file_path.json' with the actual path to your JSON file
file_path = '/Users/loureiro/Desktop/FCT/Thesis RL/Simulators/Simulador-MOBS/MOBS/output_files/out0-1.json'
read_json_file(file_path)
