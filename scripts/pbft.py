import json

results = {}
approvals = {}
errors = []
rounds = {}
recent_view_changes = set()
view_changes = {}

def read_json_file(file_path):
    try:
        with open(file_path, 'r') as file:
            total_nodes = 20
            consensus = 0
            runtime = 0
            average_approval = []
            json_array = json.load(file)
            # Read array create data
            for json_object in json_array:
                read_json_object(json_object)
            
            while len(view_changes) > 0 or len(results) > 0:
                if len(view_changes) > 0:
                    view_changes_key = list(view_changes.keys())[0]
                    view_changes_timestamp = view_changes[view_changes_key]["timestamp"]
                else:
                    view_changes_timestamp = 9223372036854775807

                if len(results) > 0:
                    results_key = list(results.keys())[0]
                    results_timestamp = results[results_key]["timestamp"]
                else:
                    results_timestamp = 9223372036854775807


                if view_changes_timestamp < results_timestamp:
                    print('View changed to ' + str(view_changes_key) + ' at ' + str(view_changes[view_changes_key]["timestamp"]) + 'ms')
                    del view_changes[view_changes_key]
                else:
                    if results_key != None and results_timestamp > 0:
                        consensus += + 1
                        if results[results_key]["timestamp"] > runtime:
                            runtime = results[results_key]["timestamp"]
                        agreed_before = len(results[results_key]["agreed_before"])
                        agreed_after = len(results[results_key]["agreed_after"])
                        total_agreement = agreed_before + agreed_after
                        average_approval.append(total_agreement/ total_nodes)
                        proposed_at = ""

                        if results[results_key]["proposed"] == -1:
                            proposed_at = " THIS VALUE WAS NOT PROPOSED"
                            errors.append("VALUE: " + str(results_key) + " was accepted but not proposed")
                        elif results[results_key]["proposed"] >= results[results_key]["timestamp"]:
                            proposed_at = " THIS VALUE PROPOSE TIME IS INCONSISTENT"
                        else:
                            proposed_at = " This value was proposed at: " + str(results[results_key]["proposed"])

                        print('Value: ' + str(results_key) + ' Reached at: ' + str(results[results_key]["timestamp"]) + 'ms nodes agreed before: ' + str(agreed_before) +
                                ' nodes agreed after: ' + str(agreed_after) + ' total agreement: ' + str(total_agreement) + proposed_at)
                    del results[results_key]

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
            if consensus != 0 : print('Average time per consensus: ' + str(runtime/consensus) + 'ms')
            if consensus != 0 : print('Average acceptance percentage: ' + str(sum(average_approval)/consensus * 100) + '%')

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
        timestamp = json_object.get('content').get('reception-timestamp')
        initialize_and_validate(value, receiver, message_type, round)
        if message_type == 'Request':
            results[value]["proposed"] = timestamp
        elif message_type == 'Accept':
            if not value in approvals[receiver]:
                recent_view_changes.clear()
                approvals[receiver].add(value)
            results[value]["timestamp"] = timestamp
        elif message_type == 'Reply':
            if results[value]["timestamp"] == -1:
                results[value]["agreed_before"].add(json_object.get('content').get('begin-node-id'))
            else:
                node_id = json_object.get('content').get('begin-node-id')
                if not node_id in results[value]["agreed_before"]:
                    results[value]["agreed_after"].add(node_id)
        elif message_type == 'ApplyNewView':
            view = json_object.get('content').get('msg-data').get('View')
            if not view in recent_view_changes:
                initialize_and_validate_view_change(view, timestamp)
                recent_view_changes.add(view)
                if len(recent_view_changes) >= 5:
                    recent_view_changes.clear()
                    errors.append('View changed 5 or more times without deciding a new value')
                
            
def initialize_and_validate(value, node_id, message_type, round):
    if not value in results:
        results[value] = {"timestamp": -1, "agreed_before": set(), "agreed_after": set(), "proposed": -1}
    if message_type == 'Accept':
        if not node_id in approvals and not node_id == -1:
            approvals[node_id] = set()
        if not round is None:
            if rounds[round] is None:
                rounds[round] = value
            elif not rounds[round] == value:
                errors.append('In round N there were two different values accepted, ' + str(value) + ' and ' + str(rounds[round]))
                
def initialize_and_validate_view_change(view, timestamp):
    if not view in view_changes:
        view_changes[view] = {"timestamp": timestamp}

# Replace 'your_file_path.json' with the actual path to your JSON file
file_path = '/Users/loureiro/Desktop/FCT/Thesis RL/Simulators/Simulador-MOBS/MOBS/output_files/out0-1.json'
read_json_file(file_path)
