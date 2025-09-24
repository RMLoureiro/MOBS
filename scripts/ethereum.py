import json

def read_json_file(file_path):
	try:
		with open(file_path, 'r') as file:
			json_array = json.load(file)

			# Data structures
			epochs_observed = set()
			finalized_by_epoch = {}  # epoch -> set of finalized_hashes

			# Read array and build structures
			for obj in json_array:
				if obj.get('kind') != 'flow-message':
					continue
				content = obj.get('content', {})
				msg_data = content.get('msg-data', {})
				msg_type = msg_data.get('type')

				# Extract epoch as int when present
				epoch_val = msg_data.get('epoch')
				epoch = None
				if epoch_val is not None:
					try:
						epoch = int(epoch_val)
					except (ValueError, TypeError):
						epoch = None

				if epoch is not None:
					epochs_observed.add(epoch)

				if msg_type == 'FinalizedNode' and epoch is not None:
					fhash = msg_data.get('finalized_hash')
					if fhash is None:
						continue
					if epoch not in finalized_by_epoch:
						finalized_by_epoch[epoch] = set()
					finalized_by_epoch[epoch].add(fhash)

			if not epochs_observed:
				print('No epochs observed in log.')
				return

			min_epoch = min(epochs_observed)
			max_epoch = max(epochs_observed)

			print()
			print('======================== FinalizedNode per epoch ========================')
			print()
			missing_epochs = []
			for e in range(min_epoch, max_epoch + 1):
				hashes = finalized_by_epoch.get(e)
				if hashes:
					# Print deterministic order
					hashes_list = sorted(list(hashes))
					print(f'Epoch {e}: Finalized hashes: {", ".join(hashes_list)}')
				else:
					print(f'Epoch {e}: No FinalizedNode messages')
					missing_epochs.append(e)

			# Detect if there exists a run of >3 consecutive missing epochs
			longest_run = 0
			current_run = 0
			for e in range(min_epoch, max_epoch + 1):
				if e in finalized_by_epoch:
					current_run = 0
				else:
					current_run += 1
					if current_run > longest_run:
						longest_run = current_run

			if longest_run > 5 and missing_epochs:
				print()
				print('================ Epochs without FinalizedNode (run > 5 detected) ================')
				print()
				print(', '.join(str(e) for e in missing_epochs))

	except FileNotFoundError:
		print(f"File not found: {file_path}")
	except json.JSONDecodeError as e:
		print(f"Error decoding JSON: {e}")


if __name__ == '__main__':
	# Path to the simulator output JSON file
	file_path = '/Users/loureiro/Desktop/FCT/Thesis RL/Simulators/Simulador-MOBS/MOBS/output_files/out0-1.json'
	read_json_file(file_path)

