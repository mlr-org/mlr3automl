import numpy as np

from deepcave.utils.configspace import sample_border_config, sample_random_config

from deepcave.runs.converters.deepcave import DeepCAVERun
from deepcave.evaluators.footprint import Footprint
from pathlib import Path

run = DeepCAVERun.from_path(Path("tmp/run/run_1"))
fp = Footprint(run)
objective = run.get_objective(0)
fp.calculate(objective, 0)
fp_distances = fp._distances

random = sample_random_config(fp.cs)
border = sample_border_config(fp.cs)