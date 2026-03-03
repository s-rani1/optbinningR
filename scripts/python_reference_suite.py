#!/usr/bin/env python3
"""Generate a multi-case parity reference suite from Python optbinning."""

import json
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.datasets import load_breast_cancer
from optbinning import OptimalBinning


ROOT = Path(__file__).resolve().parents[1]
OUT_DIR = ROOT / "inst" / "extdata"
OUT_DIR.mkdir(parents=True, exist_ok=True)


def _core_table(bt: pd.DataFrame) -> pd.DataFrame:
    core = bt.copy()
    if "Bin" in core.columns:
        core = core.loc[~core["Bin"].isin(["Special", "Missing"])]
    if "Totals" in core.index:
        core = core.loc[core.index != "Totals"]
    return core


def _to_list(values) -> list[float]:
    return [float(v) for v in np.asarray(values)]


def _run_case(case: dict, dataset_map: dict[str, pd.DataFrame]) -> dict:
    data_name = case["data_file"]
    df = dataset_map[data_name]
    x = df[case["x_column"]].to_numpy()
    y = df[case["y_column"]].to_numpy()

    p = case["params"]
    kwargs = dict(
        name=case["variable"],
        dtype=case["dtype"],
        solver="cp",
        prebinning_method="cart",
        monotonic_trend=p["monotonic_trend"],
        max_n_bins=p["max_n_bins"],
        max_n_prebins=p["max_n_prebins"],
        min_prebin_size=p["min_prebin_size"],
        min_bin_n_event=1,
        min_bin_n_nonevent=1,
    )
    if case.get("special_codes") is not None:
        kwargs["special_codes"] = case["special_codes"]

    optb = OptimalBinning(**kwargs)
    optb.fit(x, y)

    bt = optb.binning_table.build()
    core = _core_table(bt)

    out = {
        "id": case["id"],
        "dtype": case["dtype"],
        "variable": case["variable"],
        "data_file": data_name,
        "x_column": case["x_column"],
        "y_column": case["y_column"],
        "special_codes": case.get("special_codes"),
        "params": p,
        "status": optb.status,
        "iv_total": float(bt.loc["Totals", "IV"]) if "Totals" in bt.index else float(bt["IV"].sum()),
        "n_bins_core": int(len(core)),
        "event_rate_core": _to_list(pd.to_numeric(core["Event rate"], errors="coerce").dropna().to_numpy()),
        "woe_core": _to_list(pd.to_numeric(core["WoE"], errors="coerce").dropna().to_numpy()),
    }

    if case["dtype"] == "numerical":
        out["splits"] = _to_list(optb.splits)
    else:
        out["splits"] = []

    return out


def main() -> None:
    data = load_breast_cancer()
    df = pd.DataFrame(data.data, columns=data.feature_names)
    y = data.target

    full = df.copy()
    full["target"] = y
    full.to_csv(OUT_DIR / "breast_cancer_full.csv", index=False)

    # Numerical case with missing/special values.
    miss = full[["mean radius", "target"]].copy()
    miss.columns = ["x", "y"]
    miss.loc[miss.index % 29 == 0, "x"] = np.nan
    miss.loc[miss.index % 31 == 0, "x"] = -999.0
    miss.to_csv(OUT_DIR / "breast_cancer_mean_radius_missing_special.csv", index=False)

    # Synthetic categorical case (each category has both classes).
    cat_rows = []
    spec = {
        "A": (38, 2),
        "B": (30, 10),
        "C": (20, 20),
        "D": (8, 32),
    }
    for k, (n_event, n_nonevent) in spec.items():
        cat_rows += [{"x": k, "y": 1}] * n_event
        cat_rows += [{"x": k, "y": 0}] * n_nonevent
    cat_df = pd.DataFrame(cat_rows)
    cat_df.to_csv(OUT_DIR / "categorical_synthetic.csv", index=False)

    dataset_map = {
        "breast_cancer_full.csv": full,
        "breast_cancer_mean_radius_missing_special.csv": miss,
        "categorical_synthetic.csv": cat_df,
    }

    cases = [
        {
            "id": "mean_radius_default",
            "dtype": "numerical",
            "variable": "mean radius",
            "data_file": "breast_cancer_full.csv",
            "x_column": "mean radius",
            "y_column": "target",
            "special_codes": None,
            "params": {"monotonic_trend": "auto", "max_n_bins": None, "max_n_prebins": 20, "min_prebin_size": 0.05},
        },
        {
            "id": "mean_perimeter_default",
            "dtype": "numerical",
            "variable": "mean perimeter",
            "data_file": "breast_cancer_full.csv",
            "x_column": "mean perimeter",
            "y_column": "target",
            "special_codes": None,
            "params": {"monotonic_trend": "auto", "max_n_bins": None, "max_n_prebins": 20, "min_prebin_size": 0.05},
        },
        {
            "id": "mean_area_default",
            "dtype": "numerical",
            "variable": "mean area",
            "data_file": "breast_cancer_full.csv",
            "x_column": "mean area",
            "y_column": "target",
            "special_codes": None,
            "params": {"monotonic_trend": "auto", "max_n_bins": None, "max_n_prebins": 20, "min_prebin_size": 0.05},
        },
        {
            "id": "mean_smoothness_default",
            "dtype": "numerical",
            "variable": "mean smoothness",
            "data_file": "breast_cancer_full.csv",
            "x_column": "mean smoothness",
            "y_column": "target",
            "special_codes": None,
            "params": {"monotonic_trend": "auto", "max_n_bins": None, "max_n_prebins": 20, "min_prebin_size": 0.05},
        },
        {
            "id": "mean_compactness_default",
            "dtype": "numerical",
            "variable": "mean compactness",
            "data_file": "breast_cancer_full.csv",
            "x_column": "mean compactness",
            "y_column": "target",
            "special_codes": None,
            "params": {"monotonic_trend": "auto", "max_n_bins": None, "max_n_prebins": 20, "min_prebin_size": 0.05},
        },
        {
            "id": "mean_radius_desc_max6",
            "dtype": "numerical",
            "variable": "mean radius",
            "data_file": "breast_cancer_full.csv",
            "x_column": "mean radius",
            "y_column": "target",
            "special_codes": None,
            "params": {"monotonic_trend": "descending", "max_n_bins": 6, "max_n_prebins": 20, "min_prebin_size": 0.05},
        },
        {
            "id": "mean_compactness_desc_max6",
            "dtype": "numerical",
            "variable": "mean compactness",
            "data_file": "breast_cancer_full.csv",
            "x_column": "mean compactness",
            "y_column": "target",
            "special_codes": None,
            "params": {"monotonic_trend": "descending", "max_n_bins": 6, "max_n_prebins": 20, "min_prebin_size": 0.05},
        },
        {
            "id": "mean_radius_missing_special",
            "dtype": "numerical",
            "variable": "mean radius",
            "data_file": "breast_cancer_mean_radius_missing_special.csv",
            "x_column": "x",
            "y_column": "y",
            "special_codes": [-999.0],
            "params": {"monotonic_trend": "auto", "max_n_bins": None, "max_n_prebins": 20, "min_prebin_size": 0.05},
        },
        {
            "id": "categorical_synthetic",
            "dtype": "categorical",
            "variable": "x_cat",
            "data_file": "categorical_synthetic.csv",
            "x_column": "x",
            "y_column": "y",
            "special_codes": None,
            "params": {"monotonic_trend": "auto", "max_n_bins": None, "max_n_prebins": 20, "min_prebin_size": 0.05},
        },
    ]

    out_cases = [_run_case(case, dataset_map) for case in cases]

    suite = {
        "dataset": "mixed_suite",
        "n": int(len(full)),
        "target_column": "target",
        "cases": out_cases,
    }

    # Backward-compatible single-case artifacts from first case.
    first = out_cases[0]
    pd.DataFrame({"x": full[first["x_column"]].to_numpy(), "y": full[first["y_column"]].to_numpy()}).to_csv(
        OUT_DIR / "breast_cancer_mean_radius.csv", index=False
    )
    with open(OUT_DIR / "python_optbinning_reference.json", "w", encoding="utf-8") as f:
        json.dump(first, f, indent=2)

    with open(OUT_DIR / "python_optbinning_reference_suite.json", "w", encoding="utf-8") as f:
        json.dump(suite, f, indent=2)

    print(json.dumps(suite, indent=2))


if __name__ == "__main__":
    main()
