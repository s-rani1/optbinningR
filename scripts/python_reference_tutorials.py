#!/usr/bin/env python3
import json
from pathlib import Path

import numpy as np
import pandas as pd
from optbinning import OptimalBinning
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score


ROOT = Path(__file__).resolve().parents[1]
EXT = ROOT / "inst" / "extdata"


def fit_ob(x, y, solver="cp", prebinning_method="cart", max_n_bins=6, max_n_prebins=20, monotonic_trend="auto"):
    ob = OptimalBinning(
        name="x",
        dtype="numerical",
        solver=solver,
        prebinning_method=prebinning_method,
        max_n_bins=max_n_bins,
        max_n_prebins=max_n_prebins,
        min_prebin_size=0.05,
        monotonic_trend=monotonic_trend,
    )
    ob.fit(x, y)
    bt = ob.binning_table.build()
    core = bt.copy()
    if "Bin" in core.columns:
        core = core.loc[~core["Bin"].isin(["Special", "Missing", "Totals"])]
    if "Totals" in core.index:
        core = core.loc[core.index != "Totals"]
    bt_iv = bt.copy()
    if "Bin" in bt_iv.columns:
        bt_iv = bt_iv.loc[~bt_iv["Bin"].isin(["Totals"])]
    if "Totals" in bt_iv.index:
        bt_iv = bt_iv.loc[bt_iv.index != "Totals"]
    return {
        "status": str(ob.status),
        "splits": [float(v) for v in np.asarray(ob.splits if ob.splits is not None else [], dtype=float)],
        "event_rate_core": [float(v) for v in pd.to_numeric(core["Event rate"], errors="coerce").dropna().to_numpy()],
        "iv_total": float(pd.to_numeric(bt_iv["IV"], errors="coerce").dropna().sum()),
    }


def fit_workflow_binary(df_train, y_train, df_test, solver="cp"):
    vars_ = list(df_train.columns)
    Xw_train = pd.DataFrame(index=df_train.index)
    Xw_test = pd.DataFrame(index=df_test.index)
    splits = {}
    for v in vars_:
        dtype = "categorical" if not pd.api.types.is_numeric_dtype(df_train[v]) else "numerical"
        ob = OptimalBinning(
            name=v,
            dtype=dtype,
            solver=solver,
            prebinning_method="quantile",
            max_n_bins=6,
            max_n_prebins=20,
            min_prebin_size=0.05,
            monotonic_trend="auto",
        )
        ob.fit(df_train[v].to_numpy(), y_train.to_numpy())
        Xw_train[v] = ob.transform(df_train[v].to_numpy(), metric="woe")
        Xw_test[v] = ob.transform(df_test[v].to_numpy(), metric="woe")
        if dtype == "numerical":
            s = np.asarray(ob.splits if ob.splits is not None else [], dtype=float)
            splits[v] = [float(z) for z in s]
        else:
            splits[v] = []
    clf = LogisticRegression(penalty=None, solver="lbfgs", max_iter=2000)
    clf.fit(Xw_train.to_numpy(), y_train.to_numpy())
    p_train = clf.predict_proba(Xw_train.to_numpy())[:, 1]
    p_test = clf.predict_proba(Xw_test.to_numpy())[:, 1]
    return splits, p_train, p_test


def main():
    out = {}

    # LocalSolver reference on standard dataset.
    d = pd.read_csv(EXT / "breast_cancer_mean_radius.csv")
    try:
      out["localsolver_breast_cancer"] = fit_ob(
          d["x"].to_numpy(),
          d["y"].to_numpy(),
          solver="ls",
          prebinning_method="cart",
          max_n_bins=7,
          max_n_prebins=20,
          monotonic_trend="auto",
      )
      out["localsolver_breast_cancer"]["engine_available"] = True
    except Exception as e:
      out["localsolver_breast_cancer"] = {
          "engine_available": False,
          "error": f"{type(e).__name__}: {str(e)}"
      }

    # Large-scale profile reference.
    dls = pd.read_csv(EXT / "large_scale_synth.csv")
    out["large_scale_synth"] = fit_ob(
        dls["x"].to_numpy(),
        dls["y"].to_numpy(),
        solver="cp",
        prebinning_method="quantile",
        max_n_bins=8,
        max_n_prebins=20,
        monotonic_trend="auto",
    )

    # FICO workflow reference.
    fico_train = pd.read_csv(EXT / "fico_train.csv")
    fico_upd = pd.read_csv(EXT / "fico_update.csv")
    ytr = fico_train["y"].astype(int)
    yup = fico_upd["y"].astype(int)
    xtr = fico_train.drop(columns=["y"])
    xup = fico_upd.drop(columns=["y"])
    _, p_train, p_upd = fit_workflow_binary(xtr, ytr, xup, solver="cp")
    out["fico_workflow"] = {
        "train_auc": float(roc_auc_score(ytr, p_train)),
        "update_auc": float(roc_auc_score(yup, p_upd)),
    }

    # Telco workflow reference.
    telco_train = pd.read_csv(EXT / "telco_train.csv")
    telco_test = pd.read_csv(EXT / "telco_test.csv")
    ytr_t = telco_train["y"].astype(int)
    yte_t = telco_test["y"].astype(int)
    xtr_t = telco_train.drop(columns=["y"])
    xte_t = telco_test.drop(columns=["y"])
    _, _, p_test_t = fit_workflow_binary(xtr_t, ytr_t, xte_t, solver="cp")
    out["telco_workflow"] = {
        "test_auc": float(roc_auc_score(yte_t, p_test_t)),
    }

    with open(EXT / "python_tutorial_refs.json", "w", encoding="utf-8") as f:
        json.dump(out, f, indent=2)
    print(json.dumps(out, indent=2))


if __name__ == "__main__":
    main()
