######################################
## Microbiome Disease Prediction   ##
## Implemented by Jewoo Yoo         ##
## 2026-06-04                       ##
######################################

from io import StringIO
from pathlib import Path

import joblib
import pandas as pd
from flask import Flask, Response, render_template_string, request


APP_DIR = Path(__file__).resolve().parent
PROJECT_DIR = APP_DIR.parent
MODEL_NAME = "microbiome_disease_voting_model.joblib"
MODEL_PATH = APP_DIR / MODEL_NAME
if not MODEL_PATH.exists():
    MODEL_PATH = PROJECT_DIR / "model" / MODEL_NAME

app = Flask(__name__)
model_bundle = joblib.load(MODEL_PATH)
model = model_bundle["model"]
feature_cols = model_bundle["feature_cols"]


PAGE = """
<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Microbiome Disease Predictor</title>
  <style>
    body { font-family: Arial, sans-serif; margin: 36px; color: #172033; }
    h1 { font-size: 24px; margin-bottom: 6px; }
    p { color: #536071; }
    form { margin: 22px 0; padding: 18px; border: 1px solid #d7dde8; border-radius: 8px; max-width: 680px; }
    button { padding: 8px 14px; border: 0; background: #2457a6; color: white; border-radius: 6px; cursor: pointer; }
    input { margin: 10px 0; }
    table { border-collapse: collapse; margin-top: 18px; font-size: 14px; }
    th, td { border: 1px solid #d7dde8; padding: 6px 10px; text-align: left; }
    th { background: #eef3f9; }
    .error { color: #9b1c1c; font-weight: bold; }
    .download { display: inline-block; margin-top: 12px; }
  </style>
</head>
<body>
  <h1>Microbiome Disease Predictor</h1>
  <p>Upload a TSV file with an ID column and microbiome_1 ... microbiome_1953 columns.</p>
  <form method="post" enctype="multipart/form-data">
    <input type="file" name="file" accept=".tsv,.txt" required>
    <br>
    <button type="submit">Predict</button>
  </form>
  {% if error %}
    <div class="error">{{ error }}</div>
  {% endif %}
  {% if rows %}
    <h2>Prediction Preview</h2>
    <p>{{ total }} rows predicted. First 20 rows are shown below.</p>
    <a class="download" href="/download">Download full CSV</a>
    <table>
      <tr><th>ID</th><th>Prediction</th><th>Max probability</th></tr>
      {% for row in rows %}
        <tr><td>{{ row.ID }}</td><td>{{ row.Prediction }}</td><td>{{ "%.4f"|format(row.max_probability) }}</td></tr>
      {% endfor %}
    </table>
  {% endif %}
</body>
</html>
"""

last_prediction_csv = None


def predict_dataframe(df):
    if "ID" not in df.columns:
        raise ValueError("Uploaded file must include an ID column.")
    missing = [c for c in feature_cols if c not in df.columns]
    if missing:
        raise ValueError(f"Missing feature columns: {', '.join(missing[:5])}")

    X = df[feature_cols]
    pred = model.predict(X)
    proba = model.predict_proba(X).max(axis=1)
    return pd.DataFrame(
        {
            "ID": df["ID"].astype(str),
            "Prediction": pred,
            "max_probability": proba,
        }
    )


@app.route("/", methods=["GET", "POST"])
def index():
    global last_prediction_csv
    error = None
    rows = None
    total = 0

    if request.method == "POST":
        try:
            uploaded = request.files["file"]
            df = pd.read_csv(uploaded, sep="\t")
            result = predict_dataframe(df)
            total = len(result)
            last_prediction_csv = result.to_csv(index=False)
            rows = result.head(20).to_dict(orient="records")
        except Exception as exc:
            error = str(exc)

    return render_template_string(PAGE, error=error, rows=rows, total=total)


@app.route("/download")
def download():
    if last_prediction_csv is None:
        return Response("No prediction has been made yet.", status=404)
    return Response(
        last_prediction_csv,
        mimetype="text/csv",
        headers={"Content-Disposition": "attachment; filename=predictions.csv"},
    )


@app.route("/health")
def health():
    return {"status": "ok", "model": "SoftVoting_ET4_LR1", "features": len(feature_cols)}


if __name__ == "__main__":
    app.run(host="127.0.0.1", port=5002, debug=False)
