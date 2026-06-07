# Microbiome Disease Prediction Web Service

This web service predicts one of 23 disease classes from an uploaded microbiome TSV file.

## Required Files

Place the trained model file in the same folder as `app.py`.

```text
microbiome_disease_project/
  webapp/
    app.py
    README.md
    microbiome_disease_voting_model.joblib
```

The web service will not run without:

```text
microbiome_disease_voting_model.joblib
```

The model file is not included in the repository because it is large.

## Python Requirements

```text
flask
pandas
scikit-learn
joblib
```

Install them with:

```bash
pip install flask pandas scikit-learn joblib
```

## Run

Move to the webapp folder:

```bash
cd microbiome_disease_project/webapp
```

Start the Flask server:

```bash
python app.py
```

Open the local web page:

```text
http://127.0.0.1:5002
```

## Upload Format

Upload a TSV file with the same feature layout used for training:

```text
ID
microbiome_1
microbiome_2
...
microbiome_1953
```

After a successful upload, the page shows a preview table and provides a CSV download of all predictions.

## Port Conflict

If port `5002` is already in use, edit the last line of `app.py`:

```python
app.run(host="127.0.0.1", port=5003, debug=False)
```

Then open:

```text
http://127.0.0.1:5003
```
