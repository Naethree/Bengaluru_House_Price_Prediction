from flask import Flask, request, jsonify
import util

app = Flask(__name__)

util.load_saved_artifacts()

@app.route('/get_location_names')
def get_location_names():
    response = jsonify({
        'locations': util.get_location_names()
    })
    response.headers.add('Access-Control-Allow-Origin', '*')

    return response


@app.route('/predict_home_price', methods=['POST'])
def predict_home_price():
    total_sqft = float(request.form['total_sqft'])
    location = request.form['location']
    bedroom = int(request.form['bedroom'])
    balcony = int(request.form['balcony'])
    bath = int(request.form['bath'])

    # Get the estimated price as a float
    estimated_price = float(util.get_estimated_price(location, total_sqft, bedroom, balcony, bath))

    # Rounding the estimated price to 4 decimal places
    rounded_price = round(estimated_price, 4)

    response = jsonify({
        'estimated_price': rounded_price
    })

    response.headers.add('Access-Control-Allow-Origin', '*')
    return response


if __name__ == "__main__":
    print("Starting Python Flask Server For Home Price Prediction...")
    app.run()
