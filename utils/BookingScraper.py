import os
import csv
import json
import requests
from pprint import pprint
from bs4 import BeautifulSoup

"""
Configuration Variables
"""
SEARCH_DIR = "searches"
HEADERS = {
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36"
}

"""
Supporting Functions
"""
def fetch_hotel_link(url, headers):
    return requests.get(url, headers=headers).text

"""
Extract Hotels Data
"""
def get_hotel_overview(filename):
    # Read response_content.html file
    with open(filename, "r", encoding="utf-8") as file:
        html_content = file.read()

    # Parse HTML content
    soup = BeautifulSoup(html_content, "html.parser")

    # Find the div element containing the hotel listings
    property_listing_div = soup.find("div", attrs={"data-results-container": "1"})

    # Find all the hotels in the property_listing_div
    hotels_container = property_listing_div.find_all("div", attrs={"data-testid": "property-card-container"})

    # Extract hotel details in search results
    hotels = []
    for hotel in hotels_container:
        try:
            details = {
                "title": hotel.find("div", attrs={"data-testid": "title"}).get_text(strip=True),
                "link": hotel.find("a", attrs={"data-testid": "title-link"})["href"],
                "cover_src": hotel.find("img", attrs={"data-testid": "image"})["src"],
                "cover_alt": hotel.find("img", attrs={"data-testid": "image"})["alt"],
                "suburb": hotel.find("span", attrs={"data-testid": "address"}).get_text(strip=True),
                "score": hotel.find("div", attrs={"data-testid": "review-score"}).find("div", class_="a3b8729ab1 d86cee9b25").text.split()[-1] if hotel.find("div", attrs={"data-testid": "review-score"}) else "No score",
                "score_aggregate": hotel.find("div", attrs={"data-testid": "review-score"}).find("div", class_="a3b8729ab1 e6208ee469 cb2cbb3ccb").get_text(strip=True).split()[0] if hotel.find("div", attrs={"data-testid": "review-score"}) else "No aggregate",
                "score_reviews": hotel.find("div", attrs={"data-testid": "review-score"}).find("div", class_="abf093bdfe f45d8e4c32 d935416c47").get_text(strip=True).split()[0].replace(",", "") if hotel.find("div", attrs={"data-testid": "review-score"}) else "No reviews",
                "recommended_unit": hotel.find("div", attrs={"data-testid": "recommended-units"}).find("h4").get_text(strip=True),
                "recommended_bed": hotel.find("div", attrs={"data-testid": "recommended-units"}).find("div", class_="abf093bdfe").get_text(strip=True),
                "price": hotel.find("span", attrs={"data-testid": "price-and-discounted-price"}).get_text(strip=True).split("\xa0")[-1],
                "price_currency": hotel.find("span", attrs={"data-testid": "price-and-discounted-price"}).get_text(strip=True).split("\xa0")[0],
                "price_for": hotel.find("div", attrs={"data-testid": "price-for-x-nights"}).get_text(strip=True),
                "price_specification": hotel.find("div", attrs={"data-testid": "taxes-and-charges"}).get_text(strip=True)
            }
            hotels.append(details)
        except AttributeError as e:
            print(f"An error occurred when extracting hotel information: {e}")
            continue

    print("\n--------------------------------------------------------------------------------\n")

    return hotels

def get_hotel_further_details(hotel, html_content):
    soup = BeautifulSoup(html_content, "html.parser")

    # Extract longitude and latitude
    try:
        lat_lng = soup.find(attrs={"data-atlas-latlng": True})["data-atlas-latlng"].split(",")
        hotel["latitude"] = lat_lng[0]
        hotel["longitude"] = lat_lng[1]
    except (TypeError, KeyError):
        print(f"Longitude and latitude information not found: {hotel["title"]}")
        hotel["latitude"] = ""
        hotel["longitude"] = ""

    # Extract address from JSON-LD
    try:
        script_tag = soup.find("script", type="application/ld+json")
        if script_tag:
            json_content = json.loads(script_tag.string)
            if "address" in json_content and "streetAddress" in json_content["address"]:
                hotel["address"] = json_content["address"]["streetAddress"]
            else:
                print(f"Address information not found in JSON-LD: {hotel['title']}")
                hotel["address"] = ""
        else:
            print(f"JSON-LD script tag not found: {hotel['title']}")
            hotel["address"] = ""
    except (AttributeError, json.JSONDecodeError, KeyError) as e:
        print(f"Error extracting address from JSON-LD for {hotel['title']}: {e}")
        hotel["address"] = ""

    # Extract hotel description
    try:
        hotel["description"] = soup.find("p", attrs={"data-testid": "property-description"}).get_text(strip=True)
    except AttributeError:
        print(f"Description not found: {hotel["title"]}")
        hotel["description"] = ""

    return hotel

def save_hotels_to_csv(hotels, filename):
    # Define the field name of the CSV file
    fieldnames = [
        "address", "cover_alt", "cover_src", "description", "latitude", "link", "longitude",
        "price", "price_currency", "price_for", "price_specification", "recommended_unit",
        "recommended_bed", "score", "score_aggregate", "score_reviews", "suburb", "title"
    ]

    try:
        with open(filename, "w", newline="", encoding="utf-8") as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            writer.writeheader()
            for hotel in hotels:
                writer.writerow(hotel)
        print(f"Successfully saved the hotel data to {filename}")
    except IOError as e:
        print(f"An error occurred when writing the CSV file: {e}")

if __name__ == "__main__":
    os.makedirs(SEARCH_DIR, exist_ok=True)

    """
    Extract hotels data from search results
    """
    hotels = get_hotel_overview(os.path.join(SEARCH_DIR, "search_content_1.html"))
    pprint(hotels)
    print("\n--------------------------------------------------------------------------------")

    """
    Extract further data from hotel links
    """
    for idx, hotel in enumerate(hotels, start=1):
        print(f"\nJob {idx}/{len(hotels)}: {hotel["title"]}")
        try:
            response = fetch_hotel_link(hotel["link"], HEADERS)
            hotel = get_hotel_further_details(hotel, response)
        except requests.RequestException as e:
            print(f"A network error occurred when obtaining hotel details: {e}")
            continue

    """
    Save all hotels data to CSV
    """
    csv_filename = "hotels.csv"
    save_hotels_to_csv(hotels, csv_filename)
