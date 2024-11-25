import os
import glob
import requests
from requests.auth import HTTPBasicAuth

# Opal server details
opal_url = "http://your-opal-server:8080"
resource_endpoint = f"{opal_url}/ws/resources"
username = "your-username"
password = "your-password"

# Directory where files are saved
directory = "/path/to/saved/files"

# Find all CSV files in the directory
files = glob.glob(os.path.join(directory, "*.csv"))

# Ensure there are files
if not files:
    print("No CSV files found in the directory.")
else:
    # Find the most recent file
    latest_file = max(files, key=os.path.getctime)
    print(f"The most recent file is: {latest_file}")

    # Extract file name to use as the resource name
    resource_name = os.path.basename(latest_file)

    # Check if the resource already exists
    response = requests.get(
        f"{resource_endpoint}/{resource_name}",
        auth=HTTPBasicAuth(username, password)
    )

    if response.status_code == 200:
        print(f"Resource '{resource_name}' already exists in Opal.")
    elif response.status_code == 404:
        print(f"Resource '{resource_name}' does not exist. Creating it now...")
        
        # Create the resource
        resource_data = {
            "name": resource_name,
            "type": "file",
            "uri": f"file://{latest_file}",
            "description": "A dynamically created resource",
            "tags": ["example", "dynamic"]
        }

        create_response = requests.post(
            resource_endpoint,
            json=resource_data,
            auth=HTTPBasicAuth(username, password)
        )

        if create_response.status_code == 201:
            print(f"Resource '{resource_name}' created successfully!")
        else:
            print(f"Failed to create resource. Status: {create_response.status_code}, Error: {create_response.text}")
    else:
        print(f"Error checking resource. Status: {response.status_code}, Error: {response.text}")
