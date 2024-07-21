# Assisted by WCA@IBM
# Latest GenAI contribution: ibm/granite-20b-code-instruct-v2
import json
import requests

# Read the dataset from a JSON file
with open('dataset.json', 'r') as f:
    data = json.load(f)

# Define a function to make a GET request to the Tomcat server
def get_response(question):
    url = 'http://localhost:8080/restapi/api/v1/message'
    payload = {'message': question}
    response = requests.get(url, params=payload)
    return response.text

# Loop through the dataset and print the response for each question
for item in data:
    question = item['question']
    answer = item['answer']
    print(f'{question}: {answer}')

    # If the question starts with "Java call", call the Python function
    if question.startswith('Java call'):
        response = get_response(question)
        print(f'{question}: {response}')
