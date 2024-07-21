class Customer:
    def __init__(self, first_name, last_name, email):
        self.first_name = first_name
        self.last_name = last_name
        self.email = email

    def get_full_name(self):
        return self.first_name + self.last_name

    def __repr__(self):
        return f"Customer(first_name={self.first_name}, last_name={self.last_name}, email={self.email})"
    
    def __lt__(self, other):
        if self.email!= other.email:
            return self.email < other.email
        elif self.last_name!= other.last_name:
            return self.last_name < other.last_name
        else:
            return self.first_name < other.first_name
    
    @staticmethod
    def sort_customers(customers):
        return sorted(customers)


# Test Code
c1 = Customer("John", "Doe", "ajohn@gmail.com")
c2 = Customer("Jane", "Doe", "janedoe@gmail.com")
c3 = Customer("John", "Smith", "bjohn@gmail.com")
c4 = Customer("Jane", "Smith", "janesmith@gmail.com")

customers = [c1, c2, c3, c4]
print(customers)
print(Customer.sort_customers(customers))
