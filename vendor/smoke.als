// Smoke test: a simplified version of two tables with a foreign key.
// Customer has email (candidate key). Order has customer (FK).
// We assert FK integrity: every order points to some customer.

sig Customer { email: one Email }
sig Order { customer: one Customer }
sig Email {}

// Email uniqueness — functional dependency email -> Customer
fact UniqueEmail {
  all disj c1, c2: Customer | c1.email != c2.email
}

// Should always succeed (it's tautological with `one Customer`)
assert FKIntegrity {
  all o: Order | one o.customer
}
check FKIntegrity for 5

// Show me a valid instance with at least 2 customers and 3 orders
pred valid { #Customer >= 2 and #Order >= 3 }
run valid for 5
