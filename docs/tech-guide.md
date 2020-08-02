# Technical Documentation

## INDEX  
* [Model Variables](#model-variables)  
* [Objective Function](#objective-function)
* [Constraints](#constraints)


## Model Variables

For this model we can categorise the variables given to the the optimiser as of 
two types:  
1. Variables representing the number of judges allocated to each jurisdiction 
(**allocation variables**); and  
2. Variables representing the numbers of judges available, newly hired and newly 
departed (hereafter known as **resource variables**).

All variables represent numbers of judges in FTE rather than headcount terms.

The total number of variables is dependent on the number of years in the future 
the model covers, the number of different types of jurisdiction and the number 
of different types of judge. These are all things which can be changed by the user.

In this code implementation these variables are placed in a specifc order. Think 
of them as column headings in a table. There are columns for each of the allocation 
variables first, then for each of the resource variables. There is one allocation 
variable for every combination of year, jurisdiction and judge type (in that 
order). There is one resource variable for each year, judge type and “status” (in 
that order). A “status” is one of three categories: judges in-post (A); judges 
newly hired (I); judges who’ve left (O).

To illustrate, here is an example where there are only 2 years, 2 jurisdictions 
and 2 judge types. Note that the model will always add one extra ‘dummy’ judge 
type, used to create allocation variables which count the volume of unallocated 
sitting days (i.e. unmet demand). This is represented by the ‘U’ Judge Type in 
the table below.

### Allocation Variables

Variable #  | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 
------------|---|---|---|---|---|---|---|---|---|----|----|----
Year        | 1 | 1 | 1 | 1 | 1 | 1 | 2 | 2 | 2 |  2 |  2 |  2 
Jursdiction | 1 | 1 | 1 | 2 | 2 | 2 | 1 | 1 | 1 |  2 |  2 |  2 
Judge Type  | 1 | 2 | U | 1 | 2 | U | 1 | 2 | U |  1 |  2 |  U 

### Resource Variables

Variable #  | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 | 24 
------------|----|----|----|----|----|----|----|----|----|----|----|----
Year        |  1 |  1 |  1 |  1 |  1 |  1 |  2 |  2 |  2 |  2 |  2 |  2 
Judge Type  |  1 |  2 |  U |  1 |  2 |  U |  1 |  2 |  U |  1 |  2 |  U 
Status      |  A |  I |  O |  A |  I |  O |  A |  I |  O |  A |  I |  O 

This gives us a total of 12 allocation variables and a further 12 resource 
variables: 24 in total. Variable #8, for example, represents the number of 
judges of type 2 allocated to jurisdiction 1 in year 2. Variable #22 represents 
the number of judges of type 2 available to be allocated to work (in any 
jurisdiction) in year 2.

The real model follows the same pattern but is obviously larger. With 10 years, 
15 jurisdictions and 8 judge types, there are 1,350 allocation variables and 240 
resource variables: 1,590 variables in total.

## Objective Function

The objective function is to **minimise total cost**, subject to the following definitions.

Total cost =  
1. [Sum of per sitting-day cost of fee-paid judges] + 
2. [Sum of total cost of all salaried judges] + 
3. [Sum of per-sitting day penalty costs for any unsatisfied demand]

Minimise:

**EQ-000** |   |![equation](./images/Eqn_000.png)
-----------|---|----------------------------
Where:     |   |  
Cf/yjt     | = | Average cost of one sitting day (fees) for judge of type t in jurisdiction j in year y
Syjt       | = | Number of sitting days a single judge of type t can provide for jurisdiction j in year y
Ayjt       | = | Number of judges of type t allocated to provide sitting days for jurisdiction j in year y
Cs/yt      | = | Average annual cost (salary) of judge of type t in year y
Ryt        | = | Total number of judges of type t in post in year y
Cu/yt      | = | Nominal per-sitting-day penalty associated with being unable to satisfy demand in juridiction j in year y
Uyj        | = | Total number of sitting-days unallocated – i.e. to which no judge could be allocated – for jurisdiction j in year y 

This formulation does give the flexibility to specify a combination of variable 
(fee) and fixed (salary) costs for a judge, should that be something desirable 
to model in future.

##	Constraints

###	In-post Judges
Number of judges in post and available to work in a given year is equal to the number in post in the previous year plus number hired minus number leaving.

Where: 

Ryt = number of judges of type t in post in year y
Hyt = number of judges of type t new in post in year y
Lyt = number of judges of type t who left in year y 

For each year / judge type (yt) combination:


**EQ-001a** |   |![equation](./images/Eqn_001a_Pt1.png) |              |
------------|---|--------------------------------|--------------|
**EQ-001a** |   |![equation](./images/Eqn_001a_Pt2.png) | (Rearranged) |

#### Coefficients

| Variable    | Value |
| ----------- | ------|
| R(y-1)t     | +1    |
| Ryt	      | -1    |
| Hyt	      | +1    |
| Lyt	      | -1    |

#### Equation Type

‘=’ (equals)

#### Right Hand Side (RHS)

0 (zero)

#### Exceptions / Notes

In the first year (y=1), the value of y-1 (i.e. y=0) is taken as a model input parameter, a fixed number representing the (known) initial number of judges. This leads to a further rearrangement and simplification of the equation.

**EQ-001b** |![equation](./images/Eqn_001b_Pt1.png) |              |
------------|--------------------------------|--------------|
**EQ-001b** |![equation](./images/Eqn_001b_Pt2.png) | (Rearranged) |

### Outgoing Judges

Number of judges leaving post in a given year is equal to the number leaving the profession + the number who are moving to a different role (i.e. change to become a different ‘judge type’).

The number of judges leaving the profession in each year is provided as an input parameter, i.e. fixed from the optimisers point of view. The number changing role is a function of recruitment.

Where: 

| Variable   |   | Definition                                                                                    |
|------------|---|-----------------------------------------------------------------------------------------------|
| Hyi        | = | number of judges of type i new in post in year y |
| Eyt        | = | number of judges of type t exiting the profession entirely in year y |
| Pyif       | = | the proportion of judges of type i new in post in year y who were previously judges of type f |
| Lyt        | = | number of judges of type t who left in year y |

For each year / judge type (yt) combination:

**EQ-003** |![equation](./images/Eqn_003_Pt1.png) |              |
-----------|--------------------------------|--------------|
**EQ-003** |![equation](./images/Eqn_003_Pt2.png) | (Rearranged) |

####	Coefficients

| Variable | Value |
| ---------| ------|
| Lyt      |  +1   |
| Hyt      |  -Pyt |

#### Equation Type

‘=’ (equals)

#### Right Hand Side (RHS)

Eyt (number of judges of type t exiting the profession entirely in year y)

### Demand Must Be Satisfied

The total allocated sitting days plus unallocated sitting days must be at least equal to the total demand for sitting days. (Note: our objective function will lead to the optimiser using fewer allocated sitting days wherever possible.)

Where:

| Variable |   | Definition                                                                                    |
|----------|---|-----------------------------------------------------------------------------------------------|
| Ayjt     | = | number of judges of type t allocated to provide sitting days for jurisdiction j in year y     |
| Syjt     | = | number of sitting days a single judge of type t can provide for jurisdiction j in year y      |
| Uyj      | = | number of sitting days for jurisdiction j in year y for which no judge can be allocated       |
| Dyj      | = | total demand (sitting days) for jurisdiction j in year y                                      |

For each year / jurisdiction (yj) combination:

**EQ-002** |![equation](./images/Eqn_002.png) |              |
-----------|----------------------------------|--------------|

#### Coefficients

| Variable | Value |
| ---------| ------|
| Ayjt     |  Syjt |
| Uyj      |  +1   |

#### Equation Type

‘>=’ (greater than or equal to)

#### Right Hand Side (RHS)

Dyj (demand in sitting days for justiction j in year y)

#### Notes

In practice, an additional virtual placeholder judge type is used to ‘mop up’ unallocated sitting days. So the formula simplifies to:

**EQ-002b** |![equation](./images/Eqn_002b.png) |              |
------------|--------------------------------|--------------|

Other constraints may force the optimiser to allocate more resource than is needed to satisfy demand, hence this is a “>=” constraint rather than an “=”.

### Judges only work in limited jurisdictions

Judges cannot be allocated to work in a particular jurisdictions unless it is one they work in.

Where:

| Variable |   | Definition                                                                                    |
|----------|---|-----------------------------------------------------------------------------------------------|
| Ayjt     | = | number of judges of type t allocated to provide sitting days for jurisdiction j in year y     |
| Ryt      | = | Total number of judges of type t in post in year y                                            |

**EQ-006** |![equation](./images/Eqn_006.png) |              |
-----------|--------------------------------|--------------|

#### Coefficients

| Name     | Value |
| ---------| ------|
| Ayjt     |  +1   |


#### Equation Type

‘=’ (equals)

#### Right Hand Side (RHS)

0 (zero)

#### Notes

We use the Variable Costs data input to determine which types of judge can work in which jurisdiction. The omission of a particular judge-jurisdiciton combination from this input is taken to indicate that this type of judge cannot work in this jurisdiction.

### Cannot allocate more judges than are in post

The total number of judges allocated cannot exceed the total number of judges in post.

Where:

| Variable |   | Definition                                                                                    |
|----------|---|-----------------------------------------------------------------------------------------------|
| Ayjt     | = | number of judges of type t allocated to provide sitting days for jurisdiction j in year y     |
| Ryt      | = | Total number of judges of type t in post in year y                                            |

For each jurisdiction / judge type (yt) combination:


**EQ-004** |![equation](./images/Eqn_004_Pt1.png) |              |
-----------|--------------------------------|--------------|
**EQ-004** |![equation](./images/Eqn_004_Pt2.png) | (Rearranged) |

#### Coefficients

| Name     | Value |
| ---------| ------|
| Ayjt	   |   +1  |
| Ryt	   |   -1  |

#### Equation Type

‘<=’ (less than or equal to)

#### Right Hand Side (RHS)

0 (zero)

### Judges must work a minimum number of sitting days each

The total number of sitting days allocated to each judge type must be greater than or equal to the total which would be delivered if each judge did the minimum required.

Where:

| Variable |   | Definition                                                                                    |
|----------|---|-----------------------------------------------------------------------------------------------|
| Ayjt     | = | number of judges of type t allocated to provide sitting days for jurisdiction j in year y     |
| Syt      | = | average number of sitting days a single judge of type t can provide in year y                 |
| Myt      | = | minimum number of sitting days each single judge of type t must provide in year y             |
| Ryt      | = | Total number of judges of type t in post in year y                                            |

For each year / jurisdiction (yt) combination:

**EQ-007** |![equation](./images/Eqn_007_Pt1.png) |              |
-----------|--------------------------------|--------------|
**EQ-007** |![equation](./images/Eqn_007_Pt2.png) | (Rearranged) |

#### Coefficients

| Variable | Coefficient |
| ---------| ------------|
| Ayjt     | Syt         |
| Ryj      | Myj         |

#### Equation Type

‘>=’ (greater than or equal to)

#### Right Hand Side (RHS)

0 (zero)

### Practical limit on recruitment

The total number of judges recruited in a given year cannot exceed a practical limit.

Where:

| Variable |   | Definition                                                                                    |
|----------|---|-----------------------------------------------------------------------------------------------|
| Hyt      | = | number of judges of type t hired in year y                                                    |
| Cyt      | = | Practical upper limit on the total number of judges of type t who can be hired in year y      |

For each judge type / year (yt) combination:

**EQ-008** |![equation](./images/Eqn_008.png) |              |
-----------|--------------------------------|--------------|

#### Coefficients

| Name     | Value |
| ---------| ------|
| Hyt      |  +1   |

#### Equation Type

‘<=’ (less than or equals)

#### Right Hand Side (RHS)

User-defined value (greater than zero)

### Minimum proportions in allocation

For each Judge Type in each (eligible) jurisdiction, specify the minimum proportion of demand they must be allocated. This constraint give the user the ability to influence allocations so that demand in each jurisdiction is satisfied by a realistic / acceptable mix of salaried and fee-paid judge types.

Where:

| Variable |   | Definition                                                                                                            |
|----------|---|-----------------------------------------------------------------------------------------------------------------------|
| Ayjt     | = | Number of judges of type t allocated to jurisdiction j in year y                                                      |
| Syjt     | = | Average number of sitting days a single judge of type t can provide for jurisdiction j in year y                      |
| Pyjt     | = | The minimum proportion of demand for sitting days in jurisdiction j in year y which must be satisfied by judge type t |
| Dyj      | = | Total demand for sitting days in jurisdiction j in year y                                                             |

For each year / jurisdiction / judge type (yjt) combination:

**EQ-009** |![equation](./images/Eqn_009.png) | (minimum proportion applied)            |
-----------|--------------------------------|-----------------------------------------|

#### Coefficients

| Variable | Coefficient |
| ---------| ------------|
| Ayjt     |   Syjt      |

#### Equation Type

‘>=’ (greater than or equals)

#### Right Hand Side (RHS)

The product of user-defined demand and user-specified a proportion (between 0 and 1)

### Maximum proportions in allocation

For each Judge Type in each (eligible) jurisdiction, specify the maximum proportion of demand they can be allocated. This constraint give the user the ability to influence allocations so that demand in each jurisdiction is satisfied by a realistic / acceptable mix of salaried and fee-paid judge types.

In practice this is only applied in cases where a user specifies a maximum between 0% an 100%. If the user specifies a maxium of exactly 100% (or higher, or leaves it unspecified), this constraint is not applied for that judge / jurisdiction combo. We don’t apply a constraint of 100% because we need to allow the model to over-allocate if it needs to (e.g. if it starts with too many judges).

If the user specifies a maximum of 0% this is applied as a simple ‘bound’ of zero on the allocation, effectively preventing judges of the given type being allocated to the given jurisdiction. 

Where:

| Variable |   | Definition                                                                                                            |
|----------|---|-----------------------------------------------------------------------------------------------------------------------|
| Ayjt     | = | Number of judges of type t allocated to jurisdiction j in year y                                                      |
| Syjt     | = | Average number of sitting days a single judge of type t can provide for jurisdiction j in year y                      |
| \breve{Pyjt}     | = | The maximum proportion of demand for sitting days in jurisdiction j in year y which can be satisfied by judge type t (must be > 0 and < 1) |
| Dyj      | = |Total demand for sitting days in jurisdiction j in year y                                                              |

For each year / jurisdiction / judge type (yjt) combination:

**EQ-009b** |![equation](./images/Eqn_009b.png) | (maximum proportion applied)            |
------------|--------------------------------|-----------------------------------------|

#### Coefficients

| Variable | Coefficient |
| ---------| ------------|
| Ayjt     |   Syjt      |

#### Equation Type

‘<=’ (less than or equals)

#### Right Hand Side (RHS)

The product of user-defined demand and user-specified a proportion (between 0 and 1)


