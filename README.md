**Rules of Language**     
----------------------
program ⟶ stmt_list      
stmt_list ⟶ stmt stmt_list | ε      
stmt ⟶ assignment | read_stmt | write_stmt | for_loop | if_stmt      
expr ⟶ term term_tail      
term_tail ⟶ add_op term term_tail | ε      
term ⟶ factor factor_tail 
factor_tail ⟶ mult_op factor factor_tail | ε      
factor ⟶ ( expr ) | id      
add_op ⟶ - | +      
mult_op ⟶ * | /      
rel_oper ⟶ > | < | ==      
cond ⟶ expr rel_oper expr      
assignment ⟶ id := expr      
read_stmt ⟶ read id     
write_stmt ⟶ write expr      
if_stmt ⟶ if cond then stmt_list else_stmt      
else_stmt ⟶ else stmt_list fi | fi       
for_loop ⟶ for id = id to id step_stmt do stmt_list done      
step_stmt ⟶ step id | ε      
id ⟶ <any lexeme/token that's not already expressed as a terminal above>     
